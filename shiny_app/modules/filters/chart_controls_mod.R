#' Chart Controls Module
#'
#' A Shiny module that creates chart controls inside a popover and returns their value
#' (confidence intervals switch, y-axis at 0 switch, average switch)
#' The number of controls to include is optional
#'
#' @name chart_controls_mod

# UI Function ----

#' Chart Controls Module UI
#'
#' Creates collection of input_switches which can be used to amend charts
#'
#' @param id Character string. The module's namespace ID
#' @param controls Named vector. Name of controls to include and whether should be TRUE/FALSE on initial load. Optional
#' arg which when not used adds all 3 controls. Only need to use if want a different default than those listed below and/or only 
#' want to include use some of the controls


#'
#' @return collection of bslib input_switches
#'
#' copy in UI:
#' chart_controls_mod_UI(id = "simd_chart_controls")
#'




chart_controls_mod_UI <- function(id,
                                  controls = c(ci_switch = FALSE, avg_switch = FALSE, zero_yaxis_switch = TRUE)) {
  
  # namespace id
  ns <- NS(id)
  
  
  # input labels
  labels <- c(
    ci_switch = "Include confidence intervals",
    avg_switch = "Include average",
    zero_yaxis_switch = "Start y-axis at zero"
  )
  
  
  # create between 1-3 inputs
  # (depending on how many passed to 'controls' arg)
  # if argument not in use all 3 will be included
  # .y is the name of the input (e.g. ci_switch)
  # .x is the default value assigned to that input (e.g. TRUE or FALSE)
  inputs <- imap(controls, ~ {
    input_switch(
      id = ns(.y),
      label = labels[[.y]],
      value = .x
    )
  })
  
  
  # add inputs to popover within nav item
  nav_item(
    popover(
      title = "Chart settings",
      trigger = actionButton(
        inputId = ns("chart_settings_btn"),
        label = "Settings",
        icon = icon("gear"),
        class = "btn-sm" # i.e button small 
      ),
      inputs
    )
  )
  
}


# Server Function ----

#' Chart controls Module Server
#'
#' Server logic for the chart controls module:
#'
#' Manages the state of the chart‑control switches created in the UI function. 
#' It tracks both the user's current selections and the initial default values
#'
#' The function:
#'   a. stores initial value of each switch when app first loads (`rv_init`)
#'   b. updates reactive values object (`rv`) whenever the user changes any control
#'   c. compares current selections to their defaults so that unchanged controls
#'     can be excluded from Shiny bookmarking, helping to keep the URL shorter;
#'   d. returns the reactive Values object (`rv`)containing the current state of all
#'     controls
#'
#' These returned reactive values can be passed to other modules or server logic
#' to dynamically adjust chart appearance based on user‑selection.

#'
#' @param id Character string. The module's namespace ID (should match the UI function).
#'
#' @return Returns a **reactiveValues** object
#'

#' # copy in server:
#' chart_controls_mod_server(id = "simd_chart")


chart_controls_mod_server <- function(id) {
  moduleServer(id, function(input, output, session) {


    # input ids of switches created in UI
    input_ids <- c("ci_switch", "avg_switch", "zero_yaxis_switch")


    # object to store and return user selections
    rv <- reactiveValues(
      ci_switch = NULL,
      avg_switch = NULL ,
      zero_yaxis_switch = NULL
    )

    # object to store default selections when app loads
    # (this is only updated once)
    rv_init <- reactiveValues(
      ci_switch = NULL,
      avg_switch = NULL,
      zero_yaxis_switch = NULL
    )



    # populate rv_init with default selections
    # as soon as app loads (this code only runs once)
    purrr::walk(input_ids, ~ {
      observeEvent(input[[.x]], {
        rv_init[[.x]] <- input[[.x]]
      }, ignoreInit = FALSE, once = TRUE)
    })


    # store selections on both initial load and
    # each time they change thereafter
    # (these are the values the module returns to be passed
    # to other modules for building charts)
    observe({
      purrr::walk(input_ids, ~ { rv[[.x]] <- input[[.x]]})
    })



    # dynamically set bookmark exclusions (only purpose of this code is to reduce the URL length)
    # whenever rv changes (i.e. current user selections), compare against init_rv (i.e. default selections)
    # If the currently selected value is the same as the default value, then exclude from any url

    # Note doing exclusions inside a reactive context like an observer means it doesn't affect all
    # instances of this module in the app - it's namespace aware.
    # e.g. if using the chart controls mod twice and both uses have the CI switch turned off by default
    # but user turns 1 of the CI switches on - that will be the only one captured in the URL (rather
    # than all CI switches being included in the URL)

    # small quirk with bookmark exclusions in modules:
    # setBookmarkExclude overwrites other calls to setBookmarkExclude WITHIN the same module
    #  https://github.com/rstudio/shiny/issues/2323
    # This code set up gets around that issue by re-calculating which inputs to include/exclude
    # each time any of them changes
    observe({
      exclusions <- purrr::keep(
        names(rv),
        ~ {!is.null(rv[[.x]]) & identical(rv[[.x]], rv_init[[.x]])}
      )
      
      # apply exclusions alongside the chart settings button which should always be excluded!
      setBookmarkExclude(c(exclusions, "chart_settings_btn")) 

    })



    # return reactive values, ready to be used
    # elsewhere in the app (e.g. within another module)
    rv

  })
}



# Module Usage Example ----
# uncomment code below to run example shiny app
# library(shiny)
# library(bslib)

# shinyApp(
#   ui = function(request){
#     page(
#       layout_column_wrap(
#         div(
#           p("Card with inputs module:"),
#           bslib::navset_card_pill(
#             nav_panel("Chart", "placeholder"),
#             nav_spacer(),
#             chart_controls_mod_UI(id = "example")
#             )
#           ),
#           div(
#             p("The values returned from the module:"),
#             verbatimTextOutput("results"),
#           )
#         ),
#       div(
#         p("The URL that is updated whenever the bookmark button is pressed. If user doesn't change the
#                 default values of the switches, they won't appear in the URL, otherwise they will."),
#         textOutput("url"),
#         bookmarkButton()
#       )
#       )
#     },
#   server = function(input, output, session) {
# 
#   selections <- chart_controls_mod_server(id = "example")
# 
#   output$results <- renderPrint({
#     shiny::reactiveValuesToList(selections)
#   })
# 
#   onBookmarked(function(url) {
#     output$url <- renderText({
#       paste("url:", url)
#     })
#   })
# 
#   },
#   enableBookmarking = "url"
# )