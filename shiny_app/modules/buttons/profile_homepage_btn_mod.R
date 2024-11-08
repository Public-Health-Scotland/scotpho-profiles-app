#' profile_homepage_btn module
#'
#' @description A landing page button that when clicked on, navigates to the profiles tab and updates the profile filter selection
#' 
#' # profile_homepage_btn_modUI
#' This function creates the UI for the Shiny module. It is a bslib card with a description of a profile. It has an actionLink included in it,
#' # which makes it act like a button.
#'
#' @param id A unique identifier for the module.
#' @param profile_name The name of the profile.
#' @param description A brief description of the profile to include on the card
#' @param class An optional paramater to set class of the button. Set to profile-btn by default to style button according to our apps theme

profile_homepage_btn_modUI <- function(id, profile_name, description = NULL, class = "profile-btn"){
  ns <- NS(id)
  tagList(
    bslib::card(
      class = class,
      full_screen = FALSE,
      bslib::card_body(
        actionLink(
          style = "text-decoration: none; color: black;", # prevent action link from being underlined
          inputId = ns("profile_nav"), 
          div(class = "d-flex flex-column align-items-center", # align text in the centre
              h3(profile_name, style = "color: #3F3685; font-weight: 650; text-align:center;" # align text in the centre
              ),
              p(description)
          )
        )
      )
    )
  )
}





#' navigation_button_modSERVER
#' This function creates the Server for the module. It navigates to a different tab when the button is clicked.
#'
#' @param id A unique identifier for the module.
#' @param input, output, session Standard Shiny server arguments.
#' @parent_nav the id of the apps main navigation bar. Set to "nav" by default to match our apps nav bar id (set in UI script in the page_navbar() function)
#' @nav_id the id of the nav_panel() to navigate to. This should match the value of the nav_panel() OR the title of the nav_panel() if not using the value argument
#' @parent_session the main apps session required to access the id of the main apps navigation bar. This should always be set to 'session'

profile_homepage_btn_modSERVER <- function(id, profile_name, parent_session, parent_nav_id = "nav", nav_id = "Profiles", filter_id = "profile_choices") {
  moduleServer(id, function(input, output, session) {
    
    #ns <- session$ns
    
    # when user clicks button: - 
    observeEvent(input$profile_nav, {
      
      # a. navigate to the profiles tab 
      bslib::nav_select(id = parent_nav_id, selected = nav_id, session = parent_session)
      
      # b. update the profile filter
      updateSelectizeInput(inputId = filter_id, selected = profile_name, session = parent_session)
      
    })
  }) #close moduleServer
} # close server

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Example ----
# run the functions above and un-comment the code below for example
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# library(bslib)
# library(shiny)
# library(phsstyles)
# 
# ui <- bslib::page_navbar(
#   fillable = FALSE,
#   id = "nav",
#   theme = bs_theme(
#     version = 5,# BS Version 5 required to use bslib cards
#     primary = phs_colors("phs-graphite-30")
#     ),
#   nav_panel(
#     title = "Homepage",
#     layout_column_wrap(
#       1/3, # layout 3 in a row
#       profile_homepage_btn_modUI(id = "profile_1", profile_name = "profile 1", description = "Go to profile tab and filter on profile 1"),
#       profile_homepage_btn_modUI(id = "profile_2", profile_name = "profile 2", description = "Go to profile tab and filter on profile 2"),
#       profile_homepage_btn_modUI(id = "profile_3", profile_name = "profile 3", description = "Go to profile tab and filter on profile 3")
#       )
#     ),
#   nav_panel(
#     title = "Profiles",
#     selectizeInput(
#       "profile_choices",
#       label = "choose profile:",
#       choices = c("profile 1", "profile 2", "profile 3"),
#       options = list(onInitialize = I('function() { this.setValue(""); }')) # don't pre-select anything when app launches
#       ),
#     uiOutput("profile_selection")
#   )
# )
# 
# server <- function(input, output, session){
# 
#   profile_homepage_btn_modSERVER(id = "profile_1", profile_name = "profile 1", parent_session = session)
#   profile_homepage_btn_modSERVER(id = "profile_2", profile_name = "profile 2", parent_session = session)
#   profile_homepage_btn_modSERVER(id = "profile_3", profile_name = "profile 3", parent_session = session)
# 
#   output$profile_selection <- renderUI({
#     p("You have selected ", input$profile_choices)
#   })
# }
# 
# shinyApp(ui, server)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# How do add to app ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# If adding to main UI and server scripts, use the below and replace placeholders - 
# add to main UI script:
#profile_homepage_btn_modUI(id = "placeholder", profile_name = "placeholder", description = "placeholder"),

# add to main Server script:
#profile_homepage_btn_modSERVER(id = "placeholder", nav_id = "placeholder", parent_session = session)
