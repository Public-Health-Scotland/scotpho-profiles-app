#' navigation_button module
#'
#' @description A button that when clicked on, navigates to a particular tab.
#' 
#' This function creates the UI for the Shiny module. It is a button.
#'
#' @param button_id A unique identifier for the module.
#' @param button_name The buttons label 
#' @param button_icon An optional parameter to include an icon. Set to NULL by default
#' @param class An optional paramater to set class of the button. Set to NULL by default

navigation_button_modUI <- function(button_id, button_name, button_icon=NULL, class = NULL){
  ns <- shiny::NS(button_id)
  tagList(
    shiny::actionButton(
      inputId= ns("button_nav"), 
      label = button_name, 
      class = class,
      icon = button_icon)
  )
}




#' My Shiny Module Server function
#'
#' This function creates the Server for the module. It navigates to a different tab when the button is clicked.
#'
#' @param id A unique identifier for the module.
#' @param input, output, session Standard Shiny server arguments.
#' @parent_nav the id of the apps main navigation bar. Set to "nav" by default to match our apps nav bar id (set in UI script in the page_navbar() function)
#' @nav_id the id of the nav_panel() to navigate to. This should match the value of the nav_panel() OR the title of the nav_panel() if not using the value argument
#' @parent_session the main apps session required to access the id of the main apps navigation bar. This should always be set to 'session'
navigation_button_modSERVER <- function(id, nav_id, parent_nav = "nav", parent_session) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # tweaked from example here: https://stackoverflow.com/questions/77373747/adding-a-bslibnav-panel-in-shiny-modules
    observeEvent(input$button_nav, {
      bslib::nav_select(id= parent_nav, selected = nav_id, session = parent_session)
    })
    
  })
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Example ----
# run the functions above and un-comment the code below for example
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# library(bslib)
# library(shiny)
# library(phsstyles)
# 
# ui <- bslib::page_navbar(
#   id = "nav", # main navigation bars id
#   theme = bs_theme(
#     version = 5,
#     primary = phs_colors("phs-green"),
#     secondary = phs_colors("phs-blue")
#     ),
#   nav_panel(
#     title = "tab 1",
#     p("Tab number 1"),
#     navigation_button_modUI(button_id="go_to_tab2", button_name = "Click to go to Tab 2", button_icon = icon("otter"), class = "btn-primary")
#     ),
#   nav_panel(
#     title = "tab 2",
#     value = "t2",
#     p("Tab number 2"),
#     navigation_button_modUI(button_id="go_to_tab1", button_name = "Click to go to Tab 1", class = "btn-secondary")
#   )
# )
# 
# 
# 
# server <- function(input, output, session) {
#   navigation_button_modSERVER(id = "go_to_tab2", nav_id="t2", parent_session = session) # example using nav_panel() value
#   navigation_button_modSERVER(id = "go_to_tab1", nav_id="tab 1", parent_session = session) # example using nav_panel() title
# 
# }
# 
# shinyApp(ui, server)
# 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# How do add to app ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# If adding to main UI and server scripts, use the below and replace placeholders - 
# add to main UI script:
#navigation_button_modUI(button_id="placeholder", button_name = "placeholder")

# add to main Server script:
#navigation_button_modSERVER(id = "placeholder", nav_id="placeholder", parent_session = session)

