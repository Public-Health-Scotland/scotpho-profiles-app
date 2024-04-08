###to do - need a fix to close the nav menu when navigates to a profile tab - monica

# Profile card module ----------------------------------------------------------
# creates 'profile' cards for the landing page which when clicked on, navigate to the tab for that profile

# ui function
# when using this function, need to choose profile name and icon 
profile_homepage_btn_modUI <- function(id, profile_name, profile_icon) {
  ns <- NS(id)
  bslib::card(
    full_screen = FALSE,
    bslib::card_body(
      actionLink(inputId = ns("profile_nav"), 
                 label = 
                   div(
                     class = "d-flex flex-column align-items-center", 
                     div(class = "icon-box", 
                         icon(profile_icon, class = "fa-3x")
                     ),
                     h3(profile_name),
                     icon("chevron-right", class = "fa-1x")
                   )
      )
    )
  )
}


# server function
profile_homepage_btn_modSERVER <- function(id, nav_id, parent_session) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$profile_nav, {
      bslib::nav_select(id = "nav", 
                        selected = nav_id, 
                        session = parent_session)
    })
  })
}

##############################################################################
# example usage
##############################################################################
# library(bslib)
# library(shiny)
# ui <- page_navbar(id = "nav",
#                   title = "Profile card module example",
#                   nav_panel(title = "tab 1",
#                             value = "t1",
#                             profile_homepage_btn_modUI(id = "go_to_tab2",
#                                                 profile_name = "Profile 2",
#                                                 profile_icon = "line-chart")
#                             ),
#                   nav_panel(title = "tab 2",
#                             value = "t2",
#                             p("Tab number 2"))
#                   )
# 
# 
# 
# server <- function(input, output, session) {
#   profile_homepage_btn_modSERVER(id = "go_to_tab2", nav_id = "t2", parent_session = session)
# }
# 
# 
# shinyApp(ui, server)


