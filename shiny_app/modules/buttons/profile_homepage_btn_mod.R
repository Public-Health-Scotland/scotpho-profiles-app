#' Profile Homepage Button Module
#'
#' A Shiny module that creates clickable profile cards on homepage for navigation.
#' The cards can be active or inactive, and include optional 'new' badges.
#'
#' @name profile_homepage_btn_mod

# UI Function ----

#' Profile Homepage Button Module UI
#'
#' Creates a clickable card for profile navigation.
#'
#' @param id Character string. The module's namespace ID. Also the display name of the profile.
#' @param description Character string. Description text shown in the card body.
#' @param active Logical. Whether the button should be active/clickable. Default is TRUE.
#' @param new_badge Logical. Whether to display a "New" badge. Default is FALSE.
#'
#' @return A bslib card with onclick logic.
#'
#' copy in UI:
#' profile_homepage_btn_modUI(
#'   id = "Alcohol",
#'   description = "Descripion of alcohol profile",
#'   active = TRUE,
#'   new_badge = FALSE
#' )
#'
profile_homepage_btn_modUI <- function(id,
                                       description,
                                       active = TRUE,
                                       new_badge = FALSE) {
  
  ns <- NS(id)
  
  
  tagList(
    
    # change card background to grey when hovered on
    tags$style(HTML(".profile-btn:hover { background-color: #E0E0E0; }")),
    
    
    # create card
    bslib::card(
      
      # conditional card class and style depending on whether button is active/inactive
      class = if (active) "profile-btn p-3" else "profile-btn-disabled p-3",
      style = if (active) "cursor: pointer;" else "opacity: 0.6; cursor: not-allowed;",
      
      # add on-click logic only if card is active - each time card is clicked the value for
      # input$card_clicked will update to a random number, which triggers server-side code to run
      onclick = if (active) paste0("Shiny.setInputValue('", ns("card_clicked"), "', Math.random())"),
      
      # card header, containing profile name (and 'new' badge if new == TRUE)
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center border-bottom-0",
        
        h4(id,
           class = "mb-0 text-phs-purple",
           style = "font-size: 1.5rem;"
        ),
        
        if (new_badge) span(class = "badge rounded-pill text-bg-phs-green ms-auto", "New")
      ),
      
      # card body, containing description of profile
      bslib::card_body(description)
    )
  )
}






# Server Function ----

#' Profile Homepage Button Module Server
#'
#' Server logic for the profile homepage button module. When a card is clicked:
#'   - Navigates to the specified tab
#'   - If navigating to "Profiles" tab, also updates the profile filter dropdown
#'
#' @param id Character string. The module's namespace ID (should match the UI function).
#' @param nav_select Character string. The nav_panel ID to navigate to when clicked.
#' @param parent_session Shiny session object. The parent session for navigation updates. 
#' Should always be set to 'session' to target apps main server session
#'
#'
#' @return Returns the result of moduleServer().
#'

#' # copy in server:
#' profile_homepage_btn_modSERVER(
#'   id = "Alcohol",
#'   nav_select = "Profiles",
#'   parent_session = session
#' )
#'
profile_homepage_btn_modSERVER <- function(id, nav_select, parent_session) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$card_clicked, {
      # Navigate to the specified tab
      bslib::nav_select(
        id = "nav", 
        selected = nav_select,
        session = parent_session
      )
      
      # Update profile filter if navigating to Profiles tab
      if (nav_select == "Profiles") {
        updateSelectizeInput(
          inputId = "profile_choices", 
          selected = id, 
          session = parent_session
        )
      }
    })
  })
}


# Module Usage Example ----
# uncomment code below to run example shiny app
# library(shiny)
# library(bslib)
# 
# shinyApp(
#   ui = bslib::page_navbar(
#     id = "nav",
#     bslib::nav_panel(
#       title = "Home",
#       profile_homepage_btn_modUI(
#         id = "Tobacco",
#         description = "Description of Tobacco profile",
#         active = TRUE,
#         new_badge = TRUE
#         )
#       ),
#     nav_panel(
#       title = "Profiles",
#       selectizeInput(
#         inputId = "profile_choices",
#         label = NULL,
#         choices = c("Alcohol", "Drugs", "Tobacco")
#         )
#       )
#     ),
#   server = function(input, output, session) {
# 
#   profile_homepage_btn_modSERVER(
#     id = "Tobacco",
#     nav_select = "Profiles",
#     parent_session = session
#   )
#   }
# )
