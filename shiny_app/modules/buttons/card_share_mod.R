#' Share Card Module
#'
#' A Shiny module with a "Share" button. When clicked,
#' the module stores the associated `card_id` in `session$userData$share_card`
#' and triggers bookmarking of the app. This allows external sharing of
#' a URL that encodes which card the user intended to share.
#'


#' Share Button Module UI
#'
#' Creates a small Shiny `actionButton` labelled "Share", to be placed 
#' inside a `bslib::card()` containing a chart for users to share.
#'
#' @param id Character string.
#'
#' @return A Shiny `actionButton`
#'
#' @examples
#' # In UI:
#' share_button_mod_UI("chart")
#'
share_button_mod_UI <- function(id) {
  ns <- NS(id)
  actionButton(
    inputId = ns("share_btn"),
    label = "Share",
    icon = icon("share-nodes"),
    class = "btn-sm"
  )
}




#' Share Button Module Server
#'
#' When the user clicks the hare button, this function:
#' 
#' 1. Stores the provided `card_id` in `session$userData$share_card`.
#' 2. Triggers bookmarking via `session$doBookmark()`.
#' 3. Ensures the Share button input is excluded from appearing as a
#'    bookmark parameter in the resulting URL.
#'
#'
#' @param id Character string. The module's namespace ID (should match the UI function).
#' @param card_id Character string. The ID of the card being shared.
#'
#' @return None. Called for side‑effects.
#'
#' @examples
#' # In server:
#' share_button_mod_Server(id = "chart", card_id = "simd_card")
#'
share_button_mod_Server <- function(id, card_id) {
  moduleServer(id, function(input, output, session) {
    
    # When the share button is clicked: store the card ID and bookmark the session
    observeEvent(input$share_btn, {
      session$userData$share_card <- card_id
      session$doBookmark()
    })
    
    # exclude button from bookmarked URL
    setBookmarkExclude("share_btn")
  })
}

