ltmhi_UI <- function(id, ltmhi_dataset) {
  ns <- NS(id)
  tagList(

    # header and description
    div(
      class = "p-2 mb-2",
      h1("Long-term Monitoring of Health Inequalities in Scotland"),
      p("Description")
    ),

    # horizonal line
    hr(),


    # sidebar navigaton menu with sub-sections
    navset_pill_list(
      widths = c(3,9),


      # Key points section
      nav_panel(
        title = "Key points",
        class = "container",
        p("placeholder")
      ),


      # Summary table section
      nav_panel(
        title = "Summary table",
        class = "container",
        p("placeholder")
      ),


      # Explore indicators section
      nav_panel(
        title = "Explore indicators",
        class = "container",
        p("placeholder")
      ),


      # About section
      nav_panel(
        title = "About",
        class = "container",
        p("placeholder")
      ),


      # Report link
      nav_menu(
        title = "Read full report",
        nav_item(tags$a("Report link", href = "placeholder", target = "_blank"))
      )

    ) # close navset_pill_list

  )
}

ltmhi_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
    }
  )
}

