
######
# 6. Highcharter theme --------------------------------------------------------------

chart_theme <- hc_theme(
  
  colors = c("#000000", "#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99",
             "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", "#b15928"),
  
  chart = list(backgroundColor = NULL,
               style = list(fontSize = "14px",
                            fontFamily = "Arial")),
  
  title = list(
    align = "left",
    style = list(fontSize = "16px",
                 fontWeight = "bold",
                 color = "black",
                 fontFamily = "Arial")), 
  
  subtitle = list(
    align = "left",
    style = list(color = "black",
                 fontFamily = "Arial")),
  
  xAxis = list(
    labels = list(
      style = list(
        fontSize = "14px"))),
  
  yAxis = list(
    labels = list(
      style = list(
        fontSize = "14px")),
    min = 0),
  
  legend = list(
    itemStyle = list(fontFamily = "Arial",
                     color = "black"),
    itemHoverStyle = list(color = "black")),
  
  tooltip = list(
    shadow = FALSE,
    crosshairs = TRUE,
    style = list(
      fontFamily = "Arial",
      fontSize = "14px"))
)


#####

# Load and filter data
i_data <- read_csv("/PHI_conf/ScotPHO/Profiles/Data/Test Shiny Data/88007_meeting_mvpa.csv")

test_data <-i_data %>% 
  filter(split_name!="simd"&split_name!="geog")

test_data_v2 <-i_data %>% 
  filter(split_name!="simd"&split_name!="geog") %>% 
  group_by(split_name) %>% 
  mutate(trend_option = case_when(n_distinct(year) >= 3 ~ "trend", TRUE ~ "no_trend"))


pop_rank_data <-i_data %>% 
  group_by(split_name) %>% 
  filter(split_name!="simd"&split_name!="geog"&year==max(year))

palette = phs_colours(c('phs-purple','phs-rust',  'phs-teal', 'phs-blue', 'phs-green','phs-magenta'))

split_name_list = unique(test_data$split_name)
split_value_list = unique(test_data$split_value)

# colour_pal_im = c(split_value == "16-24", phs_colors(colourname = "phs-purple"),
#                    split_value == "25-34", phs_colors(colourname = "phs-magenta"),
#                    split_value == "35-44", phs_colors(colourname = "phs-teal"),
#                    split_value == "35-44", phs_colors(colourname = "phs-blue"),
#                    split_value == "45-54", phs_colors(colourname = "phs-green"),
#                    #
#                    split_value == "55-64", phs_colors(colourname = "phs-purple-50"),
#                    split_value == "65-74", phs_colors(colourname = "phs-rust"),
#                    split_value == "75+", phs_colors(colourname = "phs-liberty"),
#                    split_value == "Total ages", phs_colors(colourname = "phs-blue"),
#                    #
#                    split_value == "All sex", phs_colors(colourname = "phs-purple"),
#                    split_value == "Female", phs_colors(colourname = "phs-teal"),
#                    split_value == "Male", phs_colors(colourname = "phs-blue"),
#                    #
#                    split_value == "limiting_li", phs_colors(colourname = "phs-purple"),
#                    split_value == "no_li", phs_colors(colourname = "phs-teal"),
#                    split_value == "non_limiting_li", phs_colors(colourname = "phs-blue"),
#                                )

#color = "#FF0000", #red colour for average line



ui <- fluidPage(
  useShinyjs(),  # Include shinyjs
  titlePanel("TEST"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput(
        inputId = "split_filter",
        label = "Select equality split:",
        choices = unique(test_data$split_name),
        selected = "age"
      ),
      
      "Place holder",
      
      checkboxInput("ci_switch", label = "Include confidence intervals", TRUE)
    ), # sidebarPanel
    mainPanel(
      width = 9,
      withSpinner(highchartOutput("pop_rank_chart")),
      withSpinner(highchartOutput("pop_trend_chart"))
    ) # mainPanel
  ) # sidebarLayout
) # fluidPage

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    subset(test_data, split_name == input$split_filter)
  })
  
  observe({
    enable("ci_switch")
  })
  
  pop_rank_data <- reactive({
    filtered_data() %>%
      filter(year == max(year))
  })

  pop_trend_data <- reactive({
    filtered_data()      })
  
  
  output$pop_rank_chart <- renderHighchart({
    shiny::validate(
      need(nrow(pop_rank_data()) > 0, "Data data are not available at the selected level. Please select either Scotland, Health board or Council area.")
    )
    
    x <- hchart(pop_rank_data(), 
                type = "column", hcaes(x = sub_code, y = rate, color = phs_colors("phs-blue"))) %>%
      hc_yAxis(gridLineWidth = 0) %>%
      hc_chart(backgroundColor = 'white') %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = "")) %>%
      hc_plotOptions(series = list(animation = FALSE),
                     column = list(groupPadding = 0))
    

    
    if(input$ci_switch) {
      x <- x |>
        hc_add_series(pop_rank_data(), "errorbar", hcaes(x = sub_code, low = lowci, high = upci), zIndex = 10)
    }
    
    x
  }) # end pop_rank_chart
  
  
  # ###############
  # # trend chart
  output$pop_trend_chart <- renderHighchart({
    
    # create reactive dataset filtered by selected indicator and geography area
    # pop_trend_data <- reactive({
    #   filtered_data()      })
    
    # define objects for chart titles and labels
    selected_area <- unique(pop_trend_data()$location_name)
    definition <- unique(pop_trend_data()$def_period)
    split_name <- input$split_filter
    chart_title <- paste(definition, "in", selected_area, "by", split_name)
    
    
    # generate name value for line chart
    selected_split <- unique(pop_trend_data()$split_value)
    
    
    # create highchart object
    chart <- highchart() %>%
      hc_add_series(pop_trend_data(),
                    type = "line",
                    hcaes(x = year, y = rate, group=split_value),
                    name = selected_split) %>%
      
      
      # rename titles and format legend
      hc_title(text = chart_title) %>%
      # hc_subtitle(text = type_definition) %>%
      hc_xAxis(title = "") %>%
      #  hc_yAxis(title = list(text = type_definition)) %>%
      hc_legend(align = "left", verticalAlign = "top") %>%
      #format tooltip
      hc_tooltip(headerFormat = "", shared = TRUE) %>%
      # set theme (defined in global script) currently copied from cd_trend and run in console
      hc_add_theme(chart_theme)
    
  }) #end trend chart
  
  
} # server

shinyApp(ui = ui, server = server)