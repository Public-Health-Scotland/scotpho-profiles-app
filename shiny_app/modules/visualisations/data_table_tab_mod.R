#####################################.
# data_table_tab_mod.R
# This module creates the layout for the bulk download data tab
# It allows users to build a data table that can be downloaded in various formats
#####################################.

# TO DO: expand to include other datasets (i.e. pop groups splits etc.)

#######################################################.
## MODULE UI ----
#######################################################.

data_tab_modUI <- function(id) {
  ns <- NS(id)
    page_sidebar(
      sidebar = sidebar(
        width = 400, 
        open = list(mobile = "always-above"), # make contents of side collapse on mobiles above main content
        bg = "#F8FAFC",
        
        # header and clear filters button
        div(
          class = "d-flex justify-content-between",
          h4("Filters"),
          actionButton(
            inputId = ns("clear_table_filters"),
            label = "Clear filters",
            icon ("eraser"),
            class = "btn-sm"
            )
          ),
        
        
        # dataset selection filter
        card(
          card_header(
            class = "d-flex justify-content-between",
            div("Dataset", span("*", class = "text-red")),
            popover(
              bs_icon("info-circle"),
              "The main dataset contains data for all indicators at various geography levels.
               The inequalities dataset is a smaller subset of indicators and geographies,
               split by SIMD quintiles and other measures of inequality (this dataset underpins
               the visualisations in the inequalities sub-tabs)."
              )
            ),
          card_body(
            helpText("Select a dataset to get started."),
            radioButtons(
              inputId = ns("dataset_selector"),
              label = NULL,
              choices = c("Main Dataset","Inequalities Dataset"),
              selected = "Main Dataset" # default on main dataset
            )
          )
        ),
        
        # quintile type filter (hidden by default - only shown if inequalities dataset is selected)
        hidden(
          div(
            id = ns("inequality_extras"),
            card(
              card_header(
                class = "d-flex justify-content-between",
                div("SIMD Quintile types", span("*", class = "text-red")),
                popover(
                  bs_icon("info-circle"),
                  p("Scottish quintile 1 refers to the datazones containing the 20% most deprived population in Scotland. Local quintile 1 contains the datazones where the 20% most deprived population within a particular Health board or Council area reside."),
                  p("Scottish quintiles therefore highlight inequalities between Scotland’s most and least deprived population, while local quintiles show inequalities within an area.")
                )
              ),
              helpText("Select one or more quintile type."),
              checkboxGroupInput(
                inputId = ns("quint_type_selector"), 
                label = NULL, 
                choices = c("Local", "Scotland"),
                selected = "Scotland" # default on scottish quintiles
              )
            )
        )
        ),
        

        # Geography filters
        card(
          card_header(
            div("Geographies", span("*", class = "text-red"))
          ),
          card_body(
            helpText("Use the '+' symbol to expand areatypes and the checkboxes to select areas. Selecting the checkbox next to an areatype will select all areas within that geography level."),
            quercusInput(
              inputId = ns("geography_selector"),
              label = NULL,
              choices = main_data_geo_nodes, # defined in global script
              returnValue = "id", # return geography code instead of name 
              width = "100%",
              searchPlaceholder = "Type to search geographies",
              searchEnabled = TRUE, # add search box 
              showChildrenOnSearch = TRUE,
              checkboxSelectionEnabled = TRUE, # add checkboxes
              multiSelectEnabled = TRUE, # allow multiple selections
              cascadeSelectChildren = TRUE # select all child areas when parent area selected
            )
          )
        ),
                                         
        # profile filter
        card(
          card_header("Profiles", helpText("(optional)")),
          card_body(
            helpText("Filter dataset by profiles. Some profiles may be disabled depending on the dataset and geography selected."),
            pickerInput(
              inputId = ns("profile_selector"),
              label = NULL,
              choices = names(discard(profiles_list, ~ .x$short_name %in% c("ALL", "SHI"))), # exclude 'All indicators' and LTMHI profiles from choices
              multiple = TRUE,
              options = list(container = "body") # ensures drop-down can be expanded outside the sidebar if needed
            )
          )
        ),
        
        # indicator filter
        card(
          card_header("Indicators", helpText("(optional)")),
          helpText("Filter dataset by indicators. Indicator choices are dependent on previous selections."),
          pickerInput(
            inputId = ns("indicator_selector"),
            label = NULL,
            choices = NULL,
            multiple = TRUE,
            options = list(container = "body") # ensures drop-down can be expanded outside the sidebar if needed
          )
        )
        ), # close sidebar
      
      h1("Download data"),
      p("Use the filters to build a data table, which can then be downloaded in various formats using the button below. 
         The table below is a preview. The downloaded dataset will contain more columns containing metadata 
         than are presented here."),
                 
                 
     # download data button
     download_data_btns_ui(id = ns("datatable_downloads")),
     

     # data table
     card(
       card_header(p(textOutput(ns("row_count"), inline = TRUE), " rows")),
       card_body(DTOutput(ns("data_tab_table")))
     )
     
     ) # close layout

}

#######################################################.
## MODULE SERVER ----
#######################################################.

data_tab_mod_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns


      #############################.
      # REACTIVE DATASETS ----
      #############################.

      # selected dataset
      selectedData <- reactive({
        switch(input$dataset_selector,
               "Main Dataset" = main_dataset,
               "Inequalities Dataset" = simd_dataset)
        })
      

      # data to display in table /download
      tableData <- reactive({

        # selected dataset
        data <- selectedData()

        # filter by selected geographies
        data <- data[code %in% input$geography_selector]


        # filter by quint type(s) (if inequalities dataset selected)
        if(input$dataset_selector == "Inequalities Dataset"){
        if(length(input$quint_type_selector) == 1){
          if("Scotland" %in% input$quint_type_selector){
          data <- data[quint_type == "sc_quin"]
          } else {
          data <- data[quint_type != "sc_quin"]
          }
        }
        }

        # further filter by profile (if any selected)
        if(!is.null(input$profile_selector)){

          # find short 3-letter profile names from profiles list
          # in global script for selected profile(s) and create regex
          # to search for in dataset "e.g. DRG|ALC" 
          profile_regex <- profiles_list[input$profile_selector] |>
            map_chr("short_name") |>
            paste(collapse = "|")

          # filter rows where any of short names found in profile_domain column
          # using regex created above 
          data <- data[grepl(profile_regex, profile_domain)]
        }


        # further filter by indicator (if any selected)
        if(!is.null(input$indicator_selector)){
          data <- data[indicator %in% input$indicator_selector]
        }


        # rename some columns
        data <- data |>
          rename(area_code = code,
                 area_type = areatype,
                 area_name = areaname,
                 period = def_period,
                 upper_confidence_interval = upci,
                 lower_confidence_interval = lowci)

        
        # columns to return if main dataset was selected
        if(input$dataset_selector == "Main Dataset") {

          data <- data |>
            select(area_code, area_type, area_name, year, period, type_definition,
                   indicator, numerator, measure,
                   upper_confidence_interval, lower_confidence_interval) } else {

                     # columns to return if inequalities dataset was selected
                     # note this requires some reshaping due to the format of the inequalities dataset

                     # all inequalities data
                     data <- data |>
                       rename(value = measure,
                              measure = type_definition)

                     # sii data
                     sii <- data |>
                       filter(quintile == "Total") |>
                       mutate(value = sii,
                              upper_confidence_interval = upci_sii,
                              lower_confidence_interval = lowci_sii) |>
                       mutate(measure = "Slope index of inequality (SII)",
                              quintile = NULL
                       )

                     # rii data
                     rii <- data |>
                       filter(quintile == "Total") |>
                       mutate(value = rii,
                              upper_confidence_interval = upci_rii,
                              lower_confidence_interval = lowci_rii) |>
                       mutate(measure = "Relative index of inequality (RII)",
                              quintile = NULL)

                     # par data
                     par <- data |>
                       filter(quintile == "Total") |>
                       mutate(value = par,
                              upper_confidence_interval = upci_rii_int,
                              lower_confidence_interval = lowci_rii_int) |>
                       mutate(measure = "Population attributable risk (PAR)",
                              quintile = NULL)

                     # different inequalities measures combined
                     data <- bind_rows(data, rii, sii, par) |>
                       select(area_code, area_type, area_name, year, period, indicator,
                              quint_type, quintile, measure, value, upper_confidence_interval,
                              lower_confidence_interval) |>
                       arrange(indicator, area_name, year)
                   }

                  # add data source column
                  techdoc <- techdoc |>
                    select(indicator_name, data_source)

                  data <- data |>
                    left_join(techdoc, by = c("indicator" = "indicator_name"))

      })


    #####################################.
    # DYNAMIC FILTERS ----
    #####################################.


      # Update geography choices depending on selected dataset
      # (as only Scotland, HB and CA data available in inequalities dataset)
      observeEvent(input$dataset_selector, {

        # either return full list or only part of list containing scotland, HB, CA choices
        valid_choices <- switch(
          input$dataset_selector,
          "Main Dataset" = main_data_geo_nodes,
          "Inequalities Dataset" = main_data_geo_nodes[c(1:3)]
        )

        # update filter with choices
        updateQuercusInput(inputId = "geography_selector", choices = valid_choices)

      })
      
      
      # show/hide quintile type filter depending on selected dataset
      observeEvent(input$dataset_selector, {
        if(input$dataset_selector == "Inequalities Dataset"){
          shinyjs::show("inequality_extras")
        } else {
          shinyjs::hide("inequality_extras")
        }
      })
      
      # enable/disable quintile type filter depending on selected geographies
      # as only Scottish quintiles can be selected if no local areas selected
      observeEvent(input$geography_selector, {
        req(input$dataset_selector == "Inequalities Dataset")
        req(input$geography_selector)
        

        if(length(input$geography_selector) == 1 & "S00000001" %in% input$geography_selector){
          updateCheckboxGroupInput(
            inputId = "quint_type_selector",
            selected = "Scotland"
          )
          disable("quint_type_selector")
        } else {
          enable("quint_type_selector")
        }

      })



      # Update profile filter choices based on dataset and geography selected
      observeEvent(c(input$geography_selector, selectedData()), {
        req(input$geography_selector) # don't run until a geography has been selected

        # further filter selected dataset by selected geographies and return unique values
        # in profile_domain column 
        geo_profiles <- unique(selectedData()[code %in% input$geography_selector, profile_domain])

        # get names of profiles excluding 'All Indicators' and 'Long-term Montitoring of HE'
        # as we don't want these to be choices in the profiles filter
        all_profiles <- names(discard(profiles_list, ~ .x$short_name %in% c("ALL", "SHI")))

        # with he exception of 'All Indicators' and 'LTMHI' profiles,
        # go through each profile in the profiles list from global script and 
        # return TRUE if the profile should be disabled or FALSE if it should be enabled 
        # e.g. if 'ALC' is not found in 'geo_profiles' then return TRUE else FALSE
        profile_disable <- map_lgl(
          discard(profiles_list, ~ .x$short_name %in% c("ALL", "SHI")),
          ~ !any(grepl(.x$short_name, geo_profiles))
        )

        # convert named vector created above to unnamed vector
        profile_disable <- unname(profile_disable)



        # disable invalid choices
        updatePickerInput(
          inputId = "profile_selector",
          session = session,
          choices = all_profiles,
          choicesOpt = list(
            disabled = profile_disable,
            style = ifelse(profile_disable,
                           yes = "color: rgba(119, 119, 119, 0.5);",
                           no = "")
          )
        )

      })


      # Update indicator choices depending on selected dataset, geography(s) and (optionally) profile(s)
      observeEvent(c(input$geography_selector, input$profile_selector), {

        # take selected dataset and filter on area selections
        data <- selectedData()[code %in% input$geography_selector]


        # if profile(s) have been selected then further filter the data
        if(!is.null(input$profile_selector)){

          # get short names for selected profiles
          profile_regex <- profiles_list[input$profile_selector] |>
            map_chr("short_name") |>
            paste(collapse = "|")


          data <- data |>
            filter(grepl(profile_regex, profile_domain))
        }

        # get unique indicator names
        valid_choices <- unique(data$indicator)

        # update indicator filter choices
        updatePickerInput(inputId = "indicator_selector", choices =  valid_choices)

      })





      ## reset all filters when 'clear filters' button is clicked
      observeEvent(input$clear_table_filters, {

        # reset the dataset selector to "Main Dataset"
        updateRadioButtons(session,
                                inputId = "dataset_selector",
                                selected = "Main Dataset")

        # reset the geographies to those available for the main dataset
        updateQuercusInput(session = session,
                           inputId = "geography_selector", 
                           choices = main_data_geo_nodes)



        # reset the indicator list to those present in main dataset
        updateVirtualSelect(session = session,
                            inputId = "indicator",
                            selected = NULL,
                            choices = NULL)

        # reset the profile filter
        updateVirtualSelect(session = session,
                            inputId = "profile_selector",
                            selected = NULL,
                            choices = names(discard(profiles_list, ~ .x$short_name %in% c("ALL", "SHI"))))

      })
      
      
      # return number of rows in filtered dataset
      output$row_count <- renderText({
        nrow(tableData())
      })



    ##############################.
    # DATA TABLE ----
    ##############################.

      output$data_tab_table <- renderDT({


        datatable(tableData(),
                  style = 'bootstrap',
                  caption = sprintf('Total rows: %s', nrow(tableData())),
                  rownames = FALSE,
                  options = list(#scrollX = TRUE,
                                # scrollY = "600px",
                                 pageLength = 100,
                                 searching = FALSE,
                                 language = list(
                                   zeroRecords = "Select atleast one geography to display results.")
                  ))



      })


            ##################################.
            # Downloads ----
            ##################################.

            # data table bulk download 
            download_data_btns_server(id = "datatable_downloads", data = tableData, file_name="ScotPHO_datatab_extract")



    }
  )
}



######################################.
# EXAMPLE USAGE 
# uncomment the code below to see how this module works
# note this module requires the 'main_dataset' parquet file, the 'main_data_geo_nodes' file 
# and the 'profiles_list' object from the global script
# Therefore recommended that you first run the main app first so these 3 things become available in your global environment
######################################.

# # packages
# library(shiny)
# library(bslib)
# library(jsTreeR)
# 
# # ui
# ui <- fluidPage(
#   data_tab_modUI(id = "dt") # assign the module a unique id 
#   
# )
# 
# # server
# server <- function(input, output, session) {
#   data_tab_mod_Server(id = "dt") # use the matching id assigned in the ui
# }
# 
# # run app 
# shinyApp(ui, server)