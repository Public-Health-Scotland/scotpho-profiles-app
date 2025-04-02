#######################################################.
# indicator_definitions_mod.R
# This module creates the layout for the indicator definitions tab
# It uses the technical document to create a table with indicators, 
# that when clicked on, expands metadata from the techdoc for each indicator
#######################################################.

# TO DO: fix broken links in the technical document in supporting information and related publications column
# temporarily removed from this table since so many of the links refer to old ISD website/ beta PHS website

#######################################################.
## MODULE UI ----
#######################################################.

# id = unique id 

definitions_tab_UI <- function(id) {
  ns <- NS(id) # namespace
  tagList(
    # title 
    h2("Indicator definitions and schedule"),
    # instructions
    p("Use the filters below to search for indicators by profile and/or geography level. 
    Alternatively you can search using key words (e.g. 'cancer'). You can then click on 
    an indicator in the search results table to view metadata."),
    p("To view technical information and updates schedule for all indicators at once, use the download button below."),
    # download button
    downloadButton(ns("techdoc_download"), label = "Download as CSV", class = "btn-sm btn-download"),
    hr(),
    # filters
    layout_columns(
      # profile filter 
      selectizeInput(ns("profile_search"), 
                     label = "Filter by profile", 
                     multiple = TRUE,
                     choices = setdiff(active_profiles, "All indicators"),
                     options = list(placeholder = 'Select a profile')
      ),
      # geography level filter 
      selectizeInput(ns("geo_search"), 
                     label = "Filter by geography level",
                     choices = setdiff(areatype_list, "Scotland"),
                     multiple = TRUE,
                     options = list(placeholder = 'Select geography level(s)'))
    ),
    # table of results
    reactableOutput(ns("search_results"))
  ) # close tagList
} # close module


#######################################################.
## MODULE SERVER ----
#######################################################.

definitions_tab_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ##################################.
      # Reactive data ----
      ##################################.
      
      # Reactive data to be used in table
      tech_info <- reactive({
        
        # if no profile has been selected, then select all indicators 
        if(is.null(input$profile_search)){
          x <- techdoc 
        } else {
          x <- techdoc |>
            filter(grepl(pluck(profiles_list, input$profile_search)$short_name, profile_domain))
        }
        
        
        # if no geography has been selected, then select all indicators
        # within selected profile (if any selected)
        if(is.null(input$geo_search)){
          x
        } else {
          x <- x |>
            filter(grepl(paste(input$geo_search, collapse="|"), available_geographies))
        }
        
        
        # arrange alphabetically by indicator name
        x <- x |>
          arrange(indicator_name) |>
          rowwise() |>
          mutate(across(c("supporting_information", "related_publications", "scotpho_web_link"), ~ markdown(.)))
        
      })
      
      ###################################.
      # Data table ----
      ###################################.
      
      
      # Display indicator search results in a table with expandable rows
      # Using reactive data from above
      output$search_results <- renderReactable({
        
        reactable(
          tech_info(), # reactive data created above
          searchable = TRUE, # include a global search bar
          defaultPageSize = 25, # set max number of rows per page
          rowStyle = list(cursor = "pointer"),
          onClick = "expand", # expand rows when clicked on
          highlight = TRUE, # changes row colour when user hovers over it
          
          # table theme 
          theme = reactableTheme(
            backgroundColor = 'white',
            borderWidth = '1px',
            borderColor = 'lightgrey',
            headerStyle = list(backgroundColor = "hsl(205, 93%, 16%)", color = "white"),
            searchInputStyle = list( borderColor = '#cccccc',  width = "100%")),
          
          # language options
          language = reactableLang(
            searchPlaceholder = "Type to search for an indicator", # placeholder text for indicator search bar
            noData = "No indicators found for your selected profile and/or geography level", # text to display in table when no results to display
            pageInfo = "{rowStart}\u2013{rowEnd} of {rows} indicators" # text to display in table footer
          ),
          
          # Customise columns in the table 
          columns = list(
            
            # indicator column 
            indicator_name = colDef(name = "Indicator search results", minWidth = 350, html = T,
                                    
                                    # Technical information to display when row expanded
                                    # ${rowInfo.values['column name'] returns the value in that column for the particular row of data the user has expanded
                                    # Note this can be done in R without using any JS however it runs significantly slower with this amount of metadata.
                                    details = JS(" function(rowInfo) {
                     return  `
                     
                     
                     <br>
                     <div>
                     <h4 class = 'metadata-header'>Indicator definition</h4>
                     ${rowInfo.values['indicator_definition']}
                     </div>
                     
                     <br>
                     
                     <div>
                     <h4 class = 'metadata-header'>Profiles</h4>
                     ${rowInfo.values['profile']}
                     </div>
                     
                     <br>
                     <div>
                     <h4 class = 'metadata-header'>Data source</h4>
                     ${rowInfo.values['data_source']}
                     </div>
                     
                     <br>
                     <div>
                     <h4 class = 'metadata-header'>Numerator</h4>
                     ${rowInfo.values['numerator']}
                     </div>
                     
                     <br>
                     <div>
                     <h4 class = 'metadata-header'>Denominator</h4>
                     ${rowInfo.values['denominator']}
                     </div>
                     
                     <br>
                     <h4 class = 'metadata-header'>Methodology</h4>
                     
                     <table class = 'methodology-table' style='width:100%; table-layout: fixed'>
                     <tr>
                     <th>Confidence interval</th>
                     <th>Rounding</th>
                     <th>Disclosure control</th>
                     <th>Measure</th>
                     </tr>
                     <tr>
                     <td>${rowInfo.values['confidence_interval_method']}</td>
                     <td>${rowInfo.values['rounding']}</td>
                     <td>${rowInfo.values['disclosure_control']}</td>
                     <td>${rowInfo.values['type_definition']}</td>
                     </tr>
                     </table>
                     
                     <br>
                     
                     
                     <table class = 'methodology-table' style='width:100%; table-layout: fixed'>
                     <tr>
                     <th>Age group</th>
                     <th>Sex</th>
                     <th>Available geographies</th>
                     <th>Inequalities</th>
                     </tr>
                     <tr>
                     <td>${rowInfo.values['age_group']}</td>
                     <td>${rowInfo.values['sex']}</td>
                     <td>${rowInfo.values['available_geographies']}</td>
                     <td>${rowInfo.values['label_inequality']}</td>
                     </tr>
                     </table>
                     
                     <br>
                     <div>
                     <h4 class = 'metadata-header'>Inclusion rationale</h4>
                     ${rowInfo.values['inclusion_rationale']}
                     </div>
                     
                     <br>
                     <div>
                     <h4 class = 'metadata-header'>Notes and caveats</h4>
                     ${rowInfo.values['notes_caveats']}
                     </div>
                     
                     <br>
                     <div>
                     <h4 class = 'metadata-header'>ScotPHO website links</h4>
                     ${rowInfo.values['scotpho_web_link']}
                     </div>
                     
                     <br>
                     <div>
                     <h4 class = 'metadata-header'>Related publications</h4>
                     ${rowInfo.values['related_publications']}
                     </div>
                     
                     <br>
                     <div>
                     <h4 class = 'metadata-header'>Supporting information</h4>
                     ${rowInfo.values['supporting_information']}
                     </div>`}")),
            
            # 2 x update dates columns 
            last_updated = colDef(name = "Last updated", sortable = FALSE),
            next_update = colDef(defaultSortOrder = "desc", sortable = FALSE, name = "Next update (provisional)"),
            
            # The rest of the columns - set to hidden
            # these columns will not be visible in the table 
            # however most of them are used to populate the information when a row is expanded
            profile = colDef(show = F),
            domain = colDef(show = F),
            trends_from = colDef(show = F),
            indicator_definition = colDef(show = F),
            data_source = colDef(show = F),
            source_details = colDef(show = F),
            related_publications = colDef(show = F),
            supporting_information = colDef(show = F),
            inclusion_rationale = colDef(show = F),
            numerator = colDef(show = F),
            available_geographies = colDef(show = F),
            disclosure_control = colDef(show = F),
            confidence_interval_method = colDef(show = F),
            type_definition = colDef(show = F),
            notes_caveats = colDef(show = F),
            denominator = colDef(show = F),
            diagnostic_code_position = colDef(show = F),
            rounding = colDef(show = F),
            aggregation = colDef(show = F),
            ind_id = colDef(show = F),
            age_group = colDef(show = F),
            sex = colDef(show = F),
            update_frequency = colDef(show = F),
            label_inequality = colDef(show = F),
            supression =  colDef(show = F),
            supress_less_than =  colDef(show = F),
            type_id =  colDef(show = F),
            interpret =  colDef(show = F),
            analyst_notes = colDef(show = F),
            profile_domain1 = colDef(show = F),
            profile_domain2 = colDef(show = F),
            profile_domain3 = colDef(show = F),
            profile_domain = colDef(show = F),
            active = colDef(show = F),
            covid_impact = colDef(show = F),
            days_since_update = colDef(show = F),
            source_last_updated = colDef(show = F),
            source_next_update = colDef(show = F),
            year_type = colDef(show = F),
            indicator_author = colDef(show = F),
            interpretation = colDef(show = F),
            scotpho_profiles = colDef(show = F),
            pop_group_splits = colDef(show = F),
            scotpho_web_link = colDef(show = F)
          ) # close columns list
        ) # close reactable 
      }) # close render reactable
      
      
      #################################.
      # Downloads ----
      #################################.
      
      # Download document when button clicked
      output$techdoc_download <- downloadHandler(
        
        filename ="scotpho_indicator_definitions.csv",
        content = function(file) {
          write.csv(techdoc |>
                      select(-c(indicator_author, analyst_notes, source_last_updated, source_next_update, covid_impact, active, ind_id, days_since_update)),
                    file, row.names=FALSE) }
      )
      
      
      
    })} # close server module function 

##################################.
# EXAMPLE USAGE
# uncomment the code below in order to see how this module works
# note you will require the 'techdoc' parquet file and the 'profiles_list' (both in main app global script) for this to work
# therefore recommended that you run the main app first before running this example app
# so that these 2 items become available in your global environment
###################################.

# packages
# library(shiny)
# library(shinyjs)
# library(bslib)
# 
# # css for table
# theme <- bs_theme(version = 5) |>
#   bs_add_rules(list(
#     ".metadata-header {font-weight: 600;}", # make headers in expandable rows bolder
#     ".rt-tr-details {padding: 24px; box-shadow: inset 0 1px 3px #dbdbdb; background: #FDFDFC ;}")) # make expanded panel light grey with slight shadow
# 
# # ui
# ui <- fluidPage(
#   useShinyjs(),
#   theme = theme,
#   definitions_tab_UI(id = "metadata") # insert module in ui - assigning it a unique id
# )
# 
# server <- function(input, output, session) {
# 
#   definitions_tab_Server(id = "metadata") # call module in server - using matching id assigned in the ui
# 
# }
# 
# # run app
# shinyApp(ui, server)
