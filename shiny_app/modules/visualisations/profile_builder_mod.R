
# What should module to return - filtered datasets or is the lookup and button enough?
# Maybe lookup can be updated only when button is pressed, then all that needs returned is the lookup? 


# Bookmarking - exclusions and inclusions related to custom profile.

# 1. pop up modal to let user know that switching geography when custom profile is selected may result in some indicators
# not appearing/some splits not appearing as profile was built based on data availability for the selected geography.

# 2. Rework UI i.e. removing accordion and seperation of builder and summary of selections
# Think about anything else user may want in their summary of selections?

# 3. Will users want to be able to have a custom title for their profile?




profile_builder_UI <- function(id) {
  ns <- NS(id)
  accordion(
    id = ns("byo_accordion"), # accordion id
    #open = FALSE, # closed by default
    open = TRUE,
    accordion_panel(
      title = "Custom profile",
      value = ns("byo_panel"), # panel id
      # layout 3 cards in accordion
      layout_column_wrap(
        width = 1/3,
        
        # Card 1 - Define domains
        card(
          fill = FALSE,
          card_header("Step 1: Define domains", class = "bg-phs-magenta text-white"),
          helpText("Create your domain names. These are the groups your indicators will be organised into."),
          input_submit_textarea(
            id = ns("domain_name"),
            label = "Domain name"
          ),
          checkboxInput(
            inputId = ns("pre_populate"),
            label = tooltip(
              trigger = list(
                "Pre-populate with minimum core indicator set",
                bs_icon("info-circle")
              ),
              "Info about core indicator set"
            )
          ), 
          # empty div for adding domain removal buttons each time a domain name is submitted in input above
          div(id = ns("domains_remove_btns_div"))
        ), # close card 1
        
        # Card 2 - Assign indicators to domains
        # (disabled by default until atleast 1 domain created in step 1)
        disabled(
          card(
            fill = FALSE,
            id = ns("card_2"), # card 2
            card_header("Step 2: Choose and assign indicators", class = "bg-phs-magenta text-white"),
            helpText("Select indicators, then assign them to a domain."),
            div(
              # indicator filter
              selectizeInput(
                inputId = ns("ind_filter"),
                label = "Select indicator(s)",
                multiple = TRUE,
                choices = NULL, # choices set in server depending on selected geography
                options = list(placeholder = "Type to search..."),
                width = "100%"
              ),
              # domain filter
              selectizeInput(
                inputId = ns("domain_filter"),
                label = "Select domain",
                choices = NULL, # choices set in server using answers submitted via 'domain_name' input in step 1
                width = "100%"
              ),
              # assign button
              actionButton(
                inputId = ns("assign_inds_btn"),
                label = "Assign",
                class = "btn-sm input-group-btn" # set class for styling i.e. small button underneath input
              )
            )
          ) # close card 2
        ), # close disabled div
        
        
        # Card 3 - Review and create profile
        # (disabled by default until atleast 1 domain with indicators)
        disabled(
          card(
            id = ns("card_3"), # id for enabling/disabling card
            card_header("Step 3: Review selections and build profile", class = "bg-phs-magenta text-white"),
            helpText("Review indicators by domain. Then, press the button to create your profile."),
            accordion(id = ns("domain_accordion")), # create empty accordion - panels added/updated/removed via server
            card_footer(
              toolbar(
                align = "right",
                input_task_button(
                  id = ns("build_profile_btn"),
                  label = "Build profile",
                  icon = bs_icon("arrow-right")
                )
              )
            )
          ) # close card 3
        ) # close disabled div
      ) # close layout column wrap
    ) # close accordion panel
  ) # close accordion
}

profile_builder_Server <- function(id, geo_selections, selected_profile) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Important - different functions handle namespacing differently
      # This means that some ids need wrapped in ns() in the server too
      ns <- session$ns
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Reactive values ----
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # for storing custom domain/indicator selections
      custom_profile <- reactiveVal(list())
      
      # for storing a numeric id the next created domain should be assigned
      # set to 1 by default so the first domain added has id no. 1
      next_domain_id <- reactiveVal(1)
      
      
      
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Reactive Data ----
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # get available indicator names/ind_ids for the selected geography
      byo_indicators <- reactive({
        #req(selected_profile() == "Custom profile") # only run code if BYO profile selected
        
        main_dataset |>
          filter(areatype == geo_selections()$areatype & areaname == geo_selections()$areaname) |>
          select(ind_id, indicator) |>
          unique()
      })
      
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Enable/disable profile build steps ----
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # enable 'Step 2' card if atleast 1 domain name has been submitted
      # and saved in the 'profile_rv' object
      observe({
        toggleState(id = "card_2", condition = length(custom_profile()) > 0)
      })
      
      # enable 'Step 3' card if atleast 1 indicator has been assigned to a domain
      observe({
        toggleState(id = "card_3", condition = length(map(custom_profile(), "indicators") |> flatten_chr()) > 0)
      })
      
      
      
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Code to run when a new domain name is submitted
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # When user updates a new domain name, do the following:
      observeEvent(input$domain_name, {
        
        # get custom profile rv object as it currently stands
        current_profile <- custom_profile()
        
        # get new domain name submitted (+ remove any whitespace from start/end)
        new_domain_name <- trimws(input$domain_name)
        
        # get the number id to assign to the domain
        new_domain_id <- as.character(next_domain_id())
        
        # create new list with details about the new domain
        # e.g. "1" <- list(name = "My new domain", indicators = character(0))
        current_profile[[new_domain_id]] <- list(
          name = new_domain_name,
          indicators = character(0) # ready to start tracking indicators added to domain
        )
        
        # update rv object to include new domain list
        custom_profile(current_profile)
        
        # update domain id rv w/ the next id to use
        next_domain_id(as.numeric(new_domain_id) + 1)
        
        # Insert a button to be added to card 1 for removing domain just created
        # Button has onclick logic built into it - when the button is clicked an input
        # called 'remove_domain' is updated with the id of the domain to be removed
        # e.g. whe button clicked input$remove_domain == "1"
        # Note this input doesn't exist anywhere in the UI - it's just created here in server
        insertUI(
          selector = paste0("#", ns("domains_remove_btns_div")),
          ui = actionButton(
            inputId = ns(paste0("d_", new_domain_id, "_btn")), # e.g. "d_1_btn"
            label = tagList(new_domain_name, bs_icon("x")),
            class = "btn-sm", # small button
            onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", ns("remove_domain"), new_domain_id)
          )
        )
        
        # add domain accordion panel to step 3 card
        accordion_panel_insert(
          id = "domain_accordion",
          panel = accordion_panel(
            title = new_domain_name,
            value = ns(paste0("d",new_domain_id, "_panel")), # e.g. "d_1_panel"
            "No indicators assigned"
          )
        )
        
      })
      
      # update domain filter options in card 2 when domains added/removed
      observeEvent(custom_profile(),{
        updateSelectizeInput(
          inputId = "domain_filter",
          # extract domain ids and names from custom_profile rv
          # the user will see the domain name but the input returns the domain id
          choices = setNames(names(custom_profile()), map_chr(custom_profile(), "name"))
        )
      }, ignoreInit = TRUE)
      
      
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Code to run when domain is to be removed ------
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # When using removes a domain name (via button click) do the following:
      observeEvent(input$remove_domain, {
        
        # get custom profile rv object
        current_profile <- custom_profile()
        
        # get numeric id of input to be removed
        domain_remove_id <- as.character(input$remove_domain)
        
        # remove domain from rv
        current_profile[[domain_remove_id]] <- NULL
        custom_profile(current_profile)
        
        # remove associated domain panel
        accordion_panel_remove(
          id = "domain_accordion",
          target = ns(paste0("d", domain_remove_id, "_panel")) # e.g. "d_1_panel"
        )
        
        # remove associted button (i.e. the one just clicked)
        removeUI(selector = paste0("#", ns(paste0("d_", domain_remove_id, "_btn"))))  # e.g "d_1_btn"
      })
      
      
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Code to run when user assigns indicators to a domain ------
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      observeEvent(input$assign_inds_btn, {
        
        # get current snapshot of the custom_profle rv object
        current_profile <- custom_profile()
        
        # combine new and pre-existing domain indicators
        inds <- c(input$ind_filter, current_profile[[input$domain_filter]]$indicators)
        
        # amend the relevant domain list by adding the new indicators
        # assigned with any previously indicators already aligned/saved in the rv
        current_profile[[input$domain_filter]]$indicators <- inds
        
        # update the rv object
        custom_profile(current_profile)
        
        # get domain id
        domain_id = input$domain_filter
        
        # update the relevant domain accordion
        accordion_panel_update(
          id = "domain_accordion",
          target = ns(paste0("d", input$domain_filter, "_panel")), # e.g. "d_1_panel"
          # accordion_body(id = ns(paste0(d_id, "_panel")), domain_id = d_id, indicators = inds)
          
          # ul = unordered list
          tags$ul(
            map(inds, function(ind_id){
              # li = list item
              tags$li(
                techdoc$indicator_name[techdoc$ind_id == ind_id],
                actionLink(
                  inputId = ns(paste0("remove_", domain_id, "_", ind_id)), # e.g. "remove_1_19078"
                  label = NULL,
                  icon = bs_icon("x-circle"),
                  # returns domain id and ind_id to remove, separated by "|"
                  # e.g. input$remove_indicator == "1|19078"
                  onclick = sprintf(
                    "Shiny.setInputValue(
                  '%s',
                  '%s|%s',
                  {priority: 'event'}
                )",
                    ns("remove_indicator"),
                    domain_id,
                    ind_id
                  )
                )
              )
            })
          )
        )
        
      })
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Code to run when an indicator is to be removed from a domain -----
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      observeEvent(input$remove_indicator, {
        
        # get result of input$remove indicator e.g. "1|10893"
        parts <- strsplit(input$remove_indicator, "\\|")[[1]] # split in half
        domain <- parts[1] # domain id
        ind_delete <- parts[2] # ind id
        
        # get copy current custom_profile rv object
        current_profile <- custom_profile()
        
        # remove indicator from the relevant domain list
        current_profile[[domain]]$indicators <- setdiff(current_profile[[domain]]$indicators, ind_delete)
        custom_profile(current_profile)
        
        # remove indicator from accordion panel
        accordion_panel_update(
          id = "domain_accordion",
          target = ns(paste0("d", domain, "_panel")),
          #accordion_body(id = ns(paste0(domain, "_panel")), domain_id = domain, indicators = current_profile[[domain]]$indicators)
          
          # ul = unordered list
          tags$ul(
            map(current_profile[[domain]]$indicators, function(ind_id){
              # li = list item
              tags$li(
                techdoc$indicator_name[techdoc$ind_id == ind_id],
                actionLink(
                  inputId = ns(paste0("remove_", domain, "_", ind_id)), # e.g. "remove_1_19078"
                  label = NULL,
                  icon = bs_icon("x-circle"),
                  # returns domain id and ind_id to remove, separated by "|"
                  # e.g. input$remove_indicator == "1|19078"
                  onclick = sprintf(
                    "Shiny.setInputValue(
                  '%s',
                  '%s|%s',
                  {priority: 'event'}
                )",
                    ns("remove_indicator"),
                    domain,
                    ind_id
                  )
                )
              )
            })
          )
          
        )
        
        
      })
      
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Code to run in response to indicators being added OR removed from a domain ----
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # remove assigned indicators from indicator choices
      # (indicator can only be assigned to 1 domain at a time)
      observeEvent(custom_profile(), {
        req(length(custom_profile()) > 0)
        
        # get ind_ids that are already assigned (i.e. already stored in custom profile rv)
        assigned_inds <- map(custom_profile(), "indicators") |> flatten_chr()
        
        # filter them out of the reactive df storing available
        # indicators for the selected geography
        unassigned_inds <- byo_indicators() |>
          filter(!ind_id %in% assigned_inds)
        
        # update the indicator filter choices
        updateSelectizeInput(
          inputId = "ind_filter",
          choices = setNames(unassigned_inds$ind_id, unassigned_inds$indicator)
        )
      })
      
      
      # convert the custom_profile rv object into  a dataframe that can be used
      # as a lookup for filtering the indicator datasets
      custom_profile_lookup <- reactive({
        req(length(custom_profile()) > 0)
        imap_dfr(custom_profile(), ~ {
          tibble(
            domain = .x$name, # create domain column
            ind_id = as.numeric(.x$indicators) # create ind_id column
          )
        })
      })
      

      r_main_dataset <- eventReactive(input$build_profile_btn, {
        main_dataset |>
          inner_join(custom_profile_lookup(), by = "ind_id")
      })

      r_simd_dataset <- eventReactive(input$build_profile_btn, {
        simd_dataset |>
          filter(areatype == geo_selections()$areatype & areaname == geo_selections()$areaname) |>
          inner_join(custom_profile_lookup(), by = "ind_id")
      })
      
      r_popgroup_dataset <- eventReactive(input$build_profile_btn, {
        popgroup_dataset |>
          filter(areatype == geo_selections()$areatype & areaname == geo_selections()$areaname) |>
          inner_join(custom_profile_lookup(), by = "ind_id")
      })
      


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # to return from this module ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      return(
        list(
          lookup = reactive({custom_profile_lookup()}),
          build_profile_btn_clicked = reactive({input$build_profile_btn}),
          main_dataset = reactive({r_main_dataset()}), # the custom profile lookup with details about domain/indicators
          simd_dataset = reactive({r_simd_dataset()}), # the build profile button value for tracking when button is clicked
          popgroup_dataset = reactive({r_popgroup_dataset()})
        )
      )

    }
  )
}
