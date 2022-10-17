#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic
  
  # DEV STUFF ###########################################################################
  
  # SYNAPSE LOGIN
  # TODO: log in will eventually live in global.R/rely on config info
  reticulate::use_virtualenv(".venv/")
  synapseclient <- reticulate::import("synapseclient")
  syntab <- reticulate::import("synapseclient.table")
  syn <- synapseclient$Synapse()
  syn$login()
  
  # read in configs
  global_config <- jsonlite::read_json("inst/global.json")
  dash_config <- jsonlite::read_json("inst/datatable_dashboard_config.json")
  

  # DATASET DASH  #######################################################################
  
  # download data flow status manifest
  dfs_manifest <- manifest_download_to_df(asset_view = global_config$asset_view,
                                          dataset_id = global_config$manifest_dataset_id,
                                          input_token = global_config$schematic_token)
  
  dfs_manifest <- manifest_string_to_date(dfs_manifest)
  
  # create tabbed dashboard
  
  mod_tabbed_dashboard_server("tabbed_dashboard_1", 
                              reactive({dfs_manifest}),
                              jsonlite::read_json("inst/datatable_dashboard_config.json"))
  
  # DATASET DASH VIZ : DISTRIBUTIONS ####################################################
  
  mod_distribution_server(id = "distribution_contributor",
                          df = dfs_manifest,
                          group_by_var = "contributor",
                          title = NULL,
                          x_lab = "Contributor",
                          y_lab = "Number of Datasets",
                          fill = "#0d1c38")
  
  mod_distribution_server(id = "distribution_datatype",
                          df = dfs_manifest,
                          group_by_var = "dataset",
                          title = NULL,
                          x_lab = "Type of dataset",
                          y_lab = "Number of Datasets",
                          fill = "#0d1c38")

  # DATASET DASH VIZ : DISTRIBUTIONS ####################################################
  
  manifest_w_status <- reactive({
    
    # add some columns to manifest to make logic easier
    manifest <- dfs_manifest %>%
      dplyr::mutate(scheduled = !is.na(release_scheduled),
             no_embargo = is.na(embargo) || embargo < Sys.chmod(),
             past_due = !is.na(release_scheduled) && release_scheduled < Sys.Date())
    
    # generate status variable based on some logic that defines various data flow statuses
    status <- sapply(1:nrow(manifest), function(i) {
      row <- manifest[i, ]
      
      if (row$scheduled == FALSE) {
        status <- "not scheduled"
      } else if (row$no_embargo == FALSE || row$standard_compliance == FALSE) {
        status <- "quarantine"
      } else if (row$no_embargo == TRUE & row$standard_compliance == TRUE & row$released == FALSE) {
        status <- "quarantine (ready for release)"
      } else if (row$released == TRUE) {
        status <- "released"
      } else {
        NA
      }
      
      status
    })
    
    # add status to manifest
    manifest$data_flow_status <- status
    
    manifest
  })
  
  # wrangle data for stacked bar plot
  release_status_data <- reactive({
    
    release_status_data <- manifest_w_status() %>%
      dplyr::group_by(contributor) %>%
      dplyr::group_by(dataset, .add = TRUE) %>%
      dplyr::group_by(data_flow_status, .add = TRUE) %>%
      dplyr::tally()
    
    # reorder factors
    release_status_data$data_flow_status <- factor(release_status_data$data_flow_status, 
                                                   levels = c("released", "quarantine (ready for release)", "quarantine", "not scheduled"))
    
    release_status_data
  })
  
  # wrangle data for stacked bar plot (only scheduled)
  release_status_data_scheduled <- reactive({
    
    release_status_data()[release_status_data()$data_flow_status != "not scheduled",]
  })
  
  whichPlot <- reactiveVal(TRUE)
  
  observeEvent(input$toggle_stacked_bar, {
    whichPlot(!whichPlot())
  })
  
  which_release_data <- reactive({

    release_status_data <- manifest_w_status() %>%
      dplyr::group_by(contributor) %>%
      dplyr::group_by(dataset, .add = TRUE) %>%
      dplyr::group_by(data_flow_status, .add = TRUE) %>%
      dplyr::tally()
    
    # reorder factors
    release_status_data$data_flow_status <- factor(release_status_data$data_flow_status, 
                                                   levels = c("released", "quarantine (ready for release)", "quarantine", "not scheduled"))
    if (whichPlot() == FALSE) {
      release_status_data <- release_status_data[release_status_data$data_flow_status != "not scheduled",]
    }
    
    release_status_data
  })
  
  mod_stacked_bar_server(id = "stacked_bar_release_status",
                         df = which_release_data,
                         x_var = "contributor",
                         y_var = "n",
                         fill_var = "data_flow_status",
                         title = NULL,
                         x_lab = "Contributors",
                         y_lab = NULL,
                         colors = c("#085631", "#ffa500", "#a72a1e", "#3d3d3d"),
                         coord_flip = TRUE)
  
  # drop down for runners plot
  output$select_project_ui <- shiny::renderUI({
    
    contributors <- unique(manifest_w_status()$contributor)
    
    shiny::selectInput(inputId = "select_project_input",
                       label = NULL,
                       choices = contributors,
                       selectize = FALSE)
  })
  
  # wrangle data for stacked bar plot (runners)
  
  release_data_runners <- reactive({
    
    release_status_data <- manifest_w_status() %>%
      dplyr::filter(!is.na(release_scheduled)) %>%
      dplyr::filter(contributor == input$select_project_input) %>%
      dplyr::group_by(contributor) %>%
      dplyr::group_by(release_scheduled, .add = TRUE) %>%
      dplyr::group_by(data_flow_status, .add = TRUE) %>%
      dplyr::tally()
    
    release_status_data$data_flow_status <- factor(release_status_data$data_flow_status, 
                                                   levels = c("released", "quarantine (ready for release)", "quarantine"))
    
    release_status_data
  })
  
  
  mod_stacked_bar_server(id = "stacked_runners",
                         df = release_data_runners,
                         x_var = "release_scheduled",
                         y_var = "n",
                         fill_var = "data_flow_status",
                         title = NULL,
                         x_lab = "Release Dates",
                         y_lab = NULL,
                         x_line = Sys.Date(),
                         colors = c("#085631", "#ffa500", "#a72a1e"),
                         coord_flip = FALSE)

  # ADMINISTRATOR  #######################################################################
  
  # reactive value that holds dfs_manifest 
  rv_manifest <- reactiveVal(dfs_manifest)
  
  # STORAGE PROJECT SELECTION
  
  select_storage_project <- mod_select_storage_project_server(id = "select_storage_project_1",
                                                              asset_view = global_config$asset_view,
                                                              input_token = global_config$schematic_token)
  
  # DATASET SELECTION
  
  dataset_selection <- mod_dataset_selection_server(id = "dataset_selection_1",
                                                    storage_project_df = select_storage_project,
                                                    asset_view = global_config$asset_view,
                                                    input_token = global_config$schematic_token,
                                                    hidden_datasets = "DataFlowStatus")
  
  # UPDATE DATA FLOW STATUS SELECTIONS 
  updated_data_flow_status <- mod_update_data_flow_status_server("update_data_flow_status_1")
  
  
  # MODIFY MANIFEST
  modified_manifest <- reactive({
    req(updated_data_flow_status())
    
    update_dfs_manifest(dfs_manifest = rv_manifest(),
                        dfs_updates = updated_data_flow_status(),
                        selected_datasets_df = dataset_selection())
  })
  
  
   observeEvent(input$save_update, {
     rv_manifest(modified_manifest())
   })
  
  observeEvent(input$clear_update, {
    rv_manifest(dfs_manifest)
  })
  
  # PREP MANIFEST FOR SYNAPSE SUBMISSION
  
  manifest_submit <- reactive({
    
    manifest_date_to_string(modified_manifest())
  })
  
  # DISPLAY MANIFEST TO BE SUBMITTED
  
  # rearrange manifest so it's more readable
  admin_display_manifest <- reactive({
    rearrange_dataframe(manifest_submit(),
                        names(dash_config))
  })
  
  # get names of selected datasets
  selected_row_names <- reactive({
    dataset_selection()$name
    
  })
  
  mod_highlight_datatable_server("highlight_datatable_1",
                                admin_display_manifest,
                                selected_row_names,
                                "dataset_name")
  
  # SUBMIT MODEL TO SYNAPSE
  # make sure to submit using a manifest that has been run through date to string
  mod_submit_model_server("submit_model_1",
                          dfs_manifest = manifest_submit,
                          data_type = "DataFlow",
                          dataset_id = global_config$manifest_dataset_id,
                          manifest_dir = "./manifest",
                          input_token = global_config$schematic_token,
                          schema_url = global_config$schema_url)

}
