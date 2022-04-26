#' dataset_selection2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_dataset_selection_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    ## SELECT PROJECT BOX  ####################################################
    
    shinydashboard::box(
      title = "Select Project",
      width = 6,
      
      # Project dropdown
      uiOutput(ns("project_selector")),
      
      # Button to initiate project selection
      actionButton(ns("select_project_btn"),
                   "Submit"),
      br(),
      
    ),
    
    ## SELECT DATASET BOX  ####################################################
    
    shinydashboard::box(
      title = "Select Dataset",
      width = 6,
      
      # Table of storage project datasets 
      DT::DTOutput(ns("dataset_tbl")),
      
      br(),
      
      # Button to initiate dataset selection
      actionButton(ns("select_dataset_btn"), "Select Dataset(s)"),
      
      br()
    )
 
  )
}
    
#' dataset_selection2 Server Functions
#'
#' @noRd 
mod_dataset_selection_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # HARDCODED VARIABLES ###################################################################################
    
    # FIXME: Need to load data before app launches (like how DCA waites for data before loading)
    # Currently it takes the drop down a loooong time to render and appears to be missing.
    
    # get token from renviron
    # TODO: repetitive figure out how to store or pass as variable
    
    schematic_token <- Sys.getenv("schematicToken")
    
    # manually set asset view
    # TODO: repetitive figure out how to store or pass asset_view
    
    asset_view <- "syn20446927"
    
    # API CALL : GET STORAGE PROJECTS #######################################################################
    
    storage_projects_list <- storage_projects(asset_view = asset_view,
                                              input_token = schematic_token)
    
    # get names from list
    storage_projects_names <- purrr::map_chr(storage_projects_list, purrr::pluck(2))
    storage_projects_ids <- purrr::map_chr(storage_projects_list, purrr::pluck(1))
    storage_projects_df <- data.frame(name = storage_projects_names, 
                                      id = storage_projects_ids)

    
    # DROP DOWN LISTING STORAGE PROJECTS ####################################################################

    output$project_selector <- renderUI({
      selectInput(session$ns("selected_projects"),
                  label = "Select Project",
                  choices = storage_projects_df$name)
    })
    
    ## ON CLICK DISPLAY STORAGE PROJECT DATASETS  ###########################################################
    # on button click call storage_project_datasets using selected project ID
    
    observeEvent(input$select_project_btn, {

      # subset storage project df by selected project
      # TODO: allow multiple project selection?

      selected_project_df <- storage_projects_df[grepl(input$selected_projects, storage_projects_df$name), ]

      # call schematic API - get datasets for selected storage project

      dataset_list <- storage_project_datasets(asset_view = asset_view,
                                               project_id = selected_project_df$id,
                                               input_token = schematic_token)

      # schematic outputs a list
      # parse into a dataframe

      dataset_df <- data.frame(do.call(rbind, dataset_list))
      names(dataset_df) <- c("ids", "name")
      dataset_df <- dplyr::select(dataset_df, name, ids)
      

      # render data table with scroll bar, no pagination, and filtering
      output$dataset_tbl <- DT::renderDataTable({
        DT::datatable(dataset_df,
                      selection = "single",
                      option = list(scrollY = 500,
                                    scrollCollapse = TRUE,
                                    bPaginate = FALSE,
                                    dom = "t"),
                      filter = list(position = 'top', clear = TRUE))
      })
    })

    ## ON BUTTON CLICK SUBMIT DATASET SELECTION #############################################################

    # when button is pushed
    # if no rows selected: show no dataset selected
    # if rows selected return the selection
    eventReactive(input$select_dataset_btn, {
      s <- input$dataset_tbl_rows_selected
      if (length(s) == 0) {
        showNotification("No Dataset Selected")
        return(NULL)
      } else{
        return(dataset_df[s,])
        }
      })
 
  })
}
    
## To be copied in the UI
# mod_dataset_selection2_ui("dataset_selection2_1")
    
## To be copied in the server
# mod_dataset_selection2_server("dataset_selection2_1")
