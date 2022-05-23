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
    
    fluidRow(
      column(width = 12,
             shinydashboard::box(
               title = "Select Project",
               width = NULL,
               
               # Project dropdown
               uiOutput(ns("project_selector")),
               
               # Button to initiate project selection
               actionButton(ns("select_project_btn"),
                            "Submit"),
               br()
               )
             )
      ),
    
    ## SELECT DATASET BOX  ####################################################
    
    fluidRow(
      column(width = 12,
             shinydashboard::box(
               title = "Select Dataset",
               width = NULL,
               
               # Table of storage project datasets 
               DT::DTOutput(ns("dataset_tbl")),
               
               br(),
               
               # Button to initiate dataset selection
               actionButton(ns("select_dataset_btn"), "Select Dataset(s)"),
               
               br()
               )
             )
      )
    )
  }
    
#' dataset_selection2 Server Functions
#'
#' @noRd 
mod_dataset_selection_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # initialize object that contains reactive values
    rv <- reactiveValues()
    
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

    # COMMENT OUT FOR TESTING
    storage_projects_list <- storage_projects(asset_view = asset_view,
                                              input_token = schematic_token)

    # convert to dataframe
    storage_projects_df <- list_to_dataframe(list = storage_projects_list,
                                             col_names = c("id", "name"))

    # reorder and add to reactive values
    rv$storage_projects_df <- dplyr::select(storage_projects_df, name, id)


    # DROP DOWN LISTING STORAGE PROJECTS ####################################################################

    output$project_selector <- renderUI({
      selectInput(session$ns("selected_projects"),
                  label = "Select Project",
                  choices = rv$storage_projects_df$name)
    })
    # 
    # ## DUMMY DATA FOR TESTING
    # rv$storage_projects_df <- data.frame(name = "lw-test", id ="syn30028964")
    # 
    # output$project_selector <- renderUI({
    #   selectInput(session$ns("selected_projects"),
    #               label = "Select Project",
    #               choices = rv$storage_projects_df$name)
    # })
    
    ## ON CLICK DISPLAY STORAGE PROJECT DATASETS  ###########################################################
    # on button click call storage_project_datasets using selected project ID
    
    observeEvent(input$select_project_btn, {
      
      req(input$selected_projects)

      # subset storage project df by selected project
      # TODO: allow multiple project selection?

      selected_project_df <- rv$storage_projects_df[grepl(input$selected_projects, rv$storage_projects_df$name), ]

      # call schematic API - get datasets for selected storage project
      
      ### COMMENT OUT FOR TESTING
      dataset_list <- storage_project_datasets(asset_view = asset_view,
                                               project_id = selected_project_df$id,
                                               input_token = schematic_token)

      # schematic outputs a list
      # parse into a dataframe

      rv$dataset_df <- list_to_dataframe(list = dataset_list,
                                      col_names = c("id", "name"))

      rv$dataset_df <- dplyr::select(rv$dataset_df, name, id)


      # render data table with scroll bar, no pagination, and filtering
      output$dataset_tbl <- DT::renderDataTable({
        DT::datatable(rv$dataset_df,
                      selection = "single",
                      option = list(scrollY = 500,
                                    scrollCollapse = TRUE,
                                    bPaginate = FALSE,
                                    dom = "t"),
                      filter = list(position = 'top', clear = TRUE))
      })
      
      # ## TESTING DUMMY DATA TO SKIP API CALL
      # rv$dataset_df <- data.frame(name = "HTAN_CenterA_Demographics", id = "syn30028964")
      # 
      # output$dataset_tbl <- DT::renderDataTable({
      #   DT::datatable(rv$dataset_df,
      #                 selection = "single",
      #                 option = list(scrollY = 500,
      #                               scrollCollapse = TRUE,
      #                               bPaginate = FALSE,
      #                               dom = "t"),
      #                 filter = list(position = 'top', clear = TRUE))
      # })
        
    })

    ## ON BUTTON CLICK SUBMIT DATASET SELECTION #############################################################

    # when button is pushed
    # if no rows selected: show no dataset selected
    # if rows selected return the selection
    eventReactive(input$select_dataset_btn, {
      selected <- input$dataset_tbl_rows_selected
      if (length(selected) == 0) {
        showNotification("No Dataset Selected")
        return(NULL)
      } else{
        return(rv$dataset_df[selected,])
        }
      })
 
  })
}
    
## To be copied in the UI
# mod_dataset_selection2_ui("dataset_selection2_1")
    
## To be copied in the server
# mod_dataset_selection2_server("dataset_selection2_1")
