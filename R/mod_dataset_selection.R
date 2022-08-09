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
    
    ## SELECT DATASET BOX  ####################################################
    
    fluidRow(
      waiter::useWaiter(),
      column(width = 12,
             div(
               id = ns("select_dataset_wrapper"),
               
               shinydashboard::box(
                 title = "Select Dataset",
                 width = NULL,
                 
                 # Table of storage project datasets
                 DT::DTOutput(ns("dataset_tbl")),

                 br(),

                 # Button to initiate dataset selection
                 actionButton(ns("submit_btn"), "Submit"),
                 
                 br()
                 )
               )
             )
      )
    )
  }
    
#' dataset_selection2 Server Functions
#'
#' @noRd 
mod_dataset_selection_server <- function(id,
                                         storage_project_df,
                                         asset_view,
                                         input_token) {
  
  moduleServer( id, function(input, output, session) {
    
    ns <- session$ns
    
    # initialize waiter
    w <- Waiter$new(id = ns("select_dataset_wrapper"),
                    html = div(
                      style="color:#424874;",
                      waiter::spin_3(),
                      h4("Retrieving datasets...")),
                    color = transparent(.8))

    ## DISPLAY STORAGE PROJECT DATASETS  ###########################################################
    # call schematic API - get datasets for selected storage project
        
    datasets <- reactive({
      
      # show waiter
      w$show()
      
      # on exit - hide waiter
      on.exit({
        w$hide()
      })
      
       dataset_list <- storage_project_datasets(asset_view = asset_view,
                               project_id = storage_project_df()$id,
                               input_token = input_token)
       
       dataset_df <- list_to_dataframe(list = dataset_list,
                                       col_names = c("id", "name"))
       
       dplyr::select(dataset_df, name, id)
    })
    

      # render data table with scroll bar, no pagination, and filtering
      output$dataset_tbl <- DT::renderDataTable({
        DT::datatable(datasets(),
                      selection = "multiple",
                      option = list(scrollY = 500,
                                    scrollCollapse = TRUE,
                                    bPaginate = FALSE,
                                    dom = "t"),
                      filter = list(position = 'top', clear = TRUE))
      })
      
      # SUBSET DATAFRAME 
      selected_datasets <- reactive({
        
        # get selected rows from datatable
        selected <- input$dataset_tbl_rows_selected
        
        # subset
        df <- datasets()
        df[selected,]
      })
      
      # RETURN DATA ON CLICK
      eventReactive(input$submit_btn, {
        
        return(selected_datasets())
        
      })
    })
}
 
    
## To be copied in the UI
# mod_dataset_selection2_ui("dataset_selection2_1")
    
## To be copied in the server
# mod_dataset_selection2_server("dataset_selection2_1")
