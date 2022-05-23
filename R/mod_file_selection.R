#' file_selection UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_file_selection_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidRow(
      column(width = 12,
             # Action button that opens file selector
             # TODO: eventually this will become a toggle similar to data_curator dashboard
             
             shinydashboard::box(
               width = NULL,
               title = "Select Files",
               
               actionButton(ns("getfiles_btn"), "Get Files"),
               
               DT::DTOutput(ns("manifest_tbl"))
               # actionButton(ns("file_button"), "Turn on File Level View"),
               # 
               # # hide shinydashboard::box on app load
               # shinyjs::hidden(
               #   
               #   div(id = ns("wrapper"),
               #       #uiOutput(ns('tabs')))
               #       DT::DTOutput(ns("manifest_tbl")))
               #   
               #   )
               )
             )
      )
    )
  }
    
#' file_selection Server Functions
#'
#' @noRd 
mod_file_selection_server <- function(id, dataset){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # HARDCODED VARIABLES ###################################################################################
    
    # get token from renviron
    # TODO: repetitive figure out how to store or pass as variable
    
    schematic_token <- Sys.getenv("schematicToken")
    
    # manually set asset view
    # TODO: repetitive figure out how to store or pass asset_view
    
    asset_view <- "syn20446927"
    
    # SHINY JS TOGGLE ON BUTTON CLICK #######################################################################
    # hide/show toggle for wrapper
    
    # observeEvent(input$file_button, {
    #   shinyjs::toggle("wrapper")
    # })

    # TODO: similar to data curator will want to initiate file selector data pull
    #       after the toggle is selected
    
    
    # ON CLICK GET MANIFEST FOR SELECTED DATASET ############################################################
    # FIXME: Button click data flow is not ideal
    # Click of show file level view must happen AFTER dataset is selected
    # If you have clicked show file level view and displayed a dataset, but want to
    # change that dataset you need to click "Show file level view" again
    
    manifest <- eventReactive(input$getfiles_btn, {
      ds <- dataset()
      manifest_download_to_df(asset_view = asset_view,
                              dataset_id = ds$id,
                              input_token = schematic_token)
      
    })
    
    # DISPLAY MANIFEST AS TABLE #############################################################################
    
    output$manifest_tbl <- DT::renderDataTable({
      DT::datatable(manifest(),
                    option = list(scrollY = 500,
                                  scrollX = TRUE,
                                  scrollCollapse = TRUE,
                                  bPaginate = FALSE,
                                  dom = "t"),
                    filter = list(position = 'top', clear = TRUE))
    })
    
    # return a list containing the downloaded manifest and rows selected
    
    return(list(
      manifest = reactive({ manifest() }),
      selected_rows = reactive({ input$manifest_tbl_rows_selected })
    ))
    
    # ARCHIVE ###############################################################################################
    
    # Right now only allowing a single selection so this is unnecessary
    # TODO : Enable multiple datasets to be selected
    #        Display each corresponding manifest in a tab
    # # render tabbox dynamically
    # # create a tab for each dataset selected in mod_dataset_selection
    # output$tabs <- renderUI({
    #   ds <- dataset()
    #   nTabs <- nrow(ds)
    #   myTabs <- lapply(1:nTabs, function(x) {
    #     tabPanel(title = ds[x, "Dataset"],
    #              output$dataset_selection <- DT::renderDT({
    #                   ds[x,]
    #                 }))
    #     
    #     
    #   })
    #   do.call(shinydashboard::tabBox, myTabs)
    # })
    
  })
}