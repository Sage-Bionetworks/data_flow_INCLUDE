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
    
    # Action button that opens file selector
    # TODO: eventually this will become a toggle similar to data_curator dashboard
    actionButton(ns("file_button"), "Turn on File Level View"),
    
    # hide shinydashboard::box on app load
    shinyjs::hidden(
      
      div(id = ns("wrapper"),
          
          shinydashboard::box(
            id = ns("box"),
            title = "Select Files",
            width = 6,
            
            # table
            DT::DTOutput(ns("tbl")),
            
            # selected datasets from mod_dataset_selection as text
            verbatimTextOutput(ns("dataset_selection")),
            
            # selected files
            verbatimTextOutput(ns("file_selection")),
            
            br(),
            
            # action button to select files
            actionButton(ns("button"), "Select file(s)"))
      ))
    )
}
    
#' file_selection Server Functions
#'
#' @noRd 
mod_file_selection_server <- function(id, dataset){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # hide/show toggle for wrapper
    observeEvent(input$file_button, {
      shinyjs::toggle("wrapper")
    })
    
    # TODO: similar to data curator will want to initiate file selector data pull
    #       after the toggle is selected
    
    # output datatable
    output$tbl <- DT::renderDT({
      
      # create tst data
      datasets_col <- paste0(rep("file", 100), "_", seq(1:100))
      status_col <- sample(c("Quarantine", "Release 1"), 100, replace = TRUE)
      df <- data.frame(Dataset = datasets_col, Status = as.factor(status_col))
      
      # create DT with y scroll bar, no pagination, no search bar, top filter
      DT::datatable(df, 
                    option = list(scrollY = 500,
                                  scrollCollapse = TRUE,
                                  bPaginate = FALSE,
                                  dom = "t"),
                    filter = list(position = 'top', clear = TRUE))
    })
    
    # on button click display text
    observeEvent(input$button, {
      s <- input$tbl_rows_selected
      if (length(s) == 0) {
        output$file_selection <- renderPrint({
          cat("no files selected")}) 
      } else {
        output$file_selection <- renderPrint({
          s
        })
      }
    })
    
    output$dataset_selection <- renderPrint({
      dataset()
    })
    
  })
}