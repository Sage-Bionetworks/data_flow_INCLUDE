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
    # open up file level view?
    # TODO: eventually this will become a toggle similar to data_curator dashboard
    actionButton(ns("file_button"), "Turn on File Level View"),
    
    shinyjs::hidden(
      div(id = ns("wrapper"),
          shinydashboard::box(
            id = ns("box"),
            title = "Select Files",
            width = 6,
            DT::DTOutput(ns("tbl")),
            
            verbatimTextOutput(ns("dataset_selection")),
            
            # verbatimTextOutput(ns("dataset_selection")),
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
    
    observeEvent(input$file_button, {
      shinyjs::toggle("wrapper")
    })
    
    
  })
}