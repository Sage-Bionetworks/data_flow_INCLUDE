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
    shinydashboard::box(
      title = "Select Files",
      width = 6,
      DT::DTOutput(ns("tbl")),
      verbatimTextOutput(ns("tbl_selection"))),
    wellPanel(
      h2("Wellpanel"),
      actionButton(ns("button"), label = "show / hide")
    )
  )
}
    
#' file_selection Server Functions
#'
#' @noRd 
mod_file_selection_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$tbl <- DT::renderDT({
      datasets_col <- paste0(rep("file", 10), "_", seq(1:10))
      status_col <- sample(c("Quarantine", "Release 1"), 10, replace = TRUE)
      data.frame(Dataset = datasets_col, Status = status_col)
    })
    
    output$tbl_selection <- renderPrint({
      s = input$tbl_rows_selected
      if (length(s)) {
        cat("These rows were selected:\n\n")
        cat(s, sep = ", ")
      }
    })
    
    ## observe the button being pressed
    observeEvent(input$button, {
      
      if(input$button %% 2 == 1){
        shinyjs::hide(id = "myBox")
      }else{
        shinyjs::show(id = "myBox")
      }
    })
    
  })
}
    
## To be copied in the UI
# mod_file_selection_ui("file_selection_ui_1")
    
## To be copied in the server
# mod_file_selection_server("file_selection_ui_1")
