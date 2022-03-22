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
          
          # shinydashboard::box(
          #   id = ns("box"),
          #   title = "Select Files",
          #   width = 6,
          #   
          #   # table
          #   DT::DTOutput(ns("tbl")),
          #   
          #   # selected datasets from mod_dataset_selection as text
          #   verbatimTextOutput(ns("dataset_selection")),
          #   
          #   # selected files
          #   verbatimTextOutput(ns("file_selection")),
          #   
          #   br(),
          #   
          #   # action button to select files
          #   actionButton(ns("button"), "Select file(s)")
          #   )
          
          # shinydashboard::tabBox(
          #   title = "File Selector",
          #   # The id lets us use input$tabset1 on the server to find the current tab
          #   id = "tabset", height = "500px",
          #   tabPanel("No Datasets Selected", "No Data set selected"))
          
          uiOutput(ns('tabs'))  

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
    
    # hide/show toggle for wrapper
    observeEvent(input$file_button, {
      shinyjs::toggle("wrapper")
    })
    
    # TODO: similar to data curator will want to initiate file selector data pull
    #       after the toggle is selected
    
    
    
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
    
    # output$dataset_selection <- renderPrint({
    #   dataset()
    # })
    
    # render tabbox dynamically
    # create a tab for each dataset selected in mod_dataset_selection
    output$tabs <- renderUI({
      nTabs <- length(dataset())
      ds <- dataset()
      myTabs <- lapply(1:nTabs, function(x) {
        tabPanel(title = paste0("Dataset: ", ds[x]),
                 output$dataset_selection <- renderPrint({
                      dataset()[x]
                    }))
        
        
      })
      do.call(shinydashboard::tabBox, myTabs)
    })
    
  })
}