#' datatable UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_datatable_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    shinydashboard::box(
      title = "Dashboard",
      width = NULL,
      
      DT::DTOutput(ns("dashboard_tbl"))
    )
 
  )
}
    
#' datatable Server Functions
#'
#' @noRd 
mod_datatable_server <- function(id, df){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$dashboard_tbl <- DT::renderDataTable({
      DT::datatable(df(), 
                    escape = FALSE,
                    selection = "none",
                    filter = "top",
                    options = list(scrollX = TRUE,
                                   scrollY = 800,
                                   scrollCollapse = TRUE,
                                   bPaginate = FALSE,
                                   columnDefs = list(list(className = 'dt-center', targets = 6:10))
                    ))
      })
  
    
  })
}
    
## To be copied in the UI
# mod_datatable_ui("datatable_1")
    
## To be copied in the server
# mod_datatable_server("datatable_1")
