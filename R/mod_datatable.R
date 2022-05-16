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
    
    # define colors for icons in datatable
    # green check
    tags$style(".fa-check {color:#50C878}"),
    # red x
    tags$style(".fa-times {color:#E74C3C}"),
    
    
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
      dt <- DT::datatable(df(),
                          escape = FALSE, 
                          selection = "none",
                          filter = "top",
                          options = list(scrollX = TRUE,
                                         scrollY = 800,
                                         bPaginate = FALSE,
                                         columnDefs = list(list(className = 'dt-center', targets = 6:10),
                                                           list(targets = 4, render = DT::JS(
                                                             "function(data, type, row, meta) {",
                                                             "return data === null ? 'Not Scheduled' : data;", 
                                                             "}"
                                                           )))
                    ))
      
      # format date to get month year info only
      today <- Sys.Date()
      
      # floor date
      today <- lubridate::floor_date(today, unit = "month")
      
      DT::formatStyle(table = dt,
                      columns = "Release_Scheduled",
                      backgroundColor = DT::styleEqual(c(today, NA), c("#90EE90", "#ffffdc")))
      })
  
    
  })
}
    
## To be copied in the UI
# mod_datatable_ui("datatable_1")
    
## To be copied in the server
# mod_datatable_server("datatable_1")
