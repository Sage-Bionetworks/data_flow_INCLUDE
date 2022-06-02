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
    
    # get todays date
    today <- Sys.Date()
    
    # floor date (only care about month/year)
    today <- lubridate::floor_date(today, unit = "month")
    
    # add a column to df of TRUE/FALSE date is past due
    df_modified <- reactive({
      data <- df()
      dates <- lubridate::floor_date(data$Release_Scheduled, unit = "month")
      data$past_due <- ifelse(dates < today, "pd", 
                              ifelse(dates == today, "t", NA))
      return(data)
    })
    
    
    output$dashboard_tbl <- DT::renderDataTable({
      
      # define column styling
      defs <- list(list(className = 'dt-center', targets = 7:11),# center icon columns
                   list(targets = 5, render = DT::JS( # modify NA return_scheduled
                     "function(data, type, row, meta) {",
                     "return data === null ? 'Not Scheduled' : data;", 
                     "}"
                   )),
                   list(targets = 6, render = DT::JS( # modify NA embargo
                     "function(data, type, row, meta) {",
                     "return data === null ? 'No Embargo' : data;", 
                     "}"
                   )),
                   list(targets = 12, visible = FALSE)) # hide past_due column
      
      # create datatable
      dt <- DT::datatable(df_modified(),
                          escape = FALSE, 
                          selection = "none",
                          filter = "top",
                          options = list(scrollX = TRUE,
                                         scrollY = 800,
                                         bPaginate = FALSE,
                                         columnDefs = defs))
      
      # conditional formatting
      DT::formatStyle(table = dt,
                      "Release_Scheduled", "past_due",
                      backgroundColor = DT::styleEqual(c("t", "pd"), c("#C0EBC0", "#FF9CA0")))
      })
  
    
  })
}
    
## To be copied in the UI
# mod_datatable_ui("datatable_1")
    
## To be copied in the server
# mod_datatable_server("datatable_1")
