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
    
    DT::DTOutput(ns("datatable_out")))
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
    all_datasets <- reactive({
      data <- df()
      dates <- lubridate::floor_date(data$Release_Scheduled, unit = "month")
      data$past_due <- ifelse(dates < today, "pd", 
                              ifelse(dates == today, "t", NA))
      return(data)
    })

    # render datatable
    
    output$datatable_out <- DT::renderDataTable({
      create_dashboard(
        prep_df_for_dash(all_datasets())
        )
      })

  })
}
    
## To be copied in the UI
# mod_datatable_ui("datatable_1")
    
## To be copied in the server
# mod_datatable_server("datatable_1")
