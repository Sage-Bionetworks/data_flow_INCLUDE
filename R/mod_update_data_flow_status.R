#' update_data_flow_status UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_update_data_flow_status_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' update_data_flow_status Server Functions
#'
#' @noRd 
mod_update_data_flow_status_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_update_data_flow_status_ui("update_data_flow_status_1")
    
## To be copied in the server
# mod_update_data_flow_status_server("update_data_flow_status_1")
