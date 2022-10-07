#' distribution UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_distribution_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' distribution Server Functions
#'
#' @noRd 
mod_distribution_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_distribution_ui("distribution_1")
    
## To be copied in the server
# mod_distribution_server("distribution_1")
