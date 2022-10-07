#' stacked_bar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_stacked_bar_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' stacked_bar Server Functions
#'
#' @noRd 
mod_stacked_bar_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_stacked_bar_ui("stacked_bar_1")
    
## To be copied in the server
# mod_stacked_bar_server("stacked_bar_1")
