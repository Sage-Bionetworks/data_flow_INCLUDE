#' runners_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_runners_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' runners_plot Server Functions
#'
#' @noRd 
mod_runners_plot_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_runners_plot_ui("runners_plot_1")
    
## To be copied in the server
# mod_runners_plot_server("runners_plot_1")
