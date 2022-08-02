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
    shinydashboard::box(
      title = "Update Data Flow Status",
      width = NULL,
      
      # release scheduled input
      dateInput("release_date", label = h4("Schedule Release"), value = NA),
      
      # embargo input
      dateInput("embargo_date", label = h4("Schedule Embargo"), value = NA),
      
      # standard compliance input
      radioButtons("standard_compliance", 
                   label = h3("Standard Compliance"),
                   choices = list("TRUE" = 1, "FALSE" = 2), 
                   selected = NA),
      
      # data portal input
      radioButtons("data_portal", 
                   label = h3("Data Portal"),
                   choices = list("TRUE" = 1, "FALSE" = 2), 
                   selected = NA),
      
      # released input
      radioButtons("release", 
                   label = h3("Released"),
                   choices = list("TRUE" = 1, "FALSE" = 2), 
                   selected = NA),
    
    
      
      br(),
      
      # Button to initiate dataset selection
      actionButton(ns("submit"), "Submit"),
      
      br()
    )
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
