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
      dateInput(ns("release_date"), label = h4("Schedule Release"), value = NA),
      
      # embargo input
      dateInput(ns("embargo_date"), label = h4("Schedule Embargo"), value = NA),
      
      # standard compliance input
      radioButtons(ns("standard_compliance"), 
                   label = h4("Standard Compliance"),
                   choices = list("TRUE" = 1, "FALSE" = 2), 
                   selected = NA),
      
      # data portal input
      radioButtons(ns("data_portal"), 
                   label = h4("Data Portal"),
                   choices = list("TRUE" = 1, "FALSE" = 2), 
                   selected = NA),
      
      # released input
      radioButtons(ns("released"), 
                   label = h4("Released"),
                   choices = list("TRUE" = 1, "FALSE" = 2), 
                   selected = NA),
    
    
      
      br(),
      
      # Button to initiate dataset selection
      actionButton(ns("submit"), "Submit"),
      
      br(),
      
      verbatimTextOutput(ns("tst"))
    )
  )
}
    
#' update_data_flow_status Server Functions
#'
#' @noRd 
mod_update_data_flow_status_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    res <- reactive({
      list(release_scheduled = input$release_date,
           embargo = input$embargo_date,
           standard_compliance = input$standard_compliance,
           data_portal = input$data_portal,
           released = input$released,
           action_button = input$submit)
    })

    return(reactive({res()}))
 
  })
}
    
## To be copied in the UI
# mod_update_data_flow_status_ui("update_data_flow_status_1")
    
## To be copied in the server
# mod_update_data_flow_status_server("update_data_flow_status_1")
