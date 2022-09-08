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
      mod_scheduler_ui(ns("release_date"), 
                       dateInput_label = h4("Schedule Release"),
                       checkboxInput_label = "Unschedule Release"),
      # embargo input
      mod_scheduler_ui(ns("embargo"), 
                       dateInput_label = h4("Schedule Embargo"),
                       checkboxInput_label = "Unschedule Embargo"),
      
      # standard compliance input
      radioButtons(ns("standard_compliance"), 
                   label = h4("Standard Compliance"),
                   list("TRUE" = TRUE, "FALSE" = FALSE), 
                   selected = NA),
      
      # data portal input
      radioButtons(ns("data_portal"), 
                   label = h4("Data Portal"),
                   choices = list("TRUE" = TRUE, "FALSE" = FALSE), 
                   selected = NA),
      
      # released input
      radioButtons(ns("released"), 
                   label = h4("Released"),
                   choices = list("TRUE" = TRUE, "FALSE" = FALSE), 
                   selected = NA),
      )
  )
}

    
#' update_data_flow_status Server Functions
#'
#' @noRd 
mod_update_data_flow_status_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    release_scheduled <- mod_scheduler_server("release_date")

    embargo <- mod_scheduler_server("embargo")
    
    res <- reactive({
      list(release_scheduled = release_scheduled(),
           embargo = embargo(),
           standard_compliance = input$standard_compliance,
           data_portal = input$data_portal,
           released = input$released)
    })
    

    return(reactive({ res() }))

 
  })
}
    
## To be copied in the UI
# mod_update_data_flow_status_ui("update_data_flow_status_1")
    
## To be copied in the server
# mod_update_data_flow_status_server("update_data_flow_status_1")
