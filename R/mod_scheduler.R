#' scheduler UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param dateInput_label label for dateInput
#' @param checkboxInput_label date for checkboxInput
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_scheduler_ui <- function(id,
                             dateInput_label,
                             checkboxInput_label) {
  ns <- NS(id)
  tagList(
    # release scheduled input
    dateInput(ns("date"), label = dateInput_label, value = NA),
    checkboxInput(ns("unschedule_chk"), label = checkboxInput_label, value = FALSE))
}
    
#' scheduler Server Functions
#'
#' @noRd 
mod_scheduler_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    date_out <- reactive({
      
      if (length(input$date) == 0 & input$unschedule_chk == FALSE) {
        return(NULL)
      } else if (length(input$date) == 0 & input$unschedule_chk == TRUE) {
        return(NA)
      } else if (length(input$date) != 0 & input$unschedule_chk == TRUE) {
        return(NA)
      } else {
        return(input$date)
      }
      
    })
    
    return(reactive({ date_out() }))
 
  })
}
    
## To be copied in the UI
# mod_scheduler_ui("scheduler_1")
    
## To be copied in the server
# mod_scheduler_server("scheduler_1")
