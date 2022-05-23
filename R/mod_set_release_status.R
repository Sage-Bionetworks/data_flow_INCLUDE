#' set_release_status UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_set_release_status_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        
        shinydashboard::box(
          title = "Select Status",
          width = NULL,
          
          radioButtons(ns("select_status"),
                       label = "Select status",
                       choices = c("Quarantine", "Upcoming Release")),
          
          actionButton(ns("submit_status_btn"), "Submit")
          )
        )
      )
    )
}
    
#' set_release_status Server Functions
#'
#' @noRd 
mod_set_release_status_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # return selected item
    return(list(
      status_selection = reactive({ input$select_status }),
      btn_click = reactive({ input$submit_status_btn })
      ))
  })
}
    
## To be copied in the UI
# mod_set_release_status_ui("set_release_status_ui_1")
    
## To be copied in the server
# mod_set_release_status_server("set_release_status_ui_1")
