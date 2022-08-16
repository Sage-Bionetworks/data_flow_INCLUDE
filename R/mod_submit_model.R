#' submit_model UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_submit_model_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    actionButton(ns("submit"), "Submit to Synapse")
 
  )
}
    
#' submit_model Server Functions
#'
#' @noRd 
mod_submit_model_server <- function(id, 
                                    dfs_manifest,
                                    data_type,
                                    dataset_id,
                                    manifest_path = "./manifest",
                                    input_token) {
  moduleServer( id, function(input, output, session) {
    ns <- session$ns
    
    path <- file.path(manifest_path, "data_flow_status_manifest.csv")
    observeEvent(input$submit, {
      
      # write manifest table for upload
      write.table(dfs_manifest(),
                  path,
                  sep = ",",
                  row.names = FALSE)
      
      model_submit(data_type = data_type,
                   dataset_id = dataset_id,
                   restrict_rules = TRUE,
                   csv_file = path,
                   input_token = input_token,
                   manifest_record_type = "entity",
                   url="http://localhost:3001/v1/model/submit",
                   schema_url="https://raw.githubusercontent.com/Sage-Bionetworks/data_flow/implement_schema/inst/data_flow_component.jsonld")
      
    })
    
 
  })
}
    
## To be copied in the UI
# mod_submit_model_ui("submit_model_1")
    
## To be copied in the server
# mod_submit_model_server("submit_model_1")
