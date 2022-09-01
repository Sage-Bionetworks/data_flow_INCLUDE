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
    useWaiter(),
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
                                    manifest_dir = "./manifest",
                                    input_token,
                                    schema_url) {
  moduleServer( id, function(input, output, session) {
    ns <- session$ns
    
    w <- Waiter$new(html = div(
      style="color:#424874;",
      waiter::spin_3(),
      h4("Submitting updated manifest to Synapse...")),
      color = transparent(.8))
    
    # make sure that manifest folder exists
    if (!file.exists(manifest_dir)) {
      dir.create(manifest_dir)
    }
    
    # on button click submit model to synapse
    observeEvent(input$submit, {
      w$show()
      
      # write manifest table for upload
      path <- file.path(manifest_dir, "data_flow_status_manifest.csv")
      
      write.table(dfs_manifest(),
                  path,
                  sep = ",",
                  row.names = FALSE)
      
      # submit model to synapse
      model_submit(data_type = data_type,
                   dataset_id = dataset_id(),
                   restrict_rules = TRUE,
                   csv_file = path,
                   input_token = input_token,
                   manifest_record_type = "entity",
                   url = "http://localhost:3001/v1/model/submit",
                   schema_url = schema_url)
      w$hide()
      
    })
    
 
  })
}
    
## To be copied in the UI
# mod_submit_model_ui("submit_model_1")
    
## To be copied in the server
# mod_submit_model_server("submit_model_1")
