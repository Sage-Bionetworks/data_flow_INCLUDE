#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic
  
  # DEV STUFF #########################################################################
  
  # SYNAPSE LOGIN
  # TODO: log in will eventually live in global.R/rely on config info
  reticulate::use_virtualenv(".venv/")
  synapseclient <- reticulate::import("synapseclient")
  syntab <- reticulate::import("synapseclient.table")
  syn <- synapseclient$Synapse()
  syn$login()
  
  # CREDENTIALS REACTIVE VAL #######################################################
  # these will likely come from a config eventually
  
  creds <- reactiveValues(asset_view = "syn20446927",
                          schematic_token = Sys.getenv("schematicToken"))
  
  # APP SERVER LOGIC  ##############################################################
  
  # MOD_DATASET_SELECTION
  # input: dataframe of storage projects (name, id)
  dataset_selection <- mod_dataset_selection_server("dataset_selection_ui_1")
  
  # MOD_FILE_SELECTION
  file_selection <- mod_file_selection_server("file_selection_ui_1",
                                                  dataset_selection)

  # MOD_SET_STATUS
  release_status_selection <- mod_set_release_status_server("set_release_status_ui_1")
  
  manifest_mod <- reactive({
    manifest <- file_selection$manifest()
    manifest[file_selection$selected_rows(), ]
  })
  
  output$modified_manifest <- DT::renderDataTable({
    DT::datatable(manifest_mod())
                  # option = list(scrollY = 500,
                  #               scrollX = TRUE,
                  #               scrollCollapse = TRUE,
                  #               bPaginate = FALSE,
                  #               dom = "t"),
                  # filter = list(position = 'top', clear = TRUE))
    })
}
