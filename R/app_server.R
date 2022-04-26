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

  # APP SERVER LOGIC  ##############################################################
  
  # MOD_DATASET_SELECTION
  # input: dataframe of storage projects (name, id)
  mod_dataset_selection_server("dataset_selection_ui_1")
  
  #MOD_FILE_SELECTION
  #mod_file_selection_server("file_selection_ui_1", ds)
}
