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
  dataset_selection <- mod_dataset_selection_server("dataset_selection_ui_1")
  
  # MOD_FILE_SELECTION
  file_selection <- mod_file_selection_server("file_selection_ui_1",
                                                  dataset_selection)

  # MOD_SET_STATUS
  release_status_selection <- mod_set_release_status_server("set_release_status_ui_1")
  
  manifest_mod <- reactive({
    
    mani <- file_selection$manifest()
    
    # check manifest for status column
    # if column exists update value for selected rows
    status_col <- "release_status"
    
    if (status_col %in% colnames(mani)) {
      # TODO: Add handling for when the column exists
      print("this column exists")
      
      } else {
      
        # if the column doesn't exist, create a new column and update value for selected rows
        mani$x <- NA
        names(mani)[names(mani) == "x"] <- status_col
        mani[file_selection$selected_rows(), status_col] <- release_status_selection$status_selection()
      
      }
    
    return(mani)

  })
  
  # wait for button click to display table
  # in place of model/submit endpoint for now
  observeEvent(release_status_selection$btn_click(), {
    output$modified_manifest <- DT::renderDataTable({
      DT::datatable(manifest_mod(),
                    option = list(scrollY = 500,
                                  scrollX = TRUE,
                                  scrollCollapse = TRUE,
                                  bPaginate = FALSE,
                                  dom = "t"),
                    filter = list(position = 'top', clear = TRUE))
    })
  })
  
  
}
