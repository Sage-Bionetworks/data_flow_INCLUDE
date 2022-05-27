#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic
  
  w <- Waiter$new(id = "release_status_wrapper")
  
  # DEV STUFF #########################################################################
  
  # SYNAPSE LOGIN
  # TODO: log in will eventually live in global.R/rely on config info
  reticulate::use_virtualenv(".venv/")
  synapseclient <- reticulate::import("synapseclient")
  syntab <- reticulate::import("synapseclient.table")
  syn <- synapseclient$Synapse()
  syn$login()
  
  schematic_token <- Sys.getenv("schematicToken")
  

  # APP SERVER LOGIC  ##############################################################
  
  # MOD_DATASET_SELECTION
  dataset_selection <- mod_dataset_selection_server("dataset_selection_ui_1")
  
  # MOD_FILE_SELECTION
  file_selection <- mod_file_selection_server("file_selection_ui_1",
                                                  dataset_selection)

  # MOD_SET_STATUS
  release_status_selection <- mod_set_release_status_server("set_release_status_ui_1")
  
  # modify selected manifest rows with release status selection
  manifest_mod <- reactive({

    mani <- file_selection$manifest()

    # check manifest for status column
    # if column doesn't exist, add it

    status_col <- "ReleaseStatus"

    if (!status_col %in% colnames(mani)) {
      mani$x <- NA
      names(mani)[names(mani) == "x"] <- status_col
      }

    # add release_status_selection to selected rows
    mani[file_selection$selected_rows(), status_col] <- release_status_selection$status_selection()

    return(mani)

  })
  
  # wait for button click to display table
  # in place of model/submit endpoint for now
  observeEvent(release_status_selection$btn_click(), {
    
    # show waiter
    w$show()
    
    # on exit - hide waiter
    on.exit({
      w$hide()
    })
    
    # create manifest dir
    suppressWarnings(dir.create("./manifest"))
    
    # write modified manifest to dir
    write.table(manifest_mod(),
                "./manifest/synapse_storage_manifest.csv",
                sep = ",",
                row.names = FALSE)
    
    # submit to symapse
    model_submit(data_type = "None", 
                 dataset_id = dataset_selection()$id,
                 restrict_rules = FALSE,
                 csv_file = "./manifest/synapse_storage_manifest.csv",
                 input_token = schematic_token,
                 manifest_record_type = "table",
                 url="http://localhost:3001/v1/model/submit",
                 schema_url="https://raw.githubusercontent.com/ncihtan/data-models/main/HTAN.model.jsonld")
    
  })
  
  
}
