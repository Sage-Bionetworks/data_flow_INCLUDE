#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic
  
  # DEV STUFF ###########################################################################

  # initialize waiter
  w <- Waiter$new(id = "release_status_wrapper",
                  html = div(
                    style="color:#424874;",
                    waiter::spin_3(),
                    h4("Submitting updated manifest to Synapse...")),
                  color = transparent(.8))
  

  
  # SYNAPSE LOGIN
  # TODO: log in will eventually live in global.R/rely on config info
  reticulate::use_virtualenv(".venv/")
  synapseclient <- reticulate::import("synapseclient")
  syntab <- reticulate::import("synapseclient.table")
  syn <- synapseclient$Synapse()
  syn$login()
  
  global_config <- jsonlite::read_json("inst/global.json")

  # ADMINISTRATE  #######################################################################
  
  # MOD_DATASET_SELECTION
  dataset_selection <- mod_dataset_selection_server(id = "dataset_selection_ui_1",
                                                    asset_view = global_config$asset_view,
                                                    input_token = global_config$schematic_token)
  
  # MOD_FILE_SELECTION
  file_selection <- mod_file_selection_server(id = "file_selection_ui_1",
                                              dataset = dataset_selection,
                                              asset_view = global_config$asset_view,
                                              input_token = global_config$schematic_token)

  # MOD_SET_STATUS
  release_status_selection <- mod_set_release_status_server("set_release_status_ui_1")
  
  # UPDATE MANIFEST WITH STATUS SELECTION

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
  
  # SUBMIT MANIFEST
  
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
  
  # DATASET DASH  #######################################################################
  
  # dummy data
  dataset_dash_data <- reactive({
    
    n <- 10
    
    dates<- lubridate::ymd(paste0("2022-", seq(3:12), "-01"))
    
    ds_names <- paste0(
      sample(LETTERS, n, replace = TRUE),
      sample(LETTERS, n, replace = TRUE),
      "-unique-dataset-name")
    
    contributor <- sample(c("HTAN BU", "HTAN OHSU", "HTAN CHOP"), n, replace = TRUE)
    dataset_type <- sample(c("BiospecimanBatch1", "PatientTrial1", "scATAC-seq", "FamilyHistory"), n, replace = TRUE)
    dataset_name <- paste0(
      sample(LETTERS, n, replace = TRUE),
      sample(LETTERS, n, replace = TRUE),
      "-unique-dataset-name")
    num_items <- as.integer(runif(n, min = 5, max = 500))
    release_scheduled <- sample(c(dates, NA), n, replace = TRUE)
    embargo <- sample(c(dates, NA), n, replace = TRUE)
    standard_compliance <- sample(c(TRUE, FALSE), n, replace = TRUE)
    qc_compliance <- sample(c(TRUE, FALSE), n, replace = TRUE)
    phi_detection_compliance <- sample(c(TRUE, FALSE), n, replace = TRUE)
    access_controls_compliance <- sample(c(TRUE, FALSE), n, replace = TRUE)
    data_portal <- sample(c(TRUE, FALSE), n, replace = TRUE)
    released <- sample(c(TRUE, FALSE), n, replace = TRUE)
    
    all_check_passing_row <- data.frame(contributor = "HTAN_OHSU",
                                        dataset_name = "TF-unique-dataset-name",
                                        dataset_type = "PatientTrial1",
                                        num_items = 1,
                                        release_scheduled = lubridate::floor_date(Sys.Date(), unit = "month"),
                                        embargo = NA,
                                        standard_compliance = TRUE,
                                        data_portal = TRUE,
                                        released = FALSE)

        
    df <- data.frame(contributor,
                     dataset_name,
                     dataset_type,
                     num_items,
                     release_scheduled,
                     embargo,
                     standard_compliance,
                     data_portal,
                     released)
    
    df <- rbind(df, all_check_passing_row)
    
    return(df)
  })
  
  # prepare data to be displayed by mod_datatable

  mod_tabbed_dashboard_server("tabbed_dashboard_1", 
                              dataset_dash_data,
                              jsonlite::read_json("inst/datatable_dashboard_config.json"))
  
}
