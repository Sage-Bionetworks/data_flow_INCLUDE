#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic
  
  # DEV STUFF ###########################################################################
  
  # SYNAPSE LOGIN
  # TODO: log in will eventually live in global.R/rely on config info
  reticulate::use_virtualenv(".venv/")
  synapseclient <- reticulate::import("synapseclient")
  syntab <- reticulate::import("synapseclient.table")
  syn <- synapseclient$Synapse()
  syn$login()

  # ADMINISTRATE  #######################################################################
  
  # MOD_DATASET_SELECTION
  dataset_selection <- mod_dataset_selection_server("dataset_selection_ui_1")
  
  # MOD_FILE_SELECTION
  file_selection <- mod_file_selection_server("file_selection_ui_1",
                                                  dataset_selection)

  # MOD_SET_STATUS
  release_status_selection <- mod_set_release_status_server("set_release_status_ui_1")
  
  # UPDATE MANIFEST WITH STATUS SELECTION
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
  
  # SUBMIT MANIFEST
  
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
  
  # DATASET DASH  #######################################################################
  
  # dummy data
  dataset_dash_data <- reactive({
    
    n <- 10
    
    contributor <- sample(c("HTAN BU", "HTAN OHSU", "HTAN CHOP"), n, replace = TRUE)
    dataset <- sample(c("BiospecimanBatch1", "PatientTrial1", "scATAC-seq", "FamilyHistory"), n, replace = TRUE)
    num <- as.integer(runif(n, min = 5, max = 500))
    dates<- lubridate::ymd(paste0("2022-", seq(1:10), "-01"))
    release_scheduled <- sample(c(dates, NA), n, replace = TRUE)
    embargo <- sample(c("No embargo", "Jun 2022", "May 2022", "Sept 2022"), n, replace = TRUE)
    standard_compliance <- sample(c(TRUE, FALSE), n, replace = TRUE)
    qc_compliance <- sample(c(TRUE, FALSE), n, replace = TRUE)
    phi_detection_compliance <- sample(c(TRUE, FALSE), n, replace = TRUE)
    access_controls_compliance <- sample(c(TRUE, FALSE), n, replace = TRUE)
    data_portal <- sample(c(TRUE, FALSE), n, replace = TRUE)

        
    df <- data.frame(Contributor = contributor,
                     Dataset = dataset,
                     Num_Items = num,
                     Release_Scheduled = release_scheduled,
                     Embargo = embargo,
                     Standard_Compliance = standard_compliance,
                     QC_Compliance = qc_compliance,
                     PHI_Detection_Compliance = phi_detection_compliance,
                     Access_Controls_Compliance = access_controls_compliance,
                     Data_Portal = data_portal)
    
    return(df)
  })
  
  # prepare data to be displayed by mod_datatable
  dataset_dash_data_prepped <- reactive({
    df <- dataset_dash_data()
    
    # convert TRUE/FALSE to icons for qc columns
    qc_cols <- c("Standard_Compliance", "QC_Compliance", "PHI_Detection_Compliance", 
                 "Access_Controls_Compliance", "Data_Portal")
    
    df[qc_cols] <- lapply(df[qc_cols], true_false_icon)
    
    # convert specified columns to factors 
    factor_cols <- c("Contributor")
    df[factor_cols] <- as.factor(df[,factor_cols])
    
    return(df)
  })

  mod_datatable_server("datatable_1", dataset_dash_data_prepped)
  
  
}
