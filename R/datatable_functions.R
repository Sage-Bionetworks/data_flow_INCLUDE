###############################################
## Functions that create stylized datatables ##
###############################################

#' Create a dashboard style datatable
#'
#' @param A dataframe prepared by `prep_df_for_dash()` with the columns `Contributor`, `Dataset_Name`, `Dataset_Type`, `Num_Items`, `Release_Scheduled`, `Embargo`, `Standard_Compliance`, `QC_Compliance`,`PHI_Detection_Compliance`, `Access_Controls_Compliance`, `Data_Portal`, `Released`, `past_due`
#' 
#' @export
#' 

create_dashboard <- function(prepped_dataframe) {
  
  # define column styling
  
  defs <- list(
    
    # center icon columns
    list(className = 'dt-center', targets = 7:12),
    
    # modify NA return_scheduled
    list(targets = 5, render = DT::JS(
      "function(data, type, row, meta) {",
      "return data === null ? 'Not Scheduled' : data;", 
      "}"
    )),
    
    # modify NA embargo
    list(targets = 6, render = DT::JS( 
      "function(data, type, row, meta) {",
      "return data === null ? 'No Embargo' : data;", 
      "}"
    )),
    
    # hide past_due column
    list(targets = 13, visible = FALSE))
  
  # create datatable
  dt <- DT::datatable(prepped_dataframe,
                      escape = FALSE, 
                      selection = "none",
                      filter = "top",
                      options = list(scrollX = TRUE,
                                     scrollY = 800,
                                     bPaginate = FALSE,
                                     columnDefs = defs))
  
  # conditional formatting
  DT::formatStyle(table = dt,
                  "Release_Scheduled", "past_due",
                  backgroundColor = DT::styleEqual(c("t", "pd"), c("#C0EBC0", "#FF9CA0")))

}

#' Prepare a dataframe for a dashboard style datatable
#'
#' @param A dataframe with the columns `Contributor`, `Dataset_Name`, `Dataset_Type`, `Num_Items`, `Release_Scheduled`, `Embargo`, `Standard_Compliance`, `QC_Compliance`,`PHI_Detection_Compliance`, `Access_Controls_Compliance`, `Data_Portal`, `Released`, `past_due`
#' 
#' @export
#' 

prep_df_for_dash <- function(dataframe) {
  
  # convert TRUE/FALSE to icons for qc columns
  qc_cols <- c("Standard_Compliance", "QC_Compliance", "PHI_Detection_Compliance", 
               "Access_Controls_Compliance", "Data_Portal", "Released")
  
  dataframe[qc_cols] <- lapply(dataframe[qc_cols], true_false_icon)
  
  # convert certain columns to factors 
  # enables drop down selection style filtering for column
  factor_cols <- c("Contributor", "Dataset_Type")
  dataframe[factor_cols] <- lapply(dataframe[factor_cols], factor)  
  
  
  return(dataframe)
}


## HELPERS ##############################################################################

#' Convert a vector of TRUE/FALSE to icon html
#'
#' @param vec A vector of TRUE/FALSE
#' 
#' @export

true_false_icon <- function(vec) {
  
  # if true assign checkmark icon
  # if false assign x icon
  true_icon <- as.character(icon("check", lib = "font-awesome"))
  false_icon <- as.character(icon("times", lib = "font-awesome"))
  
  ifelse(vec == TRUE, true_icon, false_icon)
}