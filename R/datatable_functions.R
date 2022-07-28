###############################################
## Functions that create stylized datatables ##
###############################################

#' Create a dashboard style datatable
#'
#' @param df A dataframe prepared by `prep_df_for_dash()` with the columns `Contributor`, `Dataset_Name`, `Dataset_Type`, `Num_Items`, `Release_Scheduled`, `Embargo`, `Standard_Compliance`, `QC_Compliance`,`PHI_Detection_Compliance`, `Access_Controls_Compliance`, `Data_Portal`, `Released`, `past_due`
#' @param config Config for datatable dashboard module in `inst/datatable_dashboard_config.json`
#' 
#' @export
#' 

create_dashboard <- function(df,
                             config) {
  
  parsed_config <- parse_config(config,df) 
  
  prepped_df <- prep_df_for_dash(df, parsed_config)
  
  style_dashboard(prepped_df, parsed_config)
}

#' Add custom styling to dashboard based on contents of config
#'
#' @param df A dataframe prepared by `prep_df_for_dash()` with the columns `Contributor`, `Dataset_Name`, `Dataset_Type`, `Num_Items`, `Release_Scheduled`, `Embargo`, `Standard_Compliance`, `QC_Compliance`,`PHI_Detection_Compliance`, `Access_Controls_Compliance`, `Data_Portal`, `Released`, `past_due`
#' @param parsed_config updated config output by `parse_config()`
#' 
#' @export
#' 

style_dashboard <- function(prepped_dataframe,
                            parsed_config) {
  
  # define center styling for icon columns
  center_list <- list(className = 'dt-center', targets = parsed_config$icon_idx)
  
  # modify NA for release_scheduled column
  release_scheduled_list <- dt_replace_na(parsed_config$release_scheduled$idx,
                                          parsed_config$release_scheduled$na_replace)
  
  # modify NA for embargo column
  embargo_list <- dt_replace_na(parsed_config$embargo$idx,
                                parsed_config$embargo$na_replace)
  
  # find past_due column
  past_due_idx <- grep("past_due", names(prepped_dataframe))

  # capture all styling in a single variable
  defs <- list(
    center_list,
    release_scheduled_list,
    embargo_list,
    # hide past_due column
    list(targets = past_due_idx, visible = FALSE))
  
  # create datatable
  dt <- DT::datatable(prepped_dataframe,
                      escape = FALSE, 
                      selection = "none",
                      filter = "top",
                      options = list(scrollX = TRUE,
                                     scrollY = 800,
                                     bPaginate = FALSE,
                                     columnDefs = defs))
  
  if (as.logical(toupper(parsed_config$release_scheduled$color_past_due))) {
    
    dt <- DT::formatStyle(table = dt,
                          parsed_config$release_scheduled$col_name, "past_due",
                          backgroundColor = DT::styleEqual("pd", "#FF9CA0"))
  }
  
  dt
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

#' Parse datatable dashboard config
#'
#' @param config datatable_dashboard_config.json as a datatable (`jsonlite::read_json("inst/datatable_dashboard_config.json")`)
#' @param df dataframe to be turned into a dashboard
#' 
#' @export

parse_config <- function(config,
                         df) {

  # parse release scheduled columns
  config$release_scheduled$idx <- grep("release_scheduled", names(df))
  
  # embargo
  config$embargo$idx <- grep("embargo", names(df))
  
  # create a vector of display column names
  col_names<- purrr::map(config, "col_name") 
  config$col_names <- data.frame(col_names = names(df), rename = purrr::flatten_chr(col_names))
  
  # get icon cols idx
  icon_cols <- c("standard_compliance", "data_portal", "released")
  config$icon_idx <- grep(paste0(icon_cols, collapse = "|"), names(df))
  
  return(config)
}

#' Prepare a dataframe for a dashboard style datatable
#'
#' @param df A dataframe with the columns `Contributor`, `Dataset_Name`, `Dataset_Type`, `Num_Items`, `Release_Scheduled`, `Embargo`, `Standard_Compliance`, `QC_Compliance`,`PHI_Detection_Compliance`, `Access_Controls_Compliance`, `Data_Portal`, `Released`, `past_due`
#' @param parsed_config updated config output by `parse_config()`
#' 
#' @export
#' 

prep_df_for_dash <- function(df,
                             parsed_config) {
  
  # create past_due column for highlighting release_scheduled
  today <- Sys.Date()
  today <- lubridate::floor_date(today, unit = "month")
  dates <- lubridate::floor_date(df[,"release_scheduled"], unit = "month")
  df$past_due <- ifelse(dates < today, "pd", 
                        ifelse(dates == today, "t", NA))
  
  # convert TRUE / FALSE to icon html
  icon_cols <- c("standard_compliance", "data_portal", "released")
  df[icon_cols] <- lapply(df[,icon_cols], true_false_icon)
  
  # convert certain columns to factors 
  # enables drop down selection style filtering for column
  factor_cols <- c("dataset_type", "contributor")
  df[factor_cols] <- lapply(df[,factor_cols], factor)
  
  # rename columns
  names(df) <- c(parsed_config$col_names$rename, "past_due")
  
  return(df)
}


#' NA replacement - datatable custom JS
#'
#' @param df A dataframe with the columns `contributo`, `Dataset_Name`, `Dataset_Type`, `Num_Items`, `Release_Scheduled`, `Embargo`, `Standard_Compliance`, `QC_Compliance`,`PHI_Detection_Compliance`, `Access_Controls_Compliance`, `Data_Portal`, `Released`, `past_due`
#' @param parsed_config updated config output by `parse_config()`
#' 
#' @export
#' 

dt_replace_na <- function(col_index,
                          na_replacement) {
  
  list(targets = col_index, 
       render = DT::JS(
         "function(data, type, row, meta) {",
         glue::glue("return data === null ? '{na_replacement}' : data;"),
         "}"
         ))
}