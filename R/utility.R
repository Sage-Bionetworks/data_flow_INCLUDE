###############################
## Generic Utility Functions ##
###############################

#' Convert a list to a dataframe
#'
#' @param list A list of items. Only works where list entries all follow the same pattern.
#' @param col_names Desired column names. Ex: `c("id", "name")`
#' 
#' @export

list_to_dataframe <- function(list,
                              col_names = NULL) {
  
  # convert list to dataframe
  df <- data.frame(do.call(rbind, list))
  
  # if colnames are specified, add them
  if (!is.null(col_names)) {
    names(df) <- col_names
  }
  
  return(df)
}


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

