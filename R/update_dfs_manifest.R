#' Convert a list to a dataframe
#'
#' @param dfs_status_manifest A data flow status manifest
#' @param dfs_updates Output from mod_update_data_flow_status.R
#' @param selected_datasets_df Output from mod_dataset_selection.R
#' 
#' @export

update_dfs_manifest <- function(dfs_manifest,
                                dfs_updates,
                                selected_datasets_df) {
  
  # remove unchanged attributes from selections
  dfs_updates <- dfs_updates[!unlist(lapply(dfs_updates, is.null))]
  
  # loop over the list of changed attributes
  # for each attribute:
  #   - pull out the original vector
  #   - get the updated entry from the list of attributes
  #   - apply the entry to the selected datasets in dfs manifest
  res <- sapply(names(dfs_updates), function(x) {
    
    # pull out column into a vector
    vec <- dfs_manifest[[x]]
    
    # get entry from updated data flow status attributes list
    entry <- dfs_updates[[x]]
    
    # update vector by index
    manifest_selected_idx <- match(selected_datasets_df$name, dfs_manifest$dataset_name)
    vec[manifest_selected_idx] <- entry
    
    return(vec)
    
  })
  
  # get manifest idx of columns
  update_cols_idx <- match(colnames(res), names(dfs_manifest))
  
  # replace manifest columns with results
  dfs_manifest[,update_cols_idx] <- res
  
  return(dfs_manifest)
}
