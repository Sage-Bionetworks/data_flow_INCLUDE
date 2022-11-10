#' Prepare a dataframe that has been downloaded from Synapse for the Data Flow App
#'
#' @param manifest A manifest that has been downloaded from using manifest_download_to_df()
#' @param config `datatable_dashboard_config.json` read in as a dataframe
#' 
#' @export

prep_manifest_dfa <- function(manifest,
                              config) {
  
  # convert "Not Applicable" to NA
  manifest[ manifest == "Not Applicable" ] <- NA
  
  # convert contribute and dataset to factor
  manifest <- convert_column_type(df = manifest,
                                  col_names = get_colname_by_type("drop_down_filter", config),
                                  type = "factor")
  
  # num_items to integer column
  manifest <- convert_column_type(df = manifest,
                                  col_names = get_colname_by_type("integer", config),
                                  type = "integer")
  
  # release_scheduled and embargo to date columns
  manifest <- convert_column_type(df = manifest,
                                  col_names = get_colname_by_type("date", config),
                                  type = "date")
  
  return(manifest)
}

#' Prepare a dataframe for Synapse submission 
#'
#' @param manifest A manifest that has been downloaded from using manifest_download_to_df()
#' @param config `datatable_dashboard_config.json` read in as a dataframe
#' 
#' @export

prep_manifest_submit <- function(manifest,
                                 config) {
  
  # convert columns back to string
  col_names <- c(get_colname_by_type("date", config),
                 get_colname_by_type("drop_down_filter", config),
                 get_colname_by_type("integer", config))
  
  manifest <- convert_column_type(df = manifest,
                                  col_names = col_names,
                                  type = "character")
  
  # convert NA to "Not Applicable"
  manifest[ is.na(manifest) ] <- "Not Applicable"
  
  return(manifest)
}

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
  
  # capture column names to update
  col_names <- names(dfs_updates)
  
  # loop over the list of changed attributes
  # for each attribute:
  #   - pull out the original vector
  #   - get the updated entry from the list of attributes
  #   - apply the entry to the selected datasets in dfs manifest
  dfs_manifest[col_names] <- lapply(col_names, function(x) {
    
    # pull out column into a vector
    vec <- dfs_manifest[[x]]
    
    # get entry from updated data flow status attributes list
    entry <- dfs_updates[[x]]
    
    # update vector by index
    manifest_selected_idx <- match(selected_datasets_df$id, dfs_manifest$entityId)
    vec[manifest_selected_idx] <- entry
    
    return(vec)
    
  })
  
  return(dfs_manifest)
}

#' Generate a data flow status manifest. Fills in the component, contributor, data type, and dataset name columns.
#' 
#' @export

generate_data_flow_manifest <- function(storage_project_id,
                                        asset_view,
                                        input_token,
                                        contributor = NULL,
                                        calc_num_items = FALSE,
                                        testing = FALSE) {
  
  # get all manifests for storage_project
  message(glue::glue("Building data flow status manifest for {storage_project_id}"))
  message("Getting all manifests")
  all_manifests_list <- storage_project_manifests(asset_view, 
                                                  storage_project_id, 
                                                  input_token)
  
  # pull out data from list
  # dataset metadata
  dataset_naming_metadata <- purrr::map(all_manifests_list, 1)
  dataset_ids <- purrr::map_chr(dataset_naming_metadata, 1)
  dataset_names <- purrr::map_chr(dataset_naming_metadata, 2)
  
  # component metadata
  dataset_component_metadata <- purrr::map(all_manifests_list, 3)
  dataset <- purrr::map_chr(dataset_component_metadata, 1)
  
  # calculate number of items
  # pull down each manifest and count each row (1 file / row)
  if (calc_num_items) {
    message("Counting items in each manifest")
    num_items_list <- lapply(dataset_ids, function(id) {
      manifest <- suppressMessages(manifest_download_to_df(asset_view,
                                                           id,
                                                           input_token))
      if (!is.null(manifest)) {
        nrow(manifest)
      } else {
        NA
      }
    })
    
    num_items <- purrr::flatten_int(num_items_list)

  } else {
    num_items <- rep("Not Applicable", length(dataset_ids))
  }
  
  # create empty dataframe
  colnames <- c("Component", 
                "contributor",
                "entity_id",
                "dataset_name",
                "dataset",
                "num_items",
                "release_scheduled",
                "embargo",
                "standard_compliance",
                "data_portal",
                "released")
  
  df <- data.frame(matrix(nrow = length(dataset_ids), ncol = length(colnames)))
  
  names(df) <- colnames 
  
  # fill in dataframe
  df$Component <- "DataFlow"
  df$dataset_name <- dataset_names
  df$dataset <- dataset
  df$num_items <- num_items
  df$entity_id <- dataset_ids
  
  # add contributor
  if (!is.null(contributor)) {
    df$contributor <- rep(contributor, length(dataset_ids))
  }
  
  # if testing = TRUE: fill in w dummy data
  if (testing) {
    df$release_scheduled <- rep("Not Applicable", length(dataset_ids))
    df$embargo <- rep("Not Applicable", length(dataset_ids))
    df$standard_compliance <- rep(FALSE, length(dataset_ids))
    df$data_portal <- rep(FALSE, length(dataset_ids))
    df$released <- rep(FALSE, length(dataset_ids))
  }
  
  return(df)
}
