###################################################################
## Functions that build on those defined in schematic_rest_api.R ##
###################################################################

## MANIFEST / DOWNLOAD #############################################################

#' Download a manifest as JSON and parse into a dataframe
#'
#' @param asset_view ID of view listing all project data assets. For example, for Synapse this would be the Synapse ID of the fileview listing all data assets for a given project.(i.e. master_fileview in config.yml)
#' @param dataset_id Synapse ID of existing manifest
#' @param input_token Synapse login cookie, PAT, or API key.
#' 
#' @export

manifest_download_to_df <- function(asset_view,
                                    dataset_id,
                                    input_token) {
  
  # download manifest
  manifest_json <- manifest_download(asset_view = asset_view,
                                     dataset_id = dataset_id,
                                     input_token = input_token,
                                     as_json = TRUE,
                                     new_manifest_name = NULL)
  
  
  # use json lite to parse into dataframe
  content_df <- tryCatch({
    jsonlite::fromJSON(manifest_json)
  },
  error = function(e) {
    message("No manifest available")
    return(NULL)
  })
  
  # if content_df is not NA, modify column names
  if (!is.null(content_df)) {
    names(content_df) <- gsub(" ", ".", names(content_df))
  }
  
  # output df
  return(content_df)
}

