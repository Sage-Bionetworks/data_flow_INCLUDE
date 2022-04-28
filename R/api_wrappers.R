###################################################################
## Functions that build on those defined in schematic_rest_api.R ##
###################################################################

## MANIFEST DOWNLOAD ###############################################################

#' Download a manifest into a dataframe
#'
#' @param url URL to schematic API endpoint
#' @param asset_view ID of view listing all project data assets. For example, for Synapse this would be the Synapse ID of the fileview listing all data assets for a given project.(i.e. master_fileview in config.yml)
#' @param dataset_id Synapse ID of existing manifest
#' @param input_token Synapse login cookie, PAT, or API key.
#' @param as_json Synapse login cookie, PAT, or API key.
#' 
#' @export

manifest_download_to_df <- function(asset_view,
                                    dataset_id,
                                    input_token) {
  
  # download manifest
  manifest_json <- manifest_download(asset_view = asset_view,
                                     dataset_id = dataset_id,
                                     input_token = input_token,
                                     as_json = TRUE)
  
  
  # use json lite to parse into dataframe
  content_df <- jsonlite::fromJSON(manifest_json)
  
  # modify column names to match csv manifest download (replace " " with ".")
  names(content_df) <- gsub(" ", ".", names(content_df))
  
  # output df
  
  return(content_df)
}
