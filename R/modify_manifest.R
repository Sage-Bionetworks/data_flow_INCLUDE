#####################################
## Manifest Modification Functions ##
#####################################

## Need a function or set of functions that will update the selected datasets/files with 
## some information.
##  - When no files selected, update will happen to all files of selected dataset
##  - When files are selected, update will happen only to selected files
##  - Updated information will include:
##    - Status (i.e. quarantine, pre-prod, production)
##    - Date/time of modification
##    - Who made the modification

modifyManifest <- function(manifest) {
  manifest$modified <- replicate(nrow(manifest, TRUE))
  return(manifest)
}
