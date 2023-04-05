# This script is used to run a scheduled job that updates data flow status manifests
# See Sage IT service catalog section about this here:
#   https://help.sc.sageit.org/sc/Service-Catalog-Provisioning.938836322.html#ServiceCatalogProvisioning-ScheduledJobs

# load libraries
library(dataflow)

# set variables
base_url <- "https://schematic-dev.api.sagebionetworks.org/"
asset_view <- Sys.getenv("ASSET_VIEW")
manifest_dataset_id <- Sys.getenv("MANIFEST_DATASET_ID")
secrets <- jsonlite::fromJSON(Sys.getenv("SCHEDULED_JOB_SECRETS"))
input_token <- secrets$pat

# update FAIR demo
tryCatch({
  update_data_flow_manifest(asset_view = asset_view,
                            manifest_dataset_id = manifest_dataset_id,
                            input_token = input_token,
                            base_url = base_url) 
},
error=function(e) {
  message(paste0("Update to ", asset_view, " failed"))
  message(e)
}
)