# This script is used to run a scheduled job that updates data flow status manifests
# See Sage IT service catalog section about this here:
#   https://help.sc.sageit.org/sc/Service-Catalog-Provisioning.938836322.html#ServiceCatalogProvisioning-ScheduledJobs

# load libraries
library(dataflow)

# set variables
base_url <- "https://schematic-dev.api.sagebionetworks.org/"
secrets <- jsonlite::fromJSON(Sys.getenv("SCHEDULED_JOB_SECRETS"))
input_token <- secrets$pat

# update FAIR demo
tryCatch({
  update_data_flow_manifest(asset_view = "syn50896957",
                            manifest_dataset_id = "syn50900267",
                            input_token = input_token,
                            base_url = base_url) 
},
error=function(e) {
  message("Update to Fair Demo Data (syn50896957) failed")
  message(e)
}
)