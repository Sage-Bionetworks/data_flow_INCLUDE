reticulate::use_virtualenv(".venv/")
synapseclient <- reticulate::import("synapseclient")
syntab <- reticulate::import("synapseclient.table")
syn <- synapseclient$Synapse()
syn$login()

source("R/schematic_rest_api.R")
##### RUN IT

# variables
asset_view <- "syn20446927"
input_token <- Sys.getenv("schematicToken")

#' Get all storage projects the current user has access to
storage_projects_list <- storage_projects(asset_view = asset_view,
                                          input_token = input_token)

# outputs list of project names and synIDs

# get vector of storageProjects names
storage_projects_names <- unlist(lapply(seq_along(storage_projects_list), function(i) {
  storage_projects_list[[i]][2]
}))

# Gets all datasets in folder under a given storage project that the current user has access to.

# get the project_id from project 1 in list
project1 <- storage_projects_list[[1]]
centerA <- "syn20977135"

# get all datasets in project
datasets_list <- storage_project_datasets(asset_view = asset_view,
                                          project_id = project1[1],
                                          input_token = input_token)

# get all datasets in centerA
datasets_list <- storage_project_datasets(asset_view = asset_view,
                                          project_id = centerA,
                                          input_token = input_token)

# names of dataset
datasets <- unlist(lapply(seq_along(datasets_list), function(i) {
  datasets_list[[i]][2]}))

# get manifest
dataset_id <- as.character(datasets_list[[25]][1])

# save variables
dataset_id <- "syn24181594"

# Get files in dataset
HTAN_centerA_scrnaseq_ex3 <- "syn21988570"

# get file names and ids for a specific dataset
release_administratorUI::storage_dataset_files(asset_view, "syn21988570", input_token)

tst <- manifest_download(asset_view = asset_view,
                         dataset_id = dataset_id,
                         as_json = TRUE,
                         input_token = input_token)



tst <- manifest_download(asset_view = asset_view,
                         dataset_id = "syn30028964",
                         as_json = TRUE,
                         input_token = input_token)

mani<-read.table("./schematic/manifests/synapse_storage_manifest.csv", sep = ",", header = TRUE)



### TESTING PROJECT WORKFLOW #######
asset_view <- "syn20446927"
input_token <- Sys.getenv("schematicToken")

# Get storage project (HTAN Center A)
storage_projects_list <- storage_projects(asset_view = asset_view,
                                          input_token = input_token)

centerA <- storage_project_list[[2]]

# get datasets in centerA
datasets_list <- storage_project_datasets(asset_view = asset_view,
                                          project_id = centerA,
                                          input_token = input_token)

# need to drill further into dataset bc it isn't a top level folder
# modify dataset selection to only show this ID for testing
# get dataset in lw-test
lw_test_id <- "syn30028086"

# get datasets in lw-test
datasets_list_2 <-storage_project_datasets(asset_view = asset_view,
                         project_id = lw_test_id,
                         input_token = input_token)

# get manifest
lw_tsting_dataset <- "syn30028964"

tst_mani <- manifest_download(asset_view = asset_view,
                  dataset_id = lw_tsting_dataset,
                  as_json = TRUE,
                  input_token = input_token)

tst_mani_df <- jsonlite::fromJSON(tst_mani)

# tst manifest df
manifest_download_to_df(asset_view = asset_view,
                        dataset_id = lw_tsting_dataset,
                        input_token = input_token)

# modify manifest
tst_mani_df$modified <- c(TRUE, TRUE)

# save to csv
write.table(tst_mani_df, "./dev/manifest.csv")

# submit manifest
model_submit(data_type = "demographics",
             dataset_id = lw_tsting_dataset,
             input_token = input_token,
             csv_file)

# FIXME: attempted to run through swagger ui and get error 
#        submit_metadata_manifest() missing 1 required positional argument: 'manifest_record_type

# TODO: message lingling about this?

# TODO:
#   Figure out how to get working submit manifest
#   create submit manifest module?
#   on button click, modify manifest and submit to synapse
#   maybe send notification when upload is complete or explaining error
#   To start do not incorporate radio button selection


## TESTING

asset_view <- "syn20446927"
dataset_id <- "syn24181573"

tst<-manifest_download_to_df(asset_view = asset_view,
                        dataset_id = dataset_id,
                        input_token = input_token)