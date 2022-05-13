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

names(tst_mani_df)[10] <- "HTAN Participant ID"
tst_mani_df$`Age Is Obfuscated` <- c("False", "False")
tst_mani_df[1, "Race"] <- "Not Reported"

# tst manifest df
manifest_download_to_df(asset_view = asset_view,
                        dataset_id = lw_tsting_dataset,
                        input_token = input_token)

# Fix colNames


# save to csv

# WORKING SUBMIT MANIFEST CALL

model_submit(data_type = "None", 
             dataset_id = "syn30028964",
             restrict_rules = FALSE,
             csv_file = "~/Desktop/tsting_manifests/unmodified_manifest/synapse_storage_manifest.csv",
             input_token = input_token,
             manifest_record_type = "table",
             url="http://localhost:3001/v1/model/submit",
             schema_url="https://raw.githubusercontent.com/ncihtan/data-models/main/HTAN.model.jsonld")


## TESTING

asset_view <- "syn20446927"
dataset_id <- "syn24181573"

tst<-manifest_download_to_df(asset_view = asset_view,
                             dataset_id = dataset_id,
                             input_token = input_token)

