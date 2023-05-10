# CREATE TESTING VARIABLES

base_url <- base_url <- Sys.getenv("SCHEMATIC_BASE_URL_AWS")
input_token <- Sys.getenv("SYNAPSE_PAT")
asset_view <- "syn50896957"

# mock synapse / submission ready manifest
manifest_synapse <- data.frame(Component = "DataFlow",
                               contributor = "FAIR demo data",
                               entityId = "syn123",
                               dataset_name = "biospecimen",
                               dataset = "Biospecimen",
                               num_items = "1",
                               release_scheduled = "2050-01-01",
                               embargo = "Not Applicable",
                               standard_compliance = FALSE,
                               data_portal = FALSE,
                               released = FALSE)

# mock dfa ready manifest
manifest_dfa <- data.frame(Component = "DataFlow",
                           contributor = as.factor("FAIR demo data"),
                           entityId = "syn123",
                           dataset_name = "biospecimen",
                           dataset = as.factor("Biospecimen"),
                           num_items = 1,
                           release_scheduled = as.Date("2050-01-01"),
                           embargo = as.Date(NA),
                           standard_compliance = FALSE,
                           data_portal = FALSE,
                           released = FALSE)

# mock get_all_manifests output
get_all_manifests_same <- manifest_synapse[,c("Component", "contributor", "entityId", "dataset_name", "dataset")]

# read in test config
config <- jsonlite::read_json("../../inst/testing/datatable_dashboard_config.json")

# TESTS

test_that("prep_manifest_dfa modifies manifest in expected way", {
  expect_equal(prep_manifest_dfa(manifest_synapse, config),
               manifest_dfa)
})

test_that("prep_manifest_submit modifies manifest in expected way", {
  expect_equal(prep_manifest_submit(manifest_dfa, config),
               manifest_synapse)
})

test_that("update_manifest_add_datasets adds new datasets to manifest", {
  
  # add a dataset
  get_all_manifests_add_dataset <- rbind(get_all_manifests_same,
                                         data.frame(Component = "DataFlow",
                                                    contributor = "FAIR demo data",
                                                    entityId = "syn44539618",
                                                    dataset_name = "MockComponent",
                                                    dataset = "Patient"))
  
  manifest <- update_manifest_add_datasets(dataflow_manifest = manifest_synapse,
                                           get_all_manifests_out = get_all_manifests_add_dataset,
                                           asset_view = asset_view,
                                           input_token = input_token,
                                           base_url = base_url)
  
  expect_equal(get_all_manifests_add_dataset$entityId, manifest$entityId)
})

test_that("update_manifest_remove_datasets removes datasets from manifest", {
  
  # remove a dataset
  get_all_manifests_remove_dataset <- get_all_manifests_same[-2, ]
  
  manifest <- update_manifest_remove_datasets(dataflow_manifest = manifest_synapse,
                                              get_all_manifests_out = get_all_manifests_remove_dataset,
                                              asset_view = asset_view,
                                              input_token = input_token,
                                              base_url = base_url)
  
  expect_equal(get_all_manifests_remove_dataset$entityId, manifest$entityId)
})

test_that("update_manifest_column updates items in a selected column", {
  
  # change the dataset_name column
  get_all_manifests_change_dataset_name <- get_all_manifests_same
  get_all_manifests_change_dataset_name$dataset_name[1] <- "new_dataset_name"
  
  manifest <- update_manifest_column(dataflow_manifest = manifest_synapse, 
                                     get_all_manifests_out = get_all_manifests_change_dataset_name, 
                                     update_column = "dataset_name",
                                     asset_view = asset_view, 
                                     recalc_num_items = FALSE,
                                     input_token = input_token, 
                                     base_url = base_url)
  
  expect_equal(manifest$dataset_name, "new_dataset_name")
})

# FIXME: FIX THIS TEST
# test_that("update_dfs_manifest", {
#   
#   dfs_updates <-  list(release_scheduled = as.Date("2022-01-01"),
#                        embargo = as.Date("2022-01-01"),
#                        standard_compliance = TRUE,
#                        data_portal = TRUE,
#                        released = TRUE)
#   
#   selected_datasets_df <- data.frame(id = c("syn123"), name = "biospecimen")
#   
#   expected_updated_row <- data.frame(Component = "DataFlow",
#                                      contributor = as.factor("FAIR demo data"),
#                                      entityId = "syn123",
#                                      dataset_name = "biospecimen",
#                                      dataset = as.factor("Biospecimen"),
#                                      num_items = as.numeric(NA),
#                                      release_scheduled = as.Date("2022-01-01"),
#                                      embargo = as.Date("2022-01-01"),
#                                      standard_compliance = TRUE,
#                                      data_portal = TRUE,
#                                      released = TRUE)
#   expected_df <- rbind(expected_updated_row, manifest_dfa[-1,])
#   
#   expect_equal(update_dfs_manifest(dfs_manifest = manifest_dfa,
#                                    dfs_updates = dfs_updates,
#                                    selected_datasets_df = selected_datasets_df),
#                expected_df)
# })
