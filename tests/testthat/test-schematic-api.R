# adapted from afwillia's work on data_curator 
# (https://github.com/Sage-Bionetworks/data_curator/blob/schematic-rest-api/tests/testthat/test_schematic_rest_api.R)

# VARIABLES #############################################################################
# FAIR DEMO DATA PROJECT A
asset_view <- "syn50896957" # FAIR Demo All Projects, Files and Folders
project_id <- "syn50896931" # FAIR Demo Project A
dataset_id <- "syn51219090" # DataFlowStatusDFATesting
input_token <- Sys.getenv("SYNAPSE_PAT")
base_url <- Sys.getenv("SCHEMATIC_BASE_URL_AWS")
testing_manifest_path <- "../../inst/testing/synapse_storage_manifest_dataflow.csv"
schema_url <- "https://raw.githubusercontent.com/Sage-Bionetworks/data_flow/main/inst/data_flow_component.jsonld"

# TEST API ##############################################################################

test_that("storage_projects successfully returns a schematic_api object", {
  res <- try(storage_projects(asset_view = asset_view,
                             input_token = input_token),
            silent = FALSE)
  
  expect_true(class(res) == "schematic_api")
})

test_that("storage_project_datasets successfully returns a schematic_api object", {
  res <- try(storage_project_datasets(asset_view = asset_view,
                                      project_id = project_id,
                                      input_token = input_token,
                                      base_url = base_url),
             silent = FALSE)
  
  expect_true(class(res) == "schematic_api")
})

test_that("manifest_download successfully returns a schematic_api object", {
  res <- try(manifest_download(input_token = input_token,
                              asset_view = asset_view,
                              dataset_id = dataset_id,
                              base_url = base_url),
            silent = FALSE)
  
  expect_true(class(res) == "schematic_api")
  
})

test_that("model_submit successfully returns a schematic_api object", {
  res <- try(model_submit(data_type = NULL, 
                        asset_view = asset_view,
                        dataset_id = dataset_id,
                        file_name = testing_manifest_path,
                        input_token = input_token,
                        restrict_rules = TRUE,
                        manifest_record_type = "table_and_file",
                        base_url = base_url,
                        schema_url = schema_url,
                        use_schema_label = TRUE),
           silent = FALSE)
  
  expect_true(class(res) == "schematic_api")
})

test_that("storage_project_manifests successfully returns a schematic_api object", {
  res <- try(storage_project_manifests(asset_view,
                                      project_id,
                                      input_token,
                                      base_url),
            silent = FALSE)
  
  expect_true(class(res) == "schematic_api")
  
})

test_that("visualize/component successfully returns a schematic_api object", {
  res <- try(visualize_component(schema_url = schema_url,
                                 component = "DataFlow",
                                 base_url = base_url),
             silent = FALSE)
  
  expect_true(class(res) == "schematic_api")
  
})
