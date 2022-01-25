## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## tar_plan supports drake-style targets and also tar_target()
tar_plan(

  # Get paths to all the leadsheets, yield/field notes, nir,
  # and test weight files
  tar_target(lead_sheet_files,
             list.files(here("data", "leadsheets"), full.names = TRUE),
             format = "file"),

  tar_target(yield_files,
             list.files(here("data", "yield"), full.names = TRUE),
             format = "file"),

  tar_target(nir_files,
             list.files(here("data", "leadsheets"), full.names = TRUE),
             format = "file"),

  tar_target(test_weight_files,
             list.files(here("data", "test_weight"), full.names = TRUE),
             format = "file"),


  # Clean up the leadsheets, yield, nir, and test weight files
  tar_target(cleaned_lead_sheets,
             clean_lead_sheets(lead_sheet_files)),

  tar_target(cleaned_yield_files,
             clean_yield_files(yield_files)),

  tar_target(cleaned_nir_files,
             clean_nir_files(nir_files)),

  tar_target(cleaned_test_weight,
             clean_test_weight(test_weight_files))



)
