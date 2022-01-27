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
             list.files(here("data", "nir"), full.names = TRUE),
             format = "file"),

  tar_target(test_weight_files,
             list.files(here("data", "test_weight"), full.names = TRUE),
             format = "file"),

  tar_target(nir_masterfile,
             here("data", "nir_masterfile_2021_yield.csv"),
             format = "file"),


  # Clean up the leadsheets, yield, nir, and test weight files
  tar_target(cleaned_lead_sheets,
             clean_lead_sheets(lead_sheet_files)),

  tar_target(cleaned_yield_files,
             clean_yield_files(yield_files)),

  tar_target(cleaned_nir_files,
             clean_nir_files(files = nir_files, nir_masterfile = nir_masterfile)),

  tar_target(cleaned_test_weight,
             clean_test_weight(test_weight_files)),

  # Merge the yield/field notes, nir, and test weight data
  tar_target(merged_data,
             merge_all_data(yield = cleaned_yield_files,
                            nir = cleaned_nir_files,
                            twt = cleaned_test_weight)),

  # Pivot this merged data by phenotype and filter the observations so that only the samples
  # that a phenotype shouldve been collected for are kept
  tar_target(pivoted_phenotype_data,
             pivot_and_filter(phenotype_data = merged_data, leadsheets = cleaned_lead_sheets))



)
