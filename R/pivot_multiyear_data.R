#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param combined_data
#' @param leadsheets_2021
#' @param leadsheets_2020
pivot_multiyear_data <- function(combined_data = multiyear_data,
                                 leadsheets_2021 = cleaned_lead_sheets,
                                 leadsheets_2020 = leadsheets_2020_files) {

  # First, clean up the lead sheets from 2020
  cleaned_lead_sheets_2020 <- clean_lead_sheets(files = leadsheets_2020)

  # A function to clean up the "data to collect" portion of the leadsheets so that it can be joined to the rest of the data
  clean_data_to_collect <- function(leadsheets){
    # Get just the number of reps to measure for protein/oil
    # and use this to make a new table with explicit observations
    # for protein and oil seperately
    prot_oil_table <- leadsheets$`Merged tables`$`Data to collect` %>% dplyr::filter(trait == "protein/oil")

    new_prot_oil_table <- tibble(trait = c(rep("oil_dry_basis", nrow(prot_oil_table)), rep("protein_dry_basis", nrow(prot_oil_table)), rep("po", nrow(prot_oil_table))),
                                 reps_to_measure = rep(prot_oil_table$reps_to_measure, 3),
                                 test_name = rep(prot_oil_table$test_name, 3))

    new_sdwt_table <- leadsheets$`Merged tables`$`Plot techniques` %>%
      filter(component == "reps") %>%
      mutate(value = as.numeric(value) - 1,
             component = "sdwt") %>%
      rename(trait = component,
             reps_to_measure = value)

    new_sq_table <- leadsheets$`Merged tables`$`Data to collect` %>%
      filter(trait == "seed quality") %>%
      mutate(reps_to_measure = 2)

    # Also add test weight to the traits to measure (same number of reps as sdwt)
    twt_table <- leadsheets$`Merged tables`$`Data to collect` %>% dplyr::filter(trait == "sdwt") %>%
      dplyr::mutate(trait = "twt_weight")

    # Add these two tables to the "data to collect" table and recode some of the trait names
    # to match the ones that are used in the merged data
    lead_sheet_trait_table <- leadsheets$`Merged tables`$`Data to collect` %>%
      dplyr::filter(trait != "protein/oil") %>%
      dplyr::filter(trait != "sdwt") %>%
      dplyr::filter(trait != "seed quality") %>%
      dplyr::bind_rows(new_prot_oil_table) %>%
      dplyr::bind_rows(new_sdwt_table) %>%
      dplyr::bind_rows(twt_table) %>%
      dplyr::bind_rows(new_sq_table) %>%
      dplyr::mutate(trait = recode(trait,
                                   "lodging"      = "lod",
                                   "height"       = "ht",
                                   "seed quality" = "sq",
                                   "flowor color" = "fc",
                                   "maturity"     = "md",
                                   "pubescence"   = "pub"),
                    year = str_sub(test_name, start = 1, end = 4),
                    test_name = purrr::map_chr(test_name, function(x) stringr::str_remove(x, "2021 ")),
                    test_name = purrr::map_chr(test_name, function(x) stringr::str_remove(x, "2020 ")),
                    test_name = recode(test_name,
                                       "LU 5E-1"  = "LU 5 Early-1",
                                       "LU 5E-2"  = "LU 5 Early-2",
                                       "LU 5L-1"  = "LU 5 Late-1",
                                       "LU 5L-2"  = "LU 5 Late-2",
                                       "LU 6E-21" = "LU 6 Early-21",
                                       "LU 6L-21" = "LU 6 Late-21")) %>%
      dplyr::rename(test = test_name)

    return(lead_sheet_trait_table)
  }
  data_to_collect_2020 <- clean_data_to_collect(cleaned_lead_sheets_2020)
  data_to_collect_2021 <- clean_data_to_collect(leadsheets_2021)

  combined_data_to_collect <- bind_rows(data_to_collect_2020, data_to_collect_2021)

  # The traits I want to fit a model on
  measurement_variables <- c("md",
                             "ht",
                             "lod",
                             "yield",
                             "sdwt",
                             "sq",
                             "protein_dry_basis",
                             "oil_dry_basis",
                             "po")

  # Pivot the data by these measurement variables and then join to the combined data
  # to collect tables so the expected number of reps can be added as an
  # additional column
  pivoted_multiyear_data <- combined_data %>%
    mutate(loc = paste(loc, year, sep = " - ")) %>%
    pivot_longer(cols = measurement_variables, names_to = "trait") %>%
    left_join(combined_data_to_collect, by = c("trait", "test", "year")) %>%
    filter(as.numeric(rep) <= reps_to_measure) %>%
    select(-reps_to_measure) %>%
    group_by(test, loc, trait) %>%
    nest()

  return(pivoted_multiyear_data)
}
