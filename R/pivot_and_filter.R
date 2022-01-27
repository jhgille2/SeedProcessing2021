#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param phenotype_data
#' @param leadsheets
#' @return
#' @author 'Jay
#' @export
pivot_and_filter <- function(phenotype_data = merged_data, leadsheets =
                             cleaned_lead_sheets) {

  # The phenotypes to pivot
  measure_vars <- c("ht",
                    "yield",
                    "sdwt",
                    "sq",
                    "oil_dry_basis",
                    "protein_dry_basis",
                    "twt_weight")

  # pivot the merged data using these variables
  merged_longer <- phenotype_data %>%
    pivot_longer(cols = measure_vars, names_to = "trait")

  # Get just the number of reps to measure for protein/oil
  # and use this to make a new table with explicit observations
  # for protein and oil seperately
  prot_oil_table <- leadsheets$`Merged tables`$`Data to collect` %>% dplyr::filter(trait == "protein/oil")

  new_prot_oil_table <- tibble(trait = c(rep("oil_dry_basis", nrow(prot_oil_table)), rep("protein_dry_basis", nrow(prot_oil_table))),
                               reps_to_measure = rep(prot_oil_table$reps_to_measure, 2),
                               test_name = rep(prot_oil_table$test_name, 2))

  # Also add test weight to the traits to measure (same number of reps as sdwt)
  twt_table <- leadsheets$`Merged tables`$`Data to collect` %>% dplyr::filter(trait == "sdwt") %>%
    dplyr::mutate(trait = "twt_weight")

  # Add these two tables to the "data to collect" table and recode some of the trait names
  # to match the ones that are used in the merged data
  lead_sheet_trait_table <- leadsheets$`Merged tables`$`Data to collect` %>%
    dplyr::filter(trait != "protein/oil") %>%
    dplyr::bind_rows(new_prot_oil_table) %>%
    dplyr::bind_rows(twt_table) %>%
    dplyr::mutate(trait = recode(trait,
                                 "lodging"      = "lod",
                                 "height"       = "ht",
                                 "seed quality" = "sq",
                                 "flowor color" = "fc",
                                 "maturity"     = "md",
                                 "pubescence"   = "pub"),
                  test_name = purrr::map_chr(test_name, function(x) stringr::str_remove(x, "2021 ")),
                  test_name = recode(test_name,
                                     "LU 5E-1"  ="LU 5 Early-1",
                                     "LU 5E-2"  ="LU 5 Early-2",
                                     "LU 5L-1"  = "LU 5 Late-1",
                                     "LU 5L-2"  = "LU 5 Late-2",
                                     "LU 6E-21" = "LU 6 Early-21",
                                     "LU 6L-21" = "LU 6 Late-21")) %>%
    dplyr::rename(test = test_name)

  nested_data <- merged_longer %>%
    left_join(lead_sheet_trait_table) %>%
    dplyr::filter(rep <= reps_to_measure) %>%
    group_by(test, loc, trait) %>%
    nest()

  return(nested_data)
}
