tar_load(merged_data)
tar_load(cleaned_lead_sheets)


measure_vars <- c("ht",
                  "yield",
                  "sdwt",
                  "sq",
                  "oil_dry_basis",
                  "protein_dry_basis",
                  "twt_weight")

merged_longer <- merged_data %>%
  pivot_longer(cols = measure_vars, names_to = "trait")

prot_oil_table <- cleaned_lead_sheets$`Merged tables`$`Data to collect` %>% dplyr::filter(trait == "protein/oil")

twt_table <- cleaned_lead_sheets$`Merged tables`$`Data to collect` %>% dplyr::filter(trait == "sdwt") %>%
  dplyr::mutate(trait = "twt_weight")

new_prot_oil_table <- tibble(trait = c(rep("oil_dry_basis", nrow(prot_oil_table)), rep("protein_dry_basis", nrow(prot_oil_table))),
                             reps_to_measure = rep(prot_oil_table$reps_to_measure, 2),
                             test_name = rep(prot_oil_table$test_name, 2))

lead_sheet_trait_table <- cleaned_lead_sheets$`Merged tables`$`Data to collect` %>%
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
                test_name = map_chr(test_name, function(x) stringr::str_remove(x, "2021 "))) %>%
  dplyr::rename(test = test_name)

all_data <- merged_longer %>%
  dplyr::left_join(lead_sheet_trait_table, by = c("test", "trait")) %>%
  dplyr::filter(rep <= reps_to_measure)

na_prop <- function(data)
{
  sum(is.na(data$value))/nrow(data)
}


ggplot(data = filter(merged_longer, test == "HIF 5"), aes(x = value)) +
  facet_grid(~trait + test, scales = "free") +
  geom_histogram() +
  theme_hc()
