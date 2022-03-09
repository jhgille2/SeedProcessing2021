#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_2020
#' @param data_2021
merge_multiyear_data <- function(data_2020 = yield_2020, data_2021 =
                                 merged_data) {

  # I only want to keep the lines that are common between the years
  genotypes_common <- union(unique(data_2020$genotype), unique(data_2021$genotype))

  # Select the set of columns that each set of data have in common and
  # convert the types of data in each of the sets so that they match
  # one another for merging
  data_2020 %<>%
    select(test, year, loc, genotype, code, rep, plot, md, ht, lod, yield, sdwt, sq,
           protein_dry_basis, oil_dry_basis, fc, pub) %>%
    mutate(po    = protein_dry_basis + oil_dry_basis,
           yield = yield*0.033)

  data_2021 %<>%
    mutate(po    = protein_dry_basis + oil_dry_basis,
           code  = as.character(code),
           year  = as.character(year),
           rep   = as.character(rep),
           yield = ifelse(loc %in% c("PLY", "SAN"), yield*0.0252, yield*0.0228)) %>%
    select(test, year, loc, genotype, code, rep, plot, md, ht, lod, yield, sdwt, sq,
           protein_dry_basis, oil_dry_basis, po, pub)

  # Combine the two sets of data
  combined_data <- bind_rows(data_2020, data_2021) %>%
    filter(!(test %in% c("USB Oil 5 Early", "Jay Test 1", "Jay Test 2", "USB Oil 5 Late")),
           genotype %in% genotypes_common) %>%
    arrange(test, year, as.numeric(code), genotype, loc, rep)

  return(combined_data)
}
