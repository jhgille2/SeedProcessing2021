#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param yield_2020_file
clean_yield_2020 <- function(yield_2020_file) {

  # Read in the yield file
  yield_2020 <- read_excel(here("data", "yield_2020", "Mian_Yield_2020.xlsx"))

  # The columns to be converted to numerics
  numcols <- c("plot",
               "md",
               "lod",
               "ht",
               "yield",
               "sdwt",
               "sq",
               "protein_dry_basis",
               "oil_dry_basis")

  # Clean up the names, select only the columns that are wanted, rename
  # columns to match the 2021 data, and convert columns holding numeric
  # data to numerics
  yield_2020_clean <- yield_2020 %>%
    clean_names() %>%
    select(genotype, loc, test, year, rep, code, plot, fc, md, pc, lod, ht,
           yield, sdwt, sq, protein_dry_basis, oil_dry_basis) %>%
    rename(pub = pc) %>%
    mutate(across(any_of(numcols), as.numeric))

  return(yield_2020_clean)
}
