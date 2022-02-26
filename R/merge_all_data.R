#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param yield
#' @param nir
#' @param twt
merge_all_data <- function(yield = cleaned_yield_files, nir =
                           cleaned_nir_files, twt = cleaned_test_weight,
                           twt_corrections = twt_genotype_corrections) {

  # Semi join test weight data to yield data to find test weight samples with a match
  has_match <- semi_join(twt, yield, by = c("test", "loc", "genotype", "rep"))

  # Read in the genotype corrections and then add these corrected observations
  # to the observations that had a match in the yield data
  genotype_corrections <- read_csv(twt_corrections) %>%
    mutate(genotype = correct_genotype_name) %>%
    select(-correct_genotype_name) %>%
    mutate(rep = as.character(rep))

  twt <- bind_rows(has_match, genotype_corrections)

  # Correct column types so that the test weight and yield data can be combined
  yield$rep <- as.numeric(yield$rep)
  twt$rep   <- as.numeric(twt$rep)

  MergedData <- yield %>%
    mutate(loc = toupper(loc)) %>%
    left_join(nir, by = c("test", "genotype", "loc", "rep", "year", "code", "plot")) %>%
    left_join(twt, by = c("test", "genotype", "loc", "rep", "year")) %>%
    distinct() %>%
    select(-id)

  return(MergedData)
}
