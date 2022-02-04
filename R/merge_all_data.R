#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param yield
#' @param nir
#' @param twt
merge_all_data <- function(yield = cleaned_yield_files, nir =
                           cleaned_nir_files, twt = cleaned_test_weight) {

  yield$rep <- as.numeric(yield$rep)
  twt$rep   <- as.numeric(twt$rep)

  MergedData <- yield %>%
    mutate(loc = toupper(loc)) %>%
    #filter(loc == "PLY") %>%
    left_join(nir, by = c("test", "genotype", "loc", "rep", "year", "code", "plot")) %>%
    left_join(twt, by = c("test", "genotype", "loc", "rep", "year")) %>%
    distinct() %>%
    select(-id)

  return(MergedData)
}
