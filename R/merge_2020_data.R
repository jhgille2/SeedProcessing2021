#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param yield_2020_files
merge_2020_data <- function(yield_2020_files) {

  all_2020_data <- map(yield_2020_files, readxl::read_excel, na = c(".", "", "NA")) %>%
    reduce(bind_rows) %>%
    janitor::clean_names() %>%
    rename(protein_dry_basis = pro,
           oil_dry_basis     = oil,
           yield             = yieldg)

  return(all_2020_data)

}

