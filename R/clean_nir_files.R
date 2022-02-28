clean_nir_files <- function(files = NULL, nir_masterfile = NULL) {

  # Read in the nir masterfile
  masterfile <- readr::read_csv(nir_masterfile)

  # A function to read in and clean each nir file from the files argument
  clean_nir_export <- function(file, nir_lookup = masterfile)
  {
    nir_df <- readxl::read_excel(file) %>%
      janitor::clean_names() %>%
      dplyr::mutate(nir_number_extracted = toupper(stringr::str_extract(sample_id, stringr::regex("nir-[0-9]+", ignore_case = TRUE)))) %>%
      dplyr::select(sample_id, date_time_of_analysis, predicted_moisture_percent, predicted_protein_dry_basis_percent, predicted_oil_dry_basis_percent, nir_number_extracted) %>%
      tidyr::separate(sample_id, into = c("year", "loc", "test", "code", "genotype", "rep", "nir_no"), sep = "_") %>%
      dplyr::mutate(nir_no = nir_number_extracted) %>%
      dplyr::rename(moisture = predicted_moisture_percent,
                    oil_dry_basis = predicted_oil_dry_basis_percent,
                    protein_dry_basis = predicted_protein_dry_basis_percent) %>%
      dplyr::mutate(year = ifelse(stringr::str_detect(year, stringr::regex("nir", ignore.case = TRUE)), NA, year)) %>%
      dplyr::select(nir_no, date_time_of_analysis, moisture, oil_dry_basis, protein_dry_basis) %>%
      dplyr::left_join(nir_lookup, by = c("nir_no" = "NIR_No")) %>%
      dplyr::select(test, date_time_of_analysis, cross, Rows, color, plant_no, loc, year, moisture, nir_no, oil_dry_basis, protein_dry_basis) %>%
      dplyr::rename(code = cross, plot = Rows, rep = color, genotype = plant_no)

    return(nir_df)
  }

  # Apply the cleaning function to each nir export and then bind the results together
  CleanedFiles <- purrr::map(files, clean_nir_export) %>%
    purrr::reduce(dplyr::bind_rows) %>%
    dplyr::distinct()

  # Do some error checking and remove obvious errors from the merged NIR data
  # These obvious errors are any observations with negative values for
  # moisture, oil, or protein. Other more subtle (potential) errors will be identified
  # in a separate step
  Cleaned_no_negative <- CleanedFiles %>%
    filter(oil_dry_basis > 0 & protein_dry_basis > 0 &  moisture > 0 & !is.na(nir_no)) %>%
    group_by(nir_no) %>%
    top_n(1, date_time_of_analysis) %>%
    ungroup() %>%
    select(-date_time_of_analysis)

  return(Cleaned_no_negative)
}
