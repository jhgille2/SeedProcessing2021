clean_test_weight <- function(files = NULL) {

  # A function to read in the file
  read_fn <- function(file)
  {
    # Get the extension of the test weight file
    fileext <- tools::file_ext(file)

    # Read in the file as long as its either a .csv or a excel worksheet
    if(fileext == "csv"){
      out <- read.csv(file, header = TRUE)
    }else if(fileext == "xlsx"){
      out <- readxl::read_excel(file)
    }else{
      print("Please save test weight files either as a .csv or as an excel workbook (.xlsx or .xls")
      return(NULL)
    }

    return(out)
  }

  # Read in each test weight file with this function, combine them into a single data frame,
  # select a subset of the columns and add a "twt" prefix to these columns
  all_test_weight_data <- purrr::map(files, read_fn) %>%
    purrr::reduce(dplyr::bind_rows) %>%
    janitor::clean_names() %>%
    dplyr::select(sample_id, moisture, weight, temperature, date_time) %>%
    dplyr::rename_at(vars(moisture:date_time), function(x) paste0("twt_", x)) %>%
    tidyr::separate(sample_id, into = c("plot", "genotype", "test", "loc", "year", "rep"), sep = "_") %>%
    dplyr::select(test, genotype, test, loc, year, rep, twt_moisture, twt_weight, twt_temperature, twt_date_time) %>%
    dplyr::mutate(year = as.numeric(year)) %>%
    distinct()

  return(all_test_weight_data)
}
