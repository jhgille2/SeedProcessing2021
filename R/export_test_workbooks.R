#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param merged_data
#' @param export_directory
#' @return
#' @author 'Jay
#' @export
export_test_workbooks <- function(merged_data, export_directory =
                                  here("exports", "yield_files")) {

  # First split the merged data by test
  merged_data <- merged_data %>%
    dplyr::arrange(test, loc, code, rep)

  # Split this merged data into chunks by test
  merged_split <- split(merged_data, merged_data$test)

  # A function tp create an excel workbook from each element of the split list
  make_excel_workbook <- function(test_data, test_name)
  {
    # First, do some final formatting to the test data before export.
    # I left these cleaning steps till the very end because they either
    # remove data that could be helpful for programmable cleaning or would
    # make working with the data in R more difficult, for example changing
    # the column names to more humen readable (but less computer readable)
    # names
    test_data %<>%
      dplyr::select(test,
                    loc,
                    year,
                    genotype,
                    code,
                    rep,
                    plot,
                    notes,
                    note1,
                    md,
                    fc,
                    pub,
                    ht,
                    lod,
                    oil_dry_basis,
                    protein_dry_basis,
                    yield,
                    sdwt,
                    sq,
                    twt_weight) %>%
      dplyr::rename(Test            = test,
                    Location        = loc,
                    Year            = year,
                    Genotype        = genotype,
                    Code            = code,
                    Rep             = rep,
                    Plot            = plot,
                    Notes           = notes,
                    `Extra notes`   = note1,
                    `Maturity date` = md,
                    `Flower color`  = fc,
                    Pubescence      = pub,
                    Height          = ht,
                    Yield           = yield,
                    Lodginig        = lod,
                    Oil             = oil_dry_basis,
                    Protein         = protein_dry_basis,
                    `Test weight`   = twt_weight) %>%
      dplyr::mutate(`Oil + Protein` = Oil + Protein) %>%
      dplyr::relocate(`Oil + Protein`, .after = Protein)

    # Create a workbook object
    wb <- openxlsx::createWorkbook()

    # Add a worksheet that has the same name as the test name
    openxlsx::addWorksheet(wb, test_name)
    openxlsx::writeData(wb, test_name, test_data)

    # Make a path to save the new workbook to
    savepath <- paste0(export_directory, "/", test_name, ".xlsx")

    # And save the workbook to this directory
    openxlsx::saveWorkbook(wb, file = savepath, overwrite = TRUE)
  }

  # Apply this function to the merged_split list and use the names of the list
  # (the test names) to name the files
  purrr::walk2(merged_split, names(merged_split), make_excel_workbook)
}
