#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param phenotype_lsmeans
#' @param export_directory
export_lsmean_workbooks <- function(phenotype_lsmeans, export_directory =
                                    here("exports", "lsmean_files")) {

  # A function that gets the starting and ending columns for the
  # by location and overall lsmeans if they exist. Returns a named list
  # where each element holds the starting and ending columns
  # for the by location or overall lsmeans as indicated by the name of the
  # list element.
  get_loc_merge_columns <- function(data){

    # These column names have a dash in them.
    # This indicates that the means in these columns
    # come from one location
    loc_indices <- str_detect(colnames(data), " - ") %>% which()

    # Set these column names aside
    loc_colnames <- colnames(data)[loc_indices]

    # Now, get the specific location name from these column names
    locs <- map_chr(loc_colnames, function(x) str_split(x, " - ")[[1]][[1]])

    # Get the starting and ending col;umn numbers for each unique location
    unique_locs <- unique(locs)

    # A function to get the starting and ending position of the a location
    # in the original dataframe
    get_location_range <- function(locname, locIndices, location_column_names){
      loc_range <- range(which(location_column_names == locname))

      original_range <- c(locIndices[loc_range[[1]]], locIndices[loc_range[[2]]])

      return(original_range)
    }

    merge_ranges <- map(unique_locs, get_location_range, loc_indices, locs) %>%
      set_names(unique_locs)

    # Check if table has overall lsmeans
    overall_indices <- str_detect(colnames(data), " - ", negate = TRUE) %>% which()
    overall_indices <- overall_indices[which(overall_indices != 1)] %>% sort()

    if(length(overall_indices != 0)){
      merge_ranges$Overall <- c(overall_indices[1], overall_indices[length(overall_indices)])
    }

    return(merge_ranges)
  }


  # A function to make an excel workbook report for lsmeans data
  lsmeans_final_report <- function(test, data){

    # Create an excel workbook
    wb <- openxlsx::createWorkbook()

    # Add a worksheet to hold the lsmeans
    means_sheet_name <- paste(test, "LSMeans", sep = " - ")
    addWorksheet(wb, sheetName = means_sheet_name)

    data_start_row <- 3
    header_merge_start_row <- data_start_row - 1

    # Make styles to apply to different cell ranges based on the type of
    # data they hold

    # STyle for the lsmeans
    lsmeans_data_style <- openxlsx::createStyle(fontSize    = 11,
                                                border      = "TopBottomLeftRight",
                                                borderStyle = "thin",
                                                halign      = "right")

    # style for the headers and genotypes
    lsmeans_header_style <- openxlsx::createStyle(fontSize       = 11,
                                                  border         = "TopBottomLeftRight",
                                                  borderStyle    = "thin",
                                                  textDecoration = "bold",
                                                  halign         = "center")

    # Starting and ending rows of the lsmeans data
    lsmeans_start_row <- data_start_row + 1
    lsmeans_end_row   <- data_start_row + nrow(data)

    # Get the indices of the by-location columns to merge and what text to put in them
    merge_indices <- get_loc_merge_columns(data)

    # Write the lsmeans data starting on line 7 and apply the lsmeans style to it

    # First, remopve locations from the column names
    # A function to remove location names from column names
    remove_loc <- function(colname){

      split_name <- str_split(colname, " - ")[[1]]

      ifelse(length(split_name) == 2, split_name[[2]], split_name[[1]])

    }
    colnames(data) <- map_chr(colnames(data), remove_loc)

    openxlsx::writeData(wb, means_sheet_name, data, startRow = data_start_row)
    openxlsx::addStyle(wb, means_sheet_name,
                       style = lsmeans_data_style,
                       cols = 2:ncol(data),
                       gridExpand = TRUE,
                       rows = lsmeans_start_row:lsmeans_end_row)

    # Add the header style to the header and the genotype column
    openxlsx::addStyle(wb, means_sheet_name,
                       style = lsmeans_header_style,
                       cols = 2:ncol(data),
                       gridExpand = TRUE,
                       rows = header_merge_start_row)

    openxlsx::addStyle(wb, means_sheet_name,
                       style      = lsmeans_header_style,
                       cols       = 1:ncol(data),
                       gridExpand = TRUE,
                       rows       = data_start_row)

    openxlsx::addStyle(wb, means_sheet_name,
                       style      = lsmeans_header_style,
                       cols       = 1,
                       gridExpand = TRUE,
                       rows       = lsmeans_start_row:lsmeans_end_row)

    # Starting and ending rows of the lsmeans data
    lsmeans_start_row <- data_start_row + 1
    lsmeans_end_row   <- data_start_row + nrow(data)

    # Set the column widths so that they can fit the widths of the data
    width_vec     <- apply(data, 2, function(x) max(nchar(as.character(x)) + 2, na.rm = TRUE))
    col_width_vec <- map_dbl(colnames(data), function(x) nchar(x))

    width_vec <- map2_dbl(width_vec, col_width_vec, max)

    setColWidths(wb,
                 means_sheet_name,
                 cols   = 1:ncol(data),
                 widths = width_vec)

    for(i in 1:length(merge_indices)){

      # Write the location name in the starting column of the location data,
      # and one row above the data header
      location_means_name <- names(merge_indices)[[i]]

      # Add the header style to the cell the location name is going to
      addStyle(wb,
               means_sheet_name,
               style = lsmeans_header_style,
               rows = header_merge_start_row,
               cols = merge_indices[[i]][[1]])

      # Write the location name to this cell
      writeData(wb,
                means_sheet_name,
                location_means_name,
                startCol = merge_indices[[i]][[1]],
                startRow = header_merge_start_row)

      # Find the cells to merge and then merge them
      merge_cols <- c(merge_indices[[i]][[1]]:merge_indices[[i]][[2]])

      mergeCells(wb,
                 means_sheet_name,
                 cols = merge_cols,
                 rows = header_merge_start_row)
    }

    # Save the workbook
    wbname <- paste0(means_sheet_name, ".xlsx")
    saveWorkbook(wb, file = paste0(export_directory, "/", wbname), overwrite = TRUE)
  }

  # Group the lsmean data by test
  means_bytest <- phenotype_lsmeans %>%
    group_by(test) %>%
    nest() %>%
    mutate(data = map(data, janitor::remove_empty, "cols"))

  pmap(means_bytest, lsmeans_final_report)

  all_workbook_paths <- paste0(export_directory, "/", paste(means_bytest$test, "LSMeans", sep = " - "), ".xlsx")
  return(all_workbook_paths)
}
