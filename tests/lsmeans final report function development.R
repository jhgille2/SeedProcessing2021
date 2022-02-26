tar_load(phenotype_lsmeans)


means_bytest <- phenotype_lsmeans %>%
  group_by(test) %>%
  nest() %>%
  mutate(data = map(data, janitor::remove_empty, "cols"))


# Make a workbook for each lsmeans table

# First, get example data for a test that was grown in two locations
# and a test that was only grown in one location
one_loc_ex <- means_bytest$data[[1]]
two_loc_ex <- means_bytest$data[[2]]

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

  data_start_row <- 7
  header_merge_start_row <- data_start_row - 1

  # Write the lsmeans data starting on line 7
  openxlsx::writeData(wb, means_sheet_name, data, startRow = data_start_row)

  # Get the indices of the by-location columns to merge and what etxt to put in them
  merge_indices <- get_loc_merge_columns(data)

  for(i in 1:length(merge_indices)){

    # Write the location name in the starting column of the location data,
    # and one row above the data header
    location_means_name <- names(merge_indices)[[i]]

    writeData(wb,
              means_sheet_name,
              location_means_name,
              startCol = merge_indices[[i]][[1]],
              startRow = header_merge_start_row)

    merge_cols <- c(merge_indices[[i]][[1]]:merge_indices[[i]][[2]])

    mergeCells(wb,
               means_sheet_name,
               cols = merge_cols,
               rows = header_merge_start_row)
  }


  wbname <- paste0(means_sheet_name, ".xlsx")
  saveWorkbook(wb, file = here("tests", "data", wbname), overwrite = TRUE)
}


firstrow <- means_bytest[1:3, ]

pmap(firstrow, lsmeans_final_report)
