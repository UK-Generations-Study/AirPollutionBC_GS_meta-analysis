# Load RDS Data
#' MEJones
#' APril 2025
#'
#' This function loads and processes CSV data files along with
#' corresponding data dictionary files
#'
#' @param RDS_VERSION A character string specifying the folder
#' for the version of the frozen RDS data.
#' @param CSV_DATA_FILE_NAME A character string specifying the
#' name of the RDS CSV data file.
#' @param excl_date A Date specifying the exclusion date threshold
#' (default is today).
#' @param cancel_date A Date specifying the cancellation date threshold
#' (default is today).
#' @return A data frame containing the loaded RDS CSV data.
#' @export
#' @examples
#' riskfactors <- load_RDS_data("v20230927", "RDSRiskFactors.csv",
#'                              excl_date = "2025-04-01",
#'                              cancel_date = "2025-05-01")
#' Loads RDSRIskFactors.csv from v20230927 RDS folder
#' and reads data dictionary to determine variable type and label.
#' Also excludes any particpants who were excluded on or before excl_date
#' and were not cancelled exclusions before cancel_date

# Main function to load RDS data
load_RDS_data <- function(RDS_VERSION,
                          CSV_DATA_FILE_NAME,
                          excl_date = NULL,
                          cancel_date = NULL) {
  # If excl_date is provided as a string, convert it to a Date object
  if (!is.null(excl_date)) {
    excl_date <- as.Date(excl_date)
  } else {
    excl_date <- Sys.Date()  # Default to today's date if not provided
  }
  
  # If cancel_date is provided as a string, convert it to a Date object
  if (!is.null(cancel_date)) {
    cancel_date <- as.Date(cancel_date)
  } else {
    cancel_date <- Sys.Date()  # Default to today's date if not provided
  }
  
  # Get the file path using the OS-independent function
  file_path <- get_file_path(RDS_VERSION)
  # Construct the full file path for the CSV file
  full_file_path <- file.path(file_path, CSV_DATA_FILE_NAME)
  # Check if the file exists
  if (!file.exists(full_file_path)) {
    stop("File does not exist: ", full_file_path)
  }
  print("starting data load...")
  
  # Read data dictionary from frozen RDS rollout
  ddfields <- read_data_dictionary(RDS_VERSION)
  # Read variable names from frozen CSV data file
  csv_names <- read_variable_names(RDS_VERSION, CSV_DATA_FILE_NAME)
  # Merge variable names from RDS CSV data file with data dictionary
  merged_data <- merge_with_data_dictionary(csv_names, ddfields)
  # Define variable types based on data dictionary
  merged_data <- define_variable_types(merged_data)
  # Read RDS CSV data file using defined variable types from data dictionary
  csv_data_temp <- read_csv_data(RDS_VERSION, CSV_DATA_FILE_NAME, merged_data)
  # Check for problems in loading CSV data file
  check_csv_data(RDS_VERSION, csv_data_temp, CSV_DATA_FILE_NAME)
  
  joined_dataset_excluded <- apply_exclusions(csv_data_temp,
                                              excl_date,
                                              cancel_date)
  # joined_dataset_excluded <-csv_data_temp
  
  return(joined_dataset_excluded)
}

################################################################################
# Function to construct the file path based on OS and RDS_VERSION
get_file_path <- function(RDS_VERSION) {
  if (Sys.info()["sysname"] == "Windows") {
    # For Windows use actual Windows directory
    file_path <- file.path("//rds", "data", "dge", "dudge", "shared",
                           "GENERATIONS", "CoreData", RDS_VERSION)
  } else {
    # For Linux or RHEL, use the mounted path
    file_path <- file.path("/mnt", "R", "CoreData", RDS_VERSION)
  }
  
  file_path
}
################################################################################
# Function to read data dictionary
read_data_dictionary <- function(RDS_VERSION) {
  file_path <- get_file_path(RDS_VERSION)
  ddfields <- read_csv(file.path(file_path, "DDFields.csv"),
                       na = c("NULL", ""),
                       show_col_types = FALSE)
  ddfields <- ddfields %>% filter(!is.na(rds_variable))
  ddfields <- subset(ddfields, select = c(tableid,
                                          fieldid,
                                          fieldname,
                                          datatype,
                                          fielddesc))
  ddfields <- ddfields[order(ddfields$tableid, ddfields$fieldid), ]
  ddfields$lowerfieldname <- tolower(ddfields$fieldname)
  ddfields <- ddfields[!duplicated(ddfields$lowerfieldname), ]
  
  ddfields
}
################################################################################
# Function to read variable names from CSV data file
read_variable_names <- function(RDS_VERSION, CSV_DATA_FILE_NAME) {
  file_path <- get_file_path(RDS_VERSION)
  csv_data_temp <- read_csv(file.path(file_path, CSV_DATA_FILE_NAME),
                            na = c("NULL", ""),
                            n_max = 1,
                            show_col_types = FALSE)
  csv_names <- data.frame(names(csv_data_temp))
  csv_names <- csv_names %>% mutate(order = row_number())
  
  csv_names
}
################################################################################
# Function to merge variable names with data dictionary
merge_with_data_dictionary <- function(csv_names, ddfields) {
  temp <- sqldf(
    "SELECT csv.\"names.csv_data_temp.\",
            csv.\"order\",
            dd.fieldname,
            dd.datatype,
            dd.fielddesc
    FROM csv_names as csv
    LEFT JOIN ddfields as dd on
    csv.\"names.csv_data_temp.\" COLLATE NOCASE = dd.fieldname COLLATE NOCASE"
  )
  unique(temp$fieldname)
  
  temp
}
################################################################################
# Function to define variable types
define_variable_types <- function(merged_data) {
  merged_data <- merged_data %>%
    mutate(variable_type = case_when(
      datatype %in% c("char", "nvarchar", "text", "varchar", "Yes/No") ~ "c",
      datatype %in% c("datetime", "Datetime") ~ "T",
      datatype %in% c("date") ~ "D",
      datatype %in% c("float", "num", "numeric", "real") ~ "d",
      datatype %in% c("int", "smallint") ~ "i",
      TRUE ~ "c"
    ))
  
  merged_data
}
################################################################################
# Function to read CSV data file with defined variable types
read_csv_data <- function(RDS_VERSION, CSV_DATA_FILE_NAME, merged_data) {
  file_path <- get_file_path(RDS_VERSION)
  merged_data <- merged_data[order(merged_data$order), ]
  ctypes <- merged_data %>% select(variable_type) %>% pull
  cdesc <- merged_data %>% select(fielddesc) %>% pull
  csv_data_temp <- read_csv(file.path(file_path, CSV_DATA_FILE_NAME),
                            col_types = paste(ctypes, collapse = ""),
                            na = c("NULL", "NUL", ""),
                            show_col_types = FALSE)
  # Set variable labels on the data frame
  for (i in seq_along(cdesc)) {
    var_name <- names(csv_data_temp)[i]  # Get the variable name
    label(csv_data_temp[[var_name]]) <- cdesc[i]  # Assign label to variable
  }
  
  csv_data_temp
}
################################################################################
# Function to check CSV data for problems
check_csv_data <- function(RDS_VERSION, csv_data_temp, CSV_DATA_FILE_NAME) {
  file_path <- get_file_path(RDS_VERSION)
  cat(paste0("Checking CSV data file: \n",
             file.path(file_path,
                       "CoreData",
                       RDS_VERSION,
                       CSV_DATA_FILE_NAME)))
  if (nrow(problems(csv_data_temp)) == 0) {
    cat(paste("\nSuccessful CSV import:", nrow(csv_data_temp), "rows\n"))
  } else {
    cat(paste("\nThere are problems with the CSV import\n"))
    print(problems(csv_data_temp))
  }
}
################################################################################
# Function to apply exclusions
apply_exclusions <- function(csv_data, excl_date, cancel_date) {
  # Determine the path to the exclusions file based on OS
  exclusions_file <- if (Sys.info()["sysname"] == "Windows") {
    file.path("//rds", "data", "dge", "dudge", "shared", "GENERATIONS", "Exclusions", 
              "RDSExclusions.csv")
  } else {
    file.path("/mnt", "R", "RDSExclusions.csv")
  }
  # Read exclusions and convert date columns
  all_exclusions <- read.csv(exclusions_file)
  all_exclusions$ExclDate <- as.Date(all_exclusions$ExclDate,
                                     format = "%d-%m-%Y")
  # all_exclusions$CancelledDate <- as.Date(all_exclusions$CancelledDate,
  #                                        format = "%d-%m-%Y")
  
  # Filter based on provided thresholds
  valid_exclusions <- all_exclusions 
  #%>%
   # filter(ExclDate <= excl_date,
    #       is.na(CancelledDate) | CancelledDate > cancel_date)
  
  # Create temporary join key
  valid_exclusions$temp_TCode <- as.character(valid_exclusions$TCode)
  csv_data$temp_TCode <- as.character(csv_data$TCode)
  
  # Apply exclusions
  joined_dataset_excluded <- anti_join(csv_data,
                                       valid_exclusions,
                                       by = "temp_TCode")
  check_exclusion <- semi_join(csv_data, 
                               valid_exclusions,
                               by = "temp_TCode")
  
  # Print summary
  cat(paste("\nExclusions:", nrow(check_exclusion), "row(s)\n"))
  cat(paste("\nFinal CSV dataset:", nrow(joined_dataset_excluded), "rows\n"))
  
  # Clean up temp columns
  joined_dataset_excluded$temp_TCode <- NULL
  
  joined_dataset_excluded
}
