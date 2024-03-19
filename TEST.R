# Load necessary libraries
library(readr)
library(RSQLite)
library(lubridate)



# Define function to check email
isValidEmail <- function(x) {
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
}

# Function to validate and ingest data frames into SQLite database
validate_and_ingest <- function(df, table_name, primary_key) {
  valid <- TRUE
  
  # Check primary key exists and is unique
  if (any(is.na(df[[primary_key]])) || any(duplicated(df[[primary_key]]))) {
    valid <- FALSE
    print(paste("Table:", table_name, "- Error: Primary key issue"))
  }
  
  # Other validation checks can be added here
  
  if (valid) {
    print(paste("Table:", table_name, "- Status: OK"))
    # Write to SQLite database
    dbWriteTable(my_connection, table_name, df, append = TRUE)
  } else {
    print(paste("Table:", table_name, "- Status: ERROR"))
  }
}

# Directory containing CSV files
directory <- "Data_upload"

# Read CSV files from the directory and categorize them into data frames
data_frames <- read_and_categorize_csv(directory)

# Connect to SQLite database
my_connection <- dbConnect(SQLite(), "mydatabase.db")

# Validate and ingest each data frame
validate_and_ingest(data_frames$Category, "Category", "category_id")
validate_and_ingest(data_frames$Customer, "Customer", "customer_id")
validate_and_ingest(data_frames$Orders, "Orders", "order_id")
validate_and_ingest(data_frames$Payment, "Payment", "payment_id")
validate_and_ingest(data_frames$Product, "Product", "product_id")
validate_and_ingest(data_frames$Promotion, "Promotion", "promotion_id")
validate_and_ingest(data_frames$Sales, "Sales", "sale_id")
validate_and_ingest(data_frames$Settlement, "Settlement", "settlement_id")
validate_and_ingest(data_frames$Supplier, "Supplier", "supplier_id")

# Close database connection
dbDisconnect(my_connection)














# Function to read CSV files from a directory and categorize them into different data frames
read_csv <- function(directory) {
  csv_files <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)
  
  # Initialize a list to store data frames
  data_frames <- list(
    customer <- NULL,
    product <- NULL,
    address <- NULL,
    discount <- NULL,
    supplier <- NULL,
    category <- NULL,
    advertisement <- NULL,
    advertise_in <- NULL,
    order_item <- NULL,
    order_detail <- NULL)
  
  # Loop through each CSV file
  for (csv_file in csv_files) {
    # Read CSV file into a data frame
    data <- read.csv(csv_file)
    
    # Extract file name without extension
    file_name <- tools::file_path_sans_ext(basename(csv_file))

    # Determine which data frame to store the data
    df_name <- sub("s$", "", file_name)  # Remove plural form
    if (df_name %in% names(data_frames)) {
      data_frames[[df_name]] <- rbind(data_frames[[df_name]], data)
      cat("Variables in", df_name, "data frame:\n")
      print(names(data_frames[[df_name]]))
    }
  }
  return(data_frames)
}


# Directory containing CSV files
directory <- "data_upload"

# Read CSV files from the directory and categorize them into data frames
data_frames <- read_csv(directory)

# Access each data frame by its name
customer <- data_frames$customer
product <- data_frames$product
address <- data_frames$address
discount <- data_frames$discount
supplier <- data_frames$supplier
category <- data_frames$category
advertisement <- data_frames$advertisement
advertise_in <- data_frames$advertise_in
order_item <- data_frames$order_item
order_detail <- data_frames$order_detail
