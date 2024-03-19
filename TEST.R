# Load necessary libraries
library(readr)
library(RSQLite)
library(lubridate)

# Function to read CSV files from a directory and categorize them into different data frames
read_and_categorize_csv <- function(directory) {
  # Get list of CSV files in the directory
  csv_files <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)
  
  # Initialize a list to store data frames
  data_frames <- list(
    Category = NULL,
    Customer = NULL,
    Orders = NULL,
    Payment = NULL,
    Product = NULL,
    Promotion = NULL,
    Sales = NULL,
    Settlement = NULL,
    Supplier = NULL
  )
  
  # Loop through each CSV file
  for (csv_file in csv_files) {
    # Read CSV file into a data frame
    data <- read.csv(csv_file)
    
    # Extract file name without extension
    file_name <- tools::file_path_sans_ext(basename(csv_file))
    
    # Convert column names to lowercase
    colnames(data) <- tolower(colnames(data))
    
    # Determine which data frame to store the data
    df_name <- sub("s$", "", file_name)  # Remove plural form
    if (df_name %in% names(data_frames)) {
      data_frames[[df_name]] <- rbind(data_frames[[df_name]], data)
      cat("Variables in", df_name, "data frame:\n")
      print(names(data_frames[[df_name]]))
    }
  }
  
  # Return the list of data frames
  return(data_frames)
}

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





















# Return the list of data frames
return(data_frames)
}

# Directory containing CSV files
directory <- "Data_upload"



# Read CSV files from the directory and categorize them into data frames
data_frames <- read_and_categorize_csv(directory)

# Access each data frame by its name
Category <- data_frames$Category
Customer <- data_frames$Customer
Orders <- data_frames$Orders
Payment <- data_frames$Payment
Product <- data_frames$Product
Promotion <- data_frames$Promotion
Sales <- data_frames$Sales
Settlement <- data_frames$Settlement
Supplier <- data_frames$Supplier