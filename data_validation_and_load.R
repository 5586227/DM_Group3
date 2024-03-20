library(DBI)

read_csv_files <- function(directory) {
  csv_files <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)
  data_frames <- list()
  
  for (csv_file in csv_files) {
    # Extract the base file name without extension
    file_name <- tools::file_path_sans_ext(basename(csv_file))
    
    stem <- gsub("\\d+$", "", file_name)
    
    # Read the CSV file
    data <- read.csv(csv_file)
    
    # Check if data frame already exists
    if (stem %in% names(data_frames)) {
      # If exists, append data to existing data frame
      data_frames[[stem]] <- rbind(data_frames[[stem]], data)
    } else {
      # Otherwise, create new data frame
      data_frames[[stem]] <- data
    }
  }
  return(data_frames)
}

## Read csv files and assign to "data_frames"
data_frames <- read_csv_files("data_upload")


# Check for duplicate key in each table
##List primary key for each table
primary_keys <- list(
  "ADDRESS" = c("address_id"),
  "DISCOUNT" = c("promo_code"),
  "SUPPLIER" = c("supplier_id"),
  "CATEGORY" = c("category_id"),
  "ADVERTISEMENT" = c("ad_id"),
  "CUSTOMER" = c("customer_id"),
  "PRODUCT" = c("product_id"),
  "ADVERTISE_IN" = c("ad_id", "product_id"),
  "ORDER_DETAIL" = c("order_id"),
  "ORDER_ITEM" = c("order_id","product_id")
)

## Function to check for duplicate primary key and remove duplicates
remove_duplicate_primary_keys <- function(df, primary_key_cols) {
  unique_rows <- !duplicated(df[, primary_key_cols])
  return(df[unique_rows, ])
}


## Apply the function for each data frame
for (table_name in names(data_frames)) {
  if (table_name %in% names(primary_keys)) {
    data_frames[[table_name]] <- remove_duplicate_primary_keys(data_frames[[table_name]], primary_keys[[table_name]])
  }
}

# Check missing data
## Function to check for missing values in columns except those specified to skip
check_missing_values <- function(df, columns_to_skip, table_name) {
  columns <- setdiff(names(df), columns_to_skip)
  missing <- sapply(df[columns], function(x) any(is.na(x) | x == ""))
  if (table_name == "ORDER_DETAIL") {
    missing["promo_code"] <- FALSE  # Allow missing values in "promo_code" for "ORDER_DETAIL" table
  }
  return(missing)
}

## Columns to skip
columns_to_skip <- c("gender", "product_description", "product_rating", "main_product_id")

## Apply the function for each data frame
for (table_name in names(data_frames)) {
  if (table_name %in% names(primary_keys)) {
    missing_values <- check_missing_values(data_frames[[table_name]], columns_to_skip, table_name)
    if (any(missing_values)) {
      empty_row_indices <- apply(data_frames[[table_name]][, -which(names(data_frames[[table_name]]) %in% columns_to_skip)], 1, function(row) any(row == "" | is.na(row)))
      data_frames[[table_name]] <- data_frames[[table_name]][!empty_row_indices, ]
    }
  }
}

# Check name format
## Function to check for invalid name format in columns
check_name_format <- function(df) {
  invalid_format <- rep(FALSE, nrow(df))
  for (i in 1:nrow(df)) {
    if (!grepl("^[A-Z][a-z]*$", df[i, "first_name"]) || !grepl("^[A-Z][a-z]*$", df[i, "last_name"])) {
      invalid_format[i] <- TRUE
    }
  }
  return(invalid_format)
}

## Apply the function for the "customer" table
if ("CUSTOMER" %in% names(data_frames)) {
  customer_df <- data_frames$CUSTOMER
  if (nrow(customer_df) > 0) {
    invalid_format <- check_name_format(customer_df)
    if (any(invalid_format)) {
      data_frames$CUSTOMER <- customer_df[!invalid_format, ]
    }
  }
}


# Check product/category/supplier name
## Function to keep records if the specified column matches the regex pattern
naming_format <- function(df, column_to_check, regex_pattern) {
  matching_records <- grepl(regex_pattern, df[[column_to_check]], perl = TRUE)
  return(df[matching_records, ])
}

## Define regex pattern
regex_pattern <- "^[A-Za-z,.-]+( [A-Za-z,.-]+)*$"

## Apply the function for the specified columns in the respective tables
if ("CATEGORY" %in% names(data_frames)) {
  data_frames[["CATEGORY"]] <- naming_format(data_frames[["CATEGORY"]], "category_name", regex_pattern)
}

if ("SUPPLIER" %in% names(data_frames)) {
  data_frames[["SUPPLIER"]] <- naming_format(data_frames[["SUPPLIER"]], "supplier_name", regex_pattern)
}

if ("PRODUCT" %in% names(data_frames)) {
  data_frames[["PRODUCT"]] <- naming_format(data_frames[["PRODUCT"]], "product_name", regex_pattern)
}

# Foreign key check
##CUSTOMER - ADDRESS
invalid_address_fk <- data_frames$CUSTOMER[!data_frames$CUSTOMER$address_id %in% data_frames$ADDRESS$address_id, ]
data_frames$CUSTOMER <- data_frames$CUSTOMER[data_frames$CUSTOMER$address_id %in% data_frames$ADDRESS$address_id, ]

##PRODUCT - CATEGORY - SUPPLIER
invalid_category_fk <- data_frames$PRODUCT[!data_frames$PRODUCT$category_id %in% data_frames$CATEGORY$category_id, ]
data_frames$PRODUCT <- data_frames$PRODUCT[data_frames$PRODUCT$category_id %in% data_frames$CATEGORY$category_id, ]

invalid_supplier_fk <- data_frames$PRODUCT[!data_frames$PRODUCT$supplier_id %in% data_frames$SUPPLIER$supplier_id, ]
data_frames$PRODUCT <- data_frames$PRODUCT[data_frames$PRODUCT$supplier_id %in% data_frames$SUPPLIER$supplier_id, ]

##ORDER_DETAIL - CUSTOMER - DISCOUNT
invalid_customer_fk <- data_frames$ORDER_DETAIL[!data_frames$ORDER_DETAIL$customer_id %in% data_frames$CUSTOMER$customer_id, ]
data_frames$ORDER_DETAIL <- data_frames$ORDER_DETAIL[data_frames$ORDER_DETAIL$customer_id %in% data_frames$CUSTOMER$customer_id, ]

discounted_order <- data_frames$ORDER_DETAIL[!is.na(data_frames$ORDER_DETAIL$promo_code) & trimws(data_frames$ORDER_DETAIL$promo_code) != "", ]
invalid_promo_fk <- discounted_order[!discounted_order$promo_code %in% data_frames$DISCOUNT$promo_code, ]
data_frames$ORDER_DETAIL <- data_frames$ORDER_DETAIL[is.na(data_frames$ORDER_DETAIL$promo_code) | trimws(data_frames$ORDER_DETAIL$promo_code) == "" | discounted_order$promo_code %in% data_frames$DISCOUNT$promo_code, ]

##ORDER_ITEM - PRODUCT - ORDER_DETAIL
invalid_product_fk <- data_frames$ORDER_ITEM[!data_frames$ORDER_ITEM$product_id %in% data_frames$PRODUCT$product_id, ]
data_frames$ORDER_ITEM <- data_frames$ORDER_ITEM[data_frames$ORDER_ITEM$product_id %in% data_frames$PRODUCT$product_id, ]

invalid_order_fk       <- data_frames$ORDER_ITEM[!data_frames$ORDER_ITEM$order_id %in% data_frames$ORDER_DETAIL$order_id, ]
data_frames$ORDER_ITEM <- data_frames$ORDER_ITEM[ data_frames$ORDER_ITEM$order_id %in% data_frames$ORDER_DETAIL$order_id, ]

##ADVERTISE_IN - ADVERTISEMENT - PRODUCT
invalid_ad_fk <- data_frames$ADVERTISE_IN[!data_frames$ADVERTISE_IN$ad_id %in% data_frames$ADVERTISEMENT$ad_id, ]
data_frames$ADVERTISE_IN <- data_frames$ADVERTISE_IN[data_frames$ADVERTISE_IN$ad_id %in% data_frames$ADVERTISEMENT$ad_id, ]

invalid_adproduct_fk <- data_frames$ADVERTISE_IN[!data_frames$ADVERTISE_IN$product_id %in% data_frames$PRODUCT$product_id, ]
data_frames$ADVERTISE_IN <- data_frames$ADVERTISE_IN[data_frames$ADVERTISE_IN$product_id %in% data_frames$PRODUCT$product_id, ]

# Email check
##Define a function to check email format
check_email_format <- function(email) {
  grepl("^\\S+@\\S+\\.\\S+$", email)
}

##Check email format for CUSTOMER table
if ("CUSTOMER" %in% names(data_frames)) {
  invalid_emails_customer <- !sapply(data_frames$CUSTOMER$customer_email, check_email_format)
  if (any(invalid_emails_customer)) {
    data_frames$CUSTOMER <- data_frames$CUSTOMER[!invalid_emails_customer, ]
    data_frames$CUSTOMER <- data_frames$CUSTOMER[!duplicated(data_frames$CUSTOMER$customer_email), ]
  }
}

##Check email format for SUPPLIER table
if ("SUPPLIER" %in% names(data_frames)) {
  invalid_emails_supplier <- !sapply(data_frames$SUPPLIER$supplier_email, check_email_format)
  if (any(invalid_emails_supplier)) {
    data_frames$SUPPLIER <- data_frames$SUPPLIER[!invalid_emails_supplier, ]
    data_frames$SUPPLIER <- data_frames$SUPPLIER[!duplicated(data_frames$SUPPLIER$supplier_email), ]
  }
}

# Mobile number check
##Define a function to check mobile format
check_mobile_format <- function(mobile) {
  grepl("^\\+\\d{1,3}\\s[0-9]{3}\\s[0-9]{3}\\s[0-9]{4}$", mobile)
}

##Check mobile format for CUSTOMER table
if ("CUSTOMER" %in% names(data_frames)) {
  invalid_mobiles_customer <- !sapply(data_frames$CUSTOMER$customer_mobile, check_mobile_format)
  if (any(invalid_mobiles_customer)) {
    data_frames$CUSTOMER <- data_frames$CUSTOMER[!invalid_mobiles_customer, ]
    data_frames$CUSTOMER <- data_frames$CUSTOMER[!duplicated(data_frames$CUSTOMER$customer_mobile), ]
  }
}

##Check mobile format for SUPPLIER table
if ("SUPPLIER" %in% names(data_frames)) {
  invalid_mobiles_supplier <- !sapply(data_frames$SUPPLIER$supplier_mobile, check_mobile_format)
  if (any(invalid_mobiles_supplier)) {
    data_frames$SUPPLIER <- data_frames$SUPPLIER[!invalid_mobiles_supplier, ]
    data_frames$SUPPLIER <- data_frames$SUPPLIER[!duplicated(data_frames$SUPPLIER$supplier_mobile), ]
  }
}

# Check self reference product
invalid_main_product <- data_frames$PRODUCT[!is.na(data_frames$PRODUCT$main_product_id) & 
                                              !(data_frames$PRODUCT$main_product_id %in% data_frames$PRODUCT$product_id & 
                                                  data_frames$PRODUCT$main_product_id != data_frames$PRODUCT$product_id) &
                                              trimws(data_frames$PRODUCT$main_product_id) != "", ]
data_frames$PRODUCT <- data_frames$PRODUCT[!row.names(data_frames$PRODUCT) %in% row.names(invalid_main_product), ]


# Define table names
table_names <- c("CUSTOMER", "PRODUCT", "ADDRESS", "DISCOUNT", "SUPPLIER", 
                 "CATEGORY", "ADVERTISEMENT", "ADVERTISE_IN", "ORDER_ITEM", "ORDER_DETAIL")

# Connect to the database
connect <- dbConnect(RSQLite::SQLite(), "database.db")

# Iterate over table names
for (table_name in table_names) {
  # Get data frame from data_frames list
  df <- data_frames[[table_name]]
  
  cat("Total records in", table_name, ":", nrow(df), "\n")
  
  # Retrieve existing records from the database
  db_data <- dbGetQuery(connect, paste("SELECT * FROM", table_name))
  
  # Compare and find new records
  new_records <- df[!df[, 1] %in% db_data[, 1], ]
  
  # Write new records to the database
  if (nrow(new_records) > 0) {
    dbWriteTable(connect, table_name, new_records, append = TRUE, row.names = FALSE)
  }
}

# Disconnect from the database
dbDisconnect(connect)

