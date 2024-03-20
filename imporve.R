library(DBI)

# Function to read csv files from directory
read_csv_files <- function(directory) {
  csv_files <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)
  data_frames <- list()
  
  for (csv_file in csv_files) {
    # Extract the base file name without extension
    file_name <- tools::file_path_sans_ext(basename(csv_file))
    
    # Read the CSV file
    data <- read.csv(csv_file)
    
    # Check if data frame already exists
    if (file_name %in% names(data_frames)) {
      # If exists, append data to existing data frame
      data_frames[[file_name]] <- rbind(data_frames[[file_name]], data)
    } else {
      # Otherwise, create new data frame
      data_frames[[file_name]] <- data
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
data_frames$ORDER_DETAIL <- discounted_order[is.na(data_frames$ORDER_DETAIL$promo_code) | trimws(data_frames$ORDER_DETAIL$promo_code) == "" | discounted_order$promo_code %in% data_frames$DISCOUNT$promo_code, ]

##ORDER_ITEM - PRODUCT - ORDER_DETAIL
invalid_product_fk <- data_frames$ORDER_ITEM[!data_frames$ORDER_ITEM$product_id %in% data_frames$PRODUCT$product_id, ]
data_frames$ORDER_ITEM <- data_frames$ORDER_ITEM[data_frames$ORDER_ITEM$product_id %in% data_frames$PRODUCT$product_id, ]

invalid_order_fk <- data_frames$ORDER_ITEM[!data_frames$ORDER_ITEM$order_id %in% data_frames$ORDER_DETAIL$order_id, ]
data_frames$ORDER_ITEM <- data_frames$ORDER_ITEM[data_frames$ORDER_ITEM$order_id %in% data_frames$ORDER_DETAIL$order_id, ]

unique_order_ids_item <- unique(data_frames$ORDER_ITEM$order_id)
num_unique_order_ids <- length(unique_order_ids_item)

unique_order_ids_detail <- unique(data_frames$ORDER_DETAIL$order_id)
num_unique_order_ide <- length(unique_order_ids_detail)

invalid_order_fk <- order_item[!order_item$order_id %in% order_detail$order_id, c("order_id", "product_id")]

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
    cat("Invalid email format found in CUSTOMER table\n")
    # Remove records with invalid email format
    data_frames$CUSTOMER <- data_frames$CUSTOMER[!invalid_emails_customer, ]
    # Keep only the first occurrence of each unique row
    data_frames$CUSTOMER <- data_frames$CUSTOMER[!duplicated(data_frames$CUSTOMER), ]
  }
}

##Check email format for SUPPLIER table
if ("SUPPLIER" %in% names(data_frames)) {
  invalid_emails_supplier <- !sapply(data_frames$SUPPLIER$supplier_email, check_email_format)
  if (any(invalid_emails_supplier)) {
    cat("Invalid email format found in SUPPLIER table\n")
    # Remove records with invalid email format
    data_frames$SUPPLIER <- data_frames$SUPPLIER[!invalid_emails_supplier, ]
    # Keep only the first occurrence of each unique row
    data_frames$SUPPLIER <- data_frames$SUPPLIER[!duplicated(data_frames$SUPPLIER), ]
  }
}




#Check format of mobile number (+xx xxx xxx xxxx)
invalid_customer_mobile <- customer[!grepl("^\\+\\d{1,3}\\s[0-9]{3}\\s[0-9]{3}\\s[0-9]{4}$", customer$customer_mobile), c("customer_id", "customer_mobile")]

#Check duplicate mobile
duplicate_customer_mobile <- customer[duplicated(customer$customer_mobile), c("customer_id", "customer_mobile")]












##Assign data from "data_frame" into respective name
address <- data_frames$ADDRESS
discount <- data_frames$DISCOUNT
supplier <- data_frames$SUPPLIER
category <- data_frames$CATEGORY
advertisement <- data_frames$ADVERTISEMENT
customer <- data_frames$CUSTOMER
product <- data_frames$PRODUCT
advertise_in <- data_frames$ADVERTISE_IN
order_detail <- data_frames$ORDER_DETAIL
order_item <- data_frames$ORDER_ITEM


# Connect to the database
connect <- dbConnect(RSQLite::SQLite(), "database.db")

# Retrieve existing db records from the database
db_customer <- dbGetQuery(connect, "SELECT * FROM CUSTOMER")
db_product <- dbGetQuery(connect, "SELECT * FROM PRODUCT")
db_address <- dbGetQuery(connect, "SELECT * FROM ADDRESS")
db_discount <- dbGetQuery(connect, "SELECT * FROM DISCOUNT")
db_supplier <- dbGetQuery(connect, "SELECT * FROM SUPPLIER")
db_category <- dbGetQuery(connect, "SELECT * FROM CATEGORY")
db_advertisment <- dbGetQuery(connect, "SELECT * FROM ADVERTISEMENT")
db_advertise_in <- dbGetQuery(connect, "SELECT * FROM ADVERTISE_IN")
db_order_item <- dbGetQuery(connect, "SELECT * FROM ORDER_ITEM")
db_order_detail <- dbGetQuery(connect, "SELECT * FROM ORDER_DETAIL")


# Compare db and new data 
new_customer <- customer[!customer$customer_id %in% db_customer$customer_id, ]
new_product <- product[!product$product_id %in% db_product$product_id, ]
new_address <- address[!address$address_id %in% db_address$address_id, ]
new_discount <- discount[!discount$promo_code %in% db_discount$promo_code, ]
new_supplier <- supplier[!supplier$supplier_id %in% db_supplier$supplier_id, ]
new_category <- category[!category$category_id %in% db_category$category_id, ]
new_advertisement <- advertisement[!advertisement$ad_id %in% db_advertisment$ad_id, ]
new_order_detail <- order_detail[!order_detail$order_id %in% db_order_detail$order_id, ]

db_advertise_in_composite <- paste(db_advertise_in$ad_id, db_advertise_in$product_id)
advertise_in_composite <- paste(advertise_in$ad_id, advertise_in$product_id)
new_advertise_in <- advertise_in[!advertise_in_composite %in% db_advertise_in_composite, ]

db_order_item_composite <- paste(db_order_item$order_id, db_order_item$product_id)
order_item_composite <- paste(order_item$order_id, order_item$product_id)
new_order_item <- order_item[!order_item_composite %in% db_order_item_composite, ]

# Write into the db
if (nrow(new_customer) > 0) {
  dbWriteTable(connect, "CUSTOMER", new_customer, append = TRUE, row.names = FALSE)
}

if (nrow(new_product) > 0) {
  dbWriteTable(connect, "PRODUCT", new_product, append = TRUE, row.names = FALSE)
}

if (nrow(new_address) > 0) {
  dbWriteTable(connect, "ADDRESS", new_address, append = TRUE, row.names = FALSE)
}

if (nrow(new_discount) > 0) {
  dbWriteTable(connect, "DISCOUNT", new_discount, append = TRUE, row.names = FALSE)
}

if (nrow(new_supplier) > 0) {
  dbWriteTable(connect, "SUPPLIER", new_supplier, append = TRUE, row.names = FALSE)
}

if (nrow(new_category) > 0) {
  dbWriteTable(connect, "CATEGORY", new_category, append = TRUE, row.names = FALSE)
}

if (nrow(new_advertisement) > 0) {
  dbWriteTable(connect, "ADVERTISEMENT", new_advertisement, append = TRUE, row.names = FALSE)
}

if (nrow(new_order_detail) > 0) {
  dbWriteTable(connect, "ORDER_DETAIL", new_order_detail, append = TRUE, row.names = FALSE)
}

if (nrow(new_advertise_in) > 0) {
  dbWriteTable(connect, "ADVERTISE_IN", new_advertise_in, append = TRUE, row.names = FALSE)
}

if (nrow(new_order_item) > 0) {
  dbWriteTable(connect, "ORDER_ITEM", new_order_item, append = TRUE, row.names = FALSE)
}

# Disconnect from the database
dbDisconnect(connect)