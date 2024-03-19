# Load required package
library(RSQLite)

# Read Files
csv_files <- list.files("data_upload", pattern = "\\.csv$", full.names = TRUE)

# Initialize a list to store data frames
data_frames <- list(
  CUSTOMER = NULL,
  PRODUCT = NULL,
  ADDRESS = NULL,
  DISCOUNT = NULL,
  SUPPLIER = NULL,
  CATEGORY = NULL,
  ADVERTISEMENT = NULL,
  ADVERTISE_IN = NULL,
  ORDER_ITEM = NULL,
  ORDER_DETAIL = NULL
)

# Loop through each CSV file
for (csv_file in csv_files) {
  data <- read.csv(csv_file)
  file_name <- tools::file_path_sans_ext(basename(csv_file))
  if (file_name %in% names(data_frames)) {
    data_frames[[file_name]] <- rbind(data_frames[[file_name]], data)
  }
}

# Access each data frame by its name
customer <- data_frames$CUSTOMER
product <- data_frames$PRODUCT
address <- data_frames$ADDRESS
discount <- data_frames$DISCOUNT
supplier <- data_frames$SUPPLIER
category <- data_frames$CATEGORY
advertisement <- data_frames$ADVERTISEMENT
advertise_in <- data_frames$ADVERTISE_IN
order_item <- data_frames$ORDER_ITEM
order_detail <- data_frames$ORDER_DETAIL


# CUSTOMER
#Check for missing data
customer <- customer[complete.cases(customer), ]

#Check duplicate customer_id
duplicate_customer_id <- customer[duplicated(customer$customer_id), "customer_id"]
customer <- customer[!customer$customer_id %in% duplicate_customer_id, ]

#Check duplicate email
duplicate_emails <- customer[duplicated(customer$customer_email), "customer_email"]
customer <- customer[!customer$customer_email %in% duplicate_emails, ]

#Check format of first and last name (1st alphabet is uppercase, rest is lowercase)
customer <- customer[grepl("^[A-Z][a-z]$", customer$first_name) & grepl("^[A-Z][a-z]$", customer$last_name), ]

#Check email format
customer <- customer[grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", customer$customer_email), ]

#Check format of mobile number (+xx xxx xxx xxxx)
customer <- customer[grepl("^\\+\\d{1,3}\\s\\d{3}\\s\\d{3}\\s\\d{4}$", customer$customer_mobile), ]

#Check duplicate mobile
duplicate_mobiles <- customer[duplicated(customer$customer_mobile), "customer_mobile"]
customer <- customer[!customer$customer_mobile %in% duplicate_mobiles, ]

#Check if address_id exists in the ADDRESS table
customer <- customer[customer$address_id %in% address$address_id, ]


# ADDRESS
#Check for missing data
address <- address[complete.cases(address), ]

#Check duplicate address_id
duplicate_address_id <- address[duplicated(address$address_id), "address_id"]
address <- address[!address$address_id %in% duplicate_address_id, ]


# CATEGORY
#Check for missing data
category <- category[complete.cases(category), ]

#Check duplicate category_id
duplicate_category_id <- category[duplicated(category$category_id), "category_id"]
category <- category[!category$category_id %in% duplicate_category_id, ]

#Check category (can contain alphabets)
category <- category[grepl("^[A-Za-z]+( [A-Za-z]+)*$", category$category_name), ]

#Check duplicate category name
duplicate_category_name <- category[duplicated(category$category_name), "category_name"]
category <- category[!category$category_name %in% duplicate_category_name, ]

#Check for negative prices
category <- category[category$category_fee >= 0, ]


# SUPPLIER
#Check missing data
supplier <- supplier[complete.cases(supplier), ]

#Check duplicate supplier_id
duplicate_supplier_id <- supplier[duplicated(supplier$supplier_id), "supplier_id"]
supplier <- supplier[!supplier$supplier_id %in% duplicate_supplier_id, ]

#Check supplier (can contain alphabets, comma, hyphen, dot)
supplier <- supplier[grepl("^[a-zA-Z,.-]+$", supplier$supplier_name), ]

#Check email format
supplier <- supplier[grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", supplier$supplier_email), ]

#Check duplicate email
duplicate_emails <- supplier[duplicated(supplier$supplier_email), "supplier_email"]
supplier <- supplier[!supplier$supplier_email %in% duplicate_emails, ]

#Check format of mobile number (+xx xxx xxx xxxx)
supplier <- supplier[grepl("^\\+\\d{1,3}\\s\\d{3}\\s\\d{3}\\s\\d{4}$", supplier$supplier_mobile), ]

#Check duplicate mobile
duplicate_mobiles <- supplier[duplicated(supplier$supplier_mobile), "supplier_mobile"]
supplier <- supplier[!supplier$supplier_mobile %in% duplicate_mobiles, ]


# DISCOUNT
#Check missing data
discount <- discount[complete.cases(discount), ]

#Check duplicate promo_code
duplicate_promo_code <- discount[duplicated(discount$promo_code), "promo_code"]
discount <- discount[!discount$promo_code %in% duplicate_promo_code, ]

#Check percent
discount <- discount[grepl("^[0-9]+$", discount$discount_percent), ]


# ADVERTISEMENT
#Check for missing data
advertisement <- advertisement[complete.cases(advertisement), ]

#Check duplicate pk
duplicate_ad_id <- advertisement[duplicated(advertisement$ad_id), "ad_id"]
advertisement <- advertisement[!advertisement$ad_id %in% duplicate_ad_id, ]

#Check ad frequency (can only contain integer)
advertisement <- advertisement[grepl("^[0-9]+$", advertisement$ad_frequency), ]

#Check for negative ad frequency
advertisement <- advertisement[advertisement$ad_frequency >= 0, ]

#Check for negative prices
advertisement <- advertisement[advertisement$ad_price >= 0, ]


# ADVERTISE_IN
#Check for missing data
advertise_in <- advertise_in[complete.cases(advertise_in), ]

#Check duplicate for composite primary key
advertise_in_composite <- paste(advertise_in$ad_id, advertise_in$product_id)
advertise_in <- advertise_in[!duplicated(advertise_in_composite), ]

#Check if ad_id exists in the ADVERTISEMENT table
advertise_in <- advertise_in[advertise_in$ad_id %in% advertisement$ad_id, ]

#Check if product_id exists in the PRODUCT table
advertise_in <- advertise_in[advertise_in$product_id %in% product$product_id, ]



# PRODUCT
#Check for missing data
product <- product[complete.cases(product), ]

#Check duplicate product_id
duplicate_product_id <- product[duplicated(product$product_id), "product_id"]
product <- product[!product$product_id %in% duplicate_product_id, ]

#Check for negative prices
product <- product[product$unit_price >= 0, ]

#Check negative stock
product <- product[product$stock_on_hand >= 0, ]

#Check if supplier_id exists in the SUPPLIER table
product <- product[product$supplier_id %in% supplier$supplier_id, ]

#Check if category_id exists in the CATEGORY table
product <- product[product$category_id %in% category$category_id, ]

#Check if main product is self referential and it cannot be the same as product_id
product <- product[is.na(product$main_product_id) | 
                     !(product$main_product_id %in% product$product_id | 
                         product$main_product_id == product$product_id), ]


# ORDER_DETAIL
#Check for missing data
order_detail <- order_detail[complete.cases(order_detail), ]

#Check duplicate order_id
duplicate_order_id <- order_detail[duplicated(order_detail$order_id), "order_id"]
order_detail <- order_detail[!order_detail$order_id %in% duplicate_order_id, ]

#Check if customer_id exists in the CUSTOMER table
order_detail <- order_detail[order_detail$customer_id %in% customer$customer_id, ]

#Check if promo_code exists in the DISCOUNT table
order_detail <- order_detail[is.na(order_detail$promo_code) | order_detail$promo_code %in% discount$promo_code, ]

#Check order status
status <- c("Pending", "Paid", "Shipped", "Completed", "Cancelled")
order_detail <- order_detail[order_detail$order_status %in% status, ]

#Check payment method
payment <- c("Mastercard", "Visa", "Amex")
order_detail <- order_detail[order_detail$payment_method %in% payment, ]

#Check for negative delivery fee
order_detail <- order_detail[order_detail$delivery_fee >= 0, ]


# ORDER_ITEM
#Check for missing data
order_item <- order_item[complete.cases(order_item), ]

#Check duplicate for composite primary key
order_item_composite <- paste(order_item$order_id, order_item$product_id)
order_item <- order_item[!duplicated(order_item_composite), ]

#Check if order_id exists in the ORDER_ITEM table
order_item <- order_item[order_item$order_id %in% order_detail$order_id, ]

#Check if product_id exists in the PRODUCT table
order_item <- order_item[order_item$product_id %in% product$product_id, ]

#Check negative order quantity
order_item <- order_item[order_item$order_quantity >= 1, ]




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
writeToDB <- function(data, tableName, connect) {
  if (nrow(data) > 0) {
    dbWriteTable(connect, tableName, data, append = TRUE, row.names = FALSE)
  }
}

writeToDB(new_customer, "CUSTOMER", connect)
writeToDB(new_product, "PRODUCT", connect)
writeToDB(new_address, "ADDRESS", connect)
writeToDB(new_discount, "DISCOUNT", connect)
writeToDB(new_supplier, "SUPPLIER", connect)
writeToDB(new_category, "CATEGORY", connect)
writeToDB(new_advertisement, "ADVERTISEMENT", connect)
writeToDB(new_order_detail, "ORDER_DETAIL", connect)
writeToDB(new_advertise_in, "ADVERTISE_IN", connect)
writeToDB(new_order_item, "ORDER_ITEM", connect)


dbDisconnect(connect)