# Load required package
library(RSQLite)

connect <- dbConnect(RSQLite::SQLite(), "database.db")


# Create ADDRESS Table
dbExecute(connect, "
          CREATE TABLE IF NOT EXISTS ADDRESS (
            address_id VARCHAR(50) PRIMARY KEY,
            postcode VARCHAR(20) NOT NULL, 
            street VARCHAR(50) NOT NULL,
            city VARCHAR(100) NOT NULL,
            country VARCHAR(100) NOT NULL
          );
")

# Create CUSTOMER Table
dbExecute(connect, "
          CREATE TABLE IF NOT EXISTS CUSTOMER (
            customer_id VARCHAR(50) PRIMARY KEY, 
            first_name VARCHAR(50) NOT NULL,
            last_name VARCHAR(50) NOT NULL,
            gender VARCHAR(10),
            customer_email VARCHAR(50) NOT NULL UNIQUE,
            customer_mobile VARCHAR(50) NOT NULL UNIQUE,
            address_id VARCHAR(50) NOT NULL,
            FOREIGN KEY (address_id) REFERENCES ADDRESS (address_id)
          );
")

# Create PRODUCT table
dbExecute(connect, "
          CREATE TABLE IF NOT EXISTS PRODUCT (
            product_id VARCHAR(50) PRIMARY KEY, 
            product_name VARCHAR(50) NOT NULL,
            product_description VARCHAR(50),
            product_rating DECIMAL(5,2),
            unit_price DECIMAL(10,2) NOT NULL,
            stock_on_hand INT NOT NULL,
            main_product_id VARCHAR(50),
            category_id VARCHAR(50) NOT NULL,
            supplier_id VARCHAR(50) NOT NULL,
            FOREIGN KEY (supplier_id) REFERENCES SUPPLIER (supplier_id),
            FOREIGN KEY (category_id) REFERENCES CATEGORY (category_id)
          );
")

# Create DISCOUNT table
dbExecute(connect, "
          CREATE TABLE IF NOT EXISTS DISCOUNT (
            promo_code VARCHAR(20) PRIMARY KEY, 
            discount_percent INT NOT NULL
          );
")

# Create ORDER_ITEM table for CUSTOMER-PRODUCT relationship
dbExecute(connect, "
          CREATE TABLE IF NOT EXISTS ORDER_ITEM (
            order_id VARCHAR(50),
            product_id VARCHAR(50),
            order_quantity INT NOT NULL,
            PRIMARY KEY (order_id, product_id),
            FOREIGN KEY (order_id)   REFERENCES ORDER_DETAIL (order_id),
            FOREIGN KEY (product_id) REFERENCES PRODUCT (product_id)
          );
")

# Create ORDER_DETAIL table
dbExecute(connect, "
          CREATE TABLE IF NOT EXISTS ORDER_DETAIL (
            order_id VARCHAR(50) PRIMARY KEY,
            customer_id VARCHAR(50), 
            order_date DATE NOT NULL,
            order_status VARCHAR(50) NOT NULL, 
            promo_code VARCHAR(20),
            payment_method TEXT NOT NULL,
            delivery_fee DECIMAL(10, 2) NOT NULL,
            FOREIGN KEY (customer_id) REFERENCES CUSTOMER (customer_id),
            FOREIGN KEY (promo_code)  REFERENCES DISCOUNT (promo_code)
          );
")

# Create ADVERTISEMENT table
dbExecute(connect, "
          CREATE TABLE IF NOT EXISTS ADVERTISEMENT (
            ad_id VARCHAR(50) PRIMARY KEY, 
            ad_frequency INT NOT NULL,
            ad_place VARCHAR(50) NOT NULL,
            ad_price DECIMAL(10, 2) NOT NULL
          );
")

# Create ADVERTISE_IN table for PRODUCT-ADVERTISEMENT relationship
dbExecute(connect, "
          CREATE TABLE IF NOT EXISTS ADVERTISE_IN (
            product_id VARCHAR(50),
            ad_id VARCHAR(50),
            PRIMARY KEY (product_id, ad_id),
            FOREIGN KEY (product_id) REFERENCES PRODUCT (product_id),
            FOREIGN KEY (ad_id)      REFERENCES ADVERTISEMENT (ad_id)
          );
")

# Create SUPPLIER table
dbExecute(connect, "
          CREATE TABLE IF NOT EXISTS SUPPLIER (
            supplier_id VARCHAR(50) PRIMARY KEY, 
            supplier_name VARCHAR(50) NOT NULL,
            supplier_email VARCHAR(50) NOT NULL UNIQUE,
            supplier_mobile VARCHAR(20) NOT NULL UNIQUE
          );
")

# Create CATEGORY table
dbExecute(connect, "
          CREATE TABLE IF NOT EXISTS CATEGORY (
            category_id VARCHAR(50) PRIMARY KEY, 
            category_name VARCHAR(50) NOT NULL,
            category_fee INT NOT NULL
          );
")

# Disconnect from the database
dbDisconnect(connect)
