library(readr)
library(RSQLite)

customers <- readr::read_csv('data_upload/customers.csv')
my_connection <- RSQLite::dbConnect(RSQLite::SQLite(),'database/database.db')
RSQLite::dbWriteTable(my_connection, 'customers', customers)