# Load required package
library(RSQLite)
#library(DBI)
#library(readr)
library(dplyr)
#library(ggplot2)
#library(plotly)
#library(gridExtra)



connect <- dbConnect(RSQLite::SQLite(), "database.db")


product <- RSQLite::dbGetQuery(connect,'SELECT * FROM PRODUCT')
top_suppliers<- product %>% group_by(supplier_id) %>% summarise(quantity=sum(stock_on_hand))



top_5_suppliers <- top_suppliers %>% 
  arrange(desc(quantity)) %>% 
  slice(1:5)

# Create a bar plot using Plotly
plot_ly(data = top_5_suppliers, x = ~supplier_id, y = ~quantity, type = 'bar', 
        marker = list(color = 'skyblue')) %>%
  layout(title = "Top 5 Suppliers by Stock",
         xaxis = list(title = "Supplier ID"),
         yaxis = list(title = "Stock on Hand"),
         showlegend = FALSE)




order_item <- RSQLite::dbGetQuery(connect,'SELECT * FROM ORDER_ITEM')
order_detail <- RSQLite::dbGetQuery(connect,'SELECT * FROM ORDER_DETAIL')
discount <- RSQLite::dbGetQuery(connect,'SELECT * FROM DISCOUNT')

profit_data <- order_item %>%
  inner_join(order_detail, by = "order_id") %>% # Assuming order_id is the common key
  filter(order_status != "Cancelled") %>%
  inner_join(product, by = "product_id") %>%
  left_join(discount, by = c("promo_code" = "promo_code")) %>%
  mutate(
    discount_percentage = ifelse(is.na(discount_percent), 0, discount_percent), 
    total_profit = (order_quantity * unit_price) * (1 - discount_percentage / 100)
  ) %>%
  group_by(product_id, product_name) %>%
  summarise(
    total_profit = sum(total_profit), 
    .groups = 'drop'
  ) %>%
  arrange(desc(total_profit))




# Selecting the top 10 profitable products
top_10_profit_data <- head(profit_data, 10)


# graphing the results
ggplot(top_10_profit_data, aes(x = reorder(product_id, total_profit), y = total_profit, fill = product_name)) +
  geom_bar(stat = "identity") +
  scale_fill_discrete(name = "product Name") + 
  labs(title = "Top 10 Most Profitable Products", x = "Product ID", y = "Total Profit") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 10), 
        plot.title = element_text(hjust = 0.5)) 





sales_data <- order_item %>%
  group_by(product_id) %>%
  summarise(Total_Sales = sum(order_quantity, na.rm = TRUE)) %>%
  arrange(desc(Total_Sales))

top_selling_products <- sales_data %>%
  left_join(product, by = "product_id") %>%
  select(product_id, product_name, Total_Sales) %>%
  top_n(10, Total_Sales)




ggplot(top_selling_products, aes(x = reorder(product_id, Total_Sales), y = Total_Sales, fill = product_name)) +
  geom_bar(stat = "identity") +
  scale_fill_discrete(name = "product Name") + 
  labs(title = "Top Most Selling Products", x = "Product ID", y = "Total Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 10), 
        plot.title = element_text(hjust = 0.5))




# Joining Product with advertise_in and advertisement to get advertisement details
#test
advertise_in <- RSQLite::dbGetQuery(connect,'SELECT * FROM ADVERTISE_IN')
advertisement <- RSQLite::dbGetQuery(connect,'SELECT * FROM ADVERTISEMENT')

advertisement_data <- product %>%
  inner_join(advertise_in, by = "product_id") %>%
  inner_join(advertisement, by = "ad_id") %>%
  group_by(product_id, product_name, ad_place) %>%
  summarise(
    total_frequency = sum(ad_frequency), # Sum the total frequency of ads for each ad place
    .groups = 'drop' )


# Joining product with order to get the number of sales per product
number_of_sales <- product %>%
  inner_join(order_item, by = "product_id") %>%
  group_by(product_id, product_name) %>%
  summarise(sales_count = n(), .groups = 'drop')

# Note: Each product is being sold twice.
# Note: Remove Category fee 
# Note: Product table: main category id column empty

merged_data <- merge(number_of_sales, advertisement_data, by = c("product_id", "product_name"))



# Analyzing which ad place is most effective by calculating a ratio of total sales to total ad frequency
effective_ad_type <- merged_data %>%
  group_by(ad_place) %>%
  summarise( total_sales = sum(sales_count), 
             total_frequency = sum(total_frequency), .groups = 'drop') %>%
  mutate( effectiveness = total_sales / total_frequency) %>%
  arrange(desc(effectiveness))




ggplot(effective_ad_type, aes(x = ad_place, y = effectiveness, fill = ad_place)) +
  geom_col(show.legend = FALSE, width = 0.5) + # Adjust bar width here
  scale_fill_brewer(palette = "Paired") + # Use a more appealing color palette
  labs(title = "Effectiveness of Advertisement Types", 
       x = "Ad Place", 
       y = "Total Sales / Total Frequency") +
  theme_minimal(base_size = 14) + # Increase base text size for better readability
  theme(plot.title = element_text(hjust = 0.5, size = 20), # Center and style title
        axis.title = element_text(size = 16), # Style axis titles
        axis.text = element_text(size = 12), # Style axis texts
        panel.grid.major = element_blank(), # Remove major grid lines
        panel.grid.minor = element_blank(), # Remove minor grid lines
        panel.background = element_rect(fill = "white", colour = "grey50")) # Style panel background




category <- RSQLite::dbGetQuery(connect,'SELECT * FROM CATEGORY')

# Join product_df with category_df to include category names
product_with_category <- product %>%
  inner_join(category, by = "category_id")


avg_rating_by_category <- product_with_category %>%
  group_by(category_id, category_name) %>%
  summarise(Average_Rating = round(mean(product_rating, na.rm = TRUE),2), .groups = 'drop') %>%
  arrange(category_id)

print(avg_rating_by_category)




# Add a cumulative sum of Average_Rating to calculate position for labels
avg_rating_by_category <- avg_rating_by_category[order(-avg_rating_by_category$Average_Rating), ]

ggplot(avg_rating_by_category, aes(x = Average_Rating, y = reorder(category_name, Average_Rating))) +
  geom_bar(stat = "identity", color = "white", fill = ifelse(avg_rating_by_category$Average_Rating < 4, "#FFCC80", "#C8E6C9")) +
  geom_text(aes(label = sprintf("%.2f", Average_Rating), x = Average_Rating, y = category_name), 
            color = "black", size = 4, hjust = 2) +
  labs(title = "Average Rating for Each Category") +
  theme_minimal() +
  theme(axis.line = element_blank(),
        axis.text = element_text(size = 9),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5))


#library(lubridate)
time_period_sales<- RSQLite::dbGetQuery(connect,'SELECT ORDITM.order_id, order_quantity,order_date FROM ORDER_DETAIL ORDDET INNER JOIN ORDER_ITEM ORDITM ON ORDITM.order_id = ORDDET.order_id')
#test
time_period_sales$order_date <- as.Date(time_period_sales$order_date, format="%d/%m/%Y")

time_period_sales$order_date_mnth_yr <- format(time_period_sales$order_date,'%Y/%m')

time_period_sales$order_date_mnth_yr <- factor(time_period_sales$order_date_mnth_yr, levels=c('2022/06','2022/07','2022/08','2022/09','2022/10','2022/11','2022/12','2023/01','2023/02','2023/03','2023/04','2023/05','2023/06','2023/07','2023/08','2023/09','2023/10','2023/11','2023/12'))

time_period_sales_group_by<- time_period_sales %>% group_by(order_date_mnth_yr)%>%summarise(quantity=sum(order_quantity))

(ggplot(time_period_sales_group_by, aes(x = order_date_mnth_yr, y = quantity,group=1)) +  geom_point()+geom_line()+xlab('Time Period(year/month)')+ylab('Quantity Sold'))


#salesvsregion
cust_order_join<- RSQLite::dbGetQuery(connect, 'SELECT city,ORDITM.order_quantity FROM ADDRESS ADR INNER JOIN CUSTOMER CUST ON ADR.address_id= CUST.address_id INNER JOIN ORDER_DETAIL ORDDET ON ORDDET.customer_id = CUST.customer_id INNER JOIN ORDER_ITEM ORDITM ON ORDITM.order_id = ORDDET.order_id')

#cust_order_join<- inner_join(Customer,Order,by="customer_id")

sales_per_region<- cust_order_join %>% group_by(city)%>%summarise(quantity=sum(order_quantity))

top_5_sales_per_region <- sales_per_region[order(-sales_per_region$quantity),]

top_5_sales_per_region <- head(top_5_sales_per_region,10)

ggplot1 <- ggplot(top_5_sales_per_region, aes(x = reorder(city,-quantity), y = quantity,fill=quantity)) +  geom_bar(stat='identity')+scale_fill_gradient(low = "lightblue", high = "darkblue")+ xlab('Top 10 cities where sales is maximum')+ylab('Quantity Sold')


#revenuevsregion
cust_order_product_join <-RSQLite::dbGetQuery(connect, 'SELECT city,ORDITM.order_quantity,unit_price FROM ADDRESS ADR INNER JOIN CUSTOMER CUST ON ADR.address_id= CUST.address_id INNER JOIN ORDER_DETAIL ORDDET ON ORDDET.customer_id = CUST.customer_id INNER JOIN ORDER_ITEM ORDITM ON ORDITM.order_id = ORDDET.order_id INNER JOIN PRODUCT PRD ON PRD.product_id = ORDITM.product_id')

top_5_products <-RSQLite::dbGetQuery(connect, 'SELECT city,ORDITM.order_quantity,unit_price,PRD.product_id,PRD.product_name FROM ADDRESS ADR INNER JOIN CUSTOMER CUST ON ADR.address_id= CUST.address_id INNER JOIN ORDER_DETAIL ORDDET ON ORDDET.customer_id = CUST.customer_id INNER JOIN ORDER_ITEM ORDITM ON ORDITM.order_id = ORDDET.order_id INNER JOIN PRODUCT PRD ON PRD.product_id = ORDITM.product_id')

#TEST

cust_order_product_join$revenue <-  cust_order_product_join$order_quantity*cust_order_product_join$unit_price

revenue_per_region <- cust_order_product_join %>% group_by(city) %>% summarise(rev= sum(revenue))

top_5_revenue_per_region <- revenue_per_region[order(-revenue_per_region$rev),]

top_5_revenue_per_region <- head(top_5_revenue_per_region,10)

revenue_per_product_region <- cust_order_product_join %>% group_by(city) %>% summarise(rev= sum(revenue))

ggplot2 <- ggplot(top_5_revenue_per_region, aes(x = reorder(city, -rev), y = rev,fill=rev)) +  geom_bar(stat='identity')+scale_fill_gradient(low = "lightblue", high = "darkblue")+xlab('Top 10 cities with the maximum revenue')+ylab('Revenue')

grid.arrange(ggplot1,ggplot2,nrow(1))

top_5_products$revenue <- top_5_products$order_quantity*top_5_products$unit_price

top_5_products_region <- top_5_products %>% group_by(city,product_name) %>% summarise(rev= sum(revenue))

(top_5_products_region <- top_5_products_region %>% filter(city %in% head(top_5_revenue_per_region$city,2)) %>% select(product_name))


#Calculating Marketplace fee
merged_product_fee <- product %>%
  inner_join(select(category, category_fee), by = "category_id")
order_fee <- order %>%
  inner_join(select(order, category_fee), by = "product_id")