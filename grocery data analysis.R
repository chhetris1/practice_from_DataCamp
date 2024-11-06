#project: deciphering customer buying cycles in grocery stores
library(dplyr)
library(lubridate)
library(readr)

#the datasets
grocery_data1 <- read_csv("grocery_data1.csv") %>%
  mutate(Date = mdy(DateRaw)) #we need date in POSIX format
grocery_data2 <- read_csv("grocery_data2.csv") %>%
  mutate(Date = dmy(DateRaw))

grocery_data <- bind_rows(grocery_data1, grocery_data2)

#calculate TotalSaleUSD = priceUSD * Quantity
grocery_data <- grocery_data %>% 
    mutate(TotalSaleUSD = PriceUSD * Quantity )

#figure out time since each client purchased each product 
grocery_data_grouped <- grocery_data %>% 
  arrange(CustomerID, Date) %>% 
  group_by(CustomerID, ProductName) %>%
  mutate(
    DaysSinceLastPurchase = as.numeric(c(0, diff(Date))), 
    Week = week(Date), Year = year(Date), Hour = hour(Time)
    ) %>% ungroup()
        
#figuring out weekly total sales 
weekly_sales <- grocery_data_grouped %>% 
  group_by(Week, Year) %>% 
  summarise(WeeklyTotalSaleUSD = sum(TotalSaleUSD))%>% 
  arrange(Week, Year) %>% 
  ungroup()

#Now let's answer the question:
#What week of the year during the time period had the smallest absolute
# mean deviation in sales value compared to the mean WeeklyTotalSaleUSD?

mean(weekly_sales$WeeklyTotalSaleUSD)
weekly_sales %>% 
  mutate(Diff = abs(WeeklyTotalSaleUSD - mean(WeeklyTotalSaleUSD))) %>% 
  arrange(Diff)

smallest_sales_deviation <- 24

#What hour of the day had the most hourly total sales? 
hourly_sales <- grocery_data_grouped %>% 
  group_by(Hour) %>% 
  summarise(Hourly_total_sales = sum(TotalSaleUSD)) %>% 
  arrange(desc(Hourly_total_sales))  %>% 
  ungroup()

most_hourly_sales <- 22

## Now, examine Cornflakes purchases for Customoer ID 107
grocery_data_grouped %>% 
  filter(CustomerID == 107) %>% 
  filter(ProductName == "Cornflakes") %>% 
  arrange(desc(DaysSinceLastPurchase))

diff1 <- as.numeric(as.Date("2023-08-31") - as.Date("2023-07-22"))
diff1  #40

diff2<- 22-16
diff2  #6

cornflakes_days <- c(6, 40)
