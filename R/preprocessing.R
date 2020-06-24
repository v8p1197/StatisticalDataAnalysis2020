library(readxl)
library(SDAutils)
library(readr)
library(anchors)
library(mice)

## Data Loading
# Load dataset 1
data <- read_excel("Data/dataset_airline.xlsx")
data <- data[!(is.na(data$recommended) | data$recommended==""), ]
data <- data[!( is.na(data$seat_comfort) &
                is.na(data$cabin_service) &
                is.na(data$food_bev) &
                is.na(data$entertainment) &
                is.na(data$ground_service) &
                is.na(data$value_for_money)
               ), ]
# Load dataset 2
data2 <- read_csv("Data/dataset_airline2.csv")
data2 <- data2[!(is.na(data2$seat_comfort_rating) &
                 is.na(data2$cabin_staff_rating) &
                 is.na(data2$food_beverages_rating) &
                 is.na(data2$inflight_entertainment_rating) &
                 is.na(data2$ground_service_rating) &
                 is.na(data2$wifi_connectivity_rating) &
                 is.na(data2$value_money_rating)
                 ), ]

## Preprocessing

# Rename columns
data2 <- rename(data2, "airline_name", "airline")
data2 <- rename(data2, "type_traveller", "traveller_type")
data2 <- rename(data2, "cabin_flown", "class")
data <- rename(data, "cabin", "class")
data2 <- rename(data2, "seat_comfort_rating", "seat_comfort")
data2 <- rename(data2, "cabin_staff_rating", "cabin_service")
data2 <- rename(data2, "food_beverages_rating", "food_bev")
data2 <- rename(data2, "inflight_entertainment_rating", "entertainment")
data2 <- rename(data2, "ground_service_rating", "ground_service")
data2 <- rename(data2, "wifi_connectivity_rating", "wifi_connectivity")
data2 <- rename(data2, "value_money_rating", "value_for_money")
data2 <- rename(data2, "overall_rating", "overall")
data2 <- rename(data2, "date", "review_date")
data2 <- rename(data2, "content", "customer_review")

# Add missing column "wifi_connectivity" to dataset 1
data[,"wifi_connectivity"] <- NA

# Change date format from dataset 1 to dataset 2
Sys.setlocale("LC_TIME", "C")
d = as.Date(data$review_date, format="%dst %B %Y")
d[is.na(d)] <- as.Date(data$review_date[is.na(d)], format="%dnd %B %Y")
d[is.na(d)] <- as.Date(data$review_date[is.na(d)], format="%drd %B %Y")
d[is.na(d)] <- as.Date(data$review_date[is.na(d)], format="%dth %B %Y")
data$review_date = d
rm(d)

# Remove useless columns from dataset 1
data = data[,-10]
data = replace.value(data, "recommended", from='yes', to=as.integer(1), verbose=T)
data = replace.value(data, "recommended", from='no', to=as.integer(0), verbose=T)
# Remove useless columns from dataset 2
data2 = data2[,-c(2,3,5)]

# Move columns
order = colnames(data2)
data = data[,order]
rm(order)

# Merge datasets
mer = merge(data, data2, all=T)
mer$airline <- tolower(mer$airline)
mer$airline <- chartr(old="-", new=" ", mer$airline)
mer$airline <- gsub("tarom romanian airlines", "tarom romanian", mer$airline)
mer$airline <- gsub("indigo airlines", "indigo", mer$airline)
mer$airline <- gsub("swiss international air lines", "swiss intl air lines", mer$airline)
mer$airline <- gsub("ukraine international airlines", "ukraine international", mer$airline)
mer$airline <- gsub("sas scandinavian airlines", "sas scandinavian", mer$airline)
mer$airline <- gsub("air canada rouge", "air canada", mer$airline)
mer$class <- gsub("Economy Class", "Economy", mer$class)
mer$traveller_type <- gsub("FamilyLeisure", "Family Leisure", mer$traveller_type)
mer$customer_review <- gsub("  ", " ", mer$customer_review)
mer = mer[!duplicated(mer), ]

# Remove useless columns from merged dataset
mer = mer[,-c(2,3,4,5,8)]

#convert recommended character into numerica data
mer$recommended = as.numeric(as.character(mer$recommended))

#Replace 0 in NA values
merReplace = replace.value(mer, c('seat_comfort','cabin_service','food_bev',
                          'entertainment','ground_service','value_for_money'), from=0, to=NA, verbose = TRUE)

#Replace the missing Data into samples using pmm method
merComplete <- complete(mice(data = merReplace, m=1, method = 'pmm'))

## Output preprocessed datasets
write.csv(mer, file = "Data/preprocessed.csv")
# Output preprocessed complete dataset
write.csv(merComplete, file = "Data/preprocessed_complete.csv")

