library(readxl)
library(SDAutils)
library(readr)
library(anchors)

data <- read_excel("Data/dataset_airline.xlsx")
data <- data[!(is.na(data$recommended) | data$recommended==""), ]
data2 <- read_csv("Data/dataset_airline2.csv")

data_no_content = data[,-c(3,4,5,6,9,10)]
data_no_content = replace.value(data_no_content, "recommended", from='yes', to=as.integer(1), verbose=T)
data_no_content = replace.value(data_no_content, "recommended", from='no', to=as.integer(0), verbose=T)

write.csv(data_no_content, file = "temp.csv")

View(data)
dim(data)

View(data2)
dim(data2)

View(na.omit.unique(data$traveller_type))

View(na.omit.unique(data$cabin))

View(na.omit.unique(data2$type_traveller))

View(na.omit.unique(data2$cabin_flown))
