library(readxl)
library(sdautils)

data <- read_excel("Data/dataset_airline.xlsx")
data <- data[!(is.na(data$recommended) | data$recommended==""), ]

View(data)
dim(data)

data2 <- data[rowSums(is.na(data)) > 0, ]

View(na.omit.unique(data$traveller_type))

View(na.omit.unique(data$cabin))
