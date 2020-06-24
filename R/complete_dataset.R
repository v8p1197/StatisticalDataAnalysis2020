library(VIM)
library(mice)
library(SDAutils)
library(anchors)

# number of NA's value
summary(mer)

#the percentage % of entire rows with non-empty fields 
comp <- complete.cases(mer)
mean(comp)

#open a new window
dev.new()
#Graph of NA fields 
VIM::aggr(mer)

#Replace 0 in NA values
merReplace = replace.value( mer, c('seat_comfort','cabin_service','food_bev',
                          'entertainment','ground_service','value_for_money'), from=0, to=NA, verbose = TRUE)

summary(merReplace)

#View the correct replacement
View(unique(merReplace$seat_comfort))
View(unique(merReplace$cabin_service))
View(unique(merReplace$food_bev))
View(unique(merReplace$entertainment))
View(unique(merReplace$value_for_money))

#Replace the missing Data into samples using pmm method
merComplete <- complete(mice(data = merReplace, m=1, method = 'pmm'))
summary(merComplete)
View(merComplete)
