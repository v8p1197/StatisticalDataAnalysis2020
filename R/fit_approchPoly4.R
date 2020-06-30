#### Best subset selection and step forward and backward methods ####
library(leaps)
attach(merComplete)
# The regsubsets() function (part of the leaps library) performs best subset selection 
# by identifying the best model that contains a given number of predictors, 

regfit.full=regsubsets(fit.poly4,merComplete, nvmax=29)
reg.summary=summary(regfit.full)

reg.summary$rsq # R2 statistic increases monotonically as more variables are included.
which.min(reg.summary$rss) # identify the location of the minimum in the model with 29 predictors
which.max(reg.summary$adjr2) # Adjusted R2 is max in the model with 25 predictors
which.min(reg.summary$bic) # BIC is min in the model with 20 predictors

cat("\nLocation of RSS min:",which.min(reg.summary$rss),"\n")
cat("Location of adj-RSq max:",which.max(reg.summary$adjr2),"\n ")
cat("Location of Cp min:",which.min(reg.summary$cp),"\n ")
cat("Location of BIC min:",which.min(reg.summary$bic),"\n ")

# Plot RSS, adjusted R2, Cp, and BIC for all of the models at once
# As we seen in the plot, the best results is the model with 8 predictors
dev.new()
par(mfrow=c(2,2))
plot(reg.summary$rss ,xlab="Number of Variables ",ylab="RSS",
     type="l")
points(which.min(reg.summary$rss),min(reg.summary$rss), col="red",cex=2,pch=20)
plot(reg.summary$adjr2 ,xlab="Number of Variables ",
     ylab="Adjusted RSq",type="l")
points(which.max(reg.summary$adjr2),max(reg.summary$adjr2), col="red",cex=2,pch=20)
plot(reg.summary$cp ,xlab="Number of Variables ",ylab="Cp", type="l")
points(which.min(reg.summary$cp ),min(reg.summary$cp),col="red",cex=2,pch=20)
plot(reg.summary$bic ,xlab="Number of Variables ",ylab="BIC",type="l")
points(which.min(reg.summary$bic),min(reg.summary$bic),col="red",cex=2,pch=20)

# Another way to see the best model according the graphs
dev.new()
plot(regfit.full,scale="r2")
dev.new()
plot(regfit.full,scale="adjr2")
dev.new()
plot(regfit.full,scale="Cp") # best model with "Cp" min
dev.new()
plot(regfit.full,scale="bic") # best model with smaller "bic"
# The BIC is a variant of the ACI with higher penalty terms. (It is closely related to the ACI)


coef(regfit.full ,which.min(reg.summary$bic)) #see the coefficient estimates for the 21-variable model


####### Forward and Backward Stepwise Selection #######
# Using the argument method="forward" or method="backward".

## Forward
regfit.fwd=regsubsets(fit.poly4, data=merComplete, nvmax=29, method ="forward")
regfit.fwd.summary = summary(regfit.fwd)
regfit.fwd.summary$rsq # R2 statistic increases monotonically as more variables are included.
which.min(regfit.fwd.summary$rss) # identify the location of the minimum in the model with 29 predictors
which.max(regfit.fwd.summary$adjr2) # Adjusted R2 is max in the model with 25 predictors

dev.new()
plot(regfit.bwd,scale="r2")
dev.new()
plot(regfit.bwd,scale="adjr2")
dev.new()
plot(regfit.bwd,scale="Cp") # best model with "Cp" min
dev.new()
plot(regfit.bwd,scale="bic") # best model with smaller "bic"
# We see that using forward stepwise selection, the best one-variable is recommended.

## Backward
regfit.bwd=regsubsets(fit.poly4,data=merComplete,nvmax=29, method ="backward")
summary(regfit.bwd)

regfit.bwd.summary = summary(regfit.bwd)
regfit.bwd.summary$rsq # R2 statistic increases monotonically as more variables are included.
which.min(regfit.bwd.summary$rss) # identify the location of the minimum in the model with 29 predictors
which.max(regfit.bwd.summary$adjr2) # Adjusted R2 is max in the model with 25 predictors
which.min(regfit.bwd.summary$bic) # BIC is min in the model with 20 predictors.

dev.new()
plot(regfit.bwd,scale="r2")
dev.new()
plot(regfit.bwd,scale="adjr2")
dev.new()
plot(regfit.bwd,scale="Cp") # best model with "Cp" min
dev.new()
plot(regfit.bwd,scale="bic") # best model with smaller "bic"

# For this data, the best one-variable through six-variable models 
# might be identical for best subset and forward selection.
ii=29; summary(regfit.full)$outmat[ii,]==summary(regfit.fwd)$outmat[ii,]
# However, they have different best seven-variable models. 
coef(regfit.full,29)
coef(regfit.fwd,29)
coef(regfit.bwd,29)

# Same results with cleaner output 
round(coef(regfit.full,29),3)
round(coef(regfit.fwd,29),3)
round(coef(regfit.bwd,29),3)


## Choosing Among Models Using the Validation Set Approach and Cross-Validation 
####### Validation Set Approach ####
set.seed (2019)

# Sampling with replacement 
train=sample(c(TRUE,FALSE), nrow(merComplete),rep=TRUE)
sum(train) # --> 39776
test=(!train)
sum(test) # --> 39800

# Apply best subset selection to the training set
regfit.best=regsubsets(fit.poly4,data=merComplete[train,], nvmax =29)

# Make a model matrix from the test data.
# The model.matrix() function is used in many regression packages for 
# building an "X" matrix from data.
# Using a error matrix to calculate the test error
test.mat=model.matrix(fit.poly4,data=merComplete[test,])

# Now we run a loop, and for each size i, we extract the coefficients 
# from regfit.best for the best model of that size, 
# multiply them into the appropriate columns of the test model matrix
# to form the predictions, and compute the test MSE.
# Compute the MSE test for best model from 1 to 29 regressors
val.errors=rep(NA,29)
for(i in 1:29){
  coefi = coef(regfit.best,id=i)
  pred = test.mat[,names(coefi)]%*%coefi
  val.errors[i] = mean((merComplete$overall[test]-pred)^2)
}
# The best model is the one that contains which.min(val.errors) =  28 variables.
val.errors; which.min(val.errors) 
coef(regfit.best,which.min(val.errors)) # This is based on training data

# There is not a predict() method for regsubsets(). 
# Since we will be using this function again, we can capture our steps above and write our own predict method.
predict.regsubsets = function(object,newdata,id,...){ # ... <-> ellipsis
  form=as.formula(object$call[[2]])
  mat=model.matrix(form, newdata)
  coefi=coef(object, id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}
# We will demonstrate how we use this function while performing test MSE estimation by cross-validation.


######### Cross-Validation Approach ######## 
# Create a vector that allocates each observation to one of k = 8 folds and create a matrix to store the 
# results.
k=8
set.seed(2018)
folds = sample(1:k,nrow(merComplete),replace=TRUE) # Each row of the dataset is associated with a group (from 1 to 8)
cv.errors = matrix(NA,k,29, dimnames=list(NULL, paste(1:29)))

# write a for loop that performs cross-validation
for(j in 1:k){
  # folds != j si prende tutti i gruppi per il train e lascia un gruppo per il test
  best.fit=regsubsets(overall~recommended+poly(seat_comfort,4)+poly(cabin_service,4)
                      +poly(food_bev,4)+poly(entertainment,4)+poly(ground_service,4)
                      +poly(wifi_connectivity,4)+poly(value_for_money,4), data=merComplete[folds!=j,], nvmax=29)
  for(i in 1:29){
    pred = predict(best.fit, merComplete[folds==j,], id=i)
    cv.errors[j,i] = mean((merComplete$overall[folds==j]-pred)^2)
  }
}
# This has given us a 8 x 8 matrix, of which the (i,j)th element corresponds to the test MSE for the i-th 
# cross-validation fold for the best j-variable model.
mean.cv.errors=apply(cv.errors, 2, mean); mean.cv.errors# Column average
colMeans(cv.errors) # same results
par(mfrow=c(1,1))
dev.new()
plot(mean.cv.errors, type="b") 

# Compare between model incomplete and model returned by best subset selection

fit.poly4.incomplete <- lm(overall~recommended+poly(seat_comfort,4)+poly(cabin_service,4)
                           +poly(food_bev,4)+entertainment+poly(ground_service,4)
                          + wifi_connectivity+poly(value_for_money,4), data=merComplete)

# Model with max adjusted R2 (25 predictors)
fit.poly4.best <- lm(overall~recommended+poly(seat_comfort,2)+I(seat_comfort^4)+poly(cabin_service,4)
                           +poly(food_bev,2)+I(food_bev^4)+poly(entertainment,4)+poly(ground_service,4)
                           + poly(wifi_connectivity,3)+poly(value_for_money,3), data=merComplete)


# Model with min BIC. This is a model with 20 predictors and we no have substantial differences
fit.poly4.20.predictors <- lm(overall~recommended+poly(seat_comfort,2)+I(seat_comfort^4)
                         +poly(cabin_service,4)+poly(food_bev,2)+entertainment+poly(ground_service,4)
                         +poly(wifi_connectivity,2)+ poly(value_for_money,3),data = merComplete)


anova(fit.poly4.incomplete,fit.poly4.best)

# fit 4 is the polinomial-4 model with all predictors
anova(fit4,fit.poly4.best)


summary(fit4)

anova(fit.poly4.best,fit.poly4.20.predictors)
anova(fit.poly4.20.predictors,fit4)

# As we can see, between the model with all predictors and the one with 20 predictors there are no 
# substantial differences, so the model with less predictors is preferred
