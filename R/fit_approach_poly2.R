#### Best subset selection and step forward and backward methods ####
library(leaps)

# The regsubsets() function (part of the leaps library) performs best subset selection 
# by identifying the best model that contains a given number of predictors, 

regfit.full=regsubsets(fit.poly2.complete,merComplete, nvmax = 15)
reg.summary=summary(regfit.full)

reg.summary$rsq # R2 statistic increases monotonically as more variables are included.
which.min(reg.summary$rss) # identify the location of the minimum in the model with 15 predictors. 
which.max(reg.summary$adjr2) # infact Adjusted R2 statistic is max in the model with 15 predictors.
which.min(reg.summary$bic) # BIC is min in the model with 14 predictors
which.min(reg.summary$cp) # Cp is min in the model with 15 predictors

# The best results is the model with 8 predictors
cat("\nLocation of RSS min:",which.min(reg.summary$rss),"\n")
cat("Location of adj-RSq max:",which.max(reg.summary$adjr2),"\n ")
cat("Location of Cp min:",which.min(reg.summary$cp),"\n ")
cat("Location of BIC min:",which.min(reg.summary$bic),"\n ")

# Plot RSS, adjusted R2, Cp, and BIC for all of the models at once
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


coef(regfit.full ,which.min(reg.summary$bic)) #see the coefficient estimates for the 8-variable model


####### Forward and Backward Stepwise Selection #######
# Using the argument method="forward" or method="backward".

## Forward
regfit.fwd=regsubsets(fit.poly2.complete,data=merComplete, nvmax=15, method ="forward")
summary(regfit.fwd)
# We see that using forward stepwise selection, the best one-variable is recommended.

## Backward
regfit.bwd=regsubsets(fit.poly2.complete,data=merComplete,nvmax=15, method ="backward")
summary(regfit.bwd)

# The models found by best subset, forward and backward selection are equal.
ii=15; summary(regfit.full)$outmat[ii,]==summary(regfit.fwd)$outmat[ii,]

coef(regfit.full,15)
coef(regfit.fwd,15)
coef(regfit.bwd,15)

# Same results with cleaner output 
round(coef(regfit.full,15),3)
round(coef(regfit.fwd,15),3)
round(coef(regfit.bwd,15),3)


## Choosing Among Models Using the Validation Set Approach 
####### Validation Set Approach ####
set.seed (2019)

# Sampling with replacement 
train=sample(c(TRUE,FALSE), nrow(merComplete),rep=TRUE)
sum(train) # --> 39776
test=(!train)
sum(test) # --> 39800

# Apply best subset selection to the training set
regfit.best=regsubsets(fit.poly2.complete,data=merComplete[train,], nvmax =15)

# Make a model matrix from the test data.
# The model.matrix() function is used in many regression packages for 
# building an "X" matrix from data.
# Using a error matrix to calculate the test error
test.mat=model.matrix(fit.poly2.complete,data=merComplete[test,])

# Now we run a loop, and for each size i, we extract the coefficients 
# from regfit.best for the best model of that size, 
# multiply them into the appropriate columns of the test model matrix
# to form the predictions, and compute the test MSE.
# Compute the MSE test for best model from 1 to 15 regressors
val.errors=rep(NA,15)
for(i in 1:15){
  coefi = coef(regfit.best,id=i)
  pred = test.mat[,names(coefi)]%*%coefi
  val.errors[i] = mean((merComplete$overall[test]-pred)^2)
}
# The best model is the one that contains which.min(val.errors) =  14 variables.
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