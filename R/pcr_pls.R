library(pls)
attach(merComplete)

#################
###### PCR ######
set.seed (2)

# Use the pcr() function:
# Setting scale=TRUE has the effect of standardizing each predictor (scale projection)
# Setting validation = 'CV' has the effect to use cross validation to rate M parameter
pcr.fit=pcr(fit.poly2.complete,data = merComplete ,scale=TRUE, validation ="CV")

#Data: 	X dimension: [15] 79576  
#       Y dimension: [1]  79576
# By default, "pcr" gives us the RMSE (Root MSE) in terms of prediction with croos validation approach
# For each component, the result gives us the RMSE, as the number of components changes, and the variance explained.

# First line: variance explained as the number of regressors changes.
# Second line: variance explained as a function of overall variance.
# (Non tutta la varianza dei regressori mi serve per spiegare la varianza di y)

# Variance:
#   - explained: 100% when there are all regressors
#   - overall: 90.68%

summary(pcr.fit)
# Note that pcr() reports the root mean squared error; 
# It also provides the percentage of variance explained in the predictors and in the response using different numbers of components. 

# Plot the cross-validation scores
# Using val.type="MSEP" will cause the cross-validation MSE to be plotted.
dev.new()
validationplot(pcr.fit,val.type="MSEP",legendpos = "topright")
which.min(MSEP(pcr.fit)$val[1,,][-1])
# We see that the smallest CV error occurs when M = 15
# This suggests that there is no benefit in terms of reduction of dimensionality (M = 15)

# Now perform PCR on the training data and evaluate its test set performance:
set.seed (1)
x = model.matrix(fit.poly2.complete,data = merComplete)[,-1]

y = merComplete$overall
train=sample(1:nrow(x), nrow(x)/2) # another typical approach to sample
test=(-train)
y.test=y[test]

pcr.fit=pcr(fit.poly2.complete,data = merComplete ,subset=train,scale=TRUE,
            validation ="CV")
dev.new()
# Plot MSE and RMSE 
validationplot(pcr.fit,val.type="MSEP",legendpos = "topright") 
minPCR <- which.min(MSEP(pcr.fit)$val[1,,][-1]); minPCR # M=15 shows the lowest CV error
dev.new()
plot(RMSEP(pcr.fit),legendpos = "topright")

# We compute the test MSE as follows:
pcr.pred=predict(pcr.fit,x[test,], ncomp=minPCR)
mean((pcr.pred-y.test)^2) # --> 1.095797
# This test set MSE is competitive with the results obtained using ridge and the lasso

# Finally, we fit PCR on the full data set, using M = 15
pcr.fit=pcr(y~x,scale=TRUE,ncomp=which.min(MSEP(pcr.fit)$val[1,,][-1]))
summary(pcr.fit)
dev.new()
validationplot(pcr.fit,val.type="MSEP",legendpos = "topright")
dev.new()
plot(pcr.fit, ncomp = 15, asp = 1, line = TRUE)
coef(pcr.fit) ## get the coefficients

######################
######### PLS ########
set.seed (1)

# Using the plsr() function:
# Pls is similar to pcr, but is used to manage the variance of y 
# With pls, we don't concentrate our attention only on the regressions, but also the y variable
# Setting scale=TRUE has the effect of standardizing each predictor (scale projection).
# Setting validation = 'CV' has the effect to use cross validation to rate M parameter
pls.fit=plsr(fit.poly2.complete,data = merComplete, scale=TRUE, 
             validation ="CV")
summary(pls.fit)
dev.new()
validationplot(pls.fit,val.type="MSEP")
which.min(MSEP(pls.fit)$val[1,,][-1]) # M = 14

# Now perform Pls on the training data and evaluate its test set performance:
set.seed (1)
pls.fit=plsr(fit.poly2.complete,data = merComplete, subset=train, 
             scale=TRUE, validation ="CV")

dev.new()
validationplot(pls.fit,val.type="MSEP"); 
which.min(MSEP(pls.fit)$val[1,,][-1]); # M = 9
pls.pred=predict(pls.fit,x[test,],ncomp=10)
mean((pls.pred-y.test)^2) # --> 1.095796
pls.pred=predict(pls.fit,x[test,],ncomp=9)
mean((pls.pred-y.test)^2) # --> 1.095787
pls.pred=predict(pls.fit,x[test,],ncomp=8)
mean((pls.pred-y.test)^2) # --> 1.095803
# The test MSE is comparable to (slightly higher) the test MSE obtained using ridge regression, the lasso, and PCR.

# Finally, we perform PLS using the full data set, using M = 15, 
pls.fit=plsr(fit.poly2.complete,data = merComplete ,scale=TRUE,ncomp=15)
summary(pls.fit)
#Final result: with 5 components we can explain the same variance of y obtained with 15 components