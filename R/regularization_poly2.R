# Linear regression - multiple regressor, with regularization 
###### REGULARIZATION #######

library(glmnet)
attach(merComplete)
x = model.matrix(fit.poly2.complete, merComplete)[,-1] #[-1] means no intercept

y = merComplete$overall

# The model.matrix() function is particularly useful for creating x; not only does it produce a matrix 
# corresponding to the 15 predictors but it also automatically transforms any qualitative
# variables into dummy variables (recommended).
# The latter property is important because glmnet() can only take numerical, quantitative inputs.

########################
######### Ridge ########

grid=10^seq(10,-2,length=100) # Lambda values grid (from 10^10 to 10^-2)

# Inputs of glmnet: regressors matrix, dependent variable and alpha value:
# alpha = 0 (ridge regression), alpha = 1 (lasso)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid) 


dim(coef(ridge.mod)) # 16 coefficients, 100 lambda values

# We expect the coefficient estimates to be much smaller, in terms of l2 norm, when a large value of lambda is used, as
# compared to when a small value is used.
# grid[50]
ridge.mod$lambda[50] # lambda = 11497.57
coef(ridge.mod)[,50] # corresponding coefficients
sqrt(sum(coef(ridge.mod)[-1,50]^2)) # l2 norm

# grid[60]
ridge.mod$lambda[60] # lambda = 705.48
coef(ridge.mod)[,60] # corresponding coefficients
sqrt(sum(coef(ridge.mod)[-1,60]^2)) # l2 norm for lambda[60] > l2 for lambda[50]
# As lambda decreases -> the coefficients increase. As lambda increases -> the coefficients decrease

predict(ridge.mod,s=50,type="coefficients")[1:16,] # s = lambda value, predict of the coefficients

# Validation approach to estimate test error
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2) # another typical approach to sample
test=(-train)
y.test=y[test]

# Fit a ridge regression model on the training set, and evaluate its MSE on the test set, using lambda (s) = 4. 
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,]) # Note the use of the predict() function for a test set

mean((ridge.pred-y.test)^2) # test MSE = 1.62354

# test MSE, è mse di test con un modello che usa il valore medio delle y del training set, praticamente il modello con la
# la sola intercetta, cioè tutti gli altri coefficienti = 0. Capire la capacità predittiva di un modello banale, 
# infatti il mse test è motlo più grande rispetto a quello calcolato precedentemente.

# Two predictions with different (arbitrary) values of lambda: 
# lambda --> +OO (10^10) means coefficients close to zero
ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2) # 11.85287 like intercept only

# lambda --> 0 means that Least squares is simply ridge regression.
ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T,x=x[train,],y=y[train])
mean((ridge.pred-y.test)^2) # 1.095796
# Same result obtained with lm(), exact = T works again on the model (but not the known model)

# Comparation of the results between lm() and glmnet when lambda=0:
lm(y~x, subset=train)
predict(ridge.mod,s=0,exact=T,type="coefficients",x=x[train,],y=y[train])[1:16,]
#In our case, best results are obtained with MSE compared to Ridge Regression


#Instead of arbitrary values, we now use method "Cross validation" to estimate lambda:
set.seed (1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
dev.new()

# gradi di libertà = quali sono i coefficienti diversi da zero, le righe in verticale sono il range di valori che si puo 
# scegliere per lambda senza cambiare la predittività del nostro modello

plot(cv.out)
bestlam=cv.out$lambda.min
bestlam # the best lambda 
log(bestlam) # log value of previous lambda 

# Prediction of the model with the best value of lambda
ridge.pred=predict(ridge.mod,s=bestlam ,newx=x[test,])
mean((ridge.pred-y.test)^2) # MSE = 1.11337

# Prediction of the coefficients with the best value of lambda
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:16,]
# As expected, none of the coefficients is zero

# Figure that show the variation of the coefficients with different values of lambda
dev.new()
plot(out,label = T, xvar = "lambda")


#####################
####### LASSO #######

# Use the argument alpha = 1 to perform Lasso
lasso.mod = glmnet(x[train,], y[train], alpha=1, lambda=grid)
dev.new()
plot(lasso.mod,label = T) # L1 norm

# Figure that show the variation of the coefficients with different values of lambda
dev.new()
plot(lasso.mod,label = T, xvar = "lambda")

# Perform Cross-Validation for estimate the best lambda to minimize "mse test"
set.seed (1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
dev.new()
plot(cv.out)
bestlam=cv.out$lambda.min; print(bestlam);print(log(bestlam)) # the best lambda

lasso.pred=predict(lasso.mod,s=bestlam ,newx=x[test,])
mean((lasso.pred-y.test)^2) # 1.097244 slighly larger than ridge

# Comparation between lm() and lasso-model with lambda = 0
lasso.pred=predict(lasso.mod,s=0,newx=x[test,],exact=T,x=x[train,],y=y[train])
mean((lasso.pred-y.test)^2) # 1.09579

out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:16,]
lasso.coef
lasso.coef[lasso.coef!=0]
cat("Number of coefficients equal to 0:",sum(lasso.coef==0),"\n")
