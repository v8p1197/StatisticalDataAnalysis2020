###### REGULARIZATION #######

library(glmnet)
attach(merComplete)
x = model.matrix(overall~recommended+seat_comfort+cabin_service+food_bev+entertainment+ground_service
                 +wifi_connectivity+value_for_money, merComplete)[,-1] # without 1's Non vuole l'intercetta

y = merComplete$overall
 
# The model.matrix() function is particularly useful for creating x; not only does it produce a matrix 
# corresponding to the 8 predictors but it also automatically transforms any qualitative variables into dummy variables (recommended).
# The latter property is important because glmnet() can only take numerical, quantitative inputs.

######### Ridge ########

grid=10^seq(10,-2,length=100) # Griglia di valori per lambda (tra 10^10 a 10^-2)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid) # glmnet vuole la matrice dei regressori, la variabile dipendente e vuole che 
# si specifichi la tecnica da utilizzare aplha = 0 (ridge regression), alpha = 1 (lasso)


dim(coef(ridge.mod)) # 9 coefficienti e 100 valori di lambda

# We expect the coefficient estimates to be much smaller, in terms of l2 norm, when a large value of lambda is used, as
# compared to when a small value is used. 
ridge.mod$lambda[50] # grid[50] = 11497.57
coef(ridge.mod)[,50] # corresponding coefficients
sqrt(sum(coef(ridge.mod)[-1,50]^2)) # l2 norm
ridge.mod$lambda[60] # lambda = 705.48
coef(ridge.mod)[,60] # corresponding coefficients
# Al diminuire di lambda aumentano i coefficienti e all'aumentare di lambda diminuiscono i coefficienti
sqrt(sum(coef(ridge.mod)[-1,60]^2)) # l2 norm > l2 for lambda[50]

predict(ridge.mod,s=50,type="coefficients")[1:9,] # s = valore di lambda, predict of the coefficients

# Validation approach to estimate test error
set.seed(2017)
train=sample(1:nrow(x), nrow(x)/2) # another typical approach to sample
test=(-train)
y.test=y[test]

# fit a ridge regression model on the training set, and evaluate its MSE on the test set, using lambda = 4. 
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,]) # Note the use of the predict() function for a test set

mean((ridge.pred-y.test)^2) # test MSE

# test MSE, è mse di test con un modello che usa il valore medio delle y del training set, praticamente il modello con la
# la sola intercetta, cioè tutti gli altri coefficienti = 0. Capire la capacità predittiva di un modello banale, 
# infatti il mse test è motlo più grande rispetto a quello calcolato precedentemente.

mean((mean(y[train ])-y.test)^2) # DA RIVEDDERE 

# predizione con lambda --> +OO cioè significa tutti i coefficienti prossimi a zero, infatti si trova più o meno con lambda = 10^10

ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2) # like intercept only

# Least squares is simply ridge regression with lambda=0, stesso risultato che ho ottenuto con lm(), exact rifà i calcoli del modello,
# non utilizza un modello gia noto
ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T,x=x[train,],y=y[train])
mean((ridge.pred-y.test)^2)

# Comapre the results from glmnet when lambda=0 with lm(). Nel nostro caso la ridge regression da risultati peggiori rispetto a mse
lm(y~x, subset=train)
predict(ridge.mod,s=0,exact=T,type="coefficients",x=x[train,],y=y[train])[1:9,] # corrected according to errata 


# per ora abbiamo dato dei valori arbitrari a lambda, ora utilizziamo dei metodi per stimare lambda (cross validation)
set.seed (2016)
cv.out=cv.glmnet(x[train,],y[train],alpha=0) #cross validation
dev.new()
# gradi di libertà = quali sono i coefficienti diversi da zero, le righe in verticale sono il range di valori che si puo 
# scegliere per lambda senza cambiare la predittività del nostro modello

plot(cv.out)
bestlam=cv.out$lambda.min; bestlam;log(bestlam) # the best lambda 

#cv.out$lambda.1se
# Predizione del modello con la miglior lambda
ridge.pred=predict(ridge.mod,s=bestlam ,newx=x[test,])
mean((ridge.pred-y.test)^2)


out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:9,]
# As expected, none of the coefficients are zero

# Come variano i coefficienti al variare di lambda
dev.new()
plot(out,label = T, xvar = "lambda")

####### LASSO #######

# use the argument alpha = 1 to perform lasso
lasso.mod = glmnet(x[train,], y[train], alpha=1, lambda=grid)
dev.new()
plot(lasso.mod,label = T) # L1 norm
dev.new()
plot(lasso.mod,label = T, xvar = "lambda") # In funzione di lamda

# perform cross-validation for estimate the best labda to minimize mse test
set.seed (1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
dev.new()
plot(cv.out)
bestlam=cv.out$lambda.min; print(bestlam);print(log(bestlam))
print(cv.out$lambda.1se)
lasso.pred=predict(lasso.mod,s=bestlam ,newx=x[test,])
mean((lasso.pred-y.test)^2) # slighly larger than ridge
# wrt lm
lasso.pred=predict(lasso.mod,s=0,newx=x[test,],exact=T,x=x[train,],y=y[train])
mean((lasso.pred-y.test)^2)
# However, the lasso has a substantial advantage:
# 8 of the 19 coefficient estimates are exactly zero (12 on the text).
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:9,]
lasso.coef
lasso.coef[lasso.coef!=0]
cat("Number of coefficients equal to 0:",sum(lasso.coef==0),"\n")

# compare with OLS when only selected predictors are included. 
#fit.lm=lm(overall~recommended+seat_comfort+cabin_service+food_bev+entertainment+ground_service
#                         +value_for_money, data=merComplete) # 1 coeff = 0
fit.lm=lm(overall~recommended+seat_comfort+cabin_service+food_bev+entertainment+ground_service
          +value_for_money, data=merComplete)
coef(fit.lm)
# lasso.coef=predict(out,type="coefficients",s=0)[1:20,]
# lasso.coef
# coef(lm(Salary~., data=Hitters)) # differences on the 3rd place