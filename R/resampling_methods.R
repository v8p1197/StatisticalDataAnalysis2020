library(readr)
merComplete <- read_csv("Data/preprocessed_complete.csv")
View(merComplete)
merComplete$X1<-NULL

# Delete prevoius code

attach(merComplete)


n = nrow(merComplete)

######### Validation Set Approch #########

set.seed(1)
train=sample(1:n,n/2)
lm.fit=lm(fit.linear, data = merComplete, subset = train)

# the estimated test MSE for the linear regression fit is 1.111269 (seed=1)
mean(((overall-predict(lm.fit,merComplete))[-train])^2)

# use the poly() function to estimate the test error for the polynomials-2 transformation.
lm.fit2=lm(fit.poly2.complete, data = merComplete, subset=train)

# the estimated test MSE for the linear regression fit is 1.095797 (seed=1)
mean(((overall-predict(lm.fit2,merComplete))[-train])^2)

# use the poly() function to estimate the test error for the polynomials-3 transformation.
lm.fit3=lm(fit.poly3, data = merComplete, subset=train) 

# the estimated test MSE for the linear regression fit is 1.094223 (seed=1)
mean(((overall-predict(lm.fit3,merComplete))[-train])^2)

# use the poly() function to estimate the test error for the polynomials-4 transformation.
lm.fit4=lm(fit.poly4, data = merComplete, subset=train) 

# the estimated test MSE for the linear regression fit is 1.093318 (seed=1)
mean(((overall-predict(lm.fit4,merComplete))[-train])^2)

# use the poly() function to estimate the test error for the log transformation.
lm.fit5=lm(fit.log, data = merComplete, subset=train) 

# the estimated test MSE for the linear regression fit is 1.093318 (seed=1)
mean(((overall-predict(lm.fit5,merComplete))[-train])^2)

## Changing the seed, the results remain consistent with our previous findings


########## K-Fold Cross Validation ##########
library(boot)

glm.fit=glm(fit.poly4 ,data=merComplete)

cv.err=cv.glm(merComplete,glm.fit, K = 8)
cv.err$delta # The K-Fold Cross validation estimate for the test error is approximately 1.102361 (seed=1).

# K-Fold Cross validation for polynomial regressions with orders i=1,2,...,4.

cv.error=rep(0,4)
for (i in 1:4){
  glm.fit=glm(overall~recommended+poly(seat_comfort,i)+poly(cabin_service,i)
              +poly(food_bev,i)+poly(entertainment,i)+poly(ground_service,i)
              +poly(wifi_connectivity,i)+poly(value_for_money,i), data = merComplete)
  cv.error[i]=cv.glm(merComplete,glm.fit, K=8)$delta[1]
}
cv.error
# We still see little evidence that using cubic or higher-order polynomial terms leads to lower test error than simply


########## Bootstrap ##########

# The boot.fn() function can also be used in order to create bootstrap estimates 
# for the intercept and slope terms by randomly sampling from among the observations with replacement
# We will compare the estimates obtained using the bootstrap to those obtained using the previous models
library(stringr)

# No-transformation
set.seed (2)
boot.fn=function(data,index){
  return(coef(lm(overall~recommended+seat_comfort+cabin_service+food_bev+entertainment+ground_service
                 +wifi_connectivity+value_for_money, data = data,subset=index)))
}
boot.fn(merComplete, 1:n)

# Boot estimate is not deterministic
boot.fn(merComplete,sample(1:n, 79576,replace=T))
boot.fn(merComplete,sample(1:n, 79576,replace=T))
# We use the boot() function to compute the standard errors 
# of 1,000 bootstrap estimates for the intercept and slope terms.
b = boot(merComplete ,boot.fn ,1000)

s = summary(lm(fit.linear, data = merComplete))

# Take all std. errors of the bootstrap estimate 
x <- capture.output(b)
x <- str_extract(x, "^t[0-9.]+.*$")
x <- x[!is.na(x)]
se <- as.numeric(unlist(str_extract_all(x, '[0-9.]+$')))

# Take all std. errors of the linear model
c = s$coefficients[ ,2]
c = as.numeric(c)

cat("\nDifference between no-Transformation Std.errors:\n",c - se,"\n")


# Polinomials-2 no-linear transformation
set.seed (2)

boot.fn=function(data,index){
  return(coef(lm(overall~recommended+poly(seat_comfort,2)+poly(cabin_service,2)
                 +poly(food_bev,2)+poly(entertainment,2)+poly(ground_service,2)
                 +poly(wifi_connectivity,2)+poly(value_for_money,2),data = data,subset=index)))
}
boot.fn(merComplete, 1:n)

boot.fn(merComplete,sample(1:n, 79576,replace=T))

# We use the boot() function to compute the standard errors 
# of 1,000 bootstrap estimates for the intercept and slope terms.

b = boot(merComplete ,boot.fn ,1000)

s = summary(lm(fit.poly2.complete,data = merComplete))

# Take all std. errors of the bootstrap estimate 
x <- capture.output(b)
x <- str_extract(x, "^t[0-9.]+.*$")
x <- x[!is.na(x)]
se <- as.numeric(unlist(str_extract_all(x, '[0-9.]+$')))

# Take all std. errors of the poly-2 transformation
c = s$coefficients[ ,2]
c = as.numeric(c)

cat("\nDifference between poly-2 transformation Std.errors:\n",c - se,"\n")

# Polinomials-3 no-linear transformation
set.seed (2)

boot.fn=function(data,index){
  temp <- index
  return(coef(lm(fit.poly3, data = data, subset=temp)))
}

boot.fn(merComplete, 1:n)

boot.fn(merComplete,sample(1:n, 79576,replace=T))

b = boot(merComplete ,boot.fn ,1000)

s = summary(lm(fit.poly3, data = merComplete))

# Take all std. errors of the bootstrap estimate 
x <- capture.output(b)
x <- str_extract(x, "^t[0-9.]+.*$")
x <- x[!is.na(x)]
se <- as.numeric(unlist(str_extract_all(x, '[0-9.]+$')))

# Take all std. errors of the poly-3 transformation
c = s$coefficients[ ,2]
c = as.numeric(c)

cat("\nDifference between poly-3 transformation Std.errors:\n",c - se,"\n")

# Polinomials-4 no-linear transformation
set.seed (2)

boot.fn=function(data,index){
  return(coef(lm(fit.poly4, data = data,subset=index)))
}

boot.fn(merComplete, 1:n)


boot.fn(merComplete,sample(1:n, 79576,replace=T))

boot(merComplete ,boot.fn ,1000)

summary(lm(fit.poly4, data = merComplete))

# Take all std. errors of the bootstrap estimate 
x <- capture.output(b)
x <- str_extract(x, "^t[0-9.]+.*$")
x <- x[!is.na(x)]
se <- as.numeric(unlist(str_extract_all(x, '[0-9.]+$')))

# Take all std. errors of the poly-4 transformation
c = s$coefficients[ ,2]
c = as.numeric(c)

cat("\nDifference between poly-4 transformation Std.errors:\n",c - se,"\n")


######## Plot ########

## Plot linear model
dev.new()
plot(as.factor(seat_comfort),overall,main='Linear Model', xlab="seat comfort", ylab="overall")
xx=seq(min(seat_comfort),max(seat_comfort),along.with = seat_comfort)
ci_lin <- predict(lm(overall~seat_comfort,data=merComplete),newdata=data.frame(seat_comfort=xx),se.fit = T,interval = "confidence")
matplot(xx,ci_lin$fit[,1],lty=1, ltw=2, col="red", type="l", add=T)

## Plot linear model with polinomials-2 transformation
dev.new()
plot(as.factor(seat_comfort),overall,main='Polinomial-2 Model', xlab="seat comfort", ylab="overall")
xx=seq(min(seat_comfort),max(seat_comfort),along.with = seat_comfort)
ci_lin <- predict(lm(overall~I(seat_comfort^2),data=merComplete),newdata=data.frame(seat_comfort=xx),se.fit = T,interval = "confidence")
matplot(xx,ci_lin$fit[,1],lty=1, ltw=2, col="red", type="l", add=T)

## Plot linear model with polinomials-3 transformation
dev.new()
plot(as.factor(seat_comfort),overall,main='Polinomial-3 Model', xlab="seat comfort", ylab="overall")
xx=seq(min(seat_comfort),max(seat_comfort),along.with = seat_comfort)
ci_lin <- predict(lm(overall~I(seat_comfort^3),data=merComplete),newdata=data.frame(seat_comfort=xx),se.fit = T,interval = "confidence")
matplot(xx,ci_lin$fit[,1],lty=1, ltw=2, col="red", type="l", add=T)

## Plot linear model with polinomials-4 transformation
dev.new()
plot(as.factor(seat_comfort),overall,main='Polinomial-4 Model', xlab="seat comfort", ylab="overall")
xx=seq(min(seat_comfort),max(seat_comfort),along.with = seat_comfort)
ci_lin <- predict(lm(overall~I(seat_comfort^4),data=merComplete),newdata=data.frame(seat_comfort=xx),se.fit = T,interval = "confidence")
matplot(xx,ci_lin$fit[,1],lty=1, ltw=2, col="red", type="l", add=T)


