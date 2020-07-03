# Linear regression - multiple regressor, without regularization 

attach(merComplete)

# Open a new windows
dev.new()

# Check Trend between overall and regressors

par(mfrow=c(3,2))
plot(seat_comfort, overall, main="overall-regressors trend"); plot(as.factor(seat_comfort), overall,main="overall-regressors trend",xlab="seat comfort", ylab="overall")
plot(cabin_service, overall); plot(as.factor(cabin_service), overall, xlab="cabin service", ylab="overall")
plot(food_bev, overall); plot(as.factor(food_bev), overall, xlab="food bev", ylab="overall")

dev.new()
par(mfrow=c(4,2))
plot(entertainment, overall,main="overall-regressors trend"); plot(as.factor(entertainment), overall,main="overall-regressors trend", xlab="entertainment", ylab="overall")
plot(ground_service, overall); plot(as.factor(ground_service), overall, xlab="groud service", ylab="overall")
plot(wifi_connectivity, overall); plot(as.factor(wifi_connectivity), overall, xlab="wifi connectivity", ylab="overall")
plot(value_for_money, overall); plot(as.factor(value_for_money), overall, xlab="value for money", ylab="overall")
# The trend is not linear between overall and regressors

# Linear model with overall response and others parameters are regressors

fit <- lm(fit.linear, data = merComplete)

summary(fit)

# It shows confidence interval of variables
confint(fit)
dev.new()
par(mfrow=c(2,2))
plot(fit)

# The trend is not linear, so we fit polinomials-2 no-linear transformation.
              
fit2 <- lm(fit.poly2.complete ,data = merComplete)

summary(fit2)
confint(fit2)
dev.new()
par(mfrow=c(2,2))
plot(fit2)
# Compare the previous model "fit" with model "fit2"
anova(fit,fit2)

# Fit polinomials-3, no-linear transformation
  
fit3 <- lm(fit.poly3, data = merComplete)

summary(fit3)
confint(fit3)
dev.new()
par(mfrow=c(2,2))
plot(fit3)
anova(fit2,fit3)

# Fit log transformation

fit5 <- lm(fit.log , data = merComplete)

summary(fit5)
confint(fit5)
dev.new()
par(mfrow=c(2,2))
plot(fit5)
anova(fit3,fit5)

# Polinomials-4 no-linear transformation
  
fit4 <- lm(fit.poly4, data = merComplete)

summary(fit4)
anova(fit3,fit4)
dev.new()
par(mfrow=c(2,2))
plot(fit4)


# The best transformation is polinomials-4, non-linear transformation
