# Linear regression - multiple regressor, without regularization 

attach(merComplete)

# Open a new windows
dev.new()

# Check Trend between overall and regressors
par(mfrow=c(3,2))
plot(seat_comfort, overall); plot(as.factor(seat_comfort), overall)
plot(cabin_service, overall); plot(as.factor(cabin_service), overall)
plot(food_bev, overall); plot(as.factor(food_bev), overall)

dev.new()
par(mfrow=c(4,2))
plot(entertainment, overall); plot(as.factor(entertainment), overall)
plot(ground_service, overall); plot(as.factor(ground_service), overall)
plot(wifi_connectivity, overall); plot(as.factor(wifi_connectivity), overall)
plot(value_for_money, overall); plot(as.factor(value_for_money), overall)
# The trend is not linear between overall and regressor

# Linear model with overall response and others parameters are regressors
fit <- lm(overall~recommended+seat_comfort+cabin_service+food_bev+entertainment+ground_service
            +wifi_connectivity+value_for_money, data = merComplete)

summary(fit)

# It shows confidence interval of variables
confint(fit)
dev.new()
par(mfrow=c(2,2))
plot(fit)

# The trend is not linear, so we fit polinomials-2 no-linear transformation.
fit2 <- lm(overall~recommended+cabin_service+ground_service+seat_comfort+wifi_connectivity
           +value_for_money+I(entertainment^2)+I(seat_comfort^2)+I(food_bev^2)+I(ground_service^2)
           +I(cabin_service^2)+I(ground_service^2)+I(value_for_money^2),data = merComplete)

summary(fit2)
confint(fit2)
dev.new()
par(mfrow=c(2,2))
plot(fit2)
# Compare the previous model "fit" with model "fit2"
anova(fit,fit2)

# Fit polinomials-3, no-linear transformation
fit3 <- lm(overall~recommended+poly(seat_comfort,3)+poly(cabin_service,3)
           +poly(food_bev,3)+poly(entertainment,3)+poly(ground_service,3)
          +poly(wifi_connectivity,3)+poly(value_for_money,3), data = merComplete)

summary(fit3)
confint(fit3)
dev.new()
par(mfrow=c(2,2))
plot(fit3)
anova(fit2,fit3)

# Fit log transformation
fit4 <- lm(overall~recommended+log(seat_comfort)+log(cabin_service)
           +log(food_bev)+log(entertainment)+log(ground_service)
           +log(wifi_connectivity)+log(value_for_money), data = merComplete)

summary(fit4)
confint(fit4)
dev.new()
par(mfrow=c(2,2))
plot(fit4)
anova(fit3,fit4)

# Polinomials-4 no-linear transformation
fit5 <- lm(overall~recommended+poly(seat_comfort,4)+poly(cabin_service,4)
           +poly(food_bev,4)+poly(entertainment,4)+poly(ground_service,4)
           +poly(wifi_connectivity,4)+poly(value_for_money,4), data = merComplete)

summary(fit5)
anova(fit3,fit5)
dev.new()
par(mfrow=c(2,2))
plot(fit5)

# The best transformation is polinomials-3 ?, non-linear transformation



