#install.packages("bayesm")

source("header.R")
library(bayesm)
data(cheese)
#http://www.r-tutor.com/gpu-computing/rbayes/rhierlmc

quartz()
qplot(DISP, VOLUME, data = cheese) + facet_wrap(~RETAILER)

quartz()
qplot(log(PRICE), VOLUME, data = cheese)

model1 <- jags_model("model {
  alphaTypical ~ dnorm(0, 20^-2)
  sAlpha ~ dnorm(0, 10^-2)
  for(i in 1:nRETAILER) {
    alpha[i] ~ dnorm(0, sAlpha^-2)
  }
  beta ~ dnorm(0, 2^-2)
  beta2 ~ dnorm(0, 2^-2)
  sigma ~ dunif(0, 2)

  for(i in 1:length(VOLUME)) {
    eLogVolume[i] <- alphaTypical + alpha[RETAILER[i]] + beta * DISP[i] + beta2 * PRICE[i]
    VOLUME[i] ~ dlnorm(eLogVolume[i], sigma^-2)
  }
}",
derived_code = "data {
  for(i in 1:length(VOLUME)) {
    log(prediction[i]) <- alphaTypical + alpha[RETAILER[i]] + beta * DISP[i] + beta2 * PRICE[i]
  }
}",
random_effects = list(alpha = "RETAILER"),
select_data = c("VOLUME", "DISP*", "log(PRICE)*", "RETAILER"))

opts_jagr(mode = "report")
analysis1 <- jags_analysis(model1, data = cheese)
coef(analysis1)
quartz()
plot(analysis1)

price <- predict(analysis1, newdata = "PRICE")

gp <- ggplot(data = price, aes(x = PRICE, y = estimate))
gp <- gp + geom_line()
gp <- gp + geom_line(aes(y = lower), linetype = "dashed")
gp <- gp + geom_line(aes(y = upper), linetype = "dashed")
gp <- gp + expand_limits(y = 0)

quartz()
print(gp)

disp <- predict(analysis1, newdata = "DISP")

gp <- gp %+% disp
gp <- gp + aes(x = DISP)

quartz()
print(gp)

retailer <- predict(analysis1, newdata = "RETAILER", base = TRUE)

gp <- ggplot(data = retailer, aes(x = RETAILER, y = estimate))
gp <- gp + geom_pointrange(aes(ymin = lower, ymax = upper))
gp <- gp + scale_y_continuous(labels = percent)

quartz()
print(gp)
