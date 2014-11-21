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
  alpha ~ dnorm(0, 20^-2)
  beta ~ dnorm(0, 2^-2)
  beta2 ~ dnorm(0, 2^-2)
  sigma ~ dunif(0, 2)

  for(i in 1:length(VOLUME)) {
    eLogVolume[i] <- alpha + beta * DISP[i] + beta2 * PRICE[i]
    VOLUME[i] ~ dlnorm(eLogVolume[i], sigma^-2)
  }
}",
derived_code = "data {
  for(i in 1:length(VOLUME)) {
    log(prediction[i]) <- alpha + beta * DISP[i] + beta2 * PRICE[i]
  }
}",
select_data = c("VOLUME", "DISP*", "log(PRICE)*"))

opts_jagr(mode = "report")
analysis1 <- jags_analysis(model1, data = cheese)
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

