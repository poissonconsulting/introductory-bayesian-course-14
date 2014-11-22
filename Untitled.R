source("header.R")

data(peregrine)
peregrine <- left_join(data.frame(Year = 1964:2006), peregrine)
peregrine$Year <- factor(peregrine$Year)

model6 <- jags_model("model {

  mean_r ~ dnorm(0, 1^-2)
  
  sd_r ~ dunif(0, 1)
  
  logN[1] ~ dnorm(0, 10^-2)
  for(i in 2:nYear) {
    r[i-1] ~ dnorm(mean_r, sd_r^-2)
    logN[i] <- logN[i-1] + r[i-1]
  }
  for(i in 1:length(Pairs)) {
    Pairs[i] ~ dpois(exp(logN[Year[i]]))
  }
  logN1 <- logN[1]
}",
derived_code = "data {
  for(i in 1:length(Pairs)) {
    log(prediction[i]) <- logN[Year[i]]
  }
}",
random_effects = list(r = "Year", logN = "Year"),
select_data = c("Pairs", "Year"))

peregrine$Year <- factor(peregrine$Year)
analysis6 <- jags_analysis(model6, data = peregrine)
coef(analysis6)

prediction <- predict(analysis6, base = data.frame(Year = as.factor(1970)))

gp <- ggplot(data = prediction, aes(x = as.integer(as.character(Year)), y = estimate))
gp <- gp + geom_line()
gp <- gp + geom_line(aes(y = lower), linetype = "dashed")
gp <- gp + geom_line(aes(y = upper), linetype = "dashed")
gp <- gp + scale_x_continuous(name = "Year")
gp <- gp + scale_y_continuous(name = "Pairs")
gp <- gp + expand_limits(y = 0)

quartz()
print(gp)


