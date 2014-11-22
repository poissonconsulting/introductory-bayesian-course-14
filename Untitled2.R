source("header.R")

data(peregrine)

peregrine$Proportion <- peregrine$R.Pairs / peregrine$Pairs

model1 <- jags_model("model {
  alpha ~ dnorm(0, 1^-2)
  beta ~ dnorm(0, 1^-2)
  beta2 ~ dnorm(0, 1^-2)
  sigma ~ dunif(0, 1)
  sDispersion ~ dunif(0, 5)
  for(i in 1:length(Pairs)) {
  eDispersion[i] ~ dnorm(0, sDispersion^-2)
    logit(eProportion[i]) <- alpha + beta * Year[i] + beta2 * Year[i]^2 + eDispersion[i]
    R.Pairs[i] ~ dbin(eProportion[i], Pairs[i])
  }
}",
derived_code = "data {
  for(i in 1:length(Pairs)) {
    logit(prediction[i]) <- alpha + beta * Year[i] + beta2 * Year[i]^2  
  }
}",
select_data = c("Pairs", "R.Pairs", "Year+"))
analysis1 <- jags_analysis(model1, data = peregrine)
prediction <- predict(analysis1)

gp <- ggplot(data = prediction, aes(x = Year, y = estimate))
gp <- gp + geom_point(data = dataset(analysis1), aes(y = Proportion))
gp <- gp + geom_line()
gp <- gp + geom_line(aes(y = lower), linetype = "dashed")
gp <- gp + geom_line(aes(y = upper), linetype = "dashed")
gp <- gp + scale_x_continuous(name = "Year")
gp <- gp + scale_y_continuous(name = "Pairs")
gp <- gp + expand_limits(y = c(0, 1))

quartz()
print(gp)


