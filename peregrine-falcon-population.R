source("header.R")

model1 <- jags_model("model {
  alpha ~ dnorm(0, 100^-2)
  beta ~ dnorm(0, 100^-2)
  sigma ~ dunif(0, 100)
  for(i in 1:length(Pairs)) {
    ePairs[i] <- alpha + beta * Year[i]
    Pairs[i] ~ dnorm(ePairs[i], sigma^-2)
  }
}",
                     derived_code = "data {
  for(i in 1:length(Pairs)) {
    prediction[i] <- alpha + beta * Year[i]
  }
}",
                     select_data = c("Pairs", "Year+"))

data(peregrine)

analysis1 <- jags_analysis(model1, data = peregrine)

coef(analysis1)

message("Do you consider the model to be adequate?")

prediction <- predict(analysis1)

gp <- ggplot(data = prediction, aes(x = Year, y = estimate))
gp <- gp + geom_point(data = dataset(analysis1), aes(y = Pairs))
gp <- gp + geom_line()
gp <- gp + geom_line(aes(y = lower), linetype = "dashed")
gp <- gp + geom_line(aes(y = upper), linetype = "dashed")
gp <- gp + scale_y_continuous(name = "Pairs")

print(gp)

message("Replace `ePairs[i] <- ` with `log(ePairs[i]) <- ...`. How does the log-link function alter the model?")

model2 <- jags_model("model {
  alpha ~ dnorm(0, 100^-2)
  beta ~ dnorm(0, 100^-2)
  sigma ~ dunif(0, 100)
  for(i in 1:length(Pairs)) {
    log(ePairs[i]) <- alpha + beta * Year[i]
    Pairs[i] ~ dnorm(ePairs[i], sigma^-2)
  }
}",
                     derived_code = "data {
  for(i in 1:length(Pairs)) {
    log(prediction[i]) <- alpha + beta * Year[i]
  }
}",
                     select_data = c("Pairs", "Year+"))

analysis2 <- jags_analysis(model2, data = peregrine)

coef(analysis2)

prediction <- predict(analysis2)

gp <- ggplot(data = prediction, aes(x = Year, y = estimate))
gp <- gp + geom_point(data = dataset(analysis2), aes(y = Pairs))
gp <- gp + geom_line()
gp <- gp + geom_line(aes(y = lower), linetype = "dashed")
gp <- gp + geom_line(aes(y = upper), linetype = "dashed")
gp <- gp + scale_y_continuous(name = "Pairs")

print(gp)

exercise("Next, replace `Pairs[i] ~ dnorm(ePairs[i], sigma^-2)` with `Pairs[i] ~ dpois(ePairs[i])`.
         How does the assumption of Poisson distributed counts alter the model?")

model3 <- jags_model("model {
  alpha ~ dnorm(0, 100^-2)
  beta ~ dnorm(0, 100^-2)
  for(i in 1:length(Pairs)) {
    log(ePairs[i]) <- alpha + beta * Year[i]
    Pairs[i] ~ dpois(ePairs[i])
  }
}",
                     derived_code = "data {
  for(i in 1:length(Pairs)) {
    log(prediction[i]) <- alpha + beta * Year[i]
  }
}",
                     select_data = c("Pairs", "Year+"))

analysis3 <- jags_analysis(model3, data = peregrine)

coef(analysis3)

prediction <- predict(analysis3)

gp <- ggplot(data = prediction, aes(x = Year, y = estimate))
gp <- gp + geom_point(data = dataset(analysis3), aes(y = Pairs))
gp <- gp + geom_line()
gp <- gp + geom_line(aes(y = lower), linetype = "dashed")
gp <- gp + geom_line(aes(y = upper), linetype = "dashed")
gp <- gp + scale_y_continuous(name = "Pairs")

print(gp)

exercise("Fit a second-order polynomial on `Year`. How does it alter the model?")

model4 <- jags_model("model {
  alpha ~ dnorm(0, 100^-2)
  beta ~ dnorm(0, 100^-2)
  beta2 ~ dnorm(0, 100^-2)
  for(i in 1:length(Pairs)) {
    log(ePairs[i]) <- alpha + beta * Year[i]  + beta2 * Year[i]^2
    Pairs[i] ~ dpois(ePairs[i])
  }
}",
                     derived_code = "data {
  for(i in 1:length(Pairs)) {
    log(prediction[i]) <- alpha + beta * Year[i]  + beta2 * Year[i]^2
  }
}",
                     select_data = c("Pairs", "Year+"))

analysis4 <- jags_analysis(model4, data = peregrine)

coef(analysis4)

prediction <- predict(analysis4)

gp <- ggplot(data = prediction, aes(x = Year, y = estimate))
gp <- gp + geom_point(data = dataset(analysis4), aes(y = Pairs))
gp <- gp + geom_line()
gp <- gp + geom_line(aes(y = lower), linetype = "dashed")
gp <- gp + geom_line(aes(y = upper), linetype = "dashed")
gp <- gp + scale_y_continuous(name = "Pairs")

print(gp)

exercise("Fit a third-order polynomial (`beta3 * Year[i]^3`). How does it alter the model?")

model5 <- jags_model("model {
  alpha ~ dnorm(0, 100^-2)
  beta ~ dnorm(0, 100^-2)
  beta2 ~ dnorm(0, 100^-2)
  beta3 ~ dnorm(0, 100^-2)
  for(i in 1:length(Pairs)) {
    log(ePairs[i]) <- alpha + beta * Year[i]  + beta2 * Year[i]^2 + beta3 * Year[i]^3
    Pairs[i] ~ dpois(ePairs[i])
  }
}",
derived_code = "data {
  for(i in 1:length(Pairs)) {
    log(prediction[i]) <- alpha + beta * Year[i]  + beta2 * Year[i]^2 + beta3 * Year[i]^3
  }
}",
select_data = c("Pairs", "Year+"))

analysis5 <- jags_analysis(model5, data = peregrine)

coef(analysis5)

prediction <- predict(analysis5)

gp <- ggplot(data = prediction, aes(x = Year, y = estimate))
gp <- gp + geom_point(data = dataset(analysis4), aes(y = Pairs))
gp <- gp + geom_line()
gp <- gp + geom_line(aes(y = lower), linetype = "dashed")
gp <- gp + geom_line(aes(y = upper), linetype = "dashed")
gp <- gp + scale_y_continuous(name = "Pairs")

print(gp)

exercise("Use the third-order polynomial model to predict the number of breeding pairs in 2006.
         How confident are you in your answer?")

predict(analysis5, newdata = data.frame(Year = as.integer(2006)))
