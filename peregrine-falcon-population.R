source("header.R")

model1 <- jags_model("model {
  alpha ~ dnorm(0, 20000^-2)
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


prediction <- predict(analysis1)

gp <- ggplot(data = prediction, aes(x = Year, y = estimate))
gp <- gp + geom_point(data = dataset(analysis1), aes(y = Pairs))
gp <- gp + geom_line()
gp <- gp + geom_line(aes(y = lower), linetype = "dashed")
gp <- gp + geom_line(aes(y = upper), linetype = "dashed")
gp <- gp + scale_y_continuous(name = "Pairs")

quartz()
print(gp)

source("header.R")

model1 <- jags_model("model {
  alpha ~ dnorm(0, 20000^-2)
  beta ~ dnorm(0, 100^-2)
  beta2 ~ dnorm(0, 100^-2)
  beta3 ~ dnorm(0, 100^-2)
  sDispersion ~ dunif(0, 5)
  for(i in 1:length(Pairs)) {
    eDispersion[i] ~ dgamma(1 / sDispersion^2, 1 / sDispersion^2)
    log(ePairs[i]) <- alpha + beta * Year[i] + beta2 * Year[i]^2 + beta3 * Year[i]^3
    Pairs[i] ~ dpois(ePairs[i] * eDispersion[i])
  }
}",
                     derived_code = "data {
  for(i in 1:length(Pairs)) {
    log(prediction[i]) <- alpha + beta * Year[i] + beta2 * Year[i]^2 + beta3 * Year[i]^3
  }
}",
                     select_data = c("Pairs", "Year+"))

data(peregrine)

analysis1 <- jags_analysis(model1, data = peregrine)

coef(analysis1)


prediction <- predict(analysis1)

gp <- ggplot(data = prediction, aes(x = Year, y = estimate))
gp <- gp + geom_point(data = dataset(analysis1), aes(y = Pairs))
gp <- gp + geom_line()
gp <- gp + geom_line(aes(y = lower), linetype = "dashed")
gp <- gp + geom_line(aes(y = upper), linetype = "dashed")
gp <- gp + scale_y_continuous(name = "Pairs")
gp <- gp + expand_limits(y = 0)
quartz()
print(gp)

predict(analysis1, newdata = data.frame(Year = as.integer(2006)))

select_data(model1) <- c("Pairs", "Year")

analysis2 <- jags_analysis(model1, data = peregrine)

prediction <- predict(analysis2)
gp <- gp %+% prediction
quartz()
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

message("Next, replace `Pairs[i] ~ dnorm(ePairs[i], sigma^-2)` with `Pairs[i] ~ dpois(ePairs[i])`.
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

exercise("Fit a second-order polynomial on `Year`. Does it improve the model?")

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

exercise("Fit a third-order polynomial (`beta3 * Year[i]^3`). Does it improve the model?")

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

model6 <- jags_model("model {
  mean_r ~ dnorm(0, 1^-2)
  sd_r ~ dunif(0, 1)
  
  logN1 ~ dnorm(0, 10^-2)
  logN[1] <- logN1
  for(i in 2:nYear) {
    logN[i] <- logN[i-1] + r[i-1]
  }
  
  for(i in 1:nYear) {
    r[i] ~ dnorm(mean_r, sd_r^-2)
    Pairs[i] ~ dpois(exp(logN[i]))
  }
}",
                     derived_code = "data {
  for(i in 1:length(Pairs)) {
    log(prediction[i]) <- logN[Year[i]]
  }
}",
                     modify_data = function (data) {
                       stopifnot(!is.unsorted(data$Year))
                       data
                     },
                     select_data = c("Pairs", "Year"),
                     random_effects = list(r = "Year", logN = "Year"))

peregrine$Year <- factor(peregrine$Year)
analysis6 <- jags_analysis(model6, data = peregrine)

coef(analysis6)

prediction <- predict(analysis6)

gp <- ggplot(data = prediction, aes(x = as.integer(as.character(Year)), y = estimate))
gp <- gp + geom_point(data = dataset(analysis6), aes(y = Pairs))
gp <- gp + geom_line()
gp <- gp + geom_line(aes(y = lower), linetype = "dashed")
gp <- gp + geom_line(aes(y = upper), linetype = "dashed")
gp <- gp + scale_x_continuous(name = "Year")
gp <- gp + scale_y_continuous(name = "Pairs")
gp <- gp + expand_limits(y = 0)

print(gp)

message("Plot the state-space population growth rate model predictions in terms 
         of the percent change since 1970. What is the estimated percent
         change in the population in 2003 compared to 1970?")

prediction <- predict(analysis6, newdata = "Year", base = data.frame(Year = factor(1970)))

gp <- ggplot(data = prediction, aes(x = as.integer(as.character(Year)), y = estimate))
gp <- gp + geom_line()
gp <- gp + geom_line(aes(y = lower), linetype = "dashed")
gp <- gp + geom_line(aes(y = upper), linetype = "dashed")
gp <- gp + scale_x_continuous(name = "Year")
gp <- gp + scale_y_continuous(name = "Population Change (%)", labels = percent)
gp <- gp + expand_limits(y = 0)

print(gp)

filter(prediction, Year == "2003")

message("What is the probability that the population in 2008 will be
         less than that in 2003? Note you can produce the projections
         by simply appending five years of missing counts to the dataset.")

peregrine <- left_join(data.frame(Year = factor(1964:2008)), peregrine)

analysis6 <- jags_analysis(model6, data = peregrine)

prediction <- predict(analysis6)

gp <- ggplot(data = prediction, aes(x = as.integer(as.character(Year)), y = estimate))
gp <- gp + geom_point(data = dataset(analysis6), aes(y = Pairs))
gp <- gp + geom_line()
gp <- gp + geom_line(aes(y = lower), linetype = "dashed")
gp <- gp + geom_line(aes(y = upper), linetype = "dashed")
gp <- gp + scale_x_continuous(name = "Year")
gp <- gp + scale_y_continuous(name = "Pairs")
gp <- gp + expand_limits(y = 0)

print(gp)

derived_code <- "
  data{ 
  for (i in 1:length(Year)) { 
  prediction[i] <- exp(logN[Year[i]]) < exp(logN[2003-1963])
  }
}
"
prediction <- predict(analysis6, derived_code = derived_code)

filter(prediction, Year == "2008")

data(peregrine)

peregrine$Proportion <- peregrine$R.Pairs / peregrine$Pairs

model7 <- jags_model("model {
  alpha ~ dnorm(0, 1^-2)
  beta ~ dnorm(0, 1^-2)
  sigma ~ dunif(0, 1)
  for(i in 1:length(Proportion)) {
    eProportion[i] <- alpha + beta * Year[i]
    Proportion[i] ~ dnorm(eProportion[i], sigma^-2)
  }
}",
                     derived_code = "data {
  for(i in 1:length(Proportion)) {
    prediction[i] <- alpha + beta * Year[i]
  }
}",
                     select_data = c("Proportion", "Year+"))

analysis7 <- jags_analysis(model7, data = peregrine)

prediction <- predict(analysis7)

gp <- ggplot(data = prediction, aes(x = Year, y = estimate))
gp <- gp + geom_point(data = dataset(analysis7), aes(y = Proportion))
gp <- gp + geom_line()
gp <- gp + geom_line(aes(y = lower), linetype = "dashed")
gp <- gp + geom_line(aes(y = upper), linetype = "dashed")
gp <- gp + scale_x_continuous(name = "Year")
gp <- gp + scale_y_continuous(name = "Pairs")
gp <- gp + expand_limits(y = c(0,1))

print(gp)

message("Fit a second-order polynomial regression to the proportion of successful pairs")

model8 <- jags_model("model {
  alpha ~ dnorm(0, 1^-2)
  beta ~ dnorm(0, 1^-2)
  beta2 ~ dnorm(0, 1^-2)
  sigma ~ dunif(0, 1)
  for(i in 1:length(Proportion)) {
    eProportion[i] <- alpha + beta * Year[i] + beta2 * Year[i]^2
    Proportion[i] ~ dnorm(eProportion[i], sigma^-2)
  }
}",
                     derived_code = "data {
  for(i in 1:length(Proportion)) {
    prediction[i] <- alpha + beta * Year[i] + beta2 * Year[i]^2
  }
}",
                     select_data = c("Proportion", "Year+"))

analysis8 <- jags_analysis(model8, data = peregrine)

prediction <- predict(analysis8)

gp <- ggplot(data = prediction, aes(x = Year, y = estimate))
gp <- gp + geom_point(data = dataset(analysis7), aes(y = Proportion))
gp <- gp + geom_line()
gp <- gp + geom_line(aes(y = lower), linetype = "dashed")
gp <- gp + geom_line(aes(y = upper), linetype = "dashed")
gp <- gp + scale_x_continuous(name = "Year")
gp <- gp + scale_y_continuous(name = "Pairs")
gp <- gp + expand_limits(y = c(0,1))

print(gp)

message("Add a logistic-link function, i.e., logit(eProportion[i]) <- ")

model9 <- jags_model("
model {
  alpha ~ dnorm(0, 2^-2)
  beta ~ dnorm(0, 2^-2)
  beta2 ~ dnorm(0, 2^-2)
  sigma ~ dunif(0, 2)
  for(i in 1:length(Proportion)) {
    logit(eProportion[i]) <- alpha + beta * Year[i] + beta2 * Year[i]^2
    Proportion[i] ~ dnorm(eProportion[i], sigma^-2)
  }
}",
                     derived_code = "data {
  for(i in 1:length(Proportion)) {
    logit(prediction[i]) <- alpha + beta * Year[i] + beta2 * Year[i]^2
  }
}",
                     select_data = c("Proportion", "Year+"))

analysis9 <- jags_analysis(model9, data = peregrine)

prediction <- predict(analysis9)

gp <- ggplot(data = prediction, aes(x = Year, y = estimate))
gp <- gp + geom_point(data = dataset(analysis9), aes(y = Proportion))
gp <- gp + geom_line()
gp <- gp + geom_line(aes(y = lower), linetype = "dashed")
gp <- gp + geom_line(aes(y = upper), linetype = "dashed")
gp <- gp + scale_x_continuous(name = "Year")
gp <- gp + scale_y_continuous(name = "Pairs")
gp <- gp + expand_limits(y = c(0,1))

print(gp)

message("Replace the normal distribution `Proportion[i] ~ dnorm(eProportion[i], sigma^-2)` 
         with the binomial distribution `R.Pairs ~ dbin(eProportion[i], Pairs[i])`")

model10 <- jags_model("
model {
  alpha ~ dnorm(0, 2^-2)
  beta ~ dnorm(0, 2^-2)
  beta2 ~ dnorm(0, 2^-2)
  sigma ~ dunif(0, 2)
  for(i in 1:length(R.Pairs)) {
    logit(eProportion[i]) <- alpha + beta * Year[i] + beta2 * Year[i]^2
    R.Pairs[i] ~ dbin(eProportion[i], Pairs[i])
  }
}",
derived_code = "data {
  for(i in 1:length(R.Pairs)) {
    logit(prediction[i]) <- alpha + beta * Year[i] + beta2 * Year[i]^2
  }
}",
select_data = c("R.Pairs", "Pairs", "Year+"))

analysis10 <- jags_analysis(model10, data = peregrine)

prediction <- predict(analysis10)

gp <- ggplot(data = prediction, aes(x = Year, y = estimate))
gp <- gp + geom_point(data = dataset(analysis9), aes(y = Proportion))
gp <- gp + geom_line()
gp <- gp + geom_line(aes(y = lower), linetype = "dashed")
gp <- gp + geom_line(aes(y = upper), linetype = "dashed")
gp <- gp + scale_x_continuous(name = "Year")
gp <- gp + scale_y_continuous(name = "Pairs")
gp <- gp + expand_limits(y = c(0,1))

print(gp)

message("Replace the polynomial with a first-order difference auto-regressive term")

model11 <- jags_model("
model {
  theta[1] ~ dnorm(0, 2^-2)
  sigma ~ dunif(0, 2)
  for(i in 2:length(R.Pairs)) {
    theta[i] ~ dnorm(theta[i-1], sigma^-2)
  }
  for(i in 1:length(R.Pairs)) {
    logit(eProportion[i]) <- theta[i]
    R.Pairs[i] ~ dbin(eProportion[i], Pairs[i])
  }
}",
                      derived_code = "data {
  for(i in 1:length(R.Pairs)) {
    logit(prediction[i]) <- theta[Year[i]]
  }
}",
random_effect = list(theta = "Year"),
select_data = c("R.Pairs", "Pairs", "Year"))

peregrine$Year <- factor(peregrine$Year)

analysis11 <- jags_analysis(model11, data = peregrine)

prediction <- predict(analysis11)

gp <- ggplot(data = prediction, aes(x = as.integer(as.character(Year)), y = estimate))
gp <- gp + geom_point(data = dataset(analysis9), aes(y = Proportion))
gp <- gp + geom_line()
gp <- gp + geom_line(aes(y = lower), linetype = "dashed")
gp <- gp + geom_line(aes(y = upper), linetype = "dashed")
gp <- gp + scale_x_continuous(name = "Year")
gp <- gp + scale_y_continuous(name = "Proportion Successful Pairs")
gp <- gp + expand_limits(y = c(0,1))

print(gp)

message("Add extra-binomial variation through a random effect on `eProportion`")

model12 <- jags_model("
model {
  theta[1] ~ dnorm(0, 2^-2)
  sigma ~ dunif(0, 2)
  for(i in 2:length(R.Pairs)) {
    theta[i] ~ dnorm(theta[i-1], sigma^-2)
  }

  sDispersion ~ dunif(0, 2)
  for(i in 1:length(R.Pairs)) {
    eDispersion[i] ~ dnorm(0, sDispersion^-2)
    logit(eProportion[i]) <- theta[i] + eDispersion[i]
    R.Pairs[i] ~ dbin(eProportion[i], Pairs[i])
  }
}",
derived_code = "data {
  for(i in 1:length(R.Pairs)) {
    logit(prediction[i]) <- theta[Year[i]]
  }
}",
random_effect = list(theta = "Year"),
select_data = c("R.Pairs", "Pairs", "Year"))

analysis12 <- jags_analysis(model12, data = peregrine)

coef(analysis12)

prediction <- predict(analysis12)

gp <- ggplot(data = prediction, aes(x = as.integer(as.character(Year)), y = estimate))
gp <- gp + geom_point(data = dataset(analysis9), aes(y = Proportion))
gp <- gp + geom_line()
gp <- gp + geom_line(aes(y = lower), linetype = "dashed")
gp <- gp + geom_line(aes(y = upper), linetype = "dashed")
gp <- gp + scale_x_continuous(name = "Year")
gp <- gp + scale_y_continuous(name = "Proportion Successful Pairs")
gp <- gp + expand_limits(y = c(0,1))

print(gp)


