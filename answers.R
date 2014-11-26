rm(list = ls())
graphics.off()

library(dplyr)
library(ggplot2)
library(scales)
library(jaggernaut)

exercise_function <- function () {
  i <- 0
  function (x) {
    i <<- i + 1
    message(paste0("Exercise ", i, ": ", x))
  }
}

exercise <- exercise_function()

if (getDoParWorkers() == 1) {
  registerDoParallel(4)
  opts_jagr(parallel = TRUE)
}

exercise("Previous studies indicate that the coin was definitely biased
         towards tails. Modify the prior distribution accordingly
         and rerun the above model. How does the posterior distribution change?")


model1 <- jags_model("model { 
  theta ~ dunif(0, 0.5)
  y ~ dbin(theta, n)
}") 

data <- data.frame(n = 10, y = 3)
analysis1 <- jags_analysis(model1, data = data)

plot(analysis1)
coef(analysis1)

exercise("What do you notice about the trace plots? The output of 
         `auto_corr(analysis1)` and `cross_cor(analysis1)` 
         might give you some clues.")

model1 <- jags_model("model {
  alpha ~ dnorm(0, 50^-2) 
  beta ~ dnorm(0, 10^-2)
  sigma ~ dunif(0, 10)

  for(i in 1:length(Volume)) { 
    eMu[i] <- alpha + beta * Girth[i]
    Volume[i] ~ dnorm(eMu[i], sigma^-2)
  } 
}")

data(trees)
analysis1 <- jags_analysis(model1, data = trees)

auto_corr(analysis1)
cross_corr(analysis1)

exercise("What is the R-hat value for each of the parameters in the current model? 
         Clue: type `?convergence`.")

convergence(analysis1, combine = FALSE)

exercise("What is the R-hat value for each of the parameters in the current model
         with 10,000 iterations?")

analysis1 <- jags_analysis(model1, data = trees, niters = 10^4)
convergence(analysis1, combine = FALSE)

exercise("What is the effect of centering `Girth` on the trace plots with 1,000 iterations?")
select_data(model1) <- c("Volume", "Girth+")
analysis1 <- jags_analysis(model1, data = trees)
convergence(analysis1, combine = FALSE)

exercise("The `newdata` argument in the `predict` function can also take a `data.frame`.
         What is the 95% Prediction Interval for the `Volume` of a tree of `Girth` 8?")

derived_code <- "data {
  for(i in 1:length(Volume)) { 
    prediction[i] <- alpha + beta * Girth[i]

    simulated[i] ~ dnorm(prediction[i], sigma^-2)

    D_observed[i] <- log(dnorm(Volume[i], prediction[i], sigma^-2))
    D_simulated[i] <- log(dnorm(simulated[i], prediction[i], sigma^-2))
  }
  residual <- (Volume - prediction) / sigma
  discrepancy <- sum(D_observed) - sum(D_simulated)
}"

predict(analysis1, parm = "simulated", newdata = data.frame(Girth = 8), 
        derived_code = derived_code)

exercise("What does the current residual plot suggest to you about model adequacy?")
fitted <- fitted(analysis1, derived_code = derived_code)
fitted$residual <- residuals(analysis1, derived_code = derived_code)$estimate
qplot(estimate, residual, data = fitted) + geom_hline(yintercept = 0) + geom_smooth(se = FALSE)

exercise("What does the posterior predictive check suggest to you about model adequacy?")
predictive_check(analysis1, derived_code = derived_code)

exercise("Fit the linear regression of the allometric model to the `trees` data set.
         Is the model fit improved?")

model1 <- jags_model("
model {
  alpha ~ dnorm(0, 50^-2) 
  beta ~ dnorm(0, 10^-2)
  sigma ~ dunif(0, 10)
  
  for(i in 1:length(Volume)) { 
    eMu[i] <- alpha + beta * Girth[i]
    Volume[i] ~ dnorm(eMu[i], sigma^-2)
  } 
}",
                     derived_code = "data {
  for(i in 1:length(Volume)) { 
    log(prediction[i]) <- alpha + beta * Girth[i]
    
    simulated[i] ~ dlnorm(log(prediction[i]), sigma^-2)
  }
  residual <- (Volume - prediction) / sigma
}",
                     select_data = c("log(Volume)", "log(Girth)+"))

analysis1 <- jags_analysis(model1, data = trees)
coef(analysis1)

prediction <- predict(analysis1, newdata = "Girth")
simulated <- predict(analysis1, parm = "simulated", newdata = "Girth")

gp <- ggplot(data = prediction, aes(x = Girth, y = estimate))
gp <- gp + geom_point(data = dataset(analysis1), aes(y = Volume))
gp <- gp + geom_line()
gp <- gp + geom_line(aes(y = lower), linetype = "dashed")
gp <- gp + geom_line(aes(y = upper), linetype = "dashed")
gp <- gp + geom_line(data = simulated, aes(y = lower), linetype = "dotted")
gp <- gp + geom_line(data = simulated, aes(y = upper), linetype = "dotted")
gp <- gp + scale_y_continuous(name = "Volume")

print(gp)

exercise("Is there any support for adding `log(Height)+` to the model?")

model1 <- jags_model("
model {
  alpha ~ dnorm(0, 50^-2) 
  beta ~ dnorm(0, 10^-2)
  betaHeight ~ dnorm(0, 10^-2)
  sigma ~ dunif(0, 10)
  
  for(i in 1:length(Volume)) { 
    eMu[i] <- alpha + beta * Girth[i] + betaHeight * Height[i]
    Volume[i] ~ dnorm(eMu[i], sigma^-2)
  } 
}",
                     select_data = c("log(Volume)", "log(Girth)+", "log(Height)+"))

analysis1 <- jags_analysis(model1, data = trees)
coef(analysis1)

exercise("What is the significance value for the effect of `OJ` versus `VC`?")

model1 <- jags_model("
model {
  for(i in 1:nsupp) {
    alpha[i] ~ dnorm(0, 40^-2)
  }
  beta ~ dnorm(0, 100^-2)
  sigma ~ dunif(0, 10)
  
  for(i in 1:length(len)) { 
    eLen[i] <- alpha[supp[i]] + beta * dose[i]
    len[i] ~ dnorm(eLen[i], sigma^-2)
  } 
}",
                     derived_code  = "
data{
  for(i in 1:length(len)) { 
    prediction[i] <- alpha[supp[i]] + beta * dose[i]
  }
  residual <- (len - prediction) / sigma
}")

data(ToothGrowth)
analysis1 <- jags_analysis(model1, data = ToothGrowth)
coef(analysis1)
predictive_check(analysis1, newdata = "", derived_code = 
                   "data{ discrepancy <- alpha[1] - alpha[2] }")

exercise("What does the residual plot suggest about the model fit?")

residuals <- residuals(analysis1)
residuals$fitted <- fitted(analysis1)$estimate
qplot(fitted, estimate, color = supp, shape = supp, data = residuals, xlab = "Fitted", ylab = "Residual") + geom_hline(yintercept = 0)

exercise("Fit 1) the ANCOVA, 2) the linear regression, 3) the ANOVA and 4) the ANCOVA with an interaction between dose and supp models and plot their predictions. Which model do you prefer?")

model_id(model1) <- "ANCOVA"

model2 <- jags_model("model {
  alpha ~ dnorm(0, 40^-2)
  beta ~ dnorm(0, 20^-2)
  sigma ~ dunif(0, 20)
  
  for(i in 1:length(len)) { 
    eLen[i] <- alpha + beta * dose[i]
    len[i] ~ dnorm(eLen[i], sigma^-2)
  } 
}",
                     derived_code  = " data{
  for(i in 1:length(len)) { 
    prediction[i] <- alpha + beta * dose[i]
  }
  residual <- (len - prediction) / sigma
}",
                     select_data = c("len", "dose"),
                     model_id = "regression")

model3 <- jags_model("
model {
  for(i in 1:nsupp) {
    alpha[i] ~ dnorm(0, 40^-2)
  }
  sigma ~ dunif(0, 20)
  
  for(i in 1:length(len)) { 
    eLen[i] <- alpha[supp[i]] 
    len[i] ~ dnorm(eLen[i], sigma^-2)
  } 
}",
                     derived_code  = "
data{
  for(i in 1:length(len)) { 
    prediction[i] <- alpha[supp[i]]
  }
  residual <- (len - prediction) / sigma
}",
                     select_data = c("len", "supp"),
                     model_id = "ANOVA")

model4 <- jags_model("
model {
  for(i in 1:nsupp) {
    alpha[i] ~ dnorm(0, 40^-2)
    beta[i] ~ dnorm(0, 20^-2)
  }
  sigma ~ dunif(0, 20)
  
  for(i in 1:length(len)) { 
    eLen[i] <- alpha[supp[i]] + beta[supp[i]] * dose[i]
    len[i] ~ dnorm(eLen[i], sigma^-2)
  } 
}",
                     derived_code  = "
data{
  for(i in 1:length(len)) { 
    prediction[i] <- alpha[supp[i]] + beta[supp[i]] * dose[i]
  }
  residual <- (len - prediction) / sigma
}",
                     model_id = "ANCOVA+")

models <- combine(model1, model2, model3, model4)
analyses <- jags_analysis(models, data = ToothGrowth)

coef(analyses)

prediction <- data.frame()
model_ids <- model_id(analyses, reference = TRUE)
for(model_id in model_ids) {
  pred <- predict(analyses, newdata = c("supp", "dose"), model_id = model_id)  
  pred$ModelID <- factor(model_id, levels = model_ids)
  prediction <- rbind(prediction, pred)
}

gp <- ggplot(data = prediction, aes(x = dose, y = estimate, color = supp, shape = supp))
gp <- gp + facet_wrap(~ModelID)
gp <- gp + geom_point(data = dataset(analyses), aes(y = len))
gp <- gp + geom_line()
gp <- gp + scale_y_continuous(name = "len")

print(gp)

exercise("How might you further improve your preferred model?")
print("Explicit non-linear growth model")

exercise("Plot the percent change in `len` for all four models relative to 0.5 mg of Vitamin C")

prediction <- data.frame()
model_ids <- model_id(analyses, reference = TRUE)
for(model_id in model_ids) {
  pred <- predict(analyses, newdata = c("supp", "dose"), 
                  base = data.frame(supp = "VC", dose = 0.5), model_id = model_id)  
  pred$ModelID <- factor(model_id, levels = model_ids)
  prediction <- rbind(prediction, pred)
}

gp <- ggplot(data = prediction, aes(x = dose, y = estimate, color = supp, shape = supp))
gp <- gp + facet_wrap(~ModelID)
gp <- gp + geom_line()
gp <- gp + geom_line(aes(y = lower), linetype = "dashed")
gp <- gp + geom_line(aes(y = upper), linetype = "dashed")
gp <- gp + scale_y_continuous(name = "Effect on len (%)", labels = percent)

print(gp)

exercise("What happens to the model's predictions if `Year` isn't centred?")

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
                     select_data = c("Pairs", "Year"))

data(peregrine)
analysis1 <- jags_analysis(model1, data = peregrine)
coef(analysis1)

prediction <- predict(analysis1)

gp <- ggplot(data = prediction, aes(x = Year, y = estimate))
gp <- gp + geom_point(data = dataset(analysis1), aes(y = Pairs))
gp <- gp + geom_line()
gp <- gp + geom_line(aes(y = lower), linetype = "dashed")
gp <- gp + geom_line(aes(y = upper), linetype = "dashed")
gp <- gp + scale_x_continuous(name = "Year")
gp <- gp + scale_y_continuous(name = "Pairs")
gp <- gp + expand_limits(y = 0)

print(gp)

exercise("Replace `ePairs[i] <- ...` with `log(ePairs[i]) <- ...`. How does the log-link function alter the model?")

model1 <- jags_model("model {
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

analysis1 <- jags_analysis(model1, data = peregrine)
coef(analysis1)

prediction <- predict(analysis1)

gp <- gp %+% prediction
print(gp)

exercise("Next, replace `Pairs[i] ~ dnorm(ePairs[i], sigma^-2)` with `Pairs[i] ~ dpois(ePairs[i])`.
         How does the assumption of Poisson distributed counts alter the model?")

model1 <- jags_model("model {
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

analysis1 <- jags_analysis(model1, data = peregrine)
coef(analysis1)

prediction <- predict(analysis1)

gp <- gp %+% prediction
print(gp)

exercise("How does the second-order polynomial change the model's predictions?")

model1 <- jags_model("model {
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

analysis1 <- jags_analysis(model1, data = peregrine)
coef(analysis1)

prediction <- predict(analysis1)

gp <- gp %+% prediction
print(gp)

exercise("Fit a third-order (cubic) polynomial regression (`... + Year[i]^2 + beta3 * Year[i]^3`). How does it alter the model?")

model1 <- jags_model("model {
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
    log(prediction[i]) <- alpha + beta * Year[i] + beta2 * Year[i]^2 + beta3 * Year[i]^3
  }
}",
                     select_data = c("Pairs", "Year+"))

analysis1 <- jags_analysis(model1, data = peregrine)
coef(analysis1)

prediction <- predict(analysis1)

gp <- gp %+% prediction
print(gp)

exercise("Use the third-order polynomial regression to predict the number of breeding pairs in 2006.
         How confident are you in your answer?")

predict(analysis1, newdata = data.frame(Year = as.integer(2006)))

exercise("Plot the state-space population growth rate model predictions in terms 
         of the percent change since 1970. What is the estimated percent
         change in the population in 2003 compared to 1970?")

model1 <- jags_model("model {
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
                     select_data = c("Pairs", "Year"),
                     random_effects = list(r = "Year", logN = "Year"))

data(peregrine)
peregrine$Year <- factor(peregrine$Year)
analysis1 <- jags_analysis(model1, data = peregrine, niters = 10^4)
coef(analysis1)

prediction <- predict(analysis1, base = data.frame(Year = factor("1970")))

gp <- ggplot(data = prediction, aes(x = as.integer(as.character(Year)), y = estimate))
gp <- gp + geom_line()
gp <- gp + geom_line(aes(y = lower), linetype = "dashed")
gp <- gp + geom_line(aes(y = upper), linetype = "dashed")
gp <- gp + scale_x_continuous(name = "Year")
gp <- gp + scale_y_continuous(name = "Pairs")
gp <- gp + expand_limits(y = 0)

print(gp)

predict(analysis1, newdata = data.frame(Year = factor("2003")), base = data.frame(Year = factor("1970")))

exercise("What is the probability that the population in 2008 will be
         less than that in 2003? Note you can produce the projections
         by simply appending five years of missing counts to the dataset.")

peregrine <- left_join(data.frame(Year = factor(1964:2008)), peregrine, by = "Year")

analysis1 <- jags_analysis(model1, data = peregrine, niters = 10^4)

prediction <- predict(analysis1)

gp <- ggplot(data = prediction, aes(x = as.integer(as.character(Year)), y = estimate))
gp <- gp + geom_point(data = dataset(analysis1), aes(y = Pairs))
gp <- gp + geom_line()
gp <- gp + geom_line(aes(y = lower), linetype = "dashed")
gp <- gp + geom_line(aes(y = upper), linetype = "dashed")
gp <- gp + scale_x_continuous(name = "Year")
gp <- gp + scale_y_continuous(name = "Pairs")
gp <- gp + expand_limits(y = 0)

print(gp)

predict(analysis1, newdata = data.frame(Year = factor("2008")), base = data.frame(Year = factor("2003")))

exercise("Is the inclusion of a Plot the state-space population growth rate model predictions in terms 
         of the percent change since 1970. What is the estimated percent
         change in the population in 2003 compared to 1970?")

model1 <- jags_model("model {
  mean_r ~ dnorm(0, 1^-2)
  sd_r ~ dunif(0, 1)
  
  logN[1] ~ dnorm(0, 10^-2)
  for(i in 2:nYear) {
    r[i-1] ~ dnorm(mean_r, sd_r^-2)
    logN[i] <- logN[i-1] + r[i-1]
  }
  sDispersion ~ dunif(0, 5)
  for(i in 1:length(Pairs)) {
    eDispersion[i] ~ dgamma(1 / sDispersion^2, 1 / sDispersion^2)
    
    Pairs[i] ~ dpois(exp(logN[Year[i]]) * eDispersion[i])
  }
  logN1 <- logN[1]
}",
                     derived_code = "data {
for(i in 1:length(Pairs)) {
log(prediction[i]) <- logN[Year[i]]
}
}",
                     select_data = c("Pairs", "Year"),
                     random_effects = list(r = "Year", logN = "Year"))

analysis1 <- jags_analysis(model1, data = peregrine, niters = 10^4)

coef(analysis1)
prediction <- predict(analysis1, base = data.frame(Year = factor(1970)))

gp <- ggplot(data = prediction, aes(x = as.integer(as.character(Year)), y = estimate))
gp <- gp + geom_line()
gp <- gp + geom_line(aes(y = lower), linetype = "dashed")
gp <- gp + geom_line(aes(y = upper), linetype = "dashed")
gp <- gp + scale_x_continuous(name = "Year")
gp <- gp + scale_y_continuous(name = "Percent Change", labels = percent)
gp <- gp + expand_limits(y = 0)

print(gp)

filter(prediction, Year == "2003")

exercise("How does a second-order polynomial regression alter the model's predictions?")

data(peregrine)
peregrine$Proportion <- peregrine$R.Pairs / peregrine$Pairs

model1 <- jags_model("model {
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

analysis1 <- jags_analysis(model1, data = peregrine)

prediction <- predict(analysis1)

gp <- ggplot(data = prediction, aes(x = Year, y = estimate))
gp <- gp + geom_point(data = dataset(analysis1), aes(y = Proportion))
gp <- gp + geom_line()
gp <- gp + geom_line(aes(y = lower), linetype = "dashed")
gp <- gp + geom_line(aes(y = upper), linetype = "dashed")
gp <- gp + scale_x_continuous(name = "Year")
gp <- gp + scale_y_continuous(name = "Pairs")
gp <- gp + expand_limits(y = c(0,1))

print(gp)

exercise("How does adding the logistic-link function, i.e., `logit(eProportion[i]) <- ...` alter the model's
predictions?")

model1 <- jags_model("model {
  alpha ~ dnorm(0, 1^-2)
  beta ~ dnorm(0, 1^-2)
  beta2 ~ dnorm(0, 1^-2)
  sigma ~ dunif(0, 1)
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

analysis1 <- jags_analysis(model1, data = peregrine)

prediction <- predict(analysis1)

gp <- gp %+% prediction

print(gp)

exercise("How does replacing the normal distribution `Proportion[i] ~ dnorm(eProportion[i], sigma^-2)` 
         with the binomial distribution `R.Pairs ~ dbin(eProportion[i], Pairs[i])` alter the model?")

model1 <- jags_model("model {
  alpha ~ dnorm(0, 1^-2)
  beta ~ dnorm(0, 1^-2)
  beta2 ~ dnorm(0, 1^-2)
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

analysis1 <- jags_analysis(model1, data = peregrine)

prediction <- predict(analysis1)

gp <- gp %+% prediction

print(gp)

exercise("What happens to the model if you replace line 5 with `theta[i] <- theta[i-1]`?")
print("Not covered")

exercise("What effect does a normally distributed random effect on
         `logit(eProportion[i])` have on the model? Are the data overdispersed?")

model1 <- jags_model("model {
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

peregrine$Year <- factor(peregrine$Year)

analysis1 <- jags_analysis(model1, data = peregrine)

prediction <- predict(analysis1)

gp <- gp + aes(as.integer(as.character(Year)))
gp <- gp %+% prediction

print(gp)
