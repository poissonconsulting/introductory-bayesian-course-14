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

message("Plot the percent change in `len` for all four models relative to 0.5 mg of Vitamin C")

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

model_code <- "model {
  alpha ~ dnorm(0, 100^-2)
  beta ~ dnorm(0, 100^-2)
  beta2 ~ dnorm(0, 100^-2)
  for(i in 1:length(Pairs)) {
    log(ePairs[i]) <- alpha + beta * Year[i]  + beta2 * Year[i]^2
    Pairs[i] ~ dpois(ePairs[i])
  }
}"



