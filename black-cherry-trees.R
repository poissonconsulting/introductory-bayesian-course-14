source("header.R")

model1 <- jags_model("
model {
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

quartz()
plot(analysis1)
coef(analysis1)

message("What do you notice about the trace plots? The output of 
`auto_corr(analysis1)` and `cross_cor(analysis1)` 
might give you some clues.")

auto_corr(analysis1)
cross_corr(analysis1)

message("What is the R-hat value for each of the parameters in the current model? 
Use the `convergence` function with `combine = FALSE`.")

convergence(analysis1)
convergence(analysis1, combine = FALSE)

analysis1 <- jags_analysis(model1, data = trees, niters = 10^4)
convergence(analysis1, combine = FALSE)

message("What is the effect of centering `Girth` on the trace plots?")

model2 <- jags_model("
model {
  alpha ~ dnorm(0, 50^-2) 
  beta ~ dnorm(0, 10^-2)
  sigma ~ dunif(0, 10)
  
  for(i in 1:length(Volume)) { 
    eMu[i] <- alpha + beta * Girth[i]
    Volume[i] ~ dnorm(eMu[i], sigma^-2)
  } 
}",
select_data = c("Volume", "Girth+"))
derived_code <- "data {
  for(i in 1:length(Volume)) { 
    prediction[i] <- alpha + beta * Girth[i]
  }
}"

analysis2 <- jags_analysis(model2, data = trees)

quartz()
plot(analysis2)

message("If you ever want to examine the actual data being passed
to JAGS set the modify_data term of your `jags_model` 
object to be a simple function that prints and returns its one argument")

modify_data(model2) <- function (data) { print(data); data }

analysis2 <- jags_analysis(model2, data = trees)

message("The `newdata` argument in the `predict` function can also take a `data.frame`.
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

simulated <- predict(analysis1, parm = "simulated", newdata = "Girth", 
                     derived_code = derived_code)

simulated <- predict(analysis1, parm = "simulated", newdata = data.frame(Girth = 8), 
                     derived_code = derived_code)

fitted <- fitted(analysis1, derived_code = derived_code)
fitted$residual <- residuals(analysis1, derived_code = derived_code)$estimate

qplot(estimate, residual, data = fitted) + geom_hline(yintercept = 0) + 
    geom_smooth(se = FALSE)

simulated

message("What does the current residual plot suggest to you about model adequacy?")

fitted <- fitted(analysis1, derived_code = derived_code)
fitted$residual <- residuals(analysis1, derived_code = derived_code)$estimate

qplot(estimate, residual, data = fitted) + geom_hline(yintercept = 0) + geom_smooth(se = FALSE)

message("What does the posterior predictive check suggest to you about model adequacy?")

predictive_check(analysis1, derived_code = derived_code)

message("Fit the linear regression of the allometric model to the `trees` data set.
Is the model fit improved?")

model3 <- jags_model("
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
    
    simulated[i] ~ dnorm(log(prediction[i]), sigma^-2)
    
    D_observed[i] <- log(dnorm(Volume[i], prediction[i], sigma^-2))
    D_simulated[i] <- log(dnorm(simulated[i], prediction[i], sigma^-2))
  }
  residual <- (Volume - prediction) / sigma
  discrepancy <- sum(D_observed) - sum(D_simulated)
}",
select_data = c("log(Volume)", "log(Girth)+"))

analysis3 <- jags_analysis(model3, data = trees)

plot(analysis3)

prediction <- predict(analysis3, newdata = "Girth")
simulated <- predict(analysis3, parm = "simulated", newdata = "Girth")

gp <- ggplot(data = prediction, aes(x = Girth, y = estimate))
gp <- gp + geom_point(data = dataset(analysis1), aes(y = Volume))
gp <- gp + geom_line()
gp <- gp + geom_line(aes(y = lower), linetype = "dashed")
gp <- gp + geom_line(aes(y = upper), linetype = "dashed")
gp <- gp + geom_line(data = simulated, aes(y = lower), linetype = "dotted")
gp <- gp + geom_line(data = simulated, aes(y = upper), linetype = "dotted")
gp <- gp + scale_y_continuous(name = "Volume")

print(gp)

message("Is there any support for adding `log(Height)` to the model?")

model4 <- jags_model("
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
                     derived_code = "data {
                       for(i in 1:length(Volume)) { 
                         log(prediction[i]) <- alpha + beta * Girth[i] + betaHeight * Height[i]
                         
                         simulated[i] ~ dlnorm(log(prediction[i]), sigma^-2)
                         simulated2[i] ~ dnorm(log(prediction[i]), sigma^-2)
                         
                         D_observed[i] <- log(dnorm(Volume[i], prediction[i], sigma^-2))
                         D_simulated[i] <- log(dnorm(simulated2[i], prediction[i], sigma^-2))
                       }
                       residual <- (Volume - prediction) / sigma
                       discrepancy <- sum(D_observed) - sum(D_simulated)
                     }",
select_data = c("log(Volume)", "log(Girth)+", "log(Height)+"))

analysis4 <- jags_analysis(model4, data = trees)

plot(analysis4)
coef(analysis4)

prediction <- predict(analysis4, newdata = c("Girth","Height"), length_out = 10)
#simulated <- predict(analysis3, parm = "simulated", newdata = "Girth")

gp <- ggplot(data = , aes(x = Girth, y = estimate))
gp <- gp + facet_wrap(~Height)
gp <- gp + geom_point(data = dataset(analysis1), aes(y = Volume))
gp <- gp + geom_line()
gp <- gp + geom_line(aes(y = lower), linetype = "dashed")
gp <- gp + geom_line(aes(y = upper), linetype = "dashed")
#gp <- gp + geom_line(data = simulated, aes(y = lower), linetype = "dotted")
#gp <- gp + geom_line(data = simulated, aes(y = upper), linetype = "dotted")
gp <- gp + scale_y_continuous(name = "Volume")

quartz()
print(gp)
