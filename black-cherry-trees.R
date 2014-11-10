source("heder.R")

message("What do you notice about the trace plots? The output of 
         `auto_corr(analysis1)` and `cross_cor(analysis1)` 
         might give you some clues.")


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

plot(analysis1)
coef(analysis1)

auto_corr(analysis1)
cross_corr(analysis1)

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

analysis2 <- jags_analysis(model2, data = trees)

plot(analysis2)

message("If you ever want to examine the actual data being passed
to JAGS set the modify_data term of your `jags_model` 
object to be a simple function that prints and returns its one argument")

modify_data(model2) <- function (data) { print(data); data }

analysis2 <- jags_analysis(model2, data = trees)

message("What is the effect of loading the `glm` module (without manually centring `Girth`) on the trace plots?")

library(rjags)
list.modules()
load.module("glm")

analysis3 <- jags_analysis(model1, data = trees)

plot(analysis3)

unload.module("glm")
