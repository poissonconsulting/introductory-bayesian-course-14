source("header.R")

message("Previous studies indicate that the coin was definitely biased
         towards tails. Modify the prior distribution accordingly
         and rerun the above model. How does the posterior distribution change?")


model1 <- jags_model("model { 
  theta ~ dunif(0.5, 1)
  y ~ dbin(theta, n)
}") 


data <- data.frame(n = 10, y = 3)
analysis1 <- jags_analysis(model1, data = data)
plot(analysis1)
coef(analysis1)
