model1 <- jags_model("model { 
  theta ~ dunif(0, 1)
  y ~ dbin(theta, n)
}") 
data <- data.frame(n = 100, y = 30)
analysis1 <- jags_analysis(model1, data = data)
plot(analysis1)
coef(analysis1)
