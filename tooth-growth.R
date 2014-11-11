source("header.R")

load.module("glm")

model1 <- jags_model("
model {
  for(i in 1:nsupp) {
    alpha[i] ~ dnorm(0, 40^-2)
  }
  beta ~ dnorm(0, 20^-2)
  sigma ~ dunif(0, 20)
  
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

prediction <- predict(analysis1, newdata = c("supp", "dose"))

gp <- ggplot(data = prediction, aes(x = dose, y = estimate, color = supp, shape = supp))
gp <- gp + geom_point(data = dataset(analysis1), aes(y = len))
gp <- gp + geom_line()
gp <- gp + scale_y_continuous(name = "len")

print(gp)

coef(analysis1)

message("Is the effect of `OJ` significantly different from that of `VC`?")

predictive_check(analysis1, newdata = "", derived_code = 
                   "data{ discrepancy <- alpha[1] - alpha[2] }")

residuals <- residuals(analysis1)
residuals$fitted <- fitted(analysis1)$estimate

message("What does the residual plot suggest about the model fit?")

qplot(fitted, estimate, color = supp, shape = supp, data = residuals, xlab = "Fitted", ylab = "Residual") + geom_hline(yintercept = 0)

message("Fit 1) the ANCOVA, 2) the linear regression, 3) the ANOVA and 4) the ANCOVA with an interaction between dose and supp models and plot their predictions. Which model do you prefer?")

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

prediction <- predict(analysis1, newdata = c("supp", "dose"), base = data.frame(supp = "VC", dose = 0.5))

gp <- ggplot(data = prediction, aes(x = dose, y = estimate, color = supp, shape = supp))
gp <- gp + geom_line()
gp <- gp + geom_line(aes(y = lower), linetype = "dashed")
gp <- gp + geom_line(aes(y = upper), linetype = "dashed")
gp <- gp + scale_y_continuous(name = "Effect on len (%)", labels = percent)

print(gp)

message("Plot the percent change in `len` for your preferred
         ToothGrowth model relative to 1 mg of OJ")
