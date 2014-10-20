
```{r, echo = FALSE, results = 'asis'}
exercise("What is the point estimate for theta?")
exercise("And the 95% credible interval?")
exercise("How do the 95% credible interval?")
```

The following code calculates the probability that the coin can be expected to produce heads between 0.5 and 0.7 of the time.

```{r}
predict(acoin1, newdata = "", derived_code = "data { prediction <- theta >= 0.5 && theta <= 0.7 } ", estimate = "mean")$estimate
```

```{r, echo = FALSE, results = 'asis'}
exercise("What is the probability that the coin can be expected to produce a tail more than 50% of the time?")
```

The following BUGS model code includes a derived parameter `range` which 
tests the probability that theta lies between 0.5 and 0.7.
```{r, tidy=FALSE}
txt <- "model { # start of BUGS model code
theta ~ dunif(0, 1) # uniform prior distribution between 1 and 0 for theta
for(i in 1:length(y)) { # for loop over the values 1, 2, ..., length of y
y[i] ~ dbin(theta, 1) # assumed statistical distribution for observed throws
} # end of for loop
range <- theta >= 0.5 && theta <= 0.7
} # end of BUGS model code" 
writeLines(txt, "coin.jags")
```

```{r, echo = FALSE, results = 'asis'}
exercise("What is the probability that the coin can be expected to produce a tail between 50 and 70% of the time?")
exercise("How would you answer the above question in the frequentist framework?")
exercise("What is the probability that the coin can be expected to produce a tail more than 50% of the time?")
```


## Fish Stranding Events

Imagine a dam operator comes to you with the following data regarding 
fish stranding events. Historically fish stranding occurred 15% of the time.
```{r}
y <- c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 
       0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 
       0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0)
```

```{r, echo = FALSE, results = 'asis'}
exercise("What is the probability that dam operations have increased the frequency of fish stranding?")
exercise("How does the inter-run stability of your answer depend on the number of iterations (`n.iter` in `coda.samples` specifies the number of iterations)?")
``


Many researchers estimate fitted values, predictions and residuals by adding extra monitored nodes to their model code. The disadvantages of
this approach are that: the model code becomes more complicated; 
the MCMC sampling takes longer and the table of parameter estimates
becomes unwiedly. Plus, if the variables have been transformed 
back-transformation of the values is also required.

To circumvent these problems I wrote the R package `jaggernaut`
which acts as a wrapper on `rjags`. The following code implements
the previous model in jaggernaut and allows the subsequent generation 
of predictions, fitted values and residuals.

```{r, message = FALSE, warning=FALSE}
mtrees3 <- jags_model("model {
                      alpha ~ dnorm(0, 50^-2) # normal prior on intercept
                      beta ~ dnorm(0, 50^-2) # normal prior on slope
                      sigma ~ dunif(0, 50) # positive uniform prior residual variation
                      
                      for(i in 1:length(Volume)) { 
                      eVolume[i] <- alpha + beta * Girth[i] # expected volume
                      Volume[i] ~ dnorm(eVolume[i], sigma^-2) # observed volume drawn from normal
                      } 
                      }",
derived_code = "data {
for(i in 1:length(Volume)) { 
prediction[i] <- alpha + beta * Girth[i]
}
residual <- (Volume - prediction) / sigma
}
",
monitor = c("alpha", "beta", "sigma"),
select = c("Volume", "Girth+"))
```

As their names imply the `monitor` and `select` arguments
specify the parameters to monitor and the variables to select
from the input dataset. The `+` after `Girth` in the `select`
argument indicates that Girth is to be back-transformed by 
subtracting its mean

```{r, message = FALSE, warning=FALSE}
atrees3 <- jags_analysis(mtrees3, data = datasets::trees)
summary(atrees3)

prediction <- predict(atrees3, newdata = "Girth")
residuals <- residuals(atrees3)
fitted <- fitted(atrees3)$estimate
```

```{r, fig.width=3, fig.height=3}
gp <- ggplot(data = prediction, aes(x = Girth, y = estimate))
gp <- gp + geom_point(data = datasets::trees, aes(y = Volume))
gp <- gp + geom_line()
gp <- gp + geom_line(aes(y = lower), linetype = "dashed")
gp <- gp + geom_line(aes(y = upper), linetype = "dashed")
gp <- gp + scale_y_continuous(name = "Volume")

print(gp)

qplot(fitted$estimate, residuals$estimate) + geom_hline(yintercept = 0) + geom_smooth(se = FALSE)
```

As discussed in the R course [notes](http://www.poissonconsulting.ca/course/2014/09/12/an-introduction-to-r-course.html) the relationship between Volume and Girth is expected to be [allometric](http://www.nature.com/scitable/knowledge/library/allometry-the-study-of-biological-scaling-13228439) because the cross-sectional area at an given point scales to the square of the girth (circumference).

```{r, echo = FALSE, results = 'asis'}
exercise("Fit an allometric relationship by log What is the probability that dam operations have increased the frequency of fish stranding?")
```

Exercise 36 Try fitting an allometric relationship by log transforming Volume and Girth and then using the same basic linear regression model. Does the diagnostic plot suggest an improvement in the model?
