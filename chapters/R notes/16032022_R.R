#TODO : CONVERT TO NOTEBOOK

set.seed(123)
x <- rnorm(100, 0, 2)
x #values of the predictor

beta0 <- 1
beta1 <- 2

eta <- beta0 + beta1*x

mu <- exp(eta)
y <- rpois(100, mu) 
y

y[1:10]

#we know which model was used, is it possible to ...

plot(x,y) #large scale of Y values

plot(x, log(y)) #clear structure

z = log(y) #is not a good idea, for those moments when y = 0
summary(z)

z <- log(y+1) #SUDO count, it is added one count
z
summary(z)


modA <- lm(formula = z~x)
summary(modA) #a linear modle with a series of parameters. The p-values are on the right.
# The estimates of the beta0 and beta1 are different from those that we chose.

#residuals

plot(modA) #not good pattern, which seems to be with a sense. Quadratic term in x
# Residuals seem to be normally distributed.

modB <- lm(formula = z~x + I(x^2))
summary(modB)
plot(modB)
hist(y)

# negative binomials are usually used for biological data
modC <- glm(formula=y~x, family = "poisson")
summary(modC) # values are closer to the true ones
# Number of fisher Scoring iterations, start from a value of beta and changes it a number of times
plot(modC)

p1 <- predict(modC) #predict the linear predictor
summary(p1)

#it does beta0hat + beta1hat*x
eta.fitted <- coefficients(modC)[1] + coefficients(modC)[2]*x
eta.fitted[1:10]

plot(p1, log(y))
abline(0,1)
# on Y axis,

p2 <- predict(modC, type = "response") #
p2[1:10]

exp(eta.fitted)[1:10]


mu <- exp(eta)/(1+exp(eta))
summary(mu)

plot(eta, mu)
plot(x, mu) #with x small probability is small


y <- rbinom(100, size = 1, prob = mu)
table(y)
modD <- glm(y~x, family = "binomial")
modD <- glm(y~x, family = binomial(link = "logit")) #binomial can be used as a function

res <- binomial(link = "logit")
str(res)
plot(res$linkfun, from=0.001, to=0.999) #visualize components of the link function
plot(res$variance, from=0.001, to=0.999)

p4 <- predict(modD)
summary(p4) #gives eta values

p5 <- predict(modD, type="response")
summary(p5)
