library(ggplot2)
library(pscl)
library(lmtest)
library(MASS)
library(statmod)


# function for Likelihood ratio test
lrt <- function (obj1, obj2) {
  L0 <- logLik(obj1)
  L1 <- logLik(obj2)
  L01 <- as.vector(- 2 * (L0 - L1))
  df <- attr(L1, "df") - attr(L0, "df")
  list(L01 = L01, df = df,
       "p-value" = pchisq(L01, df, lower.tail = FALSE))
}

# importing data
dat <- read.csv("toy_characteristics_transformed (1).csv")
dat <- read.csv("LabRDBMToySizeWeight.csv")
View(dat)
dat$Age <- factor(dat$Age)
dat$toy_size <- factor(dat$toy_size)
dat$toy_weight <- factor(dat$toy_weight)
dat$toy_type <- factor(dat$toy_type)
class(dat$Age)
class(dat$toy_size)
class(dat$toy_type)

# Data visualizations
e <- ggplot(dat, aes(x = toy_type, y = value))
e + geom_boxplot()
e2 <- e + 
  geom_boxplot(aes(fill = Age), position = position_dodge(0.9) )
e2
#e3 <- e2 + 
 # geom_boxplot(aes(fill = Age), position = position_dodge(0.9) )
#e3
# Poisson model
pois_mod <- glm(value ~ Age + toy_weight + toy_size, data = dat, family = poisson(link = "log"))
summary(pois_mod)
qr <- qresiduals(pois_mod)
qqnorm(qr)
abline(0,1)
plot(fitted(pois_mod), qr)
# dispersion of the Poisson model
E2 <- resid(pois_mod, type = "pearson")
N  <- nrow(dat)
p  <- length(coef(pois_mod))  
sum(E2^2) / (N - p)


# Negative biomial model 
#(if the dispersion of the Poisson > 1.1)
nb_mod <- glm.nb(value ~ Age + toy_weight + toy_size , data = dat)
summary(nb_mod)
qr <- qresiduals(nb_mod)
qqnorm(qr)
abline(0,1)
plot(fitted(nb_mod), qr)

E2 <- resid(nb_mod, type = "pearson")
N  <- nrow(dat)
p  <- length(coef(nb_mod))  
sum(E2^2) / (N - p)


# ZIP model with Poisson 
m1 <- zeroinfl(value ~  toy_weight +toy_size | toy_size, data = dat)
summary(m1)


## Exponentiated coefficients of the ZIP model
expCoef <- exp(coef((m1)))
expCoef <- matrix(expCoef, ncol = 2)
colnames(expCoef) <- c("Count_model","Zero_inflation_model")
expCoef

# dispersion calculation
E2 <- resid(m1, type = "pearson")
N  <- nrow(dat)
p  <- length(coef(m1))  
sum(E2^2) / (N - p)



# Likelihood ratio tests
lrt(nb_mod, m1) # negative binomial vs ZIP
lrt(pois_mod, m1) # Poisson vs ZIP
lrtest(pois_mod, nb_mod) # Negateive binomial vs poisson


# (SKIP THIS) ZIP model with negative binomial
m2 <- zeroinfl(value ~ Age + toy_size + toy_weight    | Age + toy_size + toy_weight, dist='negbin', data = dat)
summary(m2)
# Dispersion Statistic
E2 <- resid(m2, type = "pearson")
N  <- nrow(dat)
p  <- length(coef(m2)) + 1 # '+1' is due to theta
sum(E2^2) / (N - p)


# plotting expected count for different combinations of predictors
# replace m1 with other model for getting the plot for Poisson/Negative binomial
ggplot(dat, aes(x = toy_weight, y = predict(m1, dat), colour = factor(toy_size)))+
  geom_point() +
  geom_line() +
  facet_wrap(~Age) +
  labs(x = "toy_weight", y = "Predicted count")
