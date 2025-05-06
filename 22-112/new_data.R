library(foreign)
library(VGAM)
library(boot)
library(ggplot2)
library(lmtest)
library(stringr)
library(pscl)
library(MASS)
library(emmeans)
library(data.table)
library(lmtest)
library(statmod)

# function to perform likelihood ratio test
lrt <- function (obj1, obj2) {
  L0 <- logLik(obj1)
  L1 <- logLik(obj2)
  L01 <- as.vector(- 2 * (L0 - L1))
  df <- attr(L1, "df") - attr(L0, "df")
  list(L01 = L01, df = df,
       "p-value" = pchisq(L01, df, lower.tail = FALSE))
}

#Importing wide format data
dat <- read.csv("RDBMDataStats.csv")
View(dat)

#converting to long format
df_long <- melt(dat, id.vars = c("ID","Age"), variable.name = "toy_posture")
View(df_long)

# separating the three columns for posture, toy_weight and toy_size
df_long[c('posture', 'toy_size', 'toy_weight')] <- str_split_fixed(df_long$toy_posture, '_', 3)

# creting a column called toy_type
df_long$toy_type <- paste(df_long$toy_size, "-", df_long$toy_weight)
dat <- df_long

# Changing data type
dat$Age <- factor(dat$Age)
dat$toy_size <- factor(dat$toy_size)
dat$toy_weight <- factor(dat$toy_weight)
dat$toy_type <- factor(dat$toy_type)
dat$posture <- factor(dat$posture)
class(dat$Age)
class(dat$toy_size)
class(dat$value)

# Releving the reference class to 'sit' in posture variable
dat$posture <- relevel(dat$posture, ref = "sit")

#Releving the reference class to 'medium-light' in toy_type
dat$toy_type <- relevel(dat$toy_type, ref = "medium - light")

# ZIP model using toy_weight and toy_size
zip_mod <- zeroinfl(value ~  Age + posture  + toy_weight + toy_size | 
                      posture + toy_size + toy_weight , data = dat)
summary(zip_mod)

# Poisson model using toy_weight and toy_size
pois_mod <- glm(value ~  Age + posture + toy_weight + toy_size , data = dat, family = "poisson")
summary(pois_mod)

# negative binomial model using toy_weight and toy_size
nb_mod <- glm.nb(value ~  Age + posture +  toy_weight + toy_size, data = dat)
summary(nb_mod)

# Calculation dispersion 
# change the model name for Poisson, ZIP, Neg bin
mod = pois_mod
E2 <- resid(mod, type = "pearson")
N  <- nrow(dat)
p  <- length(coef(mod))  
sum(E2^2) / (N - p)

## we are predicting log(counts) in all the models
# for eg., estimate of postureStand as -1.2 means that 
# Stand decreases the expected log(counts) by 1.2 times as compared to sit
## taking the reverse of log to convert the estimates for counts
# for eg., now if postureStand = 0.4
# it means Stand decreases the counts by 0.4 times as compared to sit
exp(coef(nb_mod))

# Likelihood ratio test for testing the models
# lower p-value means that the more complicated model is preferred
# higher means that the lower complexity can be used 
# Complexity ranking: zip_mod > nb_mod > pois_mod
#poisson vs neg bin model
lrtest(pois_mod, nb_mod)
# poisson vs neg bin model
lrt(pois_mod, zip_mod)
# neg bin vs zip
lrt(nb_mod, zip_mod)


# post-hoc analysis for pairwise comparison
# estimate column in the output is important
# it is the difference in estimates for the log(counts) because model uses log() scale
# positive means that the first factor in the constrast promotes more counts by that much factor
# change the model and the attribute
em <-  emmeans(nb_mod, specs = pairwise ~ posture)
em$contrasts  

# for combination of variables like toy_weight & toy_size
em1 <-  emmeans(nb_mod, specs = pairwise ~ toy_weight:toy_size)
em1$contrasts 

# converting it from the log scale
# the result shows the ratio of counts for two levels 
# for eg., sit/squat has 9.29 which means the counts increase by 9.29 times for sitting posture 
# in comparison to squatting
em1 <- emmeans(nb_mod, specs = pairwise ~ posture, type = "response")
em1$contrasts
# to get the LCL and UCL
em$contrasts %>%summary(infer = TRUE)
# refer here for more:
# https://aosmith.rbind.io/2019/03/25/getting-started-with-emmeans/



### Similar analysis but now using toy_type
# copy paste the code from above to perform further tests
# ZIP model using toy_weight and toy_size
zip_mod <- zeroinfl(value ~  Age  + toy_type + posture | posture + toy_type , data = dat)
summary(zip_mod)

# Poisson model using toy_weight and toy_size
pois_mod <- glm(value ~  Age + posture + toy_type, data = dat, family = "poisson")
summary(pois_mod)

# negative binomial model using toy_weight and toy_size
nb_mod <- glm.nb(value ~  Age + posture + toy_type, data = dat)
summary(nb_mod)


