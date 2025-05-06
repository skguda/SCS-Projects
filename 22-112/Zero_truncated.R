library(foreign)
library(VGAM)
library(boot)
library(ggplot2)
library(lmtest)
library(stringr)
library(pscl)
#Importing wide format data
dat <- read.csv("RDBMDataStats.csv")
View(dat)

#converting to long format
df_long <- melt(dat, id.vars = c("ID","Age"), variable.name = "toy_posture")
View(df_long)
df_long[c('posture', 'toy_size', 'toy_weight')] <- str_split_fixed(df_long$toy_posture, '_', 3)
df_long$toy_type <- paste(df_long$toy_size, "-", df_long$toy_weight)
dat <- df_long

# Importing data
#dat <- read.csv("posture_toy_transformed.csv")
#View(dat)

# Filtering data for count value > 0
dat2 <- dat[dat$value > 0, ] 
View(dat2)
# Changing data type
dat2$Age <- factor(dat2$Age)
dat2$toy_size <- factor(dat2$toy_size)
dat2$toy_weight <- factor(dat2$toy_weight)
#dat2$toy_type <- factor(dat2$toy_type)
dat2$posture <- factor(dat2$posture)
class(dat2$Age)
class(dat2$toy_size)
#class(dat2$toy_type)
class(dat2$value)
e <- ggplot(dat, aes(x = toy_weight, y = value))
e + geom_boxplot()
# Releving the reference class to 'sit' in posture variable
dat2$posture <- relevel(dat2$posture, ref = "sit")
dat$toy_type <- relevel(dat$toy_type, ref = "medium - light")
dat$toy_type <- factor(dat$toy_type)
# Poisson regression
mod_pois <- glm(value ~ Age  + posture + toy_weight + toy_size, family = "poisson", data = dat)
summary(mod_pois)

# Zero truncated poisson regression
mod_z_pois <- vglm(value ~ Age + toy_size + toy_weight + posture, family=pospoisson(), data=dat2)
summary(mod_z_pois)

# logistic regression to predict the posture
mod_bin <- glm(posture ~  Age+ toy_size + toy_weight + value, data = dat2, family = "binomial")
summary(mod_bin)

m1 <- zeroinfl(value ~  factor(Age) + posture + toy_type |posture + toy_type , data = dat)
summary(m1)
# is toy wright toy size related to the posture
# 2-way contigency tables for toy_weight and value with posture
table(dat2$posture, dat2$toy_weight)
table(dat2$posture, dat2$value)
#chi-square test to test the correlation between posture and toy_weight
chisq.test(dat2$posture, dat2$value, simulate.p.value = TRUE)

