require(nlme)
require(lme4)
require(readxl)
require(ggplot2)
require(lmerTest)
require(tidyverse)
require(emmeans)
require(grid)
require(gridExtra)
require(car)
require(segmented)
require(lsr)
require(performance)
require(glmnet)
require(dplyr)
require(magrittr)
require(pastecs)
require(r2glmm)


# Decembre 2024 - Working on the manuscript
# UPDATE Sept 2025 - after ICA and new baseline correction
# clear workspace
rm(list = ls())

# Template
theme_template<-theme(plot.title = element_text(size = 20, family = "Helvetica", face = "bold", hjust = 0.5), 
                      text = element_text(size = 20, family = "Helvetica", face = "bold"), 
                      axis.title = element_text(face = "bold"), 
                      axis.text.x = element_text(size = 20, face = "bold"), 
                      panel.border = element_blank(), legend.title = element_blank(), legend.position = "bottom")

# Read the data
setwd("~/Dropbox/Dossier de l'équipe InMignonetteWeTrust/ULB/Projects/20_FG_kids/")
d <- read_xlsx("20FG_Master_v5.xlsx", sheet = "wide_2themselves")
d$Age_band <- factor(d$Age_band)

# 1. Are there outliers in any task? 
FG_sd <- 3*sd(d$FG_dPrime)
FG_mean <- mean(d$FG_dPrime)
FG_mean-FG_sd
min(d$FG_dPrime)
FG_mean+FG_sd
max(d$FG_dPrime)

Babble_sd <- 3*sd(d$Babble_r)
Babble_mean <- mean(d$Babble_r)
Babble_mean-Babble_sd
min(d$Babble_r)
Babble_mean+Babble_sd
max(d$Babble_r)

SSN_sd <- 3*sd(d$SSN_r, na.rm = TRUE)
SSN_mean <- mean(d$SSN_r, na.rm = TRUE)
SSN_mean-SSN_sd
min(d$SSN_r, na.rm = TRUE)
SSN_mean+SSN_sd
max(d$SSN_r, na.rm = TRUE)

PROMS_sd <- 3*sd(d$PROMS, na.rm = TRUE)
PROMS_mean <- mean(d$PROMS, na.rm = TRUE)
PROMS_mean-PROMS_sd
min(d$PROMS, na.rm = TRUE)
PROMS_mean+PROMS_sd
max(d$PROMS, na.rm = TRUE)

ORN_amp_sd <- 3*sd(d$ORN_amp, na.rm = TRUE)
ORN_amp_mean <- mean(d$ORN_amp, na.rm = TRUE)
ORN_amp_mean-ORN_amp_sd
min(d$ORN_amp, na.rm = TRUE)
ORN_amp_mean+ORN_amp_sd
max(d$ORN_amp, na.rm = TRUE)

ORN_lat_sd <- 3*sd(d$ORN_lat, na.rm = TRUE)
ORN_lat_mean <- mean(d$ORN_lat, na.rm = TRUE)
ORN_lat_mean-ORN_lat_sd
min(d$ORN_lat, na.rm = TRUE)
ORN_lat_mean+ORN_lat_sd
max(d$ORN_lat, na.rm = TRUE)

P400_amp_sd <- 3*sd(d$P400_amp, na.rm = TRUE)
P400_amp_mean <- mean(d$P400_amp, na.rm = TRUE)
P400_amp_mean-P400_amp_sd
min(d$P400_amp, na.rm = TRUE)
P400_amp_mean+P400_amp_sd
max(d$P400_amp, na.rm = TRUE)

P400_lat_sd <- 3*sd(d$P400_lat, na.rm = TRUE)
P400_lat_mean <- mean(d$P400_lat, na.rm = TRUE)
P400_lat_mean-P400_lat_sd
min(d$P400_lat, na.rm = TRUE)
P400_lat_mean+P400_lat_sd
max(d$P400_lat, na.rm = TRUE)


# 2. Logistic regression on presence of ORN/P400
SaturatedModel_ORN<-glm(ORN_presence ~ Age,
                        data = d, family = binomial(), na.action=na.exclude)
summary(SaturatedModel_ORN)
anova(SaturatedModel_ORN)

SaturatedModel_P400<-glm(P400_presence ~ Age,
                         data = d, family = binomial(), na.action=na.exclude)
summary(SaturatedModel_P400)
anova(SaturatedModel_P400)

# Plot me
p1a<-ggplot(data = d, aes(x = Age, y = ORN_presence))+ 
  geom_jitter(height = 0.02, size = 2, alpha = 0.6)+ 
  geom_smooth(method = "loess", se = FALSE, colour = "black") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, size = 1) +
  theme_template
p1a
p1b<-ggplot(data = d, aes(x = Age, y = P400_presence))+ 
  geom_jitter(height = 0.02, size = 2, alpha = 0.6)+ 
  geom_smooth(method = "loess", se = FALSE, colour = "black") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, size = 1) +
  theme_template
p1b
p1 <- grid.arrange(p1a, p1b, nrow = 1)

# 3. Linear regression on amplitude/latency of ORN/P400
# Is there a developmental effect on active ORN/P400 ?
# => Trend on the ORN
t <- gather(data = d, key = "Component", value = "Amplitude", ORN_amp, P400_amp)
m3_amp <- lmer(Amplitude ~ Age*Component*PROMS + (1|SubjectID), t)
anova(m3_amp)
# Should run a model reduction here (using the step function), but didn't work
# Quick & dirty fix:
# PROMS does not contribute either as a main effect or in interaction -> got removed
m3_amp <- lmer(Amplitude ~ Age*Component + (1|SubjectID), t)
anova(m3_amp)

# Run analyses separately on ORN and P400
# ORN amplitude
m3_ORN_amp <- lm(ORN_amp ~ Age*PROMS, data = d)
anova(m3_ORN_amp)
performance(m3_ORN_amp)
# Unstandardized effect size
(r2 = r2beta(m3_ORN_amp, method = 'kr', partial = T, data = d))

# ORN latency
m3_ORN_lat <- lm(ORN_lat ~ Age*PROMS, data = d)
anova(m3_ORN_lat)
performance(m3_ORN_lat)
# Unstandardized effect size
(r2 = r2beta(m3_ORN_lat, method = 'kr', partial = T, data = d))

# P400 amplitude
m3_P400_amp <- lm(P400_amp ~ Age*PROMS, data = d)
anova(m3_P400_amp)
performance(m3_P400_amp)
# Unstandardized effect size
(r2 = r2beta(m3_P400_amp, method = 'kr', partial = T, data = d))

# P400 latency
m3_P400_lat <- lm(P400_lat ~ Age*PROMS, data = d)
anova(m3_P400_lat)
performance(m3_P400_lat)
# Eta squared
# Unstandardized effect size
(r2 = r2beta(m3_P400_lat, method = 'kr', partial = T, data = d))

by(d$ORN_amp, list(d$Age_band), stat.desc, basic = FALSE)
by(d$P400_amp, list(d$Age_band), stat.desc, basic = FALSE)

p5a <- ggplot(d, aes(x = Age, y = ORN_amp)) + 
  geom_point() + 
  geom_smooth(method = "lm", alpha = 0.1) + 
  geom_smooth(method = "loess", alpha = 0.1) + 
  coord_cartesian(xlim = c(8, 23), ylim = c(-16, 2)) +
  theme_minimal() + theme_template
p5b <- ggplot(d, aes(x = Age, y = ORN_lat)) + 
  geom_point() + 
  geom_smooth(method = "lm", alpha = 0.1) + 
  geom_smooth(method = "loess", alpha = 0.1) + 
  coord_cartesian(xlim = c(8, 23), ylim = c(100, 900)) +
  theme_minimal() + theme_template
p5c <- ggplot(d, aes(x = Age, y = P400_amp)) + 
  geom_point() + 
  geom_smooth(method = "lm", alpha = 0.1) + 
  geom_smooth(method = "loess", alpha = 0.1) + 
  coord_cartesian(xlim = c(8, 23), ylim = c(-5, 12)) +
  theme_minimal() + theme_template
p5d <- ggplot(d, aes(x = Age, y = P400_lat)) + 
  geom_point() + 
  geom_smooth(method = "lm", alpha = 0.1) + 
  geom_smooth(method = "loess", alpha = 0.1) + 
  coord_cartesian(xlim = c(8, 23), ylim = c(100, 900)) +
  theme_minimal() + theme_template
p5 <- grid.arrange(p5a, p5b, p5c, p5d, nrow = 2)

#4. could this developmental effect be due to differences in variability across groups? 
# => yes
leveneTest(ORN_lat ~ Age_band, d, center = mean)
leveneTest(ORN_lat ~ Age_band, subset(d, Age_band != "Children"), center = mean)
leveneTest(ORN_lat ~ Age_band, subset(d, Age_band != "Adults"), center = mean)
leveneTest(ORN_lat ~ Age_band, subset(d, Age_band != "Adolescents"), center = mean)

leveneTest(P400_lat ~ Age_band, d, center = mean)
leveneTest(P400_lat ~ Age_band, subset(d, Age_band != "Children"), center = mean)
leveneTest(P400_lat ~ Age_band, subset(d, Age_band != "Adults"), center = mean)
leveneTest(P400_lat ~ Age_band, subset(d, Age_band != "Adolescents"), center = mean)

# 5. Only include those who showed an ORN/P400 at the individual level
d_ORN <- filter(d, ORN_presence == 1)
d_P400 <- filter(d, P400_presence == 1)

m3_ORN_amp <- lm(ORN_amp ~ Age*PROMS, data = d_ORN)
anova(m3_ORN_amp)
performance(m3_ORN_amp)
# Unstandardized effect size
(r2 = r2beta(m3_ORN_amp, method = 'kr', partial = T, data = d_ORN))

m3_ORN_lat <- lm(ORN_lat ~ Age*PROMS, data = d_ORN)
anova(m3_ORN_lat)
performance(m3_ORN_lat)
# Unstandardized effect size
(r2 = r2beta(m3_ORN_lat, method = 'kr', partial = T, data = d_ORN))

m3_P400_amp <- lm(P400_amp ~ Age*PROMS, data = d_P400)
anova(m3_P400_amp)
performance(m3_P400_amp)
# Unstandardized effect size
(r2 = r2beta(m3_P400_amp, method = 'kr', partial = T, data = d_P400))

m3_P400_lat <- lm(P400_lat ~ Age*PROMS, data = d_P400)
anova(m3_P400_lat)
performance(m3_P400_lat)
# Unstandardized effect size
(r2 = r2beta(m3_P400_lat, method = 'kr', partial = T, data = d_P400))

p5a <- ggplot(d_ORN, aes(x = Age, y = ORN_amp)) + 
  geom_point() + 
  geom_smooth(method = "lm", alpha = 0.1) + 
  geom_smooth(method = "loess", alpha = 0.1) + 
  coord_cartesian(xlim = c(8, 23), ylim = c(-16, 2)) +
  theme_minimal() + theme_template
p5b <- ggplot(d_ORN, aes(x = Age, y = ORN_lat)) + 
  geom_point() + 
  geom_smooth(method = "lm", alpha = 0.1) + 
  geom_smooth(method = "loess", alpha = 0.1) + 
  coord_cartesian(xlim = c(8, 23), ylim = c(100, 900)) +
  theme_minimal() + theme_template
p5c <- ggplot(d_P400, aes(x = Age, y = P400_amp)) + 
  geom_point() + 
  geom_smooth(method = "lm", alpha = 0.1) + 
  geom_smooth(method = "loess", alpha = 0.1) + 
  coord_cartesian(xlim = c(8, 23), ylim = c(-5, 12)) +
  theme_minimal() + theme_template
p5d <- ggplot(d_P400, aes(x = Age, y = P400_lat)) + 
  geom_point() + 
  geom_smooth(method = "lm", alpha = 0.1) + 
  geom_smooth(method = "loess", alpha = 0.1) + 
  coord_cartesian(xlim = c(8, 23), ylim = c(100, 900)) +
  theme_minimal() + theme_template
p5 <- grid.arrange(p5a, p5b, p5c, p5d, nrow = 2)

#6. What are the predictors of Figure_Ground ?
# Lasso selection
d <- subset(d, SubjectID != "s068")
x <- model.matrix(~ Age*PROMS*P400_amp*ORN_amp*P400_lat*ORN_lat, data = d) [,-1]
y <- d$FG_dPrime

lasso_model <- cv.glmnet(x, y, family = "gaussian", alpha = 1, type.measure = "mse",
                         standardize = TRUE)
print(lasso_model)

coefficients <- coef(lasso_model, s = lasso_model$lambda.1se)
print(coefficients)

# Which model is the best fit? - Oct 2025
m6a <- lm(FG_dPrime ~ Age*PROMS*P400_amp*ORN_amp*P400_lat*ORN_lat, data = d)
anova(m6a)
performance(m6a)
m6b <- lm(FG_dPrime ~ Age*PROMS*P400_amp, data = d)
anova(m6b)
# Unstandardized effect size
(r2 = r2beta(m6b, method = 'kr', partial = T, data = d))
performance(m6b)
m6c <- lm(FG_dPrime ~ Age*P400_amp, data = d)
anova(m6c)
performance(m6c)
m6d <- lm(FG_dPrime ~ PROMS, data = d)
anova(m6d)
performance(m6d)
anova(m6a, m6b, m6c, m6d)
AIC(m6a, m6b, m6c, m6d)

# Broken stick regression on age <-> d'? -> nope
df <- data.frame(Age = d$Age, Segregation = d$FG_dPrime)
df <- df[with(df, order(Age)), ]
fit <- lm(Segregation ~ Age, data=df)
segmented.fit <- segmented(fit, seg.Z = ~Age, psi=16)
summary(segmented.fit)
anova(fit, segmented.fit)

# Broken stick regression on age <-> ORN? -> nope
df <- data.frame(Age = d$Age, ORN_amp = d$ORN_amp)
df <- df[with(df, order(Age)), ]
fit <- lm(ORN_amp ~ Age, data=df)
segmented.fit <- segmented(fit, seg.Z = ~Age, psi=12)
summary(segmented.fit)
anova(fit, segmented.fit)

#7. What are the predictors of SiN ?
# Lasso selection
SIN <- read_xlsx("20FG_Master_v5.xlsx", sheet = "long_2themselves")
SIN$Age_band <- factor(SIN$Age_band)
SIN$Condition <- factor(SIN$Condition)
SIN$score <- (SIN$r/SIN$n)*100
SIN <- subset(SIN, Condition != "SiQ")
SIN <- subset(SIN, SubjectID != "s078")
SIN <- subset(SIN, SubjectID != "s094")

x <- model.matrix(~ Age*PROMS*Condition*P400_amp*ORN_amp*P400_lat*ORN_lat*FG_dPrime, data = SIN) [,-1]
y <- SIN$score

lasso_model <- cv.glmnet(x, y, family = "gaussian", alpha = 1, type.measure = "mse",
                         standardize = TRUE)
print(lasso_model)

coefficients <- coef(lasso_model, s = lasso_model$lambda.1se)
print(coefficients)

mSIN_1 <- glmer(cbind(r, n-r) ~ Condition + P400_lat + Age + FG_dPrime + Condition:P400_lat + Age:FG_dPrime + (1|SubjectID), 
                data = SIN, family = "binomial", control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(mSIN_1)


# Decompose the Age x FG interaction
mSIN_kids <- glmer(cbind(r, n-r) ~ FG_dPrime + (1|SubjectID), 
                data = subset(SIN, Age_band == "Children"), family = "binomial", control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(mSIN_kids)
mSIN_ado <- glmer(cbind(r, n-r) ~ FG_dPrime + (1|SubjectID), 
                   data = subset(SIN, Age_band == "Adolescents"), family = "binomial", control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(mSIN_ado)
mSIN_adults <- glmer(cbind(r, n-r) ~ FG_dPrime + (1|SubjectID), 
                   data = subset(SIN, Age_band == "Adults"), family = "binomial", control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(mSIN_adults)

# Main effect of condition
# As well as an Age x FG_dPrime interaction
p7a <- ggplot(SIN, aes(x = Condition, y = score)) + 
  geom_violin(lwd = 1, na.rm = TRUE) + 
  geom_boxplot(width = 0.1, lwd = 1, na.rm = TRUE) + 
  geom_dotplot(binaxis = 'y', stackdir = 'center', shape = 16, dotsize = 0.7, alpha = 0.15) +
  stat_summary(fun.y = mean, geom = "point", size = 4, color = "red") + 
  coord_cartesian(ylim = c(0, 100)) +
  theme_minimal() + theme_template
p7a

p7b <- ggplot(SIN, aes(x = FG_dPrime, y = score, colour = Age_band)) + 
  geom_point() + 
  geom_smooth(method = "lm", alpha = 0.1) + 
#  geom_smooth(method = "loess", alpha = 0.1) + 
  coord_cartesian(xlim = c(-0.5, 4.5), ylim = c(0, 100)) +
  theme_minimal() + theme_template
p7b

p7b <- ggplot(SIN, aes(x = Age, y = score)) + 
  geom_point() + 
  geom_smooth(method = "lm", alpha = 0.1) + 
  #  geom_smooth(method = "loess", alpha = 0.1) + 
  coord_cartesian(xlim = c(8, 25), ylim = c(0, 100)) +
  theme_minimal() + theme_template
p7b

# Manuscript revision - July 2025
# Does a quadratic/cubic regression fit the presence/ absence data, better than a linear fit?
# ORN
g_linearModel <- glm(ORN_presence ~ Age, data = d, family = binomial())
summary(g_linearModel)

quadraticModel <- glm(ORN_presence ~ Age + I(Age^2), data = d, family = binomial())
summary(quadraticModel)

anova(g_linearModel, quadraticModel)


# Plot this
ageValues <- seq(8, 22, 0.1)
ORN_predict <- predict(quadraticModel, list(Age = ageValues, Age2 = ageValues^2))
plot(d$Age, d$ORN_presence, pch = 16)
lines(ageValues, ORN_predict, col = "blue")

# P400
g_linearModel <- glm(P400_presence ~ Age, data = d, family = binomial(link = "logit"))
summary(g_linearModel)

quadraticModel <- glm(P400_presence ~ Age + I(Age^2), data = d, family = binomial(link = "logit"))
summary(quadraticModel)

anova(g_linearModel, quadraticModel)

# Plot this
ageValues <- seq(8, 22, 0.1)
P400_predict <- predict(quadraticModel, list(Age = ageValues, Age2 = ageValues^2))
plot(d$Age, d$P400_presence, pch = 16)
lines(ageValues, P400_predict, col = "blue")




