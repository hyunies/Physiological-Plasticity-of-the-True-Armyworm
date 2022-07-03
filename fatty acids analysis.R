## Clear R brain
rm(list= ls())

## Import libraries 
library(dplyr)
library(ggplot2)
library(car)
library(lsmeans)

setwd("School Notes/R studio/fixfattyacids2022.csv")

fa <- read.csv("School Notes/R studio/fixfattyacids2022..csv")

################################################################################
## Summarize just 20:4 PUFA fatty acids (TAG in fat body are changed to "1")

fa20.4.sum1 <- fa %>%
  filter(tissue == 1) %>%
  group_by(treatment) %>%
  summarise(mean.X20.04 = mean(X20.04, na.rm = TRUE),
            sd.X20.04 = sd(X20.04, na.rm = TRUE)) 

summary(fa20.4.sum1)

## Make a barplot
ggplot(data = fa20.4.sum1, aes(x = treatment, y = mean.X20.04,fill = treatment,  
                            ymax = mean.X20.04+sd.X20.04,
                            ymin = mean.X20.04-sd.X20.04)) + 
  geom_col(position = "dodge", width = 0.9) + 
  geom_errorbar(position = position_dodge(0.9), width = 0.2) + 
  theme_bw () +
  ylab(expression(paste("20:4 Extracted (nmol/g)", sep = ""))) + 
  xlab("Treatment")

fa20.4.lm <- lm(X20.04 ~ treatment, data = fa)
summary(fa20.4.lm)

# ANOVA
summary(aov(fa20.4.lm))

#             Df Sum Sq Mean Sq F value Pr(>F)
# treatment    1    119   119.2   0.759  0.392
# Residuals   24   3769   157.1    
### There is a significant difference between 20.4 extracted and treatment, LD has icnreased 20:4

################################################################################
## Summarize just 16:0 PUFA fatty acids (TAG in fat body are changed to "1")

fa16.00.sum1 <- fa %>%
  filter(tissue == 1) %>%
  group_by(treatment) %>%
  summarise(mean.X16.00 = mean(X16.00, na.rm = TRUE),
            sd.X16.00 = sd(X16.00, na.rm = TRUE)) 

summary(fa20.4.sum1)

## Make a barplot
ggplot(data = fa16.00.sum1, aes(x = treatment, y = mean.X16.00, fill = treatment, 
                               ymax = mean.X16.00+sd.X16.00,
                               ymin = mean.X16.00-sd.X16.00)) + 
  geom_col(position = "dodge", width = 0.9) + 
  geom_errorbar(position = position_dodge(0.9), width = 0.2) + 
  theme_bw () +
  ylab(expression(paste("16:0 Extracted (nmol/g)", sep = ""))) + 
  xlab("Treatment")

fa16.00.lm <- lm(X16.00 ~ treatment, data = fa)
summary(fa16.00.lm)

# ANOVA
summary(aov(fa16.00.lm))

# > summary(aov(fa16.00.lm))
#              Df Sum Sq Mean Sq F value Pr(>F)
# treatment    1    262   262.1   0.971  0.334
# Residuals   24   6480   270.0 
### There is a significant difference between 16:00 extracted and treatment, SD has increased 16:00

################################################################################
## Summarize just 18:3 fatty acids (TAG in fat body are changed to "1")

fa18.03.sum1 <- fa %>%
  filter(tissue == 1) %>%
  group_by(treatment) %>%
  summarise(mean.X18.03 = mean(X18.03, na.rm = TRUE),
            sd.X18.03 = sd(X18.03, na.rm = TRUE)) 

summary(fa18.03.sum1)

## Make a barplot
ggplot(data = fa18.03.sum1, aes(x = treatment, y = mean.X18.03, fill = treatment,
                               ymax = mean.X18.03+sd.X18.03,
                               ymin = mean.X18.03-sd.X18.03)) + 
  geom_col(position = "dodge", width = 0.9) + 
  geom_errorbar(position = position_dodge(0.9), width = 0.2) + 
  theme_bw () +
  ylab(expression(paste("18:3 Extracted (nmol/g)", sep = ""))) + 
  xlab("Treatment")

fa18.03.lm <- lm(X18.03 ~ treatment, data = fa)
summary(fa18.03.lm)

# ANOVA
summary(aov(fa18.03.lm))

#             Df Sum Sq Mean Sq F value Pr(>F)
# treatment    1   20.8   20.85   0.235  0.632
# Residuals   24 2130.9   88.79 
### No significant difference in treatments
