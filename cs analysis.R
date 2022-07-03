## Clear R brain
rm(list= ls())

## Import libraries 
library(dplyr)
library(ggplot2)
library(car)
library(lsmeans)

cs <- read.csv("School Notes/R studio/fixcitratesynthase2022.csv")

# want to compare M/F LD and SD to thorax mass
# want to compare M/F LD and SD to activity

################################################################################
##### Thorax mass

masscs.sum1 <- cs %>%
  group_by(sex, treatment) %>%
  summarise(mean.thorax.mass = mean(thorax.mass, na.rm = TRUE),
            sd.thorax.mass = sd(thorax.mass, na.rm = TRUE)) 

ggplot(data = masscs.sum1, aes(x = treatment, y = mean.thorax.mass, 
                              fill = sex, group = sex,
                              ymax = mean.thorax.mass+sd.thorax.mass,
                              ymin = mean.thorax.mass-sd.thorax.mass)) + 
  geom_col(position = "dodge", width = 0.9) + 
  geom_errorbar(position = position_dodge(0.9), width = 0.2) + 
  theme_classic() +
  ylab(expression(paste("Thorax Mass", sep = ""))) + 
  xlab("Treatment")

##### STATS

masscs.lm <- lm(thorax.mass ~ sex*treatment, data = cs)

summary(aov(masscs.lm))
# Df    Sum Sq   Mean Sq F value Pr(>F)  
# sex            1 0.0001168 1.168e-04   3.799 0.0622 .
# treatment      1 0.0000901 9.013e-05   2.931 0.0988 .
# sex:treatment  1 0.0000470 4.700e-05   1.528 0.2274  
# Residuals     26 0.0007995 3.075e-05                 
# NO SIG DIFF IN MASS BETWN SEX AND TREATMENT

################################################################################
##### CS ACTIVITY

accs.sum1 <- cs %>%
  group_by(sex, treatment) %>%
  summarise(mean.activity = mean(activity, na.rm = TRUE),
            sd.activity = sd(activity, na.rm = TRUE)) 

ggplot(data = accs.sum1, aes(x = treatment, y = mean.activity, 
                               fill = sex, group = sex,
                               ymax = mean.activity+sd.activity,
                               ymin = mean.activity-sd.activity)) + 
  geom_col(position = "dodge", width = 0.9) + 
  geom_errorbar(position = position_dodge(0.9), width = 0.2) + 
  theme_classic() +
  ylab(expression(paste("CS Activity", sep = ""))) + 
  xlab("Treatment")
##### STATS

accs.lm <- lm(activity ~ sex*treatment, data = cs)

summary(aov(accs.lm))
# sex            1  14442   14442  14.323 0.000818 ***
# treatment      1    272     272   0.270 0.607891    
# sex:treatment  1   1842    1842   1.827 0.188182    
# Residuals     26  26215    1008 

##### SEX but not treatment had a significant impact on cs activity

## TUKEY set up
accs.lm2 <- lm(activity ~ sex*treatment, data = cs) %>% aov()

TukeyHSD(accs.lm2)

# $sex
# diff      lwr      upr     p adj
# M-F 43.97978 20.09329 67.86628 0.0008178 ***
# 
# $treatment
# diff       lwr      upr     p adj
# SD-LD 6.022061 -17.81129 29.85542 0.6078906
# 
# $`sex:treatment`
# diff        lwr        upr     p adj
# M:LD-F:LD  28.27469 -16.809077  73.358452 0.3338076
# F:SD-F:LD -10.73004 -57.292352  35.832271 0.9207079
# M:SD-F:LD  48.95484   3.871073  94.038602 0.0296253 ***
# F:SD-M:LD -39.00473 -84.088493   6.079036 0.1075665
# M:SD-M:LD  20.68015 -22.874904  64.235204 0.5695896
# M:SD-F:SD  59.68488  14.601114 104.768643 0.0062504 ***

## significant difference between male and female sd (males sig higher on sd)
## sig difference between sd m and ld f (male higher)
## no diff btwn males SD/SD; therefore SD males have sig higher cs activity than female sd and female ld

