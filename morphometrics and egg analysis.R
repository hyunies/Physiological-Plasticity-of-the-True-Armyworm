# Morphometrics and Eggs Analysis ####

## Hypothesis: 

## Adult emergence environment dictates adult phenotype 

## Clear R brain
rm(list= ls())

## Import libraries 
library(dplyr)
library(ggplot2)
library(car)
library(lsmeans)

## set working directory
#setwd("D:/OneDrive - The University of Western Ontario/PhD/TA/2022_01W_BIOL3625/R Lessons/RDemo/Module 2") 

# Setwd doesn't work but just use eggs and morph
setwd("School Notes/R studio/fixmorphometrics2022.csv")

#################/
#################/
## Egg counts ####
#################/
#################/
#################/

## Import moth egg count data
eggs <- read.csv("School Notes/R studio/fixMotheggs2022.csv")

################################################################################
##### EGGS DATA #####
################################################################################
eggs.lm <- lm(egg.count ~ age*treatment, data = eggs)
summary(eggs.lm)

summary(aov(eggs.lm))

# remove non-sig interactions
eggs.lm2 <- lm(egg.count ~ age*treatment, data = eggs) %>% aov()

summary(eggs.lm2)

TukeyHSD(eggs.lm2)
# $treatment
# diff       lwr       upr     p adj
# SD-LD -27.125 -47.35257 -6.897426 0.0094298

# Egg count differed significant between LD and SD, no difference in age (other than 0)
# star 3 and 6

lsmeans(eggs.lm, pairwise ~ age | treatment, adjust = "tukey") 
# $contrasts
# treatment = LD:
#   contrast estimate   SE df t.ratio p.value
# 0 - 6    -40.2500 17.9 36  -2.250  0.0307
# 
# treatment = SD:
#   contrast estimate   SE df t.ratio p.value
# 0 - 6      0.0833 17.9 36   0.005  0.9963

lsmeans(eggs.lm, pairwise ~ treatment | age, adjust = "tukey") 
# $contrasts
# age = 0:
#   contrast estimate   SE df t.ratio p.value
# LD - SD     -0.25 19.6 36  -0.013  0.9899
# 
# age = 6:
#   contrast estimate   SE df t.ratio p.value
# LD - SD     40.08 16.0 36   2.505  0.0169


################################################################################
##### EGGS BAR PLOT, change X axis on WORD #####
################################################################################
ggplot(data = eggs, aes(x = age, y = egg.count, fill = treatment)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  theme_bw() +
  ylab(expression(paste("Egg Count", sep = ""))) + 
  xlab("Age")

ggplot(data = eggs, aes(x = age, y = egg.count, fill = treatment)) + 
  geom_boxplot() + 
  theme_bw() + 
  ylab(expression(paste("Egg Count", sep = ""))) + 
  xlab("Age")

################################################################################
##### THROAX MASS #####
################################################################################

tmeggs.lm <- lm(thorax.mass ~ age*treatment, data = eggs)
summary(tmeggs.lm)

summary(aov(tmeggs.lm))

# remove non-sig interactions
tmeggs.lm2 <- lm(thorax.mass ~ age*treatment, data = eggs) %>% aov()

summary(tmeggs.lm2)

TukeyHSD(tmeggs.lm2)
# $treatment
# diff         lwr        upr     p adj
# SD-LD 0.01290625 0.001419375 0.02439312 0.0282993

lsmeans(tmeggs.lm, pairwise ~ age | treatment, adjust = "tukey") 
lsmeans(tmeggs.lm, pairwise ~ treatment | age, adjust = "tukey") 

################################################################################
##### THORAX MASS PLOT #####
################################################################################
teggs.lm <- lm(thorax.mass ~ age*treatment, data = eggs)
summary(teggs.lm)

lsmeans(teggs.lm, pairwise ~ age | treatment, adjust = "tukey") 

# $contrasts
# treatment = LD:
#   contrast estimate   SE df t.ratio p.value
# 0 - 6    -40.2500 17.9 36  -2.250  0.0307
# 
# treatment = SD:
#   contrast estimate   SE df t.ratio p.value
# 0 - 6      0.0833 17.9 36   0.005  0.9963

lsmeans(teggs.lm, pairwise ~ treatment | age, adjust = "tukey")
# $contrasts
# $contrasts
# age = 0:
#   contrast estimate   SE df t.ratio p.value
# LD - SD     -0.25 19.6 36  -0.013  0.9899
# 
# age = 6:
#   contrast estimate   SE df t.ratio p.value
# LD - SD     40.08 16.0 36   2.505  0.0169


## summarize thorax mass
tmegg.sum1 <- eggs %>%
  group_by(age, treatment) %>%
  summarise(mean.thorax.mass = mean(thorax.mass, na.rm = TRUE),
            sd.thorax.mass = sd(thorax.mass, na.rm = TRUE)) 
## Make a barplot
ggplot(data = egg.sum1, aes(x = treatment, y = mean.thorax.mass, 
                              fill = age, group = age,
                              ymax = mean.thorax.mass+sd.thorax.mass,
                              ymin = mean.thorax.mass-sd.thorax.mass)) + 
  geom_col(position = "dodge", width = 0.9) + 
  geom_errorbar(position = position_dodge(0.9), width = 0.2) + 
  theme_bw () +
  ylab(expression(paste("Thorax Mass (g)", sep = ""))) + 
  xlab("Treatment")

cegg.sum1 <- eggs %>%
  group_by(age, treatment) %>%
  summarise(mean.egg.count = mean(egg.count, na.rm = TRUE),
            sd.egg.count = sd(egg.count, na.rm = TRUE)) 

ggplot(data = cegg.sum1, aes(x = treatment, y = mean.egg.count, 
                            fill = age, group = age,
                            ymax = mean.egg.count+sd.egg.count,
                            ymin = mean.egg.count-sd.egg.count)) + 
  geom_col(position = "dodge", width = 0.9) + 
  geom_errorbar(position = position_dodge(0.9), width = 0.2) + 
  theme_bw () +
  ylab(expression(paste("Egg Count", sep = ""))) + 
  xlab("Treatment")

#####################/
#####################/
## Morphometrics ####
#####################/
#####################/

## Import morphometrics data
morph <- read_csv("School Notes/R studio/morphometrics2022.csv")

## Import fixed morphometrics data for aspect ratio
fixmorph <- read_csv("School Notes/R studio/fixmorphometrics2022.xlsx.csv")

## Calculate means and standard deviation wing.loading for all combinations of sex, age, and treatment
morph.sum <- morph %>% 
  group_by(sex, age, treatment) %>%
  summarise(mean.wing.loading = mean(wing.loading, na.rm = TRUE),
            sd.wing.loading = sd(wing.loading, na.rm = TRUE))

## PREDICTION: Short day treated moths have higher wing loading than long day moths AND that wing loading may differ between sexes and across age

## Graph our prediction! 
ggplot(data = morph.sum, aes(x = age, y = mean.wing.loading, 
                             colour = sex, shape = treatment, linetype = sex, 
                             ymax = mean.wing.loading+sd.wing.loading, 
                             ymin = mean.wing.loading-sd.wing.loading)) + 
  geom_point(size = 8) + 
  geom_line() + 
  geom_errorbar(width = 0.2) + 
  theme_classic()

## Other prediction(s) :: Wing loading is greater in SD moths at day 6 and differs between M & F.
morph.sum2 <- morph %>%
  filter(age == 6) %>%
  group_by(sex, treatment) %>%
  summarise(mean.wing.loading = mean(wing.loading, na.rm = TRUE),
            sd.wing.loading = sd(wing.loading, na.rm = TRUE)) 
## Make a barplot
ggplot(data = morph.sum2, aes(x = treatment, y = mean.wing.loading, 
                              fill = sex, group = sex,
                              ymax = mean.wing.loading+sd.wing.loading,
                              ymin = mean.wing.loading-sd.wing.loading)) + 
  geom_col(position = "dodge", width = 0.9) + 
  geom_errorbar(position = position_dodge(0.9), width = 0.2) + 
  theme_classic()


## Other prediction(s) :: Wing loading is greater in SD moths regardless of age or sex
ggplot(data = morph, aes(x = treatment, y = wing.loading)) + 
  geom_boxplot() + 
  theme_classic()
## Note that it is important to check if it's ok to remove the 'unwanted' variables before removing them from your predictions. These are all examples of 'exploratory' plots only. 

## Make some predictions! 

## Females may be heavier in LD because they will be reproductively active compared to SD.

## Plot female masses by age & treatment

## Select only females
morph.f <- morph %>%
  filter(sex == "F")

ggplot(morph.f, aes(x = as.factor(age), y = mass, fill = treatment)) + 
  geom_boxplot()

morph.f.lm <- lm(mass ~ age*treatment, data = morph.f)

summary(morph.f.lm)

## Plot male masses by age & treatment
morph.m <- morph %>%
  filter(sex == "M")

ggplot(morph.m, aes(x = as.factor(age), y = mass, fill = treatment)) + 
  geom_boxplot()




##### STATS #####

## Could compare males and females (sex and treatment comparison within each age)
morph.0 <- filter(morph, age == 0)

ggplot(data = morph.0, aes(x = sex, y = mass, fill = treatment)) + 
  geom_boxplot()


## Could examine only LD within each sex across age
morph.6 <- filter(morph, age == 6)

ggplot(data = morph.6, aes(x = sex, y = mass, fill = treatment)) + 
  geom_boxplot()


################################################################################
##### Day 0 Mass ##### DONE
################################################################################
summary(aov(mmorph.0.lm))
##ABOVE IS ANOVA


mmorph.0.lm <- lm(mass ~ sex*treatment, data = morph.0)
summary(mmorph.0.lm)

lsmeans(mmorph.0.lm, pairwise ~ sex | treatment, adjust = "tukey") 

# $contrasts
# treatment = LD:
#   contrast estimate     SE df t.ratio p.value
# F - M      0.0015 0.0201 28   0.074  0.9412

# treatment = SD:
#   contrast estimate     SE df t.ratio p.value
# F - M      0.0000 0.0201 28   0.000  1.0000

lsmeans(mmorph.0.lm, pairwise ~ treatment | sex, adjust = "tukey")
# $contrasts
# sex = F:
#   contrast estimate     SE df t.ratio p.value
# LD - SD    0.0285 0.0201 28   1.415  0.1682

# sex = M:
#   contrast estimate     SE df t.ratio p.value
# LD - SD    0.0270 0.0201 28   1.340  0.1910

# AT AGE 0, day length and sex did not have a significant impact on mass
ggplot(data = morph.0, aes(x = treatment, y = mass, fill = sex)) + 
  geom_boxplot() + 
  theme_bw() + 
  ylab(expression(paste("Body Mass (g)", sep = ""))) + 
  xlab("Treatment")

################################################################################
##### Day 6 Mass ##### DONE
################################################################################

mmorph.6.lm <- lm(mass ~ sex*treatment, data = morph.6)
summary(morph.6.lm)

summary(aov(mmorph.6.lm))

lsmeans(mmorph.6.lm, pairwise ~ sex | treatment, adjust = "tukey") 
# $contrasts
# treatment = LD:
#   contrast estimate     SE df t.ratio p.value
# F - M      0.0693 0.0219 44   3.172  0.0028
# 
# treatment = SD:
#   contrast estimate     SE df t.ratio p.value
# F - M     -0.0496 0.0219 44  -2.268  0.0283

lsmeans(mmorph.0.lm, pairwise ~ treatment | sex, adjust = "tukey")
# $contrasts
# sex = F:
#   contrast estimate     SE df t.ratio p.value
# LD - SD    0.0285 0.0201 28   1.415  0.1682
# 
# sex = M:
#   contrast estimate     SE df t.ratio p.value
# LD - SD    0.0270 0.0201 28   1.340  0.1910


# At AGE 6, during LD, females have an increased body mass compared to males
ggplot(data = morph.6, aes(x = sex, y = mass, fill = treatment)) + 
  geom_boxplot()

# Flipped: Star over LD; females have a sig diff increased body mass compared to males
ggplot(data = morph.6, aes(x = treatment, y = mass, fill = sex)) + 
  geom_boxplot() + 
  theme_bw() + 
  ylab(expression(paste("Body Mass (g)", sep = ""))) + 
  xlab("Treatment")

################################################################################
##### Day 0 Wing Loading #####
################################################################################
wmorph.0.lm <- lm(wing.loading ~ sex*treatment, data = morph.0)
summary(wmorph.0.lm)

summary(aov(wmorph.0.lm))

lsmeans(wmorph.0.lm, pairwise ~ sex | treatment, adjust = "tukey") 
# treatment = LD:
#   contrast  estimate      SE df t.ratio p.value
# F - M     0.000149 0.00521 26   0.029  0.9774
# 
# treatment = SD:
#   contrast  estimate      SE df t.ratio p.value
# F - M    -0.003198 0.00521 26  -0.614  0.5444

lsmeans(wmorph.0.lm, pairwise ~ treatment | sex, adjust = "tukey")
# $contrasts
# sex = F:
#   contrast estimate      SE df t.ratio p.value
# LD - SD  -0.00301 0.00521 26  -0.577  0.5686
# 
# sex = M:
#   contrast estimate      SE df t.ratio p.value
# LD - SD  -0.00635 0.00521 26  -1.220  0.2333


## Examine wing loading at age 0, no sig interactions
ggplot(data = morph.0, aes(x = sex, y = wing.loading, fill = treatment)) + 
  geom_boxplot() + 
  theme_bw() + 
  ylab(expression(paste("Wing Loading (g/cm^2)", sep = ""))) + 
  xlab("Sex")

################################################################################
##### Day 6 Wing Loading ##### PERFECT AND DONE
################################################################################
wmorph.6.lm <- lm(wing.loading ~ sex*treatment, data = morph.6)
summary(wmorph.6.lm)

summary(aov(wmorph.6.lm))

lsmeans(wmorph.6.lm, pairwise ~ sex | treatment, adjust = "tukey") 

# $lsmeans
# treatment = LD:
#   sex lsmean      SE df lower.CL upper.CL
# F   0.0626 0.00411 39   0.0542   0.0709
# M   0.0488 0.00432 39   0.0400   0.0575

#treatment = SD:
#  sex lsmean      SE df lower.CL upper.CL
#F   0.0449 0.00432 39   0.0361   0.0536
#M   0.0595 0.00394 39   0.0515   0.0675

#Confidence level used: 0.95 

#$contrasts
#treatment = LD:
#  contrast estimate      SE df t.ratio p.value
#F - M      0.0138 0.00596 39   2.314  0.0261 ***

#treatment = SD:
#  contrast estimate      SE df t.ratio p.value
#F - M     -0.0146 0.00584 39  -2.502  0.0167 ***

lsmeans(wmorph.6.lm, pairwise ~ treatment | sex, adjust = "tukey")
# $contrasts
# sex = F:
#  contrast estimate      SE df t.ratio p.value
# LD - SD    0.0177 0.00596 39   2.968  0.0051

# sex = M:
#   contrast estimate      SE df t.ratio p.value
# LD - SD   -0.0107 0.00584 39  -1.834  0.0743

#At age 6, LD and SD made a significant difference Female wing loading but not males (0.07)

ggplot(data = morph.6, aes(x = sex, y = wing.loading, fill = treatment)) + 
  geom_boxplot() + 
  theme_bw() + 
  ylab(expression(paste("Wing Loading (g/cm^2)", sep = ""))) + 
  xlab("Sex")
# star over female (sig dif) but not male

################################################################################
##### Day 0 Aspect Ratio #####
################################################################################
fixmorph.0 <- filter(fixmorph, age == 0)

amorph.0.lm <- lm(mass ~ sex*treatment, data = fixmorph.0)
summary(amorph.0.lm)

summary(aov(amorph.0.lm))

lsmeans(amorph.0.lm, pairwise ~ sex | treatment, adjust = "tukey") 
# $contrasts
# treatment = LD:
#   contrast estimate     SE df t.ratio p.value
# F - M      0.0015 0.0201 28   0.074  0.9412
# 
# treatment = SD:
#   contrast estimate     SE df t.ratio p.value
# F - M      0.0000 0.0201 28   0.000  1.0000

lsmeans(amorph.0.lm, pairwise ~ treatment | sex, adjust = "tukey")
# $contrasts
# sex = F:
#   contrast estimate     SE df t.ratio p.value
# LD - SD    0.0285 0.0201 28   1.415  0.1682
# 
# sex = M:
#   contrast estimate     SE df t.ratio p.value
# LD - SD    0.0270 0.0201 28   1.340  0.1910

# NO SIG INTERACTIONS
ggplot(data = fixmorph.0, aes(x = sex, y = aspect.ratio, fill = treatment)) + 
  geom_boxplot() + 
  theme_bw() + 
  ylab(expression(paste("Aspect Ratio", sep = ""))) + 
  xlab("Sex")

################################################################################
##### Day 6 Aspect Ratio #####
################################################################################
fixmorph.6 <- filter(fixmorph, age == 6)

amorph.6.lm <- lm(aspect.ratio ~ sex*treatment, data = fixmorph.6)
summary(amorph.6.lm)

summary(aov(amorph.6.lm))

lsmeans(amorph.6.lm, pairwise ~ sex | treatment, adjust = "tukey") 
# $contrasts
# treatment = LD:
#   contrast estimate     SE df t.ratio p.value
# F - M      0.0693 0.0219 44   3.172  0.0028
# 
# treatment = SD:
#   contrast estimate     SE df t.ratio p.value
# F - M     -0.0496 0.0219 44  -2.268  0.0283

lsmeans(amorph.6.lm, pairwise ~ treatment | sex, adjust = "tukey")
# $contrasts
# sex = F:
#   contrast estimate     SE df t.ratio p.value
# LD - SD   0.11125 0.0219 44   5.088  <.0001
# 
# sex = M:
#   contrast estimate     SE df t.ratio p.value
# LD - SD  -0.00768 0.0219 44  -0.351  0.7270

# for females, ld-sd differs significantly, and for ld there is a 
# significant difference between males and females
ggplot(data = fixmorph.6, aes(x = sex, y = aspect.ratio, fill = treatment)) + 
  geom_boxplot() + 
  theme_bw() + 
  ylab(expression(paste("Aspect Ratio", sep = ""))) + 
  xlab("Sex")