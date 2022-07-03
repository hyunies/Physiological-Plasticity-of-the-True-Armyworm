## Clear R brain
rm(list= ls())

## Import libraries 
library(dplyr)
library(ggplot2)
library(car)
library(lsmeans)

hae <- read.csv("School Notes/R studio/haemolymphlipids2022.csv")

hae.sum <- hae %>% 
  group_by(sex, treatment, flight.time) %>%
  summarise(mean.lipid.ug.ul = mean(lipid.ug.ul, na.rm = TRUE),
            sd.lipid.ug.ul = sd(lipid.ug.ul, na.rm = TRUE))

## Graph our prediction! 
ggplot(data = hae.sum, aes(x = flight.time, y = mean.lipid.ug.ul, 
                             colour = sex, shape = treatment, linetype = sex, 
                             ymax = mean.lipid.ug.ul+sd.lipid.ug.ul, 
                             ymin = mean.lipid.ug.ul-sd.lipid.ug.ul)) + 
  geom_point(size = 8) + 
  geom_line() + 
  geom_errorbar(width = 0.2) + 
  theme_classic() + 
  ylab(expression(paste("Lipid Concentration (", mu, "g/", mu, "L)", sep = ""))) + 
  xlab("Flight Time") 

################################################################################
##### FIND IF TREATMENT/SEX DIFFERS AT 0/15/30

## 0s #####
hae.0 <- filter(hae, flight.time == 0)

hae.0.lm <- lm(lipid.ug.ul ~ sex*treatment, data = hae.0)
summary(aov(hae.0.lm))
#               Df Sum Sq Mean Sq F value Pr(>F)
# sex            1      0     0.4   0.001  0.973
# treatment      1    418   417.7   1.188  0.292
# sex:treatment  1    263   263.3   0.749  0.400
# Residuals     16   5623   351.5 
## AT 0s, SEX & TREATMENT DIDN'T SIG IMPACT

## 15s ######
hae.15 <- filter(hae, flight.time == 15)

hae.15.lm <- lm(lipid.ug.ul ~ sex*treatment, data = hae.15)
summary(aov(hae.15.lm))
# Df Sum Sq Mean Sq F value Pr(>F)
# sex            1     23    23.1   0.028  0.869
# treatment      1    243   243.3   0.295  0.595
# sex:treatment  1    143   142.8   0.173  0.683
# Residuals     16  13199   824.9 
## AT 15s, SEX & TREATMENT DIDN'T SIG IMPACT

## 30s ######
hae.30 <- filter(hae, flight.time == 30)

hae.30.lm <- lm(lipid.ug.ul ~ sex*treatment, data = hae.30)
summary(aov(hae.30.lm))

# Df Sum Sq Mean Sq F value Pr(>F)
# sex            1   1954  1954.3   2.581  0.128
# treatment      1    367   366.8   0.484  0.496
# sex:treatment  1   1327  1326.6   1.752  0.204
# Residuals     16  12114   757.1 
## AT 30s, SEX & TREATMENT DIDN'T SIG IMPACT

hae.30.lm2 <- lm(lipid.ug.ul ~ sex*treatment, data = hae.30) %>% aov()

TukeyHSD(hae.30.lm2)
# M:LD-F:LD  36.058555 -13.73028 85.84739 0.2041232
# Even the biggest gap isn't significant

################################################################################
##### Pairwise 0-15, 15-30 and 0-30

trhae.lm <- lm(lipid.ug.ul ~ flight.time*treatment, data = hae)
summary(aov(trhae.lm))
# Df Sum Sq Mean Sq F value Pr(>F)  
# flight.time            1   2374  2374.2   3.640 0.0615 .
# treatment              1    192   191.9   0.294 0.5897  
# flight.time:treatment  1      1     0.8   0.001 0.9717  
# Residuals             56  36527   652.3    

lsmeans(trhae.lm, pairwise ~ flight.time | treatment, adjust = "tukey")
lsmeans(trhae.lm, pairwise ~ treatment | flight.time, adjust = "tukey")

sehae.lm <- lm(lipid.ug.ul ~ flight.time*sex, data = hae)
summary(aov(sehae.lm))
# Df Sum Sq Mean Sq F value Pr(>F)  
# flight.time      1   2374  2374.2   3.804 0.0561 .
# sex              1    822   821.9   1.317 0.2560  
# flight.time:sex  1    949   948.8   1.520 0.2227  
# Residuals       56  34949   624.1         

lsmeans(sehae.lm, pairwise ~ flight.time | sex, adjust = "tukey")
lsmeans(sehae.lm, pairwise ~ sex | flight.time, adjust = "tukey")

mmorph.0.lm <- lm(mass ~ sex*treatment, data = morph.0)
summary(mmorph.0.lm)

lsmeans(mmorph.0.lm, pairwise ~ sex | treatment, adjust = "tukey")

