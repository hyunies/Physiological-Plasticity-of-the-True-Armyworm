## Clear R brain
rm(list= ls())

## Import libraries 
library(dplyr)
library(ggplot2)
library(car)
library(lsmeans)

metabol <- read.csv("School Notes/R studio/fixedmetabolicrate2022.csv")


################################################################################
##### Flight & Resting means


flmetabol.sum1 <- metabol %>%
  group_by(state, sex, treatment) %>%
  summarise(mean.met.rate = mean(met.rate, na.rm = TRUE),
            sd.met.rate = sd(met.rate, na.rm = TRUE)) 

ggplot(data = flmetabol.sum1, aes(x = state, y = mean.met.rate, 
                             colour = sex, shape = treatment, linetype = treatment, 
                             ymax = mean.met.rate+sd.met.rate, 
                             ymin = mean.met.rate-sd.met.rate)) + 
  geom_point(size = 8) + 
  geom_line() + 
  geom_errorbar(width = 0.2) + 
  theme_bw() + 
  ylab(expression(paste("Metabolic Rate", sep = ""))) + 
  xlab("State")

################################################################################
##### STATS
metabol.lm <- lm(met.rate ~ sex*treatment, data = metabol)
summary(aov(metabol.lm))
# Df  Sum Sq Mean Sq F value Pr(>F)  
# sex            1   14672   14672   0.358 0.5519  
# treatment      1   75241   75241   1.837 0.1807  
# sex:treatment  1  119945  119945   2.929 0.0925 .
# Residuals     56 2293254   40951                 
# ---

summary(tmeggs.lm)

summary(aov(tmeggs.lm))



metabol.r <- filter(metabol, state == 1)

metabol.r.lm <- lm(met.rate ~ sex*treatment, data = metabol.r)
summary(metabol.r.lm)

summary(aov(metabol.r.lm))

#               Df Sum Sq Mean Sq F value Pr(>F)
# sex            1   4.66   4.662   0.195  0.671
# treatment      1   1.24   1.243   0.052  0.825
# sex:treatment  1   7.43   7.425   0.310  0.593
# Residuals      8 191.42  23.928   
##### NO SIG DIFF IN RESTING

######################################

metabol.f <- filter(metabol, state == 2)

metabol.f.lm <- lm(met.rate ~ sex*treatment, data = metabol.f)
summary(metabol.f.lm)

summary(aov(metabol.f.lm))
#               Df Sum Sq Mean Sq F value Pr(>F)  
# sex            1  18049   18049   0.801 0.3756  
# treatment      1  94394   94394   4.189 0.0467 *
#   sex:treatment  1 148878  148878   6.608 0.0136 *
#   Residuals     44 991381   22531                 
##### SIG DIFF IN FLIGHT


## TUKEY FOR FLIGHT
metabol.f.lm2 <- lm(met.rate ~ sex*treatment, data = metabol.f) %>% aov()

TukeyHSD(metabol.f.lm2)
# $sex
# diff       lwr      upr     p adj
# M-F 38.78282 -48.54599 126.1116 0.3756429
# 
# $treatment
# diff       lwr       upr     p adj
# SD-LD -88.6914 -176.0202 -1.362583 0.0466783
# 
# $`sex:treatment`
# diff        lwr       upr     p adj
# M:LD-F:LD  150.16731  -13.45047 313.78508 0.0823713
# F:SD-F:LD   22.69309 -140.92469 186.31086 0.9824353
# M:SD-F:LD  -49.90858 -213.52635 113.70920 0.8473198
# F:SD-M:LD -127.47422 -291.09200  36.14355 0.1755028
##############
# M:SD-M:LD -200.07588 -363.69366 -36.45811 0.0110113
##############
# M:SD-F:SD  -72.60166 -236.21944  91.01611 0.6395510

## NO DIFFERENCE BETWEEN SEXES, DIFFERENCES BETWEEN LD SD
## Male LD SD showed difference