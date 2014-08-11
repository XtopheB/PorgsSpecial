## CREATED 11/02/2014 : Bandwidth estimation of probit model..
# 17/02/2014 :  New file with OECDFinal dataset...
#

## First, we clean up objects in memory that might be read from disk
## from any previous runs.

rm(list=ls())

setwd("D:/progs/Celine/Water")   

## libraries
## Load the np library, then turn off i/o from bandwidth selection
## etc.

library(np)
library(foreign)

options(np.messages=FALSE)


## Load the data.

## ORIGINAL DATA :  the ones where diploma  varaible is "diplo" and has 4 mod. (0ctober 2008)
## Variable dipp is replaced by diplo

data.all<-read.dta("data/OECDFinal.dta")

summary(data.all)
 
# Data type expressed (and used)
data.all$i_tap <- factor(data.all$i_tap)
data.all$i_under18 <- factor(data.all$i_under18) 
data.all$i_posthighsch <- factor(data.all$i_posthighsch)
data.all$lowincome <- factor(data.all$lowincome)
data.all$i_town <- factor(data.all$i_town)
data.all$i_car <- factor(data.all$i_car)
data.all$isatis_health <- factor(data.all$isatis_health)
data.all$b08_locenv_water <- ordered(data.all$b08_locenv_water)
#data.all$b3_envconcern_index <- numeric(data.all$b3_envconcern_index) # <- continuous 
data.all$b03_lessconcernwaste <- ordered(data.all$b03_lessconcernwaste)
data.all$country <- factor(data.all$country)
data.all$unitcharge <- factor(data.all$unitcharge)
#data.all$b10_policy_indx <-numeric(data.all$b10_policy_indx)  # <- continuous 

# NOT USED (but declared) 
# 
# data.all$b4_vote <- factor(data.all$b4_vote)
# data.all$b5_envir <- factor(data.all$b5_envir)
# #data.all$b06_env_attid_indx <- factor(data.all$b06_env_attid_indx)   # <- continuous 
# data.all$b06_env_att2 <- ordered(data.all$b06_env_att2)
# data.all$b06_env_att6 <- ordered(data.all$b06_env_att6)
# data.all$b11_knowledge <- factor(data.all$b11_knowledge)  
# #data.all$b09_policy_indx <- factor(data.all$b09_policy_indx)  # je ne sais pas ce que c'est (continuous)
# 
# 
# # On a le choix pour eux !!
# #data.all$g8_wtrstfcn_health <- ordered(data.all$g8_wtrstfcn_health)
# #data.all$g8_wtrstfcn_taste <- ordered(data.all$g8_wtrstfcn_taste)
# 
# ## Individual 
# data.all$i_male <- factor(data.all$i_male)
# data.all$a3_hhsize <- ordered(data.all$a3_hhsize) 
# #  data.all$a2_age<- factor(data.all$a2_age)   # <- continuous 
# data.all$a6_empl <- factor(data.all$a6_empl)
# data.all$i_empl <- factor(data.all$i_empl)
# data.all$i_owner <- factor(data.all$i_owner)
# data.all$a11_residtype <- factor(data.all$a11_residtype)
# data.all$a12_residsize <- ordered(data.all$a12_residsize)
# #  data.all$a13_residarea <- factor(data.all$a13_residarea)   # cf i_town
# data.all$a14_residten <- ordered(data.all$a14_residten)
# 
# 
## Probit Model (compare with stata on 19/02/2014)

model.probit <- glm(i_tap ~ a2_age + i_under18  + i_posthighsch + lowincome 
                    + i_town + i_car + isatis_health
                    +  as.numeric(b08_locenv_water) + as.numeric(b3_envconcern_index)  
                    + as.numeric(b03_lessconcernwaste)
                    + country, 
                    family = binomial(link = probit),
                    data = data.all)

summary(model.probit) # OK Nickel with Stata   !!!

# Model with 7 Countries  
data.7 <- subset(data.all, country !="CHILE" & country !="ISRAEL" & country !="JAPAN" & country !="KOREA" )
model.probit7 <- glm(i_tap ~ a2_age + i_under18  + i_posthighsch + lowincome 
                    + i_town + i_car + isatis_health
                    +  as.numeric(b08_locenv_water) + as.numeric(b3_envconcern_index)  
                    + as.numeric(b03_lessconcernwaste)
                    + b10_policy_indx
                    + country, 
                    family = binomial(link = probit),
                    data = data.7)

summary(model.probit7) # OK Nickel with Stata (19/02/2014)  !!! !!


## ------ NP Model ESTIMATION (19/02/2014)  Sample of 7 countries 

# TRIAL SET  with with some limitations (ftol, tol, etc...)
set.seed(2512)
N <-1000#    subsample
Nobs<-nrow(data.7)
ii <- sample(seq(1:Nobs),replace=FALSE)
data.train <- data.7[ii[1:N],]

#attach(data.7)
tic  <- proc.time()
# Model equivalent to Probit Celine (7 countries)
bw.celine7 <- npcdensbw(i_tap ~ a2_age + i_under18  + i_posthighsch + lowincome 
                        + i_town + i_car + isatis_health
                        +  as.numeric(b08_locenv_water) + as.numeric(b3_envconcern_index)  
                        + as.numeric(b03_lessconcernwaste)
                        + b10_policy_indx
                        + country,
                 bandwidth.compute = TRUE,
                 tol = 0.5,
                 ftol = 0.1,
                 itmax = 20, 
                 data=data.train)

tac  <- proc.time()
duree <-tac-tic
#save(bw.celine7, duree,  file = "bw.celine7.RData")

summary(bw.celine7)

plot(bw.celine7)

npplot(bw.celine7, common.scale=FALSE)
npplot(bw.celine7, common.scale=TRUE)  # Pour voir les effets marginaux sur la  probailité conditionnelle

npplot(bw.celine7, plot.errors.method ="asymptotic", common.scale=FALSE)


model.npCeline7<-npconmode(bws=bw.celine7)
summary(model.npCeline7)




# Quelques graphiques

scatterplotMatrix(~b3_envconcern_index + b06_env_attid_indx + b09_policy_indx |country, 
                  data=data.all, subset=(country == "AUSTRALIA"),
                  plot.points=TRUE,  main="Concern variables", legend.plot=FALSE)

