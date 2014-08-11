## CREATED 11/02/2014 : Bandwidth estimation of probit model..
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

data.all<-read.dta("data/OECD.dta")

summary(data.all)
 
# Data type expressed
data.all$i_tap <- factor(data.all$i_tap)
data.all$g7_norml_water <- factor(data.all$g7_norml_water)


# Explanatory 
data.all$country <- factor(data.all$country)
data.all$b2_concern_envir <- ordered(data.all$b2_concern_envir )  
# data.all$b3_envconcern_index <- factor(data.all$b3_envconcern_index) # <- continuous 
data.all$b4_vote <- factor(data.all$b4_vote)
data.all$b5_envir <- factor(data.all$b5_envir)
#data.all$b06_env_attid_indx <- factor(data.all$b06_env_attid_indx)   # <- continuous 
data.all$b06_env_att2 <- ordered(data.all$b06_env_att2)
data.all$b06_env_att6 <- ordered(data.all$b06_env_att6)

data.all$b11_knowledge <- factor(data.all$b11_knowledge)  
#data.all$b09_policy_indx <- factor(data.all$b09_policy_indx)  # je ne sais pas ce que c'est (continuous)
#data.all$b10_policy_indx <- factor(data.all$b10_policy_indx)  # je ne sais pas ce que c'est (continuous)

# On a le choix pour eux !!
#data.all$g8_wtrstfcn_health <- ordered(data.all$g8_wtrstfcn_health)
#data.all$g8_wtrstfcn_taste <- ordered(data.all$g8_wtrstfcn_taste)


data.all$i_male <- factor(data.all$i_male)
#  data.all$a2_age<- factor(data.all$a2_age)   # <- continuous 
data.all$i_under18 <- factor()
data.all$a3_hhsize <- ordered(data.all$a3_hhsize)  
data.all$i_posthighsch <- factor(data.all$i_posthighsch)
data.all$i_lowincome <- factor()
data.all$i_town <- factor(data.all$i_town)
data.all$i_car <- factor(data.all$i_car)
data.all$isatis_health <- factor(data.all$isatis_health)
data.all$b08_locenv_water <- ordered(data.all$b08_locenv_water)
data.all$b03_lessconcernwaste <- ordered(data.all$b03_lessconcernwaste)
data.all$a6_empl <- factor(data.all$a6_empl)
data.all$i_empl <- factor(data.all$i_empl)
data.all$i_owner <- factor(data.all$i_owner)
data.all$a11_residtype <- factor(data.all$a11_residtype)
data.all$a12_residsize <- ordered(data.all$a12_residsize)
#  data.all$a13_residarea <- factor(data.all$a13_residarea)   # cf i_town

data.all$a14_residten <- ordered(data.all$a14_residten)
data.all$unitcharge <- factor(data.all$unitcharge)


## Probit Model (idem stata le 11/02/2014)
model.probit <- glm(i_tap ~ country,
                    family = binomial(link = probit),
                    data = data.all)

summary(model.probit)

## Second model (parcionious )
model.probitP <- glm(i_tap ~ country
                     + a2_age + i_empl + i_owner+ unitcharge + a12_residsize + a13_residarea 
                     + b3_envconcern_index + b06_env_att6 + b10_policy_indx 
                     + g8_wtrstfcn_health + g8_wtrstfcn_taste, 
                    family = binomial(link = probit),
                    data = data.all)

summary(model.probitP) # OK with Stata

## Modele Celine (12/02/1014) )

model.Celine <- glm(i_tap ~ a2_age + i_under18  + i_posthighsch + lowincome 
                     + i_town + i_car + isatis_health
                     +  b08_locenv_water + b3_envconcern_index  + b03_lessconcernwaste
                     + country, 
                     family = binomial(link = probit),
                     data = data.all)

summary(model.Celine) # OK with Stata



## NP Model ESTIMATION on SAMPLE 
# TRIAL with with some limitations (ftol, tol, etc...)
set.seed(2512)
N <-100#    subsample
Nobs<-nrow(data.all)
ii <- sample(seq(1:Nobs),replace=FALSE)
data.train <- data.all[ii[1:N],]

attach(data.train)

## FIRST MODEL (complete) 
tic  <- proc.time()

bwC <- npcdensbw(i_tap ~ country
                + b2_concern_envir + b3_envconcern_index + b4_vote + b5_envir +b06_env_attid_indx 
                + b11_knowledge + b09_policy_indx + b10_policy_indx + g8_wtrstfcn_health 
                + g8_wtrstfcn_taste + b06_env_att2+ b06_env_att6 + locenv_index_test
                + i_male + a2_age + a3_hhsize + i_posthighsch + a6_empl+ i_empl 
                + i_owner + a11_residtype + a12_residsize + a13_residarea 
                + i_town+ a14_residten, 
                bandwidth.compute = TRUE,
                tol = 0.5,
                ftol = 0.1,
                itmax = 20, 
                data=data.train)

tac  <- proc.time()
duree <-tac-tic

save(bwC, duree,  file = "bwC.RData")
summary(bwC)
plot(bwC)

model.npC<-npconmode(bws=bwC)
summary(model.npC)

## Second model (Parcionouous, idem probitP)
tic  <- proc.time()

bwP <- npcdensbw(i_tap ~ country
                 + a2_age + i_empl + i_owner+ unitcharge + a12_residsize + a13_residarea 
                 + b3_envconcern_index + b06_env_att6 + b10_policy_indx 
                 + g8_wtrstfcn_health + g8_wtrstfcn_taste, 
                 bandwidth.compute = TRUE,
                 tol = 0.5,
                 ftol = 0.1,
                 itmax = 20, 
                 data=data.train)

tac  <- proc.time()
duree <-tac-tic

save(bwP, duree,  file = "bwP.RData")
summary(bwP)
plot(bwP)

model.np<-npconmode(bws=bwP)
summary(model.np)

# New (après ré-examen sur 1000 points )
tic  <- proc.time()

bwP <- npcdensbw(i_tap ~ country
                 + a2_age + i_empl + i_owner+ unitcharge + a12_residsize + a13_residarea 
                 + b3_envconcern_index + b06_env_att6 + b10_policy_indx 
                 + g8_wtrstfcn_health + g8_wtrstfcn_taste, 
                 bandwidth.compute = TRUE,
                 tol = 0.5,
                 ftol = 0.1,
                 itmax = 20, 
                 data=data.train)

tac  <- proc.time()
duree <-tac-tic

save(bwP, duree,  file = "bwP.RData")
summary(bwP)
plot(bwP)

detach(data.train)

attach(data.all)
tic  <- proc.time()

bw.celine <- npcdensbw(i_tap ~ a2_age + i_under18  + i_posthighsch + lowincome 
                       + i_town + i_car + isatis_health
                       +  b08_locenv_water + b3_envconcern_index  + b03_lessconcernwaste
                       + country,
                 bandwidth.compute = TRUE,
                 tol = 0.5,
                 ftol = 0.1,
                 itmax = 20, 
                 data=data.all)

tac  <- proc.time()
duree <-tac-tic

save(bw.celine, duree,  file = "bw.celine.RData")
summary(bw.celine)
plot(bw.celine)


model.npCeline<-npconmode(bws=bw.celine)
summary(model.npCeline)
# Quelques graphiques

scatterplotMatrix(~b3_envconcern_index + b06_env_attid_indx + b09_policy_indx |country, 
                  data=data.all, subset=(country == "AUSTRALIA"),
                  plot.points=TRUE,  main="Concern variables", legend.plot=FALSE)

