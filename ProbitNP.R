## CREATED 11/02/2014 : Bandwidth estimation of probit model..
# 17/02/2014 :  New file with OECDFinal dataset...
#

## First, we clean up objects in memory that might be read from disk
## from any previous runs.

rm(list=ls())

setwd("D:/progs/Celine/Water/progs")   

## libraries
## Load the np library, then turn off i/o from bandwidth selection
## etc.

library(np)
library(foreign)

options(np.messages=FALSE)


## Load the data.

## ORIGINAL DATA :  the ones where diploma  varaible is "diplo" and has 4 mod. (0ctober 2008)
## Variable dipp is replaced by diplo

data.all<-read.dta("../data/OECDFinal.dta")

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
data.all$b3_envconcern_index <- as.numeric(data.all$b3_envconcern_index) # <- continuous 
data.all$b03_lessconcernwaste <- ordered(data.all$b03_lessconcernwaste)
data.all$country <- factor(data.all$country)
data.all$unitcharge <- factor(data.all$unitcharge)
data.all$b10_policy_indx <-as.numeric(data.all$b10_policy_indx)  # <- continuous 

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
#set.seed(2512)
#N <-500#    subsample
#Nobs<-nrow(data.7)

#data.all$b08_locenv_water <- as.numeric(data.all$b08_locenv_water)
#data.all$b3_envconcern_index <- as.numeric(data.all$b3_envconcern_index)


#ii <- sample(seq(1:Nobs),replace=FALSE)
#data.train <- data.7[ii[1:N],]

#attach(data.7)
tic  <- proc.time()
# Model equivalent to Probit Celine (7 countries)


bw.celine7 <- npcdensbw(i_tap ~ a2_age + i_under18  + i_posthighsch + lowincome 
                        + i_town + i_car + isatis_health
                        +  b08_locenv_water + b3_envconcern_index 
                        + b03_lessconcernwaste
                        + b10_policy_indx
                        + country,
                 bandwidth.compute = TRUE,
                 tol = 0.5,
                 ftol = 0.1,
                 itmax = 20, 
                 data=data.7)

tac  <- proc.time()
duree <-(tac-tic)/3600
duree

save(bw.celine7, duree,  file = "bw.celine7.RData")
#load(file = "bw.celine7.RData")


model.npCeline7<-npconmode(bws=bw.celine7)
# Archivage 
#save(bw.celine7,model.npCeline7, data.7, file = "Results/bw.celine7-27.02.RData")

summary(bw.celine7)
plot(bw.celine7)

npplot(bw.celine7, common.scale=FALSE)
npplot(bw.celine7, common.scale=TRUE)  # Pour voir les effets marginaux sur la  probailité conditionnelle

npplot(bw.celine7, plot.errors.method ="asymptotic", common.scale=FALSE)



summary(model.npCeline7)


# Version old style 
cm <-table(data.7$i_tap, ifelse(fitted(model.npCeline7) >0.5,1,0))
cm

# Quelques graphiques

scatterplotMatrix(~b3_envconcern_index + b06_env_attid_indx + b09_policy_indx |country, 
                  data=data.all, subset=(country == "AUSTRALIA"),
                  plot.points=TRUE,  main="Concern variables", legend.plot=FALSE)


### Test de sorties 

summary(model.npCeline7)
plot(bw.celine7)
hist(model.npCeline7$condens)
rug(model.npCeline7$condens)

attach(data.7)
## Graphique unidimentionnel avec comme X = b08_locenv_water
## Points de calcul
ncalc<-50
# Choix du X et generation sequence des points entre quantiles 10% et 90% ! 
 
b3_envconcern_index.calc <- seq(quantile(b3_envconcern_index,.1, na.rm=TRUE),
                                quantile(b3_envconcern_index,.9, na.rm=TRUE), length=ncalc)

# Choix des references
a2_age.ref <- quantile(a2_age, .5, na.rm=TRUE)
i_under18.ref <- c(1)
i_posthighsch.ref <- c(1)
lowincome.ref <- c(0)
i_town.ref <- c(0)
i_car.ref <- c(1)
isatis_health.ref <- c(1)
b08_locenv_water.ref <- "Unsatisfied"   #  "Satisfied",  "Very satisfied"
#b3_envconcern_index.ref <- quantile(b3_envconcern_index, .5, na.rm=TRUE)
b03_lessconcernwaste.ref <- c(1)
b10_policy_indx.ref <- quantile(b10_policy_indx, .5, na.rm=TRUE)
country.ref <- "AUSTRALIA"



x.eval <- expand.grid(
  a2_age = a2_age.ref,
  i_under18 = i_under18.ref,
  i_posthighsch = i_posthighsch.ref,
  lowincome = lowincome.ref,
  i_town = i_town.ref,
  i_car = i_car.ref,
  isatis_health = isatis_health.ref,
  b08_locenv_water = b08_locenv_water.ref,
  b3_envconcern_index = b3_envconcern_index.calc,  # <--Mon x !
  b03_lessconcernwaste = b03_lessconcernwaste.ref,
  b10_policy_indx = b10_policy_indx.ref,
  country = country.ref)


data.eval <- data.frame(i_tap = factor(rep(1,nrow(x.eval))), 
                        a2_age = as.numeric(x.eval[,1]),
                        i_under18 = factor(x.eval[,2]),
                        i_posthighsch = factor(x.eval[,3]),  
                        lowincome = factor(x.eval[,4]),
                        i_town = factor(x.eval[,5]),
                        i_car = factor(x.eval[,6]),
                        isatis_health = factor(x.eval[,7]),
                        b08_locenv_water = ordered(x.eval[,8]),
                        b3_envconcern_index = as.numeric(x.eval[,9]),  
                        b03_lessconcernwaste = ordered(x.eval[,10]),
                        b10_policy_indx = as.numeric(x.eval[,11]),
                        country = factor(x.eval[,12])
                        )


## Predictions pour les 2 modèles 

cfhat <- fitted(npcdens(bws=bw.celine7, newdata=data.eval))
cflogit <- predict(model.probit7, type = "response", newdata=data.eval)

plot(b3_envconcern_index.calc, cfhat, col="red",  type="l",
     xlim=c(quantile(b3_envconcern_index.calc,.1),quantile(b3_envconcern_index.calc,.9)),
     #ylim=c(0.3,0.8) 
     ylim = c(min(quantile(cfhat, 0.1),quantile(cflogit, 0.1)), max(quantile(cfhat, 0.9),quantile(cflogit, 0.9))),
     main= paste("Discrete choice models \n Country :",country.ref,""),
     xlab="",
     ylab="Prob[i_tab=1]",
     lty=3,
     lwd=3,
     sub=paste("Age=",a2_age.ref,", under18=", i_under18.ref,", HS =",i_posthighsch.ref, 
               ", Inc.=",lowincome.ref,", Town=",i_town.ref,", car=", i_car.ref,", Health= ",isatis_health.ref," \n",
               "",b08_locenv_water.ref, ", Waste", b03_lessconcernwaste.ref,", Policy = ",round(b10_policy_indx.ref,2),"" )   
)

lines(b3_envconcern_index.calc, cflogit, col="blue", lty=2)
legend("left",legend=c("NP model", "Probit model"),
       lty=c(3,2),col=c("red","blue"),
       cex=1.2, bty="n")

#dev.off()
