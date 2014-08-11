#  Programme d'étude des propriétés des Special régresseurs 
# 16/04/2014


rm(list=ls())

setwd("D:/progs/Celine/Water/progs")   

## libraries

library(np)
library(foreign)

options(np.messages=FALSE)

## Load the data.

data.all<-read.dta("../data/DataOECD3Special.dta")


data.work <- subset(data.all, select = c(i_tap,a2_age,country, log_income, d08_kmdriven_wk_impute, totwaste, totwaste2, totwaste3 ))
nrow(data.work)


# Selection de la variable V 

#data.work$V <- data.work$ld08_kmdriven_wk_impute
data.work$V <- as.ordered(data.work$log_income)
#data.work$V <- data.work$d08_kmdriven_wk_impute
#data.work$V <- data.work$a2_age
# data.work$V <- as.ordered(data.work$a2_age) # test non concluant

#data.work$V <- data.work$totwaste


summary(data.work$V)

## Selection de l'echantillon sans NAs 

data.reg <- na.omit(subset(data.work, select = c(i_tap,V)))
names(data.reg)[2] = "log(Income)"
summary(data.reg[2])

attach(data.reg)
nrow(data.reg)

# Histograme 
hist(data.reg[[2]],freq=FALSE, main =paste(" Density",names(data.reg)[2],""))
rug(data.reg[[2]])



# Proc. Générique d'estimation de la regression
bw.V <-npregbw(formula = i_tap~data.reg[[2]], 
               regtype="ll",
               bwmethod="cv.aic")
summary(bw.V)
model.V <- npreg(bws=bw.V)


summary(model.V)
plot(model.V)


plot(model.V,
     #ylim=c(0,1.3),
     plot.errors.method="bootstrap",
     plot.errors.boot.num=199,common.scale=FALSE, 
     main =paste("Nonparametric regression,
                 V=",names(data.reg)[2],"
                 (CV bandwidth =",round(model.V$bw,4),")" ),
    xlab=paste("Special regressor V=",names(data.reg)[2],"" )
)
rug(data.reg[[2]])

points(data.reg[[2]], i_tap)


#model.age <-npreg(i_tap~a2_age,gradients=TRUE)
summary(model.age)
plot(model.age)

plot(model.age,
     plot.errors.method="bootstrap",
     plot.errors.boot.num=199,common.scale=FALSE)

legend("topleft",
       c(paste("Nonparametric regression 
              with cross validation bandwidth (bw=",round(model.age$bw,3),")" )),
       lty=c(1),
       col=c(1),
       bty="n")









# #calcul de la fenêtre :  modèle complet 
# bw.reg <- npregbw(formula = D ~ V + i_under18  + log_income
#                   + i_town + i_car + isatis_health
#                   +  b08_locenv_water +  country
#                   + itap_2008 + iconcernwatpol_2008, 
#                   bandwidth.compute = TRUE,
#                   tol = 0.5,
#                   ftol = 0.1,
#                   itmax = 20,
#                   data = data.work)


