#  Programme de calcul de la validation croisée dUchap 
# 11/08/2014


rm(list=ls())

setwd("D:/progs/Celine/Water")   

## libraries

library(np)
library(foreign)

options(np.messages=FALSE)

## Load the data.

data.all<-read.dta("data/FinalCV.dta")


data.work <- subset(data.all, Touse == 1 & Uchap != "NA", select = c(Uchap))
nrow(data.work)

summary(data.work$Uchap)   # OK Stata   !!
attach(data.work)
# Estimation de la fenêtre par validation croisée

bw.cv <-npudensbw(~Uchap, ckertype="epanechnikov")
summary(bw.cv)

# normal-reference rule-of-thumb (Siverman) 
bw.Sil <-npudensbw(~Uchap,bwmethod="normal-reference")
summary(bw.Sil)

npplot(bw.cv)
npplot(bw.Sil)

