
/* Programme pour la REVISION du papier */
/* 08/04/2015 : Issu de SpecialEstimation3.do   */ 

clear
*global root "d:/progs/celine/water"
global root "C:/Chris/progs/celine/water"
cd $root
use "$root\data\table for ssreg.dta", clear
count

capture log close
log using ResultsModelsBoot999, replace

 
gen i_aus = (country == 1)
gen i_can = (country == 2)
gen i_fra = (country == 4)
gen i_net = (country == 8)
gen i_swe = (country == 10)

drop if country == 8
drop if country == 10
count
/* Programme d'estimation */

egen mprix_2008 = mean(prix_2008)
gen Special = -(prix_2008 - mprix_2008)

global exog "i_under18 log_income i_town i_car b08_locenv_water a2_age i_can i_fra"
global endog "isatis_health"
global instrument "itap_2008 iconcernwatpol_2008"


/***************************/
/* Resultats du papier   */
set more off
discard
sspecialreg i_tap Special, exog($exog) endog($endog) iv($instrument) /*
*/ hetero hetv(i_under18 log_income i_town b08_locenv_water) /*
*/ kdens trim(2.5) 
eststo modelPapier

/* Robustness checks on the kernel   */
/*  epanechnikov (used by Baum), epan2 , biweight, triweight, cosine, gaussian, parzen, rectangle or triangle*/


sspecialregK i_tap Special, exog($exog) endog($endog) iv($instrument) /*
*/ hetero hetv(i_under18 log_income i_town b08_locenv_water) /*
*/ kdens ker(biweight) trim(2.5) 

eststo modelbi

sspecialregK i_tap Special, exog($exog) endog($endog) iv($instrument) /*
*/ hetero hetv(i_under18 log_income i_town b08_locenv_water) /*
*/ kdens ker(triweight) trim(2.5) 

eststo modeltri

sspecialregK i_tap Special, exog($exog) endog($endog) iv($instrument) /*
*/ hetero hetv(i_under18 log_income i_town b08_locenv_water) /*
*/ kdens ker(gaussian) trim(2.5) 

eststo modelgaus

/* Pour sortie Rtf   */
esttab  using RobustK.rtf, se brackets stat(trim N) obslast replace  noeqlines compress star( * 0.10 ** 0.05  *** 0.01)

/* Recomputing TABLE 3 with several bandwidths  for model with TRIM 2.5 and no winsor  */
/* We use here part of the program LoopSpecial2.do   */

set more off
/* to recover the results in a nice way with Monspecialreg2.ado */
capture drop Meff
capture eststo drop Md*

matrix Meff = J(11,1,.)
*  - KDENS ESTIMATOR procedure 
capture log close
*log using "Logs\ResultsRevisionTrim25", replace
*set linesize 88 
 
/* Avec fenêtre choisie  automatiquement  (Silverman)  */ 
*quietly Monsspecialreg2 i_tap Special, exog($exog) endog($endog) iv($instrument) hetero kdens  trim(`t') `W'
*di in red " --- Trim = `t' ; Fenetre de Silverman :   `e(Band)'  ---" 


/* On lance aussi une version avec ordered choice : pour la sortie on tague cela Band= 999 */
/* if kdens is not specified then ordered choice is the default */

quietly Monsspecialreg2 i_tap Special, exog($exog) endog($endog) iv($instrument)  ///
						hetero hetv(i_under18 log_income i_town b08_locenv_water) ///
						trim(2.5)  
count
count if e(sample)						
/* We set TRIM = 2.5 and loop over a bunch of bandwidth  */
/*  Silverman (0.23) + 3 choix de fenêtres  et CV (1.004)  */

foreach  b in  0.1 .2317966299740502 0.30 0.5 1.004 {
*foreach  b in  0.40 0.5  0.60 0.70 0.80 0.90 {

di in red " >>>>   band = `b'  ;   trim = `t'   <<<< "

/* On utilise une version modifiée de Monspecialreg2.do qui stocke les résulats dans Md */
Monsspecialreg3 i_tap Special, exog($exog) endog($endog) iv($instrument) ///
								hetero hetv(i_under18 log_income i_town b08_locenv_water) ///
								kdens band(`b') trim(2.5) bs bsreps(399)
 count if e(sample)
}


/* Recuperation des estimations  */

esttab Md*, stat( N band trim obstrim) se star(  * 0.10 ** 0.05  *** 0.01)
quietly esttab Md* using "logs/ResultsRevisedTrim25.tex", se stat(mfx mfxse band trim  N obstrim ) ///
 obslast replace star(  * 0.10 ** 0.05  *** 0.01)

quietly esttab Md* using "logs/ResultsRevisedTrim25Mfx04-09.rtf", se stat(mfx mfxse band trim  N obstrim ) obslast replace ///
 star(  * 0.10 ** 0.05  *** 0.01) se(3)  b(3) ///
 order(isatis_health a2_age i_under18 log_income i_town  i_car  b08_locenv_water i_can  i_fra) nocons

/* forme compacte  */
estout Md*, stat(mfx mfxse band trim  N obstrim )

capture log close


/************************************/
/*  In and OUT Sample Predictions   */
/* now we neeed to keep the sample used after the bootstrap computation of Marginal Effects  */


/* using Celine's file  would give the same sample (2913 obs) */
/* but would need to redefine the variables, etc.. */
* use $root\data\tableforssreg2.dta, clear

sspecialreg i_tap Special, exog($exog) endog($endog) iv($instrument) hetero kdens trim(2.5)
sspecialreg i_tap Special, exog($exog) endog($endog) iv($instrument) hetero hetv(i_under18 log_income i_town b08_locenv_water) /*
*/ kdens trim(2.5) /*winsor*/ bs bsreps(399)

keep if e(sample)

save "$root\data\SubSampleAfterMfxBoot.dta", replace

capture log close
log using "Logs\In-OutPredRevised-part2", replace
use "$root\data\SubSampleAfterMfxBoot.dta", clear
*des
count
set more off

/*----------------------------*/
/* IN-SAMPLE prediction   */
/*----------------------------*/
* PROBIT SIMPLE

xi: probit i_tap a2_age prix_2008 i_under18 log_income i_town i_car isatis_health b08_locenv_water i_can i_fra  
predict Pprobit
gen I_Pprobit = (Pprobit > 0.5) 


* BIPROBIT

quietly xi: biprobit (i_tap a2_age prix_2008 i_under18 log_income i_town i_car isatis_health b08_locenv_water i_can i_fra ) /*
*/ (isatis_health a2_age prix_2008 i_under18 log_income i_town i_car b08_locenv_water i_can i_fra  itap_2008 iconcernwatpol_2008)

/* pcond1 is the conditional probability of success for equation 1 */
predict Pbiprobit, pmarg1
gen I_Pbiprobit = (Pbiprobit > 0.5)

* IVPROBIT 

quietly xi: ivprobit i_tap a2_age prix_2008 i_under18 log_income i_town i_car b08_locenv_water i_can i_fra  /*
*/ (isatis_health = itap_2008 iconcernwatpol_2008)
predict Pivprobit, pr
gen I_Pivprobit = (Pivprobit > 0.5)

*Special regressor with outcome uhat fuhat  !!!  MODIFIED HERE no more winsor (13/04/2015)

quietly sspecialregPred i_tap Special, exog($exog) endog($endog) iv($instrument) /*
*/  hetero hetv(i_under18 log_income i_town b08_locenv_water) /*
*/ kdens trim(2.5) 

* Version used for our computation 
/* quietly sspecialreg i_tap Special, exog($exog) endog($endog) iv($instrument) /*
*/  hetero hetv(i_under18 log_income i_town b08_locenv_water) /*
*/ kdens trim(2.5) winsor
*/

predict Dhat, xb
gen I_Pspeprobit = (Dhat + Special > 0)


*Predicted probabilities (from Lewbel's JOE-2000 and Binsel2012.zip file (SimpleSpecial.ado)
gen Dxb = Dhat + Special /* use Lewbel's notation */
qui gen Istand = (Dxb<0)
qui gen forSigma = 2*Dxb*(1-i_tap-Istand)/fUhat
qui sum forSigma
egen Sigma = mean(forSigma)
gen SigmaStd = sqrt(abs(Sigma))

qui gen probPred = normal(Dxb/SigmaStd)

/***********************************/
/* RESULTS as in TABLE 2 (updated)  */
/**********  Probit ******************/
di "--- Probit ----"

*Confusion matrix 
tab I_Pprobit i_tap
* Computing CCR 
count if I_Pprobit ==1 & i_tap==1 | I_Pprobit ==0 & i_tap==0
local ProCCR = r(N)/(_N)*100 
di  in red" ---> Correct Pred for Probit = `ProCCR' "

* Sensitivity (true positive rate)  TP/P = TP/(TP+FN)
count if I_Pprobit ==1 & i_tap==1 
local TP = r(N)

count if i_tap==1 
local P = r(N)
local ProSens = (`TP' / `P')*100 
di in red " ---> Sensitivity for Probit = `ProSens' "

* computing Specificty = TN/N = TN/(TN+FP)
count if I_Pprobit == 0 & i_tap==0 
local TN = r(N)

count if i_tap==0 
local NN = r(N)
local ProSpe = (`TN' / `NN')*100 
di in red  " ---> Specificity for Probit = `ProSpe' "

/********  BIProbit ********/
di "--- BIProbit ----"

*Confusion matrix 
tab I_Pbiprobit i_tap   
* Computing CCR 
count if I_Pbiprobit ==1 & i_tap==1 | I_Pbiprobit ==0 & i_tap==0
local biproCCR = r(N)/(_N)*100 
di  in red" ---> Correct Pred for biprobit = `biproCCR' "

* Sensitivity (true positive rate)  TP/P = TP/(TP+FN)
count if I_Pbiprobit ==1 & i_tap==1 
local TP = r(N)

local biproSens = (`TP' / `P')*100 
di in red " ---> Sensitivity for biprobit = `biproSens' "

* computing Specificty = TN/N = TN/(TN+FP)
count if I_Pbiprobit == 0 & i_tap==0 
local TN = r(N)

local biproSpe = (`TN' / `NN')*100 
di in red  " ---> Specificity for biprobit = `biproSpe' "

/********  ivprobit ********/
di "--- ivprobit ----"

*Confusion matrix 
tab I_Pivprobit i_tap
* Computing CCR 
count if I_Pivprobit ==1 & i_tap==1 | I_Pivprobit ==0 & i_tap==0
local ivproCCR = r(N)/(_N)*100 
di  in red" ---> Correct Pred for ivprobit = `ivproCCR' "

* Sensitivity (true positive rate)  TP/P = TP/(TP+FN)
count if I_Pivprobit ==1 & i_tap==1 
local TP = r(N)

local ivproSens = (`TP' / `P')*100 
di in red " ---> Sensitivity for ivprobit = `ivproSens' "

* computing Specificty = TN/N = TN/(TN+FP)
count if I_Pivprobit == 0 & i_tap==0 
local TN = r(N)

local ivproSpe = (`TN' / `NN')*100 
di in red  " ---> Specificity for ivprobit = `ivproSpe' "


/********  Special reg ********/
di "--- Specialreg----"

*Confusion matrix 
tab I_Pspeprobit i_tap
* Computing CCR 
count if I_Pspeprobit ==1 & i_tap==1 | I_Pspeprobit ==0 & i_tap==0
local speproCCR = r(N)/(_N)*100 
di  in red" ---> Correct Pred for speprobit = `speproCCR' "

* Sensitivity (true positive rate)  TP/P = TP/(TP+FN)
count if I_Pspeprobit ==1 & i_tap==1 
local TP = r(N)

local speproSens = (`TP' / `P')*100 
di in red " ---> Specificity for speprobit = `speproSens' "

* computing Specificty = TN/N = TN/(TN+FP)
count if I_Pspeprobit == 0 & i_tap==0 
local TN = r(N)

local speproSpe = (`TN' / `NN')*100 
di in red  " ---> Sensitivity for speprobit = `speproSpe' 


/*----------------------------*/
/* OUT-OF-SAMPLE Predictions */
/*----------------------------*/

*global Nsim= 1000 /* Done in two parts  */
global Nsim= 47

count
* Number of observations is now of  2635 
global Ntrain = 2240 /* 80 % = 2108;  85% =2240  */ 

global Nout = r(N) - $Ntrain 


/* Out of sample predictions */
/* Variables with CCR are suffxed by 4 */
matrix define ProPred = J($Nsim,4,0)
matrix define SpePred = J($Nsim,4,0)
matrix define BiPred = J($Nsim,4,0)
matrix define IvPred = J($Nsim,4,0)


set more off
*set seed 2512 <- first simulation with no convergence (biprobit) at sample 953
set seed 1234

forvalues i=1/$Nsim {
	di " ----------- Tirage No `i'  -------"

	capture drop P* I* 
	capture drop V 
	capture drop Dhat 
	capture drop random
	capture drop trainset
	/* trainset definition */
	gen random = runiform()  /* from a uniform distribution on [0,1)*/
	sort random
	generate trainset = (_n <= $Ntrain)  
	
	/****** PROBIT *******/
	/* Train set estimation */
	 di " --- PROBIT "
	quietly xi: probit i_tap a2_age prix_2008 i_under18 log_income i_town i_car isatis_health ///
			b08_locenv_water i_can i_fra if trainset == 1

	/* Out-of sample prediction  */
	
	predict PprobitO if trainset == 0
	gen I_PprobitO = 1 if PprobitO > 0.5 & trainset == 0
	replace I_PprobitO = 0 if PprobitO <= 0.5 & trainset == 0

	count if I_PprobitO ==0 & i_tap==0 & trainset == 0
	mat ProPred[`i',1] =r(N) 
	count if I_PprobitO ==1 & i_tap==1 & trainset == 0
	mat ProPred[`i',2] =r(N)
	count if I_PprobitO == i_tap  & trainset == 0
	mat ProPred[`i',3] =r(N)
	mat ProPred[`i',4] = (r(N)/$Nout)*100 
	
	
	/****** SPECIAL *******/
	/* Train set estimation */
	 di " --- SPECIAL without winsor "
	gen V = -prix_2008 
	quietly sspecialreg i_tap V  if trainset == 1, exog($exog) endog($endog) iv($instrument) ///
		hetero hetv(i_under18 log_income i_town b08_locenv_water) ///
		kdens trim(2.5) 
	/* Coefs  */ 
	*matrix SpePred[`i',5]  =  _b[isatis_health]
	*matrix SpePred[`i',6]  =  _b[log_income]
	
	
	/* Predictions */
	predict Dhat if trainset == 0, xb
	qui sum Dhat 
	*matrix SpePred[`i',7]  =  r(mean)

	* Computing the right V 
	qui sum V if trainset == 0
	replace V = V - r(mean) if trainset == 0
	
	* Computing decision Xb+V
	
	gen I_Pspeprobit = 1 if (Dhat + V > 0) & trainset == 0
	replace I_Pspeprobit = 0 if (Dhat + V <= 0) & trainset == 0
	qui gen foo = Dhat+V 
	qui sum foo 
	*matrix SpePred[`i',8]  =  r(mean)
	qui drop foo
	/* Out-of sample prediction  */

	count if I_Pspeprobit ==0 & i_tap==0 & trainset == 0
	mat SpePred[`i',1] =r(N) 
	count if I_Pspeprobit ==1 & i_tap==1 & trainset == 0
	mat SpePred[`i',2] =r(N)
	count if I_Pspeprobit == i_tap & trainset == 0
	mat SpePred[`i',3] = r(N)
	mat SpePred[`i',4] = (r(N)/$Nout)*100 
		
	* BIPROBIT
	 di " --- BIPROBIT "
	quietly xi: biprobit (i_tap a2_age prix_2008 i_under18 log_income i_town i_car isatis_health b08_locenv_water i_can i_fra ) /*
	*/ (isatis_health a2_age prix_2008 i_under18 log_income i_town i_car b08_locenv_water i_can i_fra  itap_2008 iconcernwatpol_2008) ///
		if trainset == 1

	/* pcond1 is the conditional probability of success for equation 1 */
	predict Pbiprobit, pmarg1
	gen I_Pbiprobit = (Pbiprobit > 0.5) & trainset == 0
	
	/* Out-of sample prediction  */
	count if I_Pbiprobit ==0 & i_tap==0 & trainset == 0
	mat BiPred[`i',1] =r(N) 
	count if I_Pbiprobit ==1 & i_tap==1 & trainset == 0
	mat BiPred[`i',2] =r(N)
	count if I_Pbiprobit == i_tap & trainset == 0
	mat BiPred[`i',3] = r(N)
	mat BiPred[`i',4] = (r(N)/$Nout)*100 

	* IVPROBIT 
	 di " --- IVPROBIT "
	quietly xi: ivprobit i_tap a2_age prix_2008 i_under18 log_income i_town i_car b08_locenv_water i_can i_fra  /*
	*/ (isatis_health = itap_2008 iconcernwatpol_2008) if trainset == 1
	predict Pivprobit, pr
	gen I_Pivprobit = (Pivprobit > 0.5) & trainset == 0
	
	/* Out-of sample prediction  */
	count if I_Pivprobit ==0 & i_tap==0 & trainset == 0
	mat IvPred[`i',1] = r(N) 
	count if I_Pivprobit ==1 & i_tap==1 & trainset == 0
	mat IvPred[`i',2] = r(N)
	count if I_Pivprobit == i_tap  & trainset == 0
	mat IvPred[`i',3] = r(N)
	mat IvPred[`i',4] = (r(N)/$Nout)*100 

 }

capture drop SpePred*
capture drop ProPred*
capture drop BiPred*
capture drop IvPred*

/* transforming matrices into variables  */

svmat SpePred
svmat ProPred
svmat BiPred
svmat IvPred

/* Variables with CCR are suffxed by 4 */
sum *Pred4
capture log close

/* We were obliged to do the simulation in two rows  */
preserve
keep *Pred4
keep if SpePred4 !=.
keep if BiPred4 !=0
count  /* 953 obs with seed = 2512 */
save "$root\progs\Results\PredIn-Out953.dta"

preserve
keep *Pred4
keep if SpePred4 !=.
keep if BiPred4 !=0
count /* 47obs with seed = 1234 */

save "$root\progs\Results\PredIn-Out47.dta"


use "$root\progs\Results\PredIn-Out47.dta", clear
append using "$root\progs\Results\PredIn-Out953.dta"

sum


		
		
		
		
		



 