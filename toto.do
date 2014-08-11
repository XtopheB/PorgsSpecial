
/* TEEEEESTTTTTTT*/
clear
drop _all
global root "d:/progs/celine/water"
cd $root
use "$root\data\table for ssreg.dta", clear
count


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

* Test de White
reg Special $exog $endog $instrument
estat imtest, white

predict uhat, res
gen uhat2 = uhat*uhat

reg uhat2 $exog $endog $instrument


/* *******   LOOP  *******   */
set more off
*  - KDENS ESTIMATOR procedure 

/* Fenetre optimale de silverman */

/* Choix du trim   */

local t= 2.5

Monsspecialreg2 i_tap Special, exog($exog) endog($endog) iv($instrument) hetero kdens  trim(`t')

/* Pour les noms de fichier  */
 local TrimE = int(10*`t')
 *local BandE = int(100*`band')
 
log using ResultsTrim`TrimE', replace

di in red " --- Trim = `t' ; Fenetre de Silverman :   `e(Band)'  ---" 

forvalues b = 0.1(0.1)0.4{
di in red " band = `b' , trim = `t' "
quietly Monsspecialreg2 i_tap Special, exog($exog) endog($endog) iv($instrument) hetero kdens band(`b') trim(`t') 
}


esttab Md*, stat(band trim obstrim)
estout Md*, stat(band trim obstrim)

coefplot  `r(names)',  bycoefs byopts(xrescale) ///
        coeflabel(,wrap(20)) xline(0) drop(_cons)  ///
        mlabel format(%9.2f) mlabposition(2) mlabgap(*2) mlabsize(2) ///
		xlabel(#5 ,format(%9.0f) labsize(2)) nolabels
		
graph export CoefsTrim`TrimE'.pdf, replace
translate ResultsTrim`TrimE'.smcl  ResultsTrim`TrimE'.pdf, translator(smcl2pdf)  pagesize(a4) ///
          cmdnumber(off) logo(off) fontsize(8) header(off)  replace

log close



/* Tests */ 
*matrix B1 = e(b)'
/*
*discard
*Monsspecialreg2 i_tap Special, exog($exog) endog($endog) iv($instrument) hetero kdens band(5) trim(2.5) 
*matrix B5 = e(b)'

*ereturn list

matrix B= B1,B5
matrix list B
 */


