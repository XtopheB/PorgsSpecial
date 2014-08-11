
/*   Programme de compilation des résultats en fonction des Trims,  bandwidth  */
/* 01/08/2014 : creation et premiers résultats  */

clear
drop _all
capture drop Meff
matrix Meff = J(11,1,.)
 
discard
capture log close
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

* Test de White (modifié pour eviter confusions....
reg Special $exog $endog $instrument
estat imtest, white

predict Uhat, res
gen Uhat2 = Uhat*Uhat

reg uhat2 $exog $endog $instrument


/* *******   LOOP  *******   */
set more off
*  - KDENS ESTIMATOR procedure 

/* <<<<<  OPTIONS  >>>>>>>>>>>>>*/ 
/* Choix du trim   */
local t= 5.0
/*winsor ou pas  */
*local W = "winsor"
local W = ""
/* Pour les noms de fichier  */
local TrimE = int(10*`t')

log using "Logs\ResultsTrim`TrimE'`W'", replace
set linesize 85 
 
 
/* Avec fenêtre choisie  automatiquement  (Silverman)  */ 

di in red " --- Trim = `t' ; Fenetre de Silverman :   `e(Band)'  ---" 
quietly Monsspecialreg2 i_tap Special, exog($exog) endog($endog) iv($instrument) hetero kdens  trim(`t') `W'


/* On lance aussi une version avec ordered choice : Band= 999 */
quietly Monsspecialreg2 i_tap Special, exog($exog) endog($endog) iv($instrument) hetero trim(`t') `W'

/* Avec 3 choix de fenêtres  */

forvalues b = 0.1(0.2)0.5{
di in red " band = `b' , trim = `t' "
/* On utilise une version modifiée de Monspecialreg2.do qui stocke les résulats dans Md */
quietly Monsspecialreg2 i_tap Special, exog($exog) endog($endog) iv($instrument) hetero kdens band(`b') trim(`t') `W'
 
}



/* Recuperation des estimations  */

esttab Md*, stat(band trim obstrim)
estout Md*, stat(band trim obstrim)

coefplot  `r(names)',  bycoefs byopts(xrescale) ///
        coeflabel(,wrap(20)) xline(0) drop(_cons)  ///
        mlabel format(%9.2f) mlabposition(2) mlabgap(*2) mlabsize(2) ///
		xlabel(#5 ,format(%9.0f) labsize(2)) nolabels
		
graph export "Graphs/CoefsTrim`TrimE'`W'.pdf", replace
translate "Logs/ResultsTrim`TrimE'`W'.smcl"  "Logs/ResultsTrim`TrimE'`W'.pdf", translator(smcl2pdf)  pagesize(a4) ///
          cmdnumber(off) logo(off) fontsize(7) header(off)  replace

/* Récupération des effets marginaux (matrice Meff)   */
matrix Meff2 = Meff[1..11, 2..6]
matrix list Meff2
		  
		  
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


