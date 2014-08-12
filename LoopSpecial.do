
/*   Programme de compilation des résultats en fonction des Trims,  bandwidth  */
/* 01/08/2014 : creation et premiers résultats  */

discard
clear
drop _all
capture drop Meff
matrix Meff = J(11,1,.)
 

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


/* Relation i_tap et Special  */
lpoly i_tap Special, noscatter  degree(0) ci lineopts(lcolor(cranberry)) ///
			ciopts(recast(rarea) fintensity(30)) ///
			title(Relation between decision variable and special regressor) scheme(sj)

graph export "Graphs/RelationD-V.pdf", replace

global exog "i_under18 log_income i_town i_car b08_locenv_water a2_age i_can i_fra"
global endog "isatis_health"
global instrument "itap_2008 iconcernwatpol_2008"

* Test de White (modifié pour eviter confusions avec uhat de la proc....)
reg Special $exog $endog $instrument
estat imtest, white

predict Uhat, res
gen Uhat2 = Uhat*Uhat

reg Uhat2 $exog $endog $instrument


/* *******   LOOP  *******   */
set more off
*  - KDENS ESTIMATOR procedure 

/* <<<<<  OPTIONS  >>>>>>>>>>>>>*/ 

/* Choix du trim   */
local t= 5.0
/*winsor ou pas  */
local W = "winsor"
*local W = ""
/* Pour les noms de fichier  */
local TrimE = int(10*`t')


log using "Logs\ResultsTrim`TrimE'`W'", replace
set linesize 88 
 
/* Avec fenêtre choisie  automatiquement  (Silverman)  */ 
*quietly Monsspecialreg2 i_tap Special, exog($exog) endog($endog) iv($instrument) hetero kdens  trim(`t') `W'
*di in red " --- Trim = `t' ; Fenetre de Silverman :   `e(Band)'  ---" 

/* On recupère les données pour calcul CV sous R */
*save "data/FinalCV.dta", replace

/* La valeur de la fenêtre CV est 1.004  */
*quietly Monsspecialreg2 i_tap Special, exog($exog) endog($endog) iv($instrument) hetero kdens band(1.00) trim(`t') `W'

/* On lance aussi une version avec ordered choice : Band= 999 */
quietly Monsspecialreg2 i_tap Special, exog($exog) endog($endog) iv($instrument) hetero trim(`t') `W'

/* Avec Silverman (0.23) , 3 choix de fenêtres  et CV (1.004)  */
foreach  b in  0.1 0.23 0.30 0.5 1.004 {

di in red " >>>>   band = `b'  ;   trim = `t'   <<<< "

/* On utilise une version modifiée de Monspecialreg2.do qui stocke les résulats dans Md */
quietly Monsspecialreg2 i_tap Special, exog($exog) endog($endog) iv($instrument) hetero kdens band(`b') trim(`t') `W'
 
}


/* Recuperation des estimations  */

esttab Md*, stat(band trim obstrim)
quietly esttab Md* using "logs/ResultsTrim`TrimE'`W'.tex", stat(band trim obstrim) obslast replace




/* forme compacte  */
estout Md*, stat(band trim obstrim)




coefplot  `r(names)',  bycoefs byopts(xrescale) ///
        coeflabel(,wrap(20)) xline(0) drop(_cons)  ///
        mlabel format(%9.2f) mlabposition(2) mlabgap(*2) mlabsize(2) ///
		xlabel(#5 ,format(%9.0f) labsize(2)) nolabels
		
graph export "Graphs/CoefsTrim`TrimE'`W'.pdf", replace

/* Récupération des effets marginaux (matrice Meff):  ATTENTION Nbre de cols !!  */
matrix Meff2 = Meff[1..11, 2..7]
matrix list Meff2
quietly outtable using "logs/MfxTrim`TrimE'`W'", mat(Meff2) replace nobox f(%9.3f)
  
/* Stats sur la variable T  */

sum Tvar*
log close

quietly sutex Tvar*, labels minmax nobs title("Summary statistics for variable T") file(logs/TvarTrim`TrimE'`W'.tex) digit(2) replace

translate "Logs/ResultsTrim`TrimE'`W'.smcl"  "Logs/ResultsTrim`TrimE'`W'.pdf", translator(smcl2pdf)  pagesize(a4) ///
          cmdnumber(off) logo(off) fontsize(7) header(off) lmargin(0.5) rmargin(0.5) tmargin(0) bmargin(1)  replace



/* Graphiques  */
twoway (kdens TvarB99900T`TrimE')(kdens TvarB10T`TrimE')(kdens TvarB23T`TrimE')    ///
		  (kdens TvarB30T`TrimE') (kdens TvarB50T`TrimE')(kdens TvarB100T`TrimE' )
graph export "Graphs/DensTvar`TrimE'`W'.pdf", replace

graph hbox TvarB* , medtype(cline) medline(lcolor(cranberry) lwidth(medthick))
graph export "Graphs/BoxTvar`TrimE'`W'.pdf", replace




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


