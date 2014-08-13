
/*   Programme de compilation des résultats en fonction des Trims,  bandwidth  */
/* 13/08/2014 : creation et premiers résultats AVEC BOOTSTRAP  */

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

global exog "i_under18 log_income i_town i_car b08_locenv_water a2_age i_can i_fra"
global endog "isatis_health"
global instrument "itap_2008 iconcernwatpol_2008"


set more off

/* <<<<<  OPTIONS  >>>>>>>>>>>>>*/ 

/* Pas Winsorization   */
*local W = "winsor"
local W = ""

	/* Nbre de bootstraps  */ 
	local Boot = 399

	/* *******   LOOP  *******   */
	/* Choix des trim   */
foreach t in 0.5 2.5 5 {
		/* Pour les noms de fichier  */
		local TrimE = int(10*`t')
		
		/* On lance aussi une version avec ordered choice : Band= 999 */
		sspecialreg2 i_tap Special, exog($exog) endog($endog) iv($instrument)  ///
								hetero hetv(i_under18 log_income i_town b08_locenv_water) ///
								trim(`t') `W'  bs bsreps(`Boot')
								
		eststo MfxT`TrimE'B999,  addscalars(band 999 boot `Boot' )

		/* Avec Silverman (0.23) , 3 choix de fenêtres  et CV (1.004)  */
		foreach  b in  0.1 0.23 0.30 0.5 1.004 {
			di in red " >>>>   band = `b'  ;   trim = `t'   <<<< "

			/* On utilise une version modifiée de sspecialreg2.do qui permet le bootstrap pour mfx */
			sspecialreg2 i_tap Special, exog($exog) endog($endog) iv($instrument) ///
											hetero hetv(i_under18 log_income i_town b08_locenv_water) ///
											kdens band(`b') trim(`t') `W'  bs bsreps(`Boot')
											
			/* stockage */
			local BB = int(`b'*100)
			eststo MfxT`TrimE'B`BB', addscalars(band `b' boot `Boot'  )
			 
		}  /* <-- Fin boucle sur band   */


		/* Recuperation des estimations  */

		esttab MfxT`TrimE'*, se brackets stat(band trim boot ) obslast replace  noeqlines compress
		esttab MfxT`TrimE'* using "logs/MfxB`Boot'Trim`TrimE'`W'.tex", se brackets stat(band trim boot) obslast  noeqlines compress replace

	} /* <-- Fin boucle sur Trim  */

