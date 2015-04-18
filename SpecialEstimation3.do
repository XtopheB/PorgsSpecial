/* Programme d'estimation sur le  fichier à 5 pays avec variables instrumentales */
/* 22/07/2014 : Issu de SpecialEstimation.do   */ 
/*            : Changement de Special regresseur    */
/* 25/07/2014 : utilisation du fichier de données de Céline  */

clear
*global root "d:/progs/celine/water"
global root "c:/Chris/progs/celine/water"

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

* Test de White
reg Special $exog $endog $instrument
estat imtest, white

predict uhat, res
gen uhat2 = uhat*uhat

reg uhat2 $exog $endog $instrument

* IV TEST - KDENS ESTIMATOR
discard
Monsspecialreg i_tap Special, exog($exog) endog($endog) iv($instrument) hetero kdens trim(2.5) 



/*
* SUPPORTS

sspecialreg i_tap Special, exog($exog) endog($endog) iv($instrument) hetero kdens trim(2.5) 

gen xbeta_kdens = _b[isatis_health]*isatis_health + _b[i_under18]*i_under18 + _b[log_income]*log_income /*
*/ + _b[i_town]*i_town + _b[i_car]*i_car + _b[b08_locenv_water]*b08_locenv_water + _b[i_can]*i_can /*
*/ + _b[i_fra]*i_fra + _b[i_net]*i_net + _b[i_swe]*i_swe + _b[_cons]


* IV TEST - SORTED DATA ESTIMATOR
discard
Monsspecialreg i_tap Special, exog($exog) endog($endog) iv($instrument) hetero trim(2.5)


* SUPPORTS

sspecialreg i_tap Special, exog($exog) endog($endog) iv($instrument) hetero trim(2.5)

gen xbeta_sorted = _b[isatis_health]*isatis_health + _b[i_under18]*i_under18 + _b[log_income]*log_income /*
*/ + _b[i_town]*i_town + _b[i_car]*i_car + _b[b08_locenv_water]*b08_locenv_water + _b[i_can]*i_can /*
*/ + _b[i_fra]*i_fra + _b[i_net]*i_net + _b[i_swe]*i_swe + _b[_cons]

summ xbeta_kdens xbeta_sorted a2_age if e(sample), d
*/

discard
sspecialreg i_tap Special, exog($exog) endog($endog) iv($instrument) /*
*/ hetero hetv(i_under18 log_income i_town b08_locenv_water) /*
*/ kdens trim(2.5) bs bsreps(999)


discard
sspecialreg i_tap Special, exog($exog) endog($endog) iv($instrument) /*
*/ hetero hetv(i_under18 log_income i_town b08_locenv_water) /*
*/ trim(2.5) bs bsreps(999)


/* 03/09/2014 : Modèle sans Hetero  */

discard
sspecialreg i_tap Special, exog($exog) endog($endog) iv($instrument) ///
kdens   trim(2.5)  bs bsreps(399) 

/* Pour sortie Rtf   */
esttab using ModelNoHetero.rtf, se brackets stat(band trim boot ) obslast replace  noeqlines compress star( * 0.10 ** 0.05  *** 0.01)
log close 


* PROBIT SIMPLE

xi: probit i_tap a2_age prix_2008 i_under18 log_income i_town i_car isatis_health b08_locenv_water i.country 
lstat
margins, dydx(*) predict(pr)

preserve
keep if e(sample)
* Sauvegarde de l'échantillon du papier !!!
save "$root\data\FinalFile.dta", replace
count
restore
* BIPROBIT



xi: biprobit (i_tap a2_age prix_2008 i_under18 log_income i_town i_car isatis_health b08_locenv_water i.country) /*
*/ (isatis_health a2_age prix_2008 i_under18 log_income i_town i_car b08_locenv_water i.country itap_2008 iconcernwatpol_2008)


* IVPROBIT 

xi: ivprobit i_tap a2_age prix_2008 i_under18 log_income i_town i_car b08_locenv_water i.country /*
*/ (isatis_health = itap_2008 iconcernwatpol_2008)

margins, dydx(*) predict(pr)





/* Special regresseur = AGE */

set seed 1234

egen ma2_age = mean(a2_age)
replace a2_age = a2_age - ma2_age

gen Special= a2_age

global exog "i_under18 log_income i_town i_car b08_locenv_water i_can i_fra i_net i_swe"
global endog "isatis_health"
global instrument "itap_2008 iconcernwatpol_2008"


/* Création de variables spécifiques aux pays pour Bootstrap   (Céline 04/04/2014) */ 
gen i_aus = (country == 1)
gen i_can = (country == 2)
gen i_fra = (country == 4)
gen i_net = (country == 8)
gen i_swe = (country == 10)


/* Modèle Céline 13/04/2014  */
sspecialreg i_tap Special, exog($exog) endog($endog) iv($instrument) /*
 */ hetero hetv(i_under18 log_income i_town b08_locenv_water) /*
 */ kdens trim(2.5) 
 * bs bsreps(999)




/* Essais chris  */ 


capture drop Special
gen Special= totwaste

egen p1_totwaste = pctile(totwaste), p(1)
egen p99_totwaste = pctile(totwaste), p(99)
/* plutot que droper, je mets à missing */
replace totwaste=. if totwaste <= p1_totwaste
replace totwaste=. if totwaste >= p99_totwaste


/* Vérification de la relation en D et Special regresseur (14/04/2014)   */
set autotabgraphs on 

twoway lpolyci i_tap a2_age
hist a2_age, kdens

twoway lpolyci i_tap log_income
hist log_income, kdens

twoway lpolyci i_tap d08_kmdriven_wk_impute
hist d08_kmdriven_wk_impute, kdens

twoway lpolyci i_tap totwaste, name(totwaste)
hist totwaste, kdens name(totwastedens)

twoway lpolyci i_tap totwaste2, name(totwaste2)
hist totwaste2, kdens name(totwastedens2)

twoway lpolyci i_tap totwaste3, name(totwaste3)
hist totwaste3, kdens name(totwastedens3)


twoway lpolyci i_tap Special
hist Special, kdens




/* TESTS   */

* Test de White
reg Special $exog $endog $instrument
estat imtest, white

predict uhat, res
gen uhat2 = uhat*uhat

reg uhat2 $exog $endog $instrument


/* Modeles    */  

discard
Monsspecialreg i_tap Special, exog($exog) endog($endog) iv($instrument) hetero kdens trim(2.5) 

estimates store M1

/* Avec sans hétérogénéité  */
discard
sspecialreg i_tap Special, exog($exog) endog($endog) iv($instrument) /*
 */ hetero hetv(i_under18 log_income i_town b08_locenv_water) /*
 */ kdens trim(2.5) bs  bsreps(99) 



/* Essais de Bootstrap  (fait le 14/04/2014 --  modification de la proc sspecialreg ) */

sspecialreg i_tap Special, exog($exog) endog($endog) iv($instrument)  kdens trim(2.5) bs bsreps(250)

/* Essais sur la fenêtre  */

sspecialreg i_tap Special, exog($exog) endog($endog) iv($instrument)  kdens trim(2.5) bs bsreps(10)


/* Sans densité  */
discard
Monsspecialreg i_tap Special, exog($exog) endog($endog) iv($instrument) hetero trim(2.5)

/*
discard
Monsspecialreg i_tap Special, exog($exog) endog($endog) iv($instrument) /*
*/ hetero hetv( i_under18 log_income i_town b08_locenv_water isatis_health itap_2008) kdens trim(2.5) bs bsreps(100)

discard
Monsspecialreg i_tap Special, exog($exog) endog($endog) iv($instrument) /*
*/ hetero hetv( i_under18 log_income i_town b08_locenv_water isatis_health itap_2008) trim(2.5) bs bsreps(100)
*/
 
discard
Monsspecialreg i_tap Special, exog($exog) endog($endog) iv($instrument) hetero kdens trim(2.5) 

di " Verification que les 2 estimations sont les mêmes " 
est table Mod*, stats(N ll chi2 df_m aic r2_p) star style(noline) b(%7.3f) 


/* On garde l'échantillon utilisé dans la procédure pour comparaison */
keep if Monsample == 1

* PROBIT SIMPLE

xi: probit i_tap a2_age i_under18 log_income i_town i_car isatis_health b08_locenv_water i.country 
lstat
margins, dydx(*) predict(pr)

* BIPROBIT

xi: biprobit (i_tap a2_age i_under18 log_income i_town i_car isatis_health b08_locenv_water i.country) /*
*/ (isatis_health a2_age i_under18 log_income i_town i_car b08_locenv_water i.country itap_2008 iconcernwatpol_2008)

* From Austin Nichols: 
* How do we calculate the marginal effect of treatment after biprobit? Three "obvious approaches: use
* margins, use predict to get probabilities, or use binormal() with predicted linear indices. 
* The last is more correct, but all should give essentially the same answers.

*Méthode 1
margins, dydx(isatis_health) predict(pmarg1) force
*margins, dydx(isatis_health) predict(pmarg1) atmeans force

*Méthode 2
predict double xb2, xb2
preserve
ren isatis_health TR
gen isatis_health=0
predict double p0, pmarg1
predict double xb0, xb1
replace isatis_health=1
predict double p1, pmarg1
predict double xb1, xb1
g double dp=p1-p0
summ dp

* Méthode 3
loc TOT1=r(mean)
loc r=e(rho)
gen double pdx=(binormal(xb1,xb2,`r')-binormal(xb0,xb2,`r'))/normal(xb2) if isatis_health==1
su pdx, mean
loc TOT2=r(mean)
qui replace pdx=normal(xb1)-normal(xb0)
summ pdx

* IVPROBIT 

xi: ivprobit i_tap a2_age i_under18 log_income i_town i_car b08_locenv_water i.country /*
*/ (isatis_health = itap_2008 iconcernwatpol_2008)

margins, dydx(*) predict(pr)




