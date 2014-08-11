/* Programme d'estimation sur le  fichier à 5 pays avec variables instrumentales */
/* 3/04/2014  : Estimations de Céline avec instrument ssur 2008 */ 
/* 14/04/2014 :  Ajout des estimations des effets marginaux par bootstrap  */
/* 15/04/2014 : Ajout de nouveaux special regresseurs   */ 

global root "d:/progs/celine/water"
use "$root\data\DataOECD5", clear

/* Variables du modèle <-----   A MODIFIER  */

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


/* Nouveaux essais de regresseur special  (Céline 15/04/2014) */ 

************************************************************************
************************************************************************

* Variables du modèle 

* Special regresseur = KM DRIVEN PER WEEK

set seed 1111

*egen ma2_age = mean(a2_age)
*replace a2_age = a2_age - ma2_age

* d08_kmdriven_wk_impute (missing if "don't know" or "do not drive")

* je mets la variable à 0 pour ceux qui ne conduisent pas
replace d08_kmdriven_wk_impute = 0 if d08_kmdriven_wk == 1
* je mets la variable à 0 pour ceux qui ont une moto et pas de voiture
replace d08_kmdriven_wk_impute = 0 if (d02_motoclc > 0 & d02_motoclc~=. & d01_cars == 0)

capture drop Special
gen Special= d08_kmdriven_wk_impute


global exog "a2_age i_under18 log_income i_town i_car b08_locenv_water i_can i_fra i_net i_swe"
global endog "isatis_health"
global instrument "itap_2008 iconcernwatpol_2008"



***********************************************************************
************************************************************************

* Variables du modèle 

* Special regresseur = TOTWASTE 
* c2_bag_size

gen bagsize = 10 if c2_bag_size == 1
replace bagsize = 20 if c2_bag_size == 2
replace bagsize = 30 if c2_bag_size == 3
replace bagsize = 50 if c2_bag_size == 4
replace bagsize = 100 if c2_bag_size == 5

destring c2_num_bags, replace

gen totwaste = bagsize*c2_num_bags/a3_hhsize_imp

/* Nouvelles variables (18/04/2012) */

bysort country: egen mtotwaste = mean(totwaste)
bysort country: egen sdtotwaste = sd(totwaste)

gen totwaste2 = (totwaste - mtotwaste)/sdtotwaste
gen totwaste3 = totwaste/mtotwaste
egen gr_waste = group(totwaste)


/* 15/05/2014 :  On vire encore 2 pays */ 

drop if country == 8
drop if country == 10

egen p1_totwaste = pctile(totwaste), p(1)
egen p99_totwaste = pctile(totwaste), p(99)
drop if totwaste <= p1_totwaste
drop if totwaste >= p99_totwaste

note : Fichier créé avec DataOECD-2.do le 15 Mai 2014
save "$root\data\DataOECD3Special", replace



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




