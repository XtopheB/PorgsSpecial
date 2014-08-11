/* Programme d'estimation avec Special regressor */
/* 19/03/2014 :  Sequence initiale sur la base du fichier crée par DataOECD. do */
/* 20/03/2014  : Différence Trimming vs Winsorization (on choisit trimming)   */
/* 28/03/2014  : Intégration de la procédure Monsspecialreg  */

global root "d:/progs/celine/water"
use "$root\data\DataOECD5", clear

/* Variables du modèle <-----   A MODIFIER  */

/* Special regresseur = AGE */

set seed 1234

gen Special= a2_age

/* Création de variables spécifiques aux pays pour Bootstrap   (Céline 04/04/2014) */ 
gen i_aus = (country == 1)
gen i_can = (country == 2)
gen i_fra = (country == 4)
gen i_net = (country == 8)
gen i_swe = (country == 10)

global exog "i_under18 log_income i_town i_car b08_locenv_water i_can i_fra i_net i_swe"
global endog "isatis_health"
global instrument "itap_2008 iconcernwatpol_2008"

/* Lancement SpecialReg.ado  */ 

discard
Monsspecialreg i_tap Special,  exog($exog) endog($endog) iv($instrument) kdens trim(2.5) 

/*------- Debut programme   ici----------------------*/ 

/* STEP 1 :   */
* identification de l'échantillon utilisable (comme dans le programme)  */ 


keep if Touse==1
count

* Demean  V  
sum Special if Touse
gen V = Special - `r(mean)' if Touse

* Regression linéaire de V sur tous les variables et les instruments   
 
regress V $exog $endog $instrument if Touse

*  récupération du résidu U   
predict Uhat if Touse, resid 

/* Step 2 : Nonparametric estimation of f(xi), creation Fu= densité(Uhat)  */

kdens Uhat, kernel(epan) bw(silver) at(Uhat) gen(Fu)

/* Step 3 : Creation de T  */
 
 * on elague la densité pour éviter les Pb numériques sur dénominateur
 * expression plus simple, mais de même esprit que Baum utilisée ici 

* Version simplifiée 
*replace Fu = 10^(-20) if Fu < 10^(-20)

* Version Baum 
replace Fu= ((abs(Fu)>10^(-20))*Fu)+((1-(abs(Fu)>10^(-20)))*10^(-20)*((2*(Fu>0))-1)) 
  
 * Creation de T
 
 gen T = ( i_tap - (V >= 0) )/ Fu
 
 * verification du Nbre d'obs restant (si on en perd au passage )
 sum i_tap V Fu T 

 /* Step 4 : Ivreg of T on exog with IV regressors and instruments    */
 
 * ici on trimme  i.e. on supprime les queues de la distribution de T
 * Baum prend le quantile de |T| comme référence (pourquoi ?) 
 * ATTENTION : on peut aussi affecter aux valeurs extrèmes les valeurs de la borne (c'est la Winsorization !)
 
 global trim = 2.5
 
 * Version dissymétrique 
 /*
 local tlimH = 100-$trim/2
 local tlimL = $trim/2
 egen ptileH = pctile(T), p(`tlimH') 
 egen ptileL = pctile(T), p(`tlimL') 
  
 replace T = cond( T > ptileH, ptileH, T)
 replace T = cond( T < ptileL, ptileL, T)
 */
 
 * version Baum (symétrique)
 
local tlimAB = 100-$trim
egen ptileAB = pctile(abs(T)), p(`tlimAB')
 
replace T = cond( abs(T) > ptileAB, ., T)
count if missing(T)

 * première option pour comparer avec sspecialreg on garde ivregress 2SLS (comme Baum)
 
 di ""
 di " Modèle  estimé :"
 di " ivregress 2sls T $exog ($endog = $instrument) "
 di ""
 
 ivregress 2sls T $exog ($endog = $instrument) 
 estimates store NOUS
 
 * On peut retrouver les probas et les effets marginaux (todo ?)
 
 /* Comparaison avec sspecialreg  Triming, densité NP et pas de Winsorization */
 
 sspecialreg i_tap Special,  exog($exog) endog($endog) iv($instrument) kdens trim(2.5) 
 estimates store EUX
 
 /* Comparaison des 2 estimations  */ 
 est table NOUS EUX , stats(N ll chi2 df_m aic r2_p ) star style(noline) b(%7.3f) label
 
  
 
 
