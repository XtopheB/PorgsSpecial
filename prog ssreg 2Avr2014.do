clear matrix
clear
set mem 500m
set more off

use "C:\Celine\annee11\OCDE\données\OECD2008_regindex.dta", clear

gen country = 1 if H_COUNTRY == 61
replace country = 2 if H_COUNTRY == 11
replace country = 4 if H_COUNTRY == 33
replace country = 8 if H_COUNTRY == 31
replace country = 10 if H_COUNTRY == 46 

rename REGION region
sort country region

save "C:\Celine\annee11\OCDE\données\OECD2008_index.dta", replace



use "C:\Celine\annee11\OCDE\données\OECDepic2011phase2\OECD_ENV_SURVEY_2011_DATA_Juillet-2012.dta", clear

rename exrate EXRATE

foreach v of varlist * {
rename `v' `=lower("`v'")'
}

rename a000_country country

/*
    country |      Freq.     Percent        Cum.
------------+-----------------------------------
  AUSTRALIA |        996        8.16        8.16
     CANADA |      1,122        9.20       17.36
      CHILE |      1,027        8.42       25.77
     FRANCE |      1,227       10.06       35.83
      JAPAN |      1,043        8.55       44.38
      KOREA |      1,116        9.15       53.52
NETHERLANDS |      1,301       10.66       64.19
      SPAIN |      1,101        9.02       73.21
     SWEDEN |      1,012        8.29       81.50
SWITZERLAND |      1,089        8.92       90.43
     ISRAEL |      1,168        9.57      100.00
------------+-----------------------------------
      Total |     12,202      100.00
*/
	  

* SEX OF THE RESPONDENT
rename a01_sex a1_sex
gen i_male = (a1_sex == 1)
drop a1_sex

* AGE AND AGE GROUP
rename a02_age a2_age 
rename a02_agegroup a2_agegroup
drop a02_yob                 


/*
a02_agegrou |
          p |      Freq.     Percent        Cum.
------------+-----------------------------------
      18-24 |      1,616       13.24       13.24
      25-34 |      2,546       20.87       34.11
      35-44 |      2,721       22.30       56.41
      45-54 |      2,539       20.81       77.22
        55+ |      2,780       22.78      100.00
------------+-----------------------------------
      Total |     12,202      100.00
*/


* HOUSEHOLD SIZE AND COMPOSITION
*rename a03_hhsize a3_hhsize
rename a03_hhsize_imp a3_hhsize_imp
rename a03_under05 a3_under5
*rename a03_under18 a3_under18
rename a03_under18_imp a3_under18_imp
      
* REGIONS      
*a04_region_aus  
*a04_region_can  
rename a04_region_che a04_region_swz  
*a04_region_chl  
*a04_region_esp  
*a04_region_fra  
*a04_region_isr  
*a04_region_jpn  
*a04_region_kor  
*a04_region_nld  
*a04_region_swe  

* POST HIGH SCHOOL EDUCATION 
rename a05_year_post~l a5_educ 
gen i_posthighsch = 0
replace i_posthighsch = 1 if a5_educ ~= 0 & a5_educ ~=.

* OCCUPATION
rename a06_employmnt a6_empl  
drop a06_employmnt~h 
rename a06_occupation a6_occup 
drop a06_occupatio~r 
* Employed or self-employed, the rest includes retired, homemaker, unemployed, student, unable to work, other
gen i_empl = 0
replace i_empl = 1 if a6_empl == 1 | a6_empl == 2

* OCCUPATION OF THE PRIMARY INCOME EARNER
rename a07_primearnr~p a07_primoccup  
drop a07_primearnr~h 

* INCOME
*rename a08_eur_midpo~t a8_inceur
rename a08_income_impute a8_inceur_imp
rename a08_income_category a8_incgroup 
rename a09_income_percptn a9_incperc

table a8_incgroup, c(mean a2_age)

* OWNERSHIP STATUS 
gen i_owner = (a10_home_owner == 1)
drop a10_home_owner

* TYPE OF PRIMARY RESIDENCE
rename a11_home_type a11_residtype 
drop a11_home_type~r 

* SIZE OF THE RESIDENCE
*rename a12_home_size a12_residsize  
 rename a12_home_size_imp a12_residsize_imp
 
* AREA OF RESIDENCE
rename a13_urban_rural a13_residarea
* Town or suburban
gen i_town = 0
replace i_town = 1 if a13_residarea == 1 | a13_residarea == 2

* HOME TENURE
*rename a14_home_tenure a14_residten
rename a14_home_tenure_imp a14_residten_imp

* STATS DESCRIPTIVES
* characteristics of the respondent
table country, c(n i_male mean i_male mean a2_age mean a5_educ mean i_empl)
summ i_male a2_age a5_educ i_empl
* characteristics of the household
table country, c(mean a3_hhsize mean a8_inceur mean i_owner mean i_town)
summ a3_hhsize a8_inceur i_owner i_town

* HAPPINESS
rename b01_happy b1_happy

* RANKING OF GENERAL CONCERNS
rename b02_rank_eco_~n b2_concern_economic
rename b02_rank_env_concern b2_concern_envir                                            
rename b02_rank_hlt_concern b2_concern_health
rename b02_rank_int_tension b2_concern_inttension
rename b02_rank_per_safety b2_concern_safety
rename b02_rank_soc_issues b2_concern_social

local shop "economic envir health inttension safety social"
foreach x of local shop {
	bysort country: egen sb2_concern_`x' = sum(b2_concern_`x')
	bysort country: gen nobs_concern_`x' = _N*(b2_concern_`x'~=.)
	gen mb2_`x' = sb2_concern_`x'/nobs_concern_`x'
}

table country, c(mean b1_happy mean mb2_economic mean mb2_envir mean mb2_health)
table country, c(mean mb2_inttension mean mb2_safety mean mb2_social)

table i_male, c(mean b2_concern_envir mean b2_concern_health)
table a2_agegroup, c(mean b2_concern_envir mean b2_concern_health)
table a8_incgroup, c(mean b2_concern_envir mean b2_concern_health)


* RANKING OF ENVIRONMENTAL CONCERN
rename b03_cncrn_air b3_concern_airpol
rename b03_cncrn_biodv b3_concern_biodiv
rename b03_cncrn_cchge b3_concern_cchange
rename b03_cncrn_deplt b3_concern_natres
rename b03_cncrn_waste b3_concern_waste
rename b03_cncrn_water b3_concern_waterpol 
rename b03_env_cncrn~x b3_envconcern_index
destring b3_concern_airpol, replace
destring b3_concern_biodiv, replace
destring b3_concern_cchange, replace
destring b3_concern_natres, replace
destring b3_concern_waste, replace
destring b3_concern_waterpol, replace
gen envcrn_index_test = (b3_concern_airpol + b3_concern_biodiv + b3_concern_cchange + b3_concern_natres /*
*/ + b3_concern_waste + b3_concern_waterpol)/6
summ b3_envconcern_index envcrn_index_test

table country, c(mean b3_concern_airpol mean b3_concern_biodiv mean b3_concern_cchange mean b3_concern_natres)
table country, c(mean b3_concern_waste mean b3_concern_waterpol mean b3_envconcern_index)

pwcorr b3_envconcern_index a8_inceur, sig


* VOTING IN ELECTIONS
rename b04_vote_dummy b4_vote 
rename b04_vote_local b4_vote_local 
rename b04_vote_nationl b4_vote_nat
rename b04_vote_none b4_vote_none 


* INVOLVEMENT IN ASSOCIATIONS/ORGANISATIONS
rename b05_cmnty_dummy b5_involv
rename b05_cmnty_cha b5_charit 
rename b05_cmnty_env b5_envir 
rename b05_cmnty_loc b5_localcom 
rename b05_cmnty_oth b5_other
rename b05_cmnty_pta b5_parentteacher

table country, c(mean b5_charit mean b5_envir mean b5_localcom mean b5_parentteacher)

* ENVIRONMENTAL ATTITUDES
* strongly disagree = 1
* disagree = 2
* agree = 3
* strongtly agree = 4
* no opinion = 5

foreach x of numlist 4 7 {
          gen b6_env_att`x' = -2 if b06_env_att`x' == 1
		  replace b6_env_att`x' = -1 if b06_env_att`x' == 2
		  replace b6_env_att`x' = 1 if b06_env_att`x' == 3
		  replace b6_env_att`x' = 2 if b06_env_att`x' == 4
		  replace b6_env_att`x' = 0 if b06_env_att`x' == 5
		  }

foreach x of numlist 1 2 3 5 6 {
          gen b6_env_att`x' = 2 if b06_env_att`x' == 1
		  replace b6_env_att`x' = 1 if b06_env_att`x' == 2
		  replace b6_env_att`x' = -1 if b06_env_att`x' == 3
		  replace b6_env_att`x' = -2 if b06_env_att`x' == 4
		  replace b6_env_att`x' = 0 if b06_env_att`x' == 5
		  }
		  
gen env_index_test = (b6_env_att1 + b6_env_att2 + b6_env_att3 + b6_env_att4 + b6_env_att5 + b6_env_att6 /*
*/ + b6_env_att7)/7
*edit b6* b06_env_attid~x env_index_test
*summ b06_env_attid~x env_index_test
table country, c(mean env_index_test)

*table country, c(mean b3_envconcern_index mean b08_locenv_indx mean b06_env_attid~x)
*pwcorr b3_envconcern_index b06_env_attid~x b08_locenv_indx, sig
*bysort country: pwcorr b3_envconcern_index b06_env_attid~x b08_locenv_indx, sig


* TRUST
destring b07_trust_con~r b07_trust_exp~s b07_trust_gvm~s b07_trust_man~t b07_trust_ngos b07_trust_indx, replace
rename b07_trust_con~r b7_trust_consumorg 
rename b07_trust_exp~s b7_trust_expert 
rename b07_trust_gvm~s b7_trust_govt
rename b07_trust_man~t b7_trust_manuf 
rename b07_trust_ngos b7_trust_ngo
rename b07_trust_indx b7_trust_index 

table country, c(mean b7_trust_consumorg mean b7_trust_expert)
table country, c(mean b7_trust_govt mean b7_trust_manuf mean b7_trust_ngo)


* SATISFACTION ABOUT THE LOCAL ENVIRONMENT
* very dissatisfied = 1
* dissatisfied = 2
* satisfied = 3
* very satisfied = 4
* no opinion = 5

local satis "airqt green noise trash water"
foreach x of local satis {
          gen b8_`x' = -2 if b08_locenv_`x' == 1
		  replace b8_`x' = -1 if b08_locenv_`x' == 2
		  replace b8_`x' = 1 if b08_locenv_`x' == 3
		  replace b8_`x' = 2 if b08_locenv_`x' == 4
		  replace b8_`x' = 0 if b08_locenv_`x' == 5
		  }

gen locenv_index_test = (b8_airqt + b8_green + b8_noise + b8_trash + b8_water)/5
summ b08_indx_locenv locenv_index_test

table country, c(mean b4_vote mean b5_involv mean b7_trust_index)

***************************************************************************
***************************************************************************

* UNIT CHARGE

gen unitcharge = 0 
replace unitcharge = 1 if g1_water_metrd == 1

gen dnk_unitcharge = 0
replace dnk_unitcharge = 1 if g1_water_metrd == 4


* INCOME (imputed): a8_inceur_imp
* LOW INCOME

gen lowincome = 1 if a8_incgroup == 1 | a8_incgroup == 2
replace lowincome = 0 if a8_incgroup > 2 & a8_incgroup ~= .

gen log_income = log(a8_inceur_imp)

* RANKING OF ENVIRONMENTAL CONCERN (B2): b2_concern_envir (1 is for highest concern??)
* Quentin seems to have a different interpretation

* ENVIRONMENTAL CONCERN INDEX: b3_envconcern_index (from B3, a higher value means more concern)

* VOTER DUMMY: b4_vote 

* ENV_GROUP_SUPPORTER: b5_envir

* ENVIRONMENTAL ATTITUDE INDEX: b06_env_attid~x (a higher value means a greener attitude)

* CLIMATE CHANGE KNOWLEDGE: b11_knowledge (do not really like that)

* POLICY_INDEX_VEHICLE: b09_policy_indx

* POLICY_INDEX_WASTE: b10_policy_indx

* SATISFY_HEALTH: g8_wtrstfcn_health (a higher value means more satisfied)

* SATISFY_TASTE: g8_wtrstfcn_taste (a higher value means more satisfied)

* GENDER: i_male

* AGE: a2_age 

* HH SIZE: a3_hhsize_imp

* POST HIGH SCHOOL EDUCATION: i_posthighsch

* OCCUPATION: a6_empl  i_empl

* OWNERSHIP STATUS: i_owner 

* TYPE OF PRIMARY RESIDENCE: a11_residtype 

* SIZE OF THE RESIDENCE: a12_residsize_imp
 
* AREA OF RESIDENCE: a13_residarea

* TOWN OR SUBURBAN: i_town

* HOME TENURE: a14_residten_imp

* b06_env_att2 b06_env_att6


forval num = 1/11 {
gen unitcharge_c`num' = unitcharge*(country==`num')
}

gen a2_age_sq = a2_age*a2_age



***************************
* TAP WATER
***************************
*
gen i_tap = 0 if g7_norml_water == 2 | g7_norml_water == 3 | g7_norml_water == 4
replace i_tap = 1 if g7_norml_water == 1 

/*
-------------------------
(encoded    |
categorical |
variable)   | mean(i_tap)
------------+------------
  AUSTRALIA |    .5861322
     CANADA |    .4830509
      CHILE |    .6061204
     FRANCE |     .441201
     ISRAEL |    .2788632
      JAPAN |    .4703557
      KOREA |    .0497561
NETHERLANDS |    .8799688
      SPAIN |    .4426386
     SWEDEN |    .8961973
SWITZERLAND |    .6193129
-------------------------

G7. For your normal household drinking water which of the following do you usually drink?
1. Straight from the tap
2. Purified/filtered/boiled tap water
3. Bottled mainly still/flat
4. Bottled mainly sparkling
5. Natural source (e.g. rainwater/surface water/well)
98. Other, please specify:______________ OPEN END
*/

gen i_under5 = 0
replace i_under5 = 1 if a3_under5>0 & a3_under5~=.

gen isatis_health = 0
replace isatis_health = 1 if g8_wtrstfcn_health > 5 & g8_wtrstfcn_health ~= .

bysort country: egen mg8_wtrstfcn_health = mean(g8_wtrstfcn_health)

gen isatis_health2 = 0 if g8_wtrstfcn_health<=mg8_wtrstfcn_health
replace isatis_health2 = 1 if g8_wtrstfcn_health>mg8_wtrstfcn_health

gen i_above55 = (a2_agegroup == 5)
gen i_under18 = (a3_under18_imp>0 & a3_under18_imp~=.) 

gen i_concern1_envir = (b2_concern_envi == 1)                                       
gen i_concern1_health = (b2_concern_health == 1)

gen i_concern1_waste = (b03_cncrn_rankwaste == 1)

gen i_car = (d01_cars>0 & d01_cars~=.)

rename b2_concern_envi b2_lessconcernenvir
rename b03_cncrn_rankwaste b03_lessconcernwaste
rename b03_cncrn_rankwater b03_lessconcernwater

* MODELE PROBIT SUR 11 PAYS

*****************************************************************
*****************************************************************
xi: probit i_tap a2_age i_under18 i_posthighsch lowincome i_town i_car isatis_health b08_locenv_water  /*
*/ b3_envconcern_index b03_lessconcernwaste i.country 
*****************************************************************
*****************************************************************

* MODELE PAYS EXCLUANT CHILI, ISRAEL, COREE ET JAPON

xi: probit i_tap a2_age i_under18 i_posthighsch lowincome i_town i_car isatis_health b08_locenv_water  /*
*/ b3_envconcern_index /*b03_lessconcernwaste*/ b10_policy_indx i.country if country~=3 & country~=5 & /*
*/ country ~= 6 & country~=7


* MODELE SUR AUSTRALIA, CANADA, FRANCE, NETHERLANDS AND SWEDEN

keep if country == 1 | country == 2 | country == 4 | country == 8 | country == 10

xi: probit i_tap a2_age i_under18 log_income i_town i_car isatis_health b08_locenv_water i.country 


*****************************************************************
*****************************************************************
* PROCEDURE CHRISTOPHE
*****************************************************************
*****************************************************************

gen region = .

* Australia
forval num = 1/8 {
          replace region = `num' if country == 1 & a04_region_aus == `num'
		  }

* Canada
replace region = 1 if country == 2 & a04_region_can == 1
replace region = 2 if country == 2 & a04_region_can == 2
replace region = 3 if country == 2 & a04_region_can == 3
replace region = 4 if country == 2 & a04_region_can == 4
replace region = 5 if country == 2 & a04_region_can == 5
replace region = 6 if country == 2 & a04_region_can == 7
replace region = 7 if country == 2 & a04_region_can == 9
replace region = 8 if country == 2 & a04_region_can == 10
replace region = 9 if country == 2 & a04_region_can == 11
replace region = 10 if country == 2 & a04_region_can == 12

* France
forval num = 1/22 {
          replace region = `num' if country == 4 & a04_region_fra == `num'
		  }
		  
* Netherlands

replace region = 2 if country == 8 & a04_region_nld == 12 | a04_region_nld == 8 | a04_region_nld == 10
replace region = 3 if country == 8 & a04_region_nld == 3 | a04_region_nld == 1 | a04_region_nld == 5
replace region = 4 if country == 8 & a04_region_nld == 9 | a04_region_nld == 4 | a04_region_nld == 2
replace region = 5 if country == 8 & a04_region_nld == 7 | a04_region_nld == 6 | a04_region_nld == 11

* Sweden
forval num = 1/21 {
          replace region = `num' if country == 10 & a04_region_swe == `num'
		  }


sort country region
merge country region using "C:\Celine\annee11\OCDE\données\OECD2008_index.dta"
tab _merge

keep if country~=. & region~=.


/* Variables du modèle <-----   A MODIFIER  */

/* Special regresseur = AGE */

set seed 1234

egen ma2_age = mean(a2_age)
replace a2_age = a2_age - ma2_age

gen Special= a2_age

gen i_aus = (country == 1)
gen i_can = (country == 2)
gen i_fra = (country == 4)
gen i_net = (country == 8)
gen i_swe = (country == 10)

global exog "i_under18 log_income i_town i_car b08_locenv_water i_can i_fra i_net i_swe"
global endog "isatis_health"
global instrument "itap_2008 iconcernwatpol_2008"

* Test de White
reg Special $exog $endog $instrument
estat imtest, white

predict uhat, res
gen uhat2 = uhat*uhat

reg uhat2 $exog $endog $instrument

discard
Monsspecialreg i_tap Special, exog($exog) endog($endog) iv($instrument) hetero kdens trim(2.5) 

discard
Monsspecialreg i_tap Special, exog($exog) endog($endog) iv($instrument) hetero trim(2.5)


discard
sspecialreg i_tap Special, exog($exog) endog($endog) iv($instrument) /*
*/ hetero hetv(i_under18 log_income i_town b08_locenv_water isatis_health itap_2008) kdens trim(2.5) bs bsreps(100)

discard
sspecialreg i_tap Special, exog($exog) endog($endog) iv($instrument) /*
*/ hetero hetv(i_under18 log_income i_town b08_locenv_water isatis_health itap_2008) trim(2.5) bs bsreps(100)

 
di " Verification que les 2 estimations sont les mêmes " 
est table Mod*, stats(N ll chi2 df_m aic r2_p) star style(noline) b(%7.3f) 


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

