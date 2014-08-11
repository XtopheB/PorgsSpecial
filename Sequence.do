
global root "D:\progs\Celine\Water\"
use $root\data\OECD_ENV_SURVEY_2011_DATA.dta, clear
tab a000_country
*keep if a000_country == "FRANCE"

*save $root\data\OECD_AUSTRALIA.dta, replace

*codebook g2_behav_*

gen id = a0000_respondentid

ds g2_behav_*
local i=1
foreach var in `r(varlist)' {
	di " la variable est `var'"
	gen g2_q`i' =`var' 
	local i=`i'+1
}
/* Un reshape pour que les questions soient identifiées en ligne : la var question est crée*/ 
reshape long g2_q , i(id) j(question)
/* on avait 996 répondants x 6 questions = 5976 lignes maintenant  */

/* syntaxe pour l'utilisation des séquences  */
sqset  g2_q id question

 /* Labellisation question */
 label define typoq 1"Never" 2 "Occasionally" 3 "Often" 4 "Always" 5 "NA" 
 label var g2_q typoq
 
/* Sequences les plus probables (toutes) */ 
*sqtab 
sqtab, ranks(1/15)  /* les 15 plus observées */


/* graphiques */
set autotabgraphs on
sqindexplot , color(cranberry*0.8 erose emidblue ltblue  gs14) 

/* if all countries */
*sqindexplot , by(a000_country)  color(cranberry*0.8 erose emidblue ltblue  gs14)



