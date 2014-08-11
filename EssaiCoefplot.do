
/* On supprime le label d'une variable (trop long)  */
*label variable  b08_locenv_water "Satisfaction with local water quality? "
label variable  b08_locenv_water ""

/* QQ commandes pour utiliser coefplot   */

coefplot, coeflabel(, wrap(20))
coefplot, coeflabel(, wrap(20)) xline(0)
coefplot, coeflabel(, wrap(20)) xline(0) drop(_cons)
coefplot, coeflabel(, wrap(20)) xline(0) drop(_cons) mlabel format(%9.2f) mlabposition(12) mlabgap(*2)
coefplot M1 M2 , coeflabel(, wrap(20))
coefplot M1 M2, coeflabel(, wrap(20)) xline(0) drop(_cons) mlabel format(%9.2f) mlabposition(12) mlabgap(*2)
coefplot M1 M2, coeflabel(, wrap(20)) xline(0) drop(_cons) mlabel format(%9.2f) mlabposition(2) mlabgap(*2) mlabsize(2)


/* Plus pertient quand les coefs ont des valeurs très différentes */

coefplot M1 M2,  bycoefs byopts(xrescale) ///
        coeflabel(,wrap(20)) xline(0) drop(_cons)  ///
        mlabel format(%9.2f) mlabposition(2) mlabgap(*2) mlabsize(2) ///
		xlabel(#5 ,format(%9.0f) labsize(2)) 
