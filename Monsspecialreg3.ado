*! sspecialreg  cfb B526 from SimpleSpecial.ado in CleanedCodesApr2010 and SimpleStata2010
*! version 1.1.0: provide for bootstrapped SEs
*! version 1.1.1: Mata translation of sorted data density, Jann kdens
*! version 1.1.2: Return proper point estimates of marginal effects 
*! version 1.1.3: ssortedfm->Ben Jann's sddens_bj; add winsor option, avg index fn->Mata indfn
*! version 1.1.4: check to see whether _kdens is installed

/* 28/03/2014 : Monsspecialreg avec remplacement de ivregress 2sls par ivreg2  */
/* 02/04/2014 :  V2  Creation de la variable Monsample qui vaut 1 si le point fait partie de l'échantillon final  */
/* 03/04/2014 :  V3 Creation de la variable Touse pour identification de l'échantillon pertinent  */
/* 25/07/2014 : Ajout d'une option BAND() pour le choix de la taille de la fenêtre,*/
/*            :  la fenêtre choise est dans ereturn  */
/* 30/07/2014 : Suppression du drop _all pour que eststo fonctionne en boucle... */
/* 11/08/2014 : Mise à 999 pour le choix ordered choice  */
/* 15/04/2015 : Création MonsspecialReg3 et _ Monsspecialreg3.ado          */
/*            : Ajout option BAND dans  _ Monsspecialreg3.ado pour effets marginaux bootstrappés */

*prog drop _all

program define Monsspecialreg3, eclass
version 11.0
syntax varlist(numeric min=2 max=2)  [if] [in], ENDOG(string) IV(string) ///
              [EXOG(string) HETERO HETV(string) KDENS BAND(real 0)  WINSOR TRIM(real 2.5) BS BSREPS(integer 10)] 

/* AJOUT CHRIS LE 28/07/2014*/
set seed 1234

di""
di  in red  " Proc Monsspecialreg3.ado - modifiée avec choix Bandwidth y compris pour Bootstrap  (Version Avril 2015 )"
di""
capture drop toto  /* On efface la matrice temporaire mfx  */
*capture drop Tvar   /* <--- le nom de la variable change a chaque fois, donc pas besoin  */
capture drop Uchap


// check for _kdens
	capt which _kdens
	if _rc != 0 {
		di as err _n "You must install the kdens package, via ssc install kdens"
		error 198
	}
// require endog and iv lists
	marksample touse
	markout `touse' `endog' `iv' `exog' `hetv'
	
	/* Creation de la variable Touse  */
	capture drop Touse
	gen Touse = `touse'
	
	loc D: word 1 of `varlist'
	loc V: word 2 of `varlist'
	su `D' if `touse', mean
	if (r(N) == 0) {
		error 2000
	}		
	if (r(max) - r(min) == 0) {
		di as err _n "No variance in D!"
		error 198
	}
// validate trim option; default 2.5 pc
	if `trim' != 2.5 {
		if `trim' < 0 | `trim' > 99 {
		di as err "Invalid trim value: must be a percentage to be trimmed"
		error 198
		}
	}
// validate winsor option
	if "`winsor'" == "winsor" & `trim' == 0 {
		di as err "winsor option can only be used with trim > 0"
		error 198
	}
// logic for bs option   /*  <--- Je dois aussi modifier _sspecialreg LA si on veut que ça fonctionne !!   */
	if "`bs'" == "bs" {
		if `bsreps' <= 1 | `bsreps' > 999 {
			di as err "Invalid value for bsreps: must be 2-250"
			error 198
		}
		
		loc nelt: word count `exog' `endog' 
		loc nelt = `nelt' + 2 // allow for V and constant
		loc bscalc
		forv i=1/`nelt' {
			loc bscalc "`bscalc' mfx`i'=r(mfx`i')"
		}

		tempname b Vee
		loc het = cond("`hetero'" == "hetero", "hetero", "homo")
		loc kd = cond("`kdens'" == "kdens", "kdens", "nokdens")
		loc hv = cond("`hetv'" == "", "nohetv", "`hetv'")
		di _n "Computing bootstrap standard errors for marginal effects, `bsreps' bootstrap samples "
        di _n " ---  Choice of bandwidth included  (04-2015)---- "
		qui bootstrap `bscalc', reps(`bsreps'): /// // saving(testbs, replace): ///	
		_Monsspecialreg3 `varlist', touse(`touse') endog(`endog') iv(`iv') exog(`exog')  hetero(`het') ///
			hetv(`hv') kdens(`kd') band(`band') trim(`trim') winsor(`winsor')
		mat `Vee' = e(V)  
	}
// end bs logic
  
	tempvar vee uhat uhat2 sc sigmau duhat fuhat T dxb sigma h kd dk m num tlim ttrm
	tempname dxb0 em mfx bee 

// demean special regressor (not in this code, but in SimpleStata2010 and simplenew13.pdf)
	su `V' if `touse', mean
	qui g double `vee' = `V' - r(mean) if `touse'

// regress demeaned special regressor on endog, exog, iv
	qui reg `vee' `exog' `endog' `iv' if `touse'
	qui predict double `uhat' if `touse', resid 

// create uhat depending on hetero option
	if "`hetero'" == "hetero" {
		qui gen double `uhat2'=`uhat'^2 if `touse'
		qui reg `uhat2'  `endog' `exog' `iv' `hetv' if `touse' 
		qui predict double `sc' if `touse', xb
		qui replace `uhat' = `uhat'/sqrt(abs(`sc')) if `sc' != 0
	}

    
    
// compute kernel density estimator via Jann's _kdens if KDENS; otherwise use sorted data density
// estimator (Ben Jann's translation) per SimpleStata2010
	if "`kdens'" == "kdens" {
    /* On rend Band optionnel */
        if "`band'" != "0" {
		  qui _kdens `uhat' if `touse', bw(`band') kernel(epanechnikov) at(`uhat') gen(`fuhat') 
          di  "-- Fenetre choisie  : `band'"
        }
        else {   /* on a pas renseigné BAND et par défaut c'est 0 */
            qui  _kdens `uhat' if `touse', kernel(epanechnikov) at(`uhat') gen(`fuhat') 
            local band = `r(width)'
            di  "-- Fenetre (Silverman) choisie  : `band'"
            }
	} 
	else { /* Procedure avec Ordered Choices  */
	qui ssortedfm `uhat' if `touse', gen(`fuhat')
    local band = 999	
     di  "-- Ordered choice  : `band'"
	}

// zrnd inline
	quietly replace `fuhat' = ((abs(`fuhat')>10^(-20))*`fuhat')+((1-(abs(`fuhat')>10^(-20)))*10^(-20)*((2*(`fuhat'>0))-1))
	
// create T variable
// if het==1|cond==2{  i.e.
// if (not hetero) or (sorted data density)
// correction: should depend only on hetero
// if "`hetero'" != "hetero" | "`dens'" != "kdens" {
	if "`hetero'" != "hetero" {
		qui gen `T' = (`D' - ( `vee' >= 0)) / `fuhat' if `touse'
		}
	else {  // BB09: apply touse here too
		qui gen `T' =(`D' - ( `vee' >= 0)) / `fuhat' * sqrt(abs(`sc')) if `touse' 
	}

// apply trimming / winsorizing
	loc ptrim = 100 - `trim'
	qui egen `tlim' = pctile(abs(`T')), p(`ptrim')
	qui gen byte `ttrm' = cond((abs(`T') > `tlim'), ., 1) 
	if "`winsor'" != "winsor" {
		markout `touse' `ttrm'
		qui count if mi(`ttrm')	
		loc delta = r(N)
	} 
	else {
		qui replace `T' = cond(`T' > `tlim', `tlim', cond(`T' < -1*`tlim', -1*`tlim', `T'))
		qui count if abs(`T') == `tlim'
		loc delta = r(N)
	}

	qui	su `T' if `touse'
	loc extreme = max(abs(r(min)), r(max))/r(sd)
	loc action = cond("`winsor'"=="winsor", "winsorized", "trimmed")
	di _n "`delta' observations `action': max abs value of transformed variable = " %5.2f `extreme' " sigma" _n
	loc qq = cond("`bs'" == "bs", "qui", "noi")   /* <-- sortie ou pas sortie  pour bootstrap  */
	
	di ""
	di in red " ---- Proc Standard :  IVREGRESS 2SLS ------"
	di " `qq' ivregress 2sls `T' `exog' (`endog' = `iv') if `touse' "
	di ""
	
	`qq' ivregress 2sls `T' `exog' (`endog' = `iv') if `touse'

/* Modif le 28/07/2014 : Coefs stockés en fonction de la fenêtre  (ne marche qu'avec coefs enties !)  */ 
    local TrimE = int(10*`trim')
    local BandE = int(100*`band')
	eststo Md`BandE'T`TrimE', addscalars(band `band' trim `trim' obstrim `delta' )
	
	qui predict double `dxb' if e(sample), xb
	qui replace `dxb' =`dxb' + `vee' if e(sample)

	qui su `dxb'
	loc k2 = r(N)
	qui g double `h' = 0.9 * r(sd) * r(N)^(-1/5)
	qui g double `m' = .
	mata: indfn("`dxb'", "`D'", "`h'", "`m'", "`touse'")
	su `m' if `touse', mean
	

	loc enn = r(N)
	matrix `mfx' = r(mean) * [ 1, e(b) ]'
	matrix colnames `mfx' = D
	loc en : colnames e(b)
	matrix rownames `mfx' = `V' `en'
    
    
	if "`bs'" == "bs" {
    di _n " --- Marginal effects computed on `bsreps' bootstraps  for bandwidth = `band' and Trim = `trim' --- "
		mat `bee' = (`mfx')'
		matrix rownames `Vee' = `V' `en'
		matrix colnames `Vee' = `V' `en'
    	eret post `bee' `Vee', depname(`D') esample(`touse')
    	eret display  
    /* Sorties pour intégration dans modèle stoké (eststo) */
    
        matrix toto = r(table)
        local foo1 =  toto[1,2]
        local mfx1 = round(`foo1', 0.0001)
        local foo2 = toto[2,2]
        local mfxse = round(`foo2', 0.001)
        estadd local  mfx `mfx1' : Md`BandE'T`TrimE'
        estadd local  mfxse `mfxse' : Md`BandE'T`TrimE'
        
 
	}
	else {
		mat li `mfx', ti("Marginal effects at the mean, average index function")



/* essai de sortie de matrice (11/08/2014) */
        matrix toto = `mfx'
        local BB = int(`band'*100)
        local TT = int(`trim'*10)
        if "`band'" != "999" {
            matrix colnames toto = B`BB'T`TT' 
        }
        else {
             matrix colnames toto = Ordered 
        }
        local Rows : rownames toto 
        matrix Meff = Meff , toto
        matrix rowname Meff = `Rows'
        
	}
	eret local cmdname = "sspecialreg"
	eret scalar N = `enn'
	eret scalar trim = `trim'
    

	
end

	version 11
	mata:
	void indfn(string scalar sdxb,
			   string scalar sd,
	           string scalar sh,
	           string scalar sm,
	           string scalar touse)
	{
		real scalar k2, j, zt, skd
		real colvector kd, em, num
		st_view(dxb, ., sdxb, touse)
		st_view(D, ., sd, touse)
		st_view(h, ., sh, touse)
		st_view(m, ., sm, touse)
		
		k2 = rows(dxb)
		kd = J(k2, 1, .)
		for(j=1; j<=k2; j++) {
			zt = (dxb :- dxb[j, 1]) :/ h
			kd =  0.75 :* ( 1 :- zt :^2) :* (abs(zt) :< 1 )
			skd = sum(kd)
			em = D :- sum(kd :* D) / skd
			num = em :* ( 0.75 :* (1 :+ 2 :* zt) ) :* (abs(zt) :< 1 )
			m[j, 1] = sum(num) :/ (h[j, 1] :* skd ) 
		}
	}
	end
	exit
