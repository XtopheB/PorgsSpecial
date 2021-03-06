*! sspecialreg  cfb B526 from SimpleSpecial.ado in CleanedCodesApr2010 and SimpleStata2010
*! version 1.1.0: provide for bootstrapped SEs
*! version 1.1.1: Mata translation of sorted data density, Jann kdens
*! version 1.1.2: Return proper point estimates of marginal effects 
*! version 1.1.3: ssortedfm->Ben Jann's sddens_bj; add winsor option, avg index fn->Mata indfn
*! version 1.1.4: check to see whether _kdens is installed

* to do: add aweights per Austin Nichols' suggestion? Not sure that cluster VCE would be of any use

/* On ne supprime pas pour un stockage des résultats */
*prog drop _all
program define sspecialregK, eclass
version 11.0
syntax varlist(numeric min=2 max=2)  [if] [in], ENDOG(string) IV(string) ///
              [EXOG(string) HETERO HETV(string) KDENS KER(string) WINSOR TRIM(real 2.5) BS BSREPS(integer 10)] 

di""
di  in red  " Proc sspecialregK.ado - modified with choice of kernel :  `ker' "
di""

// check for _kdens
	capt which _kdens
	if _rc != 0 {
		di as err _n "You must install the kdens package, via ssc install kdens"
		error 198
	}
// require endog and iv lists
	marksample touse
	markout `touse' `endog' `iv' `exog' `hetv' 
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
// logic for bs option
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
		di _n "Computing bootstrap standard errors for marginal effects, `bsreps' bootstrap samples"
		qui bootstrap `bscalc', reps(`bsreps'): /// // saving(testbs, replace): ///	
		_sspecialreg `varlist', touse(`touse') endog(`endog') iv(`iv') exog(`exog')  hetero(`het') ///
			hetv(`hv') kdens(`kd') trim(`trim') winsor(`winsor')
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
/* K can take any of the value allowed by kdens   (CB 08/04/2015)    */
	if "`kdens'" == "kdens" {

		qui _kdens `uhat' if `touse', kernel(`ker') at(`uhat') gen(`fuhat') 
	} 
	else {
	qui ssortedfm `uhat' if `touse', gen(`fuhat')	
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
	loc qq = cond("`bs'" == "bs", "qui", "noi")
	`qq' ivregress 2sls `T' `exog' (`endog' = `iv') if `touse'
	qui predict double `dxb' if e(sample), xb
	qui replace `dxb' =`dxb' + `vee' if e(sample)

	qui su `dxb'
	loc k2 = r(N)
	qui g double `h' = 0.9 * r(sd) * r(N)^(-1/5)
	qui g double `m' = .
	mata: indfn("`dxb'", "`D'", "`h'", "`m'", "`touse'")
	su `m' if `touse', mean
	
//  following rewritten in Mata indfn
/*	forv j = 1/`k2' {
		scalar `dxb0' = `dxb' in `j'
		qui gen double `kd' = 0.75 * ( 1 - ((`dxb' - `dxb0') / `h' )^2) * (abs(( `dxb' - `dxb0') / `h' ) < 1 )
		su `kd', mean
		loc sumkd = r(sum)
		qui g double `dk' = `D' * `kd'
		su `dk', mean
		qui sca `em' = r(sum) / `sumkd'
		qui g double `num' = (`D' - `em') * ( 0.75 * (1 + 2 * (`dxb' - `dxb0') / `h') ) * (abs((`dxb' - `dxb0') / `h' ) < 1 )
		su `num', mean
		qui replace `m' = r(sum) / (`h' * `sumkd') in `j'
		cap drop `kd' `dk' `num' 
	}
	di "ado"
	sum `m' // , mean
*/
	loc enn = r(N)
	matrix `mfx' = r(mean) * [ 1, e(b) ]'
	matrix colnames `mfx' = D
	loc en : colnames e(b)
	matrix rownames `mfx' = `V' `en'

	if "`bs'" == "bs" {
		mat `bee' = (`mfx')'
		matrix rownames `Vee' = `V' `en'
		matrix colnames `Vee' = `V' `en'
    	eret post `bee' `Vee', depname(`D') esample(`touse')
    	eret display  
	}
	else {
		mat li `mfx', ti("Marginal effects at the mean, average index function")
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
