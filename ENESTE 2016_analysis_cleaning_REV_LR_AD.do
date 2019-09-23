/*-------------------------------------------------------------------------------------------------------------------|
/                                                                                                                    |
|  Economic Update (Gender), Cote d'Ivoire                                                                     		 |
|  This do-file creates the source of all tables for the gender analysis - interest variable: HH head gender		 |
|  Data Source	: ENV 2015 dataset   						  														 |
|  Created by	: Gabriel Lawin 																					 |
|  Date		 	: 28 April 2017																					  	 |
|  Modified on	: 29 April 2017           																		 	 /
|__________________________________________________________________________________________________________________*/

*** Options **********************************************************************************************************
clear 
clear matrix 
clear mata
eststo clear 
set more off
set maxvar 10000
set matsize 10000, permanently

*** Set directory ****************************************************************************************************
* Gabriel
if c(username)=="kotch" {
	cd "C:\Users\kotch\Box\Economic Update CIV\Analysis"
}
* Lea
if c(username)=="WB459476" {
	cd "C:\Users\wb459476\Box Sync\Economic Update CIV\Analysis"
}
* Aletheia
if c(username)=="wb468235" {
	cd "C:\Users\wb468235\WBG\Lea Marie Rouanet - Economic Update CIV\Analysis"
}

*** Set paths ********************************************************************************************************
global rawext			.\Data\CIV-Emploi_2016\Data
global cleanext			.\Data\CIV-Emploi_2016\cleaned
global tableext			.\Tables
global logext			.\Log files


*Set dataset prefix
global bprefix		BsCl

* logfile
cap log close
log using "${logext}\Construction_Base_ENESTE2016.log", replace
* tempfiles
tempfile labor_plot labor_ind prod_sales educ agwork credit agindex temp1 temp2 temp3 temp4 educ_head agwork_head ponderation
***BEGIN PROGRAM******************************************************************************************************

cap program drop GetOutlier
program define GetOutlier
	args var
	
cap confirm label foutlier
if !_rc {
	label define foutlier 0 "Normal" 1 "Probable Outlier" 2 "Definite Outlier"
}

gen outlier_`var'=0
label value outlier_`var' foutlier

end
*** END PROGRAM*********************************************************************************************************

// section HH roster

use "${rawext}/base_ensesi_2016_finale2_11112016", clear 
 keep hh1 hh2 pondf
 duplicates drop hh1 hh2, force
 save `ponderation', replace 
 
use "${rawext}/caracteristiqu_individuel_menage", clear
numlabel, add mask("[#]  ")

	recode a5 (1/4=1 "Marié")(5/9=2 "Non marié"),gen(situation_matri)
	label var situation_matri		"Situation matrimoniale"
	recode a8a a8b a8c a8d a8e a8f a8g (2=0)
	egen handicap=rowtotal(a8a a8b a8c a8d a8e a8f a8g)
	replace handicap=1 if handicap>1
	label var handicap			"Propriétaire souffrant d'un quelconque handicape (1=oui)"

	rename (a0 a1 a2 a4a a7) (idcode sexe lienchef age  religion)

save `temp1', replace
	
*-----Education
use "${rawext}/education", clear
numlabel, add mask("[#]  ")

	recode dq7b (15=12) (16=13) (17=14) (18 19=15) (20=16)(21=17) (22=18) (23=2) (24 99 .=0), gen(annees_education)
	replace annees_education =19 if inlist(dq9,8,17,18)
	replace annees_education =22 if dq9==9
	label var annees_education "Nombre d'années de scolarisation"

* Niveau d'education vs binaire
	recode annees_education (0=1 "Sans scolarisation")(1/7=2 "Niveau primaire")(8/14=3 "Niveau secondaire")(15/22=4 "Niveau universitaire"),gen(education_niveau)
	tab education_niveau, gen(education_dummy)
	label var education_niveau "Niveau de scolarisation"
	label var education_dummy1 "Sans scolarisation"
	label var education_dummy2 "Niveau primaire"
	label var education_dummy3 "Niveau secondaire"
	label var education_dummy4 "Niveau universitaire"
	keep hh1 hh2 dqid annees_education education_niveau education_dummy*
	rename dqid idcode

merge 1:1 	hh1 hh2 idcode using `temp1'
	drop _merge
	label data "ENESTE 2016 : COMPOSITION DU MENAGE"
save "${cleanext}/${bprefix}_Composition Menage", replace


preserve
		keep hh1 hh2 idcode lienchef religion a5 situation_matri handicap annees_education education_niveau education_dummy*
		keep if lienchef ==1
		label var handicap			"Chef de ménage souffrant d'un quelconque handicap (1=oui)"
	
rename (handicap annees_education education_niveau education_dummy1 education_dummy2 education_dummy3 education_dummy4) ///
	   (hh_handicap hh_annees_education hh_education_niveau hh_education_dummy1 hh_education_dummy2 hh_education_dummy3 hh_education_dummy4)
	   
save `temp1', replace
restore
*-------- Charactéristique du ménage

	// Create a binary head of household variable to make percent female easy to compute
		gen fhead=(lienchef==1 & sexe==2)

	// Age groups
		recode age (0=1)(1/3=2)(4/6=3)(7/9=4)(10/12=5)(13/15=6) (16/19=7) (20/50=8)(nonmissing=9) (missing=.), gen(agegroup2)
		label define fagegroup2 1 "Ages 0" 	2 "Ages 1 through 3" 3 "Ages 4 through 6" 4 "Ages 7 through 9" 5 "Ages 10 through 12" ///
							6 "Ages 13 through 15"  7 "Ages 16 through 19" 8 "Ages 20 through 50" 9 "Ages 51 and older"
		label values agegroup2 fagegroup2

	//create variable to generate dependency ratio
		recode age  (0/14=1) (15/65=2) (nonmissing=3) (missing=.), gen(ageratio)
		gen zchild=1 if inlist(ageratio,1,3)
		gen zadult=1 if ageratio==2

	// HH head age
		gen headage=age  if lienchef==1

	// Setup variables for calculating HH size and adult equivalents
	/* The adult equivalent measures used are based the standard FAO adult equivalent scales
	developed in Guinea in 2004, and are therefore considered more relevant to the West African context.*/ 
		gen zhhA=1
		gen zaeA=0.27 		if agegroup2==1
		replace zaeA=.45 	if agegroup2==2
		replace zaeA=.61 	if agegroup2==3
		replace zaeA=.73 	if agegroup2==4
		replace zaeA=.86 	if agegroup2==5& sexe==1
		replace zaeA=.73 	if agegroup2==5& sexe==2
		replace zaeA=.96 	if agegroup2==6& sexe==1
		replace zaeA=.83 	if agegroup2==6& sexe==2
		replace zaeA=1.02 	if agegroup2==7& sexe==1
		replace zaeA=.77 	if agegroup2==7& sexe==2
		replace zaeA=1	 	if agegroup2==8& sexe==1
		replace zaeA=.77 	if agegroup2==8& sexe==2
		replace zaeA=.86 	if agegroup2==9& sexe==1
		replace zaeA=.79 	if agegroup2==9& sexe==2

	
	// Aggregate
	collapse (sum) z* headage (max) zfhead=fhead, by(hh1 hh2)
		gen dependency=zchild/zadult
		recode dependency (missing=0)
		recode zfhead (1=2 "Féminin")(0=1 "Masculin"),gen(sexe_du_chef) 
		
		
		label var zhhA					"Taille du ménage"
		label var zchild 				"Nombre de dépendants"
		label var zadult 				"Nombre de membres en âge de travailler (15-65 ans)"
		label var zaeA 					"Taille du ménage en équivalent adulte" 
		label var headage				"Age du chef du ménage"
		label var zfhead				"Femme chef de ménage"
		label var dependency			"Ratio de dépendance"
		label var sexe_du_chef			"Sexe du chef du ménage"
		
merge 1:1 hh1 hh2 using `temp1'
	drop _merge
merge 1:1 hh1 hh2 using "${rawext}/informations_generales" ,keepusing(b04_district b05_region b06_departemen b07_souspref b08_commune b09_zd b10_nomvillag b11_quartcpt b12_ilot)
	drop _merge
	order hh1 hh2 idcode b04_district b05_region b06_departemen b07_souspref b08_commune b09_zd b10_nomvillag b11_quartcpt b12_ilot
numlabel, add mask("[#]  ")	
	label data "ENESTE 2016 : CHARACTERISTIQUE DU MENAGE-UNIQUE AT HH LEVEL"
save "${cleanext}/${bprefix}_Characteristique Menage", replace	


// Indice de possesion d'équipements agricoles

use "${rawext}/mecanisation_agricole", clear

recode agri411 agri412 agri413 agri414 agri415 agri416 agri417 agri418 agri419 agri4110 agri4111 agri4112 agri4113 agri4114 (2 3=0)
	keep hh1 hh2 agri411 agri412 agri413 agri414 agri415 agri416 agri417 agri418 agri419 agri4110 agri4111 agri4112 agri4113 agri4114 

global agric_index agri411 agri412 agri413 agri414 agri415 agri416 agri417 agri418 agri419 agri4110 agri4111 agri4112 agri4113 agri4114 
		factor $agric_index , pcf
		predict agric_index
		label var  agric_index		 "Indice de possession d'équipements agricoles"
	keep hh1 hh2 agric_index
save `agindex', replace

// Pas d'information sur le crédit

// section parcelle and input

use "${rawext}/culture", clear
numlabel, add mask("[#]  ")

	* Superficie de la parcelle en ha
	
	recode h9a ( 99 999 99.9999 999999 = .)
	replace h9a=600 if h9a==-600
	gen plotsize_ha=h9a 
	replace plotsize_h= h9a/10000 if h9b==1
	label var plotsize_ha		"Superficie de la parcelle en ha"
	replace plotsize_ha=plotsize_ha/10000 if plotsize_ha>=100 & !missing(plotsize_ha)
	replace plotsize_ha=plotsize_ha*10000 if plotsize_ha< 0.01
	recode plotsize_ha(99=.)
	gen unite_superficie_brute=h9b==1
	label var unite_superficie_brute		"Unité de mesure de la superficie de la base brute, 1= m2"
	recode plotsize_ha (0=.x)

* Mode occupation de la parcelle
	tab h5, gen(mode_occupe)
	
* Type de droit
  
	  recode h7 (1=1)(2=2)(3 4 6 7 =3)(5 8 . =4), gen(xp)
	  tab xp,gen(type_droit)
	  	  
	  label var type_droit1		"Titre foncier"
	  label var type_droit2		"Droit coutumier" 
	  label var type_droit3		"Attestation de vente ou autres documents"
	  label var type_droit4		"Aucun document"
  
	  gen tenure_security=0
	  replace tenure_security=1 if inlist(xp,1,2,3)
	  label var tenure_security			"Titre foncier/attestation de vente ou autres documents"
	  drop xp
	  
	* Main d'oeuvre sur les parcelles: # de personnes
	recode h12aa1 h12ab1 h12ac1 h12bd1 h12be1 h12bf1 h12cg1 h12ch1 h12ci1  h12aa2 h12ab2 h12ac2 h12bd2 h12be2 h12bf2 h12cg2 h12ch2 h12ci2 h13a h14a h13b h14b (99 999 9999 99999 999.99=.)
	egen hhlabor_count		= rownonmiss(h12aa1 h12ab1 h12ac1 h12bd1 h12be1 h12bf1 h12cg1 h12ch1 h12ci1)	// Main d'oeuvre familiale
	gen hiredlabor_count	= h13a											// Main d'oeuvre salarié
	gen sharedlabor_count   = h14a											// Main d'oeuvre d'entraide	
	egen labor_all			=rowtotal(hhlabor_count hiredlabor_count sharedlabor_count) 	// Main d'oeuvre totale
 
	egen hhlabor_jr			=rowtotal(h12aa2 h12ab2 h12ac2 h12bd2 h12be2 h12bf2 h12cg2 h12ch2 h12ci2)		// main d'oeuvre familiale en personne-jours
	* Main d'oeuvre remunéré et entraide en parsonne-jour 
	 	foreach xvar of varlist h13a h14a h13b h14b plotsize_ha hiredlabor_count sharedlabor_count hhlabor_count hhlabor_jr ///
		h12aa2 h12ab2 h12ac2 h12bd2 h12be2 h12bf2 h12cg2 h12ch2 h12ci2 {
	GetOutlier `xvar'
	centile `xvar', centile(98)
	local p_99_`xvar'=r(c_1)
	replace outlier_`xvar'=2 if `xvar'> `p_99_`xvar''&!missing(`xvar')
	replace `xvar'= `p_99_`xvar'' if outlier_`xvar'==2
	}
recode h13a h14a h13b h14b  hiredlabor_count sharedlabor_count (missing=0)
drop outlier_*
	 gen hiredlabor_jr= h13a*h13b
	 gen sharedlabor_jr=h14a*h14b
	 

	 label var hhlabor_count 		"Main d'oeuvre familiale- # pers"
	 label var hiredlabor_count 	"Main d'oeuvre salarié - # pers"
	 label var sharedlabor_count	"Main d'oeuvre d'entraide -# pers"
	 label var labor_all			"Main d'oeuvre totale - # pers"
	 label var hhlabor_jr			"Main d'oeuvre familiale en personne-jour"
	 label var hiredlabor_jr		"Main d'oeuvre rémunérée en personne-jour"
	 label var sharedlabor_jr		"Main d'oeuvre non rémunérée en personne-jour"
// main d'oeuvre familiale désagrégée par homme/femme/enfant
preserve
	keep hh1 hh2 h03 h8id h12aa1 h12ab1 h12ac1 h12bd1 h12be1 h12bf1 h12cg1 h12ch1 h12ci1 h12aa2 h12ab2 h12ac2 h12bd2 h12be2 h12bf2 h12cg2 h12ch2 h12ci2
	rename (h12aa1 h12ab1 h12ac1 h12bd1 h12be1 h12bf1 h12cg1 h12ch1 h12ci1				h12aa2 h12ab2 h12ac2 h12bd2 h12be2 h12bf2 h12cg2 h12ch2 h12ci2) ///
		  (membre1 membre2 membre3 membre4  membre5  membre6  membre7  membre8  membre9 nbre_jr1 nbre_jr2 nbre_jr3  nbre_jr4 nbre_jr5 nbre_jr6 nbre_jr7 nbre_jr8 nbre_jr9)
	reshape long membre nbre_jr ,i(hh1 hh2 h03 h8id) j(obs)
	drop obs
	rename membre idcode
merge m:1 hh1 hh2 idcode using "${cleanext}/${bprefix}_Composition Menage"
		drop if _merge==2
		drop _merge
	gen hhlabor_H= 1 if sexe==1 & age>= 12& !missing(age)		// Main d'oeuvre familiale des hommes
	gen hhlabor_F= 1 if sexe==2 & age>= 12& !missing(age)		// Main d'oeuvre famililale des femmes
	gen hhlabor_E= 1 if 		  age< 12							// Main d'oeuvre familiale des enfants (moins de 12 ans pour etre consistant avec l'exemple de do file fourni)
	
	recode hhlabor_H  hhlabor_F hhlabor_E (missing=1) if idcode==98			// le cas ou tout le ménage a travaillé
	recode hhlabor_H  hhlabor_F hhlabor_E (missing=0)
	* Nombre de jour travaillé
	gen hhlabor_Hjr= nbre_jr if sexe==1 & age>= 12& !missing(age)		// Main d'oeuvre familiale des hommes
	gen hhlabor_Fjr= nbre_jr if sexe==2 & age>= 12& !missing(age)		// Main d'oeuvre famililale des femmes
	gen hhlabor_Ejr= nbre_jr if 		  age< 12							// Main d'oeuvre familiale des enfants (moins de 12 ans pour etre consistant avec l'exemple de do file fourni)
	
	gen hhlabor_HY_jr= nbre_jr if sexe==1 & age>= 15 & age<=35		// Main d'oeuvre familiale- homme jeune
	gen hhlabor_HS_jr= nbre_jr if sexe==1 & age>35 & !missing(age)	// Main d'oeuvre familiale- homme agé
	gen hhlabor_FY_jr= nbre_jr if sexe==2 & age>= 15 & age<=35		// Main d'oeuvre familiale- femme jeune
	gen hhlabor_FS_jr= nbre_jr if sexe==2 & age>35 & !missing(age)	// Main d'oeuvre familiale- femme agée
	gen hhlabor_EC_jr= nbre_jr if 		  age< 15							// Main d'oeuvre familiale des enfants (moins de 12 ans pour etre consistant avec l'exemple de do file fourni)
	
	replace hhlabor_Hjr = nbre_jr if missing(hhlabor_Hjr)& idcode==98			// le cas ou tout le ménage a travaillé
	replace hhlabor_Fjr = nbre_jr if missing(hhlabor_Fjr)& idcode==98
	replace hhlabor_Ejr = nbre_jr if missing(hhlabor_Ejr)& idcode==98
	
	
	replace hhlabor_HY_jr = nbre_jr if missing(hhlabor_HY_jr)& idcode==98			// le cas ou tout le ménage a travaillé
	replace hhlabor_HS_jr = nbre_jr if missing(hhlabor_HS_jr)& idcode==98
	replace hhlabor_FY_jr = nbre_jr if missing(hhlabor_FY_jr)& idcode==98
	replace hhlabor_FS_jr = nbre_jr if missing(hhlabor_FS_jr)& idcode==98			// le cas ou tout le ménage a travaillé
	replace hhlabor_EC_jr = nbre_jr if missing(hhlabor_EC_jr)& idcode==98
	
	
	recode hhlabor_Hjr  hhlabor_Fjr hhlabor_Ejr hhlabor_HY_jr hhlabor_HS_jr hhlabor_FY_jr hhlabor_FS_jr hhlabor_EC_jr(missing=0)
save "${cleanext}/${bprefix}_Travail_parcelle", replace	
	
collapse (sum) hhlabor_H  hhlabor_F hhlabor_E hhlabor_Hjr  hhlabor_Fjr hhlabor_Ejr hhlabor_HY_jr hhlabor_HS_jr hhlabor_FY_jr hhlabor_FS_jr hhlabor_EC_jr (first) idcode, by(hh1 hh2 h03 h8id) 
	
	label var hhlabor_H			"Main d'oeuvre familiale des hommes  - # pers"  
	label var hhlabor_F 		"Main d'oeuvre familiale des femmes  - # pers"
	label var hhlabor_E 		"Main d'oeuvre familiale des enfants - # pers"
	
	label var hhlabor_Hjr		"Main d'oeuvre familiale des hommes  - # jours"  
	label var hhlabor_Fjr 		"Main d'oeuvre familiale des femmes  - # jours"
	label var hhlabor_Ejr 		"Main d'oeuvre familiale des enfants - # jours"
	
	label var hhlabor_HY_jr		"Main d'oeuvre familiale (Jeunes hommes<=35 ans)  - # jours"  
	label var hhlabor_FY_jr 		"Main d'oeuvre familiale (Jeunes femmes)  - # jours"
	label var hhlabor_EC_jr 		"Main d'oeuvre familiale des enfants (moins de 15 ans) - # jours"
	label var hhlabor_HS_jr		"Main d'oeuvre familiale (hommes agés)  - # jours"  
	label var hhlabor_FS_jr 		"Main d'oeuvre familiale (femmes agées)  - # jours"
	
	
save `temp1', replace

restore,


merge 1:1 hh1 hh2 h03 h8id using `temp1'
		drop _merge
recode hhlabor_* (missing=0)

* Acces à l'irrigation
	 gen irrigation=inlist(h15,1,2,3,4,6)
	 label var irrigation		"Irrigation"
	 
* Utilisation intrant
	gen engrais_org=h16==1			//engrais organique
	gen engrais_chim=h17==1			//engrais chimique
	gen pesticide=h18==1			//pesticide
	
	label var engrais_org		"Engrais organique"
	label var engrais_chim		"Engrais chimique"
	label var pesticide			"Pesticide"
	
* Nombre total de parcelles dans le ménage
	gen xz=1
	bysort hh1 hh2 : egen nbre_parcelle_hh=sum(xz)
	drop xz
	label var nbre_parcelle_hh			"Nombre total de parcelles du ménage"

* Nombre de producteurs dans le ménage 
	preserve
	gen xz=1
	collapse (max) xz, by(hh1 hh2 idcode)
	collapse (sum) nbr_producteur=xz, by(hh1 hh2)
	label var nbr_producteur			"Nombre de travailleurs dans le ménage"
save `temp2', replace
	restore
merge m:1 hh1 hh2 using `temp2'
	drop _merge
	
	
	label data "ENESTE 2016 : PARCELLE & INPUT-UNIQUE AT HH -PLOT LEVEL"
save "${cleanext}/${bprefix}_PlotDetail", replace	
 
 
 collapse (mean) plotsize_ha-hhlabor_EC_jr irrigation-nbr_producteur , by(hh1 hh2)
	recode plotsize_ha (0=.)
	
	gen hhlabor_jrha		=hhlabor_jr/plotsize_ha
	gen hiredlabor_jrha		=hiredlabor_jr/plotsize_ha
	gen sharedlabor_jrha	=sharedlabor_jr/plotsize_ha
	
	
	label var plotsize_ha		"Superficie moyenne des parcelles en ha"
	label var type_droit1		"Titre foncier"
	label var type_droit2		"Droit coutumier" 
	label var type_droit3		"Attestation de vente ou autres documents"
	label var type_droit4		"Aucun document"
	label var tenure_security	"Titre foncier/attestation de vente ou autres documents"
	label var hhlabor_count 		"Main d'oeuvre familiale- # pers"
	
		 label var hiredlabor_count 	"Main d'oeuvre salarié - # pers"
		 label var sharedlabor_count	"Main d'oeuvre d'entraide -# pers"
		 label var labor_all			"Main d'oeuvre totale - # pers"
		 label var hhlabor_jr			"Main d'oeuvre familiale en personne-jour"
		 label var hhlabor_jrha			"Main d'oeuvre familiale (personne-jour/ha)"
		 label var hhlabor_H			"Main d'oeuvre familiale des hommes  - # pers"  
		 label var hhlabor_F 		"Main d'oeuvre familiale des femmes  - # pers"
		 label var hhlabor_E 		"Main d'oeuvre familiale des enfants - # pers"
		 label var hiredlabor_jr	"Main d'oeuvre rémunérée en personne-jour"
		 label var sharedlabor_jr	"Main d'oeuvre d'entraide en personne-jour"
		 label var hiredlabor_jrha	"Main d'oeuvre rémunérée (personne-jour/ha)"
		 label var sharedlabor_jrha	"Main d'oeuvre non rémunérée (personne-jour/ha)"
		 label var hhlabor_HY_jr		"Main d'oeuvre familiale (Jeunes hommes<=35 ans)  - # jours"  
	label var hhlabor_FY_jr 		"Main d'oeuvre familiale (Jeunes femmes)  - # jours"
	label var hhlabor_EC_jr 		"Main d'oeuvre familiale des enfants (moins de 15 ans) - # jours"
	label var hhlabor_HS_jr		"Main d'oeuvre familiale (hommes agés)  - # jours"  
	label var hhlabor_FS_jr 		"Main d'oeuvre familiale (femmes agées)  - # jours"
	
	label var irrigation		"Irrigation"
	label var engrais_org		"Engrais organique"
	label var engrais_chim		"Engrais chimique"
	label var pesticide			"Pesticide"
	label var nbre_parcelle_hh			"Nombre total de parcelles du ménage"
	label var nbr_producteur			"Nombre de producteurs dans le ménage"
	 
	 label var mode_occupe1 		"Propriétaire"
	 label var mode_occupe2 		"Locataire"
	 label var mode_occupe3 		"Hypothèque (mise en gage)"
	 label var mode_occupe4 		"Prêt (gratuit)"
	 label var mode_occupe5			"Autre"
	
label data "ENESTE 2016 : PARCELLE & INPUT-UNIQUE AT HH LEVEL"
save "${cleanext}/${bprefix}_PlotDetail_HH_level", replace	
 
  
// Production par culture


use "${rawext}/production_par_type_cultures", clear
numlabel, add mask("[#]  ")
merge m:1 hh1 hh2 using "${rawext}/informations_generales" ,keepusing(b04_district b05_region b06_departemen)
	drop  if _merge==2
	drop _merge
	drop if missing( h19b_codeculture)			// Toutes les valeurs sont manquantes
	recode h19b_codeculture (45=38)
	
*** Superficie par culture
recode h19b1 (99999=.)
replace h19b2= 1 if h19b2==2&h19b1>=100
replace h19b2= 2 if h19b2==1&h19b1<=10

	gen superficie_ha_culture=h19b1
	replace superficie_ha_culture=h19b1/10000 if h19b2==1
	
	
preserve
	gen xw=1
	bysort hh1 hh2 h3iid h8iid: egen association_culture=sum(xw)	   // Association de culture
	recode association_culture (1=0)(nonmissing=1)
	label var association_culture		"Pratique de culture associée sur les parcelle (1=oui)"
	drop xw
	collapse (mean) association_culture,by(hh1 hh2)
	label var association_culture		"Pratique de culture associée (% de parcelles)"
save `temp2', replace
restore, preserve
	gen xw=0
	replace xw= 1 if inlist(h19b_codeculture,1,8,9,16,22,31)
	gen foodcrop=xw==0
	collapse (max) culture_rente=xw export_dummy=xw foodcrop,by(hh1 hh2 h3iid h8iid)
	collapse (mean) culture_rente (max) export_dummy foodcrop,by(hh1 hh2)
	label var culture_rente		"Proportion de parcelle avec culture de rente"
	label var foodcrop				"Food crops dummy"
	label var export_dummy			"Export crops dummy (1 if Household produced any export crop)"
	
 save `temp3', replace

restore, preserve
	
	collapse (sum) superficie_ha_culture, by(hh1 hh2 h19b_codeculture)
	
	recode superficie_ha_culture(0=.)
	
	reshape wide superficie_ha_culture, i(hh1 hh2) j(h19b_codeculture)
	egen superficie_ha_total=rowtotal(superficie_ha_culture*),mi
	egen nbre_culture= rownonmiss(superficie_ha_culture*)
/* Cleaning farm size	
	GetOutlier superficie_ha_total
		egen P_99=pctile(superficie_ha_total), p(99)
		replace outlier_superficie_ha_total=2 if superficie_ha_total> P_99 &!missing(superficie_ha_total)
		replace superficie_ha_total=P_99 if  outlier_superficie_ha_total==2
		
		egen P_05=pctile(superficie_ha_total), p(5)
		replace outlier_superficie_ha_total=1 if superficie_ha_total< P_99 
		replace superficie_ha_total=P_05 if  outlier_superficie_ha_total==1
		drop  P_99 P_05
*/		
merge 1:1 hh1 hh2  using `temp2'
	drop _merge
	
merge 1:1 hh1 hh2  using `temp3'
	drop _merge
	
	label var superficie_ha_total				"Superficie totale cultuvée en ha"
	label var nbre_culture						"Nombre de cultures"
	
	label data "ENESTE 2016 : CULTURE ET SUPERFICIE-UNIQUE AT HH LEVEL"
save "${cleanext}/${bprefix}_Culture_superficie", replace	
 
 restore
 
 // Facteur de conversion en kg
		recode h22a (9.9 99999 99999.99 0=.x)
		replace h22b=18 if h22b==19& h22a>=1000&!missing(h22a) // les valeurs en tonne>1000 sont considerées comme en kg
		gen Prod_conv_factor=.
		replace Prod_conv_factor= 1 		if h22b==18		// kilogramme
		replace Prod_conv_factor= 1000 		if h22b==19		// tonne en kg
		label var Prod_conv_factor			"Facteur de conversion de la production en kg "
		
		recode h22c (99 999999 0= .)
	* niveau departement
		gen xz=1 if !missing(h22c)
		bysort b06_departemen h19b_codeculture h22b: egen nbr_obs1= sum(xz)	
		bysort b06_departemen h19b_codeculture h22b: egen conv_kg_dep=median(h22c) if nbr_obs1>=10&!missing(nbr_obs1)
		
		
	* niveau region
		bysort b05_region h19b_codeculture h22b: egen nbr_obs2= sum(xz)	
		bysort b05_region h19b_codeculture h22b: egen conv_kg_reg=median(h22c) if nbr_obs2>=10&!missing(nbr_obs2)
		
	* niveau national
		bysort h19b_codeculture h22b: egen conv_kg_nat=median(h22c) 
		drop xz nbr_obs1 nbr_obs2
		
		replace Prod_conv_factor= conv_kg_dep if missing(Prod_conv_factor)
		replace Prod_conv_factor= conv_kg_reg if missing(Prod_conv_factor)& !missing(conv_kg_reg)
		replace Prod_conv_factor= conv_kg_nat if missing(Prod_conv_factor)& !missing(conv_kg_nat)
		drop conv_kg_dep conv_kg_reg conv_kg_nat
		replace Prod_conv_factor= 50 if missing(Prod_conv_factor)& h22b==4
		replace Prod_conv_factor= 100 if missing(Prod_conv_factor)& h22b==5
		
		egen mode_conv=mode(Prod_conv_factor), min by(h19b_codeculture)
		replace Prod_conv_factor=mode_conv if missing(Prod_conv_factor)
		drop mode_conv
// drop outlier		
	GetOutlier Prod_conv_factor
		egen P_98=pctile(Prod_conv_factor), p(98)
		replace outlier_Prod_conv_factor=2 if Prod_conv_factor> P_98 &!missing(Prod_conv_factor)
		replace Prod_conv_factor=.x if  outlier_Prod_conv_factor==2
		drop  P_98	
				gen prod_kg=h22a*Prod_conv_factor
				label var prod_kg  "Production en kg"
				
/*	GetOutlier prod_kg
		egen P_98=pctile(prod_kg), p(98)
		replace outlier_prod_kg=2 if prod_kg> P_98 &!missing(prod_kg)
		replace prod_kg=P_98 if  outlier_prod_kg==2
		drop  P_98
*/		
	*--- Prix de vente
		recode h25a h26(99999  99999.99 9999999 0 =.x)
		gen sales_kg= h25a*Prod_conv_factor		//quantité vendue en kg avec les facteurs de conversion disponible
		gen vente_cfa=h26
		label var 		vente_cfa		"Valeur des ventes en FCFA"
		label var 		sales_kg		"Quantité vendue en kg"
		
		
		* niveau departement
		gen xz=1 if !missing(vente_cfa)
		bysort b06_departemen h19b_codeculture : egen nbr_obs1= sum(xz)	
		bysort b06_departemen h19b_codeculture : egen prix_unitaire_depp=median(vente_cfa/sales_kg) if nbr_obs1>=10&!missing(nbr_obs1)
		
		
	* niveau region
		bysort b05_region h19b_codeculture : egen nbr_obs2= sum(xz)	
		bysort b05_region h19b_codeculture : egen prix_unitaire_reg=median(vente_cfa/sales_kg) if nbr_obs2>=10&!missing(nbr_obs2)
		
	* niveau national
		bysort h19b_codeculture : egen prix_unitaire_nat=median(vente_cfa/sales_kg) 
		drop xz nbr_obs1 nbr_obs2
		
		* Aggregation des prix
		gen vente_prix_unitaire = prix_unitaire_dep	
		replace vente_prix_unitaire=prix_unitaire_reg if missing(vente_prix_unitaire)
		replace vente_prix_unitaire=prix_unitaire_nat if missing(vente_prix_unitaire)
		
		label var vente_prix_unitaire		"Prix de vente par kg"
		replace vente_prix_unitaire= 400 	if missing(vente_prix_unitaire)& h19b_codeculture==35 		// pomme de terre

// drop outlier		
/*	GetOutlier vente_prix_unitaire
		egen P_98=pctile(vente_prix_unitaire), p(98)
		replace outlier_vente_prix_unitaire=2 if vente_prix_unitaire> P_98 &!missing(vente_prix_unitaire)
		replace vente_prix_unitaire=.x if  outlier_vente_prix_unitaire==2
		drop  P_98	*/
* valeur de la production en FCFA
	gen prod_cfa=prod_kg*vente_prix_unitaire
	label var prod_cfa 				"Valeur de la production en FCFA"

/*GetOutlier prod_cfa
		egen P_98=pctile(prod_cfa), p(98)
		replace outlier_prod_cfa=2 if prod_cfa> P_98 &!missing(prod_cfa)
		replace prod_cfa=P_98 if  outlier_prod_cfa==2
		drop  P_98*/
collapse (sum) prod_kg prod_cfa vente_cfa sales_kg ,by(hh1 hh2 h19b_codeculture)
	*recode prod_kg* prod_cfa* (0=.)

	reshape wide prod_kg prod_cfa vente_cfa sales_kg, i(hh1 hh2) j(h19b_codeculture)
	egen prod_kg=rowtotal(prod_kg*),mi
	egen prod_cfa=rowtotal(prod_cfa*),mi
	egen vente_cfa=rowtotal(vente_cfa*)
		
	label var prod_kg  				"Production en kg"
	label var prod_cfa  			"Production en FCFA"
	label var vente_cfa				"Valeur des ventes en FCFA"
	label data "ENESTE 2016 : PRODUCTION ET VENTE-UNIQUE AT HH LEVEL"
save "${cleanext}/${bprefix}_Production", replace	
 
 
 
 // Agregation des bases
 
 merge 1:1 hh1 hh2 using "${cleanext}/${bprefix}_Culture_superficie"
	drop _merge

 merge 1:1 hh1 hh2 using "${cleanext}/${bprefix}_Characteristique Menage"
	drop if _merge==2
	drop _merge
merge 1:1 hh1 hh2 using	 `agindex'
	drop if _merge==2
	drop _merge
	
merge 1:1 hh1 hh2 using "${cleanext}/${bprefix}_PlotDetail_HH_level"
	drop _merge
	
	
* Superficie culture de rente/ vivriere
	egen superficie_ha_rente= rowtotal(superficie_ha_culture1 superficie_ha_culture8 superficie_ha_culture9 superficie_ha_culture16 superficie_ha_culture22 superficie_ha_culture31)
	egen  superficie_ha_vivre= rowtotal(superficie_ha_culture2 superficie_ha_culture3 superficie_ha_culture4 superficie_ha_culture5 superficie_ha_culture6 superficie_ha_culture7 superficie_ha_culture10 ///
									superficie_ha_culture11 superficie_ha_culture13 superficie_ha_culture14 superficie_ha_culture15 superficie_ha_culture17 superficie_ha_culture18 superficie_ha_culture19 ///
									superficie_ha_culture21 superficie_ha_culture23 superficie_ha_culture24 superficie_ha_culture25 superficie_ha_culture26 superficie_ha_culture27 superficie_ha_culture28 ///
									superficie_ha_culture29 superficie_ha_culture30 superficie_ha_culture33 superficie_ha_culture34 superficie_ha_culture35 superficie_ha_culture36 superficie_ha_culture37 ///
									superficie_ha_culture38 superficie_ha_culture39 superficie_ha_culture40 superficie_ha_culture41 superficie_ha_culture42 superficie_ha_culture43 superficie_ha_culture46 ///
									superficie_ha_culture47 superficie_ha_culture48 superficie_ha_culture99)
	
	label var superficie_ha_vivre 		"Superficie totale en ha culture vivrière"
	label var superficie_ha_rente		"Superficie totale en ha culture de rente"
	
recode superficie_ha_vivre superficie_ha_rente superficie_ha_total(0=.x)
// BASIC CLEANING
	foreach xvar of varlist superficie_ha_vivre superficie_ha_rente superficie_ha_total {
	GetOutlier `xvar'
		egen P_98_`xvar'=pctile(`xvar'), p(98)
		replace outlier_`xvar'=2 if `xvar'> P_98_`xvar' &!missing(`xvar')
		replace `xvar'= .x if outlier_`xvar'==2
		* smallest plots had too high productivity
		egen P_03_`xvar'=pctile(`xvar'), p(3)
		replace outlier_`xvar'=1 if `xvar'< P_03_`xvar' &!missing(`xvar')
		replace `xvar'= .x if outlier_`xvar'==1
 }
 
 // production rente/vivre
	egen prod_cfa_rente =rowtotal(prod_cfa1 prod_cfa8 prod_cfa9 prod_cfa16 prod_cfa22 prod_cfa31)
	egen  prod_cfa_vivre=rowtotal(prod_cfa2 prod_cfa3 prod_cfa4 prod_cfa5 prod_cfa6 prod_cfa7 prod_cfa10 prod_cfa11 prod_cfa13 prod_cfa14 prod_cfa15 ///
								  prod_cfa16 prod_cfa17 prod_cfa18 prod_cfa19 prod_cfa21 prod_cfa23 prod_cfa24 prod_cfa25 prod_cfa26 prod_cfa27 prod_cfa28 ///
								  prod_cfa29 prod_cfa30 prod_cfa33 prod_cfa34 prod_cfa35 prod_cfa36 prod_cfa37 prod_cfa38 prod_cfa39 prod_cfa40 prod_cfa41 ///
								  prod_cfa42 prod_cfa43 prod_cfa46 prod_cfa47 prod_cfa48 prod_cfa99)
	
	
	
// BASIC CLEANING
	foreach xvar of varlist prod_cfa prod_cfa_rente prod_cfa_vivre  {
	GetOutlier `xvar'
		egen P_98_`xvar'=pctile(`xvar'), p(98)
		replace outlier_`xvar'=2 if `xvar'> P_98_`xvar' &!missing(`xvar')
		replace `xvar'= . if outlier_`xvar'==2
 }
	
// productivité 
	gen rendement=prod_kg/superficie_ha_total
	gen prodvt_cfa=prod_cfa/superficie_ha_total
	gen prodvt_cfa_rente=prod_cfa_rente/superficie_ha_rente
	gen prodvt_cfa_vivre=prod_cfa_vivre/superficie_ha_vivre
	label var rendement				"Productivité (kg/ha)"
	label var prodvt_cfa			"Productivité (FCFA/ha)"
	label var prodvt_cfa_rente		"Productivité (FCFA/ha)- Cultures de rente"
	label var prodvt_cfa_vivre		"Productivité (FCFA/ha)- Cultures vivrières"

recode prodvt_cfa prodvt_cfa_rente prodvt_cfa_vivre superficie_ha_total prod_cfa_rente prod_cfa_vivre superficie_ha_vivre  superficie_ha_rente(0=.x)


// BASIC CLEANING
	foreach xvar of varlist rendement prodvt_cfa prodvt_cfa_rente  {
	GetOutlier `xvar'
		egen P_01_`xvar'=pctile(`xvar'), p(1)
		egen P_98_`xvar'=pctile(`xvar'), p(98)
		replace outlier_`xvar'=1 if `xvar' < P_01_`xvar'
		replace outlier_`xvar'=2 if `xvar'> P_98_`xvar' &!missing(`xvar')
		
 }
 
// culture vivriere allez à 95%	
GetOutlier prodvt_cfa_vivre
egen P_01_prodvt_cfa_vivre=pctile(prodvt_cfa_vivre), p(1)
		egen P_95_prodvt_cfa_vivre=pctile(prodvt_cfa_vivre), p(95)
		replace outlier_prodvt_cfa_vivre=1 if prodvt_cfa_vivre < P_01_prodvt_cfa_vivre
		replace outlier_prodvt_cfa_vivre=2 if prodvt_cfa_vivre> P_95_prodvt_cfa_vivre &!missing(prodvt_cfa_vivre)


		
// Main d'oeuvre et depense en intrant
	bysort sexe_du_chef: egen p_50 =pctile(plotsize_ha), p(50)
	replace plotsize_ha=p_50 if missing(plotsize_ha)&!missing(hhlabor_jr) 
	replace hhlabor_jrha		=hhlabor_jr/plotsize_ha
	replace hiredlabor_jrha		=hiredlabor_jr/plotsize_ha
	replace sharedlabor_jrha	=sharedlabor_jr/plotsize_ha
	
	*sum hiredlabor_H-  sharedlabor_E hhlabor_count -hhlabor_jr depense_intrant h20a h20b h20c,det
	foreach xvar of varlist hhlabor_count-hhlabor_E hhlabor_jrha hiredlabor_jrha sharedlabor_jrha{
	GetOutlier `xvar'
	centile `xvar', centile(99)
	local p_99_`xvar'=r(c_1)
	replace outlier_`xvar'=2 if `xvar'> `p_99_`xvar''&!missing(`xvar')
	replace `xvar'= `p_99_`xvar'' if outlier_`xvar'==2
	}

//Log Productivity = valeur brute de la production en FCFA/superficie
 foreach xvar of varlist rendement prodvt_cfa superficie_ha_total  plotsize_ha labor_all prod_kg prod_cfa prodvt_cfa_rente prodvt_cfa_vivre ///
						superficie_ha_vivre superficie_ha_rente {
		 gen log_`xvar'=log(`xvar')
		 }
	 label var log_prodvt_cfa			"Log (Productivité(FCFA/ha))"
	 label var log_plotsize_ha			"Log (superficie moyenne des parcelles en ha)"
	 label var log_labor_all			"Log (Main d'oeuvre totale-# de personnes)"
	 label var log_rendement			"Log (productivité (Kg/ha))"
	 label var log_prod_kg		 		"Log (Valeur de la production en kg)"
	 label var log_prod_cfa				"Log (Valeur de la production en FCFA)"
	 label var log_superficie_ha_total	"Log (superficie totale cultivée par le ménage en ha)"
	 label var log_prodvt_cfa_rente		"Log (Productivité(FCFA/ha))-Cultures de rente"
	 label var log_prodvt_cfa_vivre		"Log (Productivité(FCFA/ha))-Culture vivrière"
	 label var log_superficie_ha_rente	"Log (superficie totale cultivée en ha- Cultures de rente)"
	 label var log_superficie_ha_vivre	"Log (superficie totale cultivée en ha- Cultures vivrière)"
	 
	gen age_sqrt= headage^ 2
	label var age_sqrt	"Age du chef du ménange au carré"
	merge 1:1 hh1 hh2 using `ponderation'
	drop if _merge==2
	drop _merge
	order idcode-agric_index pondf ,after(hh2)
	
	* part de la production vendue
	
	egen vente_cfa_rente= rowtotal(vente_cfa1 vente_cfa8 vente_cfa9 vente_cfa16 vente_cfa22 vente_cfa31)
	gen vente_cfa_vivre= vente_cfa-vente_cfa_rente
	
	label var vente_cfa_rente			"Valeur des ventes en FCFA- Culture d'exportation"
	label var vente_cfa_vivre			"Valeur des ventes en FCFA- Culture vivrière"
	
	gen sharesales_vivre= vente_cfa_vivre/prod_cfa_vivre
	gen sharesales_rente= vente_cfa_rente/prod_cfa_rente
	replace sharesales_vivre=1 if sharesales_vivre>1 &!missing(sharesales_vivre)
	replace sharesales_rente=1 if sharesales_rente>1 &!missing(sharesales_rente)
	
	label var sharesales_vivre			"Part des cultures vivrières vendue"
	label var sharesales_rente			"Part des cultures d'exportation vendue"

	numlabel, add mask("[#]  ")
	label data "ENESTE 2016 : BASE AGRICULTURE TOUTES SECTIONS-UNIQUE AT HH LEVEL"
save "${cleanext}/${bprefix}_Agriculture_all", replace	
 
 
