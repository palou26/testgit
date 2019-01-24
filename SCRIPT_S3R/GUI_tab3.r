
###########################
# INTERFACE ETAPE 3 ######
##########################


## Creation de la fenetre
DEBUTGUI(Titre = "S3R (3/5)")



###frame formulaire
frameform <- tkframe(tt)
tkconfigure(frameform, bg=colorbg)

frameform2 <- tkframe(tt)
tkconfigure(frameform2, bg=colorbg)


tkgrid(frameform)
tkgrid(frameform2)


###label OPTIONS DE CALCULS
tkgrid(
tklabel2(frameform,text=paste(rep("S",41),collapse=""), foreground=colorbg ),
tklabel2(frameform,text=paste(rep("S",11),collapse=""), foreground=colorbg )  #colorbg
,  sticky="w")

tkgrid(tklabel3(frameform,text="  OPTIONS DE CALCUL AVANCEES :     ",font=fontlabrad ), sticky="w")
tkgrid(tklabel2(frameform,text="    ", font = fontvide))

###combo box choix de bassin
bassinlist <- c("Aucun","AEAG","AEAP","AELB","AERM","AERMC","AESN","ODE971", "ODE972", "ODE973", "ODE974","ODE976")
if ( BASSININI != "AESN" ) {
	tclRequire("BWidget")
	if (!exists("BASSIN")) {bassin_default <- tclVar("Aucun")} else {  bassin_default <- tclVar(BASSIN) }
	bassincomboBox <- tkwidget(frameform,"ComboBox",editable=FALSE,values=bassinlist , textvariable=bassin_default, width = 11)
	tkgrid(tklabel2(frameform,text="      Appliquer les spécificités d'un bassin                          ", font = fontnormal ),bassincomboBox,  sticky="w")
	tkgrid(tklabel2(frameform,text="    ", font = fontvide))
	tkconfigure(bassincomboBox ,cursor="hand2")

	tkgrid(tklabel2(frameform,text="    ", font = fontvide))
} else {
	BASSIN<-"AESN"
}


### check box ASSOUPTBE
if (SEEEPCH=="oui" & SEEEBIO=="oui" & CEPE == "CE"){
	cb10 <- tkcheckbutton2(frameform)
	if (!exists("cbValue10")) {cbValue10 <- tclVar(0)}
	tkconfigure(cb10,variable=cbValue10)
	tkgrid( tklabel2(frameform,text="      Assouplir le Très Bon Etat", font = fontnormal) , cb10, sticky="w")
	#tkgrid(tklabel2(frameform,text="    ", font = fontvide))
}

### check box ASSOUPBIO

	cb14 <- tkcheckbutton2(frameform)
	if (!exists("cbValue14")) {cbValue14 <- tclVar(0)}
	tkconfigure(cb14,variable=cbValue14)
if (SEEEBIO=="oui"  & CEPE == "CE"){
	tkgrid( tklabel2(frameform,text="      Simulation 1/4 de classe", font = fontnormal) , cb14, sticky="w")
	#tkgrid(tklabel2(frameform,text="    ", font = fontvide))
	if (MODULE == "REEE2010") {tkconfigure(cb14,state="disabled")}
}

### check box EXCLU_POLNS
if (SEEEPS=="oui") {
	cb3 <- tkcheckbutton2(frameform)
	if (!exists("cbValue3")) {cbValue3 <- tclVar(0)}
	tkconfigure(cb3,variable=cbValue3)
	tkgrid( tklabel2(frameform,text="      Exclure les polluants non synthétiques de l'état PS", font = fontnormal) , cb3, sticky="w")
	#tkgrid(tklabel2(frameform,text="    ", font = fontvide))
}

### check box Données modélisées

	cb1 <- tkcheckbutton2(frameform)
	if (!exists("cbValue1")) {cbValue1 <- tclVar(0)}
	tkconfigure(cb1,variable=cbValue1)
if (SEEEPCH=="oui"  & CEPE == "CE") {
	tkgrid( tklabel2(frameform,text="      Compléter la PCH avec des données modélisées           ", font = fontnormal) , cb1, sticky="w")
	#tkgrid(tklabel2(frameform,text="    ", font = fontvide))
}

### check box Restreindre la prise en compte de l'IPL/IPLAC aux plan d'eau stratifiés

	cb15 <- tkcheckbutton2(frameform)
	if (!exists("cbValue15")) {cbValue15 <- tclVar(0)}
	tkconfigure(cb15,variable=cbValue15)
if (SEEEBIO=="oui"  & CEPE == "PE" & MODULE == "REEE2010") {
	tkgrid( tklabel2(frameform,text="      Restreindre la prise en compte de l'IPL aux plan d'eau stratifiés           ", font = fontnormal) , cb15, sticky="w")
	#tkgrid(tklabel2(frameform,text="    ", font = fontvide))
}
if (SEEEBIO=="oui"  & CEPE == "PE" & MODULE != "REEE2010") {
	tkgrid( tklabel2(frameform,text="      Restreindre la prise en compte de l'IPLAC aux plan d'eau stratifiés           ", font = fontnormal) , cb15, sticky="w")
	#tkgrid(tklabel2(frameform,text="    ", font = fontvide))
}



### check box SEEEECOLOME
if (SEEEPCH=="oui" & SEEEBIO=="oui"){
	cb5 <- tkcheckbutton2(frameform)
	if (!exists("cbValue5")) {cbValue5 <- tclVar(0)}
	tkconfigure(cb5,variable=cbValue5)
	tkgrid( tklabel2(frameform,text="      Extrapoler l'état écologique à la masse d'eau", font = fontnormal) , cb5, sticky="w")
	#tkgrid(tklabel2(frameform,text="    ", font = fontvide))
}

### check box SEEEECOLOME
if (SEEECHIM=="oui" ){
	cb13 <- tkcheckbutton2(frameform)
	if (!exists("cbValue13")) {cbValue13 <- tclVar(0)}
	tkconfigure(cb13,variable=cbValue13)
	tkgrid( tklabel2(frameform,text="      Extrapoler l'état chimique à la masse d'eau", font = fontnormal) , cb13, sticky="w")
	#tkgrid(tklabel2(frameform,text="    ", font = fontvide))
}


### check box pour MEA et MEFM, on retient uniquement ChloA et PCH pour le calcul de l'état Ecolo

	cb16 <- tkcheckbutton2(frameform)
	if (!exists("cbValue16")) {cbValue16 <- tclVar(0)}
	tkconfigure(cb16,variable=cbValue16)
if (SEEEBIO=="oui"  & SEEEPCH=="oui" &  CEPE == "PE"  & MODULE == "REEE2010") {
	tkgrid( tklabel2(frameform,text="      Restreindre à Chlo-a et PCH pour l'état  écologique des MEFM et MEA         ", font = fontnormal) , cb16, sticky="w")
	#tkgrid(tklabel2(frameform,text="    ", font = fontvide))
}

# L'IIL ne s'applique pas aux MEA/MEFM
	cb17 <- tkcheckbutton2(frameform)
	if (!exists("cbValue17")) {cbValue17 <- tclVar(0)}
	tkconfigure(cb17,variable=cbValue17)
if (SEEEBIO=="oui"  & CEPE == "PE" & MODULE != "REEE2010") {
	tkgrid( tklabel2(frameform,text="      L'IIL ne s'applique pas aux MEA/MEFM           ", font = fontnormal) , cb17, sticky="w")
	#tkgrid(tklabel2(frameform,text="    ", font = fontvide))
}



tkgrid(tklabel2(frameform,text="    ", font = fontvide))
tkgrid(tklabel2(frameform,text="    ", font = fontvide))

####TABLEAU DE PARAMETRE
###frame formulaire



tkgrid(
tklabel2(frameform2,text="            ", font = fontvide),
tklabel2(frameform2,text="PCH    ", font = fontbold),
tklabel2(frameform2,text="POLSPE    ", font = fontbold),
tklabel2(frameform2,text="CHIMIE    ", font = fontbold),
sticky="w"
)

# CheckBox respect de la fraction cb6a, cb6b, cb6c
cb6a <- tkcheckbutton2(frameform2)
cb6b <- tkcheckbutton2(frameform2)
cb6c <- tkcheckbutton2(frameform2)
# CheckBox respect de l'unité cb11a, cb11b, cb11c
cb11a <- tkcheckbutton2(frameform2)
cb11b <- tkcheckbutton2(frameform2)
cb11c <- tkcheckbutton2(frameform2)
# CheckBox respect du nombre minimal de prélèvements
cb12a <- tkcheckbutton2(frameform2)
cb12b <- tkcheckbutton2(frameform2)
cb12c <- tkcheckbutton2(frameform2)

# CheckBox retrait de certains prélèvement si LQ > NQEMA
cb13b <- tkcheckbutton2(frameform2)
cb13c <- tkcheckbutton2(frameform2)
if (MODULE == "REEE2010") {
 tkconfigure(cb13b,state="disabled")
 tkconfigure(cb13c,state="disabled")
 }

 # CheckBox retrait de certains prélèvement si LQ > LQSEUIL (LQ ABERRANTE)
cb18b <- tkcheckbutton2(frameform2)
cb18c <- tkcheckbutton2(frameform2)
if (MODULE == "REEE2010") {
 tkconfigure(cb18b,state="disabled")
 tkconfigure(cb18c,state="disabled")
 }

 
# RadioBouton pour Modifier le nombre de prélèvement à respecter par an
rb_nbanfreq1 <- tkradiobutton(frameform2)
rb_nbanfreq2 <- tkradiobutton(frameform2)
if(!exists("rbValeur_anfreq")) { rbValeur_anfreq<- tclVar("1") }

fct_entryok<-function(){tkconfigure(entrynbanfreq , state="normal")}
fct_entrypasok<-function(){tkconfigure(entrynbanfreq , state="disabled",textvariable = tclVar("4"))}

tkconfigure(rb_nbanfreq1,variable=rbValeur_anfreq,value="1", bg=colorbg, command = fct_entrypasok)
tkconfigure(rb_nbanfreq2,variable=rbValeur_anfreq,value="0", bg=colorbg, command = fct_entryok)	

#textbox pour le nombre de prélèvement à respecter par an
if(!exists("anfreq_value") & CEPE == "CE") { anfreq_value<- tclVar("4") }
if(!exists("anfreq_value") & CEPE == "PE") { anfreq_value<- tclVar("3") }
entrynbanfreq <- tkentry(frameform2, width = 4, justify = "center", textvariable = anfreq_value, state="disabled")


# Bouton radio : methode de calcul de la moyenne (POLPSE)
rb1_b <- tkradiobutton(frameform2)
rb2_b <- tkradiobutton(frameform2)
rb3_b <- tkradiobutton(frameform2)
if(!exists("rbValeur_b")) { rbValeur_b<- tclVar("ANNEERECENTE") }
tkconfigure(rb1_b,variable=rbValeur_b,value="ANNEERECENTE", bg=colorbg)
tkconfigure(rb2_b,variable=rbValeur_b,value="ANNEECHRONIQUE", bg=colorbg)	
tkconfigure(rb3_b,variable=rbValeur_b,value="NOFILTREANNEE", bg=colorbg)	

# Bouton radio : methode de calcul de la moyenne (CHIM)
rb1_c <- tkradiobutton(frameform2)
rb2_c <- tkradiobutton(frameform2)
rb3_c <- tkradiobutton(frameform2)
if(!exists("rbValeur_c")) { rbValeur_c<- tclVar("ANNEERECENTE") }
tkconfigure(rb1_c,variable=rbValeur_c,value="ANNEERECENTE", bg=colorbg)
tkconfigure(rb2_c,variable=rbValeur_c,value="ANNEECHRONIQUE", bg=colorbg)	
tkconfigure(rb3_c,variable=rbValeur_c,value="NOFILTREANNEE", bg=colorbg)	
	

### check box FRACTIONOK et UNITEOK (PCH, PS ou CHIM)
if (SEEEPCH=="oui" ){
	cb6a <- tkcheckbutton2(frameform2)
	if (!exists("cbValue6a")) {cbValue6a <- tclVar(1)}
	tkconfigure(cb6a,variable=cbValue6a)
	
	cb11a <- tkcheckbutton2(frameform2)
	if (!exists("cbValue11a")) {cbValue11a <- tclVar(1)}
	tkconfigure(cb11a,variable=cbValue11a)

	cb12a <- tkcheckbutton2(frameform2)
	if (!exists("cbValue12a")) {cbValue12a <- tclVar(1)}
	tkconfigure(cb12a,variable=cbValue12a)


	
} else {
	tkconfigure(cb6a,state="disabled")
	tkconfigure(cb11a,state="disabled")
	tkconfigure(cb12a,state="disabled")
}


if (SEEEPS=="oui"){
	cb6b <- tkcheckbutton2(frameform2)
	if (!exists("cbValue6b")) {cbValue6b <- tclVar(1)}
	tkconfigure(cb6b,variable=cbValue6b)
	
	cb11b <- tkcheckbutton2(frameform2)
	if (!exists("cbValue11b")) {cbValue11b <- tclVar(1)}
	tkconfigure(cb11b,variable=cbValue11b)
	
	cb12b <- tkcheckbutton2(frameform2)
	if (!exists("cbValue12b")) {cbValue12b <- tclVar(1)}
	tkconfigure(cb12b,variable=cbValue12b)

	cb13b <- tkcheckbutton2(frameform2)
	if (!exists("cbValue13b")) {cbValue13b <- tclVar(0)}
	tkconfigure(cb13b,variable=cbValue13b)
	
	cb18b <- tkcheckbutton2(frameform2)
	if (!exists("cbValue18b")) {cbValue18b <- tclVar(0)}
	tkconfigure(cb18b,variable=cbValue18b)

} else {
	tkconfigure(cb6b,state="disabled")
	tkconfigure(cb11b,state="disabled")
	tkconfigure(cb12b,state="disabled")
	tkconfigure(cb13b,state="disabled")
	tkconfigure(cb18b,state="disabled")
	tkconfigure(rb1_b,state="disabled", bg=colorbg)
	tkconfigure(rb2_b,state="disabled", bg=colorbg)
	tkconfigure(rb3_b,state="disabled", bg=colorbg)
}


if (SEEECHIM=="oui"){
	cb6c <- tkcheckbutton2(frameform2)
	if (!exists("cbValue6c")) {cbValue6c <- tclVar(1)}
	tkconfigure(cb6c,variable=cbValue6c)
	
	cb11c <- tkcheckbutton2(frameform2)
	if (!exists("cbValue11c")) {cbValue11c <- tclVar(1)}
	tkconfigure(cb11c,variable=cbValue11c)
	
	cb12c <- tkcheckbutton2(frameform2)
	if (!exists("cbValue12c")) {cbValue12c <- tclVar(1)}
	tkconfigure(cb12c,variable=cbValue12c)

	cb13c <- tkcheckbutton2(frameform2)
	if (!exists("cbValue13c")) {cbValue13c <- tclVar(0)}
	tkconfigure(cb13c,variable=cbValue13c)

	cb18c <- tkcheckbutton2(frameform2)
	if (!exists("cbValue18c")) {cbValue18c <- tclVar(0)}
	tkconfigure(cb18c,variable=cbValue18c)	

} else {
	tkconfigure(cb6c,state="disabled")
	tkconfigure(cb11c,state="disabled")
	tkconfigure(cb12c,state="disabled")
	tkconfigure(cb13c,state="disabled")
	tkconfigure(cb18c,state="disabled")
	tkconfigure(rb1_c,state="disabled", bg=colorbg)
	tkconfigure(rb2_c,state="disabled", bg=colorbg)
	tkconfigure(rb3_c,state="disabled", bg=colorbg)

}

if (MODULE == "REEE2010") {
 tkconfigure(cb13b,state="disabled")
 tkconfigure(cb13c,state="disabled")
  tkconfigure(cb18b,state="disabled")
 tkconfigure(cb18c,state="disabled")
 }

tkgrid(tklabel2(frameform2,text="  RESPECTER :  ",font=fontbolditalic ), sticky="w")
tkgrid(tklabel2(frameform2,text="      La fraction               ", font = fontnormal),cb6a,cb6b,cb6c, sticky="w")
tkgrid(tklabel2(frameform2,text="      L'unité des paramètres     ", font = fontnormal),cb11a,cb11b,cb11c, sticky="w")
tkgrid(tklabel2(frameform2,text="      Le nombre minimal de prélèvements              ", font = fontnormal),cb12a,cb12b,cb12c, sticky="w")


if(CEPE == "CE") tkgrid(tklabel2(frameform2,text="             par défaut 4 minimum          ", font = fontnormal),rb_nbanfreq1, sticky="w")
if(CEPE == "PE") tkgrid(tklabel2(frameform2,text="             par défaut 3 minimum          ", font = fontnormal),rb_nbanfreq1, sticky="w")

tkgrid(tklabel2(frameform2,text="             autre :               ", font = fontnormal),rb_nbanfreq2, entrynbanfreq, sticky="w")


tkgrid(tklabel2(frameform2,text="  METHODE  - CALCUL DE LA MOYENNE AVEC:  ",font=fontbolditalic ), sticky="w")
tkgrid(tklabel2(frameform2,text="      L'année la plus récente     ", font = fontnormal),tklabel2(frameform2,text="", font = fontvide),rb1_b,rb1_c, sticky="w")
tkgrid(tklabel2(frameform2,text="      L'année ayant la meilleure fréquence de prélèvement     ", font = fontnormal),tklabel2(frameform2,text="", font = fontvide),rb2_b,rb2_c, sticky="w")
tkgrid(tklabel2(frameform2,text="      L'ensemble du jeu de données     ", font = fontnormal),tklabel2(frameform2,text="", font = fontvide),rb3_b,rb3_c, sticky="w")


tkgrid(tklabel2(frameform2,text="    ", font = fontvide))
tkgrid(tklabel2(frameform2,text="    ", font = fontvide))

tkgrid(tklabel2(frameform2,text="RETRAIT DES PRELEVEMENTS NON-QUANTIFIÉS   ",font=fontbolditalic ), sticky="w")
tkgrid(tklabel2(frameform2,text="      si LQ > NQEMA      ", font = fontnormal),tklabel2(frameform2,text="", font = fontvide),cb13b,cb13c, sticky="w")
tkgrid(tklabel2(frameform2,text="      si LQ > LQSEUIL (LQ dite 'aberrantes')   ", font = fontnormal),tklabel2(frameform2,text="", font = fontvide),cb18b,cb18c, sticky="w")

tkgrid(tklabel2(frameform2,text="    ", font = fontvide))
tkgrid(tklabel2(frameform2,text="    ", font = fontvide))



###FRAME BOUTONS
## affichage des boutons
framebut <- tkframe(frameform2)
tkconfigure(framebut, bg=colorbg)

## Fonction pour bouton SUIVANT
OnOK <- function()  ## enregistre les options dans des objets et va lire les CSV
{	
	
	NBANFREQ<<-4 # Par defaut , il faut minimum 4 prélevement par an (PCH, PS et CHIM)
	if (CEPE == "PE") {NBANFREQ<<-3}
	if (tclvalue(rbValeur_anfreq)=="0" ){
		NBANFREQ <<- as.numeric(as.character(tclvalue(anfreq_value)))
	}

	SEEEECOLOME<<-""
	SEEECHIMME<<-""

	tkconfigure(frameform,cursor="watch")
	#options calcul 
	if ( BASSININI != "AESN" ) {
	 BASSIN <<- bassinlist[as.numeric(tclvalue(tcl(bassincomboBox,"getvalue")))+1]
	}
	
	if (SEEEPCH == "oui" ) {
		if (as.character((tclvalue(cbValue1))) == "1") { SEEEPEGASE <<- "oui" } else { SEEEPEGASE <<- "non"  }
	} else { SEEEPEGASE <<- "non"  }
			 
	if (SEEEPS=="oui" ) {
		if (as.character((tclvalue(cbValue3))) == "1") { EXCLU_POLNS <<- "oui" } else { EXCLU_POLNS <<- "non"  }
	 } else { EXCLU_POLNS <<- "non"  }
	 
	if (SEEEECOLO == "oui" ){
		if (as.character((tclvalue(cbValue5))) == "1") { SEEEECOLOME <<- "oui" } else { SEEEECOLOME <<- "non"  }
	 } else {
		SEEEECOLOME <<- "non"
	 }

	 
	if (SEEEBIO == "oui"  & CEPE == "PE" & MODULE == "REEE2010"){
		if (as.character((tclvalue(cbValue15))) == "1") { REST_IPL_STATI <<- "oui" } else { REST_IPL_STATI <<- "non"  }
	 } else {
		REST_IPL_STATI <<- "non"
	 }	 

	if (SEEEBIO == "oui"  & CEPE == "PE" & MODULE != "REEE2010"){
		if (as.character((tclvalue(cbValue15))) == "1") { REST_IPLAC_STATI <<- "oui" } else { REST_IPLAC_STATI <<- "non"  }
	 } else {
		REST_IPLAC_STATI <<- "non"
	 }	 
	 
	if (SEEEBIO == "oui"  & CEPE == "PE" & MODULE != "REEE2010"){
		if (as.character((tclvalue(cbValue17))) == "1") { EXCLURE_IIL <<- "oui" } else { EXCLURE_IIL <<- "non"  }
	 } else {
		EXCLURE_IIL <<- "non"
	 }	

	if (SEEEPCH == "oui" &  SEEEBIO == "oui"   & CEPE == "PE"){
		if (as.character((tclvalue(cbValue16))) == "1") { EXCEPT_MEAFM_ECO <<- "oui" } else { EXCEPT_MEAFM_ECO <<- "non"  }
	 } else {
		EXCEPT_MEAFM_ECO <<- "non"
	 }	 
	 
	if (SEEEECOLO == "oui" & CEPE == "CE"){
		if (as.character((tclvalue(cbValue10))) == "1") { ASSOUPTBE <<- "oui" } else { ASSOUPTBE <<- "non"  }
	 } else {
		ASSOUPTBE <<- "non"
	 }	 
	 
	if (SEEEBIO == "oui"){
		if (as.character((tclvalue(cbValue14))) == "1") { 
			SIMULQUARTCLASSE <<- "oui"
			INDICE <<- "oui"
			} else { SIMULQUARTCLASSE <<- "non"  }
	 } else {
		SIMULQUARTCLASSE <<- "non"
	 }
	 
	if (SEEEPCH=="oui" ){
		if (as.character((tclvalue(cbValue6a))) == "1") { FRACTIONOKPCH <<- "oui" } else { FRACTIONOKPCH <<- "non"  }
		if (as.character((tclvalue(cbValue11a))) == "1") { UNITEOKPCH <<- "oui" } else { UNITEOKPCH <<- "non"  }
		if (as.character((tclvalue(cbValue12a))) == "1") { FREQOKPCH <<- "oui" } else { FREQOKPCH <<- "non"  }
	} else {
		FRACTIONOKPCH <<- " - "
		UNITEOKPCH <<- " - "
		FREQOKPCH <<- " - "
	}
	 
	if (SEEEPS=="oui" ){
		if (as.character((tclvalue(cbValue6b))) == "1") { FRACTIONOKPS <<- "oui" } else { FRACTIONOKPS <<- "non"  }
		if (as.character((tclvalue(cbValue11b))) == "1") { UNITEOKPS <<- "oui" } else { UNITEOKPS <<- "non"  }
		if (as.character((tclvalue(cbValue12b))) == "1") { FREQOKPS <<- "oui" } else { FREQOKPS <<- "non"  }
		if (as.character((tclvalue(cbValue13b))) == "1") { LQSUPNQEMAPS <<- "oui" } else { LQSUPNQEMAPS <<- "non"  }
		if (as.character((tclvalue(cbValue18b))) == "1") { SUPPR_LQ_ABERRANTE_PS <<- "oui" } else { SUPPR_LQ_ABERRANTE_PS <<- "non"  }
	} else {
		FRACTIONOKPS <<- " - "
		UNITEOKPS <<- " - "
		FREQOKPS <<-  " - "
		LQSUPNQEMAPS <<-  " - "
		SUPPR_LQ_ABERRANTE_PS <<- " - " 
	}
	 
	if (SEEECHIM=="oui" ){
		if (as.character((tclvalue(cbValue6c))) == "1") { FRACTIONOKCHIM <<- "oui" } else { FRACTIONOKCHIM <<- "non"  }
		if (as.character((tclvalue(cbValue11c))) == "1") { UNITEOKCHIM <<- "oui" } else { UNITEOKCHIM <<- "non"  }
		if (as.character((tclvalue(cbValue12c))) == "1") { FREQOKCHIM <<- "oui" } else { FREQOKCHIM <<- "non"  }
		if (as.character((tclvalue(cbValue13))) == "1") { SEEECHIMME <<- "oui" } else { SEEECHIMME <<- "non" }
		if (as.character((tclvalue(cbValue13c))) == "1") { LQSUPNQEMACHIM <<- "oui" } else { LQSUPNQEMACHIM <<- "non" }
		if (as.character((tclvalue(cbValue18c))) == "1") { SUPPR_LQ_ABERRANTE_CHIM <<- "oui" } else { SUPPR_LQ_ABERRANTE_CHIM <<- "non"  }

	} else {
		FRACTIONOKCHIM <<- " - "
		UNITEOKCHIM <<- " - "
		FREQOKCHIM <<- " - " 
		LQSUPNQEMACHIM <<- " - "
		SUPPR_LQ_ABERRANTE_CHIM <<- " - " 

	}
	 

     
	 ## options des boutons radio
	 if (SEEEPS=="oui" ){
	 METHODEMOYPS <<- as.character(tclvalue(rbValeur_b))}
	 if (SEEECHIM=="oui"){
	 METHODEMOYCHIM <<- as.character(tclvalue(rbValeur_c))}
	 
	 
	 OK<<-"oui"
	 ##message d'erreur si agrégation masse d'eau et restriction MEA/MEFM
	if(CEPE == "PE" & EXCEPT_MEAFM_ECO == "oui" & SEEEECOLOME == "non"){
		tkmessageBox(title = "Info", message = paste("Pour prendre en compte uniquement la Chlrorophyle-a et l'état PCH aux MEFM et MEA dans le calcul de l'état écologique,  \n il faut cocher l'option d'extrapolation à la masse d'eau",sep=""), icon = "info", type = "ok")
		OK<<-"non"
		tcl("update")
	 }

	 
	if(CEPE == "PE" & EXCLURE_IIL == "oui" & SEEEECOLOME == "oui" & MODULE != "REEE2010"){
		tkmessageBox(title = "Info", message = paste("L'exclusion de l'IIL aux MEFM et MEA dans le calcul de l'état écologique,  \n sera effective dans l'onglet 'MasseEau'",sep=""), icon = "info", type = "ok")
		tcl("update")
	 }	 
	 
	if(CEPE == "PE" & EXCEPT_MEAFM_ECO == "oui" & SEEEECOLOME == "oui" & MODULE == "REEE2010"){
		tkmessageBox(title = "Info", message = paste("La restriction à la Chlrorophyle-a et l'état PCH pour les MEFM et MEA dans le calcul de l'état écologique \n sera effective dans l'onglet 'MasseEau'  ",sep=""), icon = "info", type = "ok")
		tcl("update")
	 }
	 
	Sys.sleep(0.05)
	
	
	
    if (OK == "oui") {
		tkdestroy(tt)
		source(paste(pathS,"GUI_tab3a.r",sep="") , echo = TRUE, print.eval = TRUE)
	}
}

fontbouton <- tkfont.create(family="calibri",size=15,weight="bold")
OKbut <- tkbutton2(framebut,text="  >  ",command=OnOK, font  = fontbouton ) 

## Fonction pour bouton PRECEDENT
Retour<-function()  ## enregistress les options dans des objets et va lire les CSV
{
tkdestroy(tt)
source(paste(pathS,"GUI_tab2.r",sep="") , echo = TRUE, print.eval = TRUE)
}
returnbut <- tkbutton2(framebut,text="  <  ",command=Retour, font  = fontbouton ) 

fct_Aide <-function() { browseURL(paste(AidePath,"Aide3.htm",sep=""))}
AideBut <- tkbutton2(framebut,text="  ?  ",command=fct_Aide, font  = fontbouton) 

tkgrid(framebut)
tkgrid(returnbut,OKbut,tklabel2(framebut,text="", font = fontvide), AideBut)
tkconfigure(OKbut ,cursor="hand2")
tkconfigure(returnbut ,cursor="hand2")
tkconfigure(AideBut ,cursor="hand2")
tkgrid(tklabel2(framebut,text="    ", font = fontvide))
tkgrid(tklabel2(framebut,text="    ", font = fontvide))
tkgrid(tklabel2(framebut,text="    ", font = fontvide))
tcl("update")
tkwm.state(tt,"normal")
tcl("wm", "attributes", tt, topmost=FALSE)
tkwait.window(tt)






