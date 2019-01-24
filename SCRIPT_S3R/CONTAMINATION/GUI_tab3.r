
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

### check box Extrazpolation à la masse d'eau
if (TYPECONTA %in% c(1,2,3)){
	cb13 <- tkcheckbutton2(frameform)
	if (!exists("cbValue13")) {cbValue13 <- tclVar(0)}
	tkconfigure(cb13,variable=cbValue13)
	tkgrid( tklabel2(frameform,text="      Extrapoler  à la masse d'eau", font = fontnormal) , cb13, sticky="w")
	#tkgrid(tklabel2(frameform,text="    ", font = fontvide))



tkgrid(tklabel2(frameform,text="    ", font = fontvide))
}
tkgrid(tklabel2(frameform,text="    ", font = fontvide))

####TABLEAU DE PARAMETRE
###frame formulaire



# CheckBox respect de la fraction cb6a
cb6a <- tkcheckbutton2(frameform2)
# CheckBox respect de l'unité cb11a
cb11a <- tkcheckbutton2(frameform2)
# CheckBox respect du nombre minimal de prélèvements
cb12a <- tkcheckbutton2(frameform2)

## valeur par défaut
cb6a <- tkcheckbutton2(frameform2)
if (!exists("cbValue6a")) {cbValue6a <- tclVar(1)}
tkconfigure(cb6a,variable=cbValue6a)

cb11a <- tkcheckbutton2(frameform2)
if (!exists("cbValue11a")) {cbValue11a <- tclVar(1)}
tkconfigure(cb11a,variable=cbValue11a)

cb12a <- tkcheckbutton2(frameform2)
if (!exists("cbValue12a")) {cbValue12a <- tclVar(1)}
tkconfigure(cb12a,variable=cbValue12a)

# RadioBouton pour Modifier le nombre de prélèvement à respecter par an
rb_nbanfreq1 <- tkradiobutton(frameform2)
rb_nbanfreq2 <- tkradiobutton(frameform2)
if(!exists("rbValeurmet_anfreq")) { rbValeurmet_anfreq<- tclVar("1") }

fct_entryok<-function(){tkconfigure(entrynbanfreq , state="normal")}
fct_entrypasok<-function(){tkconfigure(entrynbanfreq , state="disabled",textvariable = tclVar("4"))}

tkconfigure(rb_nbanfreq1,variable=rbValeurmet_anfreq,value="1", bg=colorbg, command = fct_entrypasok)
tkconfigure(rb_nbanfreq2,variable=rbValeurmet_anfreq,value="0", bg=colorbg, command = fct_entryok)	

#textbox pour le nombre de prélèvement à respecter par an
if(!exists("anfreq_value") & CEPE == "CE") { anfreq_value<- tclVar("4") }
if(!exists("anfreq_value") & CEPE == "PE") { anfreq_value<- tclVar("3") }
entrynbanfreq <- tkentry(frameform2, width = 4, justify = "center", textvariable = anfreq_value, state="disabled")

# Affiche les check box du bloc Respecter
tkgrid(tklabel2(frameform2,text="  RESPECTER :  ",font=fontbolditalic ), sticky="w")
tkgrid(tklabel2(frameform2,text="      La fraction               ", font = fontnormal),cb6a, sticky="w")
tkgrid(tklabel2(frameform2,text="      L'unité des paramètres     ", font = fontnormal),cb11a, sticky="w")
tkgrid(tklabel2(frameform2,text="      Le nombre minimal de prélèvements              ", font = fontnormal),cb12a, sticky="w")


if(CEPE == "CE") tkgrid(tklabel2(frameform2,text="             par défaut 4 minimum          ", font = fontnormal),rb_nbanfreq1, sticky="w")
if(CEPE == "PE") tkgrid(tklabel2(frameform2,text="             par défaut 3 minimum          ", font = fontnormal),rb_nbanfreq1, sticky="w")

tkgrid(tklabel2(frameform2,text="             autre :               ", font = fontnormal),rb_nbanfreq2, entrynbanfreq, sticky="w")
if (TYPECONTA %in%  c(1) ) { # Si Aigue 
	 cbValue12a <- tclVar(0)
	tkconfigure(cb12a,variable=cbValue12a)
	 tkconfigure(cb12a,state="disabled")
	 tkconfigure(rb_nbanfreq2,state="disabled")
	 tkconfigure(rb_nbanfreq1,state="disabled")
	 tkconfigure(rb_nbanfreq1,variable=rbValeurmet_anfreq,value="0", bg=colorbg, command = fct_entrypasok)
}
tkgrid(tklabel2(frameform2,text="    ", font = fontvide))
tkgrid(tklabel2(frameform2,text="    ", font = fontvide))

# Utilisation des LQ progressive
if (TYPECONTA %in%  c(4) ) {

cbLQprog <- tkcheckbutton2(frameform2)
if (!exists("cbValueLQprog")) {cbValueLQprog <- tclVar(1)}
tkconfigure(cbLQprog,variable=cbValueLQprog)
tkgrid(tklabel2(frameform2,text="Utiliser la méthode des LQ progressives pour les mesures non-quantifiées ", font = fontnormal),cbLQprog, sticky="w")

tkgrid(tklabel2(frameform2,text="    ", font = fontvide))
tkgrid(tklabel2(frameform2,text="    ", font = fontvide))

}


# Affiche Méthode de calcul par type de CONTA
if (TYPECONTA %in% 1:3) {
TYPECONTALAB<-c("Contamination Aigüe","Contamination Chronique","Imprégnation")


tkgrid(tklabel2(frameform2,text=paste0(" ",toupper(TYPECONTALAB[TYPECONTA]),"  "),font=fontlabrad ), sticky="w")
tkgrid(tklabel2(frameform2,text="  METHODE DE CALCUL :  ",font=fontbolditalic ), sticky="w")


METHCONTA<-c("  A partir de la concentration maximale observée sur la période",
			"  A partir de la concentration moyenne observée sur la période")

			
if (TYPECONTA %in% 1:2) {
	NORMELQ<-"SeuilRef"
	tkgrid(tklabel2(frameform2,text=paste0(" ",METHCONTA[TYPECONTA],"  "),font=fontnormal ), sticky="w")
	tkgrid(tklabel2(frameform2,text="    ", font = fontvide))
}			


if (TYPECONTA == 3) {

NORMELQ<-"LQMax"

# Boutons radio
rb1 <- tkradiobutton(frameform2)
rb2 <- tkradiobutton(frameform2)
if(!exists("rbValeurmet")) { rbValeurmet<- tclVar("MAX") }

tkconfigure(rb1,variable=rbValeurmet,value="MAX", bg=colorbg)
tkconfigure(rb2, variable=rbValeurmet,value="MOY", bg=colorbg)

tkgrid( tklabel2(frameform2,text=METHCONTA[1], font = fontnormal) , rb1, sticky="w")
tkgrid( tklabel2(frameform2,text=METHCONTA[2], font = fontnormal) , rb2, sticky="w")
tkgrid(tklabel2(frameform2,text="    ", font = fontvide))


}



			
tkgrid(tklabel2(frameform2,text="    ", font = fontvide))
tkgrid(tklabel2(frameform2,text="    ", font = fontvide))

tkgrid(tklabel2(frameform2,text="  SEUILS DE CLASSES DE QUALITE:  ",font=fontbolditalic ), sticky="w")


### Seuils de classes de qualité

	frameform3 <- tkframe(tt)
	tkconfigure(frameform3, bg=colorbg)
	tkgrid(frameform3)

	rb3 <- tkradiobutton(frameform3)
	rb4 <- tkradiobutton(frameform3)
	if(!exists("rb34Valeur")) { rb34Valeur<- tclVar("LIBRE") }
	tkconfigure(rb3,variable=rb34Valeur,value="LIBRE", bg=colorbg)
	tkconfigure(rb4, variable=rb34Valeur,value="LQMAX", bg=colorbg)

	if (TYPECONTA == 3) {
	lab_Libre<-tklabel2(frameform3,text="Bornes libres (cf GRILLECONTAMINATION.csv)", font = fontnormal)
	tkgrid( lab_Libre,rb3,   sticky="w")

	lab_LQMAX<-tklabel2(frameform3,text="Bornes en fonction de la LQMax", font = fontnormal)
	tkgrid( lab_LQMAX,rb4,    sticky="w")
	tkconfigure(rb4,state="disabled")
	}

	frameform3b <- tkframe(tt)
	tkconfigure(frameform3b, bg=colorbg)
	tkgrid(frameform3b)

	if(!exists("seuil1_val")) { seuil1_val<- tclVar("0.3") }
	if(!exists("seuil2_val")) { seuil2_val<- tclVar("1") }
	if(!exists("seuil3_val")) { seuil3_val<- tclVar("5") }

	lab_seuil1<-tklabel2(frameform3b,text=paste0("Borne 1  : ",NORMELQ," x"), font = fontnormal)
	tb_seuil1 <- tkentry(frameform3b, width = 5, justify = "center", textvariable = seuil1_val)

	lab_seuil2<-tklabel2(frameform3b,text=paste0("    Borne 2 : ",NORMELQ," x"), font = fontnormal)
	tb_seuil2 <- tkentry(frameform3b, width = 5, justify = "center", textvariable = seuil2_val)

	lab_seuil3<-tklabel2(frameform3b,text=paste0("    Borne 3 : ",NORMELQ," x"), font = fontnormal)
	tb_seuil3 <- tkentry(frameform3b, width = 5, justify = "center", textvariable = seuil3_val)

	tkgrid( lab_seuil1,tb_seuil1,lab_seuil2,tb_seuil2,lab_seuil3,tb_seuil3    , sticky="w")
	if (TYPECONTA == 3) {
	tkconfigure(tb_seuil1,state="disabled")
	tkconfigure(tb_seuil2,state="disabled")
	tkconfigure(tb_seuil3,state="disabled")
	}

	frameform4 <- tkframe(tt)
	tkconfigure(frameform4, bg=colorbg)
	tkgrid(frameform4)
	tkgrid( tklabel2(frameform4,text="Borne1 = Bon/Moyen ; Borne2 = Moyen/Médiocre ; Borne3 = Médiocre/Mauvais", font = fontsmallitalic) ,  sticky="w")


	# Case à cocher pour autoriser le retrait des mesures non quantifiée et dont la LQ < seuil de reférence 
	frameform5 <- tkframe(tt)
	tkconfigure(frameform5, bg=colorbg)
	tkgrid(frameform5)

	cb20 <- tkcheckbutton2(frameform5)
	if (!exists("cbValue20")) {cbValue20 <- tclVar(0)}
	tkconfigure(cb20,variable=cbValue20)
	tkgrid( tklabel2(frameform5,text="      Retrait des mesures non quantifiées et dont la LQ > seuil de reférence", font = fontnormal) , cb20, sticky="w")
		

	tkgrid(tklabel2(frameform5,text="    ", font = fontvide))
	tkgrid(tklabel2(frameform5,text="    ", font = fontvide))
}

###FRAME BOUTONS
## affichage des boutons
framebut <- tkframe(tt)
tkconfigure(framebut, bg=colorbg)

## Fonction pour bouton SUIVANT
OnOK <- function()  ## enregistre les options dans des objets et va lire les CSV
{	
	
		NBANFREQ<<-4 # Par defaut , il faut minimum 4 prélevement par an (PCH, PS et CHIM)
		if (CEPE == "PE") {NBANFREQ<<-3}
		if (tclvalue(rbValeurmet_anfreq)=="0" ){
			NBANFREQ <<- as.numeric(as.character(tclvalue(anfreq_value)))
		}

		SEEEECOLOME<<-""
		SEEECHIMME<<-""

		tkconfigure(frameform,cursor="watch")
		#Recupération des valeurs 
		#Bassin
			if ( BASSININI != "AESN" ) {
			 BASSIN <<- bassinlist[as.numeric(tclvalue(tcl(bassincomboBox,"getvalue")))+1]
			}
		
		 
		# Respects
			if (as.character((tclvalue(cbValue6a))) == "1") { FRACTIONOKCONTA <<- "oui" } else { FRACTIONOKCONTA <<- "non"  }
			if (as.character((tclvalue(cbValue11a))) == "1") { UNITEOKCONTA<<- "oui" } else { UNITEOKCONTA <<- "non"  }
			if (as.character((tclvalue(cbValue12a))) == "1") { FREQOKCONTA<<- "oui" } else { FREQOKCONTA <<- "non"  }

		# Utilisation des LQ progressives
		if (TYPECONTA == 4) {
		if (as.character((tclvalue(cbValueLQprog))) == "1") { LQPROGRESSIVE <<- "oui" } else { LQPROGRESSIVE <<- "non"  }
		}

		
		if (TYPECONTA %in% 1:3) {	
			#Extrapolation à la ME	
				if (as.character((tclvalue(cbValue13))) == "1") { CONTAME <<- "oui" } else { CONTAME <<- "non"  }
			
			#Retraits des mesures non quantifiée
					if (as.character((tclvalue(cbValue20))) == "1") { CONTARETRAITNONQUANTI <<- "oui" } else { CONTARETRAITNONQUANTI <<- "non"  }

			#Methode de Calculs

			if (TYPECONTA == 1) {CALCCONTA<<-"MAX"}
			if (TYPECONTA == 2) {CALCCONTA<<-"MOY"}
			CHOIXSEUIL<<-"NORME" 
			if (TYPECONTA == 3) {
			if (as.character(tclvalue(rbValeurmet)) == "MAX" ) { CALCCONTA <<- "MAX" } else { CALCCONTA <<- "MOY" }
			if (as.character(tclvalue(rb34Valeur)) == "LQMAX" ) { CHOIXSEUIL <<- "LQMAX" } else { CHOIXSEUIL <<- "LIBRE" }
			 }
			 
			 
			 if (!is.na(as.character(tclvalue(seuil1_val))) &  as.character(tclvalue(seuil1_val)) != "") { 
				 
				 CONTASEUIL1 <<- as.numeric(gsub(",",".",as.character(tclvalue(seuil1_val))))
				 CONTASEUIL2 <<- as.numeric(gsub(",",".",as.character(tclvalue(seuil2_val))))
				 CONTASEUIL3 <<- as.numeric(gsub(",",".",as.character(tclvalue(seuil3_val))))
			 
			Sys.sleep(0.05)
			tkdestroy(tt)
			source(paste(pathS,"GUI_tab3a.r",sep="") , echo = TRUE, print.eval = TRUE)
		} else {
		tkmessageBox(message = "Seuils invalides")
		}
		
		} else { ## Si somme de pesticides
				tkdestroy(tt)
				source(paste(pathS,"GUI_tab3b.r",sep="") , echo = TRUE, print.eval = TRUE)

		}
		
	
	}


fontbouton <- tkfont.create(family="calibri",size=15,weight="bold")
OKbut <- tkbutton2(framebut,text="  >  ",command=OnOK, font  = fontbouton ) 

## Fonction pour bouton PRECEDENT
Retour<-function()  ## enregistress les options dans des objets et va lire les CSV
{
tkdestroy(tt)
source(paste(pathS,"/CONTAMINATION/GUI_tab2.r",sep="") , echo = TRUE, print.eval = TRUE)
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






