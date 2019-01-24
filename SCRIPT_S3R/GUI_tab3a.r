
###########################
# INTERFACE ETAPE 3 ######
##########################


## Creation de la fenetre
DEBUTGUI(Titre = "S3R (3a/5)")



###frame formulaire
frameform <- tkframe(tt)
tkconfigure(frameform, bg=colorbg)

tkgrid(frameform)
tkgrid(tklabel2(frameform,text="    ", font = fontvide))



###label OPTIONS D'EXPORT

tkgrid(
tklabel2(frameform,text=paste(rep("S",41),collapse=""), foreground=colorbg ),  #colorbg
tklabel2(frameform,text=paste(rep("S",26),collapse=""), foreground=colorbg )  #colorbg
,  sticky="w")

tkgrid(tklabel2(frameform,text="    ", font = fontvide))
tkgrid(tklabel2(frameform,text="    ", font = fontvide))
tkgrid(tklabel3(frameform,text="  OPTIONS D'EXPORT : ",font=fontlabrad ), sticky="w")
tkgrid(tklabel2(frameform,text="    ", font = fontvide))

### check box RANG90
cb2 <- tkcheckbutton2(frameform)
if (!exists("cbValue2")) {cbValue2 <- tclVar(1)}
tkconfigure(cb2,variable=cbValue2)
tkgrid(tklabel2(frameform,text="      Extraire R90, valeur moyenne, indice (type SEQ-EAU)", font = fontnormal) , cb2, sticky="w")
tkgrid(tklabel2(frameform,text="    ", font = fontvide))

### check box FREQPRELEV
cb7 <- tkcheckbutton2(frameform)
if (!exists("cbValue7")) {cbValue7 <- tclVar(1)}
tkconfigure(cb7,variable=cbValue7)
tkgrid( tklabel2(frameform,text="      Extraire les fréquences (prélèvements, déclassements)             ", font = fontnormal) , cb7, sticky="w")
tkgrid(tklabel2(frameform,text="    ", font = fontvide))
tcl("update")

### check box STATISTIQUES
cb9 <- tkcheckbutton2(frameform)
if (!exists("cbValue9")) {cbValue9 <- tclVar(1)}
tkconfigure(cb9,variable=cbValue9)
tkgrid( tklabel2(frameform,text="      Calculer les statistiques globales", font = fontnormal) , cb9, sticky="w")
tkgrid(tklabel2(frameform,text="    ", font = fontvide))
tcl("update")

### check box OPTION NO3
if ( BASSININI == "AESN" & SEEEPCH=="oui") {
cb25 <- tkcheckbutton2(frameform)
if (!exists("cbValue25")) {cbValue25 <- tclVar(1)}
tkconfigure(cb25,variable=cbValue25)
tkgrid( tklabel2(frameform,text="      Consulter Nitrates selon grille AESN", font = fontnormal) , cb25, sticky="w")
tkgrid(tklabel2(frameform,text="    ", font = fontvide))
tcl("update")
}


### check box EXPORT DES DONNEES BRUTES

cb26 <- tkcheckbutton2(frameform)
if (!exists("cbValue26")) {cbValue26 <- tclVar(1)}
tkconfigure(cb26,variable=cbValue26)
tkgrid( tklabel2(frameform,text="      Exporter les données brutes après la mise en conformité", font = fontnormal) , cb26, sticky="w")
tkgrid(tklabel2(frameform,text="    ", font = fontvide))
tcl("update")



### check box MISECOULEUR
cb8 <- tkcheckbutton2(frameform)
if (!exists("cbValue8") & CONTA =="non") {cbValue8 <- tclVar(1)}
if (!exists("cbValue8") & CONTA =="oui") {cbValue8 <- tclVar(0)}
tkconfigure(cb8,variable=cbValue8)
tkgrid( tklabel2(frameform,text="      Mettre les résultats en couleur", font = fontnormal) , cb8, sticky="w")
tkgrid(tklabel2(frameform,text="    ", font = fontvide))
tkgrid(tklabel2(frameform,text="    ", font = fontvide))
tkgrid(tklabel2(frameform,text="    ", font = fontvide))
tkgrid(tklabel2(frameform,text="    ", font = fontvide))
tcl("update")


### Boutons radio Export des graphiques
if ( BASSININI == "AESN" ) {
frameform2 <- tkframe(tt)
tkconfigure(frameform2, bg=colorbg)
tkgrid(frameform2)

tkgrid(tklabel2(frameform,text="  EXPORT GRAPHIQUE PAR :",font=fontbolditalic ), sticky="w")

rb1 <- tkradiobutton(frameform2)
rb2 <- tkradiobutton(frameform2)
rb3 <- tkradiobutton(frameform2)
if(!exists("rb_expgraph")) { rb_expgraph<- tclVar("AUCUN") }
tkconfigure(rb1,variable=rb_expgraph,value="ETAT", bg=colorbg)
tkconfigure(rb2,variable=rb_expgraph,value="TOUS", bg=colorbg)	
tkconfigure(rb3,variable=rb_expgraph,value="AUCUN", bg=colorbg)	

if ( SEEEPCH=="non" & SEEEBIO == "non" & SEEEPS == "non") {  #Si uniquement Etat Chim, pas d'Export de graphiques
tkconfigure(rb1,state="disabled")
tkconfigure(rb2,state="disabled")
}

tkgrid(rb1, tklabel2(frameform2,text="  Etats et Eléments de qualité uniquement               ", font = fontnormal), sticky="w")
tkgrid(rb2, tklabel2(frameform2,text="  Etats , Eléments de qualité et Paramètres               ", font = fontnormal), sticky="w")
tkgrid(rb3, tklabel2(frameform2,text="  Aucun Export               ", font = fontnormal), sticky="w")

tkgrid(tklabel2(frameform2,text="    ", font = fontvide))
tkgrid(tklabel2(frameform2,text="    ", font = fontvide))
tkgrid(tklabel2(frameform2,text="    ", font = fontvide))
tkgrid(tklabel2(frameform2,text="    ", font = fontvide))
tkgrid(tklabel2(frameform2,text="    ", font = fontvide))
tcl("update")
}




###FRAME BOUTONS
## affichage des bouton
framebut <- tkframe(tt)
tkconfigure(framebut, bg=colorbg)

## Fonction pour bouton SUIVANT
OnOK <- function()  ## enregistre les options dans des objets et va lire les CSV
{	

	 #options sortie
	 if (as.character((tclvalue(cbValue2))) == "1") { INDICE <<- "oui" } else { INDICE <<- "non"  }
	 if (as.character((tclvalue(cbValue7))) == "1") { FREQPRELEV <<- "oui" } else { FREQPRELEV <<- "non"  }
     if (as.character((tclvalue(cbValue8))) == "1") { MISECOULEUR <<- "oui" } else { MISECOULEUR <<- "non"  }
	 if (as.character((tclvalue(cbValue9))) == "1") { STATISTIQUE <<- "oui" } else { STATISTIQUE <<- "non"  }
	 
	 # OPTIONNO3 ne sera appliqué que sur AESN
	 OPTIONNO3 <<- "non" 
	 if ( BASSININI == "AESN"  & SEEEPCH=="oui" ) {
		if (as.character((tclvalue(cbValue25))) == "1") { OPTIONNO3 <<- "oui" } else { OPTIONNO3 <<- "non"  }
	 }

	 	 # OPTIONNO3 ne sera appliqué que sur AESN
	 OPTIONEXPORTDATABRUT <<- "non" 
		if (as.character((tclvalue(cbValue26))) == "1") { OPTIONEXPORTDATABRUT <<- "oui" } else { OPTIONEXPORTDATABRUT <<- "non"  }
	 
	 # On force à sortir les indice si on fait une simulation de quart de classe
	 if( !exists("SIMULQUARTCLASSE")) {  SIMULQUARTCLASSE<-"non" }
	 if (SIMULQUARTCLASSE == "oui") { INDICE <<- "oui"  } ## Option obligatoire si calul de l'option simulation 1/4 de classe

	 # Option d'export de graphiques
	 EXPORTGRAPH <<- "AUCUN"
	 if ( BASSININI == "AESN" ){
	 EXPORTGRAPH <<- as.character(tclvalue(rb_expgraph))
	 }
	 
	Sys.sleep(0.05)
	tkdestroy(tt)
    source(paste(pathS,"GUI_tab3b.r",sep="") , echo = TRUE, print.eval = TRUE)
}

fontbouton <- tkfont.create(family="calibri",size=15,weight="bold")
OKbut <- tkbutton2(framebut,text="  >  ",command=OnOK, font  = fontbouton ) 

## Fonction pour bouton PRECEDENT
Retour<-function()  ## enregistress les options dans des objets et va lire les CSV
{
tkdestroy(tt)
if (CONTA == "oui") {
source(paste(pathS,"/CONTAMINATION/GUI_tab3.r",sep="") , echo = TRUE, print.eval = TRUE)
} else {
source(paste(pathS,"GUI_tab3.r",sep="") , echo = TRUE, print.eval = TRUE)

}
}
returnbut <- tkbutton2(framebut,text="  <  ",command=Retour, font  = fontbouton ) 

fct_Aide <-function() { browseURL(paste(AidePath,"Aide3b.htm",sep=""))}
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
#tkfocus(tt)





