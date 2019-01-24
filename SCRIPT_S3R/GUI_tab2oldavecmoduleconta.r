###########################
# INTERFACE ETAPE 2  ######
##########################

## Creation de la fenetre
DEBUTGUI(Titre = "S3R (2/5)")

###frame formulaire
frameform <- tkframe(tt)
tkconfigure(frameform, bg=colorbg)
tkgrid(frameform)
tkgrid(tklabel2(frameform,text="    ", font = fontvide))


###label Options
tkgrid(tklabel2(frameform,text="    ", font = fontvide))
tkgrid(tklabel2(frameform,text="    ", font = fontvide))

tkgrid(tklabel3(frameform,text="  CALCULS À RÉALISER :  ",font=fontlabrad ), sticky="w")
tkgrid(tklabel2(frameform,text="    ", font = fontvide))
tkgrid(tklabel2(frameform,text="    ", font = fontvide))


### check box SEEEPCH
cb1 <- tkcheckbutton2(frameform)
if (!exists("cbVal1")) {cbVal1 <- tclVar(0)}
tkconfigure(cb1,variable=cbVal1)
tkgrid( tklabel2(frameform,text="      Etat Physicochimique  ", font = fontnormal) , cb1, sticky="w")
tkgrid(tklabel2(frameform,text="    ", font = fontvide))


### check box SEEEPS
cb2 <- tkcheckbutton2(frameform)
if (!exists("cbVal2")) {cbVal2 <- tclVar(0)}
tkconfigure(cb2,variable=cbVal2)
tkgrid( tklabel2(frameform,text="      Etat Polluants Spécifiques  ", font = fontnormal) , cb2, sticky="w")
tkgrid(tklabel2(frameform,text="    ", font = fontvide))



### check box SEEEBIO
cb4 <- tkcheckbutton2(frameform)
if (!exists("cbVal4")) {cbVal4 <- tclVar(0)}
tkconfigure(cb4,variable=cbVal4)
tkgrid( tklabel2(frameform,text="      Etat Biologique  ", font = fontnormal) , cb4, sticky="w")
tkgrid(tklabel2(frameform,text="    ", font = fontvide))



### check box SEEECHIM OU CONTA
cb6 <- tkcheckbutton2(frameform)
	
	# fonction associée au checkbox pour mettre les radiobouton à un état normal
On_normal<-function() {  
	tkconfigure(rb1, state="normal")
	if(MODULE != "REEE2010") {tkconfigure(rb2, state="normal")}
	tkconfigure(cb6,variable=cbVal6, command=On_disabled)
	if (tclvalue(rbValeur) %in% c("",0,"0")) { rbValeur<- tclVar("EtatChim") }
	tkconfigure(rb1,variable=rbValeur,value="EtatChim", bg=colorbg)
	tkconfigure(rb2,variable=rbValeur,value="Contamination", bg=colorbg)	
	}
	# fonction associée au checkbox pour mettre les radiobouton à un état disabled
On_disabled<-function() {
	tkconfigure(cb6,variable=cbVal6, command=On_normal)
	tkconfigure(rb1,state="disabled",variable=rbValeur,value="EtatChim", bg=colorbg)
	tkconfigure(rb2,state="disabled",variable=rbValeur,value="Contamination", bg=colorbg)
	}
if (!exists("cbVal6")) {
	cbVal6 <- tclVar(0)
	checkstate<-"disabled"
}

if(as.character(tclvalue(cbVal6)) == "1") {checkstate<-"normal"}
	
if (checkstate == "disabled") {tkconfigure(cb6,variable=cbVal6, command=On_normal)} else {tkconfigure(cb6,variable=cbVal6, command=On_disabled)}
tkgrid( tklabel2(frameform,text="      CHIMIE               ", font = fontnormal) , cb6, sticky="w")
tkgrid(tklabel2(frameform,text="    ", font = fontvide))

# Bouton radio pour le choix de calculer SOIT l'état chimique, soit le calcul de contamination
rb1 <- tkradiobutton(frameform)
rb2 <- tkradiobutton(frameform)
if(!exists("rbValeur")) { rbValeur<- tclVar("EtatChim") }
tkconfigure(rb1,state=checkstate, variable=rbValeur,value="EtatChim", bg=colorbg)
tkconfigure(rb2,state=checkstate, variable=rbValeur,value="Contamination", bg=colorbg)
tkgrid( tklabel2(frameform,text="    ", font = fontvide),rb1, tklabel2(frameform,text="  Etat Chimique", font = fontnormal) , sticky="w")
tkgrid( tklabel2(frameform,text="    ", font = fontvide), rb2, tklabel2(frameform,text="  Calcul de contamination", font = fontnormal) , sticky="w")
tkgrid(tklabel2(frameform,text="    ", font = fontvide))
tkgrid(tklabel2(frameform,text="    ", font = fontvide))
tkgrid(tklabel2(frameform,text="    ", font = fontvide))


###FRAME BOUTONS
## affichage des bouton
framebut <- tkframe(tt)
tkconfigure(framebut, bg=colorbg)


OnOK <- function()   #fonction pour bouton "SUIVANT"
{ 
	## récupération des valeurs
     tcl("update")
	 if (as.character(tclvalue(rbCEPE)) == "CE") { CEPE <<- "CE" } else { CEPE <<- "PE" }
	 if (as.character((tclvalue(cbVal1))) == "1") { SEEEPCH <<- "oui" } else { SEEEPCH <<- "non"  }
	 if (as.character((tclvalue(cbVal2))) == "1") { SEEEPS <<- "oui" } else { SEEEPS <<- "non"  }
	 if (as.character((tclvalue(cbVal4))) == "1") { SEEEBIO <<- "oui" } else { SEEEBIO <<- "non"  }
	 if (SEEEPCH == "oui" & SEEEBIO == "oui") { SEEEECOLO <<- "oui" } else { SEEEECOLO <<- "non"  }
	 if (as.character(tclvalue(cbVal6)) == "1") { SEEECHIM <<- "oui" } else { SEEECHIM <<- "non"  }
	 if (as.character(tclvalue(rbValeur)) == "Contamination" & SEEECHIM == "oui") { CONTA <<- "oui" } else { CONTA <<- "non" }
	 CHIMIE<<-as.character(tclvalue(rbValeur))
	 
	 #msgbox pour ne pas permettre d'aller sur l'écran suivant si pas de calcul coché
	 if (SEEEPCH != "oui" & SEEEPS != "oui" & SEEEBIO != "oui" & SEEEECOLO != "oui" & SEEECHIM != "oui" ) {
	 tkmessageBox(message = "Aucun état sélectionné ! ")
	 } else if (SEEEECOLO == "oui" & (SEEEPCH == "non" | SEEEBIO == "non" ))  {
	 tkmessageBox(message = "L'état écologique ne peut pas être calculé sans les états physicochimiques et biologiques.")
	 
	} else  {
	 tkdestroy(tt)
	source(paste(pathS,"GUI_tab3.r",sep="") , echo = TRUE, print.eval = TRUE)  ## passe à la fenêtre suivante
	}
}

fct_Aide <-function() { browseURL(paste(AidePath,"Aide2.htm",sep=""))}


## Fonction pour bouton PRECEDENT
Retour<-function()  ## enregistress les options dans des objets et va lire les CSV
{
tkdestroy(tt)
pathsite<<-gsub(paste0(MODULE,"/"),"",pathsite)
source(paste(pathS,"GUI_tab1.r",sep="") , echo = TRUE, print.eval = TRUE)
}



##boutons < > et ?
returnbut <- tkbutton2(framebut,text="  <  ",command=Retour, font  = fontbouton ) 
OK.but <- tkbutton2(framebut,text="  >  ",command=OnOK, font  = fontbouton) 
fct_Aide <-function() { browseURL(paste(AidePath,"Aide2.htm",sep=""))}
AideBut <- tkbutton2(framebut,text="  ?  ",command=fct_Aide, font  = fontbouton) 

tkgrid(framebut)
tkgrid(returnbut,OK.but,tklabel2(framebut,text="", font = fontvide), AideBut)
tkconfigure(OK.but ,cursor="hand2")
tkconfigure(returnbut ,cursor="hand2")
tkconfigure(AideBut ,cursor="hand2")
tkgrid(tklabel2(framebut,text="    ", font = fontvide))
tkgrid(tklabel2(framebut,text="    ", font = fontvide))
tkgrid(tklabel2(framebut,text="    ", font = fontvide))


tkwm.state(tt,"normal")
tcl("wm", "attributes", tt, topmost=FALSE)
tkwait.window(tt)







