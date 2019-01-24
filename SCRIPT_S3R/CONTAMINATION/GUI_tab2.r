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



# Boutons radio
rb1 <- tkradiobutton(frameform)
rb2 <- tkradiobutton(frameform)
rb3 <- tkradiobutton(frameform)
rb4 <- tkradiobutton(frameform)
if(!exists("rbValeur")) { rbValeur<- tclVar("1") }

tkconfigure(rb1,variable=rbValeur,value="1", bg=colorbg)
tkconfigure(rb2, variable=rbValeur,value="2", bg=colorbg)
tkconfigure(rb3, variable=rbValeur,value="3", bg=colorbg)
tkconfigure(rb4, variable=rbValeur,value="4", bg=colorbg)


	## Contamination Aigüe
tkgrid( tklabel2(frameform,text="      Contamination Aigüe  ", font = fontnormal) , rb1, sticky="w")
tkgrid(tklabel2(frameform,text="    ", font = fontvide))

	## Contamination Aigüe
tkgrid( tklabel2(frameform,text="      Contamination Chronique  ", font = fontnormal) , rb2, sticky="w")
tkgrid(tklabel2(frameform,text="    ", font = fontvide))

	## Contamination Imprégniation
tkgrid( tklabel2(frameform,text="      Imprégnation  ", font = fontnormal) , rb3, sticky="w")
tkgrid(tklabel2(frameform,text="    ", font = fontvide))

	## Somme de pesticides
tkgrid( tklabel2(frameform,text="      Somme de pesticides  ", font = fontnormal) , rb4, sticky="w")
tkgrid(tklabel2(frameform,text="    ", font = fontvide))

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

	 TYPECONTA<<-as.numeric(as.character(tclvalue(rbValeur)))

	SEEEPCH <<- "non"  
	SEEEPS <<- "non"  
	SEEEBIO <<- "non"  
	SEEEECOLO <<- "non"  
	SEEECHIM <<- "non"   

	 tkdestroy(tt)

	source(paste(pathS,"/CONTAMINATION/GUI_tab3.r",sep="") , echo = TRUE, print.eval = TRUE)  ## passe à la fenêtre suivante
	
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







