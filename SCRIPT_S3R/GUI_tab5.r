
###########################
# INTERFACE ETAPE 5 ######
##########################
#######

## Creation de la fenetre
DEBUTGUI(Titre = "S3R (5/5)")

###frame formulaire
frameform <- tkframe(tt)
tkconfigure(frameform, bg=colorbg)
tkgrid(frameform)
tkgrid(tklabel2(frameform,text="    ", font = fontvide))
tkgrid(tklabel2(frameform,text="    ", font = fontvide))
tkgrid(tklabel2(frameform,text="    ", font = fontvide))

tkgrid(tklabel3(frameform,text="MODIFICATION DES TABLES STATION & MASSE D'EAU : ",font=fontlabrad ), sticky="w")
tkgrid(tklabel2(frameform,text="    ", font = fontvide))

## modifier les stations : on propose d'ouvrir le fichier Excel 
XLS_STATION<-NAMEFILESTAT
mod_station <- function(){
	if (file.exists(XLS_STATION)) { 
		browseURL(XLS_STATION)  
	} else { 
		tkmessageBox(message = "Le fichier STATION.csv est absent dans /TABLES/STATION.csv")
	}
}
but_station <- tkbutton(frameform, text = "Modifier", command = mod_station, bg = colorbg)
tkgrid( tklabel2(frameform,text="      - Table station") , but_station, sticky="w")
tkconfigure(but_station ,cursor="hand2")
tkgrid(tklabel2(frameform,text="    ", font = fontvide))

## modifier les massed'eau : on propose d'ouvrir le fichier Excel 
#XLS_MASSEDEAU<-paste(racine,"/TABLES/MASSEDEAU.xls",sep="")
if ((SEEEECOLOME=="oui" |  SEEECHIMME=="oui" | CONTAME == "oui") & CEPE == "CE") {
XLS_MASSEDEAU<-NAMEFILEME
mod_MASSEDEAU <- function(){
	if (file.exists(XLS_MASSEDEAU)) { 
		browseURL(XLS_MASSEDEAU)
	} else { 
		tkmessageBox(message = "Le fichier MASSEDEAU.csv est absent dans /TABLES/MASSEDEAU.csv")
	}
}
but_MASSEDEAU <- tkbutton(frameform, text = "Modifier", command = mod_MASSEDEAU, bg = colorbg)
tkgrid( tklabel2(frameform,text="      - Table masse d'eau") , but_MASSEDEAU, sticky="w")
tkconfigure(but_MASSEDEAU ,cursor="hand2")
tkgrid(tklabel2(frameform,text="    ", font = fontvide))
tkgrid(tklabel2(frameform,text="    ", font = fontvide))
tcl("update")
}

###################################
tkgrid(tklabel3(frameform,text="TABLES DE PARAMETRES A UTILISER :",font=fontlabrad ), sticky="w")
tkgrid(tklabel2(frameform,text="    ", font = fontvide))

##Possibilité de prendre les paramètres par défault ou personnalisé
frameRB <- tkframe(frameform)
tkconfigure(frameRB, bg=colorbg)
tkgrid(frameRB)
rb1 <- tkradiobutton(frameRB)
rb2 <- tkradiobutton(frameRB)
if(!exists("rbparamdef")) { rbparamdef<- tclVar("default") }
tkconfigure(rb1,variable=rbparamdef,value="perso", bg=colorbg)
tkconfigure(rb2,variable=rbparamdef,value="default", bg=colorbg)
tkgrid(tklabel2(frameRB,text="                  Personnalisées"),rb1,tklabel2(frameform,text="      Par défaut"),rb2,sticky="w")
tkgrid(tklabel2(frameRB,text="    ", font = fontvide))
tkgrid(tklabel2(frameRB,text="    ", font = fontvide))

## modifier les tables de paramètres
CH_PARAM<-paste(racine,"/PARAMETRES/PERSONNALISE",sep="")
mod_PARAM <- function(){ if (file.exists(CH_PARAM)) { browseURL(CH_PARAM)  }}
but_PARAM <- tkbutton(frameform, text = "Modifier", command = mod_PARAM, bg = colorbg)
tkgrid( tklabel2(frameform,text="      - Modifier les tables personnalisées") , but_PARAM, sticky="w")
tkconfigure(but_PARAM ,cursor="hand2")
tkgrid(tklabel2(frameform,text="    ", font = fontvide))
tcl("update")
	
###FRAME BOUTONS
## affichage des bouton
framebut <- tkframe(tt)
tkconfigure(framebut, bg=colorbg)

##fonction du Bouton
OnOK <- function()
{
	 CHOIXPARAM <<- as.character(tclvalue(rbparamdef)) 
	 gc()
	 tkdestroy(tt)
	 source(paste(pathS,"GUI_tab6.r",sep="") , echo = TRUE, print.eval = TRUE)
}
## Affiche le bouton suivant

OKbut <- tkbutton2(framebut,text=" > DEMARRER",command=OnOK, font  = fontbouton ) 

## Fonction pour bouton PRECEDENT
Retour<-function()  ## enregistress les options dans des objets et va lire les CSV
{
	tkdestroy(tt)
	if (SEEEPEGASE == "oui") {
		source(paste(pathS,"GUI_tab4c.r",sep="") , echo = TRUE, print.eval = TRUE)
	} else {
		if (DATABIO_necesaire == "oui") {
			source(paste(pathS,"GUI_tab4b.r",sep="") , echo = TRUE, print.eval = TRUE)
		} else {
			if (DATAPCH_necesaire == "oui") {
				source(paste(pathS,"GUI_tab4a.r",sep="") , echo = TRUE, print.eval = TRUE)
			} else {
				source(paste(pathS,"GUI_tab3b.r",sep="") , echo = TRUE, print.eval = TRUE)
			}
		}
	}
}
returnbut <- tkbutton2(framebut,text="  <  ",command=Retour, font  = fontbouton ) 

##bouton Aide
fct_Aide <-function() { browseURL(paste(AidePath,"Aide5.htm",sep=""))}
AideBut <- tkbutton2(framebut,text="  ?  ",command=fct_Aide, font  = fontbouton) 
tkgrid(framebut)
tkgrid(returnbut,OKbut,tklabel2(framebut,text="", font = fontvide), AideBut)
tkgrid(tklabel2(framebut,text="    ", font = fontvide))
tkconfigure(OKbut ,cursor="hand2")
tkconfigure(AideBut ,cursor="hand2")
tkconfigure(returnbut ,cursor="hand2")
tkgrid(tklabel2(framebut,text="    ", font = fontvide))
tkgrid(tklabel2(framebut,text="    ", font = fontvide))
tkwm.state(tt,"normal")
tkwait.window(tt)
#tkfocus(tt)