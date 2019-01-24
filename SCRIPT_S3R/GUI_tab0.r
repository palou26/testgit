

######################################
# PARTIE A PARAMETRER PAR UTILISATEUR
######################################

##cherche le chemin absolu du dossier Aide
if (substr(pathS,1,4) == "http") {
	AidePath<-paste0(pathS,"HELP/") } else {
	AidePath<-paste0(getwd(),"/S3R/SCRIPTS/HELP/")
}

###########################
# INTERFACE ETAPE 1  ######
##########################

## Creation de la fenetre
DEBUTGUI(Titre = "S3R (1/5)")

###LOGIN
frame2 <- tkframe(tt) # création d'un nouveau frame
tkconfigure(frame2, bg=colorbg)
tkgrid(frame2)

tkgrid(tklabel2(frame2,text="Chargement des librairies ....",font=fontlabrad ),  sticky="w")
tcl("update")
tkwm.state(tt,"normal")  ## supprime le mode cachée (tkwm.state(tt,"withdrawn")) ## l'interface apparait, elle n'est plus cachée


options(java.parameters = "-Xmx1800m")
library(XLConnect)
options( java.parameters = "-Xmx1800m" )


tkdestroy(tt) # fermeture de la fenêtre
source(paste(pathS,"GUI_tab1.r",sep=""))


