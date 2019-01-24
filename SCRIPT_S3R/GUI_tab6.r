###########################
# INTERFACE ETAPE 6  ######
##########################

## Creation de la fenetre
DEBUTGUI(Titre = "S3R Calcul", geom =  "575x550")
tkconfigure(tt,cursor="watch") # souris sablier


###framae formulaire
frameform <- tkframe(tt)
tkconfigure(frameform, bg=colorbg)
tkgrid(frameform)
tkgrid(tklabel2(frameform,text="    ", font = fontvide))
tkgrid(tklabel2(frameform,text="    ", font = fontvide))

#### Lancement du script de calcul
fontcalc <- tkfont.create(family="calibri",size=15,weight="bold")
tkgrid(tklabel3(tt,text="  ETAT D'AVANCEMENT DES CALCULS", font=fontlabrad), sticky="w")
tkgrid(tklabel2(tt,text="    - Démarrage"), sticky="w")
tcl("update")

print(paste("le chemin par défaut est : ",racine))
tkwm.state(tt,"normal")
source(paste(pathS,"SEEE_CE.r",sep=""), echo = TRUE, print.eval = TRUE)
tcl("update")
tkdestroy(tt)

source(paste(pathS,"GUI_tab7.r",sep=""), echo = TRUE, print.eval = TRUE)


