

###########################
# INTERFACE ETAPE 3b ######
##########################


# cherche les parametres de mot de passe par défault
if (file.exists(paste(racine,"/PARAMETRES/INI/STA.DAT",sep=""))) {load(paste(racine,"/PARAMETRES/INI/STA.DAT",sep=""))}
if (file.exists(paste(racine,"/PARAMETRES/INI/ME.DAT",sep=""))) {load(paste(racine,"/PARAMETRES/INI/ME.DAT",sep=""))}
if (file.exists(paste(racine,"/PARAMETRES/INI/PCH.DAT",sep=""))) {load(paste(racine,"/PARAMETRES/INI/PCH.DAT",sep=""))}
if (file.exists(paste(racine,"/PARAMETRES/INI/BIO.DAT",sep=""))) {load(paste(racine,"/PARAMETRES/INI/BIO.DAT",sep=""))}
if (file.exists(paste(racine,"/PARAMETRES/INI/MODELISE.DAT",sep=""))) {load(paste(racine,"/PARAMETRES/INI/MODELISE.DAT",sep=""))}

if (!file.exists(paste(racine,"/PARAMETRES/INI/",sep=""))) {dir.create(paste(racine,"/PARAMETRES/INI/",sep=""))}


## Creation de la fenetre
DEBUTGUI(Titre = "S3R (3b/5)")


###frame formulaire
frameform <- tkframe(tt)
tkconfigure(frameform, bg=colorbg)
tkgrid(frameform)
tkgrid(tklabel2(frameform,text="    ", font = fontvide))
tkgrid(tklabel2(frameform,text="    ", font = fontvide))
tkgrid(tklabel2(frameform,text="    ", font = fontvide))

### Choix du fichier PCH . Le nom du fichier sera attribué à NAMEFILEPCH

if ( SEEEPCH == "oui"  | SEEEPS == "oui" | SEEECHIM == "oui" | CONTA == "oui" ) {
if (!exists("NAMEFILEPCH")) {NAMEFILEPCH<-""}
	
	getfilepch <- function() {
	tcl("wm", "attributes", tt, topmost=FALSE)
	NAMEFILEPCH <<- tclvalue(tkgetOpenFile(
	filetypes = " {{CSV Files} {.csv}} " , initialdir=dirname(NAMEFILEPCH)) )
		if (!nchar(NAMEFILEPCH)) {
		tkmessageBox(message = "Aucun fichier sélectionné", icon = "warning")
		} else {
			tkconfigure(entrychpch,textvariable=tclVar(NAMEFILEPCH))  # permet de mettre à jour le label avec le chemin du fichier csv
			tcl("update")
		}
		tcl("wm", "attributes", tt, topmost=TRUE) ## fenetre  en 1er plan
	}
	 ## label et bouton
	labselectfile<-tklabel3(frameform,text="IMPORTER LES DONNEES PHYSICOCHIMIQUES ou CHIMIQUES",font=fontlabrad )
	buttonselpch <- tkbutton2(frameform, text = " ... ", command = getfilepch )
	tkgrid(labselectfile,  sticky="w")
		
	##permet de mettre le nom du chemin en vert
	entrychpch <- tkentry(frameform, width = 85,textvariable = "")
	tkconfigure(entrychpch,textvariable=tclVar(""))
	tkgrid( entrychpch, tklabel2(frameform,text="   "), buttonselpch ,sticky="w")
	tkconfigure(buttonselpch ,cursor="hand2")
	tkgrid(tklabel2(frameform,text="    ", font = fontvide))
	tkgrid(tklabel2(frameform,text="    ", font = fontvide))
	tkgrid(tklabel2(frameform,text="    ", font = fontvide))
} else {rm(NAMEFILEPCH)}


### Choix du fichier PCH . Le nom du fichier sera attribué à NAMEFILEBIO
if ( SEEEBIO == "oui") {
if (!exists("NAMEFILEBIO")) {NAMEFILEBIO<-""}
		
		getfilebio <- function() {
		tcl("wm", "attributes", tt, topmost=FALSE)
			NAMEFILEBIO <<- tclvalue(tkgetOpenFile(
			filetypes = " {{CSV Files} {.csv}} " , initialdir=dirname(NAMEFILEBIO)) )
			if (!nchar(NAMEFILEBIO)) {
				tkmessageBox(message = "Aucun fichier sélectionné" , icon = "warning")
			} else {
				tkconfigure(entrychbio,textvariable=tclVar(NAMEFILEBIO))  # permet de mettre à jour le label avec le chemin du fichier csv
				tcl("update")

			}
		tcl("wm", "attributes", tt, topmost=TRUE) ## fenetre  en 1er plan
		}

		  ## label et bouton
		labselectfile<-tklabel3(frameform,text="IMPORTER LES DONNEES BIOLOGIQUES",font=fontlabrad )
		buttonselbio <- tkbutton2(frameform, text = " ... ", command = getfilebio )
		tkgrid(labselectfile,  sticky="w")
		
		  ##permet de mettre le nom du chemin en vert
		entrychbio <- tkentry(frameform, width = 85,textvariable = "")
		tkconfigure(entrychbio,textvariable=tclVar(""))
		tkgrid( entrychbio, tklabel2(frameform,text="   "), buttonselbio ,sticky="w")
		tkconfigure(buttonselbio ,cursor="hand2")
		tkgrid(tklabel2(frameform,text="    ", font = fontvide))
		tkgrid(tklabel2(frameform,text="    ", font = fontvide))
		tkgrid(tklabel2(frameform,text="    ", font = fontvide))
} else {rm(NAMEFILEBIO)}


### Choix du fichier PEGASE
if ( SEEEPEGASE == "oui" ) {
if (!exists("NAMEFILEPEG")) {NAMEFILEPEG<-""}
	
	##choix du fichier
		getfilepeg <- function() {
			tcl("wm", "attributes", tt, topmost=FALSE)
			NAMEFILEPEG <<- tclvalue(tkgetOpenFile(
			filetypes = " {{CSV Files} {.csv}} ", initialdir=dirname(NAMEFILEPEG)) )
			if (!nchar(NAMEFILEPEG)) {
				tkmessageBox(message = "Aucun fichier sélectionné", icon = "warning")
			} else {

				tkconfigure(entrychpeg,textvariable=tclVar(NAMEFILEPEG))  # permet de mettre à jour le label avec le chemin du fichier csv
				tcl("update")

			}
		tcl("wm", "attributes", tt, topmost=TRUE) ## fenetre  en 1er plan
		}

		  ## label et bouton
		labselectfile<-tklabel3(frameform,text="IMPORTER LES DONNEES MODELISEES ",font=fontlabrad )
		buttonselpeg <- tkbutton2(frameform, text = " ... ", command = getfilepeg )
		tkgrid(labselectfile,  sticky="w")
		
		  ##permet de mettre le nom du chemin en vert
		entrychpeg <- tkentry(frameform, width = 85,textvariable = "")
	    tkconfigure(entrychpeg,textvariable=tclVar(""))
		tkgrid( entrychpeg, tklabel2(frameform,text="   "), buttonselpeg,sticky="w")
		tkconfigure(buttonselpeg ,cursor="hand2")
		##particularité pour pegase : radiobouton
		frameRB <- tkframe(frameform)
		tkconfigure(frameRB, bg=colorbg)
		tkgrid(frameRB)
		rb1 <- tkradiobutton(frameRB)
		rb2 <- tkradiobutton(frameRB)
		rbValue <- tclVar("valeur")
		tkconfigure(rb1,variable=rbValue,value="valeur", bg=colorbg)
		tkconfigure(rb2,variable=rbValue,value="classe", bg=colorbg)
		tkgrid(tklabel2(frameRB,text="   valeurs brutes "),rb1,tklabel2(frameform,text="      classes d'état "),rb2,sticky="w")
		tkgrid(tklabel2(frameRB,text="    ", font = fontvide))
		tkgrid(tklabel2(frameRB,text="    ", font = fontvide))
		tkgrid(tklabel2(frameRB,text="    ", font = fontvide))
} else { 
	if (exists("NAMEFILEPEG")) {rm(NAMEFILEPEG)}
	   }


### Choix du fichier STATIONS
if (!exists("NAMEFILESTAT")) {NAMEFILESTAT<-paste(racine,"/TABLES/STATION.csv",sep="")}
	
	##choix du fichier
		getfilestat <- function() {
			tcl("wm", "attributes", tt, topmost=FALSE)
			NAMEFILESTAT <<- tclvalue(tkgetOpenFile(
			filetypes = " {{CSV Files} {.csv}} ", initialdir=dirname(NAMEFILESTAT)))
			if (!nchar(NAMEFILESTAT)) {
				tkmessageBox(message = "Aucun fichier sélectionné", icon = "warning")
			} else {

				tkconfigure(entrychstat,textvariable=tclVar(NAMEFILESTAT))  # permet de mettre à jour le label avec le chemin du fichier csv
				tcl("update")

			}
		tcl("wm", "attributes", tt, topmost=TRUE) ## fenetre  en 1er plan
		}

		  ## label et bouton
		labselectfile<-tklabel3(frameform,text="CHEMIN DE LA TABLE STATION",font=fontlabrad )
		buttonselpeg <- tkbutton2(frameform, text = " ... ", command = getfilestat )
		tkgrid(labselectfile,  sticky="w")
		
		  ##permet de mettre le nom du chemin en vert
		entrychstat <- tkentry(frameform, width = 85,textvariable = NAMEFILESTAT)
	    tkconfigure(entrychstat,textvariable=tclVar(NAMEFILESTAT))
		tkgrid( entrychstat, tklabel2(frameform,text="   "), buttonselpeg,sticky="w")
		tkconfigure(buttonselpeg ,cursor="hand2")
		tkgrid(tklabel2(frameform,text="    ", font = fontvide))
		tkgrid(tklabel2(frameform,text="    ", font = fontvide))


### Choix du fichier MASSEDEAU
if ( (SEEEECOLOME=="oui" |  SEEECHIMME=="oui" | CONTAME == "oui") & CEPE == "CE" ) {	
	if (!exists("NAMEFILEME")) {NAMEFILEME<-paste(racine,"/TABLES/MASSEDEAU.csv",sep="")}
	##choix du fichier
		getfileme <- function() {
			tcl("wm", "attributes", tt, topmost=FALSE)
			NAMEFILEME <<- tclvalue(tkgetOpenFile(
			filetypes = " {{CSV Files} {.csv}} ", initialdir=dirname(NAMEFILEME)))
			if (!nchar(NAMEFILEME)) {
				tkmessageBox(message = "Aucun fichier sélectionné", icon = "warning")
			} else {

				tkconfigure(entrychme,textvariable=tclVar(NAMEFILEME))  # permet de mettre à jour le label avec le chemin du fichier csv
				tcl("update")

			}
		tcl("wm", "attributes", tt, topmost=TRUE) ## fenetre  en 1er plan
		}

		  ## label et bouton
		labselectfile<-tklabel3(frameform,text="CHEMIN DE LA TABLE MASSEDEAU",font=fontlabrad )
		buttonselpeg <- tkbutton2(frameform, text = " ... ", command = getfileme )
		tkgrid(labselectfile,  sticky="w")
		
		  ##permet de mettre le nom du chemin en vert
		entrychme <- tkentry(frameform, width = 85,textvariable = NAMEFILEME)
	    tkconfigure(entrychme,textvariable=tclVar(NAMEFILEME))
		tkgrid( entrychme, tklabel2(frameform,text="   "), buttonselpeg,sticky="w")
		tkconfigure(buttonselpeg ,cursor="hand2")
		tkgrid(tklabel2(frameform,text="    ", font = fontvide))
		tkgrid(tklabel2(frameform,text="    ", font = fontvide))		
} else {  rm(NAMEFILEME) }		
		


###FRAME BOUTONS
## affichage des bouton
framebut <- tkframe(tt)
tkconfigure(framebut, bg=colorbg)


### FONCTION BOUTON
OnOK <- function()  ## enregistress les options dans des objets et va lire les CSV. Vérifie que le fichier a un nombe de colonne cohérent
{
	
	sortie<-1

	if (SEEEPCH == "oui"  | SEEEPS == "oui" | SEEECHIM == "oui" | CONTA == "oui") {
		if (NAMEFILEPCH != "" ) {  ## si un fichier a bien été sélectionné
			tkconfigure(tt,cursor="watch")
			DATAPCH<<-read.csv2(NAMEFILEPCH)
			save(NAMEFILEPCH,file = paste0(racine,"/PARAMETRES/INI/PCH.DAT"))
			if(ncol(DATAPCH) < 3) {
				tkmessageBox(message = "Le fichier de données Physico-chimiques n'a pas assez de colonnes. \n Vérifiez que le fichier est bien au format CSV avec séparation point-vigule")
				sortie <-0
			 }
			tkconfigure(tt,cursor="arrow")
			 } else {tkmessageBox(message = "Veuillez importer les données physicochimiques/chimiques") ; sortie <-0}}

	if (sortie == 1 & SEEEBIO == "oui"){	 
	if (NAMEFILEBIO != "" ) {  ## si un fichier a bien été sélectionné
		tkconfigure(frameform,cursor="watch")
		DATABIO<<-read.csv2(NAMEFILEBIO)
		save(NAMEFILEBIO,file = paste0(racine,"/PARAMETRES/INI/BIO.DAT"))
		if(ncol(DATABIO) < 3) {
				tkmessageBox(message = "Le fichier de données Biologiques n'a pas assez de colonnes. \n Vérifiez que le fichier est bien au format CSV avec séparation point-vigule")
				sortie <-0
		 }
		tkconfigure(tt,cursor="arrow")
		 } else {tkmessageBox(message = "Veuillez importer les données biologiques"); sortie <-0}}	 

	if (sortie == 1 & SEEEPEGASE == "oui"){
	if (NAMEFILEPEG != "") {  ## si un fichier a bien été sélectionné
		tkconfigure(tt,cursor="watch")
		DATAPEGASE<<-read.csv2(NAMEFILEPEG)
		save(NAMEFILEPEG,file = paste0(racine,"/PARAMETRES/INI/MODELISE.DAT"))
		if(ncol(DATAPEGASE) < 3) {
				tkmessageBox(message = "Le fichier de données modélisées n'a pas assez de colonnes. \n Vérifiez que le fichier est bien au format CSV avec séparation point-vigule")
				sortie <-0
		 }	
		SEEEPEGASEDATA <<- as.character(tclvalue(rbValue)) 
		tkconfigure(tt,cursor="arrow")
		} else {tkmessageBox(message = "Veuillez importer les données modélisées"); sortie <-0}}	 
	
	if (sortie == 1 ){
	if (NAMEFILESTAT != "" & file.exists(NAMEFILESTAT)) {  ## si un fichier a bien été sélectionné
		tkconfigure(tt,cursor="watch")
		STATION<<-read.csv2(NAMEFILESTAT)
		save(NAMEFILESTAT,file = paste0(racine,"/PARAMETRES/INI/STA.DAT"))
		if(ncol(STATION) < 3) {
				tkmessageBox(message = "Le fichier de données STATION n'a pas assez de colonnes. \n Vérifiez que le fichier est bien au format CSV avec séparation point-vigule")
				sortie <-0
		 }	
		} else {tkmessageBox(message = "Veuillez sélectionner une table STATION"); sortie <-0}}	 
	
	if (sortie == 1 & (SEEEECOLOME=="oui" |  SEEECHIMME=="oui" |  CONTAME == "oui") & CEPE == "CE"  ) { 
	if ( NAMEFILEME!= "" & file.exists(NAMEFILEME)) {  ## si un fichier a bien été sélectionné
		tkconfigure(tt,cursor="watch")
		MASSEDEAU<<-read.csv2(NAMEFILEME)
		save(NAMEFILEME,file = paste0(racine,"/PARAMETRES/INI/ME.DAT"))
		if(ncol(MASSEDEAU) < 3) {
				tkmessageBox(message = "Le fichier de données MASSEDEAU n'a pas assez de colonnes. \n Vérifiez que le fichier est bien au format CSV avec séparation point-vigule")
				sortie <-0
		 }	
		} else {tkmessageBox(message = "Veuillez sélectionner une table MASSEDEAU"); sortie <-0}}

	
	if(sortie==1){
	tkdestroy(tt)
	source(paste(pathS,"GUI_tab4a.r",sep="") , echo = TRUE, print.eval = TRUE)

		} 
	
}
fontbouton <- tkfont.create(family="calibri",size=15,weight="bold")
OKbut <- tkbutton2(framebut,text="  >  ",command=OnOK, font  = fontbouton ) 

## Fonction pour bouton PRECEDENT
Retour<-function()  ## enregistres les options dans des objets et va lire les CSV
{
	tkdestroy(tt)
	
	if (CONTA == "oui" & TYPECONTA == 4 ) {
		source(paste(pathS,"/CONTAMINATION/GUI_tab3.r",sep="") , echo = TRUE, print.eval = TRUE)
	
		source(paste(pathS,"GUI_tab3a.r",sep="") , echo = TRUE, print.eval = TRUE)

}
	
}
returnbut <- tkbutton2(framebut,text="  <  ",command=Retour, font  = fontbouton ) 

##bouton Aide
fct_Aide <-function() { browseURL(paste(AidePath,"Aide4.htm",sep=""))}
AideBut <- tkbutton2(framebut,text="  ?  ",command=fct_Aide, font  = fontbouton) 


tkgrid(framebut)
tkgrid(returnbut,OKbut,tklabel2(framebut,text="", font = fontvide), AideBut)
tkconfigure(OKbut ,cursor="hand2")
tkconfigure(AideBut ,cursor="hand2")
tkconfigure(returnbut ,cursor="hand2")
tkgrid(tklabel2(framebut,text="    ", font = fontvide))
tkgrid(tklabel2(framebut,text="    ", font = fontvide))
tkgrid(tklabel2(framebut,text="    ", font = fontvide))


tkwm.state(tt,"normal")
tcl("wm", "attributes", tt, topmost=FALSE)
tkwait.window(tt)
#tkfocus(tt)





