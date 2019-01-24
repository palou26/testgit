

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
if (exists("LOGIN")) {LOGINVALUE <- tclVar(LOGIN)} else {LOGINVALUE <- tclVar("user1")}
if (exists("CLE")) {PSWVALUE <- tclVar(CLE)} else {PSWVALUE <- tclVar("")}
entrylogin <-tkentry(frame2,width="30",textvariable=LOGINVALUE)
entrypwd <-tkentry(frame2,width="10",textvariable=PSWVALUE, show="*")
tkgrid(tklabel2(frame2,text="    ", font = fontvide))
tkgrid(tklabel2(frame2,text="    ", font = fontvide))
tkgrid(tklabel2(frame2,text="Utilisateur :",font=fontlabrad ),entrylogin ,tklabel2(frame2,text="    Clé :",font=fontlabrad ),entrypwd , sticky="w")
tkgrid(tklabel2(frame2,text="    ", font = fontvide))
tkgrid(tklabel2(frame2,text="    ", font = fontvide))
tcl("update")

###COMMENTAIRES
frame5 <- tkframe(tt) # création d'un nouveau frame
tkconfigure(frame5, bg=colorbg)
tkgrid(frame5)
if (exists("COMMENTAIRE")) {COMMENTAIREVALUE <- tclVar(COMMENTAIRE)} else {COMMENTAIREVALUE <- tclVar("")}
entrycomm <-tkentry(frame5,width="70",textvariable=COMMENTAIREVALUE)
tkgrid(tklabel2(frame5,text="    ", font = fontvide))
tkgrid(tklabel2(frame5,text="    ", font = fontvide))
tkgrid(tklabel2(frame5,text="Commentaires :",font=fontlabrad ), sticky="w")
tkgrid(entrycomm , sticky="w")
tkgrid(tklabel2(frame5,text="    ", font = fontvide))
tcl("update")



### Choix du dossier  où seront déposer les resultats . 

if(!exists("SEEE_DEBformat")) {SEEE_DEBformat<-paste( format(Sys.time(), "%Y%m%d%H%M"),sep="") }
if(!exists("CH_OUTPUT")  ){CH_OUTPUT<-paste0(racine,"/RESULTATS/",SEEE_DEBformat,"/")} else if  (!file.exists(CH_OUTPUT)) {
CH_OUTPUT<-paste0(racine,"/RESULTATS/",SEEE_DEBformat,"/")}


CH_OUTPUT<-gsub("//","/",CH_OUTPUT)
CH_OUTPUT_VALUE <- tclVar(CH_OUTPUT)

	
	getfileresult <- function() {
	tcl("wm", "attributes", tt, topmost=FALSE)
	CH_OUTPUT <<- tclvalue(tkchooseDirectory( ))
		if (!nchar(CH_OUTPUT)) {
		tkmessageBox(message = "Aucun fichier sélectionné", icon = "warning")
		} else {
			tkconfigure(entrychresult,textvariable=tclVar(CH_OUTPUT))
			CH_OUTPUT_VALUE <<- tclVar(CH_OUTPUT)
			#
			tcl("update")
		}
		tcl("wm", "attributes", tt, topmost=TRUE) ## fenetre  en 1er plan
	}
	 ## label et bouton
	labselectfile<-tklabel2(frame5,text="Sélectionner le dossier RESULTAT",font=fontlabrad )
	buttonselresult <- tkbutton2(frame5, text = " ... ", command = getfileresult )
	tkgrid(labselectfile,  sticky="w")
		
	##permet de mettre le nom du chemin
	entrychresult <- tkentry(frame5, width = 73,textvariable = CH_OUTPUT_VALUE)
	tkgrid( entrychresult, tklabel2(frame5,text="   "), buttonselresult ,sticky="w")
	tkconfigure(buttonselresult ,cursor="hand2")
	tkgrid(tklabel2(frame5,text="    ", font = fontvide))


	###NOM DU FICHIER RESULTAT
	if (exists("NAMEOUTPUT")) {NAMEOUTPUTVALUE <- tclVar(NAMEOUTPUT)} else {NAMEOUTPUTVALUE <- tclVar("S3R_ESU_")}
	entrynamoutput <-tkentry(frame5,width="70",textvariable=NAMEOUTPUTVALUE)
	tkgrid(tklabel2(frame5,text="    ", font = fontvide))
	tkgrid(tklabel2(frame5,text="    ", font = fontvide))
	tkgrid(tklabel2(frame5,text="Nom du fichier de sortie : (préfixe)",font=fontlabrad ), sticky="w")
	tkgrid(entrynamoutput , sticky="w")
	tkgrid(tklabel2(frame5,text="    ", font = fontvide))
	tkgrid(tklabel2(frame5,text="    ", font = fontvide))
	tkgrid(tklabel2(frame5,text="    ", font = fontvide))
	tcl("update")


	###CHOIX PE/CE
	tkgrid(tklabel3(frame5,text="CHOIX DES EAUX DE SURFACE :   ",font=fontlabrad ), sticky="w")
	frame6 <- tkframe(tt) # création d'un nouveau frame
	tkconfigure(frame6, bg=colorbg)
	tkgrid(frame6)	
	tkgrid(tklabel2(frame6,text="    ", font = fontvide))

	rbCE <- tkradiobutton(frame6)
	rbPE <- tkradiobutton(frame6)
	if(!exists("rbCEPE")) { rbCEPE<- tclVar("CE") }
	tkconfigure(rbCE, variable=rbCEPE,value="CE", bg=colorbg)
	tkconfigure(rbPE,variable=rbCEPE,value="PE", bg=colorbg)
	tkgrid( rbCE, tklabel2(frame6,text="  Cours d'eau", font = fontnormal) , sticky="w")
	tkgrid( rbPE, tklabel2(frame6,text="  Plan d'eau", font = fontnormal) , sticky="w")
	tkgrid(tklabel2(frame6,text="    ", font = fontvide))
	tcl("update")

###Fonction de chaque bouton 
OnOK <- function(Dossier)
{ 

	#vérification di login et mot de passe et enregistrement du commentaire et du nom de fichier de sortie
	LOGIN <<- as.character(tclvalue(LOGINVALUE))
	CLE <<- as.character(tclvalue(PSWVALUE))
	COMMENTAIRE <<- as.character(tclvalue(COMMENTAIREVALUE))
	NAMEOUTPUT <<- as.character(tclvalue(NAMEOUTPUTVALUE))
	CH_OUTPUT <<- as.character(tclvalue(CH_OUTPUT_VALUE))
	CONTA<<-"non"
	
	source(paste(pathS,"LIC2.r",sep="") , echo = TRUE, print.eval = TRUE)
    
	#enregistrement dans ini
	if (validok == "oui") {
		if (as.character(tclvalue(rbCEPE)) == "CE") { CEPE <<- "CE" } else { CEPE <<- "PE" }
		save(LOGIN,CLE,file=paste(racine,"/PARAMETRES/INI/ini.DAT",sep=""))
		if (pathsite == paste(pathS,"/",sep="") ) {pathsite<<-paste(pathsite,Dossier,"/",sep="")}
		MODULE<<-Dossier
		
		#création du fichier resultat et erreur
		if (file.exists(CH_OUTPUT)){
			CH_OUTPUT<<-gsub("//","/",CH_OUTPUT)
			save(CH_OUTPUT,file = paste0(racine,"/PARAMETRES/INI/CH_OUTPUT.DAT"))
			CH_OUTPUT<<-paste0(CH_OUTPUT,"/")
			if ( !file.exists(CH_OUTPUT) ) { dir.create(CH_OUTPUT) }
			CH_ERREUR<<-paste0(CH_OUTPUT,"/ERREURS/")
			CH_ERREUR<<-gsub("//","/",CH_ERREUR)
			if ( !file.exists(CH_ERREUR) ) { dir.create(CH_ERREUR) }
			
			tkdestroy(tt) # fermeture de la fenêtre
			#ouvre le bon dossier
			if (Dossier == "COMPARAISON"){
				try(source(paste(pathS,"COMPARAISON/GUI_tab2.r",sep="") , echo = TRUE, print.eval = TRUE))
			} else if (Dossier == "CONTAMINATION") {
				CONTA<<-"oui"
				try(source(paste(pathS,"CONTAMINATION/GUI_tab2.r",sep="") , echo = TRUE, print.eval = TRUE))
			} else {
				try(source(paste(pathS,"GUI_tab2.r",sep="") , echo = TRUE, print.eval = TRUE))	
			}
		} else {
			tkmessageBox(message = "le chemin du répertoire de résultats n'existe pas\nVeuillez choisir un autre dossier", icon = "warning")
			}
	} else {
	quit()
	}	
}


###BOUTON indiquant qu'un module est en dev
OnOK_endev <- function()
{ 
tkmessageBox(message = "Module non disponible. Veuillez consulter Asconit")
}

print(pathS)
#fct_Aide <-function() { browseURL(paste0(pathS,"HELP/Aide1.htm"))}
fct_Aide <-function() { browseURL(paste0(AidePath,"Aide1.htm"))}



OnOKCycle1 <- function(){  OnOK("REEE2010") }
OnOKCycle2 <- function(){  OnOK("REEE2016") }
OnOKCycle3 <- function(){  OnOK("REEE2018") }
OnOKCONTA <- function(){  OnOK("CONTAMINATION") }
#OnOKCycle3 <- function(){  OnOK_endev() }
#OnOKCOMPA <- function(){ OnOK_endev() }
#OnOKCOMPI <- function(){ OnOK_endev() }
OnOKCOMPA <- function(){  OnOK("COMPARAISON") }
OnOKCOMPI <- function(){
source(paste0(pathS,"COMPILATION/compilation.r"))
 }


#nouveau frame : frame3 avec les 3  boutons selon le cycle de l'état à déterminer
frame3 <- tkframe(tt)
tkconfigure(frame3, bg=colorbg)
tkgrid(frame3)
OK.butCycle1 <- tkbutton2(frame3,text=" REEE2010 ",command=OnOKCycle1, font  = fontboutonp) 
OK.butCycle2 <- tkbutton2(frame3,text="  REEE2016 ",command=OnOKCycle2, font  = fontboutonp)
OK.butCycle3 <- tkbutton2(frame3,text="  REEE2018 ",command=OnOKCycle3, font  = fontboutonp)  



#affichage des 3 boutons :
tkgrid(tklabel2(frame3,text="    ", font = fontvide))
tkgrid(tklabel2(frame3,text="    ", font = fontvide)) 
	tkgrid(tklabel3(frame3,text="CHOIX DU MODE DE CALCUL\t", font = fontsstitre),    sticky="w")
	tkgrid(
		tklabel2(frame3,text="    ", font = fontvide),
		OK.butCycle1 ,
		tklabel2(frame3,text="    ", font = fontvide),
		OK.butCycle2,
		tklabel2(frame3,text="    ", font = fontvide),
		OK.butCycle3 ,    sticky="w")

##Affichage module de contamination avec nouveau frame

frame3bis <- tkframe(tt)
tkconfigure(frame3bis, bg=colorbg)
OK.butCONTA <- tkbutton2(frame3bis,text="  Module Contamination ",command=OnOKCONTA, font  = fontboutonp)  
tkgrid(frame3bis)
tkgrid(tklabel3(frame3bis,text=paste(rep(" ",90),collapse = ""), font = fontvide2),    sticky="w")

tkgrid(tklabel2(frame3bis,text="    ", font = fontvide), tklabel2(frame3bis,text="    ", font = fontvide),
tklabel2(frame3bis,text="    ", font = fontvide),OK.butCONTA  ,    sticky="e")
		
tkgrid(tklabel2(frame3bis,text="    ", font = fontvide))
tkgrid(tklabel2(frame3bis,text="    ", font = fontvide))
		
#nouveau frame : frame3bis avec les 2  boutons selon les traitements complémentaires
	
frame3ter <- tkframe(tt)
tkconfigure(frame3ter, bg=colorbg)
tkgrid(frame3ter)		

OK.butCOMPARAISON <- tkbutton2(frame3ter,text="  COMPARAISON ",command=OnOKCOMPA, font  = fontboutonp) 
OK.butCOMPILATION <- tkbutton2(frame3ter,text="  COMPILATION ",command=OnOKCOMPI, font  = fontboutonp) 	
OK.butHELP <- tkbutton2(frame3ter,text=" ? ",command=fct_Aide, font  = fontboutonp) 	
	
	tkgrid(tklabel3(frame3ter,text="AUTRES TRAITEMENTS     \t", font = fontsstitre),    sticky="w")	
	tkgrid(
		tklabel2(frame3ter,text="    ", font = fontvide),
		OK.butCOMPARAISON,
		tklabel2(frame3ter,text="    ", font = fontvide),
		OK.butCOMPILATION,
		tklabel2(frame3ter,text="    ", font = fontvide),
		OK.butHELP,    sticky="w"
		)
	
tkgrid(tklabel2(frame3ter,text="    ", font = fontvide))
tkgrid(tklabel2(frame3ter,text="    ", font = fontvide))
tkgrid(tklabel2(frame3ter,text="    ", font = fontvide))
tcl("update")

#affiche la main lorsque l'on passe le curseur sur le bouton
tkconfigure(OK.butCycle1,cursor="hand2")
tkconfigure(OK.butCycle2,cursor="hand2")
tkconfigure(OK.butCOMPARAISON,cursor="hand2")
tkconfigure(OK.butCOMPILATION,cursor="hand2")







#nouveau frame : frame4
frame4 <- tkframe(tt)
tkconfigure(frame4, bg=colorbg)
tkgrid(frame4)

#Ajouter un bouton pour "à propos"
OnOK_apropos <- function()
{ 	
source(paste(pathsite,"a_propos.r",sep="") , echo = TRUE, print.eval = TRUE)
}
fontboutonapropos <- tkfont.create(family="calibri",size=11, slant="italic")
apropos.but <- tkbutton(frame4,text="A propos de S3R",command=OnOK_apropos, font  = fontboutonapropos, relief = "flat", bg=colorbg, foreground=colorblue) 

print("ok")
#### ajouter une image
#image1 <- tclVar()
#fileimage<-paste(racine,"/TEMPLATE/IMAGE/logo_Asco.gif", sep="")
#if (file.exists(fileimage)) {tcl("image","create","photo",image1,file=fileimage)
#imgAsLabel <- tklabel2(frame4,image=image1,bg="white" )
espacevide<-tklabel2(frame4,text="                  ", font = fontvide)
espacevide2<-tklabel2(frame4,text="   ", font = fontvide)
labversion<-tklabel2(frame4,text=numversion, font = fontversion)
#tkgrid(apropos.but ,espacevide, imgAsLabel, espacevide2, labversion )
tkgrid(apropos.but ,espacevide,  espacevide2, labversion )

tkconfigure(apropos.but ,cursor="hand2")
tkgrid(tklabel2(frame4,text="    ", font = fontvide))

#}


### FINALISATION
tkwm.state(tt,"normal")  ## supprime le mode cachée (tkwm.state(tt,"withdrawn")) ## l'interface apparait, elle n'est plus cachée
tcl("wm", "attributes", tt, topmost=FALSE)
## importation de la library indispensable pour écrire dans Excel+ paramètre Java pour augmenter capacités mémoire
options( java.parameters = "-Xmx1g" )
library(XLConnect)
options( java.parameters = "-Xmx1g" )

tkwait.window(tt)


