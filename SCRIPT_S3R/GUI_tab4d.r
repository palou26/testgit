###########################
# INTERFACE ETAPE 4 ######
##########################
##ici on cherche à réorganiser le tableau de données Bio
####### voir 4a qui est similaire


if ( CONTA == "oui" & CHOIXSEUIL == "LIBRE" ) {

	#On importe la Grille de parametrage du module de contamination
	ch_param<-paste(racine,"/PARAMETRES/DEFAUT/",sep="")
	PARAMETRECONTALIBRE<-read.csv2(paste(ch_param,"GRILLECONTAMINATION.csv",sep=""))

	## Creation de la fenetre
	DEBUTGUI(Titre = "S3R (4d/5)")
		
		
	###framae formulaire
	frameform <- tkframe(tt)
	tkconfigure(frameform, bg=colorbg)
	tkgrid(frameform)
	tkgrid(tklabel2(frameform,text="    ", font = fontvide))
	tkgrid(tklabel2(frameform,text="    ", font = fontvide))

	### Choix des colonnes pour GRCONTA
	tkgrid(tklabel3(frameform,text="SEULS LIBRES  : ASSOCIER LES COLONNES", font = fontsstitre) ,  sticky="w")
	NAMESGRCONTA<-c("PARAMETRE", "PARAMETRELIB", "NOMCOURT", "FAMILLE", "FRACTION", "UNITE", "SEUIL_LIBRE1", "SEUIL_LIBRE2", "SEUIL_LIBRE3", "SEUIL_LIBRE4")
	NAMESGRCONTA_vide_autorise<<-c("SEUIL_LIBRE4" )
	tclRequire("BWidget")

	# boucle permettant d'afficher les combobox
	NAMESDATAGRCONTA<-names(PARAMETRECONTALIBRE)
	PARAMETRECONTALIBRE$non_defini<-as.character("")
	CBGRCONTA<-as.character()
	for (i in 1:length(NAMESGRCONTA)){
	if (NAMESGRCONTA[i] %in% NAMESGRCONTA_vide_autorise) {listnamesGRCONTA <- names(PARAMETRECONTALIBRE)} else{ listnamesGRCONTA <-NAMESDATAGRCONTA} 
	if (!is.na(names(PARAMETRECONTALIBRE)[i])) {vardefaut1<-names(PARAMETRECONTALIBRE)[i]} else {vardefaut1<-names(PARAMETRECONTALIBRE)[1]}
		nom_cb<-paste("cbGRCONTA",i,NAMESGRCONTA[i],sep="_")
		assign( nom_cb,tkwidget(frameform,"ComboBox",editable=FALSE,values=listnamesGRCONTA , textvariable=tclVar(vardefaut1)))
		tkgrid(tklabel2(frameform,text=paste("  ",NAMESGRCONTA[i]),font=fontlabrad ),get(nom_cb),  sticky="w")
		CBGRCONTA<-c(CBGRCONTA,nom_cb) # permet de lister dans le bon ordre le résultat des combobox
		listnamesGRCONTA <- names(PARAMETRECONTALIBRE)
	}
		
	tkgrid(tklabel2(frameform,text="    ", font = fontvide))
	tkgrid(tklabel2(frameform,text="    ", font = fontvide))

	### check box OUI/non pour enregistrer la table
	cb1 <- tkcheckbutton2(frameform)
	cbValue1 <- tclVar(0)
	tkconfigure(cb1,variable=cbValue1)
	#tkgrid( tklabel2(frameform,text="  Exporter les données avec cette correspondance ?    ") , cb1, sticky="w")
	tkgrid(tklabel2(frameform,text="    ", font = fontvide))
	tkgrid(tklabel2(frameform,text="    ", font = fontvide))
	tcl("update")

	###FRAME BOUTONS
	## affichage des bouton
	framebut <- tkframe(tt)
	tkconfigure(framebut, bg=colorbg)

	###liste les combobox
	#CBGRCONTA<-sort(ls()[regexpr('cbGRCONTA',ls())>0])

	OnOK <- function()
	{ 
		tkconfigure(tt,cursor="watch") ##sablier
		## renames PARAMETRECONTALIBRE
		CBGRCONTAVALUE<-as.character()
		for (cb in CBGRCONTA){
			CBGRCONTAVALUE <- c(CBGRCONTAVALUE, listnamesGRCONTA[as.numeric(tclvalue(tcl(get(cb),"getvalue")))+1] )
		}
		CBGRCONTAVALUE2<<-CBGRCONTAVALUE

		### Pour Enregistrement de la table
		if (as.character((tclvalue(cbValue1))) == "1") { enregist_GRCONTA <<- "oui" } else { enregist_GRCONTA <<- "non"  }
			
		#### On réarrange et renommes les colonnes de PARAMETRECONTALIBRE et DATAPCH
		PARAMETRECONTALIBRE<-PARAMETRECONTALIBRE[,CBGRCONTAVALUE2]
		names(PARAMETRECONTALIBRE)<-NAMESGRCONTA
				
		PARAMETRECONTALIBRE<<-PARAMETRECONTALIBRE
		
		######################
		###MISE EN CONFORMITE
		######################

		### RESULTAT DEVIENT NUMERIC SI C'EST CODé EVARC UNE VIRGULE AU LIEU D'UN POINT
		####Export de la table 
		if (enregist_GRCONTA == "oui"){
			csvName <- tclvalue(tkgetSaveFile(initialfile = "PARAMETRECONTALIBRE.csv",
			filetypes = "{{CSV Files} {.csv .txt}} {{All files} *}" ,  title = "Enregistrement de PARAMETRECONTALIBRE sous : "))
			if (!nchar(csvName)) {
				tkmessageBox(message = "Le fichier ne sera pas enregistré")
			} else {
				write.csv2(PARAMETRECONTALIBRE,file=csvName, row.names = FALSE)
				rm(enregist_GRCONTA)
			}
		}	
		gc()		
		tkdestroy(tt)
		source(paste(pathS,"GUI_tab5.r",sep="") , echo = TRUE, print.eval = TRUE)	
	}
	## Affiche le bouton suivant
	fontbouton <- tkfont.create(family="calibri",size=15,weight="bold")
	OKbut <- tkbutton2(framebut,text="  >  ",command=OnOK, font  = fontbouton ) 

	## Fonction pour bouton PRECEDENT
	Retour<-function()  ## enregistress les options dans des objets et va lire les CSV
	{
		tkdestroy(tt)
		source(paste(pathS,"GUI_tab4a.r",sep="") , echo = TRUE, print.eval = TRUE)

	}
	returnbut <- tkbutton2(framebut,text="  <  ",command=Retour, font  = fontbouton ) 

	##bouton Aide
	fct_Aide <-function() { browseURL(paste(AidePath,"Aide4d.htm",sep=""))}
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
} else {
	source(paste(pathS,"GUI_tab5.r",sep="") , echo = TRUE, print.eval = TRUE) 
}