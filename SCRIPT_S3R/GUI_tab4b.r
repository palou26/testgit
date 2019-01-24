###########################
# INTERFACE ETAPE 4 ######
##########################

##ici on cherche à réorganiser le tableau de données Bio
####### voir 4a qui est similaire
gc()
if (SEEEBIO=="oui") {

	## Creation de la fenetre
	DATABIO_necesaire<-"oui"
	DEBUTGUI(Titre = "S3R (4b/5)")


	###framae formulaire
	frameform <- tkframe(tt)
	tkconfigure(frameform, bg=colorbg)
	tkgrid(frameform)
	tkgrid(tklabel2(frameform,text="    ", font = fontvide))
	tkgrid(tklabel2(frameform,text="    ", font = fontvide))

	### Choix des colonnes pour BIO
	tkgrid(tklabel3(frameform,text="BIOLOGIE : ASSOCIER LES COLONNES", font = fontsstitre) ,  sticky="w")

	if(CEPE=="PE" & MODULE == "REEE2010") {
		NAMESBIO<-c("STATION", "DATEPRELEV", "HEUREPRELEV", "PARAMETRE",  "RESULTAT","INTEGRE")} else {
	if (CEPE=="CE" & MODULE == "REEE2018") {	
		NAMESBIO<-c("STATION", "DATEPRELEV", "HEUREPRELEV", "PARAMETRE",  "RESULTAT","INDICEEQR")		
		} else {
		NAMESBIO<-c("STATION", "DATEPRELEV", "HEUREPRELEV", "PARAMETRE",  "RESULTAT")		
		}}
	NAMESBIO_vide_autorise<<-c("HEUREPRELEV","INTEGRE","INDICEEQR")
	tclRequire("BWidget")

	# boucle permettant d'afficher les combobox
	NAMESDATABIO<-names(DATABIO)
	DATABIO$non_dispo<-as.character("")
	CBBIO<-as.character()
	for (i in 1:length(NAMESBIO)){
		if (NAMESBIO[i] %in% NAMESBIO_vide_autorise) {listnamesbio <- names(DATABIO)} else{ listnamesbio <-NAMESDATABIO} 
		if (!is.na(names(DATABIO)[i])) {vardefaut1<-names(DATABIO)[i]} else {vardefaut1<-names(DATABIO)[1]}
		nom_cb<-paste("cbbio",i,NAMESBIO[i],sep="_")
		assign( nom_cb,tkwidget(frameform,"ComboBox",editable=FALSE,values=listnamesbio , textvariable=tclVar(vardefaut1)))
		tkgrid(tklabel2(frameform,text=paste("  ",NAMESBIO[i]),font=fontlabrad ),get(nom_cb),  sticky="w")
		CBBIO<-c(CBBIO,nom_cb)
		listnamesbio <- names(DATABIO)
	}

	tkgrid(tklabel2(frameform,text="    ", font = fontvide))
	tkgrid(tklabel2(frameform,text="    ", font = fontvide))

	### check box OUI/non pour enregistrer la table
	cb1 <- tkcheckbutton2(frameform)
	cbValue1 <- tclVar(0)
	tkconfigure(cb1,variable=cbValue1)
	tkgrid( tklabel2(frameform,text="  Exporter les données avec cette correspondance ?    ") , cb1, sticky="w")
	tkgrid(tklabel2(frameform,text="    ", font = fontvide))
	tkgrid(tklabel2(frameform,text="    ", font = fontvide))
	tcl("update")

	###FRAME BOUTONS
	## affichage des bouton
	framebut <- tkframe(tt)
	tkconfigure(framebut, bg=colorbg)


	OnOK <- function()
	{ 
		print("0")
		tkconfigure(tt,cursor="watch") ##sablier
		## renames DATABIO
		CBBIOVALUE<-as.character()
		for ( cb in CBBIO){
			CBBIOVALUE <- c(CBBIOVALUE, listnamesbio[as.numeric(tclvalue(tcl(get(cb),"getvalue")))+1] )
		}
		CBBIOVALUE2<<-CBBIOVALUE


		### Pour Enregistrement de la table
		 if (as.character((tclvalue(cbValue1))) == "1") { enregist_bio <<- "oui" } else { enregist_bio <<- "non"  }
		
		#### On réarrange et renommes les colonnes de DATABIO 
		DATABIO<-DATABIO[,CBBIOVALUE2]
		names(DATABIO)<-NAMESBIO
		DATABIO<<-DATABIO
		

		######################
		###MISE EN CONFORMITE
		######################
		### RESULTAT DEVIENT NUMERIQUE SI C'EST CODé AVEC UNE VIRGULE AU LIEU D'UN POINT
		DATABIO$RESULTAT<-gsub(",",".",as.character(DATABIO$RESULTAT))
		print("3")
		####Export de la table 
		if (enregist_bio == "oui"){
			csvName <- tclvalue(tkgetSaveFile(initialfile = "DATABIO.csv",
			filetypes = "{{CSV Files} {.csv .txt}} {{All files} *}" ,  title = "Enregistrement de DATABIO sous : "))

			if (!nchar(csvName)) {
				tkmessageBox(message = "Le fichier ne sera pas enregistré")
			} else {
				write.csv2(DATABIO,file=csvName, row.names = FALSE)
				rm(enregist_bio)
			}
		}
		DATABIO<<-DATABIO

		## ouverture de la fenetre suivante
		gc()
		tkdestroy(tt)
		source(paste(pathS,"GUI_tab4c.r",sep="") , echo = TRUE, print.eval = TRUE)		
	}
	
	## Affiche le bouton suivant
	fontbouton <- tkfont.create(family="calibri",size=15,weight="bold")
	OKbut <- tkbutton2(framebut,text="  >  ",command=OnOK, font  = fontbouton ) 

	## Fonction pour bouton PRECEDENT
	Retour<-function()  ## enregistress les options dans des objets et va lire les CSV
	{
		tkdestroy(tt)
		if (DATAPCH_necesaire == "oui") {source(paste(pathS,"GUI_tab4a.r",sep="") , echo = TRUE, print.eval = TRUE)}
		if (DATAPCH_necesaire != "oui") {source(paste(pathS,"GUI_tab3b.r",sep="") , echo = TRUE, print.eval = TRUE)}
	}
	returnbut <- tkbutton2(framebut,text="  <  ",command=Retour, font  = fontbouton ) 

	##bouton Aide
	fct_Aide <-function() { browseURL(paste(AidePath,"Aide4b.htm",sep=""))}
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
	DATABIO_necesaire<-"non"
	source(paste(pathS,"GUI_tab4c.r",sep="") , echo = TRUE, print.eval = TRUE) 
}