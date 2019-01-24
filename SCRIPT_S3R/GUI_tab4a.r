###########################
# INTERFACE ETAPE 4 ######
##########################

##ici on cherche à réorganiser le tableau de données Physico-chiqmiQ
## la même interface est dispo pour la BIO (4b)

#######
gc() #petite défragmentation

if (SEEEPCH == "oui"  | SEEEPS == "oui" | SEEECHIM == "oui" | CONTA == "oui" ) {
	
	## Creation de la fenetre
	DATAPCH_necesaire<-"oui"
	DEBUTGUI(Titre = "S3R (4a/5)")

	###frame formulaire
	frameform <- tkframe(tt)
	tkconfigure(frameform, bg=colorbg)
	tkgrid(frameform)
	tkgrid(tklabel2(frameform,text="    ", font = fontvide))
	tkgrid(tklabel2(frameform,text="    ", font = fontvide))

###############################
### Choix des colonnes pour PCH
###############################

	tkgrid(tklabel3(frameform,text="PCH / CHIMIE : ASSOCIER LES COLONNES", font = fontsstitre),    sticky="w")

	NAMESPCH<<-c("STATION", "DATEPRELEV", "HEUREPRELEV", "PARAMETRE", "FRACTION", "RESULTAT", "UNITE", "REMARQUE","LQ")
	if(CEPE == "PE") {NAMESPCH<<-c(NAMESPCH,"INTEGRE")}
	if (UNITEOKPCH=="oui" |UNITEOKPS=="oui" | UNITEOKCHIM=="oui"  |  UNITEOKCONTA=="oui" ){
		NAMESPCH_vide_autorise<<-c("HEUREPRELEV","LQ","INTEGRE")
		unitenondispo<-"non"
	} else {
		NAMESPCH_vide_autorise<<-c("HEUREPRELEV","LQ","UNITE","INTEGRE")
		unitenondispo<-"oui"
		
	}
	tclRequire("BWidget")

	# boucle permettant d'afficher les combobox
	NAMESDATAPCH<-names(DATAPCH)
	DATAPCH$non_dispo<-as.character("")
	CBPCH<-as.character()
	for (i in 1:length(NAMESPCH)){
		if (NAMESPCH[i] %in% NAMESPCH_vide_autorise) {listnamespch <- names(DATAPCH)} else{ listnamespch <-NAMESDATAPCH} #on liste les names de DATAPCH
		
		if (!is.na(names(DATAPCH)[i])) {vardefaut1<-names(DATAPCH)[i]} else {vardefaut1<-names(DATAPCH)[1]}		##Pour chaQ colonne de DATAPCH
		if (unitenondispo == "oui" & NAMESPCH[i] == "UNITE"   ) {vardefaut1<-"non_dispo"}		##Pour chaQ colonne de DATAPCH
		nom_cb<-paste("cbpch",i,NAMESPCH[i],sep="_")  ## nom du combobox
		assign( nom_cb,tkwidget(frameform,"ComboBox",editable=FALSE,values=listnamespch , textvariable=tclVar(vardefaut1))) ##création de la combobox
		tkgrid(tklabel2(frameform,text=paste("  ",NAMESPCH[i]),font=fontlabrad ),get(nom_cb),  sticky="w")
		CBPCH<-c(CBPCH,nom_cb)
		listnamespch <- names(DATAPCH)
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

	###FRAME BOUTONS
	## affichage des bouton
	framebut <- tkframe(tt)
	tkconfigure(framebut, bg=colorbg)

	#### Fonction permettant de réorganiser les colonnes et leur donner lebon nom.
	### Puis de rechercher les doublons
	### et en cas de doublons, possibilité de voir la table avec les doublons

	tcl("update")
	OnOK <- function()
	{ 
		tkconfigure(tt,cursor="watch") ##sablier
		## on récuppère dans CBPCHVALUE les valeurs des combobox, c'est à dire les colonnes à remplacer par le bon nom
		CBPCHVALUE<-as.character()
		for ( cb in CBPCH){
			CBPCHVALUE <- c(CBPCHVALUE, listnamespch[as.numeric(tclvalue(tcl(get(cb),"getvalue")))+1] )
		}
		
		CBPCHVALUE2<<-CBPCHVALUE
		### Pour Enregistrement de la table
		if (as.character((tclvalue(cbValue1))) == "1") { enregist_pch <<- "oui" } else { enregist_pch <<- "non"  }

		#### On réarrange et renommes les colonnes de DATABIO et DATAPCH
		DATAPCH<-DATAPCH[,CBPCHVALUE2]
		names(DATAPCH)<-NAMESPCH
		print(NAMESPCH)	
		


		### RESULTAT DEVIENT NUMERIC SI C'EST CODé AVEC UNE VIRGULE AU LIEU D'UN POINT
		DATAPCH$RESULTAT<-gsub(",",".",as.character(DATAPCH$RESULTAT))

		####Export de la table si on choisit de l'enregistrer
		if (enregist_pch == "oui"){
			csvName <- tclvalue(tkgetSaveFile(initialfile = "DATAPCH.csv",
			filetypes = "{{CSV Files} {.csv .txt}} {{All files} *}" ,  title = "Enregistrement de DATAPCH sous : "))
			if (!nchar(csvName)) {
				tkmessageBox(message = "Le fichier ne sera pas enregistré")
			} else {
				write.csv2(DATAPCH,file=csvName, row.names = FALSE)
				rm(enregist_pch)
			}
		}
						
		print(head(DATAPCH))

		DATAPCH<<-DATAPCH
		# ouverture de la fenetre suivante
		gc()
		tkdestroy(tt)
		source(paste(pathS,"GUI_tab4b.r",sep="") , echo = TRUE, print.eval = TRUE)
	}

	## Affiche le bouton suivant
	fontbouton <- tkfont.create(family="calibri",size=15,weight="bold")
	OKbut <- tkbutton2(framebut,text="  >  ",command=OnOK, font  = fontbouton ) 

	## Fonction pour bouton PRECEDENT
	Retour<-function()  ## enregistress les options dans des objets et va lire les CSV
	{
		tkdestroy(tt)
		source(paste(pathS,"GUI_tab3b.r",sep="") , echo = TRUE, print.eval = TRUE)
	}
	returnbut <- tkbutton2(framebut,text="  <  ",command=Retour, font  = fontbouton ) 

	##bouton Aide
	fct_Aide <-function() { browseURL(paste(AidePath,"Aide4a.htm",sep=""))}
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
	DATAPCH_necesaire<-"non"
	source(paste(pathS,"GUI_tab4b.r",sep="") , echo = TRUE, print.eval = TRUE) 
}
