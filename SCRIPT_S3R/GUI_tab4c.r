###########################
# INTERFACE ETAPE 4 ######
##########################
##ici on cherche à réorganiser le tableau de données Bio
####### voir 4a qui est similaire


if ( SEEEPEGASE == "oui" ) {

	## Creation de la fenetre
	DEBUTGUI(Titre = "S3R (4c/5)")
		
		
	###framae formulaire
	frameform <- tkframe(tt)
	tkconfigure(frameform, bg=colorbg)
	tkgrid(frameform)
	tkgrid(tklabel2(frameform,text="    ", font = fontvide))
	tkgrid(tklabel2(frameform,text="    ", font = fontvide))

	### Choix des colonnes pour PEG
	tkgrid(tklabel3(frameform,text="DONNEES MODELISEES : ASSOCIER LES COLONNES", font = fontsstitre) ,  sticky="w")
	NAMESPEG<-c("STATION", "PEGO2", "PEGSATO2", "PEGDBO5", "PEGCOD", "PEGPO43", "PEGPHOS", "PEGNH4", "PEGNO2", "PEGNO3", "PEGTEMPE", "PEGPHMIN", "PEGPHMAX")
	NAMESPEG_vide_autorise<<-c("PEGTEMPE","PEGPHMIN","PEGPHMAX" )
	tclRequire("BWidget")

	# boucle permettant d'afficher les combobox
	NAMESDATAPEG<-names(DATAPEGASE)
	DATAPEGASE$non_dispo<-as.character("")
	CBPEG<-as.character()
	for (i in 1:length(NAMESPEG)){
	if (NAMESPEG[i] %in% NAMESPEG_vide_autorise) {listnamesPEG <- names(DATAPEGASE)} else{ listnamesPEG <-NAMESDATAPEG} 
	if (!is.na(names(DATAPEGASE)[i])) {vardefaut1<-names(DATAPEGASE)[i]} else {vardefaut1<-names(DATAPEGASE)[1]}
		nom_cb<-paste("cbPEG",i,NAMESPEG[i],sep="_")
		assign( nom_cb,tkwidget(frameform,"ComboBox",editable=FALSE,values=listnamesPEG , textvariable=tclVar(vardefaut1)))
		tkgrid(tklabel2(frameform,text=paste("  ",NAMESPEG[i]),font=fontlabrad ),get(nom_cb),  sticky="w")
		CBPEG<-c(CBPEG,nom_cb) # permet de lister dans le bon ordre le résultat des combobox
		listnamesPEG <- names(DATAPEGASE)
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

	###liste les combobox
	#CBPEG<-sort(ls()[regexpr('cbPEG',ls())>0])

	OnOK <- function()
	{ 
		tkconfigure(tt,cursor="watch") ##sablier
		## renames DATAPEGASE
		CBPEGVALUE<-as.character()
		for (cb in CBPEG){
			CBPEGVALUE <- c(CBPEGVALUE, listnamesPEG[as.numeric(tclvalue(tcl(get(cb),"getvalue")))+1] )
		}
		CBPEGVALUE2<<-CBPEGVALUE

		### Pour Enregistrement de la table
		if (as.character((tclvalue(cbValue1))) == "1") { enregist_PEG <<- "oui" } else { enregist_PEG <<- "non"  }
			
		#### On réarrange et renommes les colonnes de DATAPEGASE et DATAPCH
		DATAPEGASE<-DATAPEGASE[,CBPEGVALUE2]
		names(DATAPEGASE)<-NAMESPEG
				
		DATAPEGASE<<-DATAPEGASE
		
		######################
		###MISE EN CONFORMITE
		######################

		### RESULTAT DEVIENT NUMERIC SI C'EST CODé EVARC UNE VIRGULE AU LIEU D'UN POINT
		####Export de la table 
		if (enregist_PEG == "oui"){
			csvName <- tclvalue(tkgetSaveFile(initialfile = "DATAPEGASE.csv",
			filetypes = "{{CSV Files} {.csv .txt}} {{All files} *}" ,  title = "Enregistrement de DATAPEGASE sous : "))
			if (!nchar(csvName)) {
				tkmessageBox(message = "Le fichier ne sera pas enregistré")
			} else {
				write.csv2(DATAPEGASE,file=csvName, row.names = FALSE)
				rm(enregist_PEG)
			}
		}	
		gc()		
		tkdestroy(tt)
		source(paste(pathS,"GUI_tab4d.r",sep="") , echo = TRUE, print.eval = TRUE)	
	}
	## Affiche le bouton suivant
	fontbouton <- tkfont.create(family="calibri",size=15,weight="bold")
	OKbut <- tkbutton2(framebut,text="  >  ",command=OnOK, font  = fontbouton ) 

	## Fonction pour bouton PRECEDENT
	Retour<-function()  ## enregistress les options dans des objets et va lire les CSV
	{
		tkdestroy(tt)
		if (DATABIO_necesaire == "oui") {source(paste(pathS,"GUI_tab4b.r",sep="") , echo = TRUE, print.eval = TRUE)}
		if (DATABIO_necesaire != "oui") {source(paste(pathS,"GUI_tab4a.r",sep="") , echo = TRUE, print.eval = TRUE)}
	}
	returnbut <- tkbutton2(framebut,text="  <  ",command=Retour, font  = fontbouton ) 

	##bouton Aide
	fct_Aide <-function() { browseURL(paste(AidePath,"Aide4c.htm",sep=""))}
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
	source(paste(pathS,"GUI_tab4d.r",sep="") , echo = TRUE, print.eval = TRUE) 
}