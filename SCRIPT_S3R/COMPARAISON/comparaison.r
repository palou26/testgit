#############################################################################################
#############################################################################################
###script comparaison
#############################################################################################
#############################################################################################



############ INTERFACE : CALCULS EN COURS
ttencours <- tktoplevel()
tcl("wm", "attributes", ttencours, topmost=TRUE) ## fenetre  en 1er plan
tkconfigure(ttencours,cursor="watch") # souris sablier
tkwm.iconify(ttencours)
tkwm.deiconify(ttencours)

###icone
icone<-paste(racine,"/TEMPLATE/IMAGE/icone.ico", sep="")
if (file.exists(icone)) {tk2ico.setFromFile(ttencours, icone)}
tcl("update")
####
tkwm.geometry(ttencours, "575x200")  ##taille de la fenêtre
tkwm.geometry(ttencours, "+250+250")  #position à l'écran
tkwm.resizable(ttencours, FALSE, FALSE) # Non - extensible
tkwm.title(ttencours,"Calcul en cours") # titre de la fenêtre
tkconfigure(ttencours, bg=colorbg) # fond couleur
tcl("update")

####TITRE
frametitre <- tkframe(ttencours)
tkconfigure(frametitre, bg=colorbg)
tkgrid(frametitre)
imagtitre <- tclVar()
fileimagetitre<-paste(racine,"/TEMPLATE/IMAGE/S3R.gif", sep="")
if (file.exists(fileimagetitre)) {tcl("image","create","photo",imagtitre,file=fileimagetitre)
	imgtitreAsLabel <- tklabel2(frametitre,image=imagtitre,bg="white")
	tkgrid(imgtitreAsLabel ,  sticky="w")
}

tcl("update")

###framae formulaire
frameform <- tkframe(ttencours)
tkconfigure(frameform, bg=colorbg)
tkgrid(frameform)
tkgrid(tklabel2(frameform,text="    ", font = fontvide))
tkgrid(tklabel2(frameform,text="    ", font = fontvide))
tkgrid(tklabel2(frameform,text=paste(" COMPARAISON EN COURS"), font=fontlabrad),sticky="w")
tcl("update")


#############################################################################################
#############################################################################################
#############################################################################################
## Comparaison de l'Etat Ecologique
if (exists("RLT_FINALSTATION_1") & exists("RLT_FINALSTATION_2")){


	 
	 NAMESFINAL<-unique(c(names(RLT_FINALSTATION_1),names(RLT_FINALSTATION_1)))
	 NAMESFINAL<-subset(NAMESFINAL,!(NAMESFINAL%in% c("STATION","STATIONTXT" ,"CONTEXTE_PISCICOLE" , "TYPEFR" , "EUCD"  , 
	 "REPRESENTATIVE","EXCEPT_FROID","EXCEPT_CHAUD" ,"EXCEPT_ACID" , "EXCEPT_MO" ,"EXCEPT_TOURB" , "EXCEPT_O2" ,"COMITER","ALTITUDE",
	 "ECHELLESTA","FONDGEO_ARSENIC", "FONDGEO_CHROME","FONDGEO_CUIVRE","FONDGEO_ZINC",
	 "DURETE","LIBELLE",   "LISTINDICBIO" ,"N_PCHASSOUP", "N_PCHDECLASS" , "N_PSDECLASS","NBINDICBIO","PCHDECLASS",
	"PCHDECLASSASSOUP","PSDECLASS","TYPESTAT"
	 )) )

	 #on renomme les colonnes et on merge
	names(RLT_FINALSTATION_1)[names(RLT_FINALSTATION_1) != "STATION"]<-paste0(names(RLT_FINALSTATION_1)[names(RLT_FINALSTATION_1) != "STATION"],"_1")
	names(RLT_FINALSTATION_2)[names(RLT_FINALSTATION_2) != "STATION"]<-paste0(names(RLT_FINALSTATION_2)[names(RLT_FINALSTATION_2) != "STATION"],"_2")
	COMPAECOLOSTAT<-merge(RLT_FINALSTATION_1,RLT_FINALSTATION_2, all = TRUE, by = "STATION")
	names(COMPAECOLOSTAT)<-gsub(".x","_1",names(COMPAECOLOSTAT))
	names(COMPAECOLOSTAT)<-gsub(".y","_2",names(COMPAECOLOSTAT))
	NAME<-names(COMPAECOLOSTAT)
	 #on réorganise les noms de colones

	 COLCOULORSECOLO<-as.character()
	 N<-as.character()
	 for( i in NAMESFINAL) {
		N<-c(N,paste0(i,"_1"))
		if (!(paste0(i,"_1") %in% NAME))  {COMPAECOLOSTAT[,paste0(i,"_1")]<-as.numeric(NA)  }
		N<-c(N,paste0(i,"_2"))
		if (!(paste0(i,"_2") %in% NAME))  {COMPAECOLOSTAT[,paste0(i,"_2")]<-as.numeric(NA)  }
		N<-c(N,paste0("d_",i))
		COLCOULORSECOLO<-c(COLCOULORSECOLO,paste0("d_",i))
		if (val_abs == 'oui') {
			COMPAECOLOSTAT[,paste0("d_",i)]<- abs( as.numeric(COMPAECOLOSTAT[,paste0(i,"_1")]) - as.numeric(COMPAECOLOSTAT[,paste0(i,"_2")]))
		} else {
			COMPAECOLOSTAT[,paste0("d_",i)]<-  as.numeric(COMPAECOLOSTAT[,paste0(i,"_1")]) - as.numeric(COMPAECOLOSTAT[,paste0(i,"_2")])
		}
		COMPAECOLOSTAT[ !is.na(COMPAECOLOSTAT[,paste0("d_",i)]) & (as.numeric(COMPAECOLOSTAT[,paste0(i,"_1")]) == 0 | as.numeric(COMPAECOLOSTAT[,paste0(i,"_2")]) == 0) ,paste0("d_",i)]<-0
		if (COMPAECOLOSTAT[,paste0(i,"_1")] %in% c("oui","non") |  COMPAECOLOSTAT[,paste0(i,"_2")] %in% c("oui","non") ) {
		COMPAECOLOSTAT[!is.na(COMPAECOLOSTAT[,paste0(i,"_1")] ) & !is.na(COMPAECOLOSTAT[,paste0(i,"_2")] )
		& COMPAECOLOSTAT[,paste0(i,"_1")] != COMPAECOLOSTAT[,paste0(i,"_2")],paste0("d_",i)]<-1	
			}
		}
  COMPAECOLOSTAT<-COMPAECOLOSTAT[,c("STATION",N)]
  
#STAT
	if (val_abs == 'oui') {
	COMPSTATECOLO<-data.frame(c(0,1,2,3,4))  } else {COMPSTATECOLO<-data.frame(c(-4,-3,-2,-1,0,1,2,3,4))  } 
	names(COMPSTATECOLO)<-"ECART"
	for( j in NAMESFINAL) {
	for (h in COMPSTATECOLO$ECART){
	TEMP<-COMPAECOLOSTAT[COMPAECOLOSTAT[,paste0("d_",j)]	 == h & !is.na(COMPAECOLOSTAT[,paste0("d_",j)]),]
	COMPSTATECOLO[COMPSTATECOLO$ECART == h ,j]<-nrow(TEMP)
	print(nrow(TEMP))
	}}
##Fin de comparaison de l'état écologique  
}

## Comparaison de l'Etat Chimique
if (exists("RLT_CHIMSTATION_1") & exists("RLT_CHIMSTATION_2")){


	NAMESFINAL<-unique(c(names(RLT_CHIMSTATION_1),names(RLT_CHIMSTATION_2)))
	NAMESFINAL<-subset(NAMESFINAL,!(NAMESFINAL%in% c("STATION","STATIONTXT" ,"EUCD"  , "REPRESENTATIVE" )) )
	 
	#on renomme les colonnes et on merge
	names(RLT_CHIMSTATION_1)[names(RLT_CHIMSTATION_1) != "STATION"]<-paste0(names(RLT_CHIMSTATION_1)[names(RLT_CHIMSTATION_1) != "STATION"],"_1")
	names(RLT_CHIMSTATION_2)[names(RLT_CHIMSTATION_2) != "STATION"]<-paste0(names(RLT_CHIMSTATION_2)[names(RLT_CHIMSTATION_2) != "STATION"],"_2")
	COMPACHIMSTAT<-merge(RLT_CHIMSTATION_1,RLT_CHIMSTATION_2, all = TRUE, by = "STATION")
	names(COMPACHIMSTAT)<-gsub(".x","_1",names(COMPACHIMSTAT))
	names(COMPACHIMSTAT)<-gsub(".y","_2",names(COMPACHIMSTAT))
	NAME<-names(COMPACHIMSTAT)
	 #on réorganise les noms de colones

	 COLCOULORSCHIM<-as.character()
	 N<-as.character()
	 for( i in NAMESFINAL) {
		N<-c(N,paste0(i,"_1"))
		if (!(paste0(i,"_1") %in% NAME))  {COMPACHIMSTAT[,paste0(i,"_1")]<-as.numeric(NA)  }
		N<-c(N,paste0(i,"_2"))
		if (!(paste0(i,"_2") %in% NAME))  {COMPACHIMSTAT[,paste0(i,"_2")]<-as.numeric(NA)  }
		N<-c(N,paste0("d_",i))
		COLCOULORSCHIM<-c(COLCOULORSCHIM,paste0("d_",i))
		if (val_abs == 'oui') {
			COMPACHIMSTAT[,paste0("d_",i)]<- abs( as.numeric(COMPACHIMSTAT[,paste0(i,"_1")]) - as.numeric(COMPACHIMSTAT[,paste0(i,"_2")]))
		} else {
			COMPACHIMSTAT[,paste0("d_",i)]<- as.numeric(COMPACHIMSTAT[,paste0(i,"_1")]) - as.numeric(COMPACHIMSTAT[,paste0(i,"_2")])
		}
		if (COMPACHIMSTAT[,paste0(i,"_1")] %in% c("oui","non") |  COMPACHIMSTAT[,paste0(i,"_2")] %in% c("oui","non") ) {
		COMPACHIMSTAT[!is.na(COMPACHIMSTAT[,paste0(i,"_1")] ) & !is.na(COMPACHIMSTAT[,paste0(i,"_2")] )
		& COMPACHIMSTAT[,paste0(i,"_1")] != COMPACHIMSTAT[,paste0(i,"_2")],paste0("d_",i)]<-1	
			}
		}
  COMPACHIMSTAT<-COMPACHIMSTAT[,c("STATION",N)]
 
#STAT
	if (val_abs == 'oui') {
	COMPSTATCHIM<-data.frame(c(0,1,2))  } else {COMPSTATCHIM<-data.frame(c(-2,-1,0,1,2))  } 
	names(COMPSTATCHIM)<-"ECART"
	for( j in NAMESFINAL) {
	for (h in COMPSTATCHIM$ECART){
	TEMP<-COMPACHIMSTAT[COMPACHIMSTAT[,paste0("d_",j)]	 == h & !is.na(COMPACHIMSTAT[,paste0("d_",j)]),]
	COMPSTATCHIM[COMPSTATCHIM$ECART == h ,j]<-nrow(TEMP)
	print(nrow(TEMP))
	}}
##Fin de comparaison de l'état écologique 
 
}

####################################
####EXPORT EXCEL et Mise en couleur
#####################################
## Le fichier template n'est pas le même selon si on fait la différence en valeur absolue
if (val_abs == 'oui') {
ROWSTART = 16
ROWCOM = 12
NOMXLSTEMP <- paste(racine,"/TEMPLATE/XLS_template/templateComparaisonabs.xls",sep="")
} else {
ROWSTART = 20
ROWCOM = 16
NOMXLSTEMP <- paste(racine,"/TEMPLATE/XLS_template/templateComparaison.xls",sep="")
}


if (exists("RLT_FINALSTATION_1") & exists("RLT_FINALSTATION_2") | exists("RLT_CHIMSTATION_1") & exists("RLT_CHIMSTATION_2")  ){
library(XLConnect)
FICHIERDATA<-paste( format(Sys.time(), "%Y%m%d%H%M"),sep="")

	## EXPORT EXCEL : copy le template vers un nouveau fichier Excel de réusltat

	source(paste(pathsitecompa,"misecouleur.r",sep=""))
	XLS<-paste(racine,"/RESULTATS/Compa_",FICHIERDATA,".xls",sep="")
	file.copy(NOMXLSTEMP, XLS, overwrite = TRUE)


	# Export des options dans la l'onglet Accueil
	XLSload <- loadWorkbook(XLS, create = FALSE)
	print(FICHIERDATA)
	setStyleAction(XLSload, XLC$"STYLE_ACTION.NONE")
	writeWorksheet(XLSload, data.frame(COMMENTAIRE), sheet = "Accueil", startRow = 12, startCol = 3 , header = FALSE)

	
	if( exists("DFmetaECOLO_1") & exists("DFmetaECOLO_2") ){
		writeWorksheet(XLSload, LIST_OPTIONS_TEMPLATE_ecolo_1, sheet = "Accueil", startRow = ROWSTART, startCol = 1 , header = FALSE)
		writeWorksheet(XLSload, DFmetaECOLO_1, sheet = "Accueil", startRow = ROWSTART, startCol = 3 , header = FALSE)
		writeWorksheet(XLSload, DFmetaECOLO_2, sheet = "Accueil", startRow = ROWSTART, startCol = 4 , header = FALSE)
		createSheet(XLSload, name = "Statecolo")
		writeWorksheet(XLSload, COMPSTATECOLO, sheet = "Statecolo", startRow = 1, startCol = 1 , header = TRUE)
	} else if ( exists("DFmetaCHIM_1") & exists("DFmetaCHIM_2") ){
		writeWorksheet(XLSload, LIST_OPTIONS_TEMPLATE_chim_1, sheet = "Accueil", startRow = ROWSTART, startCol = 1 , header = FALSE)
		writeWorksheet(XLSload, DFmetaCHIM_1, sheet = "Accueil", startRow = ROWSTART, startCol = 3 , header = FALSE)
		writeWorksheet(XLSload, DFmetaCHIM_2, sheet = "Accueil", startRow = ROWSTART, startCol = 4 , header = FALSE)
		createSheet(XLSload, name = "StatChim")
		writeWorksheet(XLSload, COMPSTATCHIM, sheet = "StatChim", startRow = 1, startCol = 1 , header = TRUE)
	}	
	

	saveWorkbook(XLSload)
} else {
	tkmessageBox(message = "Impossible de faire des comparaisons - Rien à comparer" , icon = "warning")
}

	
	##Export de l'onglet du dataframe de comparaison pour Etat Ecolo
if (exists("RLT_FINALSTATION_1") & exists("RLT_FINALSTATION_2")){
	NUMCOL<-(1:ncol(COMPAECOLOSTAT))[names(COMPAECOLOSTAT) %in% COLCOULORSECOLO]
	colorXLS(XLS, createxls = FALSE, nameonglet = "Ecolo", createsheet = TRUE, COMPAECOLOSTAT, numcol = NUMCOL, depR =1 , depC=1, entete = TRUE, couleur=0)
}
	##Export de l'onglet du dataframe de comparaison pour Etat Chim
if (exists("RLT_CHIMSTATION_1") & exists("RLT_CHIMSTATION_2")){
	NUMCOL<-(1:ncol(COMPACHIMSTAT))[names(COMPACHIMSTAT) %in% COLCOULORSCHIM]
	colorXLS(XLS, createxls = FALSE, nameonglet = "Chim", createsheet = TRUE, COMPACHIMSTAT, numcol = NUMCOL, depR =1 , depC=1, entete = TRUE, couleur=0)
}

#### MESSAGE BOX FINAL
tkdestroy(ttencours)
if (file.exists(XLS)){
tkmessageBox(message = paste("Les résultats de la comparaison ont été exportés dans:\n",XLS) , icon = "info")
browseURL(XLS)
} else {
tkmessageBox(message = "Impossible de faire des comparaisons - Rien à comparer" , icon = "info")
}



