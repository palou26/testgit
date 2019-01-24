##############################################
## SEEE - COURS D'EAU : éxécution de l'ensemble des scripts R
## Application de l'arrêté de janvier 2010
##############################################
SEEE_DEB<-Sys.time()

##licence
tkgrid(tklabel2(tt,text=paste("    -",infodate)), sticky="w")
tcl("update")

### Création d'un guide pour le bon report des extension de fichiers fonction du module
fileExtension <- data.frame(MODULE=c("REEE2010","REEE2016","REEE2021"),fileExtension=c("","_2c","_3c"))
fE.MOD <- as.character(fileExtension[which(as.character(fileExtension$MODULE)==MODULE),2])

#############
## MISE EN CONFORMITE
#############

labmisenconf<-tklabel2(tt,text="    - Mise en conformité : en cours")
tkgrid(labmisenconf, sticky="w")

### Execute le script de mise en conformité des données (DATAPCH, DATABIO, ....)
source(paste(pathS,"MISE_EN_CONFORMITE.r",sep="") , echo = TRUE, print.eval = TRUE)
tkconfigure(labmisenconf,textvariable=tclVar("    - Mise en conformité : terminée"), foreground=colorduS)
tcl("update")

#### On enregistre un RDATA avant de commencer les algo de calculs
## Export en Archive Rdata
gc()


#############
## ETAT PCH
#############
if (SEEEPCH=="oui") {
	labpch<-tklabel2(tt,text="    - Etat physico-chimique : en cours")
	tkgrid(labpch, sticky="w")
	tcl("update")
	
	if (CEPE == "CE") { source(paste(pathsite,"EPCH.r",sep=""), echo = TRUE, print.eval = TRUE)}
	if (CEPE == "PE") { source(paste(pathsite,"EPCHPE.r",sep=""), echo = TRUE, print.eval = TRUE)}
	tkconfigure(labpch,textvariable=tclVar("    - Etat Physico-chimique : terminé"), foreground=colorduS)
	tcl("update")
	
	# Identifier la période
	if (min(DATAPCH$ANNEE) != max(DATAPCH$ANNEE)) {
	periodePCH<-paste(min(DATAPCH$ANNEE,na.rm = TRUE),"-",max(DATAPCH$ANNEE,na.rm = TRUE))  } else {
	periodePCH<-paste(min(DATAPCH$ANNEE,na.rm = TRUE))
	}
}
##############################
## ETAT POLLUANTS SPECIFIQUES
##############################
if (SEEEPS=="oui") {
	labpolspe<-tklabel2(tt,text="    - Polluants spécifiques : en cours")
	tkgrid(labpolspe, sticky="w")
	tcl("update")

	
	 source(paste(pathsite,"EPOLSPE.r",sep="") , echo = TRUE, print.eval = TRUE)
	tkconfigure(labpolspe,textvariable=tclVar("    - Etat Polluants spécifiques : terminé"), foreground=colorduS)
	tcl("update")
	
	# Identifier la période
	if (min(DATAPOLSPE$ANNEE) != max(DATAPOLSPE$ANNEE)) {
	periodePS<-paste(min(DATAPOLSPE$ANNEE,na.rm = TRUE),"-",max(DATAPOLSPE$ANNEE,na.rm = TRUE))  } else {
	periodePS<-paste(min(DATAPOLSPE$ANNEE,na.rm = TRUE))
	}
}

####################
## ETAT BIOLOGIQUE
####################
if (SEEEBIO=="oui") {
	labbio<-tklabel2(tt,text="    - Etat biologique : en cours")
	tkgrid(labbio, sticky="w")
	tcl("update")
	if (CEPE == "CE") { source(paste(pathsite,"EBIO.r",sep=""), echo = TRUE, print.eval = TRUE)}
	if (CEPE == "PE") { source(paste(pathsite,"EBIOPE.r",sep=""), echo = TRUE, print.eval = TRUE)}
	
	tkconfigure(labbio,textvariable=tclVar("    - Etat Biologique : terminé"), foreground=colorduS)
	tcl("update")
	
	# Identifier la période
	if (min(DATABIO$ANNEE) != max(DATABIO$ANNEE)) {
	periodeBio<-paste(min(DATABIO$ANNEE,na.rm = TRUE),"-",max(DATABIO$ANNEE,na.rm = TRUE))  } else {
	periodeBio<-paste(min(DATABIO$ANNEE,na.rm = TRUE))
	}
}


####################
## ETAT ECOLOGIQUE
####################
if (SEEEECOLO=="oui") {
	labecolo<-tklabel2(tt,text="    - Etat écologique : en cours")
	tkgrid(labecolo, sticky="w")
    tcl("update")
	source(paste(pathsite,"EECOLO.r",sep="") , echo = TRUE, print.eval = TRUE)
	tkconfigure(labecolo,textvariable=tclVar("    - Etat Écologique : terminé"), foreground=colorduS)
	tcl("update")
}

####################
## ETAT CHIMIQUE
####################

if (SEEECHIM=="oui") {
	labchim<-tklabel2(tt,text="    - Etat chimique : en cours")
	tkgrid(labchim, sticky="w")
	tcl("update")
	

	# Identifier la période
	if (min(DATACHIM$ANNEE) != max(DATACHIM$ANNEE)) {
	periodeChim<-paste(min(DATACHIM$ANNEE,na.rm = TRUE),"-",max(DATACHIM$ANNEE,na.rm = TRUE))  } else {
	periodeChim<-paste(min(DATACHIM$ANNEE,na.rm = TRUE))
	}
	
	#faire des groupe de 500 000 lignes max
	nDATACHIM<-nrow(DATACHIM)
	nstatDATACHIM<-length(unique(DATACHIM$STATION))
	nbgroupeDATACHIM<-ceiling(nDATACHIM/500000)+1
	if(length(unique(DATACHIM$STATION)) == 1 |  nrow(DATACHIM) < 50000) { # condition pour faire un seul groupe!
		nbgroupeDATACHIM<-1
		STATIONGROUPCHIM<-data.frame(STATION = unique(DATACHIM$STATION), GROUPESTATCHIM = 1)
		} else {
		STATIONGROUPCHIM<-data.frame(STATION = unique(DATACHIM$STATION), GROUPESTATCHIM = as.numeric(cut(as.numeric(as.factor(unique(DATACHIM$STATION))),nbgroupeDATACHIM)))
		}
	#STATIONGROUPCHIM<-data.frame(STATION = unique(DATACHIM$STATION), GROUPESTATCHIM = as.numeric(cut(as.numeric(as.factor(unique(DATACHIM$STATION))),nbgroupeDATACHIM)))
	
	# On divise DATACHIM en autant de groupe
	for (g in 1:nbgroupeDATACHIM) {
		assign(paste0("DATACHIM",g), DATACHIM[DATACHIM$STATION %in% STATIONGROUPCHIM$STATION[STATIONGROUPCHIM$GROUPESTATCHIM == g],])
	}
	
	# On lance les calculs groupe de stations par groupe de stations
	for ( g in 1:nbgroupeDATACHIM) {
		DATACHIM<-get(paste0("DATACHIM",g))
		gc()
		print(paste("CHIM GROUPE",g,"=",nrow(DATACHIM),"lignes"))

		source(paste(pathsite,"ECHIM.r",sep="") , echo = TRUE, print.eval = TRUE)
		
		#On récupère les dataframe qui nous intéressent  l'issue du calcul de l'état chimique
		if (g == 1) {
		CHIMCMAMA_sauv <- CHIMCMAMA
		RLT_CHIMSTATION_sauv <- RLT_CHIMSTATION
		DATACHIMFREQ_sauv <- DATACHIMFREQ
		DATACHIMFREQQUANTI_sauv <- DATACHIMFREQQUANTI
		CHIMHORSHAP_sauv <- CHIMHORSHAP
		CHIMANNEE_sauv <- CHIMANNEE
		if (exists("CAS_CHIMMA") ){CAS_CHIMMA_sauv <- CAS_CHIMMA} #uniquement à priori pour REEE2016
		} else {
		CHIMCMAMA_sauv<-merge(CHIMCMAMA_sauv,CHIMCMAMA, all= TRUE)
		RLT_CHIMSTATION_sauv<-merge(RLT_CHIMSTATION_sauv,RLT_CHIMSTATION, all= TRUE)
		DATACHIMFREQ_sauv<-merge(DATACHIMFREQ_sauv,DATACHIMFREQ, all= TRUE)
		DATACHIMFREQQUANTI_sauv<-merge(DATACHIMFREQQUANTI_sauv,DATACHIMFREQQUANTI, all= TRUE)
		CHIMHORSHAP_sauv<-merge(CHIMHORSHAP_sauv,CHIMHORSHAP, all= TRUE)
		CHIMANNEE_sauv<-merge(CHIMANNEE_sauv,CHIMANNEE, all= TRUE)

		if (exists("CAS_CHIMMA") ){CAS_CHIMMA_sauv<-merge(CAS_CHIMMA_sauv,CAS_CHIMMA, all= TRUE)} #uniquement à priori pour REEE2016
		}
	
	gc()
	tcl("update")
	
	}
	
	CHIMCMAMA_sauv -> CHIMCMAMA
	RLT_CHIMSTATION_sauv -> RLT_CHIMSTATION
	DATACHIMFREQ_sauv ->DATACHIMFREQ
	DATACHIMFREQQUANTI_sauv -> DATACHIMFREQQUANTI
	CHIMHORSHAP <- CHIMHORSHAP_sauv
	CHIMANNEE <- CHIMANNEE_sauv
	
	if (exists("CAS_CHIMMA_sauv") ){CAS_CHIMMA_sauv -> CAS_CHIMMA} ##uniquement à priori pour REEE2016

	RLT_CHIMSTATION_NAMES<-names(RLT_CHIMSTATION)

	tcl("update")
	
	
####################
## ETAT CHIMIQUE à la ME
####################

	source(paste(pathsite,"ECHIM_ME.r",sep="") , echo = TRUE, print.eval = TRUE)
	tkconfigure(labchim,textvariable=tclVar("    - Etat chimique : terminé"), foreground=colorduS)

}

	

####################
## MODULE CONTAMINATION
####################

if (CONTA=="oui" ) {
	labchim<-tklabel2(tt,text="    - Module Contamination : en cours")
	tkgrid(labchim, sticky="w")
	tcl("update")
	
	#faire des groupe de 500 000 lignes max
	nDATACONTA<-nrow(DATACONTA)
	nstatDATACONTA<-length(unique(DATACONTA$STATION))
	nbgroupeDATACONTA<-ceiling(nDATACONTA/800000)+1
	if(nstatDATACONTA == 1 |  nrow(DATACONTA) < 30000) { # condition pour faire un seul groupe!
		nbgroupeDATACONTA<-1
		STATIONGROUPCHIM<-data.frame(STATION = unique(DATACONTA$STATION), GROUPESTATCHIM = 1)
		} else {
		STATIONGROUPCHIM<-data.frame(STATION = unique(DATACONTA$STATION), GROUPESTATCHIM = as.numeric(cut(as.numeric(as.factor(unique(DATACONTA$STATION))),nbgroupeDATACONTA)))
		}
	#STATIONGROUPCHIM<-data.frame(STATION = unique(DATACONTA$STATION), GROUPESTATCHIM = as.numeric(cut(as.numeric(as.factor(unique(DATACONTA$STATION))),nbgroupeDATACONTA)))
	
	# On divise DATACONTA en autant de groupe
	for (g in 1:nbgroupeDATACONTA) {
		assign(paste0("DATACONTA",g), DATACONTA[DATACONTA$STATION %in% STATIONGROUPCHIM$STATION[STATIONGROUPCHIM$GROUPESTATCHIM == g],])
	}
	
		
	for ( g in 1:nbgroupeDATACONTA) {
		DATACONTA<-get(paste0("DATACONTA",g))
		gc()
		print(paste("CONTA GROUPE",g,"=",nrow(DATACONTA),"lignes"))

		
			if (TYPECONTA %in% c(1,2,3) ) { 
				source(paste(pathsite,"CONTA.r",sep="") , echo = TRUE, print.eval = TRUE)
				
				if (g == 1) {		
					RLT_CONTA_sauv <- RLT_CONTA
					CONTAAGG_sauv <- CONTAAGG
					RECAPCONTA_sauv <- RECAPCONTA
					RLT_CONTA_PARAMLIMITANT_sauv <- RLT_CONTA_PARAMLIMITANT
					RLT_LQMAX_sauv <- RLT_LQMAX
					RLT_CAS_sauv <- RLT_CAS
					CONTACASCLASS_sauv <- CONTACASCLASS
					gc()
					if (CONTAME == "oui"){
						CONTAAGGME_sauv <- CONTAAGGME
						RLT_CONTAAGGME_sauv <- RLT_CONTAAGGME
					}
				}else{
					RLT_CONTA_sauv <- merge(RLT_CONTA_sauv,RLT_CONTA, all= TRUE)
					CONTAAGG_sauv <- merge(CONTAAGG_sauv,CONTAAGG, all= TRUE)
					RECAPCONTA_sauv <- merge(RECAPCONTA_sauv,RECAPCONTA, all= TRUE)
					RLT_CONTA_PARAMLIMITANT_sauv <- merge(RLT_CONTA_PARAMLIMITANT_sauv,RLT_CONTA_PARAMLIMITANT, all= TRUE)
					RLT_LQMAX_sauv <- merge(RLT_LQMAX_sauv,RLT_LQMAX, all= TRUE)
					RLT_CAS_sauv <- merge(RLT_CAS_sauv,RLT_CAS, all= TRUE)
					CONTACASCLASS_sauv <- merge(CONTACASCLASS_sauv,CONTACASCLASS, all= TRUE)
					gc()
					if (CONTAME == "oui"){
						CONTAAGGME_sauv <- merge(CONTAAGGME_sauv,CONTAAGGME, all= TRUE)
						RLT_CONTAAGGME_sauv <- merge(RLT_CONTAAGGME_sauv,RLT_CONTAAGGME, all= TRUE)
					}				
					
					
				
				}
			}
			
			if (TYPECONTA %in% c(4) ) { 
				source(paste(pathsite,"SOMPEST.r",sep="") , echo = TRUE, print.eval = TRUE)
			
				if (g == 1) {
					RESULTAT_SUMPEST_sauv <- RESULTAT_SUMPEST
					RESULTAT_SUMPEST_PERIODE_sauv <- RESULTAT_SUMPEST_PERIODE
				}else{
					RESULTAT_SUMPEST_sauv<-merge(RESULTAT_SUMPEST_sauv,RESULTAT_SUMPEST, all= TRUE)
					RESULTAT_SUMPEST_PERIODE_sauv<-merge(RESULTAT_SUMPEST_PERIODE_sauv,RESULTAT_SUMPEST_PERIODE, all= TRUE)
					gc()
				}
			
			}
		
	gc()
	tcl("update")
	
	}
		
	
	if (TYPECONTA %in% c(1,2,3) ) {
		RLT_CONTA_sauv -> RLT_CONTA
		CONTAAGG_sauv -> CONTAAGG
		RECAPCONTA_sauv -> RECAPCONTA
		RLT_CONTA_PARAMLIMITANT_sauv -> RLT_CONTA_PARAMLIMITANT
		RLT_LQMAX_sauv -> RLT_LQMAX
		RLT_CAS_sauv -> RLT_CAS
		CONTACASCLASS_sauv -> CONTACASCLASS
		if(nrow(CONTACASCLASS)>0){
			CONTACASCLASS<-aggregate(effectif ~ ClasseQualité + Cas , data = CONTACASCLASS, sum, na.rm = TRUE)
			}
		if (CONTAME == "oui"){
			CONTAAGGME_sauv -> CONTAAGGME
			RLT_CONTAAGGME_sauv -> RLT_CONTAAGGME
	}
		
		
	}
	
	if (TYPECONTA %in% c(4) ) {
		RESULTAT_SUMPEST_sauv -> RESULTAT_SUMPEST
		RESULTAT_SUMPEST_PERIODE_sauv -> RESULTAT_SUMPEST_PERIODE
		INDICE<-"non"
		FREQPRELEV<-"non"
	}

	
		
	# Fin module Contamination
	tkconfigure(labchim,textvariable=tclVar("    - Module Contamination : terminé"), foreground=colorduS)
	tcl("update")
	
	# Identifier la période
	if (min(DATACONTA$ANNEE) != max(DATACONTA$ANNEE)) {
	periodeConta<-paste(min(DATACONTA$ANNEE,na.rm = TRUE),"-",max(DATACONTA$ANNEE,na.rm = TRUE))  } else {
	periodeConta<-paste(min(DATACONTA$ANNEE,na.rm = TRUE))
	}
	
	
	}

	

############################################################
## INDICE, RESULTATS FINAUX, FREQUENCE et STATISTIQUE
############################################################
if (INDICE=="oui" ){
	labindice<-tklabel2(tt,text="    - Calul des indices : en cours")
	tkgrid(labindice, sticky="w")
	tcl("update")

	if(CEPE == "CE" | CONTA == "oui") {source(paste(pathsite,"INDICE.r",sep="") , echo = TRUE, print.eval = TRUE)}
	if(CEPE == "PE" & CONTA == "non") {source(paste(pathsite,"INDICE_PE.r",sep="") , echo = TRUE, print.eval = TRUE)}
    	tkconfigure(labindice,textvariable=tclVar("    - Calul des indices : terminé"), foreground=colorduS)
	tcl("update")
}

if (FREQPRELEV=="oui" ){
	labfreq<-tklabel2(tt,text="    - Récapitulatif des fréquences de prélèvements : en cours")
	tkgrid(labfreq, sticky="w")
	tcl("update")
	source(paste(pathsite,"FREQUENCE.r",sep="") , echo = TRUE, print.eval = TRUE)
	 	tkconfigure(labfreq,textvariable=tclVar("    - Récapitulatif des fréquences de prélèvements : terminé"), foreground=colorduS)
	tcl("update")
}

if(SEEEPCH=="oui" | SEEEPS=="oui" | SEEEBIO=="oui"){
	labMF<-tklabel2(tt,text="    - Mise en forme des fichiers finaux : en cours")
	tkgrid(labMF, sticky="w")
	tcl("update")
	source(paste(pathS,"RLT_FINAUX.r",sep="") , echo = TRUE, print.eval = TRUE)
		tkconfigure(labMF,textvariable=tclVar("    - Mise en forme des fichiers finaux : terminé"), foreground=colorduS)
	tcl("update")
}

if (STATISTIQUE=="oui" & CONTA != "oui" ){
	labstat<-tklabel2(tt,text="    - Calculs des statistiques : en cours")
	tkgrid(labstat, sticky="w")
	tcl("update")
	source(paste(pathsite,"STATISTIQ.r",sep="") , echo = TRUE, print.eval = TRUE)
	tkconfigure(labstat,textvariable=tclVar("    - Calculs des statistiques : terminé"), foreground=colorduS)
	tcl("update")
}


if (STATISTIQUE=="oui" & CONTA != "oui"){
	labstat<-tklabel2(tt,text="    - Calculs des indicateurs de distribution : en cours")
	tkgrid(labstat, sticky="w")
	tcl("update")
	source(paste(pathS,"VARIABILITE.r",sep="") , echo = TRUE, print.eval = TRUE)
	 tcl("update")
	tkconfigure(labstat,textvariable=tclVar("    - Calculs des indicateurs de distribution : terminé"), foreground=colorduS)
	tcl("update")
}




###Gros nettoyage avant export
LISTRM<-as.character()
	for (i in ls()) {
		 if (class(get(i))[1] =="data.frame" & !(i %in% c("DATAPCH","DATACHIM","DATAPOLPSPE","POLSPE","CONTAAGG","CONTAINDICE","PARAMETRECONTA","PARAGROUPCHIM","SOMPEST","RESULTAT_SUMPEST","RESULTAT_SUMPEST_PERIODE"))    ) {
		 if(nrow(get(i)) > nrow(STATION)*10 & nrow(get(i)) > 10000 & substr(i,1,3)  !="RLT" &  substr(i,1,3)  !="VAR"  ) {
		  print(i)
		  print(nrow(get(i)))
		  LISTRM<-c(LISTRM,i)
			}

	}}
print("nettoyage")
print(LISTRM)	
rm(list = LISTRM)
gc()


##################
#### EXPORT EXCEL ######
##################

## on déclare les chemins (qui n'existe pas
XLSCHIM<-paste0(racine,"chim.xls")
XLSECOLO<-paste0(racine,"ecolo.xls")
XLSCONTA<-paste0(racine,"conta.xls")

	tklab_export<-tklabel2(tt,text="    - Export des résultats : en cours")
	tkgrid(tklab_export, sticky="w")
	tcl("update")

#nom du fichier
if(SEEEPCH=="oui" | SEEEPS=="oui" | SEEEBIO=="oui"){
	date()
	
	##SUFIXE du nom du fichier Excel
	SUFIXE<-"ECO"
	if (SEEEPCH=="oui" ) {SUFIXE<-paste(SUFIXE,"PCH",sep="_")}
	if (SEEEPS=="oui" ) {SUFIXE<-paste(SUFIXE,"PS",sep="_")}
	if (SEEEBIO=="oui" ) {SUFIXE<-paste(SUFIXE,"BIO",sep="_")}
	
	## EXPORT EXCEL : copy le template vers un nouveau fichier Excel de réusltat

	source(paste(pathS,"misecouleur.r",sep=""))
	NOMXLSTEMP <- paste(racine,"/TEMPLATE/XLS_template/template.xls",sep="")
	XLS<-paste(CH_OUTPUT,NAMEOUTPUT,SUFIXE,".xls",sep="")
	if (file.exists(XLS)) {	XLS<-paste(CH_OUTPUT,NAMEOUTPUT,"ECOLO_",SEEE_DEBformat,".xls",sep="")}
	file.copy(NOMXLSTEMP, XLS, overwrite = TRUE)

	####Export des paramètres (métadonnées)
	if(!exists("SEEEPCH")) {SEEEPCH<-"non"}
	if(!exists("SEEEPS")) {SEEEPS<-"non"}
	if(!exists("SEEEBIO")) {SEEEBIO<-"non"}
	if(!exists("SEEEECOLO")) {SEEEECOLO<-"non"} 
	if(!exists("SEEECHIM")) {SEEECHIM<-"non"}
	if(!exists("PROFONDEUR")) {PROFONDEUR<-" - "}
	if(!exists("MISECOULEUR")) {MISECOULEUR<-"non"}
	if(!exists("NAMEFILEPCH")) {NAMEFILEPCH<-" - "}
	if(!exists("NAMEFILEBIO")) {NAMEFILEBIO<-" - "}
	if(!exists("NAMEFILEPEG")) {NAMEFILEPEG<-" - "}
	if(!exists("SEEEPEGASEDATA")) {SEEEPEGASEDATA<-" - "}
	if(!exists("INDICE")) {INDICE<-"non"}
	if(!exists("EXCLU_POLNS")) {EXCLU_POLNS<-"non"}
	if(!exists("SEEEECOLOME")) {SEEEECOLOME<-"non"}
	if(!exists("FRACTIONOKPCH")) {FRACTIONOKPCH<-" - "}
	if(!exists("FRACTIONOKPS")) {FRACTIONOKPS<-" - "}
	if(!exists("FRACTIONOKCHIM")) {FRACTIONOKCHIM<-" - "}
	if(!exists("UNITEOKPCH")) {UNITEOKPCH<-" - "}
	if(!exists("UNITEOKPS")) {UNITEOKPS<-" - "}
	if(!exists("UNITEOKCHIM")) {UNITEOKCHIM<-" - "}
	if(!exists("FREQOKPCH")) {FREQOKPCH<-" - "}
	if(!exists("FREQOKPS")) {FREQOKPS<-" - "}
	if(!exists("FREQOKCHIM")) {FREQOKCHIM<-" - "}
	if(!exists("NBANFREQ")) {NBANFREQ<-" - "}
	if(!exists("METHODEMOYPS")) {METHODEMOYPS<-" - "}
	if(!exists("METHODEMOYCHIM")) {METHODEMOYCHIM<-" - "}
	if(MODULE %in% c("REEE2016","REEE2021") & !exists("LQSUPNQEMAPS")) {LQSUPNQEMAPS<-" - "}
	if(MODULE %in% c("REEE2016","REEE2021")  & !exists("SUPPR_LQ_ABERRANTE_PS")) {SUPPR_LQ_ABERRANTE_PS<-" - "}
	if(!exists("nbstatmanQ")) {nbstatmanQ<-"non"}
	if(!exists("ndoublpch")) {ndoublpch<-" - "}
	if(!exists("ndoublps")) {ndoublps<-" - "}
	if(!exists("ndoublbio")) {ndoublbio<-" - "}
	if(!exists("ndoublpeg")) {ndoublpeg<-" - "}
	if(!exists("ndoublchim")) {ndoublchim<-" - "}
	if(!exists("nb_null_pch")) {nb_null_pch<-" - "}
	if(!exists("nb_null_bio")) {nb_null_bio<-" - "}
	if(!exists("NB_STATIONS_NCONF")) {NB_STATIONS_NCONF<-" - "}
	if(!exists("periodePCH")) {periodePCH<-" - "}
	if(!exists("periodePS")) {periodePS<-" - "}
	if(!exists("periodeBio")) {periodeBio<-" - "}
	if(!exists("NAMEFILESTAT")) {NAMEFILESTAT<-" - "}
	if(!exists("NAMEFILEME")) {NAMEFILEME<-" - "}
	if(!exists("NBLQMANQ_PS")) {NBLQMANQ_PS<-" - "}

	if (METHODEMOYPS == "ANNEERECENTE") {METHODEMOYPS = "année la plus récente"}
	if (METHODEMOYPS == "ANNEECHRONIQUE") {METHODEMOYPS = "année ayant la meilleure fréquence de prélèvement"}
	if (METHODEMOYPS == "NOFILTREANNEE") {METHODEMOYPS = "Ensemble du jeu de donnée - Pas de filtre sur les années"}

	if (METHODEMOYCHIM == "ANNEERECENTE") {METHODEMOYCHIM = "année la plus récente"}
	if (METHODEMOYCHIM == "ANNEECHRONIQUE") {METHODEMOYCHIM = "année ayant la meilleure fréquence de prélèvement"}
	if (METHODEMOYCHIM == "NOFILTREANNEE") {METHODEMOYCHIM = "Ensemble du jeu de donnée - Pas de filtre sur les années"}

    

	### calcul le nombre de stations
	nbstations<-as.character(nrow(RLT_FINALSTATION))

	## DFmetaECOLO est un data.fame qui rassemble les options métadonnées
	LIST_OPTIONS_TEMPLATE_ecolo<-read.csv2(paste(pathsite,"template_options.csv",sep=""))
	DFmetaECOLO<-data.frame(c(numversion, MODULE,LOGIN,BASSIN,format(Sys.time() , "%A %d %m %Y à %Hh%M"),
	periodePCH,periodePS,periodeBio,PROFONDEUR,
	nbstations,
	SEEEPCH, SEEEPS, SEEEBIO, SEEEECOLO,  NAMEFILEPCH, NAMEFILEBIO, NAMEFILEPEG,SEEEPEGASEDATA,NAMEFILESTAT,NAMEFILEME,
	EXCLU_POLNS, SEEEECOLOME, FRACTIONOKPCH,FRACTIONOKPS, UNITEOKPCH,UNITEOKPS, 
	FREQOKPCH, FREQOKPS, NBANFREQ, METHODEMOYPS,ifelse(MODULE=="REEE2010",NA,LQSUPNQEMAPS), ifelse(MODULE=="REEE2010",NA,SUPPR_LQ_ABERRANTE_PS),
	NBLQMANQ_PS, nbstatmanQ,ndoublpch,ndoublps, ndoublpeg,  ndoublbio,nb_null_pch,nb_null_bio, NB_STATIONS_NCONF,
	COMMENTAIRE))
	DFmetaECOLO<-na.omit(DFmetaECOLO)
	
	print(DFmetaECOLO)
	library(XLConnect)
	Sys.sleep(1)

	# Export des options dans la l'onglet Accueil
	XLSload <- loadWorkbook(XLS, create = FALSE)
	print(SEEE_DEBformat)
	setStyleAction(XLSload, XLC$"STYLE_ACTION.NONE")
	writeWorksheet(XLSload, LIST_OPTIONS_TEMPLATE_ecolo, sheet = "Accueil", startRow = 14, startCol = 1 , header = FALSE)

	writeWorksheet(XLSload, DFmetaECOLO, sheet = "Accueil", startRow = 14, startCol = 3 , header = FALSE)
	#setColumnWidth(XLSload,"Accueil",3,-1)
	saveWorkbook(XLSload)
	tcl("update")
	Sys.sleep(0.2)
	### Export du lexique
	######################
	#Selection du lexique
	lexiQ<-read.csv2(paste(pathsite,"lexique",fE.MOD,".csv",sep=""))
	if (SEEEECOLOME=="oui") {
		namesexport<-unique(c(names(RLT_FINALSTATION),names(RLT_FINALME))) 
		print(namesexport)
		} else {
		namesexport<-names(RLT_FINALSTATION)
	}

	if (INDICE=="oui"){
		namesexport<-unique(c(namesexport,names(RLT_FINALINDICE)))
	}

	if (FREQPRELEV=="oui"){
		namesexport<-unique(c(namesexport,names(RLT_FINALFREQ)))
	}

	
	if (STATISTIQUE == "oui"){
		namesexport<-unique(c(namesexport,names(RLT_STATSTATION),names(VARIA)))
	}

	if (STATISTIQUE == "oui" & SEEEECOLOME == "oui"){
		namesexport<-unique(c(namesexport,names(RLT_STATME)))
	}	
	
	if (exists("RLT_FIABILITE_PCH") ){
		namesexport<-unique(c(namesexport,names(RLT_FIABILITE_PCH)))
	}
	print("pret à exporter lexique")
	lexiQselection<-lexiQ[lexiQ$NOM_COURT %in% namesexport,]
	
	
	 #Export à proprement parlé  du lexique
	XLSload <- loadWorkbook(XLS, create = FALSE)
	createSheet(XLSload, name = "Lexique")
	writeWorksheet(XLSload, lexiQselection, sheet = "Lexique", startRow = 1, startCol = 1 , header = TRUE)
	setColumnWidth(XLSload,"Lexique",1:ncol(lexiQselection),-1) # égale à double-clic pour largeur des colonnes
	saveWorkbook(XLSload)
	tcl("update")

	#### Export des onglets (résultats des calculs) + mise en couleur
	##################################################################

	# Noms des colonnes à coloriser. Les noms des colonnes (hormis les états) sont à chercher dans les listes des paramètres
	# Par sécurité, on laisse un certain nombre de paramtres en commentaires
	# colorscol<-c(   
	# "ETATECOLO","ETATECOLOHPS" , "ETATBIO",  "ETATPCH",  "ETATPS", "IBD","IBG","IBGA","IPR","IBMR","bilano2",  "nut","temp",   
	# "acid", "nonsynth", "synth", "o2", "sato2", "dbo5", "cod","po43", "phos", "nh4","no2","no3","tempe",  
	# "phmin", "phmax","no3v2", "arsenic", "chrome", "cuivre", "zinc", "chlortoluron","oxadiazon",  
	# "24d","24mcpa","metazachlore","aminotriazole","nicosulfuron","ampa","glyphosate","bentazone","diflufenicanil","cyprodinil",
	# "imidaclopride","iprodione","azoxystrobine","toluene","phosphate tributyle","biphenyle","boscalid","metaldehyde","tebuconazole","chlorprophame",
	# "xylene","pendimethaline","chlordecone","ETATECOLOME" , "ETATECOLOHPSME" ,"ETATBIOME","ETATPCHME","ETATPSME")
	colorscol<-c(   "ETATECOLO","ETATECOLOHPS" ,"ETATECOLOHBO2", "ETATECOLOHPSetBO2", "ETATBIO",  "ETATPCH_SSBILANO2", "ETATPCH",  "ETATPS","ETATECOLOME" , "ETATECOLOHPSME" ,"ETATBIOME","ETATPCHME","ETATPSME",ifelse(MODULE=="REEE2010",NA,"no3v2"))
	colorscol<-na.omit(colorscol)
	colorscol<-c(colorscol, ETATPCH_NAMES,PARAMETREBIO_NAMES, PARAMETREPCH_NAMES, PARAMETREPS_NAMES,PARAMETREPCHELT_NAMES,PARAMETREPOLLUANT_NAMES)
	colorscol<-c(colorscol, "ETATECOLO_SSEXCEPTLOC","ETATECOLOHPS_SSEXCEPTLOC")
	colorscol<-c(colorscol, "ETATPCHMESSEXCEPTLOC","ETATPCHMESSEXCEPTLOC","ETATECOLOHPS_SSEXCEPTLOC")
	
	colorscol<-unique(c(colorscol, toupper(colorscol), tolower(colorscol)))

	## Export des résultats finaux
	print(summary(RLT_FINALSTATION))
	NUMCOL<-(1:ncol(RLT_FINALSTATION))[names(RLT_FINALSTATION) %in% colorscol]
	Sys.sleep(1) ## Essai pour relacher la mémoire Java 
	colorXLS(XLS, createxls = FALSE, nameonglet = "Station", createsheet = TRUE, RLT_FINALSTATION, numcol = NUMCOL, depR =1 , depC=1, entete = TRUE, couleur=0)

	if (SEEEECOLOME=="oui") {
		print(summary(RLT_FINALME))
		NUMCOL<-(1:ncol(RLT_FINALME))[names(RLT_FINALME) %in% colorscol]
		colorXLS(XLS, createxls = FALSE, nameonglet = "MasseEau", createsheet = TRUE, RLT_FINALME, numcol = NUMCOL, depR =1 , depC=1, entete = TRUE, couleur=0)
	}

	gc()
	tcl("update")
	Sys.sleep(1) ## Essai pour relacher la mémoire Java 

	## Export des indices
	if (INDICE == "oui"){
		XLSload <- loadWorkbook(XLS, create = FALSE)
		createSheet(XLSload, name = "StationIndice")
		writeWorksheet(XLSload, RLT_FINALINDICE, sheet = "StationIndice", startRow = 1, startCol = 1 , header = TRUE)
		setColumnWidth(XLSload,"StationIndice",1:ncol(RLT_FINALINDICE),-1)
		saveWorkbook(XLSload)
		tcl("update")

	}

	if(MODULE == "REEE2016" | MODULE == "REEE2018"  | MODULE == "REEE2021"){
	## Export des CAS PS
	if (exists("CAS_POLSPE") ){
		XLSload <- loadWorkbook(XLS, create = FALSE)
		createSheet(XLSload, name = "CasPS")
		writeWorksheet(XLSload, CAS_POLSPE, sheet = "CasPS", startRow = 1, startCol = 1 , header = TRUE)
		setColumnWidth(XLSload,"CasPS",1:ncol(RLT_FINALINDICE),-1)
		saveWorkbook(XLSload)
		tcl("update")
	}}
	
	## Export des Frequences
	if (FREQPRELEV == "oui"){
		XLSload <- loadWorkbook(XLS, create = FALSE)
		createSheet(XLSload, name = "StationFreQ")
		writeWorksheet(XLSload, RLT_FINALFREQ, sheet = "StationFreQ", startRow = 1, startCol = 1 , header = TRUE)
		setColumnWidth(XLSload,"StationFreQ",1:ncol(RLT_FINALFREQ),-1)
		saveWorkbook(XLSload)
		tcl("update")
		
		
			if(exists("RLT_POLSPEFREQQUANTI")) {  ##Ajout 20/04/2018 : calcul de frequence de mesures quantifiées
		
				XLSload <- loadWorkbook(XLS, create = FALSE)
				createSheet(XLSload, name = "PSFreQQuanti")
				writeWorksheet(XLSload, RLT_POLSPEFREQQUANTI, sheet = "PSFreQQuanti", startRow = 1, startCol = 1 , header = TRUE)
				setColumnWidth(XLSload,"PSFreQQuanti",1:ncol(RLT_POLSPEFREQQUANTI),-1)
				saveWorkbook(XLSload)
				tcl("update")
						
			}		
	}

	
	## Export des Frequences
	if (MODULE == "REEE2018"  & CEPE == "CE" & exists("PARAMBIOTYPEFR")  ){
		XLSload <- loadWorkbook(XLS, create = FALSE)
		NUMCOL<-3:ncol(PARAMBIOTYPEFR)
		colorOuiNon(XLS, createxls = FALSE, nameonglet = "ParamBio", createsheet = TRUE, PARAMBIOTYPEFR, numcol = NUMCOL, depR =1 , depC=1, entete = TRUE, couleur=0)
	
		tcl("update")
	}
	
	## Export des Statistiques
	if (STATISTIQUE == "oui"){
		XLSload <- loadWorkbook(XLS, create = FALSE)
		createSheet(XLSload, name = "StationStat")
		writeWorksheet(XLSload, RLT_STATSTATION, sheet = "StationStat", startRow = 1, startCol = 1 , header = TRUE)
		setColumnWidth(XLSload,"StationStat",1:ncol(RLT_STATSTATION),-1)
		saveWorkbook(XLSload)
		tcl("update")
	}

	## Export des Statistiques Masses d'eau
	if (SEEEECOLOME=="oui" & STATISTIQUE == "oui" & CEPE == "CE") {
		XLSload <- loadWorkbook(XLS, create = FALSE)
		createSheet(XLSload, name = "MEStat")
		writeWorksheet(XLSload, RLT_STATME, sheet = "MEStat", startRow = 1, startCol = 1 , header = TRUE)
		setColumnWidth(XLSload,"MEStat",1:ncol(RLT_STATME),-1)
		saveWorkbook(XLSload)
		tcl("update")
	}
	
	## Export des Statistiques Masses d'eau
	if (STATISTIQUE == "oui") {
		XLSload <- loadWorkbook(XLS, create = FALSE)
		createSheet(XLSload, name = "Variabilité")
		writeWorksheet(XLSload, VARIA, sheet = "Variabilité", startRow = 1, startCol = 1 , header = TRUE)
		setColumnWidth(XLSload,"Variabilité",1:ncol(VARIA),-1)
		saveWorkbook(XLSload)
		tcl("update")
	
	
		if (SEEEPCH=="oui" & CEPE == "CE") { #test uniquement dévoloppé pour la PCH
			#export fiabilité
			colorscolfiabi<-paste0("c_",PARAMETREPCH$NOM)
			NUMCOL<-(1:ncol(RLT_FIABILITE_PCH))[names(RLT_FIABILITE_PCH) %in% colorscolfiabi]
			colorXLSFia(XLS, createxls = FALSE, nameonglet = "Fiabilité", createsheet = TRUE, RLT_FIABILITE_PCH, numcol = NUMCOL, depR =1 , depC=1, entete = TRUE, couleur=0)
			}
	gc()
	tcl("update")
	
	}

	
	if (CEPE == "PE" & exists("PARAMPE")) {
		XLSload <- loadWorkbook(XLS, create = FALSE)
		createSheet(XLSload, name = "ValeursSeuil")
		writeWorksheet(XLSload, PARAMPE, sheet = "ValeursSeuil", startRow = 1, startCol = 1 , header = TRUE)
		setColumnWidth(XLSload,"ValeursSeuil",1:ncol(PARAMPE),-1)
		saveWorkbook(XLSload)
		tcl("update")

}

	
XLSECOLO<-XLS
}

#nom du fichier
if(SEEECHIM=="oui"){
	date()
	

	## EXPORT EXCEL : copy le template vers un nouveau fichier Excel de réusltat	
	source(paste(pathS,"misecouleur.r",sep=""))
	NOMXLSTEMP <- paste(racine,"/TEMPLATE/XLS_template/template_chim.xls",sep="")
	XLS<-paste(CH_OUTPUT,NAMEOUTPUT,"CHIM.xls",sep="")
	if (file.exists(XLS)) {	XLS<-paste(CH_OUTPUT,NAMEOUTPUT,"CHIM_",SEEE_DEBformat,".xls",sep="")}
	file.copy(NOMXLSTEMP, XLS, overwrite = TRUE)

	
	
	####Export des paramètres (métadonnées)
	if(!exists("SEEEPCH")) {SEEEPCH<-"non"}
	if(!exists("SEEEPS")) {SEEEPS<-"non"}
	if(!exists("SEEEBIO")) {SEEEBIO<-"non"}
	if(!exists("SEEEECOLO")) {SEEEECOLO<-"non"} 
	if(!exists("SEEECHIM")) {SEEECHIM<-"non"}
	if(!exists("PROFONDEUR")) {PROFONDEUR<-" - "}
	if(!exists("MISECOULEUR")) {MISECOULEUR<-"non"}
	if(!exists("NAMEFILEPCH")) {NAMEFILEPCH<-" - "}
	if(!exists("NAMEFILEBIO")) {NAMEFILEBIO<-" - "}
	if(!exists("NAMEFILEPEG")) {NAMEFILEPEG<-" - "}
	if(!exists("INDICE")) {INDICE<-"non"}
	if(!exists("EXCLU_POLNS")) {EXCLU_POLNS<-"non"}
	if(!exists("SEEEECOLOME")) {SEEEECOLOME<-"non"}
	if(!exists("FRACTIONOKPCH")) {FRACTIONOKPCH<-" - "}
	if(!exists("FRACTIONOKPS")) {FRACTIONOKPS<-" - "}
	if(!exists("FRACTIONOKCHIM")) {FRACTIONOKCHIM<-" - "}
	if(!exists("UNITEOKPCH")) {UNITEOKPCH<-" - "}
	if(!exists("UNITEOKPS")) {UNITEOKPS<-" - "}
	if(!exists("UNITEOKCHIM")) {UNITEOKCHIM<-" - "}
	if(!exists("FREQOKPCH")) {FREQOKPCH<-" - "}
	if(!exists("FREQOKPS")) {FREQOKPS<-" - "}
	if(!exists("FREQOKCHIM")) {FREQOKCHIM<-" - "}
	if(!exists("METHODEMOYPS")) {METHODEMOYPS<-" - "}
	if(!exists("METHODEMOYCHIM")) {METHODEMOYCHIM<-" - "}
	if(!exists("NBANFREQ")) {NBANFREQ<-" - "}
	if(MODULE %in% c("REEE2016","REEE2018","REEE2021")  & !exists("LQSUPNQEMACHIM")) {LQSUPNQEMACHIM<-" - "}
	if(MODULE %in% c("REEE2016","REEE2018","REEE2021")  & !exists("SUPPR_LQ_ABERRANTE_CHIM")) {SUPPR_LQ_ABERRANTE_CHIM<-" - "}
	if(!exists("nbstatmanQ")) {nbstatmanQ<-"non"}
	if(!exists("ndoublpch")) {ndoublpch<-" - "}
	if(!exists("ndoublps")) {ndoublps<-" - "}
	if(!exists("ndoublbio")) {ndoublbio<-" - "}
	if(!exists("ndoublpeg")) {ndoublpeg<-" - "}
	if(!exists("ndoublchim")) {ndoublchim<-" - "}
	if(!exists("nb_null_pch")) {nb_null_pch<-" - "}
	if(!exists("nb_null_bio")) {nb_null_bio<-" - "}
	if(!exists("NB_STATIONS_NCONF")) {NB_STATIONS_NCONF<-" - "}
	if(!exists("periodeChim")) {periodeChim<-" - "}
	if(!exists("NAMEFILESTAT")) {NAMEFILESTAT<-" - "}
	if(!exists("NAMEFILEME")) {NAMEFILEME<-" - "}
	if(!exists("NBLQMANQ_CHIM")) {NBLQMANQ_CHIM<-" - "}

	
	if (METHODEMOYPS == "ANNEERECENTE") {METHODEMOYPS = "année la plus récente"}
	if (METHODEMOYPS == "ANNEECHRONIQUE") {METHODEMOYPS = "année ayant la meilleure fréquence de prélèvement"}
	if (METHODEMOYPS == "NOFILTREANNEE") {METHODEMOYPS = "Ensemble du jeu de donnée - Pas de filtre sur les années"}

	if (METHODEMOYCHIM == "ANNEERECENTE") {METHODEMOYCHIM = "année la plus récente"}
	if (METHODEMOYCHIM == "ANNEECHRONIQUE") {METHODEMOYCHIM = "année ayant la meilleure fréquence de prélèvement"}
	if (METHODEMOYCHIM == "NOFILTREANNEE") {METHODEMOYCHIM = "Ensemble du jeu de donnée - Pas de filtre sur les années"}


	### calcul le nombre de stations
	nbstations<-as.character(nrow(RLT_CHIMSTATION))

	## DFmetaCHIM est un data.fame qui rassemble les options métadonnées
	LIST_OPTIONS_TEMPLATE_chim<-read.csv2(paste(pathsite,"template_options_chim.csv",sep=""))
	DFmetaCHIM<-data.frame(c(numversion, MODULE,LOGIN,BASSIN,format(Sys.time() , "%A %d %m %Y à %Hh%M"),
	periodeChim,PROFONDEUR,
	nbstations,
	SEEECHIM, NAMEFILEPCH, NAMEFILESTAT,NAMEFILEME,
	SEEECHIMME, FRACTIONOKCHIM,  UNITEOKCHIM,
	FREQOKCHIM,  NBANFREQ, METHODEMOYCHIM,
	ifelse(MODULE=="REEE2010",NA,LQSUPNQEMACHIM),ifelse(MODULE=="REEE2010",NA,SUPPR_LQ_ABERRANTE_CHIM),
	NBLQMANQ_CHIM,
	nbstatmanQ,ndoublchim,
	COMMENTAIRE))
	DFmetaCHIM<-na.omit(DFmetaCHIM)

	print(DFmetaCHIM)
	Sys.sleep(0.5)

	# Export des options dans la l'onglet Accueil
	XLSload <- loadWorkbook(XLS, create = FALSE)
	print(SEEE_DEBformat)
	setStyleAction(XLSload, XLC$"STYLE_ACTION.NONE")
	writeWorksheet(XLSload, LIST_OPTIONS_TEMPLATE_chim, sheet = "Accueil", startRow = 12, startCol = 1 , header = FALSE)

	writeWorksheet(XLSload, DFmetaCHIM, sheet = "Accueil", startRow = 12, startCol = 3 , header = FALSE)
	#setColumnWidth(XLSload,"Accueil",3,-1)
	saveWorkbook(XLSload)
	tcl("update")
	### Export du lexique
	######################
	#Selection du lexique
	lexiQ<-read.csv2(paste(pathsite,"lexiquechim.csv",sep=""))
	if (SEEECHIMME=="oui") {
		namesexport<-unique(c(names(RLT_CHIMSTATION),names(RLT_CHIMETATME))) 
		} else {
		namesexport<-names(RLT_CHIMSTATION)
	}

	if (INDICE=="oui"){
		namesexport<-unique(c(namesexport,names(RLT_CHIMCMAMA)))
	}

	if (FREQPRELEV=="oui"){
		namesexport<-unique(c(namesexport,names(RLT_CHIMFREQ)))
	}

	if (STATISTIQUE == "oui"){
		namesexport<-unique(c(namesexport,names(RLT_CHIMSTAT),names(VARIA_CHIM)))
	}

	if (STATISTIQUE == "oui" & SEEECHIMME == "oui"){
		namesexport<-unique(c(namesexport,names(RLT_STATCHIMME)))
	}	
	
	lexiQselection<-lexiQ[lexiQ$NOM_COURT %in% namesexport,]
	 #Export à proprement parlé  du lexique
	XLSload <- loadWorkbook(XLS, create = FALSE)
	createSheet(XLSload, name = "Lexique")
	writeWorksheet(XLSload, lexiQselection, sheet = "Lexique", startRow = 1, startCol = 1 , header = TRUE)
	setColumnWidth(XLSload,"Lexique",1:ncol(lexiQselection),-1) # égale à double-clic pour largeur des colonnes
	saveWorkbook(XLSload)
	tcl("update")

	#### Export des onglets (résultats des calculs) + mise en couleur
	##################################################################

	colorscol<-c(     # colonne à mettre en couleur
	"CLASSEETAT","ETATCHIMME", "ETATCHIMME_SSHAPUBI","ETATCHIM","ETATCHIM_SSHAPUBI","METAUX","PEST","POLIND","AUTREPOL","Cd","Hg","Ni","Pb",
	"Alachlore","Atrazine","Chlorfenvi","Diuron","Endosulfan","EtChlorpy","HCH",
	"Isoproturon","PentaClBz","Simazine","Triflural","Anthracene","Benzene","C1013Clalc",
	"CHCl3","DEHP","1.2-2ClEth","2ClMethane","2Phbrome","Naphtalene","4-n-nonylp","4-ter-ocph",
	"TTCE","CCl4","TCE","DDT 44","DDT","Fluoranth","Benzo(a)py","Be(b+k)flu","Be(ghi)indeno","HCB","HCBu",
	"PCP","Pestcyclo","Tributytin","SomTriClBz",toupper(PARAGROUPCHIM$PARAGROUPLIBCOURT) )
	colorscol<-na.omit(colorscol)
	colorscol<-unique(c(colorscol, toupper(colorscol)))

	## Export des résultats finaux
	print(summary(RLT_CHIMSTATION))
	NUMCOL<-(1:ncol(RLT_CHIMSTATION))[names(RLT_CHIMSTATION) %in% colorscol]
	colorXLSCHIM(XLS, createxls = FALSE, nameonglet = "Station", createsheet = TRUE, RLT_CHIMSTATION, numcol = NUMCOL, depR =1 , depC=1, entete = TRUE, couleur=0)

	if (SEEECHIMME=="oui") {
		print(summary(RLT_CHIMETATME))
		NUMCOL<-(1:ncol(RLT_CHIMETATME))[names(RLT_CHIMETATME) %in% colorscol]
		print("list des NUMCOL ME")
		print(NUMCOL)
		colorXLSCHIM(XLS, createxls = FALSE, nameonglet = "MasseEau", createsheet = TRUE, RLT_CHIMETATME, numcol = NUMCOL, depR =1 , depC=1, entete = TRUE, couleur=0)
	}
	
	rm(XLSload)
	xlcFreeMemory()
	gc()
	Sys.sleep(0.5) ## Essai pour relacher la mémoire Java 
	
	
	
	## Export des indices (Soit sous forme d'onglet si < à 10 000 lignes soit en csv)
	if (INDICE == "oui"){
		print("Export onglet StationIndice")
		print("nombre de lignes : ")
		print(nrow(RLT_CHIMCMAMA))
		XLSload <- loadWorkbook(XLS, create = FALSE)
		createSheet(XLSload, name = "StationIndice")
		if (nrow(RLT_CHIMCMAMA) < 10000) {
			writeWorksheet(XLSload, RLT_CHIMCMAMA, sheet = "StationIndice", startRow = 1, startCol = 1 , header = TRUE)
			setColumnWidth(XLSload,"StationIndice",1:ncol(RLT_CHIMCMAMA),-1)
			print("Export au format Excel")
		} else {
			CSV_NAME<-gsub(".xls","_StationIndice.csv",XLS)
			CSV_NAME<-gsub(".xls","_StationIndice.csv",CSV_NAME)
			write.csv2(RLT_CHIMCMAMA, file = CSV_NAME)
			writeWorksheet(XLSload, paste0("Nombre de lignes trop important"), sheet = "StationIndice", startRow = 1, startCol = 1, header = FALSE )
			writeWorksheet(XLSload, paste0("cf : ",CSV_NAME)  , sheet = "StationIndice", startRow = 2, startCol = 1, header = FALSE  )
			setColumnWidth(XLSload,"StationIndice",1,-1)
			tkmessageBox(title = "Info", message = paste("Compte tenu du nombre important de lignes, l'onglet StationIndice a été exporté au format CSV\n",CSV_NAME ,sep=""), icon = "info", type = "ok")
			tcl("update")
			print("Export au format Excel")
		}
		saveWorkbook(XLSload)
		tcl("update")
		print(" Fin Export onglet StationIndice")
	}

	
	rm(XLSload)
	xlcFreeMemory() ## Essai pour relacher la mémoire Java 
	gc()
	Sys.sleep(0.5) 

	
	if(MODULE == "REEE2016" | MODULE == "REEE2018" | MODULE == "REEE2021"){

	## Export des CAS CHIMMA
	if (exists("CAS_CHIMMA") ){
		print("Export onglet CasChimMA")
		print("nombre de lignes : ")
		print(nrow(CAS_CHIMMA))
		XLSload <- loadWorkbook(XLS, create = FALSE)
		createSheet(XLSload, name = "CasChimMA")
		writeWorksheet(XLSload, CAS_CHIMMA, sheet = "CasChimMA", startRow = 1, startCol = 1 , header = TRUE)
		setColumnWidth(XLSload,"CasChimMA",1:ncol(CAS_CHIMMA),-1)
		saveWorkbook(XLSload)
		tcl("update")
		print(" Fin Export onglet CasChimMA")

	}}
	
	
	
	rm(XLSload)
	xlcFreeMemory() ## Essai pour relacher la mémoire Java 
	gc()
	Sys.sleep(0.5) 

	
	## Export des Frequences
	if (FREQPRELEV == "oui"){
		print("Export onglet StationFreQ")
		gc()
		XLSload <- loadWorkbook(XLS, create = FALSE)
		createSheet(XLSload, name = "StationFreQ")
		writeWorksheet(XLSload, RLT_CHIMFREQ, sheet = "StationFreQ", startRow = 1, startCol = 1 , header = TRUE)
		setColumnWidth(XLSload,"StationFreQ",1:ncol(RLT_CHIMFREQ),-1)
		saveWorkbook(XLSload)
		tcl("update")
	
		rm(XLSload)
		xlcFreeMemory() ## Essai pour relacher la mémoire Java 
		gc()
		Sys.sleep(0.5) 
	
		XLSload <- loadWorkbook(XLS, create = FALSE)
		createSheet(XLSload, name = "FreQQuanti")
		writeWorksheet(XLSload, RLT_CHIMFREQQUANTI, sheet = "FreQQuanti", startRow = 1, startCol = 1 , header = TRUE)
		setColumnWidth(XLSload,"FreQQuanti",1:ncol(RLT_CHIMFREQQUANTI),-1)
		saveWorkbook(XLSload)
		tcl("update")
	
	
	
	}

	
	rm(XLSload)
	xlcFreeMemory() ## Essai pour relacher la mémoire Java 
	gc()
	Sys.sleep(0.5) 
	
	
	## Export des Statistiques
	if (STATISTIQUE == "oui"){
		gc()
		#tableau rapportage
		print("Export onglet Rapportage")
		NUMCOL<-(1:ncol(RLT_CHIMSTAT))[names(RLT_CHIMSTAT) %in% "CLASSEETAT"]
		colorXLSCHIM(XLS, createxls = FALSE, nameonglet = "Rapportage", createsheet = TRUE, RLT_CHIMSTAT, numcol = NUMCOL, depR =1 , depC=1, entete = TRUE, couleur=0)
		tcl("update")

		rm(XLSload)
		xlcFreeMemory() ## Essai pour relacher la mémoire Java 
		gc()
		Sys.sleep(0.5) 
	
		
		#tableau statistique Station
		print("Export onglet StationStat")
		XLSload <- loadWorkbook(XLS, create = FALSE)
		createSheet(XLSload, name = "StationStat")
		writeWorksheet(XLSload, RLT_STATCHIMSTATION, sheet = "StationStat", startRow = 1, startCol = 1 , header = TRUE)
		setColumnWidth(XLSload,"StationStat",1:ncol(RLT_STATCHIMSTATION),-1)
		saveWorkbook(XLSload)
		tcl("update")
		
		#tableau statistique MAsse d'eau
		if (SEEECHIMME=="oui") {
			gc()
			XLSload <- loadWorkbook(XLS, create = FALSE)
			createSheet(XLSload, name = "MeStat")
			writeWorksheet(XLSload, RLT_STATCHIMME, sheet = "MeStat", startRow = 1, startCol = 1 , header = TRUE)
			setColumnWidth(XLSload,"MeStat",1:ncol(RLT_STATCHIMME),-1)
			saveWorkbook(XLSload)
			tcl("update")
		}

	}

		## Export de l'onglet Variabilité
	if (STATISTIQUE == "oui") {
		
		rm(XLSload)
		xlcFreeMemory() ## Essai pour relacher la mémoire Java 
		gc()
		Sys.sleep(0.5) 
	
		print("Export onglet Variabilité")
		print("nombre de lignes : ")
		print(nrow(VARIA_CHIM))
		XLSload <- loadWorkbook(XLS, create = FALSE)
		createSheet(XLSload, name = "Variabilité")
		if (nrow(VARIA_CHIM) < 10000) {
			writeWorksheet(XLSload, VARIA_CHIM, sheet = "Variabilité", startRow = 1, startCol = 1 , header = TRUE)
			setColumnWidth(XLSload,"Variabilité",1:ncol(VARIA_CHIM),-1)
			print("Export au format Excel")
		} else {
			CSV_NAME<-gsub(".xls","_variabilite.csv",XLS)
			CSV_NAME<-gsub(".xls","_variabilite.csv",CSV_NAME)
			write.csv2(VARIA_CHIM, file = CSV_NAME)
			writeWorksheet(XLSload, paste0("Nombre de lignes trop important"), sheet = "Variabilité", startRow = 1, startCol = 1 , header = FALSE)
			writeWorksheet(XLSload, paste0("cf : ",CSV_NAME)  , sheet = "Variabilité", startRow = 2, startCol = 1 , header = FALSE)
			setColumnWidth(XLSload,"Variabilité",1,-1)
			tkmessageBox(title = "Info", message = paste("Compte tenu du nombre important de lignes, l'onglet Variabilité a été exporté au format CSV\n",CSV_NAME ,sep=""), icon = "info", type = "ok")
			tcl("update")
			print("Export au format Excel")
		}
		saveWorkbook(XLSload)
		tcl("update")
		print(" Fin Export onglet Variabilité")
	
	
	
	}
	
	if(MODULE == "REEE2016" | MODULE == "REEE2018" |  MODULE == "REEE2021"){
	## Export des tables de parametres pour le calcul de contamination

		XLSload <- loadWorkbook(XLS, create = FALSE)
		createSheet(XLSload, name = "Parametres")
		writeWorksheet(XLSload, PARAMETRECHIM, sheet = "Parametres", startRow = 1, startCol = 1 , header = TRUE)
		setColumnWidth(XLSload,"Parametres",1:ncol(PARAMETRECHIM),-1)
		
		createSheet(XLSload, name = "Paragroup")
		writeWorksheet(XLSload, PARAGROUPCHIM, sheet = "Paragroup", startRow = 1, startCol = 1 , header = TRUE)
		setColumnWidth(XLSload,"Paragroup",1:ncol(PARAGROUPCHIM),-1)
		
		saveWorkbook(XLSload)
		tcl("update")
	}



		rm(XLSload)
		xlcFreeMemory() ## Essai pour relacher la mémoire Java 
		gc()
		tcl("update")

	## Export des Statistiques Masses d'eau
XLSCHIM<-XLS	

}

if(CONTA=="oui"  & TYPECONTA %in% c(1:3)){
	date()
	

	## EXPORT EXCEL : copy le template vers un nouveau fichier Excel de réusltat	
	source(paste(pathS,"misecouleur.r",sep=""))
	# if (NBSEUIL == 3 ) {
	# NOMXLSTEMP <- paste(racine,"/TEMPLATE/XLS_template/template_conta_4classes.xls",sep="") }
	# if (NBSEUIL == 4 ) {
	# NOMXLSTEMP <- paste(racine,"/TEMPLATE/XLS_template/template_conta_5classes.xls",sep="") }
	# if (NBSEUIL == 5 ) {
	# NOMXLSTEMP <- paste(racine,"/TEMPLATE/XLS_template/template_conta_6classes.xls",sep="") }
	NOMXLSTEMP <- paste(racine,"/TEMPLATE/XLS_template/template_conta.xls",sep="") 
	XLS<-paste(CH_OUTPUT,NAMEOUTPUT,"CONTAMINATION.xls",sep="")
	if (file.exists(XLS)) {	XLS<-paste(CH_OUTPUT,NAMEOUTPUT,"CONTAMINATION_",SEEE_DEBformat,".xls",sep="")}
	file.copy(NOMXLSTEMP, XLS, overwrite = TRUE)

	####Export des paramètres (métadonnées)
	if(!exists("MISECOULEUR")) {MISECOULEUR<-"non"}
	if(!exists("NAMEFILEPCH")) {NAMEFILEPCH<-" - "}
	if(!exists("NBANFREQ")) {NBANFREQ<-" - "}
	if(!exists("nbstatmanQ")) {nbstatmanQ<-"non"}
	if(!exists("ndoublchim")) {ndoublchim<-" - "}
	if(!exists("periodeConta")) {periodeConta<-" - "}
	if(!exists("NAMEFILESTAT")) {NAMEFILESTAT<-" - "}
	if(!exists("NAMEFILEME")) {NAMEFILEME<-" - "}
	if(!exists("NBLQMANQ_CHIM")) {NBLQMANQ_CHIM<-" - "}

	if(!exists("CHOIXMETHODE")) {CHOIXMETHODE<-" - "}
	if(!exists("CHOIXSEUIL")) {CHOIXSEUIL<-" - "}
	if(!exists("CONTASEUIL1")) {CONTASEUIL1<-" - "}
	if(!exists("CONTASEUIL2")) {CONTASEUIL2<-" - "}
	if(!exists("CONTASEUIL3")) {CONTASEUIL3<-" - "}

	##Type de contamination
	if (TYPECONTA == 1) {CHOIXMETHODE<-"Contamination Aigüe"}
	if (TYPECONTA == 2) {CHOIXMETHODE<-"Contamination Chronique"}
	if (TYPECONTA == 3 ) {CHOIXMETHODE<-"Imprégnation"}

	# Nombre de classes
	NBCLASSES<-NBSEUIL + 1
	
	##Type de contamination
	if (TYPECONTA == 1) {CHOIXMETHODE2<-"A partir de la concentration maximale observée sur la période"}
	if (TYPECONTA == 2) {CHOIXMETHODE2<-"A partir de la concentration moyenne observée sur la période"}
	if (TYPECONTA == 3 & CALCCONTA == "MAX")	{CHOIXMETHODE2<-"A partir de la concentration maximale observée sur la période"}
	if (TYPECONTA == 3 & CALCCONTA == "MOY")	{CHOIXMETHODE2<-"A partir de la concentration moyenne observée sur la période"}

	### calcul le nombre de stations
	nbstations<-as.character(nrow(RLT_CONTA))

	## DFmetaCHIM est un data.fame qui rassemble les options métadonnées
	LIST_OPTIONS_TEMPLATE_chim<-read.csv2(paste(pathsite,"template_options_conta.csv",sep=""))
	DFmetaCHIM<-data.frame(c(numversion, MODULE,LOGIN,BASSIN,format(Sys.time() , "%A %d %m %Y à %Hh%M"),
	periodeConta,
	nbstations,
	NAMEFILEPCH, NAMEFILESTAT,NAMEFILEME,
	CONTAME, FRACTIONOKCONTA,  UNITEOKCONTA,
	FREQOKCONTA,  NBANFREQ, NBLQMANQ_CHIM,
	nbstatmanQ,ndoublchim,
	CHOIXMETHODE,
	CHOIXMETHODE2,
	 CHOIXSEUIL,
	CONTASEUIL1,
	CONTASEUIL2,
	CONTASEUIL3,
	NBCLASSES,
	COMMENTAIRE))
	DFmetaCHIM<-na.omit(DFmetaCHIM)


	# Export des options dans la l'onglet Accueil
	XLSload <- loadWorkbook(XLS, create = FALSE)
	print(SEEE_DEBformat)
	setStyleAction(XLSload, XLC$"STYLE_ACTION.NONE")
	writeWorksheet(XLSload, LIST_OPTIONS_TEMPLATE_chim, sheet = "Accueil", startRow = 15, startCol = 1 , header = FALSE)

	writeWorksheet(XLSload, DFmetaCHIM, sheet = "Accueil", startRow = 15, startCol = 3 , header = FALSE)
	#setColumnWidth(XLSload,"Accueil",3,-1)
	saveWorkbook(XLSload)
	tcl("update")
	
	### Export du lexique
	######################
	#Selection du lexique
	# lexiQ<-read.csv2(paste(pathsite,"lexiquechim.csv",sep=""))
	# if (SEEECHIMME=="oui") {
		# namesexport<-unique(c(names(RLT_CHIMSTATION),names(RLT_CHIMETATME))) 
		# } else {
		# namesexport<-names(RLT_CHIMSTATION)
	# }

	# if (INDICE=="oui"){
		# namesexport<-unique(c(namesexport,names(RLT_CHIMCMAMA)))
	# }

	# if (FREQPRELEV=="oui"){
		# namesexport<-unique(c(namesexport,names(RLT_CHIMFREQ)))
	# }

	# if (STATISTIQUE == "oui"){
		# namesexport<-unique(c(namesexport,names(RLT_CHIMSTAT),names(VARIA_CHIM)))
	# }

	# if (STATISTIQUE == "oui" & SEEECHIMME == "oui"){
		# namesexport<-unique(c(namesexport,names(RLT_STATCHIMME)))
	# }	
	
	# lexiQselection<-lexiQ[lexiQ$NOM_COURT %in% namesexport,]
	 #Export à proprement parlé  du lexique
	# XLSload <- loadWorkbook(XLS, create = FALSE)
	# createSheet(XLSload, name = "Lexique")
	# writeWorksheet(XLSload, lexiQselection, sheet = "Lexique", startRow = 1, startCol = 1 , header = TRUE)
	# setColumnWidth(XLSload,"Lexique",1:ncol(lexiQselection),-1) # égale à double-clic pour largeur des colonnes
	# saveWorkbook(XLSload)
	# tcl("update")

	#### Export des onglets (résultats des calculs) + mise en couleur
	##################################################################

	colorscol<-c(     # colonne à mettre en couleur
	"CONTAMINATION","CONTAMINATIONME",
	PARAMETRECONTA_ETUDIE$NOMCOURT,
	TABLEFAMILLE$FAMILLE
	)
	colorscol<-na.omit(colorscol)
	colorscol<-unique(c(colorscol, toupper(colorscol)))

	## Export des résultats finaux
	#Calcul du nombre d'onglet si + de 254 colonnes
	NCOLRLTCONTA<-ncol(RLT_CONTA)
	nbonglet<-ceiling(NCOLRLTCONTA/254)
	coldep<-2
	colfin<-254
		
	for(i in 1:nbonglet){
		print(paste("export de Station, onlet numero ",i))
		colfin<-pmin(colfin,NCOLRLTCONTA)
		RLT_CONTA_EXPORT<-RLT_CONTA[,c(1,coldep:colfin)]
		 XLSload <- loadWorkbook(XLS, create = FALSE)
		if(i==1) { nameongletconta<-"Station" } 
		if(i>1) { nameongletconta<- paste0("Station",i) } 
		
		NUMCOL<-(1:ncol(RLT_CONTA_EXPORT))[names(RLT_CONTA_EXPORT) %in% colorscol]
		colorXLS(XLS, createxls = FALSE, nameonglet = nameongletconta, createsheet = TRUE, RLT_CONTA_EXPORT, numcol = NUMCOL, depR =1 , depC=1, entete = TRUE, couleur=0,nbclasses = NBSEUIL + 1)	
		
		 coldep<-colfin+1
		 colfin<-colfin+254
	}
	
	
	## Export des résultats d'extrapolation à la Masse d'eau
	 if (CONTAME=="oui") {
		 				## Export des résultats finaux
			#Calcul du nombre d'onglet si + de 254 colonnes
			NCOLRLTCONTA<-ncol(RLT_CONTAAGGME)
			nbonglet<-ceiling(NCOLRLTCONTA/254)
			coldep<-2
			colfin<-254
				
			for(i in 1:nbonglet){
				print(paste("export de MasseEau, onlet numero ",i))
				colfin<-pmin(colfin,NCOLRLTCONTA)
				RLT_CONTA_EXPORT<-RLT_CONTAAGGME[,c(1,coldep:colfin)]
				 XLSload <- loadWorkbook(XLS, create = FALSE)
				if(i==1) { nameongletconta<-"MasseEau" } 
				if(i>1) { nameongletconta<- paste0("MasseEau",i) } 
				
				NUMCOL<-(1:ncol(RLT_CONTA_EXPORT))[names(RLT_CONTA_EXPORT) %in% colorscol]
				colorXLS(XLS, createxls = FALSE, nameonglet = nameongletconta, createsheet = TRUE, RLT_CONTA_EXPORT, numcol = NUMCOL, depR =1 , depC=1, entete = TRUE, couleur=0,nbclasses = NBSEUIL + 1)	
				
				 coldep<-colfin+1
				 colfin<-colfin+254
			} 
	 
	 }

	gc()
	Sys.sleep(0.5) ## Essai pour relacher la mémoire Java 

	## Export des indices  (en deux ou trois onglets si trop de colonnes à cause d'un problème JAVA)
	 if (INDICE == "oui"){
		NCOLINDICE<-ncol(RLT_CONTAINDICE)
		nbonglet<-ceiling(NCOLINDICE/254)
		coldep<-2
		colfin<-254
		
		for(i in 1:nbonglet){
			colfin<-pmin(colfin,NCOLINDICE)
			 XLSload <- loadWorkbook(XLS, create = FALSE)
			if(i==1) { nameongletindice<-"StationIndice" } 
			if(i>1) { nameongletindice<- paste0("StationIndice",i) } 
			createSheet(XLSload, name = nameongletindice)
			 writeWorksheet(XLSload, RLT_CONTAINDICE[,c(1,coldep:colfin)], sheet = nameongletindice, startRow = 1, startCol = 1 , header = TRUE)
			 setColumnWidth(XLSload,nameongletindice,1:ncol(RLT_CONTAINDICE),-1)
			 saveWorkbook(XLSload)
			 tcl("update")		 
			 coldep<-colfin+1
			 colfin<-colfin+254
	 
	 }
	 }


	 
	 
	## Export des Frequences (en deux ou trois onglets si trop de colonnes à cause d'un problème JAVA)
	
	 if (FREQPRELEV == "oui"){
		NCOLFREQ<-ncol(RLT_CONTAFREQ)
		nbonglet<-ceiling(NCOLFREQ/254)
		coldep<-2
		colfin<-254
		
		for(i in 1:nbonglet){
			colfin<-pmin(colfin,NCOLFREQ)
			 XLSload <- loadWorkbook(XLS, create = FALSE)
			if(i==1) { nameongletfreq<-"StationFreQ" } 
			if(i>1) { nameongletfreq<- paste0("StationFreQ",i) } 
			createSheet(XLSload, name = nameongletfreq)
			 writeWorksheet(XLSload, RLT_CONTAFREQ[,c(1,coldep:colfin)], sheet = nameongletfreq, startRow = 1, startCol = 1 , header = TRUE)
			 setColumnWidth(XLSload,nameongletfreq,1:ncol(RLT_CONTAFREQ),-1)
			 saveWorkbook(XLSload)
			 tcl("update")		 
			 coldep<-colfin+1
			 colfin<-colfin+254
	 
		}
		
			#frequence des mesures quantifiée
		NCOLFREQ<-ncol(RLT_CONTAFREQQUANTI)
		nbonglet<-ceiling(NCOLFREQ/254)
		coldep<-2
		colfin<-254
		
		for(i in 1:nbonglet){
			colfin<-pmin(colfin,NCOLFREQ)
			 XLSload <- loadWorkbook(XLS, create = FALSE)
			if(i==1) { nameongletfreq<-"FreQQuanti" } 
			if(i>1) { nameongletfreq<- paste0("FreQQuanti",i) } 
			createSheet(XLSload, name = nameongletfreq)
			 writeWorksheet(XLSload, RLT_CONTAFREQQUANTI[,c(1,coldep:colfin)], sheet = nameongletfreq, startRow = 1, startCol = 1 , header = TRUE)
			 setColumnWidth(XLSload,nameongletfreq,1:ncol(RLT_CONTAFREQQUANTI),-1)
			 saveWorkbook(XLSload)
			 tcl("update")		 
			 coldep<-colfin+1
			 colfin<-colfin+254
	 
		}
		
		
	 }	
	
	
	###Export niveau de contamination
	if (exists("RECAPCONTA")){
		XLSload <- loadWorkbook(XLS, create = FALSE)
		createSheet(XLSload, name = "niveau_conta")
		writeWorksheet(XLSload, RECAPCONTA, sheet = "niveau_conta", startRow = 1, startCol = 1 , header = TRUE)
		setColumnWidth(XLSload,"niveau_conta",1:ncol(RECAPCONTA),-1)
		saveWorkbook(XLSload)
		tcl("update")
		}
	
	
	
	###Export des parametres limitants
	NCOLPLIM<-ncol(RLT_CONTA_PARAMLIMITANT)
	nbonglet<-ceiling(NCOLPLIM/254)
	coldep<-2
	colfin<-254
	
	for(i in 1:nbonglet){
		colfin<-pmin(colfin,NCOLPLIM)
		 XLSload <- loadWorkbook(XLS, create = FALSE)
		if(i==1) { nameongletfreq<-"ParamLimit" } 
		if(i>1) { nameongletfreq<- paste0("ParamLimit",i) } 
		createSheet(XLSload, name = nameongletfreq)
		 writeWorksheet(XLSload, RLT_CONTA_PARAMLIMITANT[,c(1,coldep:colfin)], sheet = nameongletfreq, startRow = 1, startCol = 1 , header = TRUE)
		 setColumnWidth(XLSload,nameongletfreq,1:ncol(RLT_CONTA_PARAMLIMITANT),-1)
		 saveWorkbook(XLSload)
		 tcl("update")		 
		 coldep<-colfin+1
		 colfin<-colfin+254
 
	}	

	###Export des LQMAX
	NCOLLQMAX<-ncol(RLT_LQMAX)
	nbonglet<-ceiling(NCOLLQMAX/254)
	coldep<-2
	colfin<-254
	
	for(i in 1:nbonglet){
		colfin<-pmin(colfin,NCOLLQMAX)
		 XLSload <- loadWorkbook(XLS, create = FALSE)
		if(i==1) { nameongletfreq<-"LQMAX" } 
		if(i>1) { nameongletfreq<- paste0("LQMAX",i) } 
		createSheet(XLSload, name = nameongletfreq)
		 writeWorksheet(XLSload, RLT_LQMAX[,c(1,coldep:colfin)], sheet = nameongletfreq, startRow = 1, startCol = 1 , header = TRUE)
		 setColumnWidth(XLSload,nameongletfreq,1:ncol(RLT_LQMAX),-1)
		 saveWorkbook(XLSload)
		 tcl("update")		 
		 coldep<-colfin+1
		 colfin<-colfin+254
 
	}	

	###Export des CAS
	NCOLCAS<-ncol(RLT_CAS)
	nbonglet<-ceiling(NCOLCAS/254)
	coldep<-2
	colfin<-254
	
	for(i in 1:nbonglet){
		colfin<-pmin(colfin,NCOLCAS)
		 XLSload <- loadWorkbook(XLS, create = FALSE)
		if(i==1) { nameongletfreq<-"CAS" } 
		if(i>1) { nameongletfreq<- paste0("CAS",i) } 
		createSheet(XLSload, name = nameongletfreq)
		 writeWorksheet(XLSload, RLT_CAS[,c(1,coldep:colfin)], sheet = nameongletfreq, startRow = 1, startCol = 1 , header = TRUE)
		 setColumnWidth(XLSload,nameongletfreq,1:ncol(RLT_CAS),-1)
		 saveWorkbook(XLSload)
		 tcl("update")		 
		 coldep<-colfin+1
		 colfin<-colfin+254
 
	}	

	# Export du tableau de contingence
	XLSload <- loadWorkbook(XLS, create = FALSE)
	createSheet(XLSload, name = "Stat_CAS")
	writeWorksheet(XLSload, CONTACASCLASS, sheet = "Stat_CAS", startRow = 1, startCol = 1 , header = TRUE)
	setColumnWidth(XLSload,"Stat_CAS",1:ncol(CONTACASCLASS),-1)
	saveWorkbook(XLSload)
	tcl("update")

	 ##Export des grille de parametrage
	PARAMETRECONTA_ETUDIE_EXPORT<-PARAMETRECONTA[PARAMETRECONTA$PARAMETRE %in% CONTAAGG$PARAMETRE,]
	if (TYPECONTA == 3 & CHOIXSEUIL=="LQMAX" ) {
		names(PARAMETRECONTA_ETUDIE_EXPORT)<-gsub("VALSEUIL","COEFF_SEUIL",names(PARAMETRECONTA_ETUDIE_EXPORT))}
	gc()
	 XLSload <- loadWorkbook(XLS, create = FALSE)
	 createSheet(XLSload, name = "GrilleConta")
	 writeWorksheet(XLSload, PARAMETRECONTA_ETUDIE_EXPORT, sheet = "GrilleConta", startRow = 1, startCol = 1 , header = TRUE)
	 setColumnWidth(XLSload,"GrilleConta",1:ncol(PARAMETRECONTA_ETUDIE_EXPORT),-1)
	 saveWorkbook(XLSload)
	 tcl("update")

XLSCONTA<-XLS

}

if(CONTA=="oui" & TYPECONTA == 4){
	date()
	

	## EXPORT EXCEL : copy le template vers un nouveau fichier Excel de réusltat	
	source(paste(pathS,"misecouleur.r",sep=""))
	# if (NBSEUIL == 3 ) {
	# NOMXLSTEMP <- paste(racine,"/TEMPLATE/XLS_template/template_conta_4classes.xls",sep="") }
	# if (NBSEUIL == 4 ) {
	# NOMXLSTEMP <- paste(racine,"/TEMPLATE/XLS_template/template_conta_5classes.xls",sep="") }
	# if (NBSEUIL == 5 ) {
	# NOMXLSTEMP <- paste(racine,"/TEMPLATE/XLS_template/template_conta_6classes.xls",sep="") }
	NOMXLSTEMP <- paste(racine,"/TEMPLATE/XLS_template/template_Sompest.xls",sep="") 
	XLS<-paste(CH_OUTPUT,NAMEOUTPUT,"SOMME_PEST.xls",sep="")
	if (file.exists(XLS)) {	XLS<-paste(CH_OUTPUT,NAMEOUTPUT,"SOMME_PEST_",SEEE_DEBformat,".xls",sep="")}
	file.copy(NOMXLSTEMP, XLS, overwrite = TRUE)

	####Export des paramètres (métadonnées)
	if(!exists("MISECOULEUR")) {MISECOULEUR<-"non"}
	if(!exists("NAMEFILEPCH")) {NAMEFILEPCH<-" - "}
	if(!exists("FREQOKCONTA")) {FREQOKCONTA<-" - "}
	if(!exists("NBANFREQ")) {NBANFREQ<-" - "}
	if(!exists("nbstatmanQ")) {nbstatmanQ<-"non"}
	if(!exists("ndoublchim")) {ndoublchim<-" - "}
	if(!exists("periodeConta")) {periodeConta<-" - "}
	if(!exists("NAMEFILESTAT")) {NAMEFILESTAT<-" - "}
	if(!exists("NBLQMANQ_CHIM")) {NBLQMANQ_CHIM<-" - "}
	if(!exists("LQPROGRESSIVE")) {LQPROGRESSIVE<-" - "}



	### calcul le nombre de stations
	nbstations<-as.character(nrow(RESULTAT_SUMPEST_PERIODE))

	## DFmetaCHIM est un data.fame qui rassemble les options métadonnées
	LIST_OPTIONS_TEMPLATE_chim<-read.csv2(paste(pathsite,"template_options_sompest.csv",sep=""))
	DFmetaCHIM<-data.frame(c(numversion, MODULE,LOGIN,BASSIN,format(Sys.time() , "%A %d %m %Y à %Hh%M"),
	periodeConta,
	nbstations,
	NAMEFILEPCH, NAMEFILESTAT, FRACTIONOKCONTA,  UNITEOKCONTA,FREQOKCONTA, NBANFREQ,ndoublchim, LQPROGRESSIVE,
	COMMENTAIRE))
	DFmetaCHIM<-na.omit(DFmetaCHIM)

	
	# Export des options dans la l'onglet Accueil
	XLSload <- loadWorkbook(XLS, create = FALSE)
	print(SEEE_DEBformat)
	
	
	setStyleAction(XLSload, XLC$"STYLE_ACTION.NONE")
	writeWorksheet(XLSload, LIST_OPTIONS_TEMPLATE_chim, sheet = "Accueil", startRow = 15, startCol = 1 , header = FALSE)

	writeWorksheet(XLSload, DFmetaCHIM, sheet = "Accueil", startRow = 15, startCol = 3 , header = FALSE)
	#setColumnWidth(XLSload,"Accueil",3,-1)
	saveWorkbook(XLSload)
	
	tcl("update")
	

	## station date somme
	XLSload <- loadWorkbook(XLS, create = FALSE)
	print(SEEE_DEBformat)
	setStyleAction(XLSload, XLC$"STYLE_ACTION.NONE")
	createSheet(XLSload, name = "StationDate")
	writeWorksheet(XLSload, RESULTAT_SUMPEST, sheet = "StationDate", startRow = 1, startCol = 1 , header = TRUE)
	setColumnWidth(XLSload,"StationDate",1:ncol(RESULTAT_SUMPEST) ,-1)
	saveWorkbook(XLSload)

	tcl("update")
		
	
	## Somme pesticides
	XLSload <- loadWorkbook(XLS, create = FALSE)
	print(SEEE_DEBformat)
	setStyleAction(XLSload, XLC$"STYLE_ACTION.NONE")
	createSheet(XLSload, name = "StationPeriode")

	writeWorksheet(XLSload, RESULTAT_SUMPEST_PERIODE, sheet = "StationPeriode", startRow = 1, startCol = 1 , header = TRUE)

	setColumnWidth(XLSload,"StationPeriode",1:ncol(RESULTAT_SUMPEST_PERIODE) ,-1)
	saveWorkbook(XLSload)
	tcl("update")
		
		
	
XLSCONTA<-XLS

}



tkconfigure(tklab_export,textvariable=tclVar("    - Export des résultats : terminé"), foreground=colorduS)
tcl("update")

###########################
######## VALORISATION######
if ( BASSININI == "AESN" ) {
	val_lab<-tklabel2(tt,text="    - Export des graphiques : en cours")
	tkgrid(val_lab, sticky="w")
	tcl("update")
	source(paste0(pathS,"VALORISATION/valorisation.r"))
	tkconfigure(val_lab,textvariable=tclVar("    - Export des graphiques : terminé"), foreground=colorduS)
	tcl("update")
}

	
#################
## CARTO PCH
###############
# fol<-""  # fol est le nom du fichier OpenLayers
# if (SEEEPCH=="oui" & substr(BASSIN,1,2) == "AE"  ) {
	# source(paste(pathsite,"CARTO_PCH.r",sep="") , echo = TRUE, print.eval = TRUE)
# }

###########################
######## FINALISATION######
###########################

SEEE_FIN<-Sys.time()
DUREE_CALC<-round(as.numeric(difftime(SEEE_FIN, SEEE_DEB, units = "mins")),2)
DUREE_CALC_min<-trunc(DUREE_CALC)
DUREE_CALC_sec<-round((DUREE_CALC-DUREE_CALC_min)*60)


print(paste("Début traitement : ",SEEE_DEB,sep=""))
print(paste("Fin traitement : ",SEEE_FIN,sep=""))

## Export en Archive Rdata
	exptarchive_lab<-tklabel2(tt,text="    - Archivage final : en cours")
	tkgrid(exptarchive_lab, sticky="w")
	tcl("update")
	print(paste("début Archivage : ",Sys.time(),sep=""))
	
	listobject_to_save<-c("listobject_to_save",ls()[ls() %in% c("DFmetaECOLO","DFmetaCHIM","LIST_OPTIONS_TEMPLATE_ecolo","LIST_OPTIONS_TEMPLATE_chim","RLT_FINALSTATION", "RLT_FINALME",
									"RLT_FINALINDICE", "RLT_FINALFREQ", "RLT_STATSTATION", "RLT_STATME,RLT_CHIMFREQ",
									"RLT_CHIMCMAMA", "RLT_CHIMSTAT", "RLT_CHIMETATME", "RLT_CHIMSTATION",
									"RLT_CONTA","RLT_CONTAINDICE","RESULTAT_SUMPEST_PERIODE","RESULTAT_SUMPEST"
									)])
	
	
	RDATAFINAL<-paste(CH_OUTPUT,NAMEOUTPUT,SEEE_DEBformat,".RData",sep="")
	try(save( list= listobject_to_save,   file = RDATAFINAL, eval.promises = FALSE))

	try(save.image(file= paste(racine,"/LOGR/SAVE_RDATA/imageFIN.Rdata",sep="")))
	
	print(paste("fin Archivage : ",Sys.time(),sep=""))
	
	tkconfigure(exptarchive_lab,textvariable=tclVar("    - Archivage final : terminé"), foreground=colorduS)
	tcl("update")
