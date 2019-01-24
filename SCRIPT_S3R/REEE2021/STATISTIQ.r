##############################################
## SEEE - COURS D'EAU
## Application de l'arrêté de janvier 2010
##############################################


#########################
##STATISTIQUE : STATION - etat ecologique
#########################
if (SEEEBIO=="oui" | SEEEPCH=="oui" | SEEEPS=="oui") {

	TABLESTAT_STATION<-RLT_FINALSTATION[,toupper(c("STATION",ETATECOLO_NAMES,ETATECOLOHPS_NAMES,ETATBIO_NAMES,ETATPCH_NAMES,ETATPS_NAMES,PARAMETREBIO_NAMES,PARAMETREPCHELT_NAMES,PARAMETREPOLLUANT_NAMES,PARAMETREPCH_NAMES,PARAMETREPS_NAMES,ASSOUPLI_NAMES,MODELISE_NAMES))]
	# on transforme les oui/non en valeur numérique pour les faire correspondre aux 6eme et 7eme colonne (meme principe que pour les classes d'état)
	if (length(ASSOUPLI_NAMES) > 0){
		TABLESTAT_STATION$ASSOUPLI[TABLESTAT_STATION$ASSOUPLI == "oui"]<-6 
		TABLESTAT_STATION$ASSOUPLI[TABLESTAT_STATION$ASSOUPLI == "non"]<-7
	}

	if (length(MODELISE_NAMES) > 0){
		TABLESTAT_STATION$MODELISE[TABLESTAT_STATION$MODELISE == "oui"]<-6 
		TABLESTAT_STATION$MODELISE[TABLESTAT_STATION$MODELISE == "non"]<-7
	}

	if ((length(ASSOUPLI_NAMES) > 0) | (length(MODELISE_NAMES) > 0)){	
		RLT_STATSTATION<-data.frame(matrix(0,nrow=ncol(TABLESTAT_STATION)-1, ncol = 9)) # création tableau résultat final (selon nb station)
		RLT_STATSTATIONP<-data.frame(matrix(0,nrow=ncol(TABLESTAT_STATION)-1, ncol = 9))
		names(RLT_STATSTATION)<-c("TABSTATION","NONQUALIFIE","TRESBON","BON","MOYEN","MEDIOCRE","MAUVAIS", "OUI", "NON")
		names(RLT_STATSTATIONP)<-c("TABSTATION","%NONQUALIFIE","%TRESBON","%BON","%MOYEN","%MEDIOCRE","%MAUVAIS","%OUI","%NON")
	} else {
		RLT_STATSTATION<-data.frame(matrix(0,nrow=ncol(TABLESTAT_STATION)-1, ncol = 7)) # création tableau résultat final (selon nb station)
		RLT_STATSTATIONP<-data.frame(matrix(0,nrow=ncol(TABLESTAT_STATION)-1, ncol = 7))
		names(RLT_STATSTATION)<-c("TABSTATION","NONQUALIFIE","TRESBON","BON","MOYEN","MEDIOCRE","MAUVAIS")
		names(RLT_STATSTATIONP)<-c("TABSTATION","%NONQUALIFIE","%TRESBON","%BON","%MOYEN","%MEDIOCRE","%MAUVAIS")
	}

	for (i in 2:(ncol(TABLESTAT_STATION))) {
		TABLEAGG<-table(TABLESTAT_STATION[,i])
		RLT_STATSTATION[i-1,1]<-colnames(TABLESTAT_STATION)[i] # création de la 1er colonne du tableau (nom parametre)
		RLT_STATSTATIONP[i-1,1]<-colnames(TABLESTAT_STATION)[i]
		RLT_STATSTATION[i-1,as.numeric(row.names(TABLEAGG))+2]<-TABLEAGG # intégration des résultats dans les colonnes adéquates
		RLT_STATSTATIONP[i-1,as.numeric(row.names(TABLEAGG))+2]<-round(TABLEAGG*100/length(TABLESTAT_STATION[!is.na(TABLESTAT_STATION[,i]),i]),1)
	}

	#############################
	## STATISTIQUE : MASSE D'EAU
	#############################
	if (SEEEBIO=="oui" & SEEEPCH=="oui" & SEEEECOLOME=="oui" & CEPE=="CE") {
		TABLESTAT_ME<-RLT_FINALME[,toupper(c("EUCD",ETATECOLOME_NAMES,ETATECOLOHPSME_NAMES,ETATBIOME_NAMES,ETATPCHME_NAMES,ETATPSME_NAMES,ETATECOLO_NAMES,ETATECOLOHPS_NAMES,ETATBIO_NAMES,ETATPCH_NAMES,ETATPS_NAMES,PARAMETREBIO_NAMES,PARAMETREPCHELT_NAMES,PARAMETREPOLLUANT_NAMES,PARAMETREPCH_NAMES,PARAMETREPS_NAMES,ASSOUPLI_NAMES,MODELISE_NAMES,NIVEAU_NAMES))]	
		# on transforme les oui/non en valeur numérique pour les faire correspondre aux 6eme à 10eme colonne (meme principe que pour les classes d'état)
		if (length(ASSOUPLI_NAMES) > 0){
			TABLESTAT_ME$ASSOUPLI[TABLESTAT_ME$ASSOUPLI == "oui"]<-6 
			TABLESTAT_ME$ASSOUPLI[TABLESTAT_ME$ASSOUPLI == "non"]<-7
		}
		if (length(MODELISE_NAMES) > 0){
			TABLESTAT_ME$MODELISE[TABLESTAT_ME$MODELISE == "oui"]<-6 
			TABLESTAT_ME$MODELISE[TABLESTAT_ME$MODELISE == "non"]<-7
		}
		if (length(NIVEAU_NAMES) > 0){
			TABLESTAT_ME$NIVEAU[TABLESTAT_ME$NIVEAU==3]<-8 
			TABLESTAT_ME$NIVEAU[TABLESTAT_ME$NIVEAU==2]<-9
			TABLESTAT_ME$NIVEAU[TABLESTAT_ME$NIVEAU==1]<-10
		}

		RLT_STATME<-data.frame(matrix(0,nrow=ncol(TABLESTAT_ME)-1, ncol = 12))
		RLT_STATMEP<-data.frame(matrix(0,nrow=ncol(TABLESTAT_ME)-1, ncol = 12))
		names(RLT_STATME)<-c("TABME","NONQUALIFIE","TRESBON","BON","MOYEN","MEDIOCRE","MAUVAIS","OUI","NON","NIVELEVE", "NIVMOYEN", "NIVFAIBLE")
		names(RLT_STATMEP)<-c("TABME","%NONQUALIFIE","%TRESBON","%BON","%MOYEN","%MEDIOCRE","%MAUVAIS","%OUI","%NON","%NIVELEVE", "%NIVMOYEN", "%NIVFAIBLE")

		for (i in 2:ncol(TABLESTAT_ME)) {
			TABLEAGG<-table(TABLESTAT_ME[,i])
			RLT_STATME[i-1,1]<-colnames(TABLESTAT_ME)[i]
			RLT_STATMEP[i-1,1]<-colnames(TABLESTAT_ME)[i]
			RLT_STATME[i-1,as.numeric(row.names(TABLEAGG))+2]<-TABLEAGG
			RLT_STATMEP[i-1,as.numeric(row.names(TABLEAGG))+2]<-round(TABLEAGG*100/length(TABLESTAT_ME[!is.na(TABLESTAT_ME[,i]),i]),1)
		}
	}
	
##########################################
# CONSTRUCTION DES TABLEAUX - STATISTIQUE
##########################################
	RLT_STATSTATION$NB_STAT_TRAITEES<-rowSums(RLT_STATSTATION[,2:ncol(RLT_STATSTATION)]) # les colonnes 2à9 sont NONQUALIFIE TRESBON BON MOYEN MEDIOCRE MAUVAIS
	RLT_STATSTATION<-merge(RLT_STATSTATION,RLT_STATSTATIONP,by="TABSTATION", all.x=TRUE, sort=FALSE)
	rm(RLT_STATSTATIONP)
		
	if (SEEEECOLOME=="oui" & CEPE=="CE") {
		RLT_STATME$NB_ME_TRAITEES<-rowSums(RLT_STATME[,2:12])
		RLT_STATME<-merge(RLT_STATME,RLT_STATMEP,by="TABME", all.x=TRUE, sort=FALSE)
		RLT_STATME<-RLT_STATME[!(RLT_STATME$TABME %in% c("ETATECOLO","ETATECOLOHPS","ETATBIO","ETATPCH","ETATPS") ) ,]
		rm(RLT_STATMEP)
	}	
	
}



##########################################################################

##################################
##CHIMIE : Statistique station
## réalisé le 25/04/2016
##################################

### CHIMIE  : STATION

if (SEEECHIM=="oui") {

	colonnesel<-names(RLT_CHIMSTATION) %in%  c("STATION", "ETATCHIM" , "ETATCHIM_SSHAPUBI","METAUX", "PEST" , "POLIND" , "AUTREPOL"  , toupper(PARAGROUPCHIM$PARAGROUPLIBCOURT)  )  
	TABLESTATCHIM_STATION<-RLT_CHIMSTATION[,colonnesel]

		RLT_STATCHIMSTATION<-data.frame(matrix(0,nrow=ncol(TABLESTATCHIM_STATION)-1, ncol = 4)) # création tableau résultat final (selon nb station)
		RLT_STATCHIMSTATIONP<-data.frame(matrix(0,nrow=ncol(TABLESTATCHIM_STATION)-1, ncol = 4))
		names(RLT_STATCHIMSTATION)<-c("TABSTATION","NONQUALIFIE","BON","MAUVAIS")
		names(RLT_STATCHIMSTATIONP)<-c("TABSTATION","%NONQUALIFIE","%BON","%MAUVAIS")
	

	for (i in 2:(ncol(TABLESTATCHIM_STATION))) {
		TABLEAGG<-table(TABLESTATCHIM_STATION[,i])
		RLT_STATCHIMSTATION[i-1,1]<-colnames(TABLESTATCHIM_STATION)[i] # création de la 1er colonne du tableau (nom parametre)
		RLT_STATCHIMSTATIONP[i-1,1]<-colnames(TABLESTATCHIM_STATION)[i]
		RLT_STATCHIMSTATION[i-1,as.numeric(row.names(TABLEAGG))+2]<-TABLEAGG # intégration des résultats dans les colonnes adéquates
		RLT_STATCHIMSTATIONP[i-1,as.numeric(row.names(TABLEAGG))+2]<-round(TABLEAGG*100/length(TABLESTATCHIM_STATION[!is.na(TABLESTATCHIM_STATION[,i]),i]),1)
	}


	##########################################
	# CONSTRUCTION DES TABLEAUX - STATISTIQUE
	##########################################
		RLT_STATCHIMSTATION$NB_STAT_TRAITEES<-rowSums(RLT_STATCHIMSTATION[,2:ncol(RLT_STATCHIMSTATION)]) # les colonnes 2à9 sont NONQUALIFIE TRESBON BON MOYEN MEDIOCRE MAUVAIS
		RLT_STATCHIMSTATION<-merge(RLT_STATCHIMSTATION,RLT_STATCHIMSTATIONP,by="TABSTATION", all.x=TRUE, sort=FALSE)
		rm(RLT_STATCHIMSTATIONP)
			
			}

			
	### CHIMIE  : MASSE D'eAU


if (SEEECHIM=="oui" & SEEECHIMME=="oui" ) {

	colonnesel<-names(RLT_CHIMETATME) %in%  c("EUCD", "ETATCHIMME" , "ETATCHIMME_SSHAPUBI","METAUX", "PEST" , "POLIND" , "AUTREPOL"  , toupper(PARAGROUPCHIM$PARAGROUPLIBCOURT)  )  
	TABLESTATCHIM_ME<-RLT_CHIMETATME[,colonnesel]

		RLT_STATCHIMME<-data.frame(matrix(0,nrow=ncol(TABLESTATCHIM_ME)-1, ncol = 4)) # création tableau résultat final (selon nb ME)
		RLT_STATCHIMMEP<-data.frame(matrix(0,nrow=ncol(TABLESTATCHIM_ME)-1, ncol = 4))
		names(RLT_STATCHIMME)<-c("TABME","NONQUALIFIE","BON","MAUVAIS")
		names(RLT_STATCHIMMEP)<-c("TABME","%NONQUALIFIE","%BON","%MAUVAIS")
	

	for (i in 2:(ncol(TABLESTATCHIM_ME))) {
		TABLEAGG<-table(TABLESTATCHIM_ME[,i])
		RLT_STATCHIMME[i-1,1]<-colnames(TABLESTATCHIM_ME)[i] # création de la 1er colonne du tableau (nom parametre)
		RLT_STATCHIMMEP[i-1,1]<-colnames(TABLESTATCHIM_ME)[i]
		RLT_STATCHIMME[i-1,as.numeric(row.names(TABLEAGG))+2]<-TABLEAGG # intégration des résultats dans les colonnes adéquates
		RLT_STATCHIMMEP[i-1,as.numeric(row.names(TABLEAGG))+2]<-round(TABLEAGG*100/length(TABLESTATCHIM_ME[!is.na(TABLESTATCHIM_ME[,i]),i]),1)
	}


	##########################################
	# CONSTRUCTION DES TABLEAUX - STATISTIQUE
	##########################################
		RLT_STATCHIMME$NB_STAT_TRAITEES<-rowSums(RLT_STATCHIMME[,2:ncol(RLT_STATCHIMME)]) # les colonnes 2à9 sont NONQUALIFIE BON  MAUVAIS
		RLT_STATCHIMME<-merge(RLT_STATCHIMME,RLT_STATCHIMMEP,by="TABME", all.x=TRUE, sort=FALSE)
		rm(RLT_STATCHIMMEP)
			
			}	
	

###########################################
## ETAT CHIMIQUE : STATISTIQUE POUR RAPPORTAGE
###########################################
if (SEEECHIM=="oui") {
# Tableau statistique pour rapportage Europe : % par famille
	NBPARAGLOBAL<-aggregate(PARAGROUP ~ FAMILLE, data = PARAGROUPCHIM , length)
	names(NBPARAGLOBAL)[2]<-"NBTOTAL"
	CHIMCMAMA<-merge(CHIMCMAMA,PARAGROUPCHIM[,c("PARAGROUP","FAMILLE")],by="PARAGROUP")
	TEMPOCHIMSTAT<-aggregate(PARAGROUP ~ STATION + FAMILLE + CLASSEETAT, data = CHIMCMAMA , length)
	names(TEMPOCHIMSTAT)[4]<-"NBPARA"
	
	# identification du nb de parametre absent par famille et import dans table résultat pour affichage dans tableau
	TEMPOCHIMABS<-aggregate(PARAGROUP ~ STATION + FAMILLE , data = CHIMCMAMA , length)
	names(TEMPOCHIMABS)[3]<-"NBDISPO"
	TEMPOCHIMABS<-merge(TEMPOCHIMABS,NBPARAGLOBAL,by="FAMILLE")
	TEMPOCHIMABS$NBPARA<-TEMPOCHIMABS$NBTOTAL-TEMPOCHIMABS$NBDISPO
	TEMPOCHIMABS$CLASSEETAT<-9
	TEMPOCHIMABS<-TEMPOCHIMABS[,c("STATION", "FAMILLE", "CLASSEETAT", "NBPARA")]
	TEMPOCHIMSTAT<-rbind(TEMPOCHIMSTAT,TEMPOCHIMABS)
	
	# calcul ratio
	TEMPOCHIMSTAT<-merge(TEMPOCHIMSTAT,NBPARAGLOBAL,by="FAMILLE")
	TEMPOCHIMSTAT$RATIO<-round((TEMPOCHIMSTAT$NBPARA/TEMPOCHIMSTAT$NBTOTAL)*100,0)
	TEMPOCHIMSTAT$ID<-paste(TEMPOCHIMSTAT$STATION,TEMPOCHIMSTAT$CLASSEETAT,sep="_")
	
	#Elaboration de toutes les combinaisons station / classe etat
	RLT_CHIMSTAT<-data.frame(unique(TEMPOCHIMSTAT$STATION))
	names(RLT_CHIMSTAT)[1]<-"STATION"
	TEMPCLASS<-data.frame(unique(TEMPOCHIMSTAT$CLASSEETAT))
	names(TEMPCLASS)[1]<-"CLASSEETAT"
	RLT_CHIMSTAT<-merge(RLT_CHIMSTAT,TEMPCLASS)
	RLT_CHIMSTAT$ID<-paste(RLT_CHIMSTAT$STATION,RLT_CHIMSTAT$CLASSEETAT,sep="_")
	
	#mise en forme des colonnes
	for (i in  1:nrow(TABLEFAMILLE)  ) {
		TEMPP<-TEMPOCHIMSTAT[TEMPOCHIMSTAT$FAMILLE==TABLEFAMILLE$FAMILLE[i],c("ID", "RATIO")]
		names(TEMPP)[2]<-TABLEFAMILLE$FAMILLE[i]
		RLT_CHIMSTAT<-merge(RLT_CHIMSTAT,TEMPP,by="ID", all.x=TRUE)
		rm(TEMPP)
	}
	RLT_CHIMSTAT<-RLT_CHIMSTAT[order(RLT_CHIMSTAT$STATION, RLT_CHIMSTAT$CLASSEETAT),]
		
# Tableau statistique pour rapportage Europe : % station globale
	NBPARAGLOBAL$GLOBAL<-"station"
	NBPARAGLOBALSTAT<-aggregate(NBTOTAL ~ GLOBAL , data = NBPARAGLOBAL , sum)
	
	TEMPOCHIMSTATGLOB<-aggregate(PARAGROUP ~ STATION + CLASSEETAT, data = CHIMCMAMA , length)
	names(TEMPOCHIMSTATGLOB)[3]<-"NBPARA"
	TEMPOCHIMSTATGLOBsave<-TEMPOCHIMSTATGLOB
	
	# identification du nb de parametre absent sur la station et import dans table résultat pour affichage dans tableau
	TEMPOCHIMABSSTATION<-aggregate(PARAGROUP ~ STATION , data = CHIMCMAMA , length)
	names(TEMPOCHIMABSSTATION)[2]<-"NBDISPO"
	TEMPOCHIMABSSTATION<-cbind(TEMPOCHIMABSSTATION,NBPARAGLOBALSTAT)
	TEMPOCHIMABSSTATION$NBPARA<-TEMPOCHIMABSSTATION$NBTOTAL-TEMPOCHIMABSSTATION$NBDISPO
	TEMPOCHIMABSSTATION$CLASSEETAT<-9
	TEMPOCHIMABSSTATION<-TEMPOCHIMABSSTATION[,c("STATION", "CLASSEETAT", "NBPARA")]
	TEMPOCHIMSTATGLOB<-rbind(TEMPOCHIMSTATGLOB,TEMPOCHIMABSSTATION)
	
	# calcul ratio
	TEMPOCHIMSTATGLOB<-cbind(TEMPOCHIMSTATGLOB,NBPARAGLOBALSTAT)
	TEMPOCHIMSTATGLOB$RATIO<-round((TEMPOCHIMSTATGLOB$NBPARA/TEMPOCHIMSTATGLOB$NBTOTAL)*100,0)
	TEMPOCHIMSTATGLOB$ID<-paste(TEMPOCHIMSTATGLOB$STATION,TEMPOCHIMSTATGLOB$CLASSEETAT,sep="_")
	
	#Mise en forme finale dans tableau de résultat
	RLT_CHIMSTAT<-merge(RLT_CHIMSTAT,TEMPOCHIMSTATGLOB[,c("ID","RATIO")],by="ID")
	names(RLT_CHIMSTAT)[ncol(RLT_CHIMSTAT)]<-"GLOBAL"
	RLT_CHIMSTAT<-RLT_CHIMSTAT[,c(2:ncol(RLT_CHIMSTAT))]
	RLT_CHIMSTAT<-RLT_CHIMSTAT[order(RLT_CHIMSTAT$STATION, RLT_CHIMSTAT$CLASSEETAT),]
	RLT_CHIMSTAT$LEGENDE[RLT_CHIMSTAT$CLASSEETAT==0]<-"inconnu"
	RLT_CHIMSTAT$LEGENDE[RLT_CHIMSTAT$CLASSEETAT==1]<-"bon"
	RLT_CHIMSTAT$LEGENDE[RLT_CHIMSTAT$CLASSEETAT==2]<-"mauvais"
	RLT_CHIMSTAT$LEGENDE[RLT_CHIMSTAT$CLASSEETAT==9]<-"abs data"
	
	rm(TEMPOCHIMABS,TEMPOCHIMSTAT,NBPARAGLOBAL,TEMPCLASS,TEMPOCHIMABSSTATION,TEMPOCHIMSTATGLOB)
}
