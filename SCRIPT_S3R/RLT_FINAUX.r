##############################################
## SEEE - COURS D'EAU : calcul de l'état écologique
## Application de l'arrêté de janvier 2010
##############################################


#####################################################
## MISE EN FORME DES TABLEAUX FINAUX POUR EXPORT XLS
#####################################################


### UNIQUEMENT POUR LES ETATS (SAUF CHIMIE)

############################
# CONSTRUCTION DES TABLEAUX
############################
# STATION - PCH : mise en forme du tableau
if (SEEEPCH=="oui" & CEPE == "CE") {
	PCH_XLS<-PCHFINALEXCEPT_ASSOUP[,RLT_PCHFINALEXCEPT_NAMES]
	# importation des classes d'état des PARAMETRES PCH sans tenir compte des exceptions typologiques
	PCH_XLS<-merge(PCH_XLS, RLT_PCHFINALEXCEPT[,c("STATION", PARAMETREPCH_NAMES)], by="STATION", all.x=TRUE)
	PCH_XLS_NAMES<-names(PCH_XLS)
	
	if (SEEEPEGASE=="oui") {
		###liste des stations contenu dans PEGASE
		statpeg<-unique(PEGASE_XLS$STATION)
		# importation des classes d'état des PARAMETRES PCH modélisés (PEGASE) sans tenir compte des exceptions typologiques
		PCH_XLS<-merge(PCH_XLS, PEGASE_XLS, by="STATION", all.x=TRUE)
		condstatpeg<-PCH_XLS$STATION %in% statpeg
		PCH_XLS$o2[PCH_XLS$STATION %in% statpeg]<-PCH_XLS$PEGO2[condstatpeg]
		PCH_XLS$sato2[condstatpeg]<-PCH_XLS$PEGSATO2[condstatpeg]
		PCH_XLS$dbo5[condstatpeg]<-PCH_XLS$PEGDBO5[condstatpeg]
		PCH_XLS$cod[condstatpeg]<-PCH_XLS$PEGCOD[condstatpeg]
		PCH_XLS$po43[condstatpeg]<-PCH_XLS$PEGPO43[condstatpeg]
		PCH_XLS$phos[condstatpeg]<-PCH_XLS$PEGPHOS[condstatpeg]
		PCH_XLS$nh4[condstatpeg]<-PCH_XLS$PEGNH4[condstatpeg]
		PCH_XLS$no2[condstatpeg]<-PCH_XLS$PEGNO2[condstatpeg]
		PCH_XLS$no3[condstatpeg]<-PCH_XLS$PEGNO3[condstatpeg]
		PCH_XLS$tempe[condstatpeg]<-PCH_XLS$PEGTEMPE[condstatpeg]
		PCH_XLS$phmin[condstatpeg]<-PCH_XLS$PEGPHMIN[condstatpeg]
		PCH_XLS$phmax[condstatpeg]<-PCH_XLS$PEGPHMAX[condstatpeg]
		PCH_XLS<-PCH_XLS[,PCH_XLS_NAMES]
	}
}


if (SEEEPCH=="oui" & CEPE == "PE") {
	PCH_XLS<-RLT_PCHFINAL
}



# STATION - CAS OPTION : Ajout des colonnes de résulats si  traitement de la BIO ou/et de la PCH (sans PS)
if (SEEEBIO!="oui" & SEEEPCH=="oui" ) {
	RLT_FINALSTATION<-PCH_XLS
} else if (SEEEBIO=="oui" & SEEEPCH!="oui") {
	RLT_FINALSTATION<-RLT_BIO
} else if (SEEEBIO=="oui" & SEEEPCH=="oui") {
	RLT_FINALSTATION<-merge(EECOLO[,c("STATION", ETATECOLO_NAMES, ASSOUPLI_NAMES, ETATECOLOHPS_NAMES)], PCH_XLS, by="STATION", all.x=TRUE) # all.y est intégré car les stations sont prises en compte dans le calcul de l'état écolo
	RLT_FINALSTATION<-merge(RLT_FINALSTATION, RLT_BIO, by="STATION", all.x=TRUE) # all.y -> idem que remarque précédente
}

# STATION - CAS OPTION : Ajout des résultats PS si traités
if ((SEEEBIO=="oui" | SEEEPCH=="oui") & SEEEPS=="oui") {
	RLT_FINALSTATION<-merge(RLT_FINALSTATION, RLT_POLSPE, by="STATION", all.x=TRUE) # all.y -> idem que remarque précédente
} else if (SEEEBIO=="non" & SEEEPCH=="non" & SEEEPS=="oui") {
	RLT_FINALSTATION<-RLT_POLSPE
}


##Extraire la date
ANNEE_NAMES<-as.character()

if(SEEEPCH=="oui"  & exists("DATAPCH")) {

	ANNEEPCH<-data.frame(
						STATION = sort(unique(DATAPCH$STATION)),
						ANNEEMINPCH = tapply(DATAPCH$ANNEE,DATAPCH$STATION,min),
						ANNEEMAXPCH = tapply(DATAPCH$ANNEE,DATAPCH$STATION,max)
						)

	RLT_FINALSTATION<-merge(RLT_FINALSTATION,ANNEEPCH,by="STATION",all.x=TRUE)

	ANNEE_NAMES<-c("ANNEEMINPCH","ANNEEMAXPCH")
	
					}


if(SEEEBIO=="oui" & exists("DATABIO")) {

	ANNEEBIO<-data.frame(
						STATION = sort(unique(DATABIO$STATION)),
						ANNEEMINBIO = tapply(DATABIO$ANNEE,DATABIO$STATION,min),
						ANNEEMAXBIO = tapply(DATABIO$ANNEE,DATABIO$STATION,max)
						)

	RLT_FINALSTATION<-merge(RLT_FINALSTATION,ANNEEBIO,by="STATION",all.x=TRUE)
	
	ANNEE_NAMES<-c(	ANNEE_NAMES, "ANNEEMINBIO", "ANNEEMAXBIO")
					}


if(SEEEPS=="oui" & exists("POLSPE")) {

	ANNEEPS<-data.frame(
						STATION = sort(unique(POLSPE$STATION)),
						ANNEEMINPS = tapply(POLSPE$ANNEE,POLSPE$STATION,min),
						ANNEEMAXPS = tapply(POLSPE$ANNEE,POLSPE$STATION,max)
						)

	RLT_FINALSTATION<-merge(RLT_FINALSTATION,ANNEEPS,by="STATION",all.x=TRUE)
	
	ANNEE_NAMES<-c(	ANNEE_NAMES, "ANNEEMINPS", "ANNEEMAXPS")
					}

					
					
					
########################################################
### CONSTRUCTION DES TABLEAUX : PARAMETRES DECLASSANTS
### Fonction pour sortir les parametres déclassants 
# RLT_FINAL = dataframe résultat
# ETAT = colonne de l'état à comparer 
# LIST_PARAM = List des parametres à comparer
FUNC_DECLASS<-function(RLT_FINAL, ETAT, LIST_PARAM) {
	COLETAT<-RLT_FINAL[,ETAT]
	MAT<-as.matrix(RLT_FINAL[,LIST_PARAM])
	colnames(MAT)<-LIST_PARAM
	C1<-MAT >= COLETAT & COLETAT %in% c(3,4,5) 
	INDICE<-which( C1  ,  arr.ind=TRUE)  
	MAT2<-matrix(data="",nrow = nrow(MAT), ncol=ncol(MAT))
	MAT2[INDICE] <- colnames(MAT)[INDICE[,2]]
	DECLASS<-apply(MAT2, 1, function(x) paste( x,collapse = ";"))
	A<- sum(nchar(DECLASS))+1
	
	while (A - sum(nchar(DECLASS))   > 0 ) { #boucle pour supprimer les ";" inutiles
		A<-sum(nchar(DECLASS))
		DECLASS<-gsub(";;",";",DECLASS)
		cond1<-substr(DECLASS,1,1) == ";"
		DECLASS[cond1]<-substr(DECLASS[cond1],2,nchar(DECLASS[cond1]))
		cond2<-substr(DECLASS,nchar(DECLASS),nchar(DECLASS) ) == ";"
		DECLASS[cond2]<-substr(DECLASS[cond2],1,nchar(DECLASS[cond2])-1)
		}
	return(DECLASS)
}	

FUNC_MOINSKEBON<-function(RLT_FINAL, LIST_PARAM) {
	MAT<-as.matrix(RLT_FINAL[,LIST_PARAM])
	colnames(MAT)<-LIST_PARAM
	C1<-!is.na(MAT) & MAT  %in% c(3,4,5) 
	INDICE<-which( C1  ,  arr.ind=TRUE)  
	MAT2<-matrix(data="",nrow = nrow(MAT), ncol=ncol(MAT))
	MAT2[INDICE] <- colnames(MAT)[INDICE[,2]]
	DECLASS<-apply(MAT2, 1, function(x) paste( x,collapse = ";"))
	A<- sum(nchar(DECLASS))+1
	
	while (A - sum(nchar(DECLASS))   > 0 ) { #boucle pour supprimer les ";" inutiles
		A<-sum(nchar(DECLASS))
		DECLASS<-gsub(";;",";",DECLASS)
		cond1<-substr(DECLASS,1,1) == ";"
		DECLASS[cond1]<-substr(DECLASS[cond1],2,nchar(DECLASS[cond1]))
		cond2<-substr(DECLASS,nchar(DECLASS),nchar(DECLASS) ) == ";"
		DECLASS[cond2]<-substr(DECLASS[cond2],1,nchar(DECLASS[cond2])-1)
		}
	return(DECLASS)
}

#Asuppr
# RLT_FINALSTATION<-RLT_FINALSTATION[!is.na(RLT_FINALSTATION$ETATBIO),]
# RLT_FINAL<-RLT_FINALSTATION
# LIST_PARAM<-Liste_PARAMBio
# LISTOK<-RLT_FINAL$LISTINDICBIO


FUNC_MOINSKEBON_BIO<-function(RLT_FINAL, LIST_PARAM, LISTOK) {
	MAT<-as.matrix(RLT_FINAL[,LIST_PARAM])
	colnames(MAT)<-LIST_PARAM
	C1<-!is.na(MAT) & MAT  %in% c(3,4,5) 
	INDICE<-which( C1  ,  arr.ind=TRUE)  
	MAT2<-matrix(data="",nrow = nrow(MAT), ncol=ncol(MAT))
	MAT2[INDICE] <- colnames(MAT)[INDICE[,2]]
	
	for ( i in 1:ncol(MAT2)) {
		MAT2[nchar(gsub(LIST_PARAM[i],"",LISTOK )) ==   nchar(LISTOK)  ,i]<-""
	}
	
	DECLASS<-apply(MAT2, 1, function(x) paste( x,collapse = ";"))
	A<- sum(nchar(DECLASS))+1
	
	while (A - sum(nchar(DECLASS))   > 0 ) { #boucle pour supprimer les ";" inutiles
		A<-sum(nchar(DECLASS))
		DECLASS<-gsub(";;",";",DECLASS)
		cond1<-substr(DECLASS,1,1) == ";"
		DECLASS[cond1]<-substr(DECLASS[cond1],2,nchar(DECLASS[cond1]))
		cond2<-substr(DECLASS,nchar(DECLASS),nchar(DECLASS) ) == ";"
		DECLASS[cond2]<-substr(DECLASS[cond2],1,nchar(DECLASS[cond2])-1)
		}
	return(DECLASS)
}

### Fonction pour sortir les parametres assouplis
FUNC_ASSOUP<-function(RLT_FINAL, ETAT, LIST_PARAM, COLASSOUP = "ASSOUPLI") {
	COLETAT<-RLT_FINAL[,ETAT]
	COLASSOUPLI<-RLT_FINAL[,COLASSOUP]
	MAT<-as.matrix(RLT_FINAL[,LIST_PARAM])
	colnames(MAT)<-LIST_PARAM
	C1<-MAT > COLETAT & COLASSOUPLI == "oui"
	INDICE<-which( C1  ,  arr.ind=TRUE)  
	MAT2<-matrix(data="",nrow = nrow(MAT), ncol=ncol(MAT))
	MAT2[INDICE] <- colnames(MAT)[INDICE[,2]]
	DECLASS<-apply(MAT2, 1, function(x) paste( x,collapse = ";"))
	A<- sum(nchar(DECLASS))+1
	
	while (A - sum(nchar(DECLASS))   > 0 ) {  #boucle pour supprimer les ";" inutiles
		A<-sum(nchar(DECLASS))
		DECLASS<-gsub(";;",";",DECLASS)
		cond1<-substr(DECLASS,1,1) == ";"
		DECLASS[cond1]<-substr(DECLASS[cond1],2,nchar(DECLASS[cond1]))
		cond2<-substr(DECLASS,nchar(DECLASS),nchar(DECLASS) ) == ";"
		DECLASS[cond2]<-substr(DECLASS[cond2],1,nchar(DECLASS[cond2])-1)
		}
	return(DECLASS)
}

###Fonction pour calculer le nombre de parametres (déclassants, assouplis, moins que bon)
FUNC_NBPARAM<-function(colparam = RLT_FINALSTATION$PCHDECLASS){
	temp<- unlist(lapply(gregexpr(";",colparam,TRUE),length))
	temp2<-unlist(gregexpr(";",colparam[temp == 1],TRUE)) == -1
	NBPARAM<-temp+1
	NBPARAM[temp == 1][temp2]<- 1
	NBPARAM[colparam == "" ]<-0
	return(NBPARAM)
}


## Exécution des fonctions pour détermination parametre déclassants
Liste_PARAMBio<-as.character()	
if (SEEEBIO=="oui" ) {
	Liste_PARAMBio<-names(RLT_FINALSTATION)[names(RLT_FINALSTATION) %in% PARAMETREBIO$PARALIBGROUP ]
	if (MODULE == "REEE2010" & CEPE == "PE") {Liste_PARAMBio[Liste_PARAMBio %in% c("CHLOA","IPL")]}
	NAME_ETAT<-"ETATBIO"
	RLT_FINALSTATION$BIODECLASS<-FUNC_MOINSKEBON_BIO(RLT_FINALSTATION, Liste_PARAMBio,RLT_FINALSTATION$LISTINDICBIO)
	BIODECLASS_NAMES<-c("BIODECLASS")
	
	#nb de declass
	RLT_FINALSTATION$N_BIODECLASS<-FUNC_NBPARAM(RLT_FINALSTATION$BIODECLASS)
	N_BIODECLASS_NAMES<-c("N_BIODECLASS")
}
 
Liste_PARAMPCH<-as.character()	
if (SEEEPCH=="oui") {

	##Paramètre générique de la fonction
	if (OPTIONNO3=="oui" & CEPE == "CE") {
		Liste_PARAMPCH<-PARAMETREPCH_NAMES[1:length(PARAMETREPCH_NAMES)-1] # on retire colonne no3v2 qui n'est pas à traiter avec les parametres déclassants
	} else {
		Liste_PARAMPCH<-PARAMETREPCH_NAMES
	}

	NAME_ETAT<-"ETATPCH"
	RLT_FINALSTATION$PCHDECLASS<-FUNC_MOINSKEBON(RLT_FINALSTATION,Liste_PARAMPCH)
	PCHDECLASS_NAMES<-c("PCHDECLASS")
	
	
	if (EXCEPTLOC == "oui") {
		#COD
		condcod<-RLT_FINALSTATION$STATION %in% STATION$STATION[STATION$EXCEPTLOC_COD == "oui"]
		RLT_FINALSTATION$PCHDECLASS[condcod]<-gsub("cod;","",RLT_FINALSTATION$PCHDECLASS[condcod])
		RLT_FINALSTATION$PCHDECLASS[condcod]<-gsub("cod","",RLT_FINALSTATION$PCHDECLASS[condcod])
	
		#SAT02
		condcod<-RLT_FINALSTATION$STATION %in% STATION$STATION[STATION$EXCEPTLOC_O2SATO2 == "oui"]
		RLT_FINALSTATION$PCHDECLASS[condcod]<-gsub("sato2;","",RLT_FINALSTATION$PCHDECLASS[condcod])
		RLT_FINALSTATION$PCHDECLASS[condcod]<-gsub("sato2","",RLT_FINALSTATION$PCHDECLASS[condcod])
	
		#O2
		condcod<-RLT_FINALSTATION$STATION %in% STATION$STATION[STATION$EXCEPTLOC_O2SATO2 == "oui"]
		RLT_FINALSTATION$PCHDECLASS[condcod]<-gsub("o2;","",RLT_FINALSTATION$PCHDECLASS[condcod])
		RLT_FINALSTATION$PCHDECLASS[condcod]<-gsub("o2","",RLT_FINALSTATION$PCHDECLASS[condcod])
		
	}
	
	
	#nb de declass
	RLT_FINALSTATION$N_PCHDECLASS<-FUNC_NBPARAM(RLT_FINALSTATION$PCHDECLASS)
	N_PCHDECLASS_NAMES<-c("N_PCHDECLASS")
}
 

if (SEEEBIO=="oui" & SEEEPCH=="oui" & CEPE == "CE") {
	NAME_ETAT<-"ETATPCH"
	RLT_FINALSTATION$PCHASSOUP<-FUNC_ASSOUP(RLT_FINALSTATION,NAME_ETAT, Liste_PARAMPCH, "ASSOUPLI")
	PCHDECLASSASSOUP_NAMES<-c("PCHASSOUP")	
	
	#nb de declass assoup
	RLT_FINALSTATION$N_PCHASSOUP<-FUNC_NBPARAM(RLT_FINALSTATION$PCHASSOUP)
	N_PCHASSOUP_NAMES<-c("N_PCHASSOUP")
	
	}

Liste_PARAMPS<-as.character()	
if (SEEEPS=="oui") {
	NAME_ETAT<-"ETATPS"
	Liste_PARAMPS<-PARAMETREPS_NAMES
	RLT_FINALSTATION$PSDECLASS<-FUNC_MOINSKEBON(RLT_FINALSTATION,Liste_PARAMPS)
	PSDECLASS_NAMES<-c("PSDECLASS")	
	
	#nb de declass
	RLT_FINALSTATION$N_PSDECLASS<-FUNC_NBPARAM(RLT_FINALSTATION$PSDECLASS)
	N_PSDECLASS_NAMES<-c("N_PSDECLASS")
}


if (SEEEECOLO=="oui") {
	ListParamdeclassecolo<-c(Liste_PARAMBio,Liste_PARAMPCH,Liste_PARAMPS)
	RLT_FINALSTATION$ECODECLASS<-FUNC_MOINSKEBON(RLT_FINALSTATION,ListParamdeclassecolo)
	ECOLODECLASS_NAMES<-c("ECODECLASS")	
	
	
		
	if (EXCEPTLOC == "oui") {
		#COD
		condcod<-RLT_FINALSTATION$STATION %in% STATION$STATION[STATION$EXCEPTLOC_COD == "oui"]
		RLT_FINALSTATION$ECODECLASS[condcod]<-gsub("cod;","",RLT_FINALSTATION$ECODECLASS[condcod])
		RLT_FINALSTATION$ECODECLASS[condcod]<-gsub("cod","",RLT_FINALSTATION$ECODECLASS[condcod])
	
		#SAT02
		condcod<-RLT_FINALSTATION$STATION %in% STATION$STATION[STATION$EXCEPTLOC_O2SATO2 == "oui"]
		RLT_FINALSTATION$ECODECLASS[condcod]<-gsub("sato2;","",RLT_FINALSTATION$ECODECLASS[condcod])
		RLT_FINALSTATION$ECODECLASS[condcod]<-gsub("sato2","",RLT_FINALSTATION$ECODECLASS[condcod])
	
		#O2
		condcod<-RLT_FINALSTATION$STATION %in% STATION$STATION[STATION$EXCEPTLOC_O2SATO2 == "oui"]
		RLT_FINALSTATION$ECODECLASS[condcod]<-gsub("o2;","",RLT_FINALSTATION$ECODECLASS[condcod])
		RLT_FINALSTATION$ECODECLASS[condcod]<-gsub("o2","",RLT_FINALSTATION$ECODECLASS[condcod])
		
	}
	
	
	#nb de declass
	RLT_FINALSTATION$N_ECODECLASS<-FUNC_NBPARAM(RLT_FINALSTATION$ECODECLASS)
	N_ECODECLASS_NAMES<-c("N_ECODECLASS")
	
	
	##?On implémente une ancienne règle pour lister les parametres déclassants qui est le ou les parametres 
	#qui sont égales ou inférieur à l'état
	RLT_FINALSTATION$ECO_ORIGINDECLASS<-FUNC_DECLASS(RLT_FINALSTATION,"ETATECOLO", ListParamdeclassecolo)
	ECOLOORIGINDECLASS_NAMES<-c("ECO_ORIGINDECLASS")	
	
		if (EXCEPTLOC == "oui") {
		#COD
		condcod<-RLT_FINALSTATION$STATION %in% STATION$STATION[STATION$EXCEPTLOC_COD == "oui"]
		RLT_FINALSTATION$ECO_ORIGINDECLASS[condcod]<-gsub("cod;","",RLT_FINALSTATION$ECO_ORIGINDECLASS[condcod])
		RLT_FINALSTATION$ECO_ORIGINDECLASS[condcod]<-gsub("cod","",RLT_FINALSTATION$ECO_ORIGINDECLASS[condcod])
	
		#SAT02
		condcod<-RLT_FINALSTATION$STATION %in% STATION$STATION[STATION$EXCEPTLOC_O2SATO2 == "oui"]
		RLT_FINALSTATION$ECO_ORIGINDECLASS[condcod]<-gsub("sato2;","",RLT_FINALSTATION$ECO_ORIGINDECLASS[condcod])
		RLT_FINALSTATION$ECO_ORIGINDECLASS[condcod]<-gsub("sato2","",RLT_FINALSTATION$ECO_ORIGINDECLASS[condcod])
	
		#O2
		condcod<-RLT_FINALSTATION$STATION %in% STATION$STATION[STATION$EXCEPTLOC_O2SATO2 == "oui"]
		RLT_FINALSTATION$ECO_ORIGINDECLASS[condcod]<-gsub("o2;","",RLT_FINALSTATION$ECO_ORIGINDECLASS[condcod])
		RLT_FINALSTATION$ECO_ORIGINDECLASS[condcod]<-gsub("o2","",RLT_FINALSTATION$ECO_ORIGINDECLASS[condcod])
		
	}
	
	
	#nb de declass
	RLT_FINALSTATION$N_ECO_ORIGINDECLASS <-FUNC_NBPARAM(RLT_FINALSTATION$ECO_ORIGINDECLASS)
	N_ECOLOORIGINDECLASS_NAMES<-c("N_ECO_ORIGINDECLASS")
	
}


##########################################################
# CONSTRUCTION DES TABLEAUX : AJOUT DES COLONNES SUPPLEMENTAIRES POUR TRACABILITE & ORDINATION DES COLONNES	

if ( BASSIN == "AESN") {
ETATECOLOHPS_NAMES<-as.character()
STATION_NAMES<-STATION_NAMES[!(STATION_NAMES %in%  c("FONDGEO_ARSENIC" ,"FONDGEO_CHROME" , "FONDGEO_CUIVRE" , "FONDGEO_ZINC"  )   )]
}


	
# STATION ETAT ECOLO : Traitement commun à l'ensemble des exports, quelles que soient les options (if) choisies
if (SEEEBIO=="oui" | SEEEPCH=="oui" | SEEEPS=="oui") {
	RLT_FINALSTATION<-merge(RLT_FINALSTATION, STATION, by="STATION", all.x=TRUE) # all.y -> idem que remarque précédente
	NAMES_ATTENDUS_RLT<-c("STATION",ETATECOLO_NAMES,ETATECOLOHPS_NAMES,ETATBIO_NAMES,ETATPCH_NAMES,ETATPS_NAMES,PARAMETREBIO_NAMES, 
										PARAMETREPCHELT_NAMES,
										PARAMETREPOLLUANT_NAMES,PARAMETREPCH_NAMES,PARAMETREPS_NAMES,ASSOUPLI_NAMES,MODELISE_NAMES,
										NBBIO_NAMES,BIODECLASS_NAMES,N_BIODECLASS_NAMES,PCHDECLASS_NAMES, N_PCHDECLASS_NAMES, 
										PCHDECLASSASSOUP_NAMES, N_PCHASSOUP_NAMES, 
										PSDECLASS_NAMES, N_PSDECLASS_NAMES, ECOLODECLASS_NAMES,N_ECODECLASS_NAMES,ECOLOORIGINDECLASS_NAMES,
										N_ECOLOORIGINDECLASS_NAMES,ANNEE_NAMES,
										STATION_NAMES)
	NAMES_ATTENDUS_RLT<-unique(NAMES_ATTENDUS_RLT[NAMES_ATTENDUS_RLT %in% names(RLT_FINALSTATION)])
	RLT_FINALSTATION<-RLT_FINALSTATION[,NAMES_ATTENDUS_RLT]
	names(RLT_FINALSTATION)<-toupper(names(RLT_FINALSTATION))
	RLT_FINALSTATION<-RLT_FINALSTATION[order(RLT_FINALSTATION$STATION),] #tri du tabeau selon code STATION
}

#Rajout STATIONTXT si station au format SANDRE
RLT_FINALSTATION_NAMES<-names(RLT_FINALSTATION)
	RLT_FINALSTATION$STATIONTXT<-as.character(RLT_FINALSTATION$STATION)
	RLT_FINALSTATION$STATIONTXT[nchar(as.character(RLT_FINALSTATION$STATION)) == 7 & !is.na(RLT_FINALSTATION$STATION)]<-paste("0",RLT_FINALSTATION$STATION[nchar(as.character(RLT_FINALSTATION$STATION)) == 7 & !is.na(RLT_FINALSTATION$STATION)],sep="")
	RLT_FINALSTATION<-RLT_FINALSTATION[,c("STATIONTXT",RLT_FINALSTATION_NAMES)]
	RLT_FINALSTATION_NAMES<-names(RLT_FINALSTATION)

if (MODULE == "REEE2021"){

	# Cycle 3 (modif le 04/05/15) : modif nom colonne pour cohérence avec data bio du 3eme cycle
	# modif 1 dans vecteur de names
	RLT_FINALSTATION_NAMES[RLT_FINALSTATION_NAMES == "IBG"]<-"I2M2"
	RLT_FINALSTATION_NAMES[RLT_FINALSTATION_NAMES == "IPR"]<-"IPRP"
	PARAMETREBIO_NAMES[PARAMETREBIO_NAMES == "IBG"]<-"I2M2" # utile pour STATISTIQ.r
	PARAMETREBIO_NAMES[PARAMETREBIO_NAMES == "IPR"]<-"IPRP" # utile pour STATISTIQ.r

	# modif 2 dans dataframe
	names(RLT_FINALSTATION)[names(RLT_FINALSTATION) == "IBG"]<-"I2M2"
	names(RLT_FINALSTATION)[names(RLT_FINALSTATION) == "IPR"]<-"IPRP"

	if (SEEEBIO=="oui") {
		RLT_FINALSTATION$LISTINDICBIO<-gsub("IBG","I2M2",RLT_FINALSTATION$LISTINDICBIO)
		RLT_FINALSTATION$LISTINDICBIO<-gsub("I2M2A","IBGA",RLT_FINALSTATION$LISTINDICBIO)
		RLT_FINALSTATION$LISTINDICBIO<-gsub("IPR","IPRP",RLT_FINALSTATION$LISTINDICBIO)
	}


}

# MASSE D'EAU ETAT ECOLO : tri des colonnes du tableau final
if (SEEEECOLOME=="oui" & CEPE == "CE") {
	RLT_FINALSTATION_NAMES<-names(RLT_FINALSTATION[,!(names(RLT_FINALSTATION) %in% c("EUCD"))]) # retrait du champ EUCD pour le tableau final à la ME, sinon doublon de colonne
	RLT_FINALME<-merge(EECOLOME[,c("STATION",ETATECOLOME_NAMES,NIVEAU_NAMES,CAS_NAMES,ETATBIOME_NAMES,ETATPCHME_NAMES,ETATPSME_NAMES,ETATECOLOHPSME_NAMES)], RLT_FINALSTATION, by="STATION", all.x=TRUE)
	
	#trier sur code masse d'eau , état écologique, niveau de confiance (décroissant), cas (par ordre croissant)
	#Puis ne retenir qu'une ligne par masse d'eau (on ne souhaite pas de doublons sur les masses d'eau)
	nrow(RLT_FINALME)
	RLT_FINALME<-RLT_FINALME[order(RLT_FINALME$NIVEAU ,decreasing = TRUE), ]
	RLT_FINALME<-RLT_FINALME[order(RLT_FINALME$EUCD, RLT_FINALME$ETATECOLOME, RLT_FINALME$CAS), ]
	RLT_FINALME<-RLT_FINALME[!duplicated(RLT_FINALME[ ,c("EUCD","ETATECOLOME" )]),]
	nrow(RLT_FINALME)

	RLT_FINALME<-merge(RLT_FINALME,MASSEDEAU[,subset(names(MASSEDEAU), !(names(MASSEDEAU) %in% "COMITER"))], by="EUCD", all.x=TRUE)
	RLT_FINALME<-RLT_FINALME[,c("NAME","EUCD",ETATECOLOME_NAMES,NIVEAU_NAMES,CAS_NAMES,ETATECOLOHPSME_NAMES,ETATBIOME_NAMES,ETATPCHME_NAMES,ETATPSME_NAMES,RLT_FINALSTATION_NAMES,MASSEDEAU_NAMES)]
	names(RLT_FINALME)<-toupper(names(RLT_FINALME))
	RLT_FINALME<-RLT_FINALME[order(RLT_FINALME$EUCD, RLT_FINALME$STATION),] #tri du tabeau selon ME & STATION	
	}

if (SEEEECOLOME=="oui" & CEPE == "PE") {
	RLT_FINALME<-EECOLOME
	
	#Choix des noms
	NAMES_ATTENDUS_RLT<-c("EUCD","NAME",ETATECOLOMEPE_NAMES,ETATECOLOHPS_NAMES,ETATBIO_NAMES,ETATPCH_NAMES,ETATPS_NAMES,PARAMETREBIO_NAMES, 
									PARAMETREPCHELT_NAMES,
									PARAMETREPOLLUANT_NAMES,PARAMETREPCH_NAMES,PARAMETREPS_NAMES,ASSOUPLI_NAMES,MODELISE_NAMES,
									NBBIO_NAMES,BIODECLASS_NAMES,N_BIODECLASS_NAMES,PCHDECLASS_NAMES, N_PCHDECLASS_NAMES, 
									PCHDECLASSASSOUP_NAMES, N_PCHASSOUP_NAMES, 
									PSDECLASS_NAMES, N_PSDECLASS_NAMES, ECOLODECLASS_NAMES,N_ECODECLASS_NAMES,ECOLOORIGINDECLASS_NAMES,
									N_ECOLOORIGINDECLASS_NAMES,
									STATION_NAMES,MASSEDEAU_NAMES)
	NAMES_ATTENDUS_RLT<-unique(NAMES_ATTENDUS_RLT[NAMES_ATTENDUS_RLT %in% names(RLT_FINALME)])
	RLT_FINALME<-RLT_FINALME[,NAMES_ATTENDUS_RLT]
	
	
}
	
##########################################################
# CONSTRUCTION DES TABLEAUX - RANG 90 / MOYENNE / INDICE
##########################################################
if (INDICE=="oui") {
	RLT_FINALINDICE<-data.frame(RLT_FINALSTATION[,c("STATION")])
	names(RLT_FINALINDICE)[1]<-as.character("STATION")
	
	if (SEEEBIO=="oui") {
		RLT_FINALINDICE<-merge(RLT_FINALINDICE,RLT_BIOINDICE,by="STATION", all.x=TRUE)
	}
	if (SEEEPCH=="oui") {
		RLT_FINALINDICE<-merge(RLT_FINALINDICE,RLT_PCHINDICE,by="STATION", all.x=TRUE)
	}
	if (BASSIN =="AESN" & SEEEBIO=="oui" & SEEEPCH=="oui" ) {
		RLT_FINALINDICE<-merge(RLT_FINALINDICE,ETATECOLOHPSINDICE,by="STATION", all.x=TRUE)
	}
	if (SEEEPS=="oui") {
		RLT_FINALINDICE<-merge(RLT_FINALINDICE,RLT_POLSPEINDICE,by="STATION", all.x=TRUE)
	}
	if (BASSIN =="AESN" & SEEEBIO=="oui" & SEEEPCH=="oui" & SEEEPS=="oui") {
		RLT_FINALINDICE<-merge(RLT_FINALINDICE,ETATECOLOINDICE,by="STATION", all.x=TRUE)
	}	
	# modifié le 04/02/14 : option Quart de classe
	RLT_FINALINDICE<-RLT_FINALINDICE[,c("STATION",as.character(na.omit(ifelse(MODULE == "REEE2010",vPARAMETREBIO_NAMES,eqrPARAMETREBIO_NAMES))),iETATECOLO_NAMES,iETATECOLOHPS_NAMES,iETATBIO_NAMES,iPARAMETREBIO_NAMES,QUARTCLASSE_NAMES,vPARAMETREPCH_NAMES,iETATPCH_NAMES,iPARAMETREPCHELT_NAMES,iPARAMETREPCH_NAMES,iETATPS_NAMES,vPARAMETREPS_NAMES,iPARAMETREPS_NAMES)]

	RLT_FINALINDICE<-RLT_FINALINDICE[order(RLT_FINALINDICE$STATION),] #tri du tabeau selon code STATION
	
		#Rajout STATIONTXT si station au format SANDRE
		RLT_FINALINDICE_NAMES<-names(RLT_FINALINDICE)
			RLT_FINALINDICE$STATIONTXT<-as.character(RLT_FINALINDICE$STATION)
			RLT_FINALINDICE$STATIONTXT[nchar(as.character(RLT_FINALINDICE$STATION)) == 7 & !is.na(RLT_FINALINDICE$STATION)]<-paste("0",RLT_FINALINDICE$STATION[nchar(as.character(RLT_FINALINDICE$STATION)) == 7 & !is.na(RLT_FINALINDICE$STATION)],sep="")
			RLT_FINALINDICE<-RLT_FINALINDICE[,c("STATIONTXT",RLT_FINALINDICE_NAMES)]
			RLT_FINALINDICE_NAMES<-names(RLT_FINALINDICE)


	if (MODULE == "REEE2021"){
	
		# Cycle 3 (modif le 04/05/15) : modif nom colonne pour cohérence avec data bio du 3eme cycle
		# modif 1 dans vecteur de names
		RLT_FINALINDICE_NAMES[RLT_FINALINDICE_NAMES == "vIBG"]<-"vI2M2"
		RLT_FINALINDICE_NAMES[RLT_FINALINDICE_NAMES == "iIBG"]<-"iI2M2"
		RLT_FINALINDICE_NAMES[RLT_FINALINDICE_NAMES == "eqrIBG"]<-"eqrI2M2"
		RLT_FINALINDICE_NAMES[RLT_FINALINDICE_NAMES == "vIPR"]<-"vIPRP"
		RLT_FINALINDICE_NAMES[RLT_FINALINDICE_NAMES == "iIPR"]<-"iIPRP"
		RLT_FINALINDICE_NAMES[RLT_FINALINDICE_NAMES == "eqrIPR"]<-"eqrIPRP"

		# modif 2 dans dataframe
		names(RLT_FINALINDICE)[names(RLT_FINALINDICE) == "vIBG"]<-"vI2M2"
		names(RLT_FINALINDICE)[names(RLT_FINALINDICE) == "iIBG"]<-"iI2M2"
		names(RLT_FINALINDICE)[names(RLT_FINALINDICE) == "eqrIBG"]<-"eqrI2M2"
		names(RLT_FINALINDICE)[names(RLT_FINALINDICE) == "vIPR"]<-"vIPRP"
		names(RLT_FINALINDICE)[names(RLT_FINALINDICE) == "iIPR"]<-"iIPRP"	
		names(RLT_FINALINDICE)[names(RLT_FINALINDICE) == "eqrIPR"]<-"eqrIPRP"	
	}
}

#########################################
# CONSTRUCTION DES TABLEAUX - FREQUENCE
#########################################
if (FREQPRELEV=="oui") {
	RLT_FINALFREQ<-data.frame(RLT_FINALSTATION[,c("STATION")])
	names(RLT_FINALFREQ)[1]<-as.character("STATION")
	
	if (SEEEBIO=="oui") {
		RLT_FINALFREQ<-merge(RLT_FINALFREQ,RLT_BIOFREQ,by="STATION", all.x=TRUE)
	}
	if (SEEEPCH=="oui") {
		RLT_FINALFREQ<-merge(RLT_FINALFREQ,RLT_PCHFREQ,by="STATION", all.x=TRUE)
		if(MODULE != "2010" & CEPE == "CE") {
			RLT_FINALFREQ<-merge(RLT_FINALFREQ,RLT_PCHFREQDECLAS,by="STATION", all.x=TRUE)
		}
	}
	if (SEEEPS=="oui") {
		RLT_FINALFREQ<-merge(RLT_FINALFREQ,RLT_POLSPEFREQ,by="STATION", all.x=TRUE)
	}
	
	   # On prend les colonnes en fonction de s'il ya des parametres déclassants
	if (SEEEPCH=="oui") {   
	if (nrow(TEMPDECLAS) > 0) {
	RLT_FINALFREQ<-RLT_FINALFREQ[,c("STATION",fPARAMETREBIO_NAMES,fPARAMETREPCH_NAMES,fdPARAMETREPCH_NAMES,fPARAMETREPS_NAMES,aPARAMETREPS_NAMES)]
	} else {
	RLT_FINALFREQ<-RLT_FINALFREQ[,c("STATION",fPARAMETREBIO_NAMES,fPARAMETREPCH_NAMES,fPARAMETREPS_NAMES,aPARAMETREPS_NAMES)]
	}
	} else {
	RLT_FINALFREQ<-RLT_FINALFREQ[,c("STATION",fPARAMETREBIO_NAMES,fPARAMETREPCH_NAMES,fPARAMETREPS_NAMES,aPARAMETREPS_NAMES)]
	}
	
	RLT_FINALFREQ<-RLT_FINALFREQ[order(RLT_FINALFREQ$STATION),] #tri du tabeau selon code STATION
	
		
		#Rajout STATIONTXT si station au format SANDRE
		RLT_FINALFREQ_NAMES<-names(RLT_FINALFREQ)

			RLT_FINALFREQ$STATIONTXT<-as.character(RLT_FINALFREQ$STATION)
			RLT_FINALFREQ$STATIONTXT[nchar(as.character(RLT_FINALFREQ$STATION)) == 7 & !is.na(RLT_FINALFREQ$STATION)]<-paste("0",RLT_FINALFREQ$STATION[nchar(as.character(RLT_FINALFREQ$STATION)) == 7 & !is.na(RLT_FINALFREQ$STATION)],sep="")
			RLT_FINALFREQ<-RLT_FINALFREQ[,c("STATIONTXT",RLT_FINALFREQ_NAMES)]
			RLT_FINALFREQ_NAMES<-names(RLT_FINALFREQ)


	if (MODULE == "REEE2021"){
		# Cycle 3 (modif le 04/05/15) : modif nom colonne pour cohérence avec data bio du 3eme cycle

		# modif 1 dans vecteur de names
		RLT_FINALINDICE_NAMES[RLT_FINALINDICE_NAMES == "fIBG"]<-"fI2M2"
		RLT_FINALINDICE_NAMES[RLT_FINALINDICE_NAMES == "fIPR"]<-"fIPRP"

		# modif 2 dans dataframe
		names(RLT_FINALINDICE)[names(RLT_FINALINDICE) == "fIBG"]<-"fI2M2"
		names(RLT_FINALINDICE)[names(RLT_FINALINDICE) == "fIPR"]<-"fIPRP"	
	
	}

}


