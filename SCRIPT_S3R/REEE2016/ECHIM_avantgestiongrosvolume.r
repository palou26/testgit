##############################################
## SEEE - COURS D'EAU : état chimique
## Application de l'arrêté de janvier 2010
##  script modifié en juin 2015 pour les règles du 2nd cycle
##############################################
#save.image("D:/_ETUDES/E3648 - AELB carto 2014-2015/D4/C043_E4294/TECHNIQUE/SaveCHIM.Rdata")
#load("D:\\_ETUDES\\E3648 - AELB carto 2014-2015\\D4\\C043_E4294\\TECHNIQUE\\SaveCHIM.Rdata")

#PARAMETRECHIM$LQSEUIL<-0.035 ; SUPPR_LQ_ABERRANTE<-"oui"

##############################
## Mise en forme des données
##############################
gc() ## compacte R
DATACHIM$IDAN<-as.character(paste0(DATACHIM$STATION,DATACHIM$PARAMETRE,DATACHIM$ANNEE))

#Valeur par defaut du nombre minimal de prélevements
if (!exists("NBANFREQ")){NBANFREQ<-4}
if (FREQOKCHIM != "oui") {NBANFREQ<-0}


### Extraction selon les paramètres prioritaires notamment pour les groupes afin de recalculer les valeurs brutes
NAMES_DATACHIM<-names(DATACHIM)
DATACHIM<-merge(DATACHIM, PARAMETRECHIM[,c("PARAMETRE","PRIORITE")], by="PARAMETRE")
gc()
DATACHIM<-merge(DATACHIM, LIAISONCHIM, by="PARAMETRE")
gc()
TEMPDATA<-aggregate(PRIORITE ~ STATION + PARAGROUP + DATEPRELEV , data = DATACHIM , min)
DATACHIM<-DATACHIM[paste0(DATACHIM$STATION,DATACHIM$PARAGROUP,DATACHIM$DATEPRELEV,DATACHIM$PRIORITE) %in% paste0(TEMPDATA$STATION,TEMPDATA$PARAGROUP,TEMPDATA$DATEPRELEV,TEMPDATA$PRIORITE),]
DATACHIM<-DATACHIM[,NAMES_DATACHIM]
gc()
rm(TEMPDATA)


# Substance individuelle ou non
LIAISONCHIMPARAGROUP<-merge(LIAISONCHIM,PARAMETRECHIM,by="PARAMETRE")
PRIORITE<-aggregate(PRIORITE ~ PARAGROUP, data = LIAISONCHIMPARAGROUP, min)
SUBSTGROUPEE<-aggregate(PRIORITE ~ PARAGROUP , data = LIAISONCHIMPARAGROUP[paste0(LIAISONCHIMPARAGROUP$PARAGROUP, LIAISONCHIMPARAGROUP$PRIORITE) %in% paste0(PRIORITE$PARAGROUP, PRIORITE$PRIORITE) ,], length)
SUBSTGROUPEE$INDIVIDUELLE<-"non"
SUBSTGROUPEE$INDIVIDUELLE[SUBSTGROUPEE$PRIORITE==1]<-"oui"


######################################################################
## METHODE : choix de la méthode pour la preparation des données brutes
# ANNEERECENTE : extraction des données de l'année la + récente pour chaque paramètre de la station
# ANNEECHRONIQUE : extraction des données d'une station pour l'ensemble des paramètres ayant la chronique la plus complète
# NOFILTREANNEE : on ne fait aucun filtre sur l'année et on prend en compte tout le jeu de données
######################################################################
gc()

if (METHODEMOYCHIM =="NOFILTREANNEE") {
	CHIMANNEEmax<-aggregate(ANNEE ~ STATION + PARAMETRE, data = DATACHIM , max)
	CHIMANNEEmin<-aggregate(ANNEE ~ STATION + PARAMETRE, data = DATACHIM , min)
	CHIMANNEEmax$IDAN<-as.character(paste0(CHIMANNEEmax$STATION,CHIMANNEEmax$PARAMETRE))
	CHIMANNEEmin$IDAN<-as.character(paste0(CHIMANNEEmin$STATION,CHIMANNEEmin$PARAMETRE))
	CHIMANNEE<-merge(CHIMANNEEmin,CHIMANNEEmax[,c("IDAN","ANNEE")], by = "IDAN")
	CHIMANNEE$ANNEE<-CHIMANNEE$ANNEE.x
	condAnnee<-CHIMANNEE$ANNEE.x != CHIMANNEE$ANNEE.y
	CHIMANNEE$ANNEE[condAnnee]<-paste0(CHIMANNEE$ANNEE.x[condAnnee],"-",CHIMANNEE$ANNEE.y[condAnnee])
}
if (METHODEMOYCHIM =="ANNEERECENTE") {
	CHIMANNEE<-aggregate(ANNEE ~ STATION + PARAMETRE, data = DATACHIM , max)
	CHIMANNEE$IDAN<-as.character(paste0(CHIMANNEE$STATION,CHIMANNEE$PARAMETRE,CHIMANNEE$ANNEE))
	DATACHIM<-DATACHIM[DATACHIM$IDAN %in% c(CHIMANNEE$IDAN),]
} else if (METHODEMOYCHIM =="ANNEECHRONIQUE") {
	CHIMANNEE<-aggregate(RESULTAT ~ STATION + PARAMETRE + ANNEE, data = DATACHIM , length)
	CHIMANNEE$IDAN<-as.character(paste0(CHIMANNEE$STATION,CHIMANNEE$PARAMETRE,CHIMANNEE$ANNEE))
	CHIMANNEE$IDFREQ<-as.character(paste0(CHIMANNEE$STATION,CHIMANNEE$PARAMETRE,CHIMANNEE$RESULTAT))
	CHIMANNEE<-CHIMANNEE[order(CHIMANNEE$IDAN),]
	
	TEMPCHIMANNEE1<-aggregate(RESULTAT ~ STATION + PARAMETRE, data = CHIMANNEE , max) # colonne RESULTAT non renommée mais contient le nb de prelevement
	TEMPCHIMANNEE1$IDFREQ<-as.character(paste0(TEMPCHIMANNEE1$STATION,TEMPCHIMANNEE1$PARAMETRE,TEMPCHIMANNEE1$RESULTAT))
	CHIMANNEE<-CHIMANNEE[CHIMANNEE$IDFREQ %in% c(TEMPCHIMANNEE1$IDFREQ),]
		
	#extraction de l'année la + récente si plusieurs années ont les mêmes fréquences
	TEMPCHIMANNEE2<-aggregate(ANNEE ~ STATION + PARAMETRE, data = CHIMANNEE , max)
	TEMPCHIMANNEE2$IDAN<-as.character(paste0(TEMPCHIMANNEE2$STATION,TEMPCHIMANNEE2$PARAMETRE,TEMPCHIMANNEE2$ANNEE))
	DATACHIM<-DATACHIM[DATACHIM$IDAN %in% c(TEMPCHIMANNEE2$IDAN),]
	
	#on recrée CHIMANNEE depuis DATACHIM filtré pour calculer les fréquences. CHIMANNEE est ainsi identique à "ANNEERECENTE" dans FREQUENCE.r --> ça facilite le script et les éventuelles modif.
	CHIMANNEE<-aggregate(ANNEE ~ STATION + PARAMETRE, data = DATACHIM , max)
	
	rm(TEMPCHIMANNEE1,TEMPCHIMANNEE2)
}

# Mise en forme de la table principale
DATACHIM<-merge(DATACHIM, LISTECODERQE[,c("REMARQUE","QUANTIFIE")], by="REMARQUE")
DATACHIM<-merge(DATACHIM, LIAISONCHIM, by="PARAMETRE")
DATACHIM<-merge(DATACHIM,PARAGROUPCHIM,by="PARAGROUP")
gc()

# 5/12/2016 : on retire les LQABERANTE
DATACHIM<-merge(DATACHIM, PARAMETRECHIM[,c("PARAMETRE","LQSEUIL")], by="PARAMETRE", all.x = TRUE)
if ( SUPPR_LQ_ABERRANTE_CHIM == "oui" ) {
	if (LQ_NONDISPO =="oui") { # si LQ non dispo on prend les valeurs du champ RESULTAT lorsque non quantifié --> car c'est la LQ
		PBLQCHIMABERRANTE<-DATACHIM[!is.na(DATACHIM$LQSEUIL) & DATACHIM$RESULTAT>DATACHIM$LQSEUIL & DATACHIM$QUANTIFIE=="0" & DATACHIM$LQSEUIL != -99 ,]
		DATACHIM<-DATACHIM[(!(!is.na(DATACHIM$LQSEUIL) & DATACHIM$RESULTAT>DATACHIM$LQSEUIL & DATACHIM$QUANTIFIE=="0"))| DATACHIM$LQSEUIL == -99 | is.na(DATACHIM$LQSEUIL) ,]
	} else {
		PBLQCHIMABERRANTE<-DATACHIM[!is.na(DATACHIM$LQSEUIL) & DATACHIM$LQ>DATACHIM$LQSEUIL  & DATACHIM$LQSEUIL != -99  ,]
		DATACHIM<-DATACHIM[(!(!is.na(DATACHIM$LQSEUIL) & DATACHIM$LQ>DATACHIM$LQSEUIL))| DATACHIM$LQSEUIL == -99 | is.na(DATACHIM$LQSEUIL)  ,]
	}

	if( nrow(PBLQCHIMABERRANTE)>0) {
		CSV<-paste(CH_ERREUR,"CHIM_LQ_ABERRANTE_",SEEE_DEBformat,".csv",sep="")
		write.csv2(PBLQCHIMABERRANTE,CSV)
		MSGBOX <- tkmessageBox(title = "Info", message = paste("Les données brutes avec LQ aberrantes (strictement > LQSEUIL) ont été retirées \n du calcul de l'état chimique et exportées dans \n",CSV,sep=""), icon = "info", type = "ok")
	}

	if (nrow(DATACHIM) == 0) {
			MSGBOX <- tkmessageBox(title = "Info", message = paste("Pas de données à traiter après le retrait des mesures avec LQ aberrantes \n",CSV,sep=""), icon = "info", type = "ok")
			tcl("update")
			source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
	}	
}



# Calcul de la frequence de prélèvement
DATACHIM<-DATACHIM[!duplicated(DATACHIM[,c("STATION", "PARAGROUP", "PARAMETRE","DATEPRELEV", "HEUREPRELEV")]),]
DATACHIM_FREQ<-aggregate(RESULTAT ~ STATION + PARAMETRE + PARAGROUP + ANNEE, data = DATACHIM , length) # les données sont déjà filtrées sur l'année, on le prend dans aggregate uniquement pour l'avoir ensuite dans IDAN
names(DATACHIM_FREQ)[5]<-"FREQ"
DATACHIM_FREQ<-DATACHIM_FREQ[!duplicated(DATACHIM_FREQ[,c("STATION","PARAMETRE","ANNEE")]),]
DATACHIM_FREQ$IDAN<-as.character(paste0(DATACHIM_FREQ$STATION,DATACHIM_FREQ$PARAMETRE,DATACHIM_FREQ$ANNEE))
DATACHIM_FREQ<-DATACHIM_FREQ[,c("IDAN","FREQ")]

DATACHIM<-merge(DATACHIM,DATACHIM_FREQ,by="IDAN")
rm(DATACHIM_FREQ)
DATACHIMFREQ<-DATACHIM #save de DATACHIM avant retrait des freq non respectées pour les afficher dans les stat sur l'ensemble des paramètres
gc()

# ## Respect de la fréquence minimale de prélèvement par paramètre
# if (FREQOKCHIM == "oui") {
	# TEMPCHIM_RETRAIT<-DATACHIM[DATACHIM$FREQ<4, c("STATION", "PARAGROUP", "PARAMETRE", "ANNEE", "DATEPRELEV", "HEUREPRELEV","FREQ")]
	# DATACHIM<-DATACHIM[c(!(paste0(DATACHIM$STATION, DATACHIM$PARAGROUP) %in% paste0(TEMPCHIM_RETRAIT$STATION, TEMPCHIM_RETRAIT$PARAGROUP))),] # retrait du parametre et/ou groupe de parametres si un parametre ne respecte pas la frequence
	# #Export des stations qui ne sont plus dans le jeux de données car la fréquence est trop faible
	# TEMPCHIM_RETRAIT<-TEMPCHIM_RETRAIT[!duplicated(TEMPCHIM_RETRAIT),]
	# if( nrow(TEMPCHIM_RETRAIT>0)) {
		# CSV<-paste(CH_ERREUR,"CHIM_FREQ_TROP_BASSE_",SEEE_DEBformat,".csv",sep="")
		# write.csv2(TEMPCHIM_RETRAIT,CSV)
		# MSGBOX <- tkmessageBox(title = "Info", message = paste("Les données avec fréquences trop basses ont été exportées dans \n",CSV,sep=""), icon = "info", type = "ok")

	# }
# }
# gc()

DATACHIM<-merge(DATACHIM,STATION[,c("STATION","FONDGEO_CADMIUM","FONDGEO_PLOMB","FONDGEO_MERCURE","FONDGEO_NICKEL","DURETE")],by="STATION", all.x=TRUE)
gc()

if(CONTA != "oui"){
	# Détermination de NQEMA & CMA selon durete et Fond géochim

	DATACHIM$NQEMA[DATACHIM$PARAGROUP=="6" & DATACHIM$DURETE>=40 & DATACHIM$DURETE<50 & !is.na(DATACHIM$DURETE)]<-DATACHIM$NQEMA2[DATACHIM$PARAGROUP=="6" & DATACHIM$DURETE>=40 & DATACHIM$DURETE<50 & !is.na(DATACHIM$DURETE)]
	DATACHIM$NQEMA[DATACHIM$PARAGROUP=="6" & DATACHIM$DURETE>=50 & DATACHIM$DURETE<100 & !is.na(DATACHIM$DURETE)]<-DATACHIM$NQEMA3[DATACHIM$PARAGROUP=="6" & DATACHIM$DURETE>=50 & DATACHIM$DURETE<100 & !is.na(DATACHIM$DURETE)]
	DATACHIM$NQEMA[DATACHIM$PARAGROUP=="6" & DATACHIM$DURETE>=100 & DATACHIM$DURETE<200 & !is.na(DATACHIM$DURETE)]<-DATACHIM$NQEMA4[DATACHIM$PARAGROUP=="6" & DATACHIM$DURETE>=100 & DATACHIM$DURETE<200 & !is.na(DATACHIM$DURETE)]
	DATACHIM$NQEMA[DATACHIM$PARAGROUP=="6" & DATACHIM$DURETE>=200 & !is.na(DATACHIM$DURETE)]<-DATACHIM$NQEMA5[DATACHIM$PARAGROUP=="6" & DATACHIM$DURETE>=200 & !is.na(DATACHIM$DURETE)]

	condcadmium<-DATACHIM$PARAGROUP=="6" & !is.na(DATACHIM$FONDGEO_CADMIUM)
	DATACHIM$NQEMA[condcadmium]<-DATACHIM$NQEMA[condcadmium]+DATACHIM$FONDGEO_CADMIUM[condcadmium]
	DATACHIM$NQECMA[condcadmium]<-DATACHIM$NQECMA[condcadmium]+DATACHIM$FONDGEO_CADMIUM[condcadmium]

	condplomb<-DATACHIM$PARAGROUP=="20" & !is.na(DATACHIM$FONDGEO_PLOMB)
	DATACHIM$NQEMA[condplomb]<-DATACHIM$NQEMA[condplomb]+DATACHIM$FONDGEO_PLOMB[condplomb]
	DATACHIM$NQECMA[condplomb]<-DATACHIM$NQECMA[condplomb]+DATACHIM$FONDGEO_PLOMB[condplomb]

	condmercure<-DATACHIM$PARAGROUP=="21" & !is.na(DATACHIM$FONDGEO_MERCURE)
	DATACHIM$NQEMA[condmercure]<-DATACHIM$NQEMA[condmercure]+DATACHIM$FONDGEO_MERCURE[condmercure]
	DATACHIM$NQECMA[condmercure]<-DATACHIM$NQECMA[condmercure]+DATACHIM$FONDGEO_MERCURE[condmercure]

	condnickel<-DATACHIM$PARAGROUP=="23" & !is.na(DATACHIM$FONDGEO_NICKEL)
	DATACHIM$NQEMA[condnickel]<-DATACHIM$NQEMA[condnickel]+DATACHIM$FONDGEO_NICKEL[condnickel]
	DATACHIM$NQECMA[condnickel]<-DATACHIM$NQECMA[condnickel]+DATACHIM$FONDGEO_NICKEL[condnickel]
	rm(condcadmium,condplomb,condmercure,condnickel)
}

## Nouvelle matrice des NQEMA et MQECMA après modif classe de dureté et fond géochim
CHIM_NQE<-DATACHIM[!duplicated(DATACHIM[,c("STATION","PARAGROUP")]),c("STATION","PARAGROUP","NQEMA","NQECMA","GROUPE")]
CHIM_NQE$IDNQE<-paste0(CHIM_NQE$STATION,"_",CHIM_NQE$PARAGROUP)

# Extraction de la LQMAX par station pour comparaison à la CMA & MA
if (LQ_NONDISPO =="oui") { # recherche LQmax dans colonne resultat si colonne LQ non dispo
	LQMAX<-DATACHIM[DATACHIM$QUANTIFIE=="0", c("STATION","PARAGROUP","QUANTIFIE","RESULTAT")]
	LQMAX<-aggregate(RESULTAT~ STATION + PARAGROUP, data = LQMAX , max)
	names(LQMAX)[3]<-"LQMAX"
	LQMAX$LQMAX<-round(LQMAX$LQMAX,8)
	LQMAX$ID<-paste(LQMAX$STATION, LQMAX$PARAGROUP, sep="_")
} else { # recherche LQmax dans colonne LQ
	#LQMAX<-DATACHIM[DATACHIM$QUANTIFIE=="0", c("STATION","PARAGROUP","QUANTIFIE","RESULTAT")]
	LQMAX<-aggregate(LQ ~ STATION + PARAGROUP, data = DATACHIM , max)
	names(LQMAX)[3]<-"LQMAX"
	LQMAX$ID<-paste(LQMAX$STATION, LQMAX$PARAGROUP, sep="_")
}

gc()

##############################
## CHIMIE : comparaison CMA
##############################
if( nrow(DATACHIM[DATACHIM$FREQ >= NBANFREQ,]) == 0) {
	MSGBOX <- tkmessageBox(title = "Info", message = paste("Pas de données à traiter si respect de la fréquence de prélèvement\n Veuillez abaisser la fréquence ou décocher le respect \n",CSV,sep=""), icon = "info", type = "ok")
	}

#identification des paramètres groupe quantifié à minima 1 fois dans l'année
CHIMCMA_QUANTIFAN<-aggregate(QUANTIFIE ~ STATION + PARAGROUP, data = DATACHIM[DATACHIM$FREQ >= NBANFREQ,] , max)
names(CHIMCMA_QUANTIFAN)[3]<-"QUANTIFEAN"
CHIMCMA_QUANTIFAN$IDNQE<-paste(CHIMCMA_QUANTIFAN$STATION, CHIMCMA_QUANTIFAN$PARAGROUP,sep="_")
CHIMCMA_QUANTIFAN$ID<-paste(CHIMCMA_QUANTIFAN$STATION, CHIMCMA_QUANTIFAN$PARAGROUP, CHIMCMA_QUANTIFAN$QUANTIFEAN,sep="_")

CHIMCMA_MESUREMAX<-aggregate(RESULTAT ~ STATION + PARAGROUP + QUANTIFIE, data = DATACHIM[DATACHIM$FREQ >= NBANFREQ,] , max)
names(CHIMCMA_MESUREMAX)[4]<-"RESULTATMAX"
CHIMCMA_MESUREMAX$RESULTATMAX<-round(CHIMCMA_MESUREMAX$RESULTATMAX,8)
 #correspond à RESULTATMAX si paragroup quantifié ou à LQMAX qui paragroup non quantifié
CHIMCMA_MESUREMAX$ID<-paste(CHIMCMA_MESUREMAX$STATION, CHIMCMA_MESUREMAX$PARAGROUP, CHIMCMA_MESUREMAX$QUANTIFIE,sep="_")


CHIMCMA<-CHIMCMA_QUANTIFAN
CHIMCMA<-merge(CHIMCMA,CHIM_NQE[,c("NQEMA","NQECMA","GROUPE","IDNQE")],by="IDNQE")
CHIMCMA<-merge(CHIMCMA,CHIMCMA_MESUREMAX[,c("RESULTATMAX","ID")],by="ID")

# CAS 1 - Comparaison NQECMA si paragroup quantifié à minima 1 fois dans l'année - On traite la mesure max
CHIMCMA$CLASSECMA<-as.numeric(NA)
CHIMCMA$CLASSECMA[CHIMCMA$QUANTIFEAN=="1" & CHIMCMA$RESULTATMAX <= CHIMCMA$NQECMA]<-1
CHIMCMA$CLASSECMA[CHIMCMA$QUANTIFEAN=="1" & CHIMCMA$RESULTATMAX > CHIMCMA$NQECMA]<-2

# CAS 2 - Comparaison NQECMA si paragroup n'est jamais quantifié dans l'année - On traite en réalité la LQMAX
CHIMCMA$CLASSECMA[CHIMCMA$QUANTIFEAN=="0" & CHIMCMA$RESULTATMAX <= CHIMCMA$NQECMA]<-1
CHIMCMA$CLASSECMA[CHIMCMA$QUANTIFEAN=="0" & CHIMCMA$RESULTATMAX > CHIMCMA$NQECMA]<-0
CHIMCMA$CLASSECMA[CHIMCMA$NQECMA==-99]<- -99

rm(CHIMCMA_QUANTIFAN,CHIMCMA_MESUREMAX)

##############################
## CHIMIE : comparaison MA
##
##############################

# modif 24/04/15 : retrait prelevement pour calcul moyenne si LQ > NQE (mail Ministere du 08/04/15 à destination des agences pour cycle 2
if ( LQSUPNQEMACHIM == "oui" ) {
	if (LQ_NONDISPO =="oui") { # si LQ non dispo on prend les valeurs du champ RESULTAT lorsque non quantifié --> car c'est la LQ
		PBLQCHIM<-DATACHIM[DATACHIM$RESULTAT>DATACHIM$NQEMA & DATACHIM$QUANTIFIE=="0" & DATACHIM$NQEMA != -99 ,]
		DATACHIM<-DATACHIM[!(DATACHIM$RESULTAT>DATACHIM$NQEMA & DATACHIM$QUANTIFIE=="0") | DATACHIM$NQEMA == -99 ,]
	} else {
		PBLQCHIM<-DATACHIM[DATACHIM$LQ > DATACHIM$NQEMA & DATACHIM$NQEMA != -99 ,]
		DATACHIM<-DATACHIM[DATACHIM$LQ <= DATACHIM$NQEMA | DATACHIM$NQEMA == -99  ,]
	}

	if( nrow(PBLQCHIM)>0) {
		CSV<-paste(CH_ERREUR,"CHIM_LQ_SUP_NQEMA_",SEEE_DEBformat,".csv",sep="")
		write.csv2(PBLQCHIM,CSV)
		MSGBOX <- tkmessageBox(title = "Info", message = paste("Les données brutes avec LQ > NQEMA ont été retirées /n du calcul de l'état chimique et exportées dans /n",CSV,sep=""), icon = "info", type = "ok")

	}	
	}


# Modification suite à la publiation de l'arreté et prestation C43 AELB 17 dec 
# Si le resultat est inférieur à la LQ on remplace par LQ/2
DATACHIM<-merge(DATACHIM,SUBSTGROUPEE[,c("PARAGROUP","INDIVIDUELLE")], by = "PARAGROUP",all.x = TRUE)
DATACHIM$RESULTAT2<-DATACHIM$RESULTAT
#Si  on est pas quantifié 
if (LQ_NONDISPO =="oui") {
	condindiv<- DATACHIM$QUANTIFIE=="0"  & DATACHIM$INDIVIDUELLE == "oui"
	DATACHIM$RESULTAT2[condindiv]<-DATACHIM$RESULTAT2[condindiv]/2
	condgroup<- DATACHIM$QUANTIFIE=="0" & DATACHIM$INDIVIDUELLE == "non"
	DATACHIM$RESULTAT2[condgroup]<-0
} else {
	condindiv<- (DATACHIM$QUANTIFIE=="0"  | DATACHIM$RESULTAT <= DATACHIM$LQ) & DATACHIM$INDIVIDUELLE == "oui"
	DATACHIM$RESULTAT2[condindiv]<-DATACHIM$LQ[condindiv]/2
	condgroup<- (DATACHIM$QUANTIFIE=="0" | DATACHIM$RESULTAT <= DATACHIM$LQ )& DATACHIM$INDIVIDUELLE == "non"
	DATACHIM$RESULTAT2[condgroup]<-0
}


# Extraction de la LQMAX par station pour comparaison à la MA
if (LQ_NONDISPO =="oui") { # recherche LQmax dans colonne resultat si colonne LQ non dispo
	LQMAX<-DATACHIM[DATACHIM$QUANTIFIE=="0", c("STATION","PARAGROUP","QUANTIFIE","RESULTAT")]
	LQMAX<-aggregate(RESULTAT~ STATION + PARAGROUP, data = LQMAX , max)
	names(LQMAX)[3]<-"LQMAX"
	LQMAX$LQMAX<-round(LQMAX$LQMAX,8)
	LQMAX$ID<-paste(LQMAX$STATION, LQMAX$PARAGROUP, sep="_")
} else { # recherche LQmax dans colonne LQ
	LQMAX<-aggregate(LQ ~ STATION + PARAGROUP, data = DATACHIM , max)
	names(LQMAX)[3]<-"LQMAX"
	LQMAX$ID<-paste(LQMAX$STATION, LQMAX$PARAGROUP, sep="_")
}
	
	
# on somme les resultats notamment si famille de substance	
DATACHIM1<-aggregate(RESULTAT2~ STATION + DATEPRELEV + HEUREPRELEV + ANNEE+ PARAGROUP+GROUPE+NQEMA, data = DATACHIM[DATACHIM$FREQ >= NBANFREQ,] , sum)
nrow(DATACHIM1)
DATACHIMSUP<-DATACHIM[ !(paste(DATACHIM$STATION, DATACHIM$PARAGROUP) %in%  paste(DATACHIM1$STATION, DATACHIM1$PARAGROUP) )  ,     ]
DATACHIM<-rbind(DATACHIM1, DATACHIMSUP[DATACHIMSUP$FREQ < NBANFREQ,names(DATACHIM1)] )
nrow(DATACHIM)
rm(DATACHIM1, DATACHIMSUP)
gc()

DATACHIM_NQEMA<-DATACHIM[!duplicated(DATACHIM[,c("STATION","PARAGROUP")]),c("STATION","PARAGROUP","NQEMA")]
DATACHIM_NQEMA$ID<-paste0(DATACHIM_NQEMA$STATION,"_",DATACHIM_NQEMA$PARAGROUP)



# Calcul de la moyenne
DATACHIM$MOY<-DATACHIM$RESULTAT2
CHIMMA<-aggregate(MOY~ STATION + PARAGROUP + GROUPE, data = DATACHIM , mean)
CHIMMA$MOY<-round(CHIMMA$MOY,8)

CHIMMA$ID<-paste0(CHIMMA$STATION,"_",CHIMMA$PARAGROUP)
gc()
CHIMMA<-merge(CHIMMA,DATACHIM_NQEMA[,c("ID","NQEMA")],by="ID")
gc()



# rapatriment de la LQMAX
CHIMMA<-merge(CHIMMA,LQMAX[,c("ID","LQMAX")],by="ID", all.x=TRUE)
gc()


# On indique pour chaque prélèvement s'il s'agit de substance groupée ou non
CHIMMA<-merge(CHIMMA,SUBSTGROUPEE[,c("PARAGROUP","INDIVIDUELLE")], by = "PARAGROUP",all.x = TRUE)



# Calcul de la frequence de prélèvement
DATACHIM_FREQ<-aggregate(MOY ~ STATION + PARAGROUP, data = DATACHIM , length)
names(DATACHIM_FREQ)[3]<-"FREQ"
DATACHIM_FREQ$ID<-as.character(paste0(DATACHIM_FREQ$STATION,"_",DATACHIM_FREQ$PARAGROUP))
DATACHIM_FREQ<-DATACHIM_FREQ[,c("ID","FREQ")]

CHIMMA<-merge(CHIMMA,DATACHIM_FREQ,by="ID")
CHIMMA<-CHIMMA[order(CHIMMA$STATION, CHIMMA$PARAGROUP),]
rm(DATACHIM_FREQ)

#initialisation de CALSSEPS et CASCHIMMA
CHIMMA$CLASSEMA<-as.numeric(NA)
CHIMMA$CASCHIMMA<-""


#Si on a supprimé des enregistrements à cause de retrait prelevement pour calcul moyenne si LQ > NQE 

## CAS 0 - Respect de la fréquence minimale de prélèvement
if (FREQOKCHIM == "oui") {
	CHIMMA$CASCHIMMA[CHIMMA$FREQ<NBANFREQ]<-"0"
	CHIMMA$CLASSEMA[CHIMMA$FREQ<NBANFREQ]<-0
	
	if ( LQSUPNQEMACHIM == "oui" ) {
	if (nrow(PBLQCHIM) > 0) {
		PBLQCHIMFREQ<-data.frame(table(PBLQCHIM$STATION ,PBLQCHIM$PARAGROUP))
		names(PBLQCHIMFREQ)<-c("STATION","PARAGROUP","FREQPBLQ")
		PBLQCHIMFREQ<-PBLQCHIMFREQ[PBLQCHIMFREQ$FREQPBLQ > 0,]
		PBLQCHIMFREQ$ID<-paste(PBLQCHIMFREQ$STATION ,PBLQCHIMFREQ$PARAGROUP,sep="_")
		CHIMMA$ID<-paste(CHIMMA$STATION ,CHIMMA$PARAGROUP,sep="_")
		CHIMMA<-merge(CHIMMA,PBLQCHIMFREQ,all=TRUE)
		CHIMMA$FREQ[is.na(CHIMMA$FREQ)]<-0
		CHIMMA$FREQPBLQ[is.na(CHIMMA$FREQPBLQ)]<-0
		#cas où si on avait conservé tous les prélevements on aurait atteint la frequence de prelevement
		CHIMMA$CASCHIMMA[CHIMMA$FREQ>0 & CHIMMA$FREQ<NBANFREQ & CHIMMA$FREQ + CHIMMA$FREQPBLQ  >= NBANFREQ ]<-"0*"
		# cas où la frequence après retrait est à zero et on aurait quand même pas attend la frequence de prélevement si on avait conservé tous les prélevements
		CHIMMA$CASCHIMMA[CHIMMA$FREQ== 0 & CHIMMA$FREQPBLQ  > 0 ]<-"0**"
		# cas où la frequence après retrait est à zero et on aurait  atteind la frequence de prélevement si on avait conservé tous les prélevements		
		CHIMMA$CASCHIMMA[CHIMMA$FREQ== 0 & CHIMMA$FREQPBLQ  >= NBANFREQ ]<-"0***"
		CHIMMA$CLASSEMA[CHIMMA$CASCHIMMA %in% c("0","0*","0**","0***")]<-0
		print(table(CHIMMA$CASCHIMMA))		
		}}
}

## CAS 0 - Respect de la fréquence minimale de prélèvement
if (FREQOKCHIM == "oui") {
	CHIMMA$CASCHIMMA[CHIMMA$FREQ<NBANFREQ & CHIMMA$CASCHIMMA == ""]<-"0"
	CHIMMA$CLASSEMA[CHIMMA$FREQ<NBANFREQ  & CHIMMA$CASCHIMMA == ""]<-0
	
	if ( SUPPR_LQ_ABERRANTE_CHIM == "oui" ) {
	if (nrow(PBLQCHIMABERRANTE) > 0) {
		PBLQCHIMABERRANTEFREQ<-data.frame(table(PBLQCHIMABERRANTE$STATION ,PBLQCHIMABERRANTE$PARAGROUP))
		names(PBLQCHIMABERRANTEFREQ)<-c("STATION","PARAGROUP","FREQPBLQ")
		PBLQCHIMABERRANTEFREQ<-PBLQCHIMABERRANTEFREQ[PBLQCHIMABERRANTEFREQ$FREQPBLQ > 0,]
		PBLQCHIMABERRANTEFREQ$ID<-paste(PBLQCHIMABERRANTEFREQ$STATION ,PBLQCHIMABERRANTEFREQ$PARAGROUP,sep="_")
		CHIMMA$ID<-paste(CHIMMA$STATION ,CHIMMA$PARAGROUP,sep="_")
		CHIMMA<-merge(CHIMMA,PBLQCHIMABERRANTEFREQ,all=TRUE)
		CHIMMA$FREQ[is.na(CHIMMA$FREQ)]<-0
		CHIMMA$FREQPBLQ[is.na(CHIMMA$FREQPBLQ)]<-0
		#cas où si on avait conservé tous les prélevements on aurait atteint la frequence de prelevement
		CHIMMA$CASCHIMMA[CHIMMA$FREQ>0 & CHIMMA$FREQ<NBANFREQ & CHIMMA$FREQ + CHIMMA$FREQPBLQ  >= NBANFREQ ]<-"0+"
		# cas où la frequence après retrait est à zero et on aurait quand même pas attend la frequence de prélevement si on avait conservé tous les prélevements
		CHIMMA$CASCHIMMA[CHIMMA$FREQ== 0 & CHIMMA$FREQPBLQ  > 0 ]<-"0++"
		# cas où la frequence après retrait est à zero et on aurait  atteind la frequence de prélevement si on avait conservé tous les prélevements		
		CHIMMA$CASCHIMMA[CHIMMA$FREQ== 0 & CHIMMA$FREQPBLQ  >= NBANFREQ ]<-"0+++"
		CHIMMA$CLASSEMA[CHIMMA$CASCHIMMA %in% c("0","0+","0++","0+++")]<-0
		print(table(CHIMMA$CASCHIMMA))		
		}}
}



####SUBSTANCES INDIVIDUELLES

###CAS 1 -  la MOYENNE <= NQE 
condCAS1<-!( CHIMMA$CASCHIMMA  %in% c("0","0+","0++","0+++","0*","0**","0***"))   &  (is.na(CHIMMA$LQMAX) | CHIMMA$LQMAX <= CHIMMA$NQEMA) & CHIMMA$INDIVIDUELLE == "oui"
	# Cas 1A
	condCAS1a<-condCAS1 & CHIMMA$MOY <= CHIMMA$NQEMA &  !is.na(CHIMMA$LQMAX)
	CHIMMA$CLASSEMA[condCAS1a]<-1  
	CHIMMA$CASCHIMMA[condCAS1a ]<-"1A"
	# Cas 1B
	condCAS1c<-condCAS1 & CHIMMA$MOY > CHIMMA$NQEMA 
	CHIMMA$CLASSEMA[condCAS1c]<-2  
	CHIMMA$CASCHIMMA[condCAS1c ]<-"1B"
	
###CAS 2 -  la MOYENNE > NQE 
condCAS2<-!( CHIMMA$CASCHIMMA  %in% c("0","0+","0++","0+++","0*","0**","0***","1A","1B")) &  !is.na(CHIMMA$LQMAX) & CHIMMA$LQMAX > CHIMMA$NQEMA 
	# Cas 2A
	condCAS2a<-condCAS2 & CHIMMA$MOY >=CHIMMA$LQMAX
	CHIMMA$CLASSEMA[condCAS2a]<-2  
	CHIMMA$CASCHIMMA[condCAS2a ]<-"2A"
	# Cas 2B
	condCAS2b<-condCAS2 & CHIMMA$MOY < CHIMMA$LQMAX
	CHIMMA$CLASSEMA[condCAS2b]<-0  
	CHIMMA$CASCHIMMA[condCAS2b ]<-"2B"

####SUBSTANCES FAMILLE	
###CAS 3 -  la MOYENNE > NQE 
condCAS3<-!( CHIMMA$CASCHIMMA  %in% c("0","0+","0++","0+++","0*","0**","0***","1A","1B","2A","2B")) & CHIMMA$INDIVIDUELLE == "non"
	# Cas 2A
	condCAS3a<-condCAS3 & CHIMMA$MOY <= CHIMMA$NQEMA
	CHIMMA$CLASSEMA[condCAS3a]<-1  
	CHIMMA$CASCHIMMA[condCAS3a ]<-"3A"
	# Cas 2B
	condCAS3b<-condCAS3 & CHIMMA$MOY > CHIMMA$NQEMA
	CHIMMA$CLASSEMA[condCAS3b]<-2  
	CHIMMA$CASCHIMMA[condCAS3b ]<-"3B"
	
	
#verif
table(CHIMMA$CASCHIMMA)
table(CHIMMA$CASCHIMMA,	CHIMMA$CLASSEMA)


##Si NQEMA = -99 (sans objet)
CHIMMA$CLASSEMA[CHIMMA$NQEMA == -99]<- -99
gc()

##Création de l'onglet CAS_CHIMMA
PARAGROUPCHIM<-PARAGROUPCHIM[order(PARAGROUPCHIM$IDTRI),]
CAS_CHIMMA<-data.frame(STATION = sort(unique(CHIMMA$STATION)))
for (i in  1:nrow(PARAGROUPCHIM)  ) {
	TEMPP<-CHIMMA[CHIMMA$PARAGROUP==PARAGROUPCHIM$PARAGROUP[i],c("STATION", "CASCHIMMA")]
	names(TEMPP)[2]<-toupper(PARAGROUPCHIM$PARAGROUPLIBCOURT[i])
	CAS_CHIMMA<-merge(CAS_CHIMMA,TEMPP,by="STATION", all.x=TRUE)
	rm(TEMPP)
}


##############################
## Calcul de l'état chimique
##############################
# Etat par PARAGROUP
CHIMCMA$ID<-paste(CHIMCMA$STATION, CHIMCMA$PARAGROUP, sep="_")
CHIMMA$ID<-paste(CHIMMA$STATION, CHIMMA$PARAGROUP, sep="_")
CHIMCMAMA<-merge(CHIMCMA[,c("QUANTIFEAN","RESULTATMAX", "NQECMA","CLASSECMA", "ID")],CHIMMA[,c("ID", "STATION","PARAGROUP", "MOY",  "LQMAX", "NQEMA", "CLASSEMA","CASCHIMMA","GROUPE")], by="ID",all=TRUE)
CHIMCMAMA$CLASSEETAT<-as.numeric(NA)
CHIMCMAMA$CLASSECMA[is.na(CHIMCMAMA$CLASSECMA)]<-0
CHIMCMAMA$CLASSEMA[is.na(CHIMCMAMA$CLASSEMA)]<-0
CHIMCMAMA$CLASSEETAT[CHIMCMAMA$CLASSECMA==2 | CHIMCMAMA$CLASSEMA==2]<-2
CHIMCMAMA$CLASSEETAT[CHIMCMAMA$CLASSECMA==1 & CHIMCMAMA$CLASSEMA %in% c(-99,0)]<-0
CHIMCMAMA$CLASSEETAT[CHIMCMAMA$CLASSECMA==1 & CHIMCMAMA$CLASSEMA==1]<-1
CHIMCMAMA$CLASSEETAT[CHIMCMAMA$CLASSECMA %in% c(-99,0) & CHIMCMAMA$CLASSEMA==1]<-1
CHIMCMAMA$CLASSEETAT[CHIMCMAMA$CLASSECMA %in% c(-99,0) & CHIMCMAMA$CLASSEMA  %in% c(-99,0) ]<-0

# Etat par famille
CHIMFAMILLE<-merge(CHIMCMAMA,PARAGROUPCHIM,by="PARAGROUP")
CHIMFAMILLE<-aggregate(CLASSEETAT ~ STATION + FAMILLE, data = CHIMFAMILLE , max)
names(CHIMFAMILLE)[3]<-"ETATFAMILLE"


# Etat chimique sans HAP et ubiquiste
CHIMHORSHAP<-merge(CHIMCMAMA,PARAGROUPCHIM[PARAGROUPCHIM$HAPUBI != "oui",],by="PARAGROUP")
CHIMHORSHAP<-aggregate(CLASSEETAT ~ STATION, data = CHIMHORSHAP , max)
names(CHIMHORSHAP)[2]<-"ETATCHIM_SSHAPUBI"
CHIMHORSHAP<-CHIMHORSHAP[order(CHIMHORSHAP$STATION),]


# Etat chimique final
RLT_CHIMSTATION<-aggregate(ETATFAMILLE ~ STATION, data = CHIMFAMILLE , max)
names(RLT_CHIMSTATION)[2]<-"ETATCHIM"
RLT_CHIMSTATION<-merge(RLT_CHIMSTATION,CHIMHORSHAP,by="STATION", all.x=TRUE)
RLT_CHIMSTATION<-RLT_CHIMSTATION[order(RLT_CHIMSTATION$STATION),]

##############################
## Mise en forme tableau final

##############################
PARAGROUPCHIM<-PARAGROUPCHIM[order(PARAGROUPCHIM$IDTRI),]

# Ajout colonnes des familles 
for (i in  1:nrow(TABLEFAMILLE)  ) {
	TEMPP<-CHIMFAMILLE[CHIMFAMILLE$FAMILLE==TABLEFAMILLE$FAMILLE[i],c("STATION", "ETATFAMILLE")]
	names(TEMPP)[2]<-toupper(TABLEFAMILLE$FAMILLE[i])
	RLT_CHIMSTATION<-merge(RLT_CHIMSTATION,TEMPP,by="STATION", all.x=TRUE)
	rm(TEMPP)
}

# Ajout colonnes des paramètres 
for (i in  1:nrow(PARAGROUPCHIM)  ) {
	TEMPP<-CHIMCMAMA[CHIMCMAMA$PARAGROUP==PARAGROUPCHIM$PARAGROUP[i],c("STATION", "CLASSEETAT")]
	names(TEMPP)[2]<-PARAGROUPCHIM$PARAGROUPLIBCOURT[i]
	RLT_CHIMSTATION<-merge(RLT_CHIMSTATION,TEMPP,by="STATION", all.x=TRUE)
	rm(TEMPP)
}

#### METTRE TOUT EN MAJUSCULE
names(RLT_CHIMSTATION)<-toupper(names(RLT_CHIMSTATION))
gc()


###########################################
####RAJOUT DES PARAMETRES DECLASSANTS
#### DU NOMBRE DE PARAMETRE DECLASSANT
## DU NOMBRE DE PARAMETRE INDETERMINE
## PPL, 24/09/2015
##########################################


	### Fonction pour sortir les parametres déclassants 
	FUNC_DECLASS<-function(RLT_FINAL, ETAT, LIST_PARAM) {
		COLETAT<-RLT_FINAL[,ETAT]
		MAT<-as.matrix(RLT_FINAL[,LIST_PARAM])
		colnames(MAT)<-LIST_PARAM
		C1<-MAT >= COLETAT & COLETAT %in% c(2) 
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

NAME_ETAT<-"ETATCHIM"
NAMES_PARAMCHIM<-toupper(PARAGROUPCHIM$PARAGROUPLIBCOURT)[toupper(PARAGROUPCHIM$PARAGROUPLIBCOURT) %in% names(RLT_CHIMSTATION)]
RLT_CHIMSTATION$PARAMDECLASS<-FUNC_DECLASS(RLT_CHIMSTATION,NAME_ETAT, NAMES_PARAMCHIM)


#nb de declass
temp<- unlist(lapply(gregexpr(";",RLT_CHIMSTATION$PARAMDECLASS,TRUE),length))
temp2<-unlist(gregexpr(";",RLT_CHIMSTATION$PARAMDECLASS[temp == 1],TRUE)) == -1
RLT_CHIMSTATION$N_CHIMDECLASS<-temp+1
RLT_CHIMSTATION$N_CHIMDECLASS[temp == 1][temp2]<- 1
RLT_CHIMSTATION$N_CHIMDECLASS[RLT_CHIMSTATION$PARAMDECLASS == "" ]<-0


#Nombre de parametres indéterminés	
temp<-rep(0,nrow(RLT_CHIMSTATION))
for ( i in NAMES_PARAMCHIM) {
	temp[is.na(RLT_CHIMSTATION[,i]) | RLT_CHIMSTATION[,i] == 0 ]<-temp[is.na(RLT_CHIMSTATION[,i]) | RLT_CHIMSTATION[,i] == 0 ]+1
}
RLT_CHIMSTATION$N_PARAM_IND<-temp

#Nombre de parametres "BON"	
temp<-rep(0,nrow(RLT_CHIMSTATION))
for ( i in NAMES_PARAMCHIM) {
	temp[!is.na(RLT_CHIMSTATION[,i]) & RLT_CHIMSTATION[,i] == 1 ]<-temp[!is.na(RLT_CHIMSTATION[,i]) & RLT_CHIMSTATION[,i] == 1 ]+1
}
RLT_CHIMSTATION$N_PARAM_BON<-temp


# Ajout de la colonne "REPRESENTATIVE"
RLT_CHIMSTATION<-merge(RLT_CHIMSTATION, STATION[,c("STATION","REPRESENTATIVE","ECHELLESTA")], by="STATION", all.x=TRUE)
gc()





 	## RAJOUTER STATIONTXT 

	RLT_CHIMSTATION_NAMES<-names(RLT_CHIMSTATION)
	if (nchar(as.character(RLT_CHIMSTATION$STATION))[1] == 7 & !is.na(RLT_CHIMSTATION$STATION[1])) {
			RLT_CHIMSTATION$STATIONTXT<-as.character(RLT_CHIMSTATION$STATION)
			RLT_CHIMSTATION$STATIONTXT[nchar(as.character(RLT_CHIMSTATION$STATION)) == 7 & !is.na(RLT_CHIMSTATION$STATION)]<-paste("0",RLT_CHIMSTATION$STATION[nchar(as.character(RLT_CHIMSTATION$STATION)) == 7 & !is.na(RLT_CHIMSTATION$STATION)],sep="")
			RLT_CHIMSTATION<-RLT_CHIMSTATION[,c("STATIONTXT",RLT_CHIMSTATION_NAMES)]
			RLT_CHIMSTATION_NAMES<-names(RLT_CHIMSTATION)
	}


gc()
flush.console()


###################################
# CALCUL ETAT CHIMIQUE - MASSE D'EAU
###################################
if (SEEECHIMME=="oui") {

	#ETAT AVEC HAPetUBI
		TEMPECHIMME<-merge(CHIMCMAMA[,c("STATION", "PARAGROUP","CLASSEETAT")], STATION[,c("STATION", "EUCD", "REPRESENTATIVE")], by="STATION")
		TEMPECHIMME_OUI<-TEMPECHIMME[TEMPECHIMME$REPRESENTATIVE %in% c("oui"),]
		TEMPECHIMME_TEMPO<-TEMPECHIMME[TEMPECHIMME$REPRESENTATIVE %in% c("temporaire"),]
		
		# Calcul de l'état ME pour les stations représentatives
		TEMPECHIMME_CAS1<-aggregate(CLASSEETAT ~ EUCD + REPRESENTATIVE, data = TEMPECHIMME_OUI, max)
		
		# Calcul de l'état ME pour les stations temporaires s'il n'y a pas de station représentative
		if (nrow(TEMPECHIMME_TEMPO)>0) {
			TEMPECHIMME_CAS2<-aggregate(CLASSEETAT ~ EUCD + REPRESENTATIVE, data = TEMPECHIMME_TEMPO, max)
			CHIMETATME<-rbind(TEMPECHIMME_CAS1,TEMPECHIMME_CAS2[!(TEMPECHIMME_CAS2$EUCD %in% c(TEMPECHIMME_CAS1$EUCD)),])
			rm(TEMPECHIMME_CAS2)
		} else {
			CHIMETATME<-TEMPECHIMME_CAS1
		}
	
		names(CHIMETATME)[3]<-"ETATCHIMME"
		
		
		
	#ETAT SANS HAPetUBI
		TEMPECHIMMESHAP<-merge(CHIMHORSHAP[,c("STATION","ETATCHIM_SSHAPUBI")], STATION[,c("STATION", "EUCD", "REPRESENTATIVE")], by="STATION")
		TEMPECHIMMESHAP_OUI<-TEMPECHIMMESHAP[TEMPECHIMMESHAP$REPRESENTATIVE %in% c("oui"),]
		TEMPECHIMMESHAP_TEMPO<-TEMPECHIMMESHAP[TEMPECHIMMESHAP$REPRESENTATIVE %in% c("temporaire"),]
		
		# Calcul de l'état ME pour les stations représentatives
		TEMPECHIMMESHAP_CAS1<-aggregate(ETATCHIM_SSHAPUBI ~ EUCD + REPRESENTATIVE, data = TEMPECHIMMESHAP_OUI, max)
		
		# Calcul de l'état ME pour les stations temporaires s'il n'y a pas de station représentative
		if (nrow(TEMPECHIMMESHAP_TEMPO)>0) {
			TEMPECHIMMESHAP_CAS2<-aggregate(ETATCHIM_SSHAPUBI ~ EUCD + REPRESENTATIVE, data = TEMPECHIMMESHAP_TEMPO, max)
			CHIMETATMESHAP<-rbind(TEMPECHIMMESHAP_CAS1,TEMPECHIMMESHAP_CAS2[!(TEMPECHIMMESHAP_CAS2$EUCD %in% c(TEMPECHIMMESHAP_CAS1$EUCD)),])
			rm(TEMPECHIMMESHAP_CAS2)
		} else {
			CHIMETATMESHAP<-TEMPECHIMMESHAP_CAS1
		}
	
		names(CHIMETATMESHAP)[3]<-"ETATCHIMME_SSHAPUBI"	
	
	#on rappatri les infos de ETATCHIMME_SSHAPUBI
	CHIMETATME$ID<-paste(CHIMETATME$EUCD, CHIMETATME$REPRESENTATIVE)
	CHIMETATMESHAP$ID<-paste(CHIMETATMESHAP$EUCD, CHIMETATMESHAP$REPRESENTATIVE)		
	CHIMETATME<-merge(CHIMETATME,CHIMETATMESHAP[,c("ID","ETATCHIMME_SSHAPUBI")], by = "ID", all = TRUE)

	
###################################
# CALCUL ETAT CHIMIQUE - NIVEAU DE CONFIANCE
###################################
	CHIMETATME$NIVCONF<-as.numeric(NA)
	CHIMETATME$CAS<-as.character(NA)
	
	# Préparation des données pour l'estimation des % et data dispo (BENZO+INDENO & DEHP)
	NBPARAGLOBAL<-aggregate(PARAGROUP ~ FAMILLE, data = PARAGROUPCHIM , length)
	names(NBPARAGLOBAL)[2]<-"NBTOTAL"
	NBPARAGLOBAL$GLOBAL<-"station"
	NBPARAGLOBALSTAT<-aggregate(NBTOTAL ~ GLOBAL , data = NBPARAGLOBAL , sum)
	
	TEMPOCHIMSTATGLOB<-aggregate(PARAGROUP ~ STATION + CLASSEETAT, data = CHIMCMAMA , length)
	names(TEMPOCHIMSTATGLOB)[3]<-"NBPARA"
	# calcul ratio % global
	TEMPOCHIMSTATGLOB<-cbind(TEMPOCHIMSTATGLOB,NBPARAGLOBALSTAT)
	TEMPOCHIMSTATGLOB$RATIO<-round((TEMPOCHIMSTATGLOB$NBPARA/TEMPOCHIMSTATGLOB$NBTOTAL)*100,0)
	TEMPOCHIMSTATGLOB$ID<-paste(TEMPOCHIMSTATGLOB$STATION,TEMPOCHIMSTATGLOB$CLASSEETAT,sep="_")
	
	#selection DATA pour calcul du niveau de confiance
	TEMPORATIOBON<-TEMPOCHIMSTATGLOB[c(TEMPOCHIMSTATGLOB$CLASSEETAT==1),c("STATION", "RATIO","CLASSEETAT")]
	
	TEMPODISPBENZO<-CHIMCMAMA[c(CHIMCMAMA$PARAGROUP %in% c("28_1","28_2","28_3","28_4","28_5") & CHIMCMAMA$CLASSEETAT==1) ,c("STATION","CLASSEETAT")]
	if (nrow(TEMPODISPBENZO) > 0){
	TEMPODISPBENZO$BENZO<-as.character("BE")} else {TEMPODISPBENZO$BENZO<-as.character()}
	
	TEMPODISPDEHP<-CHIMCMAMA[c(CHIMCMAMA$PARAGROUP=="12" & CHIMCMAMA$CLASSEETAT==1),c("STATION","CLASSEETAT")]
	if (nrow(TEMPODISPDEHP) > 0){
	TEMPODISPDEHP$DEHP<-as.character("BE")} else {TEMPODISPDEHP$DEHP<-as.character() }
	
	TEMPODISPPARA<-merge(TEMPODISPBENZO,TEMPODISPDEHP,by="STATION", all.x=TRUE, all.y=TRUE)
	
	TEMPORATIOBON<-merge(TEMPORATIOBON,TEMPODISPPARA[,c("STATION","BENZO","DEHP")], by="STATION", all.x=TRUE)
	
	# Ajout du code station au résultat masse d'eau necessaire pour info sur le niveau de confiance	
	CHIMETATME$ID<-paste(CHIMETATME$EUCD,CHIMETATME$ETATCHIMME,CHIMETATME$REPRESENTATIVE,sep="_")
	TEMPSTATION<-RLT_CHIMSTATION[,c("STATION","ETATCHIM")]
	TEMPSTATION<-merge(TEMPSTATION,STATION[,c("STATION", "EUCD", "REPRESENTATIVE")],by="STATION")
	TEMPSTATION$ID<-paste(TEMPSTATION$EUCD,TEMPSTATION$ETATCHIM,TEMPSTATION$REPRESENTATIVE,sep="_")
	CHIMETATME<-merge(CHIMETATME,TEMPSTATION[,c("ID", "STATION","REPRESENTATIVE")],by="ID")
		
	# Calcul niveau de confiance
	CHIMETATME$NIVCONF[CHIMETATME$ETATCHIMME==2]<-3
	CHIMETATME$CAS[CHIMETATME$ETATCHIMME==2]<-"Q1"
	CHIMETATME$NIVCONF[CHIMETATME$ETATCHIMME==1 & CHIMETATME$STATION %in% TEMPORATIOBON$STATION[TEMPORATIOBON$RATIO>=80 & !is.na(TEMPORATIOBON$BENZO) & !is.na(TEMPORATIOBON$DEHP)]]<-3
	CHIMETATME$CAS[CHIMETATME$ETATCHIMME==1 & CHIMETATME$STATION %in% TEMPORATIOBON$STATION[TEMPORATIOBON$RATIO>=80 & !is.na(TEMPORATIOBON$BENZO) & !is.na(TEMPORATIOBON$DEHP)]]<-"Q2"
	CHIMETATME$NIVCONF[CHIMETATME$ETATCHIMME==1 & CHIMETATME$STATION %in% TEMPORATIOBON$STATION[TEMPORATIOBON$RATIO>=50 & TEMPORATIOBON$RATIO <80 & !is.na(TEMPORATIOBON$BENZO) & !is.na(TEMPORATIOBON$DEHP)]]<-2
	CHIMETATME$CAS[CHIMETATME$ETATCHIMME==1 & CHIMETATME$STATION %in% TEMPORATIOBON$STATION[TEMPORATIOBON$RATIO>=50 & TEMPORATIOBON$RATIO <80 & !is.na(TEMPORATIOBON$BENZO) & !is.na(TEMPORATIOBON$DEHP)]]<-"Q3"
	CHIMETATME$NIVCONF[CHIMETATME$ETATCHIMME==1 & CHIMETATME$STATION %in% TEMPORATIOBON$STATION[TEMPORATIOBON$RATIO <50]]<-1
	CHIMETATME$CAS[CHIMETATME$ETATCHIMME==1 & CHIMETATME$STATION %in% TEMPORATIOBON$STATION[TEMPORATIOBON$RATIO <50]]<-"Q4"
	CHIMETATME$NIVCONF[CHIMETATME$ETATCHIMME==1 & is.na(CHIMETATME$NIVCONF) ]<-1
	CHIMETATME$CAS[CHIMETATME$ETATCHIMME==1 & is.na(CHIMETATME$CAS)]<-"Q5"
	CHIMETATME$NIVCONF[CHIMETATME$ETATCHIMME==0]<-0
	CHIMETATME$CAS[CHIMETATME$ETATCHIMME==0]<-"Q6"
	rm(TEMPOCHIMSTATGLOB,TEMPODISPBENZO,TEMPODISPDEHP,TEMPODISPPARA,TEMPORATIOBON)
	gc()

################################
# TABLE FINALE - mise en forme
###############################
	
	RLT_CHIMETATME<-CHIMETATME[,c("EUCD","ETATCHIMME","ETATCHIMME_SSHAPUBI","NIVCONF","CAS","STATION")]
	RLT_CHIMETATME<-merge(RLT_CHIMETATME,RLT_CHIMSTATION,by="STATION")
	NAMES_RLTSTATION<-names(RLT_CHIMSTATION[,c(3:ncol(RLT_CHIMSTATION))])
	RLT_CHIMETATME<-RLT_CHIMETATME[,c("EUCD","ETATCHIMME","ETATCHIMME_SSHAPUBI","NIVCONF","CAS","STATION",NAMES_RLTSTATION)]
	RLT_CHIMETATME<-RLT_CHIMETATME[order(RLT_CHIMETATME$EUCD),]
	RLT_CHIMETATME<-merge(RLT_CHIMETATME,MASSEDEAU[,c("EUCD","NAME","MODIFIED","ARTIFICIAL","ECHELLEME")],by = "EUCD",all.x = TRUE)
	
	

	#PPL, 24/09/20145
	# Pour supprimer les doublons de ME les règles seraient les suivantes :  
	# •	Sélection de la ligne avec l’état chimique le plus déclassant 
	# •	Si même état : sélection de la ligne avec le meilleur niveau de confiance
	# •	Si même niveau de confiance : sélection de la ligne avec le plus de paramètres déclassant
	# •	Si même nombre de paramètres déclassant : sélection de la ligne avec moins d’état paramètre inconnu 
	# •	Si idem : sélection au hasard.
	
	print(nrow(RLT_CHIMETATME))
	RLT_CHIMETATME<-RLT_CHIMETATME[order(RLT_CHIMETATME$EUCD, -RLT_CHIMETATME$ETATCHIMME, RLT_CHIMETATME$NIVCONF,  -RLT_CHIMETATME$N_CHIMDECLASS, RLT_CHIMETATME$N_PARAM_IND ),]
	RLT_CHIMETATME<-RLT_CHIMETATME[!duplicated(RLT_CHIMETATME[ ,c("EUCD")]),]
	print(nrow(RLT_CHIMETATME))
	
	
	rm(TEMPECHIMME_OUI,TEMPECHIMME_TEMPO,TEMPECHIMME_CAS1,TEMPSTATION)
	gc()

	flush.console()
	
	
	#### METTRE TOUT EN MAJUSCULE
	names(RLT_CHIMETATME)<-toupper(names(RLT_CHIMETATME))
	
	
	#############################################
	## RAJOUTER STATIONTXT 
	RLT_CHIMETATME_NAMES<-names(RLT_CHIMETATME)
	if (nchar(as.character(RLT_CHIMETATME$STATION))[1] == 7 & !is.na(RLT_CHIMETATME$STATION[1])) {
			RLT_CHIMETATME$STATIONTXT<-as.character(RLT_CHIMETATME$STATION)
			RLT_CHIMETATME$STATIONTXT[nchar(as.character(RLT_CHIMETATME$STATION)) == 7 & !is.na(RLT_CHIMETATME$STATION)]<-paste("0",RLT_CHIMETATME$STATION[nchar(as.character(RLT_CHIMETATME$STATION)) == 7 & !is.na(RLT_CHIMETATME$STATION)],sep="")
			RLT_CHIMETATME<-RLT_CHIMETATME[,c(RLT_CHIMETATME_NAMES,"STATIONTXT")]
			RLT_CHIMETATME_NAMES<-names(RLT_CHIMETATME)
	}

	
	
}

gc()










