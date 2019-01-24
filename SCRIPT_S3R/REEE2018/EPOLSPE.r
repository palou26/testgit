##############################################
## SEEE - COURS D'EAU : état polluants spécifiques
## Application de l'arrêté de janvier 2016
##############################################
# load("D:\\_ETUDES\\E4309_AESN_S3R_Adapt2016\\S3R_DEV\\S3R\\imageMon-Dec-05-17_03_08-2016.Rdata")

#SAVE()
##############################
## Mise en forme des données
##############################
gc()
DATAPOLSPE$IDAN<-as.character(paste0(DATAPOLSPE$STATION,DATAPOLSPE$PARAMETRE,DATAPOLSPE$ANNEE))


######################################################################
## METHODE : choix de la méthode pour la preparation des données brutes
# ANNEERECENTE : extraction des données de l'année la + récente pour chaque paramètre de la station
# ANNEECHRONIQUE : extraction des données d'une station pour l'ensemble des paramètres ayant la chronique la plus complète
# NOFILTREANNEE : on ne fait aucun filtre sur l'année et on prend en compte tout le jeu de données
######################################################################

if (METHODEMOYPS =="NOFILTREANNEE") {
	PSANNEEmax<-aggregate(ANNEE ~ STATION + PARAMETRE, data = DATAPOLSPE , max)
	PSANNEEmin<-aggregate(ANNEE ~ STATION + PARAMETRE, data = DATAPOLSPE , min)
	PSANNEEmax$IDAN<-as.character(paste0(PSANNEEmax$STATION,PSANNEEmax$PARAMETRE))
	PSANNEEmin$IDAN<-as.character(paste0(PSANNEEmin$STATION,PSANNEEmin$PARAMETRE))
	PSANNEE<-merge(PSANNEEmin,PSANNEEmax[,c("IDAN","ANNEE")], by = "IDAN")
	PSANNEE$ANNEE<-PSANNEE$ANNEE.x
	condAnnee<-PSANNEE$ANNEE.x != PSANNEE$ANNEE.y
	PSANNEE$ANNEE[condAnnee]<-paste0(PSANNEE$ANNEE.x[condAnnee],"-",PSANNEE$ANNEE.y[condAnnee])
}

if (METHODEMOYPS =="ANNEERECENTE") {
	PSANNEE<-aggregate(ANNEE ~ STATION + PARAMETRE, data = DATAPOLSPE , max)
	PSANNEE$IDAN<-as.character(paste0(PSANNEE$STATION,PSANNEE$PARAMETRE,PSANNEE$ANNEE))
	DATAPOLSPE<-DATAPOLSPE[DATAPOLSPE$IDAN %in% PSANNEE$IDAN,]
} else if (METHODEMOYPS =="ANNEECHRONIQUE") {
	PSANNEE<-aggregate(RESULTAT ~ STATION + PARAMETRE + ANNEE, data = DATAPOLSPE , length)
	PSANNEE$IDAN<-as.character(paste0(PSANNEE$STATION,PSANNEE$PARAMETRE,PSANNEE$ANNEE))
	PSANNEE$IDFREQ<-as.character(paste0(PSANNEE$STATION,PSANNEE$PARAMETRE,PSANNEE$RESULTAT))
	PSANNEE<-PSANNEE[order(PSANNEE$IDAN),]
	
	TEMPPSANNEE1<-aggregate(RESULTAT ~ STATION + PARAMETRE, data = PSANNEE , max) # colonne RESULTAT non renommée mais contient le nb de prelevement
	TEMPPSANNEE1$IDFREQ<-as.character(paste0(TEMPPSANNEE1$STATION,TEMPPSANNEE1$PARAMETRE,TEMPPSANNEE1$RESULTAT))
	PSANNEE<-PSANNEE[PSANNEE$IDFREQ %in% c(TEMPPSANNEE1$IDFREQ),]
	
	#extraction de l'année la + récente si plusieurs années ont les mêmes fréquences
	TEMPPSANNEE2<-aggregate(ANNEE ~ STATION + PARAMETRE, data = PSANNEE , max)
	TEMPPSANNEE2$IDAN<-as.character(paste0(TEMPPSANNEE2$STATION,TEMPPSANNEE2$PARAMETRE,TEMPPSANNEE2$ANNEE))
	DATAPOLSPE<-DATAPOLSPE[DATAPOLSPE$IDAN %in% c(TEMPPSANNEE2$IDAN),]
	
	#on recrée PSANNEE depuis DATAPOLSPE filtré pour calculer les fréquences. PSANNEE est ainsi identique à "ANNEERECENTE" dans FREQUENCE.r --> ça facilite le script et les éventuels modif.
	PSANNEE<-aggregate(ANNEE ~ STATION + PARAMETRE, data = DATAPOLSPE , max)
	 
	rm(TEMPPSANNEE1,TEMPPSANNEE2)
}

#################
# CAS DU XYLENE
################

DATAPOLSPE_XYL<-DATAPOLSPE[DATAPOLSPE$PARAMETRE %in% c(1780,1293,1294,1292,2925),]
DATAPOLSPE_XYL$LQ[is.na(DATAPOLSPE_XYL$LQ)]<-DATAPOLSPE_XYL$RESULTAT[is.na(DATAPOLSPE_XYL$LQ)]
print(nrow(DATAPOLSPE_XYL))

	# on somme 1293 + 1294 = 2925
DATAPOLSPE_1293_1294<-DATAPOLSPE_XYL[DATAPOLSPE_XYL$PARAMETRE %in% c(1293,1294),]
if(nrow(DATAPOLSPE_1293_1294) > 0){
	DATAPOLSPE_1293_1294_2<-aggregate(RESULTAT  ~ STATION+DATEPRELEV+HEUREPRELEV+REMARQUE+LQ+ANNEE+MOIS+FRACTION+UNITE, data = DATAPOLSPE_1293_1294, sum)
	DATAPOLSPE_1293_1294_2$PARAMETRE<-"2925"
	DATAPOLSPE_XYL<-merge(DATAPOLSPE_XYL, DATAPOLSPE_1293_1294_2,all=TRUE)
	DATAPOLSPE_XYL<-DATAPOLSPE_XYL[order(-DATAPOLSPE_XYL$RESULTAT),]
	DATAPOLSPE_XYL<-DATAPOLSPE_XYL[!duplicated(DATAPOLSPE_XYL[,c("STATION","DATEPRELEV","HEUREPRELEV","PARAMETRE")]),]
	print(nrow(DATAPOLSPE_XYL))
}
	
	# on somme 2925 + 1292 = 1780
DATAPOLSPE_2925_1292<-DATAPOLSPE_XYL[DATAPOLSPE_XYL$PARAMETRE %in% c(2925,1292),]
if(nrow(DATAPOLSPE_2925_1292) > 0){
	DATAPOLSPE_2925_1292_2<-aggregate(RESULTAT  ~ STATION+DATEPRELEV+HEUREPRELEV+REMARQUE+LQ+ANNEE+MOIS+FRACTION+UNITE, data = DATAPOLSPE_2925_1292, sum)
	DATAPOLSPE_2925_1292_2$PARAMETRE<-"1780"
	DATAPOLSPE_XYL<-merge(DATAPOLSPE_XYL, DATAPOLSPE_2925_1292_2,all=TRUE)
	DATAPOLSPE_XYL<-DATAPOLSPE_XYL[order(-DATAPOLSPE_XYL$RESULTAT),]
	DATAPOLSPE_XYL<-DATAPOLSPE_XYL[!duplicated(DATAPOLSPE_XYL[,c("STATION","DATEPRELEV","HEUREPRELEV","PARAMETRE")]),]
	print(nrow(DATAPOLSPE_XYL))
}
DATAPOLSPE_XYL<-DATAPOLSPE_XYL[DATAPOLSPE_XYL$PARAMETRE == "1780",names(DATAPOLSPE)]	
print(nrow(DATAPOLSPE_XYL))

	#finalisation
DATAPOLSPE<-rbind(
	DATAPOLSPE[!(DATAPOLSPE$PARAMETRE %in% c(1780,1293,1294,1292,2925)),]
	, DATAPOLSPE_XYL)
	

##############################
## Calcul de l'état des polluants spécifiques
##############################
# requete préparatoire
POLSPE<-merge(DATAPOLSPE,LISTECODERQE[,c("REMARQUE","QUANTIFIE")],by="REMARQUE")
POLSPE<-merge(POLSPE,PARAMETREPOLSPE[,c("PARAMETRE","NQEMA","NQEMA2")],by="PARAMETRE")
gc()

# 5/12/2016 : on retire les LQABERANTE
POLSPE<-merge(POLSPE, PARAMETREPOLSPE[,c("PARAMETRE","LQSEUIL")], by="PARAMETRE", all.x = TRUE)
if ( SUPPR_LQ_ABERRANTE_PS== "oui" ) {


	if (LQ_NONDISPO =="oui") { # si LQ non dispo on prend les valeurs du champ RESULTAT lorsque non quantifié --> car c'est la LQ
		PBLQPSABERRANTE<-POLSPE[!is.na(POLSPE$LQSEUIL) & POLSPE$RESULTAT>POLSPE$LQSEUIL & POLSPE$QUANTIFIE=="0" & POLSPE$LQSEUIL != -99 ,]
		POLSPE<-POLSPE[(!(!is.na(POLSPE$LQSEUIL) & POLSPE$RESULTAT>POLSPE$LQSEUIL & POLSPE$QUANTIFIE=="0"))| POLSPE$LQSEUIL == -99 | is.na(POLSPE$LQSEUIL) ,]
	} else {
		PBLQPSABERRANTE<-POLSPE[!is.na(POLSPE$LQSEUIL) & POLSPE$LQ>POLSPE$LQSEUIL  & POLSPE$LQSEUIL != -99  ,]
		POLSPE<-POLSPE[(!(!is.na(POLSPE$LQSEUIL) & POLSPE$LQ>POLSPE$LQSEUIL))| POLSPE$LQSEUIL == -99 | is.na(POLSPE$LQSEUIL)  ,]
	}

	# if (LQ_NONDISPO =="oui") { # si LQ non dispo on prend les valeurs du champ RESULTAT lorsque non quantifié --> car c'est la LQ
		# PBLQPSABERRANTE<-POLSPE[POLSPE$RESULTAT>POLSPE$LQSEUIL & POLSPE$QUANTIFIE=="0" & POLSPE$LQSEUIL != -99 ,]
		# POLSPE<-POLSPE[!(POLSPE$RESULTAT>POLSPE$LQSEUIL & POLSPE$QUANTIFIE=="0") | POLSPE$LQSEUIL == -99 ,]
	# } else {
		# PBLQPSABERRANTE<-POLSPE[POLSPE$LQ > POLSPE$LQSEUIL & POLSPE$LQSEUIL != -99 ,]
		# POLSPE<-POLSPE[POLSPE$LQ <= POLSPE$LQSEUIL | POLSPE$LQSEUIL == -99  ,]
	# }

	if( nrow(PBLQPSABERRANTE)>0) {
		CSV<-paste(CH_ERREUR,"PS_LQ_ABERRANTE_",SEEE_DEBformat,".csv",sep="")
		write.csv2(PBLQPSABERRANTE,CSV)
		MSGBOX <- tkmessageBox(title = "Info", message = paste("Les données brutes avec LQ aberrantes (strictement > LQSEUIL) ont été retirées \n du calcul de l'état chimique et exportées dans \n",CSV,sep=""), icon = "info", type = "ok")
	}

	if (nrow(POLSPE) == 0) {
			MSGBOX <- tkmessageBox(title = "Info", message = paste("Pas de données à traiter après le retrait des mesures avec LQ aberrantes \n",CSV,sep=""), icon = "info", type = "ok")
			tcl("update")
			source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
	}	
}


# 27/03/2018 :  traitement des LQ à zéro
# cond 1 : Si LQ = 0 et CodeRemarque = (10,2ou7) et Résultat >= 0 alors LQ = Résultat puis Résultat = LQ/2.
# Si LQ= 0 et CodeRemarque = 1 et Résultat > 0, alors ne rien changer
# cond3 : Si LQ= 0 et CodeRemarque = 1 et Résultat = 0, alors retirer l'analyse avec un message d'alerte
cond1<-POLSPE$LQ == 0 & POLSPE$RESULTAT >= 0 & POLSPE$REMARQUE %in% c(2,7,10)
POLSPE$LQ[cond1]<-POLSPE$RESULTAT[cond1]

cond3<-POLSPE$LQ == 0 & POLSPE$RESULTAT == 0 & POLSPE$REMARQUE == 1

	if( nrow(POLSPE[cond3,])>0) {
		CSV<-paste(CH_ERREUR,"POLSPE_LQ_ZERO_QUANTIFIEE_",SEEE_DEBformat,".csv",sep="")
		write.csv2(POLSPE[cond3,],CSV)
		POLSPE<-POLSPE[!cond3,]
		MSGBOX <- tkmessageBox(title = "Info", message = paste("Les", nrow(POLSPE[cond3,])  ," analyses avec 'LQ= 0 et CodeRemarque = 1 et Résultat = 0' \n ont été retirées  du calcul de l'état chimique et exportées dans \n",CSV,sep=""), icon = "info", type = "ok")
	}

	if (nrow(POLSPE) == 0) {
			MSGBOX <- tkmessageBox(title = "Info", message = paste("Pas de données à traiter après le retrait des mesures avec LQ aberrantes \n",CSV,sep=""), icon = "info", type = "ok")
			tcl("update")
			source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
	}
gc()




# Détermination de NQEMA selon durete et Fond géochim

gc()
POLSPE<-merge(POLSPE,STATION[,c("STATION","FONDGEO_ARSENIC","FONDGEO_CHROME","FONDGEO_CUIVRE","FONDGEO_ZINC","DURETE")],by="STATION", all.x=TRUE)
POLSPE$ID<-paste(POLSPE$STATION, POLSPE$PARAMETRE, sep="_")
gc()

CONDDUR<-POLSPE$PARAMETRE=="1383" & POLSPE$DURETE>24 & !is.na(POLSPE$DURETE)
POLSPE$NQEMA[CONDDUR]<-POLSPE$NQEMA2[CONDDUR]

condarsenic<-POLSPE$PARAMETRE=="1369" & !is.na(POLSPE$FONDGEO_ARSENIC)
POLSPE$NQEMA[condarsenic]<-POLSPE$NQEMA[condarsenic]+POLSPE$FONDGEO_ARSENIC[condarsenic]
condchrome<-POLSPE$PARAMETRE=="1389" & !is.na(POLSPE$FONDGEO_CHROME)
POLSPE$NQEMA[condchrome]<-POLSPE$NQEMA[condchrome]+POLSPE$FONDGEO_CHROME[condchrome]
condcuivre<-POLSPE$PARAMETRE=="1392" & !is.na(POLSPE$FONDGEO_CUIVRE)
POLSPE$NQEMA[condcuivre]<-POLSPE$NQEMA[condcuivre]+POLSPE$FONDGEO_CUIVRE[condcuivre]
condzinc<-POLSPE$PARAMETRE=="1383" & !is.na(POLSPE$FONDGEO_ZINC)
POLSPE$NQEMA[condzinc]<-POLSPE$NQEMA[condzinc]+POLSPE$FONDGEO_ZINC[condzinc]
rm(condarsenic,condchrome,condcuivre,condzinc)






# Modification suite à la publiation de l'arreté et prestation C43 AELB 17 dec 
POLSPE$RESULTAT2<-POLSPE$RESULTAT
#Si  on est pas quantifié 
if (LQ_NONDISPO =="oui") {
	cond<- POLSPE$QUANTIFIE=="0" 
	POLSPE$RESULTAT2[cond]<-POLSPE$RESULTAT[cond]/2
} else {
	cond<- POLSPE$QUANTIFIE=="0" | POLSPE$RESULTAT <= POLSPE$LQ
	POLSPE$RESULTAT2[cond]<-POLSPE$LQ[cond]/2
}

# on créé un dataframe qui donne par station et par parametre la valeur de NQEMA (sans doublons)
POLSPE_NQEMA<-POLSPE[!duplicated(POLSPE[,c("STATION","PARAMETRE")]),c("STATION","PARAMETRE","NQEMA")]
POLSPE_NQEMA$ID<-paste0(POLSPE_NQEMA$STATION,"_",POLSPE_NQEMA$PARAMETRE)


#modif 24/04/15 : retrait prelevement pour calcul moyenne si LQ > NQE (mail Ministere du 08/04/15 à destination des agences pour cycle 2
if ( LQSUPNQEMAPS == "oui" ) {
	if (LQ_NONDISPO =="oui") { #si LQ non dispo on prend les valeurs du champ RESULTAT lorsque non quantifié --> car c'est la LQ
		PBLQPS<-POLSPE[POLSPE$RESULTAT>POLSPE$NQEMA & POLSPE$QUANTIFIE=="0" ,]
		POLSPE<-POLSPE[!(POLSPE$RESULTAT>POLSPE$NQEMA & POLSPE$QUANTIFIE=="0"),]
	} else {
		PBLQPS<-POLSPE[POLSPE$LQ > POLSPE$NQEMA & POLSPE$QUANTIFIE=="0" ,]
		POLSPE<-POLSPE[!(POLSPE$LQ > POLSPE$NQEMA & POLSPE$QUANTIFIE=="0" ),]
	}
	
	if( nrow(PBLQPS)>0) {
		CSV<-paste(CH_ERREUR,"PS_LQ_SUP_NQEMA_",SEEE_DEBformat,".csv",sep="")
		write.csv2(PBLQPS,CSV)
		MSGBOX <- tkmessageBox(title = "Info", message = paste("Les données brutes avec LQ > NQEMA ont été retirées \n du calcul du calcul de la concentration moyenne et exportées dans \n",CSV,sep=""), icon = "info", type = "ok")
	}	
}


# Calcul de la moyenne
POLSPE$MOY<-POLSPE$RESULTAT2
POLSPE_MOY<-aggregate(cbind(MOY,RESULTAT)~ STATION + PARAMETRE, data = POLSPE , mean)
POLSPE_MOY$MOY<-round(POLSPE_MOY$MOY,3)

POLSPE_MOY$ID<-paste0(POLSPE_MOY$STATION,"_",POLSPE_MOY$PARAMETRE)
gc()
POLSPE_MOY<-merge(POLSPE_MOY,POLSPE_NQEMA[,c("ID","NQEMA")],by="ID")
POLSPE_MOY<-merge(POLSPE_MOY,PARAMETREPOLSPE[,c("PARAMETRE","POLLUANT")],by="PARAMETRE")
gc()

# Extraction de la LQMAX par station pour comparaison à la MA
if (LQ_NONDISPO =="oui") { # recherche LQmax dans colonne resultat si colonne LQ non dispo
	LQMAX<-POLSPE[POLSPE$QUANTIFIE=="0", c("STATION","PARAMETRE","QUANTIFIE","RESULTAT")]
	LQMAX<-aggregate(RESULTAT~ STATION + PARAMETRE, data = LQMAX , max)
	names(LQMAX)[3]<-"LQMAX"
	LQMAX$LQMAX<-round(LQMAX$LQMAX,5)
	LQMAX$ID<-paste(LQMAX$STATION, LQMAX$PARAMETRE, sep="_")
} else { # recherche LQmax dans colonne LQ
	#LQMAX<-POLSPE[POLSPE$QUANTIFIE=="0", c("STATION","PARAMETRE","QUANTIFIE","RESULTAT")]
	LQMAX<-aggregate(LQ ~ STATION + PARAMETRE, data = POLSPE , max)
	names(LQMAX)[3]<-"LQMAX"
	LQMAX$ID<-paste(LQMAX$STATION, LQMAX$PARAMETRE, sep="_")
}

POLSPE_MOY<-merge(POLSPE_MOY,LQMAX[,c("ID","LQMAX")],by="ID", all.x=TRUE)
gc()



# Calcul de la frequence de prélèvement
POLSPE_FREQ<-aggregate(MOY ~ STATION + PARAMETRE, data = POLSPE , length)
names(POLSPE_FREQ)[3]<-"FREQ"
POLSPE_FREQ$ID<-as.character(paste0(POLSPE_FREQ$STATION,"_",POLSPE_FREQ$PARAMETRE))
POLSPE_FREQ<-POLSPE_FREQ[,c("ID","FREQ")]

POLSPE_MOY<-merge(POLSPE_MOY,POLSPE_FREQ,by="ID")
POLSPE_MOY<-POLSPE_MOY[order(POLSPE_MOY$STATION, POLSPE_MOY$PARAMETRE),]


# Calcul de la frequence de prélèvement avec mesure quantifiée
POLSPEFREQ<-POLSPE[,c("STATION","PARAMETRE")] #save de POLSPE avant retrait des freq non respectées pour les afficher dans les stat sur l'ensemble des paramètres
POLSPEFREQQUANTI<-POLSPE[POLSPE$QUANTIFIE == 1,c("STATION","PARAMETRE", "QUANTIFIE")]
gc()


#initialisation de CALSSEPS et CASPS
POLSPE_MOY$CLASSEPS<-as.numeric(NA)
POLSPE_MOY$CASPS<-""

## CAS 0 - Respect de la fréquence minimale de prélèvement
if (FREQOKPS == "oui") {
	POLSPE_MOY$CASPS[POLSPE_MOY$FREQ<NBANFREQ]<-"0"
	POLSPE_MOY$CLASSEPS[POLSPE_MOY$FREQ<NBANFREQ]<-0
	
		if ( LQSUPNQEMAPS == "oui" ) {
		if (nrow(PBLQPS) > 0) {
		PBLQPSFREQ<-data.frame(table(PBLQPS$STATION ,PBLQPS$PARAMETRE))
		names(PBLQPSFREQ)<-c("STATION","PARAMETRE","FREQPBLQ")
		PBLQPSFREQ<-PBLQPSFREQ[PBLQPSFREQ$FREQPBLQ > 0,]
		PBLQPSFREQ$ID<-paste(PBLQPSFREQ$STATION ,PBLQPSFREQ$PARAMETRE,sep="_")
		POLSPE_MOY$ID<-paste(POLSPE_MOY$STATION ,POLSPE_MOY$PARAMETRE,sep="_")
		POLSPE_MOY<-merge(POLSPE_MOY,PBLQPSFREQ,all=TRUE)
		#POLSPE_MOY<-merge(POLSPE_MOY,PBLQPSFREQ[,c("ID","FREQPBLQ")],by="ID",all=TRUE)
		POLSPE_MOY$FREQ[is.na(POLSPE_MOY$FREQ)]<-0
		POLSPE_MOY$FREQPBLQ[is.na(POLSPE_MOY$FREQPBLQ)]<-0
		#cas où si on avait conservé tous les prélevements on aurait atteint la frequence de prelevement
		POLSPE_MOY$CASPS[POLSPE_MOY$FREQ>0 & POLSPE_MOY$FREQ<NBANFREQ & POLSPE_MOY$FREQ + POLSPE_MOY$FREQPBLQ  >= NBANFREQ ]<-"0*"
		# cas où la frequence après retrait est à zero et on aurait quand même pas attend la frequence de prélevement si on avait conservé tous les prélevements
		POLSPE_MOY$CASPS[POLSPE_MOY$FREQ== 0 & POLSPE_MOY$FREQPBLQ  > 0 ]<-"0**"
		# cas où la frequence après retrait est à zero et on aurait  atteind la frequence de prélevement si on avait conservé tous les prélevements		
		POLSPE_MOY$CASPS[POLSPE_MOY$FREQ== 0 & POLSPE_MOY$FREQPBLQ  >= NBANFREQ ]<-"0***"
		POLSPE_MOY$CLASSEPS[POLSPE_MOY$CASPS %in% c("0","0*","0**","0***")]<-0
		print(table(POLSPE_MOY$CASPS))		
		}}
}

## CAS 0 - Respect de la fréquence minimale de prélèvement
if (FREQOKPS == "oui") {
	POLSPE_MOY$CASPS[POLSPE_MOY$FREQ<NBANFREQ & POLSPE_MOY$CASPS == ""]<-"0"
	POLSPE_MOY$CLASSEPS[POLSPE_MOY$FREQ<NBANFREQ  & POLSPE_MOY$CASPS == ""]<-0
	
	
	if ( SUPPR_LQ_ABERRANTE_PS == "oui" ) {
	if (nrow(PBLQPSABERRANTE) > 0) {
		PBLQPSABERRANTEFREQ<-data.frame(table(PBLQPSABERRANTE$STATION ,PBLQPSABERRANTE$PARAMETRE))
		names(PBLQPSABERRANTEFREQ)<-c("STATION","PARAMETRE","FREQPBLQ2")
		PBLQPSABERRANTEFREQ<-PBLQPSABERRANTEFREQ[PBLQPSABERRANTEFREQ$FREQPBLQ2 > 0,]
		PBLQPSABERRANTEFREQ$ID<-paste(PBLQPSABERRANTEFREQ$STATION ,PBLQPSABERRANTEFREQ$PARAMETRE,sep="_")
		POLSPE_MOY$ID<-paste(POLSPE_MOY$STATION ,POLSPE_MOY$PARAMETRE,sep="_")
		POLSPE_MOY<-merge(POLSPE_MOY,PBLQPSABERRANTEFREQ,all=TRUE)
		POLSPE_MOY$FREQ[is.na(POLSPE_MOY$FREQ)]<-0
		POLSPE_MOY$FREQPBLQ2[is.na(POLSPE_MOY$FREQPBLQ2)]<-0
		#cas où si on avait conservé tous les prélevements on aurait atteint la frequence de prelevement
		POLSPE_MOY$CASPS[POLSPE_MOY$FREQ>0 & POLSPE_MOY$FREQ<NBANFREQ & POLSPE_MOY$FREQ + POLSPE_MOY$FREQPBLQ2  >= NBANFREQ ]<-"0+"
		# cas où la frequence après retrait est à zero et on aurait quand même pas attend la frequence de prélevement si on avait conservé tous les prélevements
		POLSPE_MOY$CASPS[POLSPE_MOY$FREQ== 0 & POLSPE_MOY$FREQPBLQ2  > 0 ]<-"0++"
		# cas où la frequence après retrait est à zero et on aurait  atteind la frequence de prélevement si on avait conservé tous les prélevements		
		POLSPE_MOY$CASPS[POLSPE_MOY$FREQ== 0 & POLSPE_MOY$FREQPBLQ2  >= NBANFREQ ]<-"0+++"
		POLSPE_MOY$CLASSEPS[POLSPE_MOY$CASPS %in% c("0","0+","0++","0+++")]<-0
		print(table(POLSPE_MOY$CASPS))		
		}}
}



###CAS 1 -  la MOYENNE <= NQE 
condCAS1<-!(POLSPE_MOY$CASPS  %in% c("0","0+","0++","0+++","0*","0**","0***"))   &  (is.na(POLSPE_MOY$LQMAX) | POLSPE_MOY$LQMAX <= POLSPE_MOY$NQEMA)
	# Cas 1A
	condCAS1a<-condCAS1 & POLSPE_MOY$MOY <= POLSPE_MOY$NQEMA &  !is.na(POLSPE_MOY$LQMAX)
	POLSPE_MOY$CLASSEPS[condCAS1a]<-2  
	POLSPE_MOY$CASPS[condCAS1a ]<-"1A"
	# Cas 1B
	condCAS1c<-condCAS1 & POLSPE_MOY$MOY > POLSPE_MOY$NQEMA 
	POLSPE_MOY$CLASSEPS[condCAS1c]<-3  
	POLSPE_MOY$CASPS[condCAS1c ]<-"1B"
	
###CAS 2 -  la MOYENNE > NQE 
condCAS2<-POLSPE_MOY$CASPS != "0" & !is.na(POLSPE_MOY$LQMAX) & POLSPE_MOY$LQMAX > POLSPE_MOY$NQEMA
	# Cas 2A
	condCAS2a<-condCAS2 & POLSPE_MOY$MOY >=POLSPE_MOY$LQMAX
	POLSPE_MOY$CLASSEPS[condCAS2a]<-3  
	POLSPE_MOY$CASPS[condCAS2a ]<-"2A"
	# Cas 2B
	condCAS2b<-condCAS2 & POLSPE_MOY$MOY < POLSPE_MOY$LQMAX
	POLSPE_MOY$CLASSEPS[condCAS2b]<-0  
	POLSPE_MOY$CASPS[condCAS2b ]<-"2B"

###CAS 3 -  la LQMAX <= LQCIBLE (= LQSEUIL) on considère que c'est du très bon Etat
	NAM_POLSPE_MOY<-names(POLSPE_MOY)
	POLSPE_MOY<-merge(POLSPE_MOY, PARAMETREPOLSPE[,c("PARAMETRE","LQSEUIL")], by="PARAMETRE", all.x = TRUE)
	condCAS3<-POLSPE_MOY$MOY <= POLSPE_MOY$LQSEUIL & POLSPE_MOY$CLASSEPS == 2  
	POLSPE_MOY$CLASSEPS[condCAS3]<-1  
	POLSPE_MOY$CASPS[condCAS3 ]<-"1AA"
	POLSPE_MOY<-POLSPE_MOY[,NAM_POLSPE_MOY]
#verif
table(POLSPE_MOY$CASPS,	POLSPE_MOY$CLASSEPS)


# Calcul de l'état par type de polluant
POLSPE_POL<-aggregate(CLASSEPS ~ STATION + POLLUANT, data = POLSPE_MOY, max)
names(POLSPE_POL)[3]<-"ETATPOL" #renomme la colonne CLASSEPS sur laquelle il y a eu le max car correspond à l'état par type de polluants
POLSPE_POL<-POLSPE_POL[order(POLSPE_POL$STATION, POLSPE_POL$POLLUANT),]

# Calcul de l'état global des polluants spécifiques
# si les polluants NS sont exclus, l'état des PS est uniquement déterminé par les polluants synthétiques
if (EXCLU_POLNS=="oui") {
	RLT_POLSPE<-POLSPE_POL[POLSPE_POL$POLLUANT=="synth",c("STATION", "ETATPOL")]
	} else {
	RLT_POLSPE<-aggregate(ETATPOL ~ STATION, data = POLSPE_POL, max) # Résultat final des polluants spécifiques
	}
names(RLT_POLSPE)[2]<-"ETATPS" #renomme la colonne ETATPOL sur laquelle il y a eu le max car correspond à l'état global des polluants spécifiques
flush.console()

##############################
## Mise en forme tableau final
##############################
# Ajout colonnes des polluants 
for (i in  1:nrow(TABLEPOL)  ) {
	TEMPP<-POLSPE_POL[POLSPE_POL$POLLUANT==TABLEPOL$POLLUANT[i],c("STATION", "ETATPOL")]
	names(TEMPP)[2]<-TABLEPOL$POLLUANT[i]
	RLT_POLSPE<-merge(RLT_POLSPE,TEMPP,by="STATION", all.x=TRUE)
	rm(TEMPP)
}

# Ajout colonnes des paramètres 
PARAMETREPOLSPE<-PARAMETREPOLSPE[order(PARAMETREPOLSPE$IDTRI),]

for (i in  1:nrow(PARAMETREPOLSPE)  ) {
	TEMPP<-POLSPE_MOY[POLSPE_MOY$PARAMETRE==PARAMETREPOLSPE$PARAMETRE[i],c("STATION", "CLASSEPS")]
	names(TEMPP)[2]<-PARAMETREPOLSPE$PARAMETRELIB[i]
	RLT_POLSPE<-merge(RLT_POLSPE,TEMPP,by="STATION", all.x=TRUE)
	rm(TEMPP)
}

###Mise en forme des Cas
CAS_POLSPE<-data.frame(STATION = sort(unique(POLSPE_MOY$STATION)))
for (i in  1:nrow(PARAMETREPOLSPE)  ) {
	TEMPP<-POLSPE_MOY[POLSPE_MOY$PARAMETRE==PARAMETREPOLSPE$PARAMETRE[i],c("STATION", "CASPS")]
	names(TEMPP)[2]<-PARAMETREPOLSPE$PARAMETRELIB[i]
	CAS_POLSPE<-merge(CAS_POLSPE,TEMPP,by="STATION", all.x=TRUE)
	rm(TEMPP)
}


gc() ## compacte R
flush.console()
