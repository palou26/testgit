##############################################
## SEEE - COURS D'EAU : état polluants spécifiques
## Application de l'arrêté de janvier 2016
##############################################


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

##############################
## Calcul de l'état des polluants spécifiques
##############################
# requete préparatoire
POLSPE<-merge(DATAPOLSPE,LISTECODERQE[,c("REMARQUE","QUANTIFIE")],by="REMARQUE")
POLSPE<-merge(POLSPE,PARAMETREPOLSPE[,c("PARAMETRE","NQEMA","NQEMA2")],by="PARAMETRE")
gc()
POLSPE<-merge(POLSPE,STATION[,c("STATION","FONDGEO_ARSENIC","FONDGEO_CHROME","FONDGEO_CUIVRE","FONDGEO_ZINC","DURETE")],by="STATION", all.x=TRUE)
POLSPE$ID<-paste(POLSPE$STATION, POLSPE$PARAMETRE, sep="_")
gc()

# Détermination de NQEMA selon durete et Fond géochim
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
		PBLQPS<-POLSPE[POLSPE$LQ > POLSPE$NQEMA  ,]
		POLSPE<-POLSPE[POLSPE$LQ<=POLSPE$NQEMA,]
	}
	
	if( nrow(PBLQPS>0)) {
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


##30/09/2015 : On supprime les prélevements par PARAGROUP qui ont une valeur moyenne mesurée est inférieure à la LQ
# if ( LQSUPNQEMAPS == "oui" ) {
	# POLSPE_MOYexept<-POLSPE_MOY[POLSPE_MOY$MOYMAX < POLSPE_MOY$LQMAX,]
	# POLSPE_MOY<-POLSPE_MOY[!(POLSPE_MOY$MOYMAX < POLSPE_MOY$LQMAX) ,]
# }



# Calcul de la frequence de prélèvement
POLSPE_FREQ<-aggregate(MOY ~ STATION + PARAMETRE, data = POLSPE , length)
names(POLSPE_FREQ)[3]<-"FREQ"
POLSPE_FREQ$ID<-as.character(paste0(POLSPE_FREQ$STATION,"_",POLSPE_FREQ$PARAMETRE))
POLSPE_FREQ<-POLSPE_FREQ[,c("ID","FREQ")]

POLSPE_MOY<-merge(POLSPE_MOY,POLSPE_FREQ,by="ID")
POLSPE_MOY<-POLSPE_MOY[order(POLSPE_MOY$STATION, POLSPE_MOY$PARAMETRE),]
rm(POLSPE_FREQ)

#initialisation de CALSSEPS et CASPS
POLSPE_MOY$CLASSEPS<-as.numeric(NA)
POLSPE_MOY$CASPS<-""

## CAS 0 - Respect de la fréquence minimale de prélèvement
if (FREQOKPS == "oui") {
	POLSPE_MOY$CASPS[POLSPE_MOY$FREQ<NBANFREQ]<-"0"
	POLSPE_MOY$CLASSEPS[POLSPE_MOY$FREQ<NBANFREQ]<-0
	
		if ( LQSUPNQEMAPS == "oui" ) {
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
		}
}



###CAS 1 -  la MOYENNE <= NQE 
condCAS1<-POLSPE_MOY$CASPS != "0"  &  (is.na(POLSPE_MOY$LQMAX) | POLSPE_MOY$LQMAX <= POLSPE_MOY$NQEMA)
	# Cas 1A
	condCAS1a<-condCAS1 & POLSPE_MOY$MOY < POLSPE_MOY$LQMAX &  !is.na(POLSPE_MOY$LQMAX)
	POLSPE_MOY$CLASSEPS[condCAS1a]<-2  
	POLSPE_MOY$CASPS[condCAS1a ]<-"1A"
	# Cas 1B
	condCAS1b<-condCAS1 & POLSPE_MOY$CASPS != "1A" & POLSPE_MOY$MOY <= POLSPE_MOY$NQEMA 
	POLSPE_MOY$CLASSEPS[condCAS1b]<-2  
	POLSPE_MOY$CASPS[condCAS1b ]<-"1B"
	# Cas 1B
	condCAS1c<-condCAS1 & POLSPE_MOY$MOY > POLSPE_MOY$NQEMA 
	POLSPE_MOY$CLASSEPS[condCAS1c]<-3  
	POLSPE_MOY$CASPS[condCAS1c ]<-"1C"
	
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
