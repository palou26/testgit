##############################################
## SEEE - COURS D'EAU : �tat polluants sp�cifiques
## Application de l'arr�t� de janvier 2016
##############################################
# load("D:\\_ETUDES\\E4309_AESN_S3R_Adapt2016\\S3R_DEV\\S3R\\imageMon-Dec-05-17_03_08-2016.Rdata")


##############################
## Mise en forme des donn�es
##############################
gc()
DATAPOLSPE$IDAN<-as.character(paste0(DATAPOLSPE$STATION,DATAPOLSPE$PARAMETRE,DATAPOLSPE$ANNEE))


######################################################################
## METHODE : choix de la m�thode pour la preparation des donn�es brutes
# ANNEERECENTE : extraction des donn�es de l'ann�e la + r�cente pour chaque param�tre de la station
# ANNEECHRONIQUE : extraction des donn�es d'une station pour l'ensemble des param�tres ayant la chronique la plus compl�te
# NOFILTREANNEE : on ne fait aucun filtre sur l'ann�e et on prend en compte tout le jeu de donn�es
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
	
	TEMPPSANNEE1<-aggregate(RESULTAT ~ STATION + PARAMETRE, data = PSANNEE , max) # colonne RESULTAT non renomm�e mais contient le nb de prelevement
	TEMPPSANNEE1$IDFREQ<-as.character(paste0(TEMPPSANNEE1$STATION,TEMPPSANNEE1$PARAMETRE,TEMPPSANNEE1$RESULTAT))
	PSANNEE<-PSANNEE[PSANNEE$IDFREQ %in% c(TEMPPSANNEE1$IDFREQ),]
	
	#extraction de l'ann�e la + r�cente si plusieurs ann�es ont les m�mes fr�quences
	TEMPPSANNEE2<-aggregate(ANNEE ~ STATION + PARAMETRE, data = PSANNEE , max)
	TEMPPSANNEE2$IDAN<-as.character(paste0(TEMPPSANNEE2$STATION,TEMPPSANNEE2$PARAMETRE,TEMPPSANNEE2$ANNEE))
	DATAPOLSPE<-DATAPOLSPE[DATAPOLSPE$IDAN %in% c(TEMPPSANNEE2$IDAN),]
	
	#on recr�e PSANNEE depuis DATAPOLSPE filtr� pour calculer les fr�quences. PSANNEE est ainsi identique � "ANNEERECENTE" dans FREQUENCE.r --> �a facilite le script et les �ventuels modif.
	PSANNEE<-aggregate(ANNEE ~ STATION + PARAMETRE, data = DATAPOLSPE , max)
	 
	rm(TEMPPSANNEE1,TEMPPSANNEE2)
}


#################
# CAS DU XYLENE
################

DATAPOLSPE_XYL<-DATAPOLSPE[DATAPOLSPE$PARAMETRE %in% c(1780,1293,1294,1292,2925),]
DATAPOLSPE_XYL
	# on somme 1293 + 1294 = 2925
DATAPOLSPE_1293_1294<-DATAPOLSPE_XYL[DATAPOLSPE_XYL$PARAMETRE %in% c(1293,1294),]
if(nrow(DATAPOLSPE_1293_1294) > 0){
	DATAPOLSPE_1293_1294_2<-aggregate(RESULTAT  ~ STATION+DATEPRELEV+HEUREPRELEV+REMARQUE+LQ+ANNEE+MOIS+FRACTION+UNITE, data = DATAPOLSPE_1293_1294, sum)
	DATAPOLSPE_1293_1294_2$PARAMETRE<-"2925"
	DATAPOLSPE_XYL<-merge(DATAPOLSPE_XYL, DATAPOLSPE_1293_1294_2,all=TRUE)
	DATAPOLSPE_XYL<-DATAPOLSPE_XYL[order(-DATAPOLSPE_XYL$RESULTAT),]
	DATAPOLSPE_XYL<-DATAPOLSPE_XYL[!duplicated(DATAPOLSPE_XYL[,c("STATION","DATEPRELEV","HEUREPRELEV","PARAMETRE")]),]
}
	
	# on somme 2925 + 1292 = 1780
DATAPOLSPE_2925_1292<-DATAPOLSPE_XYL[DATAPOLSPE_XYL$PARAMETRE %in% c(2925,1292),]
if(nrow(DATAPOLSPE_2925_1292) > 0){
	DATAPOLSPE_2925_1292_2<-aggregate(RESULTAT  ~ STATION+DATEPRELEV+HEUREPRELEV+REMARQUE+LQ+ANNEE+MOIS+FRACTION+UNITE, data = DATAPOLSPE_2925_1292, sum)
	DATAPOLSPE_2925_1292_2$PARAMETRE<-"1780"
	DATAPOLSPE_XYL<-merge(DATAPOLSPE_XYL, DATAPOLSPE_2925_1292_2,all=TRUE)
	DATAPOLSPE_XYL<-DATAPOLSPE_XYL[order(-DATAPOLSPE_XYL$RESULTAT),]
	DATAPOLSPE_XYL<-DATAPOLSPE_XYL[!duplicated(DATAPOLSPE_XYL[,c("STATION","DATEPRELEV","HEUREPRELEV","PARAMETRE")]),]
}
DATAPOLSPE_XYL<-DATAPOLSPE_XYL[DATAPOLSPE_XYL$PARAMETRE == "1780",names(DATAPOLSPE)]	

	#finalisation
DATAPOLSPE<-rbind(
	DATAPOLSPE[!(DATAPOLSPE$PARAMETRE %in% c(1780,1293,1294,1292,2925)),]
	, DATAPOLSPE_XYL)


##############################
## Calcul de l'�tat des polluants sp�cifiques
##############################
# requete pr�paratoire
POLSPE<-merge(DATAPOLSPE,LISTECODERQE[,c("REMARQUE","QUANTIFIE")],by="REMARQUE")
POLSPE<-merge(POLSPE,PARAMETREPOLSPE[,c("PARAMETRE","NQEMA","NQEMA2")],by="PARAMETRE")
gc()

# 5/12/2016 : on retire les LQABERANTE
POLSPE<-merge(POLSPE, PARAMETREPOLSPE[,c("PARAMETRE","LQSEUIL")], by="PARAMETRE", all.x = TRUE)
if ( SUPPR_LQ_ABERRANTE_PS== "oui" ) {


	if (LQ_NONDISPO =="oui") { # si LQ non dispo on prend les valeurs du champ RESULTAT lorsque non quantifi� --> car c'est la LQ
		PBLQPSABERRANTE<-POLSPE[!is.na(POLSPE$LQSEUIL) & POLSPE$RESULTAT>POLSPE$LQSEUIL & POLSPE$QUANTIFIE=="0" & POLSPE$LQSEUIL != -99 ,]
		POLSPE<-POLSPE[(!(!is.na(POLSPE$LQSEUIL) & POLSPE$RESULTAT>POLSPE$LQSEUIL & POLSPE$QUANTIFIE=="0"))| POLSPE$LQSEUIL == -99 | is.na(POLSPE$LQSEUIL) ,]
	} else {
		PBLQPSABERRANTE<-POLSPE[!is.na(POLSPE$LQSEUIL) & POLSPE$LQ>POLSPE$LQSEUIL  & POLSPE$LQSEUIL != -99  ,]
		POLSPE<-POLSPE[(!(!is.na(POLSPE$LQSEUIL) & POLSPE$LQ>POLSPE$LQSEUIL))| POLSPE$LQSEUIL == -99 | is.na(POLSPE$LQSEUIL)  ,]
	}

	# if (LQ_NONDISPO =="oui") { # si LQ non dispo on prend les valeurs du champ RESULTAT lorsque non quantifi� --> car c'est la LQ
		# PBLQPSABERRANTE<-POLSPE[POLSPE$RESULTAT>POLSPE$LQSEUIL & POLSPE$QUANTIFIE=="0" & POLSPE$LQSEUIL != -99 ,]
		# POLSPE<-POLSPE[!(POLSPE$RESULTAT>POLSPE$LQSEUIL & POLSPE$QUANTIFIE=="0") | POLSPE$LQSEUIL == -99 ,]
	# } else {
		# PBLQPSABERRANTE<-POLSPE[POLSPE$LQ > POLSPE$LQSEUIL & POLSPE$LQSEUIL != -99 ,]
		# POLSPE<-POLSPE[POLSPE$LQ <= POLSPE$LQSEUIL | POLSPE$LQSEUIL == -99  ,]
	# }

	if( nrow(PBLQPSABERRANTE)>0) {
		CSV<-paste(CH_ERREUR,"PS_LQ_ABERRANTE_",SEEE_DEBformat,".csv",sep="")
		write.csv2(PBLQPSABERRANTE,CSV)
		MSGBOX <- tkmessageBox(title = "Info", message = paste("Les donn�es brutes avec LQ aberrantes (strictement > LQSEUIL) ont �t� retir�es \n du calcul de l'�tat chimique et export�es dans \n",CSV,sep=""), icon = "info", type = "ok")
	}

	if (nrow(POLSPE) == 0) {
			MSGBOX <- tkmessageBox(title = "Info", message = paste("Pas de donn�es � traiter apr�s le retrait des mesures avec LQ aberrantes \n",CSV,sep=""), icon = "info", type = "ok")
			tcl("update")
			source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
	}	
}





gc()
POLSPE<-merge(POLSPE,STATION[,c("STATION","FONDGEO_ARSENIC","FONDGEO_CHROME","FONDGEO_CUIVRE","FONDGEO_ZINC","DURETE")],by="STATION", all.x=TRUE)
POLSPE$ID<-paste(POLSPE$STATION, POLSPE$PARAMETRE, sep="_")
gc()

# D�termination de NQEMA selon durete et Fond g�ochim
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






# Modification suite � la publiation de l'arret� et prestation C43 AELB 17 dec 
POLSPE$RESULTAT2<-POLSPE$RESULTAT
#Si  on est pas quantifi� 
if (LQ_NONDISPO =="oui") {
	cond<- POLSPE$QUANTIFIE=="0" 
	POLSPE$RESULTAT2[cond]<-POLSPE$RESULTAT[cond]/2
} else {
	cond<- POLSPE$QUANTIFIE=="0" | POLSPE$RESULTAT <= POLSPE$LQ
	POLSPE$RESULTAT2[cond]<-POLSPE$LQ[cond]/2
}

# on cr�� un dataframe qui donne par station et par parametre la valeur de NQEMA (sans doublons)
POLSPE_NQEMA<-POLSPE[!duplicated(POLSPE[,c("STATION","PARAMETRE")]),c("STATION","PARAMETRE","NQEMA")]
POLSPE_NQEMA$ID<-paste0(POLSPE_NQEMA$STATION,"_",POLSPE_NQEMA$PARAMETRE)


#modif 24/04/15 : retrait prelevement pour calcul moyenne si LQ > NQE (mail Ministere du 08/04/15 � destination des agences pour cycle 2
if ( LQSUPNQEMAPS == "oui" ) {
	if (LQ_NONDISPO =="oui") { #si LQ non dispo on prend les valeurs du champ RESULTAT lorsque non quantifi� --> car c'est la LQ
		PBLQPS<-POLSPE[POLSPE$RESULTAT>POLSPE$NQEMA & POLSPE$QUANTIFIE=="0" ,]
		POLSPE<-POLSPE[!(POLSPE$RESULTAT>POLSPE$NQEMA & POLSPE$QUANTIFIE=="0"),]
	} else {
		PBLQPS<-POLSPE[POLSPE$LQ > POLSPE$NQEMA & POLSPE$QUANTIFIE=="0" ,]
		POLSPE<-POLSPE[!(POLSPE$LQ > POLSPE$NQEMA & POLSPE$QUANTIFIE=="0" ),]
	}
	
	if( nrow(PBLQPS)>0) {
		CSV<-paste(CH_ERREUR,"PS_LQ_SUP_NQEMA_",SEEE_DEBformat,".csv",sep="")
		write.csv2(PBLQPS,CSV)
		MSGBOX <- tkmessageBox(title = "Info", message = paste("Les donn�es brutes avec LQ > NQEMA ont �t� retir�es \n du calcul du calcul de la concentration moyenne et export�es dans \n",CSV,sep=""), icon = "info", type = "ok")
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

# Extraction de la LQMAX par station pour comparaison � la MA
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



# Calcul de la frequence de pr�l�vement
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

## CAS 0 - Respect de la fr�quence minimale de pr�l�vement
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
		#cas o� si on avait conserv� tous les pr�levements on aurait atteint la frequence de prelevement
		POLSPE_MOY$CASPS[POLSPE_MOY$FREQ>0 & POLSPE_MOY$FREQ<NBANFREQ & POLSPE_MOY$FREQ + POLSPE_MOY$FREQPBLQ  >= NBANFREQ ]<-"0*"
		# cas o� la frequence apr�s retrait est � zero et on aurait quand m�me pas attend la frequence de pr�levement si on avait conserv� tous les pr�levements
		POLSPE_MOY$CASPS[POLSPE_MOY$FREQ== 0 & POLSPE_MOY$FREQPBLQ  > 0 ]<-"0**"
		# cas o� la frequence apr�s retrait est � zero et on aurait  atteind la frequence de pr�levement si on avait conserv� tous les pr�levements		
		POLSPE_MOY$CASPS[POLSPE_MOY$FREQ== 0 & POLSPE_MOY$FREQPBLQ  >= NBANFREQ ]<-"0***"
		POLSPE_MOY$CLASSEPS[POLSPE_MOY$CASPS %in% c("0","0*","0**","0***")]<-0
		print(table(POLSPE_MOY$CASPS))		
		}}
}

## CAS 0 - Respect de la fr�quence minimale de pr�l�vement
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
		#cas o� si on avait conserv� tous les pr�levements on aurait atteint la frequence de prelevement
		POLSPE_MOY$CASPS[POLSPE_MOY$FREQ>0 & POLSPE_MOY$FREQ<NBANFREQ & POLSPE_MOY$FREQ + POLSPE_MOY$FREQPBLQ2  >= NBANFREQ ]<-"0+"
		# cas o� la frequence apr�s retrait est � zero et on aurait quand m�me pas attend la frequence de pr�levement si on avait conserv� tous les pr�levements
		POLSPE_MOY$CASPS[POLSPE_MOY$FREQ== 0 & POLSPE_MOY$FREQPBLQ2  > 0 ]<-"0++"
		# cas o� la frequence apr�s retrait est � zero et on aurait  atteind la frequence de pr�levement si on avait conserv� tous les pr�levements		
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

#verif
table(POLSPE_MOY$CASPS,	POLSPE_MOY$CLASSEPS)


# Calcul de l'�tat par type de polluant
POLSPE_POL<-aggregate(CLASSEPS ~ STATION + POLLUANT, data = POLSPE_MOY, max)
names(POLSPE_POL)[3]<-"ETATPOL" #renomme la colonne CLASSEPS sur laquelle il y a eu le max car correspond � l'�tat par type de polluants
POLSPE_POL<-POLSPE_POL[order(POLSPE_POL$STATION, POLSPE_POL$POLLUANT),]

# Calcul de l'�tat global des polluants sp�cifiques
# si les polluants NS sont exclus, l'�tat des PS est uniquement d�termin� par les polluants synth�tiques
if (EXCLU_POLNS=="oui") {
	RLT_POLSPE<-POLSPE_POL[POLSPE_POL$POLLUANT=="synth",c("STATION", "ETATPOL")]
	} else {
	RLT_POLSPE<-aggregate(ETATPOL ~ STATION, data = POLSPE_POL, max) # R�sultat final des polluants sp�cifiques
	}
names(RLT_POLSPE)[2]<-"ETATPS" #renomme la colonne ETATPOL sur laquelle il y a eu le max car correspond � l'�tat global des polluants sp�cifiques
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

# Ajout colonnes des param�tres 
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
