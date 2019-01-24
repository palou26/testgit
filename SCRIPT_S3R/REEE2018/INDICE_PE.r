##############################################
## SEEE - COURS D'EAU : état physicochimique
## Application de l'arrêté de janvier 2010

## script crée en mars 2013
##############################################

##############################################
## CALCUL DES RANG 90 / VALEUR MOYENNE / INDICE (type SEQ-eau)

## PCH
# principe de calcul de l'indice
# (((résultat - valeur inf borne)*20) / (valeur sup borne - valeur inf borne)) + indice borne inf

### BIO
# principe de calcul de l'indice par ordre croissant (IMOL, IBG, IBGA)
# (((résultat - valeur inf borne)*20) / (valeur sup borne - valeur inf borne)) + indice borne inf

# principe de calcul de l'indice par ordre décroissant (IPR)
# (((valeur sup borne - résultat)*20) / (valeur sup borne - valeur inf borne)) + indice borne inf

### NOTA :
# La fonction "merge" retrie le vecteur. la commande "sort" ne semble pas fonctionner --> pourquoi ?
# on déclare donc les conditions "condXXX" après chaque "merge" pour s'assurer que le true/false créé avec les conditions est toujours dans le même ordre que le merge.
# sinon le résultat ne correspond pas à la condition.
# On aurait pu rester comme initialement, avec 1 condition pour les paramètres et 1 autre pour les classes et la combinaison des 2 donne la condition globale,
# mais comme on les déclarait avant les merge ça obligait à définir un ID pour tjs trier les enregistrements dans le même sens et faire un "order" après chaque "merge".
# on a jugé cette solution moins robuste que de répéter les conditions après chaque merge, même si ça engendre un code avec plus de lignes.

#########################################
# PCH INDICE : Paramètre ordre croissant




if (SEEEPCH=="oui") {
	gc()
	
	FORMULEINDICE<- function (DAT=RLT_PCH) {
		DAT$INDICE[condPCH1]<-round((((DAT$RESULTAT[condPCH1]-DAT$INFB[condPCH1])*20)/(DAT$SUPB[condPCH1]-DAT$INFB[condPCH1]))+80) 
		DAT$INDICE[condPCH2]<-round((((DAT$RESULTAT[condPCH2]-DAT$INFV[condPCH2])*20)/(DAT$INFB[condPCH2]-DAT$INFV[condPCH2]))+60)
		DAT$INDICE[condPCH3]<-round((((DAT$RESULTAT[condPCH3]-DAT$INFJ[condPCH3])*20)/(DAT$INFV[condPCH3]-DAT$INFJ[condPCH3]))+40)
		DAT$INDICE[condPCH4]<-round((((DAT$RESULTAT[condPCH4]-DAT$INFO[condPCH4])*20)/(DAT$INFJ[condPCH4]-DAT$INFO[condPCH4]))+20)
		DAT$INDICE[condPCH5]<-round((((DAT$RESULTAT[condPCH5]-DAT$INFR[condPCH5])*20)/(DAT$INFO[condPCH5]-DAT$INFR[condPCH5]))+0)
		DAT$INDICE[DAT$INDICE>100]<-100
		DAT$INDICE[DAT$INDICE<0]<-0
		DAT
	}

	#ordre croissant
	condparam<-RLT_PCH$PARAMETRE %in% c("1340","1335","1350") 
	condPCH1<-condparam & !is.na(RLT_PCH$CLASSEPCH) & RLT_PCH$RESULTAT<=RLT_PCH$INFB
	condPCH2<-condparam & !is.na(RLT_PCH$CLASSEPCH) & RLT_PCH$RESULTAT>RLT_PCH$INFB & RLT_PCH$RESULTAT<=RLT_PCH$INFV
	condPCH3<-condparam & !is.na(RLT_PCH$CLASSEPCH) & RLT_PCH$RESULTAT>RLT_PCH$INFV & RLT_PCH$RESULTAT<=RLT_PCH$INFJ
	condPCH4<-condparam & !is.na(RLT_PCH$CLASSEPCH) & RLT_PCH$RESULTAT>RLT_PCH$INFJ & RLT_PCH$RESULTAT<=RLT_PCH$INFO
	condPCH5<-condparam & !is.na(RLT_PCH$CLASSEPCH) & RLT_PCH$RESULTAT>RLT_PCH$INFO 

	
	# On considère que SUPB = INFB - (INFV - INFB)
	RLT_PCH$SUPB[condparam]<-RLT_PCH$INFB[condparam] - (RLT_PCH$INFV[condparam] - RLT_PCH$INFB[condparam] ) 
	RLT_PCH$INFR[condparam]<-RLT_PCH$INFO[condparam] + (RLT_PCH$INFO[condparam] - RLT_PCH$INFJ[condparam] ) 
	RLT_PCH<-FORMULEINDICE(DAT = RLT_PCH)
	rm(condPCH1, condPCH2, condPCH3, condPCH4, condPCH5)
	gc()
	flush.console()

# PCH INDICE : Paramètre ordre décroissant (cas de la transparence)
	condparam<-RLT_PCH$PARAMETRE %in% c("1332") 
	condPCH1<-condparam & !is.na(RLT_PCH$CLASSEPCH) & RLT_PCH$RESULTAT>=RLT_PCH$INFB
	condPCH2<-condparam & !is.na(RLT_PCH$CLASSEPCH) & RLT_PCH$RESULTAT>=RLT_PCH$INFV & RLT_PCH$RESULTAT<RLT_PCH$INFB
	condPCH3<-condparam & !is.na(RLT_PCH$CLASSEPCH) & RLT_PCH$RESULTAT>=RLT_PCH$INFJ & RLT_PCH$RESULTAT<RLT_PCH$INFV
	condPCH4<-condparam & !is.na(RLT_PCH$CLASSEPCH) & RLT_PCH$RESULTAT>=RLT_PCH$INFO & RLT_PCH$RESULTAT<RLT_PCH$INFJ
	condPCH5<-condparam & !is.na(RLT_PCH$CLASSEPCH)  & RLT_PCH$RESULTAT<RLT_PCH$INFO

	RLT_PCH$SUPB[condparam]<-RLT_PCH$INFB[condparam] + (RLT_PCH$INFB[condparam] - RLT_PCH$INFV[condparam] ) 
	RLT_PCH$INFR[condparam]<-RLT_PCH$INFO[condparam] - (RLT_PCH$INFJ[condparam] - RLT_PCH$INFO[condparam] ) 
	
	RLT_PCH<-FORMULEINDICE(DAT = RLT_PCH)
	rm(condPCH1, condPCH2, condPCH3, condPCH4, condPCH5)

	
	#cas de l'ilox
	condilox<-RLT_PCH$PARAMETRE == "ilox"
	RLT_PCH$INDICE[condilox]<- 100-RLT_PCH$RESULTAT[condilox]
	RLT_PCH$INDICE[condilox & RLT_PCH$INDICE>100]<-100
	RLT_PCH$INDICE[condilox & RLT_PCH$INDICE<0]<-0

	# On met vide si classe indéterminée
	RLT_PCH$INDICE[RLT_PCH$CLASSEPCH==0]<-NA
	
	RLT_PCH<-RLT_PCH[,c("IDTRI","STATION", "PARAMETRE",  "RESULTAT", "CLASSEPCH", "INDICE", "Freq","ELTQUALITE")]
	RLT_PCH<-merge(RLT_PCH, STATION, by="STATION")
	gc()

	flush.console()
	
# TABLEAU FINAL : mise en forme

 ## d'abord pour l'état puis par élémént de qualité (c'est indice minimum = le plus déclassant)
iPCH_ELTQUALITE<-aggregate(INDICE ~ STATION + ELTQUALITE, data = RLT_PCH, min, na.rm = TRUE)
iPCH_ELTQUALITE<-iPCH_ELTQUALITE[order(iPCH_ELTQUALITE$STATION, iPCH_ELTQUALITE$ELTQUALITE),]
names(iPCH_ELTQUALITE)[3]<-"INDICEELT" 

  # Prise en compte des exeptions typologiques 
RLT_PCHINDICE<-data.frame(STATION=as.character(), INDICEELT=as.numeric())
iPCH_ELTQUALITE<-merge(iPCH_ELTQUALITE,STATION[,c("STATION","EXCEPT_TRANS")],by="STATION")
PASEXEPTION<-iPCH_ELTQUALITE[iPCH_ELTQUALITE$ELTQUALITE != "bilano2" & iPCH_ELTQUALITE$EXCEPT_TRANS != "oui",]
if (nrow(PASEXEPTION) > 0){
	PCH_STATION_SS_EXEPT<-aggregate(INDICEELT ~ STATION, data =  PASEXEPTION, min, na.rm = TRUE)
	RLT_PCHINDICE<-rbind(RLT_PCHINDICE,PCH_STATION_SS_EXEPT)
}

EXCEPTIONTRANSP<-iPCH_ELTQUALITE[iPCH_ELTQUALITE$ELTQUALITE != "bilano2" & iPCH_ELTQUALITE$EXCEPT_TRANS == "oui"  & iPCH_ELTQUALITE$ELTQUALITE != "transpa",]
if (nrow(EXCEPTIONTRANSP) > 0){
	PCH_STATION_AV_EXEPT<-aggregate(INDICEELT ~ STATION, data =EXCEPTIONTRANSP  , min, na.rm = TRUE)
	RLT_PCHINDICE<-rbind(RLT_PCHINDICE,PCH_STATION_AV_EXEPT)
}


RLT_PCHINDICE<-RLT_PCHINDICE[order(RLT_PCHINDICE$STATION),]
names(RLT_PCHINDICE)[2]<-"iPCH_SSBILANO2" #renomme la colonne INDICEELT sur laquelle il y a eu le max car correspond à l'état de la station



for (i in  1:nrow(TABLEELT)) {
	TEMPELT<-iPCH_ELTQUALITE[iPCH_ELTQUALITE$ELTQUALITE==TABLEELT$ELTQUALITE[i],c("STATION", "INDICEELT")]
	names(TEMPELT)[2]<-paste0("i",toupper(TABLEELT$ELTQUALITE[i]))
	RLT_PCHINDICE<-merge(RLT_PCHINDICE,TEMPELT,by="STATION", all=TRUE)
	rm(TEMPELT)
}


# on déclasse à 3 si bilanO2 = 3 et etatpch = 1ou 2
cond<-!is.na(RLT_PCHINDICE$iPCH_SSBILANO2) & RLT_PCHINDICE$iPCH_SSBILANO2 >= 60 & !is.na(RLT_PCHINDICE$iBILANO2) & RLT_PCHINDICE$iBILANO2 <60
RLT_PCHINDICE$iETATPCH<-RLT_PCHINDICE$iPCH_SSBILANO2
print(RLT_PCHINDICE)
if (nrow(RLT_PCHINDICE[cond,])) {
	RLT_PCHINDICE$iETATPCH[cond]<-pmin(RLT_PCHINDICE$iPCH_SSBILANO2[cond], RLT_PCHINDICE$iBILANO2[cond],40, na.rm = TRUE)
}
##Puis par parametre (indice)
PARAMETRES<-unique(PARAMETREPCH$PARAMETRE)
for (i in  PARAMETRES) {
	TEMP<-RLT_PCH[RLT_PCH$PARAMETRE==i,c("STATION","INDICE")]
	names(TEMP)[2]<-paste0("i",toupper(PARAMETREPCH$NOM[PARAMETREPCH$PARAMETRE == i][1]))
	RLT_PCHINDICE<-merge(RLT_PCHINDICE,TEMP,by="STATION", all=TRUE)
	rm(TEMP)
}
NAMESRLT<-names(RLT_PCHINDICE)

##Puis par parametre (valeur mediane ou max)
PARAMETRES<-unique(PARAMETREPCH$PARAMETRE)
for (i in  PARAMETRES) {
	TEMP<-RLT_PCH[RLT_PCH$PARAMETRE==i,c("STATION","RESULTAT")]
	names(TEMP)[2]<-paste0("v",toupper(PARAMETREPCH$NOM[PARAMETREPCH$PARAMETRE == i][1]))
	RLT_PCHINDICE<-merge(RLT_PCHINDICE,TEMP,by="STATION", all=TRUE)
	rm(TEMP)
}
NAMESRLT<-names(RLT_PCHINDICE)

	# L'indice PCH ne sera effectué que pour AESN	
	if (BASSIN=="AESN") { iETATPCH_NAMES<-"iETATPCH"}
			
	gc()	
}

######################################
## BIOLOGIE : VALEUR MOYENNE & INDICE 
######################################
if (SEEEBIO=="oui") {
	BIO_MOY$INDICEBIO<-as.numeric("")

#calcul indice 
	#Pour hors IPL
	condBio1<- BIO_MOY$PARAGROUP!="IPL" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==1
	condBio2<- BIO_MOY$PARAGROUP!="IPL" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==2
	condBio3<- BIO_MOY$PARAGROUP!="IPL" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==3
	condBio4<- BIO_MOY$PARAGROUP!="IPL" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==4
	condBio5<- BIO_MOY$PARAGROUP!="IPL" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==5
	
	BIO_MOY$INDICEBIO[condBio1]<-round((((BIO_MOY$RESULTAT[condBio1]-BIO_MOY$INFB[condBio1])*20)/(1-BIO_MOY$INFB[condBio1]))+80) 
	BIO_MOY$INDICEBIO[condBio2]<-round((((BIO_MOY$RESULTAT[condBio2]-BIO_MOY$INFV[condBio2])*20)/(BIO_MOY$INFB[condBio2]-BIO_MOY$INFV[condBio2]))+60)
	BIO_MOY$INDICEBIO[condBio3]<-round((((BIO_MOY$RESULTAT[condBio3]-BIO_MOY$INFJ[condBio3])*20)/(BIO_MOY$INFV[condBio3]-BIO_MOY$INFJ[condBio3]))+40)
	BIO_MOY$INDICEBIO[condBio4]<-round((((BIO_MOY$RESULTAT[condBio4]-BIO_MOY$INFO[condBio4])*20)/(BIO_MOY$INFJ[condBio4]-BIO_MOY$INFO[condBio4]))+20)
	BIO_MOY$INDICEBIO[condBio5]<-round((((BIO_MOY$RESULTAT[condBio5]-BIO_MOY$INFR[condBio5])*20)/(BIO_MOY$INFO[condBio5]-BIO_MOY$INFR[condBio5]))+0.0)
	BIO_MOY$INDICEBIO[ BIO_MOY$INDICEBIO>100]<-100
	BIO_MOY$INDICEBIO[ BIO_MOY$INDICEBIO<0]<-0

	#Pour IPL sandre 6591
	condIPL1<-BIO_MOY$PARAGROUP %in% c("6591","IPL") & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==1
	condIPL2<-BIO_MOY$PARAGROUP %in% c("6591","IPL") & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==2
	condIPL3<-BIO_MOY$PARAGROUP %in% c("6591","IPL") & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==3
	condIPL4<-BIO_MOY$PARAGROUP %in% c("6591","IPL") & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==4
	condIPL5<-BIO_MOY$PARAGROUP %in% c("6591","IPL") & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==5

	BIO_MOY$INDICEBIO[condIPL1]<-round((((BIO_MOY$INFB[condIPL1]-BIO_MOY$RESULTAT[condIPL1])*20)/(BIO_MOY$INFB[condIPL1]-0))+80)   
	BIO_MOY$INDICEBIO[condIPL2]<-round((((BIO_MOY$INFV[condIPL2]-BIO_MOY$RESULTAT[condIPL2])*20)/(BIO_MOY$INFV[condIPL2]-BIO_MOY$INFB[condIPL2]))+60)
	BIO_MOY$INDICEBIO[condIPL3]<-round((((BIO_MOY$INFJ[condIPL3]-BIO_MOY$RESULTAT[condIPL3])*20)/(BIO_MOY$INFJ[condIPL3]-BIO_MOY$INFV[condIPL3]))+40)
	BIO_MOY$INDICEBIO[condIPL4]<-round((((BIO_MOY$INFO[condIPL4]-BIO_MOY$RESULTAT[condIPL4])*20)/(BIO_MOY$INFO[condIPL4]-BIO_MOY$INFJ[condIPL4]))+20)
	BIO_MOY$INDICEBIO[condIPL5]<-round((((BIO_MOY$INFR[condIPL5]-BIO_MOY$RESULTAT[condIPL5])*20)/(BIO_MOY$INFR[condIPL5]-BIO_MOY$INFO[condIPL5]))+0.0)
	BIO_MOY$INDICEBIO[BIO_MOY$RESULTAT>BIO_MOY$INFR & BIO_MOY$CLASSEBIO==5]<-0
		

	# On met vide si classe indéterminée
	BIO_MOY$INDICEBIO[BIO_MOY$CLASSEBIO==0]<-NA
	
	
	BIO_MOY<-BIO_MOY[,c(BIO_MOY_NAMES, "INDICEBIO")]
	rm(condBio1, condBio2, condBio3, condBio4, condBio5)
	flush.console()
	
# TABLEAU FINAL : mise en forme
	# Tableau pour les valeurs moyennes
	TEMP_BIOMOY<-data.frame(RLT_BIO[,"STATION"])
	names(TEMP_BIOMOY)[1]<-"STATION"
	for (i in  1:nrow(TABLEBIO)  ) {
		TEMPBIO<-BIO_MOY[BIO_MOY$PARAGROUP==TABLEBIO$PARAGROUP[i],c("STATION", "RESULTAT")]
		names(TEMPBIO)[2]<-paste ("eqr",TABLEBIO$PARALIBGROUP[i], sep="")
		TEMP_BIOMOY<-merge(TEMP_BIOMOY,TEMPBIO,by="STATION", all=TRUE)
		rm(TEMPBIO)
	}
	
	# Tableau pour les indices
	TEMP_BIOINDICE<-data.frame(RLT_BIO[,"STATION"])
	names(TEMP_BIOINDICE)[1]<-"STATION"
	for (i in  1:nrow(TABLEBIO)  ) {
		TEMPBIO<-BIO_MOY[BIO_MOY$PARAGROUP==TABLEBIO$PARAGROUP[i],c("STATION", "INDICEBIO")]
		names(TEMPBIO)[2]<-paste ("i",TABLEBIO$PARALIBGROUP[i], sep="")
		TEMP_BIOINDICE<-merge(TEMP_BIOINDICE,TEMPBIO,by="STATION", all=TRUE)
		rm(TEMPBIO)
	}
		
	# Mise en forme et Tableau pour l'indice ETATBIO : demande spécifique AESN
	if (BASSIN=="AESN") {
		TEMP_BIOINDICEETAT<-aggregate(INDICEBIO ~ STATION , data = BIO_MOY , min, na.rm = TRUE) # calcul de l'indice de l'état BIO
		names(TEMP_BIOINDICEETAT)[2]<-"iETATBIO"
		iETATBIO_NAMES<-"iETATBIO"
		RLT_BIOINDICE<-merge(TEMP_BIOMOY,TEMP_BIOINDICEETAT,by="STATION", all=TRUE)
		RLT_BIOINDICE<-merge(RLT_BIOINDICE,TEMP_BIOINDICE,by="STATION", all=TRUE)
		rm(TEMP_BIOMOY, TEMP_BIOINDICE, TEMP_BIOINDICEETAT)
	} else {
		RLT_BIOINDICE<-merge(TEMP_BIOMOY,TEMP_BIOINDICE,by="STATION", all=TRUE)
		rm(TEMP_BIOMOY, TEMP_BIOINDICE)	
	}		

	


}

###########################################
## ETAT ECOLOGIQUE : INDICE (spécifique AESN)
###########################################
if (BASSIN =="AESN" & SEEEBIO=="oui" & SEEEPCH=="oui" ) {
	ETATECOLOINDICE<-merge(RLT_PCHINDICE[,c("STATION","iETATPCH")],RLT_BIOINDICE[,c("STATION","iETATBIO")],by="STATION",all.x=TRUE,all.y=TRUE)
	ETATECOLOINDICE$iETATECOLOHPS<-pmin(ETATECOLOINDICE$iETATPCH,ETATECOLOINDICE$iETATBIO,na.rm = TRUE) #na.rm permet de ne pas tenir compte des NA
	ETATECOLOHPSINDICE<-ETATECOLOINDICE[,c("STATION","iETATECOLOHPS")]
	iETATECOLOHPS_NAMES<-"iETATECOLOHPS"
}

###########################################
## POLLUANTS SPECIFIQUES : VALEUR MOYENNE
# Extraction uniquement de la valeur moyenne. 
# Indice calculé à partir d'une formule transmise par AESN
###########################################
# principe retenu : valeur de l'indice = 60 si moyenne concentration = NQE. 
# valeur = 80 si NQE/10
# valeur = 100 si NQE/100 ou moins
# valeur = 40 si 10xNQE
# valeur = 20 si 100x NQE
# valeur= 0 si > 1000xNQE
# formule : indice = (log10(NQE/concent moyenne) + 3) x 20  avec les indices > 100 ou <0 ramenés à 100 ou 0 

if (SEEEPS=="oui") {
	RLT_POLSPEVALEUR<-data.frame(RLT_POLSPE[,c("STATION")])
	names(RLT_POLSPEVALEUR)[1]<-"STATION"
	for (i in  1:nrow(PARAMETREPOLSPE)  ) {
		TEMPP<-POLSPE_MOY[POLSPE_MOY$PARAMETRE==PARAMETREPOLSPE$PARAMETRE[i],c("STATION", "MOY")]
		names(TEMPP)[2]<-paste("v",toupper(PARAMETREPOLSPE$PARAMETRELIB[i]), sep="")
		RLT_POLSPEVALEUR<-merge(RLT_POLSPEVALEUR,TEMPP,by="STATION", all.x=TRUE)
		rm(TEMPP)
	}
	
	# CALCUL DES INDICES (9/03/2016 Adaptation AESN)
	POLSPE_MOY$INDICEPOLSPE<-round((log10(POLSPE_MOY$NQEMA/POLSPE_MOY$MOY)+3)*20,3)
	POLSPE_MOY$INDICEPOLSPE[POLSPE_MOY$INDICEPOLSPE>100]<-100
	POLSPE_MOY$INDICEPOLSPE[POLSPE_MOY$INDICEPOLSPE<0]<-0
	#On met Vide si Etat indéterminé
	POLSPE_MOY$INDICEPOLSPE[POLSPE_MOY$CLASSEPS == 0]<-NA
		
	
	# Tableau pour les indices
	RLT_POLSPEINDICE<-data.frame(RLT_POLSPE[,"STATION"])
	names(RLT_POLSPEINDICE)[1]<-"STATION"
	for (i in  1:nrow(PARAMETREPOLSPE)  ) {
		TEMPPOLSPE<-POLSPE_MOY[POLSPE_MOY$PARAMETRE==PARAMETREPOLSPE$PARAMETRE[i],c("STATION", "INDICEPOLSPE")]
		names(TEMPPOLSPE)[2]<-paste ("i",toupper(PARAMETREPOLSPE$PARAMETRELIB[i]), sep="")
		RLT_POLSPEINDICE<-merge(RLT_POLSPEINDICE,TEMPPOLSPE,by="STATION", all=TRUE)
		rm(TEMPPOLSPE)
	}

	
	# Indice ETAT PS - demande spécifique AESN
		#Indice du parametre ayant le moins bon indice (MIN) hors état indéterminé
	if (BASSIN=="AESN") {
		indPS<-as.matrix(RLT_POLSPEINDICE[,2:ncol(RLT_POLSPEINDICE)])
		MAT<-RLT_POLSPE[,5:ncol(RLT_POLSPE)]
		MAT[MAT > 0 & !is.na(MAT)]<-1
		MAT[MAT == 0 & !is.na(MAT)]<-NA
		indPS<-indPS * MAT
		cond<-RLT_POLSPE$ETATPS != 0 & !is.na(RLT_POLSPE$ETATPS)
		RLT_POLSPEINDICE$iETATPS<-as.numeric(NA)
		RLT_POLSPEINDICE$iETATPS[cond]<-apply(indPS[cond,],1, min, na.rm=TRUE)
		rm(indPS,MAT)
		iETATPS_NAMES<-"iETATPS"
	} 	
	gc()


	
	RLT_POLSPEINDICE<-merge(RLT_POLSPEVALEUR,RLT_POLSPEINDICE,by="STATION")
}

###########################################
## ETAT CHIMIQUE : VALEUR MOYENNE
# Extraction uniquement de la valeur moyenne. Indice non calculé car non demandé par les clients pour le moment
###########################################

if (SEEECHIM=="oui") {
# Mise en forme du tableau avec les classes d'état & valeurs moyennes (MOY, MOYMIN, MOYMAX)
	RLT_CHIMCMAMA<-CHIMCMAMA[,c(2:(ncol(CHIMCMAMA)))]
	RLT_CHIMCMAMA<-merge(RLT_CHIMCMAMA, PARAGROUPCHIM[,c("PARAGROUP","PARAGROUPLIBCOURT","FAMILLE","PARAGROUPLIB","HAPUBI")],by="PARAGROUP")
	RLT_CHIMCMAMA<-RLT_CHIMCMAMA[,c("STATION","PARAGROUP","PARAGROUPLIBCOURT", "FAMILLE", "HAPUBI","QUANTIFEAN", "RESULTATMAX", "NQECMA","CLASSECMA", "MOY", "LQMAX","NQEMA","CLASSEMA", "CLASSEETAT","PARAGROUPLIB", "GROUPE")]
	RLT_CHIMCMAMA<-RLT_CHIMCMAMA[order(RLT_CHIMCMAMA$STATION,RLT_CHIMCMAMA$PARAGROUP),]
	
# 22/03/2016 PPL: Calcul d'un indice comme pour PS (si NA c'est normal et c'est corrigé par la suite grâce à l'argument na.ram = T)	
	RLT_CHIMCMAMA$IndiceCMA<-round((log10(RLT_CHIMCMAMA$NQECMA/RLT_CHIMCMAMA$RESULTATMAX)+3)*20,3)
	RLT_CHIMCMAMA$IndiceCMA[RLT_CHIMCMAMA$RESULTATMAX == 0 & RLT_CHIMCMAMA$CLASSECMA == 1]<-100
	
	RLT_CHIMCMAMA$IndiceMA<-round((log10(RLT_CHIMCMAMA$NQEMA/RLT_CHIMCMAMA$MOY)+3)*20,3)
	RLT_CHIMCMAMA$IndiceMA[RLT_CHIMCMAMA$MOY == 0 & RLT_CHIMCMAMA$CLASSEMA == 1]<-100

	RLT_CHIMCMAMA$IndiceChim<-pmin(RLT_CHIMCMAMA$IndiceMA,RLT_CHIMCMAMA$IndiceCMA, na.rm=TRUE)
	
	RLT_CHIMCMAMA$IndiceMA[RLT_CHIMCMAMA$IndiceMA>100]<-100
	RLT_CHIMCMAMA$IndiceMA[RLT_CHIMCMAMA$IndiceMA<0]<-0
	RLT_CHIMCMAMA$IndiceCMA[RLT_CHIMCMAMA$IndiceCMA>100]<-100
	RLT_CHIMCMAMA$IndiceCMA[RLT_CHIMCMAMA$IndiceCMA<0]<-0
	RLT_CHIMCMAMA$IndiceChim[RLT_CHIMCMAMA$IndiceChim>100]<-100
	RLT_CHIMCMAMA$IndiceChim[RLT_CHIMCMAMA$IndiceChim<0]<-0
	
	#calul de l'état chimique à la station (valaur minimale hors indeterminé)
	# on regarde dabord les parametres déterminés puis indeterminés
	INDICECHIMSTAT_det<-aggregate(IndiceChim ~ STATION, data = RLT_CHIMCMAMA[RLT_CHIMCMAMA$CLASSEETAT != 0,], min, na.rm = TRUE)
	condindeterm<-RLT_CHIMCMAMA$CLASSEETAT ==0 &  !(RLT_CHIMCMAMA$STATION %in% INDICECHIMSTAT_det$STATION)
	if( nrow(RLT_CHIMCMAMA[condindeterm,])> 0) {
	INDICECHIMSTAT_indet<-aggregate(IndiceChim ~ STATION, data = RLT_CHIMCMAMA[condindeterm,], min, na.rm = TRUE)
	INDICECHIMSTAT<-rbind(INDICECHIMSTAT_det,INDICECHIMSTAT_indet)
	rm(INDICECHIMSTAT_indet,INDICECHIMSTAT_det)
	} else {
	INDICECHIMSTAT<-INDICECHIMSTAT_det
	rm(INDICECHIMSTAT_det)
	}
	names(INDICECHIMSTAT)[2]<-"IndiceChim_Station"
	RLT_CHIMCMAMA<-merge(RLT_CHIMCMAMA,INDICECHIMSTAT,by="STATION",all.x = TRUE)
	gc()
	
	## même chose : On calcule un indice ss hubiquiste
	INDICECHIMSTAT_det<-aggregate(IndiceChim ~ STATION, data = RLT_CHIMCMAMA[RLT_CHIMCMAMA$CLASSEETAT != 0 & RLT_CHIMCMAMA$HAPUBI != "oui",], min, na.rm = TRUE)
	condindeterm<-RLT_CHIMCMAMA$CLASSEETAT ==0 &  !(RLT_CHIMCMAMA$STATION %in% INDICECHIMSTAT_det$STATION) & RLT_CHIMCMAMA$HAPUBI != "oui"
	if( nrow(RLT_CHIMCMAMA[condindeterm,])> 0) {
	INDICECHIMSTAT_indet<-aggregate(IndiceChim ~ STATION, data = RLT_CHIMCMAMA[condindeterm,], min, na.rm = TRUE)
	INDICECHIMSTAT<-rbind(INDICECHIMSTAT_det,INDICECHIMSTAT_indet)
	rm(INDICECHIMSTAT_indet,INDICECHIMSTAT_det)
	} else {
	INDICECHIMSTAT<-INDICECHIMSTAT_det
	rm(INDICECHIMSTAT_det)
	}
	names(INDICECHIMSTAT)[2]<-"IndiceChim_Station_horsHAPUBI"
	RLT_CHIMCMAMA<-merge(RLT_CHIMCMAMA,INDICECHIMSTAT,by="STATION",all.x = TRUE)
	gc()
	
	
	## RAJOUT STATIONTXT	
	RLT_CHIMCMAMA_NAMES<-names(RLT_CHIMCMAMA)
	if (nchar(as.character(RLT_CHIMCMAMA$STATION))[1] == 7 & !is.na(RLT_CHIMCMAMA$STATION[1])) {
			RLT_CHIMCMAMA$STATIONTXT<-as.character(RLT_CHIMCMAMA$STATION)
			RLT_CHIMCMAMA$STATIONTXT[nchar(as.character(RLT_CHIMCMAMA$STATION)) == 7 & !is.na(RLT_CHIMCMAMA$STATION)]<-paste("0",RLT_CHIMCMAMA$STATION[nchar(as.character(RLT_CHIMCMAMA$STATION)) == 7 & !is.na(RLT_CHIMCMAMA$STATION)],sep="")
			RLT_CHIMCMAMA<-RLT_CHIMCMAMA[,c("STATIONTXT",RLT_CHIMCMAMA_NAMES)]
			RLT_CHIMCMAMA_NAMES<-names(RLT_CHIMCMAMA)
	}
	
	#On réordonne
	RLT_CHIMCMAMA<-RLT_CHIMCMAMA[order(RLT_CHIMCMAMA$STATION,RLT_CHIMCMAMA$PARAGROUP),]
	
}
