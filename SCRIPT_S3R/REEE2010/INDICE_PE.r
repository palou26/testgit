##############################################
## SEEE - PLAN  D'EAU : état physicochimique
## Application de l'arrêté de janvier 2010

## script crée en Mai 2016
##############################################

##############################################
## CALCUL DES RANG 90 / VALEUR MOYENNE / INDICE (type SEQ-eau)

## PCH
# principe de calcul de l'indice
# (((résultat - valeur inf borne)*20) / (valeur sup borne - valeur inf borne)) + indice borne inf

### BIO
# principe de calcul de l'indice par ordre croissant (IMOL, IBG, IBGA)
# (((résultat - valeur inf borne)*20) / (valeur sup borne - valeur inf borne)) + indice borne inf


### NOTA :
# La fonction "merge" retrie le vecteur. 
# on déclare donc les conditions "condXXX" après chaque "merge" pour s'assurer que le true/false créé avec les conditions est toujours dans le même ordre que le merge.
# sinon le résultat ne correspond pas à la condition.
# On aurait pu rester comme initialement, avec 1 condition pour les paramètres et 1 autre pour les classes et la combinaison des 2 donne la condition globale,
# mais comme on les déclarait avant les merge ça obligait à définir un ID pour tjs trier les enregistrements dans le même sens et faire un "order" après chaque "merge".
# on a jugé cette solution moins robuste que de répéter les conditions après chaque merge, même si ça engendre un code avec plus de lignes.

#########################################
# PCH INDICE : Paramètre ordre croissant




if (SEEEPCH=="oui") {
	gc()
	RLT_PCH<-merge(RLT_PCH,PARAGROUPPCH[,c("PARAGROUP", "IDTRI", "SUPB", "INFB", "INFV", "INFJ", "INFO", "INFR")],
			by.x="PARAMETRE", by.y = "PARAGROUP", all.x=TRUE, sort = FALSE)
	FORMULEINDICE<- function (DAT=RLT_PCH) {
		DAT$INDICE[condPCH1]<-round((((DAT$RESULTAT[condPCH1]-DAT$INFB[condPCH1])*20)/(DAT$SUPB[condPCH1]-DAT$INFB[condPCH1]))+80) 
		DAT$INDICE[condPCH2]<-round((((DAT$RESULTAT[condPCH2]-DAT$INFV[condPCH2])*20)/(DAT$INFB[condPCH2]-DAT$INFV[condPCH2]))+60)
		DAT$INDICE[condPCH3]<-round((((DAT$RESULTAT[condPCH3]-DAT$INFJ[condPCH3])*20)/(DAT$INFV[condPCH3]-DAT$INFJ[condPCH3]))+40)
		DAT$INDICE[condPCH4]<-round((((DAT$RESULTAT[condPCH4]-DAT$INFO[condPCH4])*20)/(DAT$INFJ[condPCH4]-DAT$INFO[condPCH4]))+20)
		DAT$INDICE[condPCH5]<-round((((DAT$RESULTAT[condPCH5]-DAT$INFR[condPCH5])*20)/(DAT$INFO[condPCH5]-DAT$INFR[condPCH5]))+0)
		DAT$INDICE[DAT$INDICE>=100]<-100
		DAT$INDICE[DAT$INDICE<=0]<-0
		DAT
	}

	#ordre croissant
	condPCH1<-RLT_PCH$PARAMETRE != "transpa" & !is.na(RLT_PCH$CLASSEPCH) & RLT_PCH$RESULTAT<=RLT_PCH$INFB
	condPCH2<-RLT_PCH$PARAMETRE != "transpa" & !is.na(RLT_PCH$CLASSEPCH) & RLT_PCH$RESULTAT>RLT_PCH$INFB & RLT_PCH$RESULTAT<=RLT_PCH$INFV
	condPCH3<-RLT_PCH$PARAMETRE != "transpa" & !is.na(RLT_PCH$CLASSEPCH) & RLT_PCH$RESULTAT>RLT_PCH$INFV & RLT_PCH$RESULTAT<=RLT_PCH$INFJ
	condPCH4<-RLT_PCH$PARAMETRE != "transpa" & !is.na(RLT_PCH$CLASSEPCH) & RLT_PCH$RESULTAT>RLT_PCH$INFJ & RLT_PCH$RESULTAT<=RLT_PCH$INFO
	condPCH5<-RLT_PCH$PARAMETRE != "transpa" & !is.na(RLT_PCH$CLASSEPCH) & RLT_PCH$RESULTAT>RLT_PCH$INFO 

	
	
	RLT_PCH<-FORMULEINDICE(DAT = RLT_PCH)
	rm(condPCH1, condPCH2, condPCH3, condPCH4, condPCH5)
	gc()
	flush.console()

# PCH INDICE : Paramètre ordre décroissant (cas de la transparence)
	condPCH1<-RLT_PCH$PARAMETRE == "transpa" & !is.na(RLT_PCH$CLASSEPCH) & RLT_PCH$RESULTAT>=RLT_PCH$INFB
	condPCH2<-RLT_PCH$PARAMETRE == "transpa" & !is.na(RLT_PCH$CLASSEPCH) & RLT_PCH$RESULTAT>=RLT_PCH$INFV & RLT_PCH$RESULTAT<RLT_PCH$INFB
	condPCH3<-RLT_PCH$PARAMETRE == "transpa" & !is.na(RLT_PCH$CLASSEPCH) & RLT_PCH$RESULTAT>=RLT_PCH$INFJ & RLT_PCH$RESULTAT<RLT_PCH$INFV
	condPCH4<-RLT_PCH$PARAMETRE == "transpa" & !is.na(RLT_PCH$CLASSEPCH) & RLT_PCH$RESULTAT>=RLT_PCH$INFO & RLT_PCH$RESULTAT<RLT_PCH$INFJ
	condPCH5<-RLT_PCH$PARAMETRE == "transpa" & !is.na(RLT_PCH$CLASSEPCH)  & RLT_PCH$RESULTAT<RLT_PCH$INFO

	RLT_PCH<-FORMULEINDICE(DAT = RLT_PCH)
	rm(condPCH1, condPCH2, condPCH3, condPCH4, condPCH5)

	
# Cas spécifique de l'ILOX
	condilox<-RLT_PCH$PARAMETRE == "ilox"
	RLT_PCH$INDICE[condilox]<- 100-RLT_PCH$RESULTAT[condilox]
	RLT_PCH$INDICE[condilox & RLT_PCH$INDICE>=100]<-100
	RLT_PCH$INDICE[condilox & RLT_PCH$INDICE<=0]<-0

	# On met vide si classe indéterminée
	RLT_PCH$INDICE[RLT_PCH$CLASSEPCH==0]<-NA
	
	
# On réarrange les colonnes	
	RLT_PCH<-RLT_PCH[,c("IDTRI","STATION", "PARAMETRE",  "RESULTAT", "CLASSEPCH", "INDICE", "Freq")]
	RLT_PCH<-merge(RLT_PCH, STATION, by="STATION")
	gc()

	gc()
	flush.console()
	
# TABLEAU FINAL : mise en forme
	PARAGROUPPCH<-PARAGROUPPCH[order(PARAGROUPPCH$IDTRI),]
	
	# Tableau pour les valeurs au rang 90
	PCHVALEUR<-data.frame(STATION = sort(unique(RLT_PCH$STATION)))
		for (i in  1:nrow(PARAGROUPPCH)) {
		TEMPP<-RLT_PCH[RLT_PCH$PARAMETRE==PARAGROUPPCH$PARAGROUP[i],c("STATION","RESULTAT")]
		names(TEMPP)[2]<-paste("v",toupper(PARAGROUPPCH$PARAGROUP[i]), sep="")
		PCHVALEUR<-merge(PCHVALEUR,TEMPP,by="STATION", all=TRUE)
		rm(TEMPP)
		gc()
	}	

	# Tableau pour les indices par paramètre
	PCHINDICE<-data.frame(STATION = sort(unique(RLT_PCH$STATION)))
	for (i in  1:nrow(PARAGROUPPCH)) {
		TEMPP<-RLT_PCH[RLT_PCH$PARAMETRE==PARAGROUPPCH$PARAGROUP[i],c("STATION","INDICE")]
		names(TEMPP)[2]<-paste("i",toupper(PARAGROUPPCH$PARAGROUP[i]), sep="")
		PCHINDICE<-merge(PCHINDICE,TEMPP,by="STATION", all=TRUE)
		rm(TEMPP)
		gc()
	}	
	
	# Tableau pour les indices par élément de qualité
	RLT_PCH<-merge(RLT_PCH,PARAGROUPPCH[,c("PARAGROUP","ELTQUALITE")],by.x="PARAMETRE", by.y="PARAGROUP", all.x=TRUE)
	
	PCHELT_temp<-aggregate(INDICE ~ STATION + ELTQUALITE, data = RLT_PCH , min, na.rm=TRUE) # calcul de l'indice déclassant par élément
	names(PCHELT_temp)[3]<-"ETATELT"
			
	PCHELTINDICE<-data.frame(STATION = sort(unique(RLT_PCH$STATION)))
	for (i in  1:nrow(TABLEELT)  ) {
		TEMPELT<-PCHELT_temp[PCHELT_temp$ELTQUALITE==TABLEELT$ELTQUALITE[i],c("STATION", "ETATELT")]
		names(TEMPELT)[2]<-paste("i",toupper(TABLEELT$ELTQUALITE[i]), sep="")
		PCHELTINDICE<-merge(PCHELTINDICE,TEMPELT,by="STATION", all=TRUE)
		rm(TEMPELT)
	}
	
	# Tableau avec Indice ETAT PCH - demande spécifique AESN	
	if (BASSIN=="AESN") {
		PCHETATINDICE<-aggregate(ETATELT ~ STATION , data = PCHELT_temp , min, na.rm=TRUE) # calcul de l'indice de l'état PCH
		names(PCHETATINDICE)[2]<-"iETATPCH"
		iETATPCH_NAMES<-"iETATPCH"
		RLT_PCHINDICE<-merge(PCHVALEUR, PCHETATINDICE,by="STATION")
		RLT_PCHINDICE<-merge(RLT_PCHINDICE, PCHELTINDICE,by="STATION")
		RLT_PCHINDICE<-merge(RLT_PCHINDICE, PCHINDICE,by="STATION")
	} else {
		RLT_PCHINDICE<-merge(PCHVALEUR, PCHELTINDICE,by="STATION")
		RLT_PCHINDICE<-merge(RLT_PCHINDICE, PCHINDICE,by="STATION")
		rm(PCHVALEUR, PCHINDICE,PCHELT_temp)
	}	
	gc()	
}

######################################
## BIOLOGIE : VALEUR MOYENNE & INDICE 
######################################
if (SEEEBIO=="oui") {
	BIO_MOY$INDICEBIO<-as.numeric("")
#calcul indice IMOL (max = 8)
	condIMOL1<-BIO_MOY$PARAGROUP=="5857" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==1
	condIMOL2<-BIO_MOY$PARAGROUP=="5857" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==2
	condIMOL3<-BIO_MOY$PARAGROUP=="5857" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==3
	condIMOL4<-BIO_MOY$PARAGROUP=="5857" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==4
	condIMOL5<-BIO_MOY$PARAGROUP=="5857" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==5
	
	BIO_MOY$INDICEBIO[condIMOL1]<-round((((BIO_MOY$RESULTAT[condIMOL1]-BIO_MOY$INFB[condIMOL1])*20)/(8-BIO_MOY$INFB[condIMOL1]))+80) 
	BIO_MOY$INDICEBIO[condIMOL2]<-round((((BIO_MOY$RESULTAT[condIMOL2]-BIO_MOY$INFV[condIMOL2])*20)/(BIO_MOY$INFB[condIMOL2]-BIO_MOY$INFV[condIMOL2]))+60)
	BIO_MOY$INDICEBIO[condIMOL3]<-round((((BIO_MOY$RESULTAT[condIMOL3]-BIO_MOY$INFJ[condIMOL3])*20)/(BIO_MOY$INFV[condIMOL3]-BIO_MOY$INFJ[condIMOL3]))+40)
	BIO_MOY$INDICEBIO[condIMOL4]<-round((((BIO_MOY$RESULTAT[condIMOL4]-BIO_MOY$INFO[condIMOL4])*20)/(BIO_MOY$INFJ[condIMOL4]-BIO_MOY$INFO[condIMOL4]))+20)
	BIO_MOY$INDICEBIO[condIMOL5]<-round((((BIO_MOY$RESULTAT[condIMOL5]-BIO_MOY$INFR[condIMOL5])*20)/(BIO_MOY$INFO[condIMOL5]-BIO_MOY$INFR[condIMOL5]))+0.0)
	BIO_MOY$INDICEBIO[BIO_MOY$PARAGROUP=="5857" & BIO_MOY$INDICEBIO>100]<-100
	rm(condIMOL1, condIMOL2, condIMOL3, condIMOL4, condIMOL5)
	
#calcul indice IOBL
	condIOBL1<-BIO_MOY$PARAGROUP=="6346" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==1
	condIOBL2<-BIO_MOY$PARAGROUP=="6346" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==2
	condIOBL3<-BIO_MOY$PARAGROUP=="6346" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==3
	condIOBL4<-BIO_MOY$PARAGROUP=="6346" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==4
	condIOBL5<-BIO_MOY$PARAGROUP=="6346" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==5
	
	BIO_MOY$INDICEBIO[condIOBL1]<-round((((BIO_MOY$RESULTAT[condIOBL1]-BIO_MOY$INFB[condIOBL1])*20)/(20-BIO_MOY$INFB[condIOBL1]))+80) 
	BIO_MOY$INDICEBIO[condIOBL2]<-round((((BIO_MOY$RESULTAT[condIOBL2]-BIO_MOY$INFV[condIOBL2])*20)/(BIO_MOY$INFB[condIOBL2]-BIO_MOY$INFV[condIOBL2]))+60)
	BIO_MOY$INDICEBIO[condIOBL3]<-round((((BIO_MOY$RESULTAT[condIOBL3]-BIO_MOY$INFJ[condIOBL3])*20)/(BIO_MOY$INFV[condIOBL3]-BIO_MOY$INFJ[condIOBL3]))+40)
	BIO_MOY$INDICEBIO[condIOBL4]<-round((((BIO_MOY$RESULTAT[condIOBL4]-BIO_MOY$INFO[condIOBL4])*20)/(BIO_MOY$INFJ[condIOBL4]-BIO_MOY$INFO[condIOBL4]))+20)
	BIO_MOY$INDICEBIO[condIOBL5]<-round((((BIO_MOY$RESULTAT[condIOBL5]-BIO_MOY$INFR[condIOBL5])*20)/(BIO_MOY$INFO[condIOBL5]-BIO_MOY$INFR[condIOBL5]))+0.0)
	BIO_MOY$INDICEBIO[BIO_MOY$PARAGROUP=="6346" & BIO_MOY$INDICEBIO>100]<-100
	rm(condIOBL1, condIOBL2, condIOBL3, condIOBL4, condIOBL5)

#calcul indice CHLOA
	condCHLOA1<-BIO_MOY$PARAGROUP=="1439" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==1
	condCHLOA2<-BIO_MOY$PARAGROUP=="1439" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==2
	condCHLOA3<-BIO_MOY$PARAGROUP=="1439" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==3
	condCHLOA4<-BIO_MOY$PARAGROUP=="1439" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==4
	condCHLOA5<-BIO_MOY$PARAGROUP=="1439" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==5

	BIO_MOY$INDICEBIO[condCHLOA1]<-round((((BIO_MOY$INFB[condCHLOA1]-BIO_MOY$RESULTAT[condCHLOA1])*20)/(BIO_MOY$INFB[condCHLOA1]-0))+80)   
	BIO_MOY$INDICEBIO[condCHLOA2]<-round((((BIO_MOY$INFV[condCHLOA2]-BIO_MOY$RESULTAT[condCHLOA2])*20)/(BIO_MOY$INFV[condCHLOA2]-BIO_MOY$INFB[condCHLOA2]))+60)
	BIO_MOY$INDICEBIO[condCHLOA3]<-round((((BIO_MOY$INFJ[condCHLOA3]-BIO_MOY$RESULTAT[condCHLOA3])*20)/(BIO_MOY$INFJ[condCHLOA3]-BIO_MOY$INFV[condCHLOA3]))+40)
	BIO_MOY$INDICEBIO[condCHLOA4]<-round((((BIO_MOY$INFO[condCHLOA4]-BIO_MOY$RESULTAT[condCHLOA4])*20)/(BIO_MOY$INFO[condCHLOA4]-BIO_MOY$INFJ[condCHLOA4]))+20)
	BIO_MOY$INDICEBIO[condCHLOA5]<-round((((BIO_MOY$INFR[condCHLOA5]-BIO_MOY$RESULTAT[condCHLOA5])*20)/(BIO_MOY$INFR[condCHLOA5]-BIO_MOY$INFO[condCHLOA5]))+0.0)
	BIO_MOY$INDICEBIO[BIO_MOY$RESULTAT>BIO_MOY$INFR & BIO_MOY$CLASSEBIO==5]<-0
		
#calcul indice IPL
	condIPL1<-BIO_MOY$PARAGROUP=="6591" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==1
	condIPL2<-BIO_MOY$PARAGROUP=="6591" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==2
	condIPL3<-BIO_MOY$PARAGROUP=="6591" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==3
	condIPL4<-BIO_MOY$PARAGROUP=="6591" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==4
	condIPL5<-BIO_MOY$PARAGROUP=="6591" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==5

	BIO_MOY$INDICEBIO[condIPL1]<-round((((BIO_MOY$INFB[condIPL1]-BIO_MOY$RESULTAT[condIPL1])*20)/(BIO_MOY$INFB[condIPL1]-0))+80)   
	BIO_MOY$INDICEBIO[condIPL2]<-round((((BIO_MOY$INFV[condIPL2]-BIO_MOY$RESULTAT[condIPL2])*20)/(BIO_MOY$INFV[condIPL2]-BIO_MOY$INFB[condIPL2]))+60)
	BIO_MOY$INDICEBIO[condIPL3]<-round((((BIO_MOY$INFJ[condIPL3]-BIO_MOY$RESULTAT[condIPL3])*20)/(BIO_MOY$INFJ[condIPL3]-BIO_MOY$INFV[condIPL3]))+40)
	BIO_MOY$INDICEBIO[condIPL4]<-round((((BIO_MOY$INFO[condIPL4]-BIO_MOY$RESULTAT[condIPL4])*20)/(BIO_MOY$INFO[condIPL4]-BIO_MOY$INFJ[condIPL4]))+20)
	BIO_MOY$INDICEBIO[condIPL5]<-round((((BIO_MOY$INFR[condIPL5]-BIO_MOY$RESULTAT[condIPL5])*20)/(BIO_MOY$INFR[condIPL5]-BIO_MOY$INFO[condIPL5]))+0.0)
	BIO_MOY$INDICEBIO[BIO_MOY$RESULTAT>BIO_MOY$INFR & BIO_MOY$CLASSEBIO==5]<-0
		


	# On met vide si classe indéterminée
	BIO_MOY$INDICEBIO[BIO_MOY$CLASSEBIO==0]<-NA
	
			
	BIO_MOY<-BIO_MOY[,c(BIO_MOY_NAMES, "INDICEBIO")]
	rm(condIPL1, condIPL2, condIPL3, condIPL4, condIPL5)
	flush.console()
	
# TABLEAU FINAL : mise en forme
	# Tableau pour les valeurs moyennes
	TEMP_BIOMOY<-data.frame(RLT_BIO[,"STATION"])
	names(TEMP_BIOMOY)[1]<-"STATION"
	for (i in  1:nrow(TABLEBIO)  ) {
		TEMPBIO<-BIO_MOY[BIO_MOY$PARAGROUP==TABLEBIO$PARAGROUP[i],c("STATION", "RESULTAT")]
		names(TEMPBIO)[2]<-paste ("v",TABLEBIO$PARALIBGROUP[i], sep="")
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
		RLT_BIOINDICE<-merge(TEMP_BIOMOY,TEMP_BIOINDICEETAT,by="STATION")
		RLT_BIOINDICE<-merge(RLT_BIOINDICE,TEMP_BIOINDICE,by="STATION")
		rm(TEMP_BIOMOY, TEMP_BIOINDICE, TEMP_BIOINDICEETAT)
	} else {
		RLT_BIOINDICE<-merge(TEMP_BIOMOY,TEMP_BIOINDICE,by="STATION")
		rm(TEMP_BIOMOY, TEMP_BIOINDICE)	
	}		

	
#### SIMULATION POUR LE 1/4 de classe biologique
## simulation ajouté le 02/02/15
# assouplissement de la bio si 1 seul indicateur est J alors que les autres B ou V
# il faut 3 indicateurs présents minimum pour pouvoir assouplir. on ne tient pas compte de la PCH.
# l'indice de l'indicateur J doit etre dans le 1/4 supérieur de la classe J --> entre [55 et 60[
	if (SIMULQUARTCLASSE=="oui") {
		# calcul du nb d'indicateur bio présent ; IBGA ne compte pas comme un indicateur bio, il est confondu avec IBG
		TEMPSIMUL<-RLT_BIO
		TEMPSIMUL$NBINDICBIO<-as.numeric(0)
		TEMPSIMUL$NBINDICBIO[!is.na(TEMPSIMUL$IMOL)]<-TEMPSIMUL$NBINDICBIO[!is.na(TEMPSIMUL$IMOL)]+1
		TEMPSIMUL$NBINDICBIO[!is.na(TEMPSIMUL$IPR)]<-TEMPSIMUL$NBINDICBIO[!is.na(TEMPSIMUL$IPR)]+1
		TEMPSIMUL$NBINDICBIO[!is.na(TEMPSIMUL$IBMR)]<-TEMPSIMUL$NBINDICBIO[!is.na(TEMPSIMUL$IBMR)]+1	
		if (BASSIN =="AESN"){ # AESN moyenne IBGA et IBG --> ligne spécifique sinon condidation marche pas
			TEMPSIMUL$NBINDICBIO[!is.na(TEMPSIMUL$IBG)]<-TEMPSIMUL$NBINDICBIO[!is.na(TEMPSIMUL$IBG)]+1 
		} else {
			TEMPSIMUL$NBINDICBIO[!is.na(TEMPSIMUL$IBG) | !is.na(TEMPSIMUL$IBGA)]<-TEMPSIMUL$NBINDICBIO[!is.na(TEMPSIMUL$IBG) | !is.na(TEMPSIMUL$IBGA)]+1 
		}
		
		# calcul du nb d'indicateur J quand l'état bio est J
		TEMPSIMUL$NBBIOJAUNE<-as.numeric(0)	
		TEMPSIMUL$NBBIOJAUNE[TEMPSIMUL$ETATBIO ==3 & TEMPSIMUL$IMOL==3 & !is.na(TEMPSIMUL$IMOL)]<-TEMPSIMUL$NBBIOJAUNE[TEMPSIMUL$ETATBIO ==3 & TEMPSIMUL$IMOL==3 & !is.na(TEMPSIMUL$IMOL)]+1
		TEMPSIMUL$NBBIOJAUNE[TEMPSIMUL$ETATBIO ==3 & TEMPSIMUL$IPR==3 & !is.na(TEMPSIMUL$IPR)]<-TEMPSIMUL$NBBIOJAUNE[TEMPSIMUL$ETATBIO ==3 & TEMPSIMUL$IPR==3 & !is.na(TEMPSIMUL$IPR)]+1
		TEMPSIMUL$NBBIOJAUNE[TEMPSIMUL$ETATBIO ==3 & TEMPSIMUL$IBMR==3 & !is.na(TEMPSIMUL$IBMR)]<-TEMPSIMUL$NBBIOJAUNE[TEMPSIMUL$ETATBIO ==3 & TEMPSIMUL$IBMR==3 & !is.na(TEMPSIMUL$IBMR)]+1
		if (BASSIN =="AESN"){ # AESN moyenne IBGA et IBG --> ligne spécifique sinon condidation marche pas
			TEMPSIMUL$NBBIOJAUNE[TEMPSIMUL$ETATBIO ==3 & TEMPSIMUL$IBG==3 & !is.na(TEMPSIMUL$IBG)]<-TEMPSIMUL$NBBIOJAUNE[TEMPSIMUL$ETATBIO ==3 & TEMPSIMUL$IBG==3 & !is.na(TEMPSIMUL$IBG)]+1
		} else {
			TEMPSIMUL$NBBIOJAUNE[TEMPSIMUL$ETATBIO ==3 & ((TEMPSIMUL$IBG==3 & !is.na(TEMPSIMUL$IBG)) | ((TEMPSIMUL$IBGA==3 & !is.na(TEMPSIMUL$IBGA))))]<-TEMPSIMUL$NBBIOJAUNE[TEMPSIMUL$ETATBIO ==3 & ((TEMPSIMUL$IBG==3 & !is.na(TEMPSIMUL$IBG)) | ((TEMPSIMUL$IBGA==3 & !is.na(TEMPSIMUL$IBGA))))]+1
		}
		
		# détermine si quart de classe peut s'appliquer
		QUARTCLASSE<-merge(RLT_BIOINDICE[,c("STATION",iPARAMETREBIO_NAMES)],TEMPSIMUL,by="STATION",all.x=TRUE)
		condinitial<-QUARTCLASSE$ETATBIO==3 & QUARTCLASSE$NBINDICBIO>=3 & QUARTCLASSE$NBBIOJAUNE==1
		QUARTCLASSE$QUARTCLASSE<-as.character("non")
		QUARTCLASSE$QUARTCLASSE[condinitial & QUARTCLASSE$iIMOL >=55 & QUARTCLASSE$iIMOL <60 & !is.na(QUARTCLASSE$iIMOL) ]<-"oui"
		QUARTCLASSE$QUARTCLASSE[condinitial & QUARTCLASSE$iIBG >=55 & QUARTCLASSE$iIBG <60 & !is.na(QUARTCLASSE$iIBG) ]<-"oui"
		QUARTCLASSE$QUARTCLASSE[condinitial & QUARTCLASSE$iIBGA >=55 & QUARTCLASSE$iIBGA <60 & !is.na(QUARTCLASSE$iIBGA) ]<-"oui" # if pas nécessaire si bassin ==AESN
		QUARTCLASSE$QUARTCLASSE[condinitial & QUARTCLASSE$iIPR >=55 & QUARTCLASSE$iIPR <60 & !is.na(QUARTCLASSE$iIPR) ]<-"oui"
		QUARTCLASSE$QUARTCLASSE[condinitial & QUARTCLASSE$iIBMR >=55 & QUARTCLASSE$iIBMR <60 & !is.na(QUARTCLASSE$iIBMR) ]<-"oui"
		#rm(condinitial,TEMPSIMUL)
		
		# ajout de la colonne QUARTCLASSE au résultat
		RLT_BIOINDICE<-merge(RLT_BIOINDICE,QUARTCLASSE[,c("STATION","QUARTCLASSE")],by="STATION",all.x=TRUE)
		QUARTCLASSE_NAMES<-"QUARTCLASSE" # utile pour affiche colonne dans export
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
		RLT_POLSPEINDICE<-merge(RLT_POLSPEINDICE,TEMPPOLSPE,by="STATION", all.x=TRUE)
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


# iETATECO
if (BASSIN =="AESN" & SEEEBIO=="oui" & SEEEPCH=="oui" & SEEEPS=="oui") {
	ETATECOLOINDICE<-merge(RLT_PCHINDICE[,c("STATION","iETATPCH")],RLT_BIOINDICE[,c("STATION","iETATBIO")],by="STATION",all.x=TRUE,all.y=TRUE)
	ETATECOLOINDICE<-merge(ETATECOLOINDICE,RLT_POLSPEINDICE[,c("STATION","iETATPS")],by="STATION",all.x=TRUE,all.y=TRUE)
	ETATECOLOINDICE$iETATECOLO<-pmin(ETATECOLOINDICE$iETATPCH,ETATECOLOINDICE$iETATBIO,   ETATECOLOINDICE$iETATPS,  na.rm = TRUE) #na.rm permet de ne pas tenir compte des NA
	ETATECOLOINDICE<-ETATECOLOINDICE[,c("STATION","iETATECOLO")]
	iETATECOLO_NAMES<-"iETATECOLO"
}


###########################################
## ETAT CHIMIQUE : VALEUR MOYENNE
# Extraction uniquement de la valeur moyenne. Indice non calculé car non demandé par les clients pour le moment
###########################################
if (SEEECHIM=="oui") {
# Mise en forme du tableau avec les classes d'état & valeurs moyennes (MOY, MOYMIN, MOYMAX)
	RLT_CHIMCMAMA<-CHIMCMAMA[,c(2:(ncol(CHIMCMAMA)))]
	RLT_CHIMCMAMA<-merge(RLT_CHIMCMAMA, PARAGROUPCHIM[,c("PARAGROUP","PARAGROUPLIBCOURT","FAMILLE","PARAGROUPLIB")],by="PARAGROUP")
	RLT_CHIMCMAMA<-RLT_CHIMCMAMA[,c("STATION","PARAGROUP","PARAGROUPLIBCOURT", "FAMILLE", "QUANTIFEAN", "RESULTATMAX", "NQECMA","CLASSECMA", "MOY", "MOYMIN", "MOYMAX", "LQMAX","NQEMA","CLASSEMA", "CLASSEETAT","PARAGROUPLIB", "GROUPE")]
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
	
	## RAJOUT STATIONTXT	
	RLT_CHIMCMAMA_NAMES<-names(RLT_CHIMCMAMA)
	if (nchar(as.character(RLT_CHIMCMAMA$STATION))[1] == 7 & !is.na(RLT_CHIMCMAMA$STATION[1])) {
			RLT_CHIMCMAMA$STATIONTXT<-as.character(RLT_CHIMCMAMA$STATION)
			RLT_CHIMCMAMA$STATIONTXT[nchar(as.character(RLT_CHIMCMAMA$STATION)) == 7 & !is.na(RLT_CHIMCMAMA$STATION)]<-paste("0",RLT_CHIMCMAMA$STATION[nchar(as.character(RLT_CHIMCMAMA$STATION)) == 7 & !is.na(RLT_CHIMCMAMA$STATION)],sep="")
			RLT_CHIMCMAMA<-RLT_CHIMCMAMA[,c("STATIONTXT",RLT_CHIMCMAMA_NAMES)]
			RLT_CHIMCMAMA_NAMES<-names(RLT_CHIMCMAMA)
	}

	
	
}
