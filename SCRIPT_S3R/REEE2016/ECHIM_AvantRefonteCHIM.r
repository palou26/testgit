##############################################
## SEEE - COURS D'EAU : �tat chimique
## Application de l'arr�t� de janvier 2010
##  script modifi� en juin 2015 pour les r�gles du 2nd cycle
##############################################

##############################
## Mise en forme des donn�es
##############################
gc() ## compacte R
DATACHIM$IDAN<-as.character(paste0(DATACHIM$STATION,DATACHIM$PARAMETRE,DATACHIM$ANNEE))

######################################################################
## METHODE : choix de la m�thode pour la preparation des donn�es brutes
# ANNEERECENTE : extraction des donn�es de l'ann�e la + r�cente pour chaque param�tre de la station
# ANNEECHRONIQUE : extraction des donn�es d'une station pour l'ensemble des param�tres ayant la chronique la plus compl�te
######################################################################

### Extraction selon les param�tres prioritaires notamment pour les groupes afin de recalculer les valeurs brutes
NAMES_DATACHIM<-names(DATACHIM)
DATACHIM<-merge(DATACHIM, PARAMETRECHIM[,c("PARAMETRE","PRIORITE")], by="PARAMETRE")
DATACHIM<-merge(DATACHIM, LIAISONCHIM, by="PARAMETRE")
TEMPDATA<-aggregate(PRIORITE ~ STATION + PARAGROUP + DATEPRELEV , data = DATACHIM , min)
DATACHIM<-DATACHIM[c(paste0(DATACHIM$STATION,DATACHIM$PARAGROUP,DATACHIM$DATEPRELEV,DATACHIM$PRIORITE) %in% paste0(TEMPDATA$STATION,TEMPDATA$PARAGROUP,TEMPDATA$DATEPRELEV,TEMPDATA$PRIORITE)),]
DATACHIM<-DATACHIM[,NAMES_DATACHIM]
rm(TEMPDATA)

### M�thode de calcul en fonction de l'ann�e la plus r�cente ou sur un chronique
if (METHODEMOYCHIM =="ANNEERECENTE") {
	CHIMANNEE<-aggregate(ANNEE ~ STATION + PARAMETRE, data = DATACHIM , max)
	CHIMANNEE$IDAN<-as.character(paste0(CHIMANNEE$STATION,CHIMANNEE$PARAMETRE,CHIMANNEE$ANNEE))
	DATACHIM<-DATACHIM[DATACHIM$IDAN %in% c(CHIMANNEE$IDAN),]
} else if (METHODEMOYCHIM =="ANNEECHRONIQUE") {
	CHIMANNEE<-aggregate(RESULTAT ~ STATION + PARAMETRE + ANNEE, data = DATACHIM , length)
	CHIMANNEE$IDAN<-as.character(paste0(CHIMANNEE$STATION,CHIMANNEE$PARAMETRE,CHIMANNEE$ANNEE))
	CHIMANNEE$IDFREQ<-as.character(paste0(CHIMANNEE$STATION,CHIMANNEE$PARAMETRE,CHIMANNEE$RESULTAT))
	CHIMANNEE<-CHIMANNEE[order(CHIMANNEE$IDAN),]
	
	TEMPCHIMANNEE1<-aggregate(RESULTAT ~ STATION + PARAMETRE, data = CHIMANNEE , max) # colonne RESULTAT non renomm�e mais contient le nb de prelevement
	TEMPCHIMANNEE1$IDFREQ<-as.character(paste0(TEMPCHIMANNEE1$STATION,TEMPCHIMANNEE1$PARAMETRE,TEMPCHIMANNEE1$RESULTAT))
	CHIMANNEE<-CHIMANNEE[CHIMANNEE$IDFREQ %in% c(TEMPCHIMANNEE1$IDFREQ),]
		
	#extraction de l'ann�e la + r�cente si plusieurs ann�es ont les m�mes fr�quences
	TEMPCHIMANNEE2<-aggregate(ANNEE ~ STATION + PARAMETRE, data = CHIMANNEE , max)
	TEMPCHIMANNEE2$IDAN<-as.character(paste0(TEMPCHIMANNEE2$STATION,TEMPCHIMANNEE2$PARAMETRE,TEMPCHIMANNEE2$ANNEE))
	DATACHIM<-DATACHIM[DATACHIM$IDAN %in% c(TEMPCHIMANNEE2$IDAN),]
	
	#on recr�e CHIMANNEE depuis DATACHIM filtr� pour calculer les fr�quences. CHIMANNEE est ainsi identique � "ANNEERECENTE" dans FREQUENCE.r --> �a facilite le script et les �ventuelles modif.
	CHIMANNEE<-aggregate(ANNEE ~ STATION + PARAMETRE, data = DATACHIM , max)
	
	rm(TEMPCHIMANNEE1,TEMPCHIMANNEE2)
}

# Mise en forme de la table principale
DATACHIM<-merge(DATACHIM, LISTECODERQE[,c("REMARQUE","QUANTIFIE")], by="REMARQUE")
DATACHIM<-merge(DATACHIM, LIAISONCHIM, by="PARAMETRE")
DATACHIM<-merge(DATACHIM,PARAGROUPCHIM,by="PARAGROUP")

# Calcul de la frequence de pr�l�vement
DATACHIM<-DATACHIM[!duplicated(DATACHIM[,c("STATION", "PARAGROUP", "PARAMETRE","DATEPRELEV", "HEUREPRELEV")]),]
DATACHIM_FREQ<-aggregate(RESULTAT ~ STATION + PARAMETRE + PARAGROUP + ANNEE, data = DATACHIM , length) # les donn�es sont d�j� filtr�es sur l'ann�e, on le prend dans aggregate uniquement pour l'avoir ensuite dans IDAN
names(DATACHIM_FREQ)[5]<-"FREQ"
DATACHIM_FREQ<-DATACHIM_FREQ[!duplicated(DATACHIM_FREQ[,c("STATION","PARAMETRE","ANNEE")]),]
DATACHIM_FREQ$IDAN<-as.character(paste0(DATACHIM_FREQ$STATION,DATACHIM_FREQ$PARAMETRE,DATACHIM_FREQ$ANNEE))
DATACHIM_FREQ<-DATACHIM_FREQ[,c("IDAN","FREQ")]

DATACHIM<-merge(DATACHIM,DATACHIM_FREQ,by="IDAN")
rm(DATACHIM_FREQ)
DATACHIMFREQ<-DATACHIM #save de DATACHIM avant retrait des freq non respect�es pour les afficher dans les stat sur l'ensemble des param�tres


## Respect de la fr�quence minimale de pr�l�vement par param�tre
if (FREQOKCHIM == "oui") {
	TEMPCHIM_RETRAIT<-DATACHIM[DATACHIM$FREQ<4, c("STATION", "PARAGROUP", "PARAMETRE", "ANNEE", "DATEPRELEV", "HEUREPRELEV","FREQ")]
	DATACHIM<-DATACHIM[c(!(paste0(DATACHIM$STATION, DATACHIM$PARAGROUP) %in% paste0(TEMPCHIM_RETRAIT$STATION, TEMPCHIM_RETRAIT$PARAGROUP))),] # retrait du parametre et/ou groupe de parametres si un parametre ne respecte pas la frequence
	#Export des stations qui ne sont plus dans le jeux de donn�es car la fr�quence est trop faible
	TEMPCHIM_RETRAIT<-TEMPCHIM_RETRAIT[!duplicated(TEMPCHIM_RETRAIT),]
	if( nrow(TEMPCHIM_RETRAIT>0)) {
		CSV<-paste(CH_ERREUR,"CHIM_FREQ_TROP_BASSE_",SEEE_DEBformat,".csv",sep="")
		write.csv2(TEMPCHIM_RETRAIT,CSV)
		MSGBOX <- tkmessageBox(title = "Info", message = paste("Les donn�es avec fr�quences trop basses ont �t� export�es dans \n",CSV,sep=""), icon = "info", type = "ok")

	}
}
gc()

DATACHIM<-merge(DATACHIM,STATION[,c("STATION","FONDGEO_CADMIUM","FONDGEO_PLOMB","FONDGEO_MERCURE","FONDGEO_NICKEL","DURETE")],by="STATION", all.x=TRUE)

# D�termination de NQEMA & CMA selon durete et Fond g�ochim
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

## Nouvelle matrice des NQEMA et MQECMA apr�s modif classe de duret� et fond g�ochim
CHIM_NQE<-DATACHIM[!duplicated(DATACHIM[,c("STATION","PARAGROUP")]),c("STATION","PARAGROUP","NQEMA","NQECMA","GROUPE")]
CHIM_NQE$IDNQE<-paste0(CHIM_NQE$STATION,"_",CHIM_NQE$PARAGROUP)

# Extraction de la LQMAX par station pour comparaison � la CMA & MA
if (LQ_NONDISPO =="oui") { # recherche LQmax dans colonne resultat si colonne LQ non dispo
	LQMAX<-DATACHIM[DATACHIM$QUANTIFIE=="0", c("STATION","PARAGROUP","QUANTIFIE","RESULTAT")]
	LQMAX<-aggregate(RESULTAT~ STATION + PARAGROUP, data = LQMAX , max)
	names(LQMAX)[3]<-"LQMAX"
	LQMAX$LQMAX<-round(LQMAX$LQMAX,5)
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
#identification des param�tres groupe quantifi� � minima 1 fois dans l'ann�e
CHIMCMA_QUANTIFAN<-aggregate(QUANTIFIE ~ STATION + PARAGROUP, data = DATACHIM , max)
names(CHIMCMA_QUANTIFAN)[3]<-"QUANTIFEAN"
CHIMCMA_QUANTIFAN$IDNQE<-paste(CHIMCMA_QUANTIFAN$STATION, CHIMCMA_QUANTIFAN$PARAGROUP,sep="_")
CHIMCMA_QUANTIFAN$ID<-paste(CHIMCMA_QUANTIFAN$STATION, CHIMCMA_QUANTIFAN$PARAGROUP, CHIMCMA_QUANTIFAN$QUANTIFEAN,sep="_")

CHIMCMA_MESUREMAX<-aggregate(RESULTAT ~ STATION + PARAGROUP + QUANTIFIE, data = DATACHIM , max)
names(CHIMCMA_MESUREMAX)[4]<-"RESULTATMAX"
CHIMCMA_MESUREMAX$RESULTATMAX<-round(CHIMCMA_MESUREMAX$RESULTATMAX,5)
 #correspond � RESULTATMAX si paragroup quantifi� ou � LQMAX qui paragroup non quantifi�
CHIMCMA_MESUREMAX$ID<-paste(CHIMCMA_MESUREMAX$STATION, CHIMCMA_MESUREMAX$PARAGROUP, CHIMCMA_MESUREMAX$QUANTIFIE,sep="_")


CHIMCMA<-CHIMCMA_QUANTIFAN
CHIMCMA<-merge(CHIMCMA,CHIM_NQE[,c("NQEMA","NQECMA","GROUPE","IDNQE")],by="IDNQE")
CHIMCMA<-merge(CHIMCMA,CHIMCMA_MESUREMAX[,c("RESULTATMAX","ID")],by="ID")

# CAS 1 - Comparaison NQECMA si paragroup quantifi� � minima 1 fois dans l'ann�e - On traite la mesure max
CHIMCMA$CLASSECMA<-as.numeric(NA)
CHIMCMA$CLASSECMA[CHIMCMA$QUANTIFEAN=="1" & CHIMCMA$RESULTATMAX <= CHIMCMA$NQECMA]<-1
CHIMCMA$CLASSECMA[CHIMCMA$QUANTIFEAN=="1" & CHIMCMA$RESULTATMAX > CHIMCMA$NQECMA]<-2

# CAS 2 - Comparaison NQECMA si paragroup n'est jamais quantifi� dans l'ann�e - On traite en r�alit� la LQMAX
CHIMCMA$CLASSECMA[CHIMCMA$QUANTIFEAN=="0" & CHIMCMA$RESULTATMAX <= CHIMCMA$NQECMA]<-1
CHIMCMA$CLASSECMA[CHIMCMA$QUANTIFEAN=="0" & CHIMCMA$RESULTATMAX > CHIMCMA$NQECMA]<-0
CHIMCMA$CLASSECMA[CHIMCMA$NQECMA==-99]<- -99

rm(CHIMCMA_QUANTIFAN,CHIMCMA_MESUREMAX)

##############################
## CHIMIE : comparaison MA
##
##############################

# # Pr�paration des valeurs pour calcul de la moyenne
# if (LQ_NONDISPO =="oui") { # si LQ non dispo on prend les valeurs du champ RESULTAT lorsque non quantifi� --> car c'est la LQ
	# DATACHIM$MOY[DATACHIM$QUANTIFIE=="1"]<-DATACHIM$RESULTAT[DATACHIM$QUANTIFIE=="1"] # pour valeur quantifi�e on conserve en l'�tat, car risque de ne pas avoir de LQ si quantifi� tt l'ann�e
	# DATACHIM$MOYMIN[DATACHIM$QUANTIFIE=="1"]<-DATACHIM$RESULTAT[DATACHIM$QUANTIFIE=="1"]
	# DATACHIM$MOYMAX[DATACHIM$QUANTIFIE=="1"]<-DATACHIM$RESULTAT[DATACHIM$QUANTIFIE=="1"]
	
	# condLQ2<-DATACHIM$QUANTIFIE=="0" & DATACHIM$GROUPE=="non" & DATACHIM$RESULTAT <= (DATACHIM$NQEMA*0.33)
	# condLQ0<-DATACHIM$QUANTIFIE=="0" & DATACHIM$GROUPE=="non" & DATACHIM$RESULTAT > (DATACHIM$NQEMA*0.33)
	# condLQ<-DATACHIM$QUANTIFIE=="0"  & DATACHIM$GROUPE=="non" & DATACHIM$RESULTAT > (DATACHIM$NQEMA*0.33)
	# #condition pour parametre en groupe
	# condLQgp<-DATACHIM$QUANTIFIE=="0" & DATACHIM$GROUPE=="oui" 
	
# } else {
	# condLQ2<-DATACHIM$GROUPE=="non" & DATACHIM$LQ <= (DATACHIM$NQEMA*0.33)
	# condLQ0<-DATACHIM$GROUPE=="non" & DATACHIM$LQ > (DATACHIM$NQEMA*0.33)
	# condLQ<-DATACHIM$GROUPE=="non" & DATACHIM$LQ > (DATACHIM$NQEMA*0.33)
	# #condition pour parametre en groupe
	# condLQgp<-DATACHIM$GROUPE=="oui"
	
	# DATACHIM$MOY<-DATACHIM$RESULTAT
	# DATACHIM$MOYMIN<-DATACHIM$RESULTAT
	# DATACHIM$MOYMAX<-DATACHIM$RESULTAT
# }

# modif� le 15/07/15 suite mail AESN
if (LQ_NONDISPO =="oui") { # si LQ non dispo on prend les valeurs du champ RESULTAT lorsque non quantifi� --> car c'est la LQ
	condLQ2<-DATACHIM$QUANTIFIE=="0" & DATACHIM$GROUPE=="non" & DATACHIM$RESULTAT <= (DATACHIM$NQEMA*0.33)
	condLQ0<-DATACHIM$QUANTIFIE=="0" & DATACHIM$GROUPE=="non" & DATACHIM$RESULTAT > (DATACHIM$NQEMA*0.33)
	condLQ<-DATACHIM$QUANTIFIE=="0"  & DATACHIM$GROUPE=="non" & DATACHIM$RESULTAT > (DATACHIM$NQEMA*0.33)
	#condition pour parametre en groupe
	condLQgp<-DATACHIM$QUANTIFIE=="0" & DATACHIM$GROUPE=="oui" 
	
} else {
	condLQ2<-DATACHIM$QUANTIFIE=="0" & DATACHIM$GROUPE=="non" & DATACHIM$LQ <= (DATACHIM$NQEMA*0.33)
	condLQ0<-DATACHIM$QUANTIFIE=="0" & DATACHIM$GROUPE=="non" & DATACHIM$LQ > (DATACHIM$NQEMA*0.33)
	condLQ<-DATACHIM$QUANTIFIE=="0" & DATACHIM$GROUPE=="non" & DATACHIM$LQ > (DATACHIM$NQEMA*0.33)
	#condition pour parametre en groupe
	condLQgp<-DATACHIM$QUANTIFIE=="0" & DATACHIM$GROUPE=="oui"
}
# modif� le 15/07/15 suite mail AESN
DATACHIM$MOY<-DATACHIM$RESULTAT
DATACHIM$MOYMIN<-DATACHIM$RESULTAT
DATACHIM$MOYMAX<-DATACHIM$RESULTAT

# Modif R�sultat pour parametre hors groupe avant calcul moyenne
DATACHIM$MOY[condLQ2]<-DATACHIM$RESULTAT[condLQ2]/2
DATACHIM$MOYMIN[condLQ0]<-0
DATACHIM$MOYMAX[condLQ]<-DATACHIM$RESULTAT[condLQ]
# Modif R�sultat pour parametre en groupe avant calcul moyenne
DATACHIM$MOY[condLQgp]<-0
DATACHIM$MOYMIN[condLQgp]<-0 # inutile mais on laisse pour �viter les NA
DATACHIM$MOYMAX[condLQgp]<-DATACHIM$RESULTAT[condLQgp] # inutile mais on laisse pour �viter les NA

# modif 24/04/15 : retrait prelevement pour calcul moyenne si LQ > NQE (mail Ministere du 08/04/15 � destination des agences pour cycle 2
if ( LQSUPNQEMACHIM == "oui" ) {
	if (LQ_NONDISPO =="oui") { # si LQ non dispo on prend les valeurs du champ RESULTAT lorsque non quantifi� --> car c'est la LQ
		PBLQCHIM<-DATACHIM[DATACHIM$RESULTAT>DATACHIM$NQEMA & DATACHIM$QUANTIFIE=="0" & DATACHIM$NQEMA != -99 ,1:27]
		DATACHIM<-DATACHIM[!(DATACHIM$RESULTAT>DATACHIM$NQEMA & DATACHIM$QUANTIFIE=="0") | DATACHIM$NQEMA == -99 ,]
	} else {
		PBLQCHIM<-DATACHIM[DATACHIM$LQ > DATACHIM$NQEMA & DATACHIM$NQEMA != -99 ,1:27]
		DATACHIM<-DATACHIM[DATACHIM$LQ <= DATACHIM$NQEMA | DATACHIM$NQEMA == -99  ,]
	}

	if( nrow(PBLQCHIM>0)) {
		CSV<-paste(CH_ERREUR,"CHIM_LQ_SUP_NQEMA_",SEEE_DEBformat,".csv",sep="")
		write.csv2(PBLQCHIM,CSV)
		MSGBOX <- tkmessageBox(title = "Info", message = paste("Les donn�es brutes avec LQ > NQEMA ont �t� retir�es \n du calcul de l'�tat chimique et export�es dans \n",CSV,sep=""), icon = "info", type = "ok")

	}	
	}
## OLD SCRIPT (S3R v1)
# # Pr�paration des valeurs pour calcul de la moyenne - CAS r�sultat quantifi�
# DATACHIM$MOY[DATACHIM$QUANTIFIE=="1"]<-DATACHIM$RESULTAT[DATACHIM$QUANTIFIE=="1"]
# DATACHIM$MOYMIN[DATACHIM$QUANTIFIE=="1"]<-DATACHIM$RESULTAT[DATACHIM$QUANTIFIE=="1"]
# DATACHIM$MOYMAX[DATACHIM$QUANTIFIE=="1"]<-DATACHIM$RESULTAT[DATACHIM$QUANTIFIE=="1"]

# # Pr�paration des valeurs pour calcul de la moyenne - CAS r�sultat non quantifi� et param�tre hors groupe
# DATACHIM$MOY[DATACHIM$QUANTIFIE=="0" & DATACHIM$GROUPE=="non"]<-DATACHIM$RESULTAT[DATACHIM$QUANTIFIE=="0" & DATACHIM$GROUPE=="non"]/2
# DATACHIM$MOYMIN[DATACHIM$QUANTIFIE=="0" & DATACHIM$GROUPE=="non"]<-0
# DATACHIM$MOYMAX[DATACHIM$QUANTIFIE=="0" & DATACHIM$GROUPE=="non"]<-DATACHIM$RESULTAT[DATACHIM$QUANTIFIE=="0" & DATACHIM$GROUPE=="non"]

# # Pr�paration des valeurs pour calcul de la moyenne - CAS r�sultat non quantifi� et param�tre en groupe
# DATACHIM$MOY[DATACHIM$QUANTIFIE=="0" & DATACHIM$GROUPE=="oui"]<-0
# DATACHIM$MOYMIN[DATACHIM$QUANTIFIE=="0" & DATACHIM$GROUPE=="oui"]<-0 # inutile pour guide dec2012, mais on laisse pour �viter les NA
# DATACHIM$MOYMAX[DATACHIM$QUANTIFIE=="0" & DATACHIM$GROUPE=="oui"]<-DATACHIM$RESULTAT[DATACHIM$QUANTIFIE=="0" & DATACHIM$GROUPE=="oui"]# inutile pour guide dec2012, mais on laisse pour �viter les NA

# Calcul des moyennes
CHIMMA<-aggregate(cbind(MOY, MOYMIN, MOYMAX)~ STATION + DATEPRELEV + PARAGROUP, data = DATACHIM , sum) # on somme les valeurs pour additionner les valeurs de param�tres en groupe.
CHIMMA<-aggregate(cbind(MOY, MOYMIN, MOYMAX)~ STATION + PARAGROUP, data = CHIMMA , mean)
CHIMMA$ID<-paste(CHIMMA$STATION, CHIMMA$PARAGROUP, sep="_")
CHIMMA$MOY<-round(CHIMMA$MOY,5)
CHIMMA$MOYMIN<-round(CHIMMA$MOYMIN,5)
CHIMMA$MOYMAX<-round(CHIMMA$MOYMAX,5)
 
## OLD SCRIPT (S3R v1)
# # Extraction de la LQMAX par station pour comparaison � la MA
# LQMAX<-DATACHIM[DATACHIM$QUANTIFIE=="0", c("STATION","PARAGROUP","QUANTIFIE","RESULTAT")]
# LQMAX<-aggregate(RESULTAT~ STATION + PARAGROUP, data = LQMAX , max)
# names(LQMAX)[3]<-"LQMAX"
# LQMAX$LQMAX<-round(LQMAX$LQMAX,5)
# LQMAX$ID<-paste(LQMAX$STATION, LQMAX$PARAGROUP, sep="_")

### D�termination classe MA
CHIMMA<-merge(CHIMMA,LQMAX[,c("ID","LQMAX")],by="ID", all.x=TRUE)
CHIMMA<-merge(CHIMMA,CHIM_NQE[,c("NQEMA","NQECMA","GROUPE","IDNQE")],by.x="ID",by.y="IDNQE")

##30/09/2015 : On supprime les pr�levements par PARAGROUP qui ont une valeur moyenne mesur�e est inf�rieure � la LQ
if ( LQSUPNQEMACHIM == "oui" ) {
	CHIMMAexept<-CHIMMA[CHIMMA$MOYMAX < CHIMMA$LQMAX,]
	CHIMMA<-CHIMMA[!(CHIMMA$MOYMAX < CHIMMA$LQMAX) ,]
}

# CAS 1 - Hors Groupe - Comparaison NQEMA LQmax <= 1/3 NQE
CHIMMA$CLASSEMA<-as.numeric(NA)
CHIMMA$CAS1<-as.character(NA)
CHIMMA$CLASSEMA[CHIMMA$GROUPE=="non" & (is.na(CHIMMA$LQMAX) | CHIMMA$LQMAX <= (CHIMMA$NQEMA*0.33)) & CHIMMA$MOY <=CHIMMA$NQEMA ]<-1  # LQmax > 1/3 NQEMA, is.na(CHIMMA$LQMAX) --> cas des prlvt tjs quantifi� donc pas de LQMAX trouv� dans les data
CHIMMA$CAS1[CHIMMA$GROUPE=="non" & (is.na(CHIMMA$LQMAX) | CHIMMA$LQMAX <= (CHIMMA$NQEMA*0.33)) & CHIMMA$MOY <=CHIMMA$NQEMA ]<-"oui"
CHIMMA$CLASSEMA[CHIMMA$GROUPE=="non" & (is.na(CHIMMA$LQMAX) | CHIMMA$LQMAX <= (CHIMMA$NQEMA*0.33)) & CHIMMA$MOY >CHIMMA$NQEMA ]<-2
CHIMMA$CAS1[CHIMMA$GROUPE=="non" & (is.na(CHIMMA$LQMAX) | CHIMMA$LQMAX <= (CHIMMA$NQEMA*0.33)) & CHIMMA$MOY >CHIMMA$NQEMA ]<-"oui"

# CAS 2 - Hors Groupe - Comparaison NQEMA LQmax > 1/3 NQE : encadremment avec MOYMIN
CHIMMA$CAS2<-as.character(NA)
CHIMMA$CLASSEMA[CHIMMA$GROUPE=="non" & !is.na(CHIMMA$LQMAX) & CHIMMA$LQMAX > (CHIMMA$NQEMA*0.33) & CHIMMA$MOYMIN >CHIMMA$NQEMA ]<-2 
CHIMMA$CAS2[CHIMMA$GROUPE=="non" & !is.na(CHIMMA$LQMAX) & CHIMMA$LQMAX > (CHIMMA$NQEMA*0.33) & CHIMMA$MOYMIN >CHIMMA$NQEMA ]<-"oui"

# CAS 3 - Hors Groupe - Comparaison NQEMA LQmax > 1/3 NQE  : encadremment avec MOYMAX
CHIMMA$CAS3<-as.character(NA)
CHIMMA$CLASSEMA[CHIMMA$GROUPE=="non" & !is.na(CHIMMA$LQMAX) & CHIMMA$LQMAX > (CHIMMA$NQEMA*0.33) & CHIMMA$MOYMIN <= CHIMMA$NQEMA  & CHIMMA$MOYMAX <= CHIMMA$NQEMA  ]<-1
CHIMMA$CAS3[CHIMMA$GROUPE=="non" & !is.na(CHIMMA$LQMAX) & CHIMMA$LQMAX > (CHIMMA$NQEMA*0.33) & CHIMMA$MOYMIN <= CHIMMA$NQEMA  & CHIMMA$MOYMAX <= CHIMMA$NQEMA  ]<-"oui" 
CHIMMA$CLASSEMA[CHIMMA$GROUPE=="non" & !is.na(CHIMMA$LQMAX) & CHIMMA$LQMAX > (CHIMMA$NQEMA*0.33) & CHIMMA$MOYMIN <= CHIMMA$NQEMA & CHIMMA$MOYMAX > CHIMMA$NQEMA  ]<-0
CHIMMA$CAS3[CHIMMA$GROUPE=="non" & !is.na(CHIMMA$LQMAX) & CHIMMA$LQMAX > (CHIMMA$NQEMA*0.33) & CHIMMA$MOYMIN <= CHIMMA$NQEMA & CHIMMA$MOYMAX > CHIMMA$NQEMA  ]<-"oui"

# CAS 4 - En GROUPE - Comparaison Moyenne annuelle - MQEMA
CHIMMA$CAS4<-as.character(NA)
CHIMMA$CLASSEMA[CHIMMA$GROUPE=="oui" & CHIMMA$MOY <=CHIMMA$NQEMA ]<-1
CHIMMA$CAS4[CHIMMA$GROUPE=="oui" & CHIMMA$MOY <=CHIMMA$NQEMA ]<-"oui"
CHIMMA$CLASSEMA[CHIMMA$GROUPE=="oui" & CHIMMA$MOY >CHIMMA$NQEMA ]<-2
CHIMMA$CAS4[CHIMMA$GROUPE=="oui" & CHIMMA$MOY >CHIMMA$NQEMA ]<-"oui"


##Si NQEMA = -99 (sans objet)
CHIMMA$CLASSEMA[CHIMMA$NQEMA == -99]<- -99
gc()

# # CAS 1 - Comparaison NQEMA Substances individuelles
# CHIMMA$CLASSEMA<-as.numeric(NA)
# CHIMMA$CLASSEMA[CHIMMA$GROUPE=="non" & (is.na(CHIMMA$LQMAX) | CHIMMA$LQMAX <= (CHIMMA$NQEMA*1.3)) & CHIMMA$MOY <= CHIMMA$NQEMA]<-1  # LQmax > 30% NQEMA
# CHIMMA$CLASSEMA[CHIMMA$GROUPE=="non" & (is.na(CHIMMA$LQMAX) | CHIMMA$LQMAX <= (CHIMMA$NQEMA*1.3)) & CHIMMA$MOY > CHIMMA$NQEMA]<-2
# CHIMMA$CLASSEMARQ[CHIMMA$GROUPE=="non" & CHIMMA$LQMAX <= (CHIMMA$NQEMA*1.3) & CHIMMA$MOY < CHIMMA$LQMAX]<-as.character("<LQmax")

# CHIMMA$CLASSEMA[CHIMMA$GROUPE=="non" & CHIMMA$LQMAX > (CHIMMA$NQEMA*1.3) & CHIMMA$MOYMIN > CHIMMA$NQEMA]<-2
# CHIMMA$CLASSEMA[CHIMMA$GROUPE=="non" & CHIMMA$LQMAX > (CHIMMA$NQEMA*1.3) & CHIMMA$MOYMAX <= CHIMMA$NQEMA]<-1
# CHIMMA$CLASSEMA[CHIMMA$GROUPE=="non" & CHIMMA$LQMAX > (CHIMMA$NQEMA*1.3) & (CHIMMA$MOYMIN <= CHIMMA$NQEMA & CHIMMA$MOYMAX > CHIMMA$NQEMA) ]<-0

# # CAS 2 - Comparaison NQEMA Substances en groupe
# CHIMMA$CLASSEMA[CHIMMA$GROUPE=="oui" & CHIMMA$MOY <= CHIMMA$NQEMA]<-1
# CHIMMA$CLASSEMA[CHIMMA$GROUPE=="oui" & CHIMMA$MOY > CHIMMA$NQEMA]<-2
# gc()

# ##############################################################################
# ## CHIMIE : comparaison MA
# ## M�thode de calcul du guide de mars 2009 (test� et v�rifi� avec data AEAG
# ##############################################################################
# # Pr�paration des valeurs pour calcul de la moyenne - CAS r�sultat quantifi�
# DATACHIM$MOY[DATACHIM$QUANTIFIE=="1"]<-DATACHIM$RESULTAT[DATACHIM$QUANTIFIE=="1"]
# DATACHIM$MOYMIN[DATACHIM$QUANTIFIE=="1"]<-DATACHIM$RESULTAT[DATACHIM$QUANTIFIE=="1"]
# DATACHIM$MOYMAX[DATACHIM$QUANTIFIE=="1"]<-DATACHIM$RESULTAT[DATACHIM$QUANTIFIE=="1"]

# # Pr�paration des valeurs pour calcul de la moyenne - CAS r�sultat non quantifi� et param�tre en groupe
# DATACHIM$MOY[DATACHIM$QUANTIFIE=="0" & DATACHIM$GROUPE=="oui"]<-0
# DATACHIM$MOYMIN[DATACHIM$QUANTIFIE=="0" & DATACHIM$GROUPE=="oui"]<-0
# DATACHIM$MOYMAX[DATACHIM$QUANTIFIE=="0" & DATACHIM$GROUPE=="oui"]<-DATACHIM$RESULTAT[DATACHIM$QUANTIFIE=="0" & DATACHIM$GROUPE=="oui"]

# # Pr�paration des valeurs pour calcul de la moyenne - CAS r�sultat non quantifi� et param�tre hors groupe
# DATACHIM$MOY[DATACHIM$QUANTIFIE=="0" & DATACHIM$GROUPE=="non"]<-DATACHIM$RESULTAT[DATACHIM$QUANTIFIE=="0" & DATACHIM$GROUPE=="non"]/2
# DATACHIM$MOYMIN[DATACHIM$QUANTIFIE=="0" & DATACHIM$GROUPE=="non"]<-0
# DATACHIM$MOYMAX[DATACHIM$QUANTIFIE=="0" & DATACHIM$GROUPE=="non"]<-DATACHIM$RESULTAT[DATACHIM$QUANTIFIE=="0" & DATACHIM$GROUPE=="non"]

# # Calcul des moyennes
# CHIMMA<-aggregate(cbind(MOY, MOYMIN, MOYMAX)~ STATION + DATEPRELEV + PARAGROUP, data = DATACHIM , sum) # on somme les valeurs pour additionner les valeurs de param�tres en groupe.
# CHIMMA<-aggregate(cbind(MOY, MOYMIN, MOYMAX)~ STATION + PARAGROUP, data = CHIMMA , mean)
# CHIMMA$ID<-paste(CHIMMA$STATION, CHIMMA$PARAGROUP, sep="_")

# # Extraction de la LQMAX par station pour comparaison � la MA
# LQMAX<-DATACHIM[DATACHIM$QUANTIFIE=="0", c("STATION","PARAGROUP","QUANTIFIE","RESULTAT")]
# LQMAX<-aggregate(RESULTAT~ STATION + PARAGROUP, data = LQMAX , max)
# names(LQMAX)[3]<-"LQMAX"
# LQMAX$ID<-paste(LQMAX$STATION, LQMAX$PARAGROUP, sep="_")

# CHIMMA<-merge(CHIMMA,LQMAX[,c("ID","LQMAX")],by="ID", all.x=TRUE)
# CHIMMA<-merge(CHIMMA,PARAGROUPCHIM,by="PARAGROUP")

# # CAS 1 - Comparaison NQEMA dans les conditions o� MOY>= LQMAX ou comparaison impossible car LQMAX non dispo car parametre est tjs quantifi� sur la station
# CHIMMA$CLASSEMA<-as.numeric(NA)
# CHIMMA$CLASSEMA[(is.na(CHIMMA$LQMAX) | CHIMMA$MOY >= CHIMMA$LQMAX) & CHIMMA$MOY < CHIMMA$NQEMA]<-1
# CHIMMA$CLASSEMA[(is.na(CHIMMA$LQMAX) | CHIMMA$MOY >= CHIMMA$LQMAX) & CHIMMA$MOY >= CHIMMA$NQEMA]<-2

# # CAS 2 - Comparaison NQEMA dans les conditions o� MOY<LQMAX
# CHIMMA$CLASSEMA[CHIMMA$MOY < CHIMMA$LQMAX & CHIMMA$NQEMA < CHIMMA$MOYMIN ]<-2
# CHIMMA$CLASSEMA[CHIMMA$MOY < CHIMMA$LQMAX & CHIMMA$MOYMAX <= CHIMMA$NQEMA]<-1
# CHIMMA$CLASSEMA[CHIMMA$MOY < CHIMMA$LQMAX & (CHIMMA$NQEMA >= CHIMMA$MOYMIN & CHIMMA$MOYMAX > CHIMMA$NQEMA)]<-0
# #CHIMMA$CLASSEMA[is.na(CHIMMA$CLASSEMA)]<-0
# gc()

##############################
## Calcul de l'�tat chimique
##############################
# Etat par PARAGROUP
CHIMCMA$ID<-paste(CHIMCMA$STATION, CHIMCMA$PARAGROUP, sep="_")
CHIMCMAMA<-merge(CHIMCMA[,c("STATION","PARAGROUP", "QUANTIFEAN","RESULTATMAX", "CLASSECMA", "ID")],CHIMMA[,c("ID", "MOY", "MOYMIN", "MOYMAX", "LQMAX", "NQEMA", "NQECMA","GROUPE","CLASSEMA")], by="ID")
CHIMCMAMA$CLASSEETAT<-as.numeric(NA)
CHIMCMAMA$CLASSEETAT[CHIMCMAMA$CLASSECMA==2 | CHIMCMAMA$CLASSEMA==2]<-2
CHIMCMAMA$CLASSEETAT[CHIMCMAMA$CLASSECMA==1 & CHIMCMAMA$CLASSEMA %in% c(-99,0)]<-0
CHIMCMAMA$CLASSEETAT[CHIMCMAMA$CLASSECMA==1 & CHIMCMAMA$CLASSEMA==1]<-1
CHIMCMAMA$CLASSEETAT[CHIMCMAMA$CLASSECMA %in% c(-99,0) & CHIMCMAMA$CLASSEMA==1]<-1
CHIMCMAMA$CLASSEETAT[CHIMCMAMA$CLASSECMA %in% c(-99,0) & CHIMCMAMA$CLASSEMA  %in% c(-99,0) ]<-0

# Etat par famille
CHIMFAMILLE<-merge(CHIMCMAMA,PARAGROUPCHIM,by="PARAGROUP")
CHIMFAMILLE<-aggregate(CLASSEETAT ~ STATION + FAMILLE, data = CHIMFAMILLE , max)
names(CHIMFAMILLE)[3]<-"ETATFAMILLE"

RLT_CHIMSTATION<-aggregate(ETATFAMILLE ~ STATION, data = CHIMFAMILLE , max)
names(RLT_CHIMSTATION)[2]<-"ETATCHIM"
RLT_CHIMSTATION<-RLT_CHIMSTATION[order(RLT_CHIMSTATION$STATION),]

##############################
## Mise en forme tableau final

##############################
PARAGROUPCHIM<-PARAGROUPCHIM[order(PARAGROUPCHIM$IDTRI),]

# Ajout colonnes des familles 
for (i in  1:nrow(TABLEFAMILLE)  ) {
	TEMPP<-CHIMFAMILLE[CHIMFAMILLE$FAMILLE==TABLEFAMILLE$FAMILLE[i],c("STATION", "ETATFAMILLE")]
	names(TEMPP)[2]<-TABLEFAMILLE$FAMILLE[i]
	RLT_CHIMSTATION<-merge(RLT_CHIMSTATION,TEMPP,by="STATION", all.x=TRUE)
	rm(TEMPP)
}
# Ajout colonnes des param�tres 

for (i in  1:nrow(PARAGROUPCHIM)  ) {
	TEMPP<-CHIMCMAMA[CHIMCMAMA$PARAGROUP==PARAGROUPCHIM$PARAGROUP[i],c("STATION", "CLASSEETAT")]
	names(TEMPP)[2]<-PARAGROUPCHIM$PARAGROUPLIBCOURT[i]
	RLT_CHIMSTATION<-merge(RLT_CHIMSTATION,TEMPP,by="STATION", all.x=TRUE)
	rm(TEMPP)
}

#### METTRE TOUT EN MAJUSCULE
names(RLT_CHIMSTATION)<-toupper(names(RLT_CHIMSTATION))


###########################################
####RAJOUT DES PARAMETRES DECLASSANTS
#### DU NOMBRE DE PARAMETRE DECLASSANT
## DU NOMBRE DE PARAMETRE INDETERMINE
## PPL, 24/09/2015
##########################################


	### Fonction pour sortir les parametres d�classants 
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


#Nombre de parametres ind�termin�s	
temp<-rep(0,nrow(RLT_CHIMSTATION))
for ( i in NAMES_PARAMCHIM) {
	temp[is.na(RLT_CHIMSTATION[,i]) | RLT_CHIMSTATION[,i] == 0 ]<-temp[is.na(RLT_CHIMSTATION[,i]) | RLT_CHIMSTATION[,i] == 0 ]+1
}
RLT_CHIMSTATION$N_PARAM_IND<-temp


# Ajout de la colonne "REPRESENTATIVE"
RLT_CHIMSTATION<-merge(RLT_CHIMSTATION, STATION[,c("STATION","REPRESENTATIVE","ECHELLESTA")], by="STATION", all.x=TRUE)





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
	TEMPECHIMME<-merge(CHIMCMAMA[,c("STATION", "PARAGROUP","CLASSEETAT")], STATION[,c("STATION", "EUCD", "REPRESENTATIVE")], by="STATION")
	TEMPECHIMME_OUI<-TEMPECHIMME[TEMPECHIMME$REPRESENTATIVE %in% c("oui"),]
	TEMPECHIMME_TEMPO<-TEMPECHIMME[TEMPECHIMME$REPRESENTATIVE %in% c("temporaire"),]
	
	# Calcul de l'�tat ME pour les stations repr�sentatives
	TEMPECHIMME_CAS1<-aggregate(CLASSEETAT ~ EUCD + REPRESENTATIVE, data = TEMPECHIMME_OUI, max)
	
	# Calcul de l'�tat ME pour les stations temporaires s'il n'y a pas de station repr�sentative
	if (nrow(TEMPECHIMME_TEMPO)>0) {
		TEMPECHIMME_CAS2<-aggregate(CLASSEETAT ~ EUCD + REPRESENTATIVE, data = TEMPECHIMME_TEMPO, max)
		CHIMETATME<-rbind(TEMPECHIMME_CAS1,TEMPECHIMME_CAS2[!(TEMPECHIMME_CAS2$EUCD %in% c(TEMPECHIMME_CAS1$EUCD)),])
		rm(TEMPECHIMME_CAS2)
	} else {
		CHIMETATME<-TEMPECHIMME_CAS1
	}
	
	names(CHIMETATME)[3]<-"ETATCHIMME"
		
###################################
# CALCUL ETAT CHIMIQUE - NIVEAU DE CONFIANCE
###################################
	CHIMETATME$NIVCONF<-as.numeric(NA)
	CHIMETATME$CAS<-as.character(NA)
	
	# Pr�paration des donn�es pour l'estimation des % et data dispo (BENZO+INDENO & DEHP)
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
	
	# Ajout du code station au r�sultat masse d'eau necessaire pour info sur le niveau de confiance	
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
	#rm(NBPARAGLOBAL,NBPARAGLOBALSTAT,TEMPOCHIMSTATGLOB,TEMPODISPBENZO,TEMPODISPDEHP,TEMPODISPPARA,TEMPORATIOBON)
	
################################
# TABLE FINALE - mise en forme
###############################
	
	RLT_CHIMETATME<-CHIMETATME[,c("EUCD","ETATCHIMME","NIVCONF","CAS","STATION")]
	RLT_CHIMETATME<-merge(RLT_CHIMETATME,RLT_CHIMSTATION,by="STATION")
	NAMES_RLTSTATION<-names(RLT_CHIMSTATION[,c(3:ncol(RLT_CHIMSTATION))])
	RLT_CHIMETATME<-RLT_CHIMETATME[,c("EUCD","ETATCHIMME","NIVCONF","CAS","STATION",NAMES_RLTSTATION)]
	RLT_CHIMETATME<-RLT_CHIMETATME[order(RLT_CHIMETATME$EUCD),]
	RLT_CHIMETATME<-merge(RLT_CHIMETATME,MASSEDEAU[,c("EUCD","NAME","MODIFIED","ARTIFICIAL","ECHELLEME")],by = "EUCD",all.x = TRUE)
	
	

	#PPL, 24/09/20145
	# Pour supprimer les doublons de ME les r�gles seraient les suivantes :  
	# �	S�lection de la ligne avec l��tat chimique le plus d�classant 
	# �	Si m�me �tat : s�lection de la ligne avec le meilleur niveau de confiance
	# �	Si m�me niveau de confiance : s�lection de la ligne avec le plus de param�tres d�classant
	# �	Si m�me nombre de param�tres d�classant : s�lection de la ligne avec moins d��tat param�tre inconnu 
	# �	Si idem : s�lection au hasard.
	
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











