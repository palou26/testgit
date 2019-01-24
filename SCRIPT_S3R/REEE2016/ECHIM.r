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

#Valeur par defaut du nombre minimal de pr�levements
if (!exists("NBANFREQ")){NBANFREQ<-4}
if (FREQOKCHIM != "oui") {NBANFREQ<-0}


### Extraction selon les param�tres prioritaires notamment pour les groupes afin de recalculer les valeurs brutes
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
## METHODE : choix de la m�thode pour la preparation des donn�es brutes
# ANNEERECENTE : extraction des donn�es de l'ann�e la + r�cente pour chaque param�tre de la station
# ANNEECHRONIQUE : extraction des donn�es d'une station pour l'ensemble des param�tres ayant la chronique la plus compl�te
# NOFILTREANNEE : on ne fait aucun filtre sur l'ann�e et on prend en compte tout le jeu de donn�es
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
gc()

# 5/12/2016 : on retire les LQABERANTE
DATACHIM<-merge(DATACHIM, PARAMETRECHIM[,c("PARAMETRE","LQSEUIL")], by="PARAMETRE", all.x = TRUE)
if ( SUPPR_LQ_ABERRANTE_CHIM == "oui" ) {
	if (LQ_NONDISPO =="oui") { # si LQ non dispo on prend les valeurs du champ RESULTAT lorsque non quantifi� --> car c'est la LQ
		PBLQCHIMABERRANTE<-DATACHIM[!is.na(DATACHIM$LQSEUIL) & DATACHIM$RESULTAT>DATACHIM$LQSEUIL & DATACHIM$QUANTIFIE=="0" & DATACHIM$LQSEUIL != -99 ,]
		DATACHIM<-DATACHIM[(!(!is.na(DATACHIM$LQSEUIL) & DATACHIM$RESULTAT>DATACHIM$LQSEUIL & DATACHIM$QUANTIFIE=="0"))| DATACHIM$LQSEUIL == -99 | is.na(DATACHIM$LQSEUIL) | DATACHIM$QUANTIFIE=="1",]
	} else {
		PBLQCHIMABERRANTE<-DATACHIM[!is.na(DATACHIM$LQSEUIL) & DATACHIM$LQ>DATACHIM$LQSEUIL & DATACHIM$QUANTIFIE=="0" & DATACHIM$LQSEUIL != -99  ,]
		DATACHIM<-DATACHIM[(!(!is.na(DATACHIM$LQSEUIL) & DATACHIM$LQ>DATACHIM$LQSEUIL & DATACHIM$QUANTIFIE=="0"))| DATACHIM$LQSEUIL == -99 | is.na(DATACHIM$LQSEUIL)  | DATACHIM$QUANTIFIE=="1",]
	}

	if( nrow(PBLQCHIMABERRANTE)>0) {
		CSV<-paste(CH_ERREUR,"CHIM_LQ_ABERRANTE_",g,"_",SEEE_DEBformat,".csv",sep="")
		write.csv2(PBLQCHIMABERRANTE,CSV)
		MSGBOX <- tkmessageBox(title = "Info", message = paste("Les donn�es brutes avec LQ aberrantes (strictement > LQSEUIL) ont �t� retir�es \n du calcul de l'�tat chimique et export�es dans \n",CSV,sep=""), icon = "info", type = "ok")
	}

	if (nrow(DATACHIM) == 0) {
			MSGBOX <- tkmessageBox(title = "Info", message = paste("Pas de donn�es � traiter apr�s le retrait des mesures avec LQ aberrantes \n",CSV,sep=""), icon = "info", type = "ok")
			tcl("update")
			source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
	}	
}



# Calcul de la frequence de pr�l�vement
DATACHIM<-DATACHIM[!duplicated(DATACHIM[,c("STATION", "PARAGROUP", "PARAMETRE","DATEPRELEV", "HEUREPRELEV")]),]
DATACHIM_FREQ<-aggregate(RESULTAT ~ STATION + PARAGROUP + ANNEE, data = DATACHIM , length) # les donn�es sont d�j� filtr�es sur l'ann�e, on le prend dans aggregate uniquement pour l'avoir ensuite dans IDAN
names(DATACHIM_FREQ)[4]<-"FREQ"
DATACHIM_FREQ<-DATACHIM_FREQ[!duplicated(DATACHIM_FREQ[,c("STATION","PARAGROUP","ANNEE")]),]
DATACHIM_FREQ<-DATACHIM_FREQ[,c("STATION", "PARAGROUP","FREQ")]

DATACHIM<-merge(DATACHIM,DATACHIM_FREQ,by=c("STATION", "PARAGROUP"))
rm(DATACHIM_FREQ)
DATACHIMFREQ<-DATACHIM[,c("STATION","PARAGROUP")] #save de DATACHIM avant retrait des freq non respect�es pour les afficher dans les stat sur l'ensemble des param�tres
DATACHIMFREQQUANTI<-DATACHIM[DATACHIM$QUANTIFIE == 1,c("STATION","PARAGROUP", "QUANTIFIE")]

gc()


# ## Respect de la fr�quence minimale de pr�l�vement par param�tre
# if (FREQOKCHIM == "oui") {
	# TEMPCHIM_RETRAIT<-DATACHIM[DATACHIM$FREQ<4, c("STATION", "PARAGROUP", "PARAMETRE", "ANNEE", "DATEPRELEV", "HEUREPRELEV","FREQ")]
	# DATACHIM<-DATACHIM[c(!(paste0(DATACHIM$STATION, DATACHIM$PARAGROUP) %in% paste0(TEMPCHIM_RETRAIT$STATION, TEMPCHIM_RETRAIT$PARAGROUP))),] # retrait du parametre et/ou groupe de parametres si un parametre ne respecte pas la frequence
	# #Export des stations qui ne sont plus dans le jeux de donn�es car la fr�quence est trop faible
	# TEMPCHIM_RETRAIT<-TEMPCHIM_RETRAIT[!duplicated(TEMPCHIM_RETRAIT),]
	# if( nrow(TEMPCHIM_RETRAIT>0)) {
		# CSV<-paste(CH_ERREUR,"CHIM_FREQ_TROP_BASSE_",SEEE_DEBformat,".csv",sep="")
		# write.csv2(TEMPCHIM_RETRAIT,CSV)
		# MSGBOX <- tkmessageBox(title = "Info", message = paste("Les donn�es avec fr�quences trop basses ont �t� export�es dans \n",CSV,sep=""), icon = "info", type = "ok")

	# }
# }
# gc()

DATACHIM<-merge(DATACHIM,STATION[,c("STATION","FONDGEO_CADMIUM","FONDGEO_PLOMB","FONDGEO_MERCURE","FONDGEO_NICKEL","DURETE")],by="STATION", all.x=TRUE)
gc()

if(CONTA != "oui"){
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
}

## Nouvelle matrice des NQEMA et MQECMA apr�s modif classe de duret� et fond g�ochim
CHIM_NQE<-DATACHIM[!duplicated(DATACHIM[,c("STATION","PARAGROUP")]),c("STATION","PARAGROUP","NQEMA","NQECMA","GROUPE")]
CHIM_NQE$IDNQE<-paste0(CHIM_NQE$STATION,"_",CHIM_NQE$PARAGROUP)

# Extraction de la LQMAX par station pour comparaison � la CMA & MA
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
	MSGBOX <- tkmessageBox(title = "Info", message = paste("Pas de donn�es � traiter si respect de la fr�quence de pr�l�vement\n Veuillez abaisser la fr�quence ou d�cocher le respect \n",CSV,sep=""), icon = "info", type = "ok")
	}

#identification des param�tres groupe quantifi� � minima 1 fois dans l'ann�e
CHIMCMA_QUANTIFAN<-aggregate(QUANTIFIE ~ STATION + PARAGROUP, data = DATACHIM[DATACHIM$FREQ >= NBANFREQ,] , max)
names(CHIMCMA_QUANTIFAN)[3]<-"QUANTIFEAN"
CHIMCMA_QUANTIFAN$IDNQE<-paste(CHIMCMA_QUANTIFAN$STATION, CHIMCMA_QUANTIFAN$PARAGROUP,sep="_")
CHIMCMA_QUANTIFAN$ID<-paste(CHIMCMA_QUANTIFAN$STATION, CHIMCMA_QUANTIFAN$PARAGROUP, CHIMCMA_QUANTIFAN$QUANTIFEAN,sep="_")

CHIMCMA_MESUREMAX<-aggregate(RESULTAT ~ STATION + PARAGROUP + QUANTIFIE, data = DATACHIM[DATACHIM$FREQ >= NBANFREQ,] , max)
names(CHIMCMA_MESUREMAX)[4]<-"RESULTATMAX"
CHIMCMA_MESUREMAX$RESULTATMAX<-round(CHIMCMA_MESUREMAX$RESULTATMAX,8)
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

# modif 24/04/15 : retrait prelevement pour calcul moyenne si LQ > NQE (mail Ministere du 08/04/15 � destination des agences pour cycle 2
if ( LQSUPNQEMACHIM == "oui" ) {
	if (LQ_NONDISPO =="oui") { # si LQ non dispo on prend les valeurs du champ RESULTAT lorsque non quantifi� --> car c'est la LQ
		PBLQCHIM<-DATACHIM[DATACHIM$RESULTAT>DATACHIM$NQEMA & DATACHIM$QUANTIFIE=="0" & DATACHIM$NQEMA > 0 ,]
		DATACHIM<-DATACHIM[!(DATACHIM$RESULTAT>DATACHIM$NQEMA & DATACHIM$QUANTIFIE=="0") | DATACHIM$QUANTIFIE=="1"  | DATACHIM$NQEMA < 0 ,]
	} else {
		PBLQCHIM<-DATACHIM[DATACHIM$LQ > DATACHIM$NQEMA & DATACHIM$QUANTIFIE=="0" & DATACHIM$NQEMA > 0 ,]
		DATACHIM<-DATACHIM[(DATACHIM$LQ <= DATACHIM$NQEMA & DATACHIM$QUANTIFIE=="0" )| DATACHIM$QUANTIFIE=="1"  | DATACHIM$NQEMA < 0  ,]
	}

	if( nrow(PBLQCHIM)>0) {
		CSV<-paste(CH_ERREUR,"CHIM_LQ_SUP_NQEMA_",g,"_",SEEE_DEBformat,".csv",sep="")
		write.csv2(PBLQCHIM,CSV)
		MSGBOX <- tkmessageBox(title = "Info", message = paste("Les donn�es brutes non quantifi�es avec LQ > NQEMA ont �t� retir�es \n du calcul de l'�tat chimique et export�es dans \n",CSV,sep=""), icon = "info", type = "ok")

	}	
	}



# Modification suite � la publiation de l'arret� et prestation C43 AELB 17 dec 
# Si le resultat est inf�rieur � la LQ on remplace par LQ/2
DATACHIM<-merge(DATACHIM,SUBSTGROUPEE[,c("PARAGROUP","INDIVIDUELLE")], by = "PARAGROUP",all.x = TRUE)
DATACHIM$RESULTAT2<-DATACHIM$RESULTAT
#Si  on est pas quantifi� 
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


# Extraction de la LQMAX par station pour comparaison � la MA
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


# On indique pour chaque pr�l�vement s'il s'agit de substance group�e ou non
CHIMMA<-merge(CHIMMA,SUBSTGROUPEE[,c("PARAGROUP","INDIVIDUELLE")], by = "PARAGROUP",all.x = TRUE)



# Calcul de la frequence de pr�l�vement
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


#Si on a supprim� des enregistrements � cause de retrait prelevement pour calcul moyenne si LQ > NQE 

## CAS 0 - Respect de la fr�quence minimale de pr�l�vement
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
		#cas o� si on avait conserv� tous les pr�levements on aurait atteint la frequence de prelevement
		CHIMMA$CASCHIMMA[CHIMMA$FREQ>0 & CHIMMA$FREQ<NBANFREQ & CHIMMA$FREQ + CHIMMA$FREQPBLQ  >= NBANFREQ ]<-"0*"
		# cas o� la frequence apr�s retrait est � zero et on aurait quand m�me pas attend la frequence de pr�levement si on avait conserv� tous les pr�levements
		CHIMMA$CASCHIMMA[CHIMMA$FREQ== 0 & CHIMMA$FREQPBLQ  > 0 ]<-"0**"
		# cas o� la frequence apr�s retrait est � zero et on aurait  atteind la frequence de pr�levement si on avait conserv� tous les pr�levements		
		CHIMMA$CASCHIMMA[CHIMMA$FREQ== 0 & CHIMMA$FREQPBLQ  >= NBANFREQ ]<-"0***"
		CHIMMA$CLASSEMA[CHIMMA$CASCHIMMA %in% c("0","0*","0**","0***")]<-0
		print(table(CHIMMA$CASCHIMMA))		
		}}
}

## CAS 0 - Respect de la fr�quence minimale de pr�l�vement
if (FREQOKCHIM == "oui") {
	CHIMMA$CASCHIMMA[CHIMMA$FREQ<NBANFREQ & CHIMMA$CASCHIMMA == ""]<-"0"
	CHIMMA$CLASSEMA[CHIMMA$FREQ<NBANFREQ  & CHIMMA$CASCHIMMA == ""]<-0
	
	if ( SUPPR_LQ_ABERRANTE_CHIM == "oui" ) {
	if (nrow(PBLQCHIMABERRANTE) > 0) {
		PBLQCHIMABERRANTEFREQ<-data.frame(table(PBLQCHIMABERRANTE$STATION ,PBLQCHIMABERRANTE$PARAGROUP))
		names(PBLQCHIMABERRANTEFREQ)<-c("STATION","PARAGROUP","FREQPBLQ2")
		PBLQCHIMABERRANTEFREQ<-PBLQCHIMABERRANTEFREQ[PBLQCHIMABERRANTEFREQ$FREQPBLQ2 > 0,]
		PBLQCHIMABERRANTEFREQ$ID<-paste(PBLQCHIMABERRANTEFREQ$STATION ,PBLQCHIMABERRANTEFREQ$PARAGROUP,sep="_")
		CHIMMA$ID<-paste(CHIMMA$STATION ,CHIMMA$PARAGROUP,sep="_")
		CHIMMA<-merge(CHIMMA,PBLQCHIMABERRANTEFREQ,all=TRUE)
		CHIMMA$FREQ[is.na(CHIMMA$FREQ)]<-0
		CHIMMA$FREQPBLQ2[is.na(CHIMMA$FREQPBLQ2)]<-0
		#cas o� si on avait conserv� tous les pr�levements on aurait atteint la frequence de prelevement
		CHIMMA$CASCHIMMA[CHIMMA$FREQ>0 & CHIMMA$FREQ<NBANFREQ & CHIMMA$FREQ + CHIMMA$FREQPBLQ2  >= NBANFREQ ]<-"0+"
		# cas o� la frequence apr�s retrait est � zero et on aurait quand m�me pas attend la frequence de pr�levement si on avait conserv� tous les pr�levements
		CHIMMA$CASCHIMMA[CHIMMA$FREQ== 0 & CHIMMA$FREQPBLQ2  > 0 ]<-"0++"
		# cas o� la frequence apr�s retrait est � zero et on aurait  atteind la frequence de pr�levement si on avait conserv� tous les pr�levements		
		CHIMMA$CASCHIMMA[CHIMMA$FREQ== 0 & CHIMMA$FREQPBLQ2  >= NBANFREQ ]<-"0+++"
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
condCAS3<-!( CHIMMA$CASCHIMMA  %in% c("0","0+","0++","0+++","0*","0**","0***","1A","1B","2A","2B")) & (is.na(CHIMMA$LQMAX) | CHIMMA$LQMAX <= CHIMMA$NQEMA) & CHIMMA$INDIVIDUELLE == "non"
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
CHIMMA$CLASSEMA[CHIMMA$NQEMA < 0]<- -99
gc()

##Cr�ation de l'onglet CAS_CHIMMA
PARAGROUPCHIM<-PARAGROUPCHIM[order(PARAGROUPCHIM$IDTRI),]
CAS_CHIMMA<-data.frame(STATION = sort(unique(CHIMMA$STATION)))
for (i in  1:nrow(PARAGROUPCHIM)  ) {
	TEMPP<-CHIMMA[CHIMMA$PARAGROUP==PARAGROUPCHIM$PARAGROUP[i],c("STATION", "CASCHIMMA")]
	names(TEMPP)[2]<-toupper(PARAGROUPCHIM$PARAGROUPLIBCOURT[i])
	CAS_CHIMMA<-merge(CAS_CHIMMA,TEMPP,by="STATION", all.x=TRUE)
	rm(TEMPP)
}


##############################
## Calcul de l'�tat chimique
##############################
# Etat par PARAGROUP
CHIMCMA$ID<-paste(CHIMCMA$STATION, CHIMCMA$PARAGROUP, sep="_")
CHIMMA$ID<-paste(CHIMMA$STATION, CHIMMA$PARAGROUP, sep="_")
CHIMCMAMA<-merge(CHIMCMA[,c("QUANTIFEAN","RESULTATMAX", "NQECMA","CLASSECMA", "ID")],CHIMMA[,c("ID", "STATION","PARAGROUP", "MOY",  "LQMAX", "NQEMA", "CLASSEMA","CASCHIMMA","GROUPE")], by="ID",all=TRUE)
CHIMCMAMA$CLASSEETAT<-as.numeric(NA)
CHIMCMAMA$CLASSECMA[is.na(CHIMCMAMA$CLASSECMA)]<-0
CHIMCMAMA$CLASSEMA[is.na(CHIMCMAMA$CLASSEMA)]<-0
CHIMCMAMA$CLASSEETAT[CHIMCMAMA$CLASSECMA==2 | CHIMCMAMA$CLASSEMA==2]<-2
CHIMCMAMA$CLASSEETAT[CHIMCMAMA$CLASSECMA==1 & CHIMCMAMA$CLASSEMA %in% c(-99) ]<-1
CHIMCMAMA$CLASSEETAT[CHIMCMAMA$CLASSECMA==1 & CHIMCMAMA$CLASSEMA %in% c(0)]<-0
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

# Ajout colonnes des param�tres 
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





