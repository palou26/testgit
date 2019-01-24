##############################################
## SEEE - COURS D'EAU : Somme de Pesticide est comparaison � 0.5 microGramme/L

##############################################


##############################
## Mise en forme des donn�es
##############################
gc()
#SAVE()

##########################################
## Calcul du module de contamination
##########################################
# requete pr�paratoire
CONTAM<-DATACONTA[DATACONTA$STATION %in% STATION$STATION,]
CONTAM$ID<-paste(CONTAM$STATION, CONTAM$PARAMETRE, sep="_")
gc()
CONTAM<-merge(CONTAM,LISTECODERQE[,c("REMARQUE","QUANTIFIE")],by="REMARQUE")
gc()




# 27/03/2018 :  traitement des LQ � z�ro
# cond 1 : Si LQ = 0 et CodeRemarque = (10,2ou7) et R�sultat >= 0 alors LQ = R�sultat puis R�sultat = LQ/2.
# Si LQ= 0 et CodeRemarque = 1 et R�sultat > 0, alors ne rien changer
# cond3 : Si LQ= 0 et CodeRemarque = 1 et R�sultat = 0, alors retirer l'analyse avec un message d'alerte
cond1<-CONTAM$LQ == 0 & CONTAM$RESULTAT >= 0 & CONTAM$REMARQUE %in% c(2,7,10)
CONTAM$LQ[cond1]<-CONTAM$RESULTAT[cond1]

cond3<-CONTAM$LQ == 0 & CONTAM$RESULTAT == 0 & CONTAM$REMARQUE == 1

	if( nrow(CONTAM[cond3,])>0) {
		CSV<-paste(CH_ERREUR,"CONTA_LQ_ZERO_QUANTIFIEE_",SEEE_DEBformat,".csv",sep="")
		write.csv2(CONTAM[cond3,],CSV)
		CONTAM<-CONTAM[!cond3,]
		MSGBOX <- tkmessageBox(title = "Info", message = paste("Les", nrow(CONTAM[cond3,])  ," analyses avec 'LQ= 0 et CodeRemarque = 1 et R�sultat = 0' \n ont �t� retir�es  du calcul de l'�tat chimique et export�es dans \n",CSV,sep=""), icon = "info", type = "ok")
	}

	if (nrow(CONTAM) == 0) {
			MSGBOX <- tkmessageBox(title = "Info", message = paste("Pas de donn�es � traiter apr�s le retrait des mesures avec LQ aberrantes \n",CSV,sep=""), icon = "info", type = "ok")
			tcl("update")
			source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
	}
gc()



# On met � z�ro  si Resultat <= LQ ou si pas qualtifi� pour contamination ChroniQ ou Impr�gantion moyenne
CONTAM$RESULTAT2<-CONTAM$RESULTAT
#Si  on est pas quantifi� 

	if (LQ_NONDISPO =="oui") {
		cond<- CONTAM$QUANTIFIE=="0" 
		CONTAM$RESULTAT2[cond]<-0
	} else {
		cond<- CONTAM$QUANTIFIE=="0" | CONTAM$RESULTAT <= CONTAM$LQ
		CONTAM$RESULTAT2[cond]<-0
	}


gc()







###Concentration moyenne par parametre
CONTAAGG<-aggregate(RESULTAT2 ~ STATION + PARAMETRE, data = CONTAM , mean)

names(CONTAAGG)[3]<-"CONCENTRATION_MOY"
CONTAAGG<-merge(CONTAAGG,STATION[, c("STATION","LIBELLE","EUCD","TYPESTAT","ECHELLESTA")] , 
				by = "STATION",all.x = TRUE)	
				
CONTAAGG<-merge(CONTAAGG,PARAMETRECONTA[, c("PARAMETRE","PARAMETRELIB","NOMCOURT", "FAMILLE","CAS", "PESTICIDE", "ETATPS", "ETATCHIM","USAGE_PRINC")] , 
				by = "PARAMETRE",all.x = TRUE)

CONTAAGG<-CONTAAGG[order(CONTAAGG$STATION,CONTAAGG$PARAMETRE ),
					c("STATION","LIBELLE","EUCD","TYPESTAT","ECHELLESTA","PARAMETRE","PARAMETRELIB","NOMCOURT","CONCENTRATION_MOY",
					"PESTICIDE","FAMILLE","CAS","ETATPS", "ETATCHIM","USAGE_PRINC") ]

head(CONTAAGG)


##Frequence
CONTAFREQ<-aggregate(RESULTAT2 ~ STATION + PARAMETRE, data = CONTAM , length)
names(CONTAFREQ)[3]<-"FreqPrelev"
CONTAAGG<-merge(CONTAAGG,CONTAFREQ, by= c("STATION","PARAMETRE"),all.x = TRUE)

CONTAAGG<-CONTAAGG[order(CONTAAGG$STATION,CONTAAGG$PARAMETRE ),
					c("STATION","LIBELLE","EUCD","TYPESTAT","ECHELLESTA","PARAMETRE","PARAMETRELIB","NOMCOURT","CONCENTRATION_MOY","FreqPrelev",
					"PESTICIDE","FAMILLE","CAS","ETATPS", "ETATCHIM","USAGE_PRINC") ]


					
###Somme des pesticides
if (nrow(CONTAAGG[CONTAAGG$PESTICIDE== "oui",]) > 0) {				
	SOMPEST<-aggregate(CONCENTRATION_MOY ~ STATION , data = CONTAAGG[CONTAAGG$PESTICIDE== "oui",] , sum)
	names(SOMPEST)[2]<-"SOMM_PEST"
	SOMPEST$SUP_SEUIL<-"non"
	SOMPEST$SUP_SEUIL[SOMPEST$SOMM_PEST > 0.5]<-"oui"	


		#nombre de PESTICIDE quantifi�e
	if (nrow(CONTAAGG[CONTAAGG$PESTICIDE== "oui" & CONTAAGG$CONCENTRATION_MOY > 0 ,]) > 0) {		
	NBPESTQUANTI<-aggregate(CONCENTRATION_MOY ~ STATION , data = CONTAAGG[CONTAAGG$PESTICIDE== "oui" & CONTAAGG$CONCENTRATION_MOY > 0,] , length)
	names(NBPESTQUANTI)[2]<-"NB_PEST_QUANTI"	
	SOMPEST<-merge(SOMPEST,NBPESTQUANTI, by = "STATION",all.x=TRUE)
	SOMPEST$NB_PEST_QUANTI[is.na(SOMPEST$NB_PEST_QUANTI)]<-0
	}
} else {

SOMPEST<-data.frame(STATION=unique(CONTAAGG$STATION),SOMM_PEST = rep("Pas de pesticides", length(unique(CONTAAGG$STATION))   ) )

}

SOMPEST<-merge(SOMPEST,STATION[, c("STATION","LIBELLE","EUCD","TYPESTAT","ECHELLESTA")] , 
				by = "STATION",all.x = TRUE)

SOMPEST<-SOMPEST[,c("STATION","LIBELLE","SOMM_PEST","SUP_SEUIL","NB_PEST_QUANTI","EUCD","TYPESTAT","ECHELLESTA")]

				
				

