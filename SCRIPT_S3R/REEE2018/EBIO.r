##############################################
## SEEE - COURS D'EAU : �tat biologique
## Application de l'arr�t� de janvier 2010
## cr�� janvier 2013
######
##############################################

#SAVE()

gc() ## compacte R
########################################################
## Calcul de la moyenne de chaque indicateur de qualit�
## modifi� le 09/12/14
########################################################

###TRANSFORMATION INDICE => EQR
DATABIO$EQRBIO<-DATABIO$RESULTAT
DATABIO<-merge(DATABIO,STATION[,c("STATION", "TYPEFR","ALTITUDE","ME_BV_SUP10000KM2")],by="STATION")
NDATABIO<-names(DATABIO)

# IBD calcul EQR
DATABIO<-merge(DATABIO,GRILLEIBD,by="TYPEFR", all.x=TRUE)
condIBD<-DATABIO$PARAGROUP=="5856" & DATABIO$INDICEEQR == "INDICE"
DATABIO$EQRBIO[condIBD]<-(DATABIO$RESULTAT[condIBD]-DATABIO$VALMIN[condIBD])/(DATABIO$VALREF[condIBD]-DATABIO$VALMIN[condIBD])
DATABIO<-DATABIO[,NDATABIO]

# IBG & IBGA calcul EQR
DATABIO<-merge(DATABIO,GRILLEIBG,by="TYPEFR", all.x=TRUE)
condIBG<-DATABIO$PARAGROUP=="5910" & DATABIO$INDICEEQR == "INDICE"
condIBGA<-DATABIO$PARAGROUP=="6951" & DATABIO$INDICEEQR == "INDICE"
condIBMA<-DATABIO$PARAGROUP=="IBMA" & DATABIO$INDICEEQR == "INDICE"

DATABIO$EQRBIO[condIBG]<-(DATABIO$RESULTAT[condIBG]-1)/(DATABIO$VALREF[condIBG]-1)
DATABIO$EQRBIO[condIBGA]<-(DATABIO$RESULTAT[condIBGA]-1)/(DATABIO$VALREF[condIBGA]-1)
DATABIO$EQRBIO[condIBMA]<-DATABIO$RESULTAT[condIBMA]

DATABIO<-DATABIO[,NDATABIO]

# IBMR calcul EQR
DATABIO<-merge(DATABIO,GRILLEIBMR,by="TYPEFR", all.x=TRUE)
condIBMR<-DATABIO$PARAGROUP=="2928" & DATABIO$INDICEEQR == "INDICE"
DATABIO$EQRBIO[condIBMR]<-DATABIO$RESULTAT[condIBMR]/DATABIO$VALREF[condIBMR]
DATABIO<-DATABIO[,NDATABIO]

# I2M2 calcul EQR
condI2M2<-DATABIO$PARAGROUP=="7613"  
DATABIO$EQRBIO[condI2M2]<-DATABIO$RESULTAT[condI2M2]


##########################
##calcul de la moyenne
BIO_MOY_RESULTAT<-aggregate(RESULTAT ~ STATION + PARAGROUP , data = DATABIO , mean)
BIO_MOY_RESULTAT$RESULTAT<-round(BIO_MOY_RESULTAT$RESULTAT,2)
BIO_MOY_RESULTAT<-BIO_MOY_RESULTAT[order(BIO_MOY_RESULTAT$STATION, BIO_MOY_RESULTAT$PARAGROUP),]

BIO_MOY_EQR<-aggregate(EQRBIO ~ STATION + PARAGROUP, data = DATABIO , mean)
BIO_MOY_EQR$EQRBIO<-round(BIO_MOY_EQR$EQRBIO,5)
BIO_MOY_EQR<-BIO_MOY_EQR[order(BIO_MOY_EQR$STATION, BIO_MOY_EQR$PARAGROUP),]

BIO_MOY<-merge(BIO_MOY_RESULTAT,BIO_MOY_EQR, by = c("STATION","PARAGROUP"), all = TRUE)


## Calcul des classes
BIO_MOY$CLASSEBIO<-as.numeric("")
BIO_MOY<-merge(BIO_MOY,STATION[,c("STATION", "TYPEFR","ALTITUDE","ME_BV_SUP10000KM2")],by="STATION")
BIO_MOY_NAMES<-names(BIO_MOY)

##### IBD
	# BV inf�rieur � 10 000km2 (la majorit� des stations en g�n�ral)
BIO_MOY<-BIO_MOY[,c(BIO_MOY_NAMES)]
BIO_MOY<-merge(BIO_MOY,GRILLEIBD,by="TYPEFR", all.x=TRUE)
condIBD<-BIO_MOY$PARAGROUP=="5856" & BIO_MOY$ME_BV_SUP10000KM2 == "non"

BIO_MOY$CLASSEBIO[condIBD & BIO_MOY$EQRBIO >=BIO_MOY$INFB]<-1
BIO_MOY$CLASSEBIO[condIBD & BIO_MOY$EQRBIO >=BIO_MOY$INFV & BIO_MOY$EQRBIO <BIO_MOY$INFB]<-2
BIO_MOY$CLASSEBIO[condIBD & BIO_MOY$EQRBIO >=BIO_MOY$INFJ & BIO_MOY$EQRBIO <BIO_MOY$INFV]<-3
BIO_MOY$CLASSEBIO[condIBD & BIO_MOY$EQRBIO >=BIO_MOY$INFO & BIO_MOY$EQRBIO <BIO_MOY$INFJ]<-4
BIO_MOY$CLASSEBIO[condIBD & BIO_MOY$EQRBIO >=BIO_MOY$INFR & BIO_MOY$EQRBIO <BIO_MOY$INFO]<-5

	# BV sup�rieur � 10 000km2 
BIO_MOY<-BIO_MOY[,c(BIO_MOY_NAMES)]
BIO_MOY<-merge(BIO_MOY,GRILLEIBD_GRANDBV,by="TYPEFR", all.x=TRUE)
condIBD<-BIO_MOY$PARAGROUP=="5856" & BIO_MOY$ME_BV_SUP10000KM2 == "oui"

BIO_MOY$CLASSEBIO[condIBD & BIO_MOY$EQRBIO >=BIO_MOY$INFB]<-1
BIO_MOY$CLASSEBIO[condIBD & BIO_MOY$EQRBIO >=BIO_MOY$INFV & BIO_MOY$EQRBIO <BIO_MOY$INFB]<-2
BIO_MOY$CLASSEBIO[condIBD & BIO_MOY$EQRBIO >=BIO_MOY$INFJ & BIO_MOY$EQRBIO <BIO_MOY$INFV]<-3
BIO_MOY$CLASSEBIO[condIBD & BIO_MOY$EQRBIO >=BIO_MOY$INFO & BIO_MOY$EQRBIO <BIO_MOY$INFJ]<-4
BIO_MOY$CLASSEBIO[condIBD & BIO_MOY$EQRBIO >=BIO_MOY$INFR & BIO_MOY$EQRBIO <BIO_MOY$INFO]<-5


###### IBG & IBGA
BIO_MOY<-BIO_MOY[,c(BIO_MOY_NAMES)]
# calcul EQR
BIO_MOY<-merge(BIO_MOY,GRILLEIBG,by="TYPEFR", all.x=TRUE)
condIBG<-BIO_MOY$PARAGROUP=="5910"
condIBGA<-BIO_MOY$PARAGROUP=="6951"
condIBMA<-BIO_MOY$PARAGROUP=="IBMA"

BIO_MOY$CLASSEBIO[condIBG & BIO_MOY$EQRBIO >=BIO_MOY$INFB]<-1
BIO_MOY$CLASSEBIO[condIBG & BIO_MOY$EQRBIO >=BIO_MOY$INFV & BIO_MOY$EQRBIO <BIO_MOY$INFB]<-2
BIO_MOY$CLASSEBIO[condIBG & BIO_MOY$EQRBIO >=BIO_MOY$INFJ & BIO_MOY$EQRBIO <BIO_MOY$INFV]<-3
BIO_MOY$CLASSEBIO[condIBG & BIO_MOY$EQRBIO >=BIO_MOY$INFO & BIO_MOY$EQRBIO <BIO_MOY$INFJ]<-4
BIO_MOY$CLASSEBIO[condIBG & BIO_MOY$EQRBIO >=BIO_MOY$INFR & BIO_MOY$EQRBIO <BIO_MOY$INFO]<-5

BIO_MOY$CLASSEBIO[condIBGA & BIO_MOY$EQRBIO >=BIO_MOY$INFB]<-1
BIO_MOY$CLASSEBIO[condIBGA & BIO_MOY$EQRBIO >=BIO_MOY$INFV & BIO_MOY$EQRBIO <BIO_MOY$INFB]<-2
BIO_MOY$CLASSEBIO[condIBGA & BIO_MOY$EQRBIO >=BIO_MOY$INFJ & BIO_MOY$EQRBIO <BIO_MOY$INFV]<-3
BIO_MOY$CLASSEBIO[condIBGA & BIO_MOY$EQRBIO >=BIO_MOY$INFO & BIO_MOY$EQRBIO <BIO_MOY$INFJ]<-4
BIO_MOY$CLASSEBIO[condIBGA & BIO_MOY$EQRBIO >=BIO_MOY$INFR & BIO_MOY$EQRBIO <BIO_MOY$INFO]<-5

BIO_MOY$CLASSEBIO[condIBMA & BIO_MOY$EQRBIO >=BIO_MOY$INFB]<-1
BIO_MOY$CLASSEBIO[condIBMA & BIO_MOY$EQRBIO >=BIO_MOY$INFV & BIO_MOY$EQRBIO <BIO_MOY$INFB]<-2
BIO_MOY$CLASSEBIO[condIBMA & BIO_MOY$EQRBIO >=BIO_MOY$INFJ & BIO_MOY$EQRBIO <BIO_MOY$INFV]<-3
BIO_MOY$CLASSEBIO[condIBMA & BIO_MOY$EQRBIO >=BIO_MOY$INFO & BIO_MOY$EQRBIO <BIO_MOY$INFJ]<-4
BIO_MOY$CLASSEBIO[condIBMA & BIO_MOY$EQRBIO >=BIO_MOY$INFR & BIO_MOY$EQRBIO <BIO_MOY$INFO]<-5


###### I2M2
BIO_MOY<-BIO_MOY[,c(BIO_MOY_NAMES)]
# calcul EQR
BIO_MOY<-merge(BIO_MOY,GRILLEI2M2,by="TYPEFR", all.x=TRUE)
condI2M2<-BIO_MOY$PARAGROUP=="7613"

BIO_MOY$CLASSEBIO[condI2M2 & BIO_MOY$EQRBIO >=BIO_MOY$INFB]<-1
BIO_MOY$CLASSEBIO[condI2M2 & BIO_MOY$EQRBIO >=BIO_MOY$INFV & BIO_MOY$EQRBIO <BIO_MOY$INFB]<-2
BIO_MOY$CLASSEBIO[condI2M2 & BIO_MOY$EQRBIO >=BIO_MOY$INFJ & BIO_MOY$EQRBIO <BIO_MOY$INFV]<-3
BIO_MOY$CLASSEBIO[condI2M2 & BIO_MOY$EQRBIO >=BIO_MOY$INFO & BIO_MOY$EQRBIO <BIO_MOY$INFJ]<-4
BIO_MOY$CLASSEBIO[condI2M2 & BIO_MOY$EQRBIO >=BIO_MOY$INFR & BIO_MOY$EQRBIO <BIO_MOY$INFO]<-5

########## IPR
BIO_MOY<-BIO_MOY[,c(BIO_MOY_NAMES)]
BIO_MOY<-merge(BIO_MOY,GRILLEIPR,by="TYPEFR", all.x=TRUE)

# MAJ borne inferieure Verte si alti >=500m. Si alti non renseign�e, INFV ne change pas.
CONDALTI<-BIO_MOY$ALTITUDE>=500 & !(is.na(BIO_MOY$ALTITUDE))
BIO_MOY$INFV[CONDALTI]<-BIO_MOY$VALALTIV[CONDALTI]

#Calcul classe de qualit� selon note moyenne IPR
condIPR<-BIO_MOY$PARAGROUP=="7036"
BIO_MOY$CLASSEBIO[condIPR & BIO_MOY$RESULTAT >=0 & BIO_MOY$RESULTAT <=BIO_MOY$INFB]<-1
BIO_MOY$CLASSEBIO[condIPR & BIO_MOY$RESULTAT >BIO_MOY$INFB & BIO_MOY$RESULTAT <=BIO_MOY$INFV]<-2
BIO_MOY$CLASSEBIO[condIPR & BIO_MOY$RESULTAT >BIO_MOY$INFV & BIO_MOY$RESULTAT <=BIO_MOY$INFJ]<-3
BIO_MOY$CLASSEBIO[condIPR & BIO_MOY$RESULTAT >BIO_MOY$INFJ & BIO_MOY$RESULTAT <=BIO_MOY$INFO]<-4
BIO_MOY$CLASSEBIO[condIPR & BIO_MOY$RESULTAT >BIO_MOY$INFO ]<-5  # borne INFR sert uniquement pour le calcul de l'indice (limite max de l'indice 0)
BIO_MOY<-BIO_MOY[,c(BIO_MOY_NAMES)]

########## IPR PLUS  (IPR+)
BIO_MOY<-BIO_MOY[,c(BIO_MOY_NAMES)]
BIO_MOY<-merge(BIO_MOY,GRILLEIPRPLUS,by="TYPEFR", all.x=TRUE)


condIPRp<-BIO_MOY$PARAGROUP=="7614"
	# IPR+ forc�ment en EQR
BIO_MOY$EQRBIO[condIPRp]<-BIO_MOY$RESULTAT[condIPRp] # r�sultat d�j� en EQR via code SANDRE

#Calcul classe de qualit� selon note moyenne IPR
BIO_MOY$CLASSEBIO[condIPRp & BIO_MOY$RESULTAT >=0 & BIO_MOY$RESULTAT <=BIO_MOY$INFB]<-1
BIO_MOY$CLASSEBIO[condIPRp & BIO_MOY$RESULTAT >BIO_MOY$INFB & BIO_MOY$RESULTAT <=BIO_MOY$INFV]<-2
BIO_MOY$CLASSEBIO[condIPRp & BIO_MOY$RESULTAT >BIO_MOY$INFV & BIO_MOY$RESULTAT <=BIO_MOY$INFJ]<-3
BIO_MOY$CLASSEBIO[condIPRp & BIO_MOY$RESULTAT >BIO_MOY$INFJ & BIO_MOY$RESULTAT <=BIO_MOY$INFO]<-4
BIO_MOY$CLASSEBIO[condIPRp & BIO_MOY$RESULTAT >BIO_MOY$INFO ]<-5  
BIO_MOY<-BIO_MOY[,c(BIO_MOY_NAMES)]


########## IBMR
# calcul EQR
BIO_MOY<-merge(BIO_MOY,GRILLEIBMR,by="TYPEFR", all.x=TRUE)
condIBMR<-BIO_MOY$PARAGROUP=="2928"

#Calcul classe de qualit� selon EQRBIO : IBMR
BIO_MOY$CLASSEBIO[condIBMR & BIO_MOY$EQRBIO >=BIO_MOY$INFB]<-1
BIO_MOY$CLASSEBIO[condIBMR & BIO_MOY$EQRBIO >=BIO_MOY$INFV & BIO_MOY$EQRBIO <BIO_MOY$INFB]<-2
BIO_MOY$CLASSEBIO[condIBMR & BIO_MOY$EQRBIO >=BIO_MOY$INFJ & BIO_MOY$EQRBIO <BIO_MOY$INFV]<-3
BIO_MOY$CLASSEBIO[condIBMR & BIO_MOY$EQRBIO >=BIO_MOY$INFO & BIO_MOY$EQRBIO <BIO_MOY$INFJ]<-4
BIO_MOY$CLASSEBIO[condIBMR & BIO_MOY$EQRBIO >=BIO_MOY$INFR & BIO_MOY$EQRBIO <BIO_MOY$INFO]<-5
BIO_MOY<-BIO_MOY[,c(BIO_MOY_NAMES)]
flush.console()
gc()


#PARAMBIOTYPEFR (on selectionne que les TYPE-FR qui sont �tudi�es
PARAMBIOTYPEFR<-PARAMBIOTYPEFR[PARAMBIOTYPEFR$TYPEFR %in% STATION$TYPEFR,]

###################################################
## Calcul de l'�tat bio et mise en forme du tableau
###################################################
#if (BASSIN %in% c("AESN","AERC","AELB","AEAG") ){
#TABLEBIO<-TABLEBIO[as.character(TABLEBIO$PARAGROUP) != "IBMA",]
#}
#TABLEBIO<-TABLEBIO[ TABLEBIO$PARALIBGROUP %in% names(PARAMBIOTYPEFR),]
# On cr�e une copie de BIO_MOY et on supprime les lignes en fonction de PARAMBIOTYPEFR
BIO_MOY2<-merge(BIO_MOY,PARAMBIOTYPEFR,by="TYPEFR",all.x=TRUE)
print(nrow(BIO_MOY2))
for ( i in 1:nrow(TABLEBIO)) {
BIO_MOY2<-BIO_MOY2[!(  BIO_MOY2$PARAGROUP == TABLEBIO$PARAGROUP[i] & BIO_MOY2[,TABLEBIO$PARALIBGROUP[i]] != "oui"),]
BIO_MOY2<-BIO_MOY2[!is.na(BIO_MOY2$TYPEFR),]
}
gc()

BIO_MOY<-BIO_MOY2  ##N�cessaire � moins de voir le calcul des indicateurs BIO qui ne sont pas pris en compte dans REEE2018
gc()

## On prend la classe de l'�l�ment de qualit� le plus d�calssant
RLT_BIO<-aggregate(CLASSEBIO ~ STATION, data = BIO_MOY2 , max)
RLT_BIO<-RLT_BIO[order(RLT_BIO$STATION),]
names(RLT_BIO)[2]<-"ETATBIO"
gc()

##mise en forme des classe d'�tat pour les autres indices

for (i in  1:nrow(TABLEBIO)  ) {
	TEMPBIO<-BIO_MOY[BIO_MOY$PARAGROUP==TABLEBIO$PARAGROUP[i],c("STATION", "CLASSEBIO")]  ##ICI on prend BIO_MOY pour avoir tous les indice m�me ceux � "non" dans PARAMBIOTYPEFR
	names(TEMPBIO)[2]<-TABLEBIO$PARALIBGROUP[i]
	RLT_BIO<-merge(RLT_BIO,TEMPBIO,by="STATION", all=TRUE)
	rm(TEMPBIO)
}

########################################
# Calcul Nb indicateur bio pr�sents & listing
######################################
NBBIO<-RLT_BIO
N_RLT_BIO<-names(NBBIO)
NBBIO<-merge(NBBIO,STATION[,c("STATION", "TYPEFR")],by="STATION",all.x=TRUE)
N_PARAMBIOTYPEFR<-names(PARAMBIOTYPEFR)
names(PARAMBIOTYPEFR)[3:ncol(PARAMBIOTYPEFR)]<-paste0(names(PARAMBIOTYPEFR)[3:ncol(PARAMBIOTYPEFR)],"ok")
NBBIO<-merge(NBBIO,PARAMBIOTYPEFR,by="TYPEFR",all.x=TRUE)
names(PARAMBIOTYPEFR)<-N_PARAMBIOTYPEFR

NBBIO$NBINDICBIO<-as.numeric(0)
NBBIO$NBINDICBIO[!is.na(NBBIO$IBD) & NBBIO$IBDok == "oui" & !is.na(NBBIO$ETATBIO) ]<-NBBIO$NBINDICBIO[!is.na(NBBIO$IBD)& NBBIO$IBDok == "oui" & !is.na(NBBIO$ETATBIO) ]+1
NBBIO$NBINDICBIO[!is.na(NBBIO$IPR)& NBBIO$IPRok == "oui" & !is.na(NBBIO$ETATBIO)]<-NBBIO$NBINDICBIO[!is.na(NBBIO$IPR) &  NBBIO$IPRok == "oui" & !is.na(NBBIO$ETATBIO) ]+1
NBBIO$NBINDICBIO[!is.na(NBBIO$IPRPLUS)  & NBBIO$IPRPLUSok == "oui" & !is.na(NBBIO$ETATBIO)]<-NBBIO$NBINDICBIO[!is.na(NBBIO$IPRPLUS) & NBBIO$IPRPLUSok == "oui" & !is.na(NBBIO$ETATBIO) ]+1 
NBBIO$NBINDICBIO[!is.na(NBBIO$IBMR) & NBBIO$IBMRok == "oui" & !is.na(NBBIO$ETATBIO)]<-NBBIO$NBINDICBIO[!is.na(NBBIO$IBMR) & NBBIO$IBMRok == "oui" & !is.na(NBBIO$ETATBIO)]+1	
if (BASSIN =="AESN"){ # AESN moyenne IBGA et IBG --> ligne sp�cifique sinon condition marche pas
	NBBIO$NBINDICBIO[!is.na(NBBIO$IBG)  & NBBIO$IBGok == "oui" & !is.na(NBBIO$ETATBIO)]<-NBBIO$NBINDICBIO[!is.na(NBBIO$IBG) & NBBIO$IBGok == "oui" & !is.na(NBBIO$ETATBIO) ]+1 
	NBBIO$NBINDICBIO[!is.na(NBBIO$I2M2)  & NBBIO$I2M2ok == "oui" & !is.na(NBBIO$ETATBIO)]<-NBBIO$NBINDICBIO[!is.na(NBBIO$I2M2) & NBBIO$I2M2ok == "oui" & !is.na(NBBIO$ETATBIO) ]+1 
	NBBIO$NBINDICBIO[!is.na(NBBIO$IBGA) & NBBIO$IBGAok == "oui"  & !is.na(NBBIO$ETATBIO) ]<-NBBIO$NBINDICBIO[!is.na(NBBIO$IBGA) & NBBIO$IBGAok == "oui"  & !is.na(NBBIO$ETATBIO) ]+1 

} else {
	NBBIO$NBINDICBIO[(!is.na(NBBIO$IBG) | !is.na(NBBIO$IBGA)) & NBBIO$IBGok == "oui"  & !is.na(NBBIO$ETATBIO) ]<-NBBIO$NBINDICBIO[(!is.na(NBBIO$IBG) | !is.na(NBBIO$IBGA)) & NBBIO$IBGok == "oui"  & !is.na(NBBIO$ETATBIO) ]+1 
	NBBIO$NBINDICBIO[!is.na(NBBIO$I2M2)  & NBBIO$I2M2ok == "oui" & !is.na(NBBIO$ETATBIO)]<-NBBIO$NBINDICBIO[!is.na(NBBIO$I2M2) & NBBIO$I2M2ok == "oui" & !is.na(NBBIO$ETATBIO) ]+1 
}

NBBIO$LISTINDICBIO<-as.character("")
NBBIO$LISTINDICBIO[!is.na(NBBIO$IBD) & NBBIO$IBDok == "oui" & !is.na(NBBIO$ETATBIO)]<-"IBD"
if (BASSIN =="AESN"){ # AESN moyenne IBGA et IBG --> ligne sp�cifique sinon condidation marche pas
	NBBIO$LISTINDICBIO[!is.na(NBBIO$IBG)  & NBBIO$IBGok == "oui" & !is.na(NBBIO$ETATBIO) ]<-paste0(NBBIO$LISTINDICBIO[!is.na(NBBIO$IBG) & NBBIO$IBGok == "oui" & !is.na(NBBIO$ETATBIO)],";IBG") 
	NBBIO$LISTINDICBIO[!is.na(NBBIO$IBGA)  & NBBIO$IBGAok == "oui" & !is.na(NBBIO$ETATBIO) ]<-paste0(NBBIO$LISTINDICBIO[!is.na(NBBIO$IBGA) & NBBIO$IBGAok == "oui" & !is.na(NBBIO$ETATBIO)],";IBGA") 

	NBBIO$LISTINDICBIO[!is.na(NBBIO$I2M2) & NBBIO$I2M2ok == "oui" & !is.na(NBBIO$ETATBIO)]<-paste0(NBBIO$LISTINDICBIO[!is.na(NBBIO$I2M2) & NBBIO$I2M2ok == "oui" & !is.na(NBBIO$ETATBIO)],";I2M2") 
} else {
	NBBIO$LISTINDICBIO[(!is.na(NBBIO$IBG) | !is.na(NBBIO$IBGA)) & NBBIO$IBGok == "oui" & !is.na(NBBIO$ETATBIO)]<-paste0(NBBIO$LISTINDICBIO[(!is.na(NBBIO$IBG) | !is.na(NBBIO$IBGA)) & NBBIO$IBGok == "oui" & !is.na(NBBIO$ETATBIO)],";IBG")  
	NBBIO$LISTINDICBIO[!is.na(NBBIO$I2M2)]<-paste0(NBBIO$LISTINDICBIO[!is.na(NBBIO$I2M2)],";I2M2") 
}
NBBIO$LISTINDICBIO[!is.na(NBBIO$IPR) & NBBIO$IPRok == "oui" & !is.na(NBBIO$ETATBIO)]<-paste0(NBBIO$LISTINDICBIO[!is.na(NBBIO$IPR) & NBBIO$IPRok == "oui" & !is.na(NBBIO$ETATBIO)],";IPR")
NBBIO$LISTINDICBIO[!is.na(NBBIO$IPRPLUS) & NBBIO$IPRPLUSok == "oui" & !is.na(NBBIO$ETATBIO)]<-paste0(NBBIO$LISTINDICBIO[!is.na(NBBIO$IPRPLUS) & NBBIO$IPRPLUSok == "oui" & !is.na(NBBIO$ETATBIO)],";IPRPLUS")
NBBIO$LISTINDICBIO[!is.na(NBBIO$IBMR) & NBBIO$IBMRok == "oui" & !is.na(NBBIO$ETATBIO)]<-paste0(NBBIO$LISTINDICBIO[!is.na(NBBIO$IBMR) & NBBIO$IBMRok == "oui" & !is.na(NBBIO$ETATBIO)],";IBMR")

# retrait du ";" si c'est le 1er symbole, pour faire propre dans tableau
cond1<-substr(NBBIO$LISTINDICBIO,1,1)==";"
NBBIO$LISTINDICBIO[cond1]<-substr(NBBIO$LISTINDICBIO[cond1],2,nchar(NBBIO$LISTINDICBIO[cond1]))

NBBIO_NAMES<-c("NBINDICBIO","LISTINDICBIO") # cr�ation names pour mise en forme du fichier final export�
RLT_BIO<-NBBIO
RLT_BIO<-RLT_BIO[,c(N_RLT_BIO,NBBIO_NAMES)]
gc() ## compacte R
flush.console()