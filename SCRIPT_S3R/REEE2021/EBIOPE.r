##############################################
## SEEE - COURS D'EAU : �tat biologique
## Application de l'arr�t� de janvier 2010
##############################################


gc() ## compacte R



########################################################
## Calcul de la moyenne de chaque indicateur de qualit�
########################################################
BIO_MOY<-aggregate(RESULTAT ~ STATION + PARAGROUP, data = DATABIO , mean)
BIO_MOY$RESULTAT<-round(BIO_MOY$RESULTAT,4)
BIO_MOY<-BIO_MOY[order(BIO_MOY$STATION, BIO_MOY$PARAGROUP),]

BIO_MOY$CLASSEBIO<-as.numeric("")
BIO_MOY<-merge(BIO_MOY,STATION[,c("STATION","NOMSTATION","EUCD","ZMOY","STRATIFIE","TPSSEJOUR", "FIABILITSEJOUR","EXCEPT_IBML")],by="STATION")
BIO_MOY_NAMES<-names(BIO_MOY)

BIO_MOY<-merge(BIO_MOY,PARAMETREBIO,by="PARAGROUP", all.x=TRUE)

#Calcul classe de qualit� 
BIO_MOY$CLASSEBIO[  BIO_MOY$RESULTAT >=BIO_MOY$INFB]<-1
BIO_MOY$CLASSEBIO[ BIO_MOY$RESULTAT >=BIO_MOY$INFV & BIO_MOY$RESULTAT <BIO_MOY$INFB]<-2
BIO_MOY$CLASSEBIO[ BIO_MOY$RESULTAT >=BIO_MOY$INFJ & BIO_MOY$RESULTAT <BIO_MOY$INFV]<-3
BIO_MOY$CLASSEBIO[ BIO_MOY$RESULTAT >=BIO_MOY$INFO & BIO_MOY$RESULTAT <BIO_MOY$INFJ]<-4
BIO_MOY$CLASSEBIO[BIO_MOY$RESULTAT >=BIO_MOY$INFR & BIO_MOY$RESULTAT <BIO_MOY$INFO]<-5


###SAUVEGARDE DES PARAMETRES
BIO_MOY$COMPARTIMENT<-"BIO"
COLONNESPARAM<-c("STATION","ZMOY","TPSSEJOUR", "FIABILITSEJOUR","COMPARTIMENT","PARAGROUP" ,"INFB","INFV","INFJ","INFO")
PARAMPEBIO<-BIO_MOY[!duplicated(BIO_MOY[,COLONNESPARAM]),COLONNESPARAM]
names(PARAMPEBIO)<-names(PARAMPE)
PARAMPE<-merge(PARAMPE,PARAMPEBIO, all = TRUE)

##Finalisation
flush.console()

###################################################
## Calcul de l'�tat bio et mise en forme du tableau
###################################################
if (REST_IPLAC_STATI == "oui") {
	cond_suppr_iplac_nonstrati<- BIO_MOY$PARAGROUP == "IPLAC" & BIO_MOY$STRATIFIE == "non"
	RLT_BIO_avecIBML<-aggregate(CLASSEBIO ~ STATION, data = BIO_MOY[!cond_suppr_iplac_nonstrati & BIO_MOY$EXCEPT_IBML != "oui",] , max)
	RLT_BIO_sansIBML<-aggregate(CLASSEBIO ~ STATION, data = BIO_MOY[!cond_suppr_iplac_nonstrati & BIO_MOY$EXCEPT_IBML == "oui" &  BIO_MOY$PARAGROUP != "IBML" ,] , max)
} else {
	RLT_BIO_avecIBML<-aggregate(CLASSEBIO ~ STATION, data = BIO_MOY[BIO_MOY$EXCEPT_IBML != "oui",] , max)
	RLT_BIO_sansIBML<-aggregate(CLASSEBIO ~ STATION, data = BIO_MOY[BIO_MOY$EXCEPT_IBML == "oui" &  BIO_MOY$PARAGROUP != "IBML" ,] , max)
}


RLT_BIO<-rbind(RLT_BIO_avecIBML,RLT_BIO_sansIBML)
RLT_BIO<-RLT_BIO[order(RLT_BIO$STATION),]
names(RLT_BIO)[2]<-"ETATBIO"

##On ajoute en �tat ind�termin�, les stations manquante
AJOUT_STATMANQ<-data.frame(STATION=unique(BIO_MOY$STATION[!(BIO_MOY$STATION %in%  RLT_BIO$STATION)]) )
if(nrow(AJOUT_STATMANQ) > 0) {
	AJOUT_STATMANQ$ETATBIO<-0
	RLT_BIO<-rbind(RLT_BIO,AJOUT_STATMANQ)
	RLT_BIO<-RLT_BIO[order(RLT_BIO$STATION),]
}

#on r�organise pour mettre en forme le fichier de r�sultats en rajoutant les autres parametres
for (i in  1:nrow(TABLEBIO)  ) {
	TEMPBIO<-BIO_MOY[BIO_MOY$PARAGROUP==TABLEBIO$PARAGROUP[i],c("STATION", "CLASSEBIO")]
	names(TEMPBIO)[2]<-TABLEBIO$PARALIBGROUP[i]
	RLT_BIO<-merge(RLT_BIO,TEMPBIO,by="STATION", all.x=TRUE)
	rm(TEMPBIO)
}

########################################
# Calcul Nb indicateur bio pr�sents & listing
######################################
NBBIO<-RLT_BIO
NBBIO$NBINDICBIO<-as.numeric(0)
NBBIO$NBINDICBIO[!is.na(NBBIO$IPLAC)]<-NBBIO$NBINDICBIO[!is.na(NBBIO$IPLAC)]+1
NBBIO$NBINDICBIO[!is.na(NBBIO$IBML)]<-NBBIO$NBINDICBIO[!is.na(NBBIO$IBML)]+1
NBBIO$NBINDICBIO[!is.na(NBBIO$IIL)]<-NBBIO$NBINDICBIO[!is.na(NBBIO$IIL)]+1	
	
 

NBBIO$LISTINDICBIO<-as.character("")
NBBIO$LISTINDICBIO[!is.na(NBBIO$IPLAC)]<-"IPLAC"
NBBIO$LISTINDICBIO[!is.na(NBBIO$IBML) ]<-paste0(NBBIO$LISTINDICBIO[!is.na(NBBIO$IBML)],";IBML")  
NBBIO$LISTINDICBIO[!is.na(NBBIO$IIL)]<-paste0(NBBIO$LISTINDICBIO[!is.na(NBBIO$IIL)],";IIL")

# retrait du ";" si c'est le 1er symbole, pour faire propre dans tableau
cond1<-substr(NBBIO$LISTINDICBIO,1,1)==";"
NBBIO$LISTINDICBIO[cond1]<-substr(NBBIO$LISTINDICBIO[cond1],2,nchar(NBBIO$LISTINDICBIO[cond1]))

NBBIO_NAMES<-c("NBINDICBIO","LISTINDICBIO") # cr�ation names pour mise en forme du fichier final export�
RLT_BIO<-NBBIO

gc() ## compacte R
flush.console()
