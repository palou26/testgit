##############################################
## SEEE - COURS D'EAU : état biologique
## Application de l'arrêté de janvier 2010
##############################################


gc() ## compacte R

# pour la ChloA on ne prend que la saison estivale
DATABIO<-rbind(
				DATABIO[DATABIO$PARAMETRE == "1439" & DATABIO$SAISON == "ETE" ,],
				DATABIO[DATABIO$PARAMETRE != "1439",])

########################################################
## Calcul de la moyenne de chaque indicateur de qualité
########################################################
BIO_MOY<-aggregate(RESULTAT ~ STATION + PARAGROUP, data = DATABIO , mean)
BIO_MOY$RESULTAT<-round(BIO_MOY$RESULTAT,4)
BIO_MOY<-BIO_MOY[order(BIO_MOY$STATION, BIO_MOY$PARAGROUP),]

BIO_MOY$CLASSEBIO<-as.numeric("")
BIO_MOY<-merge(BIO_MOY,STATION[,c("STATION","NOMSTATION","EUCD","ZMOY","STRATIFIE","TPSSEJOUR","FIABILITSEJOUR")],by="STATION")
BIO_MOY_NAMES<-names(BIO_MOY)

BIO_MOY<-merge(BIO_MOY,PARAMETREBIO,by="PARAGROUP", all.x=TRUE)
#Calcul classe de qualité IPL
BIO_MOY$CLASSEBIO[BIO_MOY$PARAGROUP=="6591" & BIO_MOY$RESULTAT >=0 & BIO_MOY$RESULTAT <=BIO_MOY$INFB]<-1
BIO_MOY$CLASSEBIO[BIO_MOY$PARAGROUP=="6591" & BIO_MOY$RESULTAT >BIO_MOY$INFB & BIO_MOY$RESULTAT <=BIO_MOY$INFV]<-2
BIO_MOY$CLASSEBIO[BIO_MOY$PARAGROUP=="6591" & BIO_MOY$RESULTAT >BIO_MOY$INFV & BIO_MOY$RESULTAT <=BIO_MOY$INFJ]<-3
BIO_MOY$CLASSEBIO[BIO_MOY$PARAGROUP=="6591" & BIO_MOY$RESULTAT >BIO_MOY$INFJ & BIO_MOY$RESULTAT <=BIO_MOY$INFO]<-4
BIO_MOY$CLASSEBIO[BIO_MOY$PARAGROUP=="6591" & BIO_MOY$RESULTAT >BIO_MOY$INFO ]<-5

#Calcul classe de qualité IMOL (5857) et "IOBL" (6346)
BIO_MOY$CLASSEBIO[BIO_MOY$PARAGROUP %in%  c("5857","6346") & BIO_MOY$RESULTAT >=BIO_MOY$INFB]<-1
BIO_MOY$CLASSEBIO[BIO_MOY$PARAGROUP %in%  c("5857","6346") & BIO_MOY$RESULTAT >=BIO_MOY$INFV & BIO_MOY$RESULTAT <BIO_MOY$INFB]<-2
BIO_MOY$CLASSEBIO[BIO_MOY$PARAGROUP %in%  c("5857","6346") & BIO_MOY$RESULTAT >=BIO_MOY$INFJ & BIO_MOY$RESULTAT <BIO_MOY$INFV]<-3
BIO_MOY$CLASSEBIO[BIO_MOY$PARAGROUP %in%  c("5857","6346") & BIO_MOY$RESULTAT >=BIO_MOY$INFO & BIO_MOY$RESULTAT <BIO_MOY$INFJ]<-4
BIO_MOY$CLASSEBIO[BIO_MOY$PARAGROUP %in%  c("5857","6346") & BIO_MOY$RESULTAT >=BIO_MOY$INFR & BIO_MOY$RESULTAT <BIO_MOY$INFO]<-5

###############################################
###Calcul classe de qualité Chlorophile A ChloA
################################################
	# fonction en fonction del aprofondeur moyenne
funct_limChloA<-function(prof, constante) {
	Y<-10^(0.754-0.489*log10(prof)+constante*(1.038+((log10(prof)-0.942)^2)/4.077)^0.5)
	return(as.numeric(Y))
}

condChloA<-BIO_MOY$PARAGROUP == "1439"
BIO_MOY$INFB[condChloA]<-funct_limChloA(BIO_MOY$ZMOY[condChloA],0.244)
BIO_MOY$INFV[condChloA]<-funct_limChloA(BIO_MOY$ZMOY[condChloA],0.487)
BIO_MOY$INFJ[condChloA]<-funct_limChloA(BIO_MOY$ZMOY[condChloA],0.731)
BIO_MOY$INFO[condChloA]<-funct_limChloA(BIO_MOY$ZMOY[condChloA],0.945)
BIO_MOY$INFR[condChloA]<-funct_limChloA(BIO_MOY$ZMOY[condChloA],1.189)  #constante = 0.945 + 0.244

###Pour certains Plan d'eau, il faut utiliser le tableau 1 (p49 du guide dec 2012)
BIO_MOY<-merge(BIO_MOY,PECasPartiChloA, by.x="EUCD",by.y="EU_CD",all.x=TRUE)
condPEparti<-condChloA & !is.na(BIO_MOY$TBB)
BIO_MOY$INFB[condPEparti]<-BIO_MOY$TBB[condPEparti]
BIO_MOY$INFV[condPEparti]<-BIO_MOY$Bmo[condPEparti]
BIO_MOY$INFJ[condPEparti]<-BIO_MOY$MoMe[condPEparti]
BIO_MOY$INFO[condPEparti]<-BIO_MOY$MeMa[condPEparti]
BIO_MOY$INFR[condPEparti]<-BIO_MOY$MeMa[condPEparti] + (BIO_MOY$MeMa[condPEparti] -   BIO_MOY$MeMa[condPEparti]  )

#mise en classe
BIO_MOY$CLASSEBIO[BIO_MOY$PARAGROUP=="1439" & BIO_MOY$RESULTAT >=0 & BIO_MOY$RESULTAT <=BIO_MOY$INFB]<-1
BIO_MOY$CLASSEBIO[BIO_MOY$PARAGROUP=="1439" & BIO_MOY$RESULTAT >BIO_MOY$INFB & BIO_MOY$RESULTAT <=BIO_MOY$INFV]<-2
BIO_MOY$CLASSEBIO[BIO_MOY$PARAGROUP=="1439" & BIO_MOY$RESULTAT >BIO_MOY$INFV & BIO_MOY$RESULTAT <=BIO_MOY$INFJ]<-3
BIO_MOY$CLASSEBIO[BIO_MOY$PARAGROUP=="1439" & BIO_MOY$RESULTAT >BIO_MOY$INFJ & BIO_MOY$RESULTAT <=BIO_MOY$INFO]<-4
BIO_MOY$CLASSEBIO[BIO_MOY$PARAGROUP=="1439" & BIO_MOY$RESULTAT >BIO_MOY$INFO ]<-5


###SAUVEGARDE DES PARAMETRES
BIO_MOY$COMPARTIMENT<-"BIO"
COLONNESPARAM<-c("STATION","ZMOY","TPSSEJOUR", "FIABILITSEJOUR", "COMPARTIMENT","PARAGROUP" ,"INFB","INFV","INFJ","INFO")
PARAMPEBIO<-BIO_MOY[!duplicated(BIO_MOY[,COLONNESPARAM]),COLONNESPARAM]
names(PARAMPEBIO)<-names(PARAMPE)
PARAMPE<-merge(PARAMPE,PARAMPEBIO, all = TRUE)

##Finalisation
flush.console()

###################################################
## Calcul de l'état bio et mise en forme du tableau
###################################################

# Attention, pour EtatBIO ,seul lChloA et IPL compte
RLT_BIO<-data.frame(STATION = sort(unique(BIO_MOY$STATION)))
if (REST_IPL_STATI == "oui") {
	EBIO<-aggregate(CLASSEBIO ~ STATION, data = BIO_MOY[BIO_MOY$PARAGROUP =="1439" | (BIO_MOY$PARAGROUP =="6591" & BIO_MOY$STRATIFIE == "oui"  ),]  , max)
} else {
	EBIO<-aggregate(CLASSEBIO ~ STATION, data = BIO_MOY[BIO_MOY$PARAGROUP %in% c("1439","6591"),]  , max)
}
names(EBIO)[2]<-"ETATBIO"
RLT_BIO<-merge(RLT_BIO,EBIO,by="STATION", all.x=TRUE)
RLT_BIO<-RLT_BIO[order(RLT_BIO$STATION),]

##On ajoute en état indéterminé, les stations manquante
AJOUT_STATMANQ<-data.frame(STATION=unique(BIO_MOY$STATION[!(BIO_MOY$STATION %in%  RLT_BIO$STATION)]) )
if(nrow(AJOUT_STATMANQ) > 0) {
	AJOUT_STATMANQ$ETATBIO<-0
	RLT_BIO<-rbind(RLT_BIO,AJOUT_STATMANQ)
	RLT_BIO<-RLT_BIO[order(RLT_BIO$STATION),]
}


#on réorganise pour mettre en forme le fichier de résultats en rajoutant les autres parametres
for (i in  1:nrow(TABLEBIO)  ) {
	TEMPBIO<-BIO_MOY[BIO_MOY$PARAGROUP==TABLEBIO$PARAGROUP[i],c("STATION", "CLASSEBIO")]
	names(TEMPBIO)[2]<-TABLEBIO$PARALIBGROUP[i]
	RLT_BIO<-merge(RLT_BIO,TEMPBIO,by="STATION", all.x=TRUE)
	rm(TEMPBIO)
}


########################################
# Calcul Nb indicateur bio présents & listing
######################################
NBBIO<-RLT_BIO
NBBIO$NBINDICBIO<-as.numeric(0)
NBBIO$NBINDICBIO[!is.na(NBBIO$CHLOA)]<-NBBIO$NBINDICBIO[!is.na(NBBIO$CHLOA)]+1
NBBIO$NBINDICBIO[!is.na(NBBIO$IPL)]<-NBBIO$NBINDICBIO[!is.na(NBBIO$IPL)]+1
NBBIO$NBINDICBIO[!is.na(NBBIO$IMOL)]<-NBBIO$NBINDICBIO[!is.na(NBBIO$IMOL)]+1	
NBBIO$NBINDICBIO[!is.na(NBBIO$IOBL)]<-NBBIO$NBINDICBIO[!is.na(NBBIO$IOBL)]+1	
 

NBBIO$LISTINDICBIO<-as.character("")
NBBIO$LISTINDICBIO[!is.na(NBBIO$CHLOA)]<-"CHLOA"
NBBIO$LISTINDICBIO[!is.na(NBBIO$IPL) ]<-paste0(NBBIO$LISTINDICBIO[!is.na(NBBIO$IPL)],";IPL")  
NBBIO$LISTINDICBIO[!is.na(NBBIO$IMOL)]<-paste0(NBBIO$LISTINDICBIO[!is.na(NBBIO$IMOL)],";IMOL")
NBBIO$LISTINDICBIO[!is.na(NBBIO$IOBL)]<-paste0(NBBIO$LISTINDICBIO[!is.na(NBBIO$IOBL)],";IOBL")

# retrait du ";" si c'est le 1er symbole, pour faire propre dans tableau
cond1<-substr(NBBIO$LISTINDICBIO,1,1)==";"
NBBIO$LISTINDICBIO[cond1]<-substr(NBBIO$LISTINDICBIO[cond1],2,nchar(NBBIO$LISTINDICBIO[cond1]))

NBBIO_NAMES<-c("NBINDICBIO","LISTINDICBIO") # création names pour mise en forme du fichier final exporté
RLT_BIO<-NBBIO

gc() ## compacte R
flush.console()
