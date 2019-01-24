##############################################
## SEEE - COURS D'EAU : état biologique
## Application de l'arrêté de janvier 2010
## créé janvier 2013
######
## script modifié en decembre 2014 pour adapter les regles du 2nd cycle (arrêté janvier 2010 révisé
##############################################

##############################
## Mise en forme des données
##############################

## variable uniquement pour DEV hors interface -- a supprimer
# cheminPARAMR<-"D:\\S3Rlocal - 2nd cycle\\S3R1.0\\PARAMETRES\\DEFAUT\\"
# cheminDATABIO<-"D:\\S3Rlocal - 2nd cycle\\S3R1.0\\DONNEES_BRUTES\\"
# ZPARAMBIO<-read.csv2(paste0 (cheminPARAMR, "PARAMETREBIO.csv"),sep = ";")

# TABLEBIO<-aggregate(IDTRI ~ PARAGROUP + PARALIBGROUP, data = ZPARAMBIO , min)
# TABLEBIO<-TABLEBIO[order(TABLEBIO$IDTRI),]
# TABLEBIO$PARALIBGROUP<-as.character(TABLEBIO$PARALIBGROUP)
# TABLEBIO$PARAGROUP<-as.character(TABLEBIO$PARAGROUP)
# TABLEBIO$IDTRI<-as.numeric(as.character(TABLEBIO$IDTRI))

# STATION<-read.csv2(paste0 (cheminPARAMR, "STATION.csv"),sep = ";")
# GRILLEIBD_2c<-read.csv2(paste0 (cheminPARAMR, "GRILLEIBD_2c.csv"),sep = ";")
# GRILLEIBG_2c<-read.csv2(paste0 (cheminPARAMR, "GRILLEIBG_2c.csv"),sep = ";")
# GRILLEIPR_2c<-read.csv2(paste0 (cheminPARAMR, "GRILLEIPR_2c.csv"),sep = ";")
# GRILLEIBMR_2c<-read.csv2(paste0 (cheminPARAMR, "GRILLEIBMR_c2.csv"),sep = ";")

# # mise en forme des données pour DEV
# DATABIO<-read.csv2(paste0 (cheminDATABIO, "S3E_DATABIO_RCS2013.csv"),sep = ";")
# DATABIO<-merge(DATABIO, ZPARAMBIO[,c("PARAMETRE", "PARAGROUP", "PARALIBGROUP", "IDTRI")], by="PARAMETRE")
# DATABIO$STATION<-as.character(DATABIO$STATION)
# DATABIO$PARAMETRE<-as.character(DATABIO$PARAMETRE)
# DATABIO$PARAMETRELIB<-as.character(DATABIO$PARAMETRELIB)
# DATABIO$PARAGROUP<-as.character(DATABIO$PARAGROUP)
# DATABIO$RESULTAT<-as.numeric(DATABIO$RESULTAT)
# DATABIO$DATEPRELEV<-as.character(DATABIO$DATEPRELEV)
# DATABIO$HEUREPRELEV<-as.character(DATABIO$HEUREPRELEV)

# STATION$STATION<-as.character(STATION$STATION)
# STATION$TYPEFR<-as.character(STATION$TYPEFR)

# GRILLEIBD_2c$TYPEFR<-as.character(GRILLEIBD_2c$TYPEFR)
# GRILLEIBD_2c$INFB<-as.numeric(as.character(GRILLEIBD_2c$INFB))
# GRILLEIBD_2c$INFV<-as.numeric(as.character(GRILLEIBD_2c$INFV))
# GRILLEIBD_2c$INFJ<-as.numeric(as.character(GRILLEIBD_2c$INFJ))
# GRILLEIBD_2c$INFO<-as.numeric(as.character(GRILLEIBD_2c$INFO))
# GRILLEIBD_2c$INFR<-as.numeric(as.character(GRILLEIBD_2c$INFR))
# GRILLEIBD_2c$VALREF<-as.numeric(as.character(GRILLEIBD_2c$VALREF))  ## à intégrer MEconformité
# GRILLEIBD_2c$VALMIN<-as.numeric(as.character(GRILLEIBD_2c$VALMIN)) ## à intégrer MEconformité

# GRILLEIBG_2c$TYPEFR<-as.character(GRILLEIBG_2c$TYPEFR)
# GRILLEIBG_2c$INFB<-as.numeric(as.character(GRILLEIBG_2c$INFB))
# GRILLEIBG_2c$INFV<-as.numeric(as.character(GRILLEIBG_2c$INFV))
# GRILLEIBG_2c$INFJ<-as.numeric(as.character(GRILLEIBG_2c$INFJ))
# GRILLEIBG_2c$INFO<-as.numeric(as.character(GRILLEIBG_2c$INFO))
# GRILLEIBG_2c$INFR<-as.numeric(as.character(GRILLEIBG_2c$INFR))
# GRILLEIBG_2c$VALREF<-as.numeric(as.character(GRILLEIBG_2c$VALREF))  ## à intégrer MEconformité

# GRILLEIPR_2c$TYPEFR<-as.character(GRILLEIPR_2c$TYPEFR)
# GRILLEIPR_2c$INFB<-as.numeric(as.character(GRILLEIPR_2c$INFB))
# GRILLEIPR_2c$INFV<-as.numeric(as.character(GRILLEIPR_2c$INFV))
# GRILLEIPR_2c$INFJ<-as.numeric(as.character(GRILLEIPR_2c$INFJ))
# GRILLEIPR_2c$INFO<-as.numeric(as.character(GRILLEIPR_2c$INFO))
# GRILLEIPR_2c$INFR<-as.numeric(as.character(GRILLEIPR_2c$INFR))
# GRILLEIPR_2c$VALALTIV<-as.numeric(as.character(GRILLEIPR_2c$VALALTIV))  ## à intégrer MEconformité + CHAMP ALTITUDE dans table STATION.csv

# GRILLEIBMR_2c$TYPEFR<-as.character(GRILLEIBMR_2c$TYPEFR)
# GRILLEIBMR_2c$INFB<-as.numeric(as.character(GRILLEIBMR_2c$INFB))
# GRILLEIBMR_2c$INFV<-as.numeric(as.character(GRILLEIBMR_2c$INFV))
# GRILLEIBMR_2c$INFJ<-as.numeric(as.character(GRILLEIBMR_2c$INFJ))
# GRILLEIBMR_2c$INFO<-as.numeric(as.character(GRILLEIBMR_2c$INFO))
# GRILLEIBMR_2c$INFR<-as.numeric(as.character(GRILLEIBMR_2c$INFR))
# GRILLEIBMR_2c$VALREF<-as.numeric(as.character(GRILLEIBMR_2c$VALREF))  ## à intégrer MEconformité

#### FIN variable uniquement pour DEV

gc() ## compacte R
########################################################
## Calcul de la moyenne de chaque indicateur de qualité
## modifié le 09/12/14
########################################################
BIO_MOY<-aggregate(RESULTAT ~ STATION + PARAGROUP, data = DATABIO , mean)
BIO_MOY$RESULTAT<-round(BIO_MOY$RESULTAT,2)
BIO_MOY<-BIO_MOY[order(BIO_MOY$STATION, BIO_MOY$PARAGROUP),]

## modifié le 09/12/14
BIO_MOY$CLASSEBIO<-as.numeric("")
BIO_MOY$EQRBIO<-as.numeric("")
BIO_MOY<-merge(BIO_MOY,STATION[,c("STATION", "TYPEFR","ALTITUDE")],by="STATION")
BIO_MOY_NAMES<-names(BIO_MOY)

##### IBD
# calcul EQR
BIO_MOY<-merge(BIO_MOY,GRILLEIBD,by="TYPEFR", all.x=TRUE)
condIBD<-BIO_MOY$PARAGROUP=="5856"
BIO_MOY$EQRBIO[condIBD]<-round((BIO_MOY$RESULTAT[condIBD]-BIO_MOY$VALMIN[condIBD])/(BIO_MOY$VALREF[condIBD]-BIO_MOY$VALMIN[condIBD]),3)

#Calcul classe de qualité selon EQRBIO : IBD
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
BIO_MOY$EQRBIO[condIBG]<-round(((BIO_MOY$RESULTAT[condIBG]-1)/(BIO_MOY$VALREF[condIBG]-1)),5)
BIO_MOY$EQRBIO[condIBGA]<-round(((BIO_MOY$RESULTAT[condIBGA]-1)/(BIO_MOY$VALREF[condIBGA]-1)),5)

#Calcul classe de qualité selon EQRBIO : IBG & IBGA
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

########## IPR
BIO_MOY<-BIO_MOY[,c(BIO_MOY_NAMES)]
BIO_MOY<-merge(BIO_MOY,GRILLEIPR,by="TYPEFR", all.x=TRUE)

# MAJ borne inferieure Verte si alti >=500m. Si alti non renseignée, INFV ne change pas.
BIO_MOY$INFV[BIO_MOY$ALTITUDE>=500 & !(is.na(BIO_MOY$ALTITUDE))]<-BIO_MOY$VALALTIV[BIO_MOY$ALTITUDE>=500 & !(is.na(BIO_MOY$ALTITUDE))]

#Calcul classe de qualité selon note moyenne IPR
condIPR<-BIO_MOY$PARAGROUP=="7036"
BIO_MOY$CLASSEBIO[condIPR & BIO_MOY$RESULTAT >=0 & BIO_MOY$RESULTAT <=BIO_MOY$INFB]<-1
BIO_MOY$CLASSEBIO[condIPR & BIO_MOY$RESULTAT >BIO_MOY$INFB & BIO_MOY$RESULTAT <=BIO_MOY$INFV]<-2
BIO_MOY$CLASSEBIO[condIPR & BIO_MOY$RESULTAT >BIO_MOY$INFV & BIO_MOY$RESULTAT <=BIO_MOY$INFJ]<-3
BIO_MOY$CLASSEBIO[condIPR & BIO_MOY$RESULTAT >BIO_MOY$INFJ & BIO_MOY$RESULTAT <=BIO_MOY$INFO]<-4
BIO_MOY$CLASSEBIO[condIPR & BIO_MOY$RESULTAT >BIO_MOY$INFO ]<-5  # borne INFR sert uniquement pour le calcul de l'indice (limite max de l'indice 0)
BIO_MOY<-BIO_MOY[,c(BIO_MOY_NAMES)]

########## IBMR
# calcul EQR
BIO_MOY<-merge(BIO_MOY,GRILLEIBMR,by="TYPEFR", all.x=TRUE)
condIBMR<-BIO_MOY$PARAGROUP=="2928"
BIO_MOY$EQRBIO[condIBMR]<-round(BIO_MOY$RESULTAT[condIBMR]/BIO_MOY$VALREF[condIBMR],3)

#Calcul classe de qualité selon EQRBIO : IBMR
BIO_MOY$CLASSEBIO[condIBMR & BIO_MOY$EQRBIO >=BIO_MOY$INFB]<-1
BIO_MOY$CLASSEBIO[condIBMR & BIO_MOY$EQRBIO >=BIO_MOY$INFV & BIO_MOY$EQRBIO <BIO_MOY$INFB]<-2
BIO_MOY$CLASSEBIO[condIBMR & BIO_MOY$EQRBIO >=BIO_MOY$INFJ & BIO_MOY$EQRBIO <BIO_MOY$INFV]<-3
BIO_MOY$CLASSEBIO[condIBMR & BIO_MOY$EQRBIO >=BIO_MOY$INFO & BIO_MOY$EQRBIO <BIO_MOY$INFJ]<-4
BIO_MOY$CLASSEBIO[condIBMR & BIO_MOY$EQRBIO >=BIO_MOY$INFR & BIO_MOY$EQRBIO <BIO_MOY$INFO]<-5
BIO_MOY<-BIO_MOY[,c(BIO_MOY_NAMES)]
flush.console()

###################################################
## Calcul de l'état bio et mise en forme du tableau
###################################################
RLT_BIO<-aggregate(CLASSEBIO ~ STATION, data = BIO_MOY , max)
RLT_BIO<-RLT_BIO[order(RLT_BIO$STATION),]
names(RLT_BIO)[2]<-"ETATBIO"

for (i in  1:nrow(TABLEBIO)  ) {
	TEMPBIO<-BIO_MOY[BIO_MOY$PARAGROUP==TABLEBIO$PARAGROUP[i],c("STATION", "CLASSEBIO")]
	names(TEMPBIO)[2]<-TABLEBIO$PARALIBGROUP[i]
	RLT_BIO<-merge(RLT_BIO,TEMPBIO,by="STATION", all.x=TRUE)
	rm(TEMPBIO)
}

gc() ## compacte R
flush.console()
