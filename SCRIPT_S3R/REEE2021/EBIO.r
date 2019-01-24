##############################################
## SEEE - COURS D'EAU : état biologique
## Application de l'arrêté de janvier 2010
## créé janvier 2013
######
## script modifié en avril 2015 pour adapter les regles du 3nd cycle (arrêté janvier 2010 révisé
##############################################


gc() ## compacte R
########################################################
## Calcul de la moyenne de chaque indicateur de qualité
## modifié le 24/04/15
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
BIO_MOY$EQRBIO[condIBD]<-(BIO_MOY$RESULTAT[condIBD]-BIO_MOY$VALMIN[condIBD])/(BIO_MOY$VALREF[condIBD]-BIO_MOY$VALMIN[condIBD])

## round supprimé car pb d'arrondi constaté malgré tout
# BIO_MOY$EQRBIO[condIBD]<-round((BIO_MOY$RESULTAT[condIBD]-BIO_MOY$VALMIN[condIBD])/(BIO_MOY$VALREF[condIBD]-BIO_MOY$VALMIN[condIBD]),3)

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
## modif cycle 3 : 24/04/15 --> en commentaire car I2M2 fournit en EQR
#BIO_MOY$EQRBIO[condIBG]<-(BIO_MOY$RESULTAT[condIBG]-1)/(BIO_MOY$VALREF[condIBG]-1)
#BIO_MOY$EQRBIO[condIBGA]<-(BIO_MOY$RESULTAT[condIBGA]-1)/(BIO_MOY$VALREF[condIBGA]-1)
BIO_MOY$EQRBIO[condIBG]<-BIO_MOY$RESULTAT[condIBG] # résultat déjà en EQR via code SANDRE
BIO_MOY$EQRBIO[condIBGA]<-BIO_MOY$RESULTAT[condIBGA]

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
# modif cycle 3 : désactivé car par de valeur alti pour l'EQR !
#BIO_MOY$INFV[BIO_MOY$ALTITUDE>=500 & !(is.na(BIO_MOY$ALTITUDE))]<-BIO_MOY$VALALTIV[BIO_MOY$ALTITUDE>=500 & !(is.na(BIO_MOY$ALTITUDE))]
condIPR<-BIO_MOY$PARAGROUP=="7036"

# modif cycle 3 : Copie Résultat dans EQR pour export final (option) car EQR IPR est donné au départ
BIO_MOY$EQRBIO[condIPR]<-BIO_MOY$RESULTAT[condIPR] # résultat déjà en EQR via code SANDRE

#Calcul classe de qualité selon note moyenne IPR
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
BIO_MOY$EQRBIO[condIBMR]<-BIO_MOY$RESULTAT[condIBMR]/BIO_MOY$VALREF[condIBMR]

## round supprimé car pb d'arrondi constaté malgré tout
# BIO_MOY$EQRBIO[condIBMR]<-round(BIO_MOY$RESULTAT[condIBMR]/BIO_MOY$VALREF[condIBMR],3)

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

########################################
# Calcul Nb indicateur bio présents & listing
######################################
NBBIO<-RLT_BIO
NBBIO$NBINDICBIO<-as.numeric(0)
NBBIO$NBINDICBIO[!is.na(NBBIO$IBD)]<-NBBIO$NBINDICBIO[!is.na(NBBIO$IBD)]+1
NBBIO$NBINDICBIO[!is.na(NBBIO$IPR)]<-NBBIO$NBINDICBIO[!is.na(NBBIO$IPR)]+1
NBBIO$NBINDICBIO[!is.na(NBBIO$IBMR)]<-NBBIO$NBINDICBIO[!is.na(NBBIO$IBMR)]+1	
if (BASSIN =="AESN"){ # AESN moyenne IBGA et IBG --> ligne spécifique sinon condition marche pas
	NBBIO$NBINDICBIO[!is.na(NBBIO$IBG)]<-NBBIO$NBINDICBIO[!is.na(NBBIO$IBG)]+1 
} else {
	NBBIO$NBINDICBIO[!is.na(NBBIO$IBG) | !is.na(NBBIO$IBGA)]<-NBBIO$NBINDICBIO[!is.na(NBBIO$IBG) | !is.na(NBBIO$IBGA)]+1 
}

NBBIO$LISTINDICBIO<-as.character("")
NBBIO$LISTINDICBIO[!is.na(NBBIO$IBD)]<-"IBD"
if (BASSIN =="AESN"){ # AESN moyenne IBGA et IBG --> ligne spécifique sinon condidation marche pas
	NBBIO$LISTINDICBIO[!is.na(NBBIO$IBG)]<-paste0(NBBIO$LISTINDICBIO[!is.na(NBBIO$IBG)],";IBG") 
} else {
	NBBIO$LISTINDICBIO[!is.na(NBBIO$IBG) | !is.na(NBBIO$IBGA)]<-paste0(NBBIO$LISTINDICBIO[!is.na(NBBIO$IBG) | !is.na(NBBIO$IBGA)],";IBG")  
}
NBBIO$LISTINDICBIO[!is.na(NBBIO$IPR)]<-paste0(NBBIO$LISTINDICBIO[!is.na(NBBIO$IPR)],";IPR")
NBBIO$LISTINDICBIO[!is.na(NBBIO$IBMR)]<-paste0(NBBIO$LISTINDICBIO[!is.na(NBBIO$IBMR)],";IBMR")

# retrait du ";" si c'est le 1er symbole, pour faire propre dans tableau
cond1<-substr(NBBIO$LISTINDICBIO,1,1)==";"
NBBIO$LISTINDICBIO[cond1]<-substr(NBBIO$LISTINDICBIO[cond1],2,nchar(NBBIO$LISTINDICBIO[cond1]))

NBBIO_NAMES<-c("NBINDICBIO","LISTINDICBIO") # création names pour mise en forme du fichier final exporté
RLT_BIO<-NBBIO
gc() ## compacte R
flush.console()
