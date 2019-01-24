##############################################
## SEEE - COURS D'EAU : calcul de l'état écologique
## Application de l'arrêté de janvier 2010
##############################################
#SAVE()
##############################
## ASSOUPLISSEMENT DE LA PCH
##############################
if (CEPE == "CE") {
TEMPASSOUP1<-merge(PCHFINALEXCEPT_ASSOUP, RLT_BIO, by="STATION")
PCHASSOUP_BIO_NAMES<-names(TEMPASSOUP1) # nom de champ dans un vecteur pour les rappeler ultérieurement

## Données à assouplir : 1er niveau de sélection (BIO et PCH répondent aux critères définis dans l'arrêté)
# Le choix d'assouplir le TBE se fait uniquement au 1er niveau, dans les niv 2 et 3 les requetes ne trouveront rien si le TBE n'est pas dans le 1er niveau !
if (ASSOUPTBE=="oui") {
	TEMPASSOUP_NIV1<-TEMPASSOUP1[(TEMPASSOUP1$ETATPCH==3 & TEMPASSOUP1$ETATBIO %in% c(1,2) & TEMPASSOUP1$no3 %in% c(1,2,NA)) | (TEMPASSOUP1$ETATPCH==2 & TEMPASSOUP1$ETATBIO==1),] 
	} else {
	TEMPASSOUP_NIV1<-TEMPASSOUP1[TEMPASSOUP1$ETATPCH==3 & TEMPASSOUP1$ETATBIO %in% c(1,2) & TEMPASSOUP1$no3 %in% c(1,2,NA),]
}

## Données à assouplir : 2eme niveau de sélection (ELTQUALITE répondent aux critère du 1er niveau)
## RMA : le guide dec2012 ne précise pas si on assouplit si l'état de l'élt de qualité est indéterminé (0) --> dans le doute on n'assouplit pas c'est + cohérent !
## RMA : Désactivation de l'assouplissement sur TEMP et ACID (BE et TBE) car non autorisé dans arrêté 2010 (ça été précisé dans arrêté 2010 révisé).
## la désactivation se fait uniquement sur 2eme Niveau, car script trouvera rien sur 3eme niveau. On garde 3eme niveau en cas d'un retour en arriere.

TEMPASSOUPBE_NIV2c1<-TEMPASSOUP_NIV1[TEMPASSOUP_NIV1$bilano2==3 & TEMPASSOUP_NIV1$nut %in% c(1,2, NA) & TEMPASSOUP_NIV1$temp %in% c(1,2, NA) & TEMPASSOUP_NIV1$acid %in% c(1,2, NA),]
TEMPASSOUPBE_NIV2c2<-TEMPASSOUP_NIV1[TEMPASSOUP_NIV1$bilano2 %in% c(1,2, NA) & TEMPASSOUP_NIV1$nut==3 & TEMPASSOUP_NIV1$temp %in% c(1,2, NA) & TEMPASSOUP_NIV1$acid %in% c(1,2, NA),]
#TEMPASSOUPBE_NIV2c3<-TEMPASSOUP_NIV1[TEMPASSOUP_NIV1$bilano2 %in% c(1,2, NA) & TEMPASSOUP_NIV1$nut %in% c(1,2, NA) & TEMPASSOUP_NIV1$temp==3 & TEMPASSOUP_NIV1$acid %in% c(1,2, NA),]
#TEMPASSOUPBE_NIV2c4<-TEMPASSOUP_NIV1[TEMPASSOUP_NIV1$bilano2 %in% c(1,2, NA) & TEMPASSOUP_NIV1$nut %in% c(1,2, NA) & TEMPASSOUP_NIV1$temp %in% c(1,2, NA) & TEMPASSOUP_NIV1$acid==3,]

TEMPASSOUPTBE_NIV2c1<-TEMPASSOUP_NIV1[TEMPASSOUP_NIV1$bilano2==2 & TEMPASSOUP_NIV1$nut %in% c(1, NA) & TEMPASSOUP_NIV1$temp %in% c(1, NA) & TEMPASSOUP_NIV1$acid %in% c(1, NA),]
TEMPASSOUPTBE_NIV2c2<-TEMPASSOUP_NIV1[TEMPASSOUP_NIV1$bilano2 %in% c(1, NA) & TEMPASSOUP_NIV1$nut==2 & TEMPASSOUP_NIV1$temp %in% c(1, NA) & TEMPASSOUP_NIV1$acid %in% c(1, NA),]
#TEMPASSOUPTBE_NIV2c3<-TEMPASSOUP_NIV1[TEMPASSOUP_NIV1$bilano2 %in% c(1, NA) & TEMPASSOUP_NIV1$nut %in% c(1, NA) & TEMPASSOUP_NIV1$temp==2 & TEMPASSOUP_NIV1$acid %in% c(1, NA),]
#TEMPASSOUPTBE_NIV2c4<-TEMPASSOUP_NIV1[TEMPASSOUP_NIV1$bilano2 %in% c(1, NA) & TEMPASSOUP_NIV1$nut %in% c(1, NA) & TEMPASSOUP_NIV1$temp %in% c(1, NA) & TEMPASSOUP_NIV1$acid==2,]

# Rbind et RM modifiés pour retiré les data.frame des cas de TEMP et ACID
TEMPASSOUP_NIV2<-rbind(TEMPASSOUPBE_NIV2c1, TEMPASSOUPBE_NIV2c2, TEMPASSOUPTBE_NIV2c1, TEMPASSOUPTBE_NIV2c2)
rm(TEMPASSOUPBE_NIV2c1, TEMPASSOUPBE_NIV2c2)
rm(TEMPASSOUPTBE_NIV2c1, TEMPASSOUPTBE_NIV2c2)
gc()

## Calcul du nb de paramètres déclassants par élément de qualité
PCHFINALEXCEPT_ASSOUP<-PCHFINALEXCEPT_ASSOUP[order(PCHFINALEXCEPT_ASSOUP$STATION),] # station triée pour éviter l'erreur avec le split

PCHFINALEXCEPT_ASSOUP$nbdeclassbilano2<-c(lapply(split(PCHFINALEXCEPT_ASSOUP[,c("STATION","ETATPCH",PARAMETREPCH$NOM[PARAMETREPCH$ELTQUALITE == "bilano2"]  )], PCHFINALEXCEPT_ASSOUP$STATION), 
function(x)  length(as.numeric(x[3:ncol(x)])[!is.na(as.numeric(x[3:ncol(x)])) & as.numeric(x[3:ncol(x)]) == as.numeric(x[2])])), recursive=T)

PCHFINALEXCEPT_ASSOUP$nbdeclassnut<-c(lapply(split(PCHFINALEXCEPT_ASSOUP[,c("STATION","ETATPCH",PARAMETREPCH$NOM[PARAMETREPCH$ELTQUALITE == "nut"]  )], PCHFINALEXCEPT_ASSOUP$STATION), 
function(x)  length(as.numeric(x[3:ncol(x)])[!is.na(as.numeric(x[3:ncol(x)])) & as.numeric(x[3:ncol(x)]) == as.numeric(x[2])])), recursive=T)

PCHFINALEXCEPT_ASSOUP$nbdeclasstemp<-c(lapply(split(PCHFINALEXCEPT_ASSOUP[,c("STATION","ETATPCH",PARAMETREPCH$NOM[PARAMETREPCH$ELTQUALITE == "temp"]  )], PCHFINALEXCEPT_ASSOUP$STATION), 
function(x)  length(as.numeric(x[3:ncol(x)])[!is.na(as.numeric(x[3:ncol(x)])) & as.numeric(x[3:ncol(x)]) == as.numeric(x[2])])), recursive=T)

PCHFINALEXCEPT_ASSOUP$nbdeclassacid<-c(lapply(split(PCHFINALEXCEPT_ASSOUP[,c("STATION","ETATPCH",PARAMETREPCH$NOM[PARAMETREPCH$ELTQUALITE == "acid"]  )], PCHFINALEXCEPT_ASSOUP$STATION), 
function(x)  length(as.numeric(x[3:ncol(x)])[!is.na(as.numeric(x[3:ncol(x)])) & as.numeric(x[3:ncol(x)]) == as.numeric(x[2])])), recursive=T)
gc() ## compacte R

## Données à assouplir : 3eme niveau de sélection (PARAMETRES PCH répondent aux critères du 2eme niveau)
TEMPASSOUP_NIV2<-merge(TEMPASSOUP_NIV2, PCHFINALEXCEPT_ASSOUP[,c("STATION", "nbdeclassbilano2", "nbdeclassnut", "nbdeclasstemp", "nbdeclassacid")], by="STATION")
gc()
TEMPASSOUP_NIV3c1<-TEMPASSOUP_NIV2[TEMPASSOUP_NIV2$nbdeclassbilano2==1 & TEMPASSOUP_NIV2$nbdeclassnut %in% c(0, NA) & TEMPASSOUP_NIV2$nbdeclasstemp %in% c(0, NA) & TEMPASSOUP_NIV2$nbdeclassacid %in% c(0, NA),]
TEMPASSOUP_NIV3c2<-TEMPASSOUP_NIV2[TEMPASSOUP_NIV2$nbdeclassbilano2 %in% c(0, NA) & TEMPASSOUP_NIV2$nbdeclassnut==1 & TEMPASSOUP_NIV2$nbdeclasstemp %in% c(0, NA) & TEMPASSOUP_NIV2$nbdeclassacid %in% c(0, NA),]
TEMPASSOUP_NIV3c3<-TEMPASSOUP_NIV2[TEMPASSOUP_NIV2$nbdeclassbilano2 %in% c(0, NA) & TEMPASSOUP_NIV2$nbdeclassnut %in% c(0, NA) & TEMPASSOUP_NIV2$nbdeclasstemp==1 & TEMPASSOUP_NIV2$nbdeclassacid %in% c(0, NA),]
TEMPASSOUP_NIV3c4<-TEMPASSOUP_NIV2[TEMPASSOUP_NIV2$nbdeclassbilano2 %in% c(0, NA) & TEMPASSOUP_NIV2$nbdeclassnut %in% c(0, NA) & TEMPASSOUP_NIV2$nbdeclasstemp %in% c(0, NA) & TEMPASSOUP_NIV2$nbdeclassacid==1,]
#3ème niveau : Cas particulier de l'O2 et SatO2
TEMPASSOUP_NIV3c5<-TEMPASSOUP_NIV2[TEMPASSOUP_NIV2$nbdeclassbilano2==2 & (TEMPASSOUP_NIV2$bilano2==TEMPASSOUP_NIV2$o2) & (TEMPASSOUP_NIV2$bilano2==TEMPASSOUP_NIV2$sato2) & TEMPASSOUP_NIV2$nbdeclassnut %in% c(0, NA) & TEMPASSOUP_NIV2$nbdeclasstemp %in% c(0, NA) & TEMPASSOUP_NIV2$nbdeclassacid %in% c(0, NA),]

TEMPASSOUP_NIV3<-rbind(TEMPASSOUP_NIV3c1, TEMPASSOUP_NIV3c2, TEMPASSOUP_NIV3c3, TEMPASSOUP_NIV3c4, TEMPASSOUP_NIV3c5)
rm(TEMPASSOUP_NIV3c1, TEMPASSOUP_NIV3c2, TEMPASSOUP_NIV3c3, TEMPASSOUP_NIV3c4, TEMPASSOUP_NIV3c5)
gc()
## Assouplissement des valeurs
TEMPASSOUPFINAL<-TEMPASSOUP_NIV3[,c("STATION", "ETATPCH", "bilano2", "nut", "temp", "acid")]
names(TEMPASSOUPFINAL)[2]<-"ETATPCHassoup"
names(TEMPASSOUPFINAL)[3]<-"bilano2assoup"
names(TEMPASSOUPFINAL)[4]<-"nutassoup"
names(TEMPASSOUPFINAL)[5]<-"tempassoup"
names(TEMPASSOUPFINAL)[6]<-"acidassoup"

condBO2<-TEMPASSOUPFINAL$bilano2assoup==TEMPASSOUPFINAL$ETATPCHassoup & !is.na(TEMPASSOUPFINAL$bilano2assoup)
condNUT<-TEMPASSOUPFINAL$nutassoup==TEMPASSOUPFINAL$ETATPCHassoup & !is.na(TEMPASSOUPFINAL$nutassoup)
condTEMP<-TEMPASSOUPFINAL$tempassoup==TEMPASSOUPFINAL$ETATPCHassoup & !is.na(TEMPASSOUPFINAL$tempassoup)
condACID<-TEMPASSOUPFINAL$acidassoup==TEMPASSOUPFINAL$ETATPCHassoup & !is.na(TEMPASSOUPFINAL$acidassoup)

TEMPASSOUPFINAL$bilano2assoup[condBO2]<-TEMPASSOUPFINAL$bilano2assoup[condBO2]-1
TEMPASSOUPFINAL$nutassoup[condNUT]<-TEMPASSOUPFINAL$nutassoup[condNUT]-1
TEMPASSOUPFINAL$tempassoup[condTEMP]<-TEMPASSOUPFINAL$tempassoup[condTEMP]-1
TEMPASSOUPFINAL$acidassoup[condACID]<-TEMPASSOUPFINAL$acidassoup[condACID]-1
TEMPASSOUPFINAL$ETATPCHassoup<-TEMPASSOUPFINAL$ETATPCHassoup-1
rm(condBO2, condNUT, condTEMP, condACID)
gc()

PCHFINALEXCEPT_ASSOUP<-merge(PCHFINALEXCEPT_ASSOUP, TEMPASSOUPFINAL, by="STATION", all.x=TRUE)
gc()
PCHFINALEXCEPT_ASSOUP$ETATPCH[!is.na(PCHFINALEXCEPT_ASSOUP$ETATPCHassoup)]<-PCHFINALEXCEPT_ASSOUP$ETATPCHassoup[!is.na(PCHFINALEXCEPT_ASSOUP$ETATPCHassoup)]
PCHFINALEXCEPT_ASSOUP$bilano2[!is.na(PCHFINALEXCEPT_ASSOUP$bilano2assoup)]<-PCHFINALEXCEPT_ASSOUP$bilano2assoup[!is.na(PCHFINALEXCEPT_ASSOUP$bilano2assoup)]
PCHFINALEXCEPT_ASSOUP$nut[!is.na(PCHFINALEXCEPT_ASSOUP$nutassoup)]<-PCHFINALEXCEPT_ASSOUP$nutassoup[!is.na(PCHFINALEXCEPT_ASSOUP$nutassoup)]
PCHFINALEXCEPT_ASSOUP$temp[!is.na(PCHFINALEXCEPT_ASSOUP$tempassoup)]<-PCHFINALEXCEPT_ASSOUP$tempassoup[!is.na(PCHFINALEXCEPT_ASSOUP$tempassoup)]
PCHFINALEXCEPT_ASSOUP$acid[!is.na(PCHFINALEXCEPT_ASSOUP$acidassoup)]<-PCHFINALEXCEPT_ASSOUP$acidassoup[!is.na(PCHFINALEXCEPT_ASSOUP$acidassoup)]
PCHFINALEXCEPT_ASSOUP$ASSOUPLI<-"non"
PCHFINALEXCEPT_ASSOUP$ASSOUPLI[!is.na(PCHFINALEXCEPT_ASSOUP$ETATPCHassoup)]<-"oui"

rm(TEMPASSOUP1, TEMPASSOUP_NIV1,TEMPASSOUP_NIV2,TEMPASSOUP_NIV3)
gc() ## compacte R
flush.console()

##############################
# CALCUL ETAT ECOLO - STATION
##############################
EECOLO<-merge(PCHFINALEXCEPT_ASSOUP, RLT_BIO, by="STATION", all.x=TRUE, all.y=TRUE)
}

if (CEPE == "PE") {
EECOLO<-merge(RLT_PCHFINAL, RLT_BIO, by="STATION", all.x=TRUE, all.y=TRUE)
PCHASSOUP_BIO_NAMES<-names(EECOLO)
}
## Fonction pour le calcul de l'état écologique hors polluant spécifique
## fonction marche uniquement parce qu'il n'y a pas de tri dans les vecteurs
# nota : ETABIO (valeur possible) --> NA, 1 à 5  (le 0 n'existe pas)
# nota : ETATPCH (valeur possible) --> NA, 0 à 5
FCTEECOLOHPS<- function (VARBIO=ECOLO$ETATBIO, VARPCH=ECOLO$ETATPCH) {
    #création de la colonne de résultat pour maitriser le format de sortie
	VARECOLOHPS<-as.numeric(rep(NA,length(VARBIO)))
	# Etat écolo pas bon (hors polluant spécifique)
	VARECOLOHPS[VARBIO>=3 & !is.na(VARBIO)]<-VARBIO[VARBIO>=3 & !is.na(VARBIO)]
	VARECOLOHPS[VARBIO<=2 & VARPCH>=3 & !is.na(VARBIO) & !is.na(VARPCH)]<-3
	#VARECOLOHPS[VARPCH>=3 & is.na(VARBIO) & !is.na(VARPCH)]<-3 # plus en J, mais en indéterminé

	# Etat ECOLO non qualifié (hors polluant spécifique)
	VARECOLOHPS[VARPCH<=2 & is.na(VARBIO) & !is.na(VARPCH)]<-0
	VARECOLOHPS[VARBIO<=2 & is.na(VARPCH) & !is.na(VARBIO)]<-0
	VARECOLOHPS[VARBIO<=2 & VARPCH==0 & !is.na(VARBIO) &!is.na(VARPCH)]<-0
	VARECOLOHPS[is.na(VARBIO) & is.na(VARPCH) ]<-0
	VARECOLOHPS[VARPCH>=3 & is.na(VARBIO) & !is.na(VARPCH)]<-0
	
	# Etat ECOLO bon (hors polluant spécifique)
 	condBE<-VARBIO<=2 & VARPCH %in% c(1,2) &!is.na(VARBIO) &!is.na(VARPCH)
	VARECOLOHPS[condBE]<-pmax(VARBIO[condBE], VARPCH[condBE], na.rm=TRUE)
    VARECOLOHPS
}

# fonction aggrégation spécifique AESN
# nota : ETABIO (valeur possible) --> NA, 1 à 5  (le 0 n'existe pas)
# nota : ETATPCH (valeur possible) --> NA, 0 à 5
FCTEECOLOHPS_AESN<- function (VARBIO=ECOLO$ETATBIO, VARPCH=ECOLO$ETATPCH) {
    #création de la colonne de résultat pour maitriser le format de sortie
	VARECOLOHPS<-as.numeric(rep(NA,length(VARBIO)))
	# Etat écolo pas bon (hors polluant spécifique)
	VARECOLOHPS[VARBIO>=3 & !is.na(VARBIO)]<-VARBIO[VARBIO>=3 & !is.na(VARBIO)]
	VARECOLOHPS[VARBIO<=2 & VARPCH>=3 & !is.na(VARBIO) & !is.na(VARPCH)]<-3
	VARECOLOHPS[VARPCH %in% c(3,4,5) & is.na(VARBIO) & !is.na(VARPCH)]<-VARPCH[VARPCH %in% c(3,4,5) & is.na(VARBIO) & !is.na(VARPCH)]
	
	# Etat ECOLO non qualifié (hors polluant spécifique)
	VARECOLOHPS[VARPCH==0 & is.na(VARBIO) & !is.na(VARPCH)]<-0
	VARECOLOHPS[is.na(VARBIO) & is.na(VARPCH) ]<-0
	
	# Etat ECOLO bon (hors polluant spécifique)
 	VARECOLOHPS[VARPCH %in% c(1,2) & is.na(VARBIO) & !is.na(VARPCH)]<-2
	VARECOLOHPS[VARBIO<=2 & VARPCH==0 & !is.na(VARBIO) &!is.na(VARPCH)]<-VARBIO[VARBIO<=2 & VARPCH==0 & !is.na(VARBIO) &!is.na(VARPCH)]
	VARECOLOHPS[VARBIO<=2 & is.na(VARPCH) & !is.na(VARBIO)]<-VARBIO[VARBIO<=2 & is.na(VARPCH) & !is.na(VARBIO)]
	
	condBE<-VARBIO<=2 & VARPCH %in% c(1,2) &!is.na(VARBIO) &!is.na(VARPCH)
	VARECOLOHPS[condBE]<-pmax(VARBIO[condBE], VARPCH[condBE], na.rm=TRUE)
    VARECOLOHPS
}

## Fonction pour le calcul de l'état écologique avec polluant spécifique
## fonction marche uniquement parce qu'il n'y a pas de tri dans les vecteurs
FCTEECOLO<- function (VARECOLOHPS=EECOLO$ETATECOLOHPS, VARPS=EECOLO$ETATPS) {
	#création de la colonne de résultat pour maitriser le format de sortie
	VARECOLO<-as.numeric(rep(NA,length(VARECOLOHPS)))
	#algo de calcul
	VARECOLO[VARECOLOHPS==2 & VARPS==3 & !is.na(VARPS)]<-3
	VARECOLO[VARECOLOHPS==2 & VARPS<=2 & !is.na(VARPS)]<-2
	VARECOLO[VARECOLOHPS==2 & is.na(VARPS)]<-2
	VARECOLO[VARECOLOHPS %in% c(0,1,3,4,5)]<-VARECOLOHPS[VARECOLOHPS %in% c(0,1,3,4,5)]
	VARECOLO
}

# fonction aggrégation spécifique AESN
FCTEECOLO_AESN<- function (VARECOLOHPS=EECOLO$ETATECOLOHPS, VARPS=EECOLO$ETATPS) {
	#création de la colonne de résultat pour maitriser le format de sortie
	VARECOLO<-as.numeric(rep(NA,length(VARECOLOHPS)))
	#algo de calcul
	VARECOLO[VARECOLOHPS %in% c(0,1,2) & VARPS==3 & !is.na(VARECOLOHPS) & !is.na(VARPS)]<-3
	VARECOLO[VARECOLOHPS %in% c(1,2) & VARPS==2 & !is.na(VARECOLOHPS) & !is.na(VARPS)]<-2
	VARECOLO[VARECOLOHPS==0 & VARPS==2 & !is.na(VARECOLOHPS) & !is.na(VARPS)]<-0
	VARECOLO[VARECOLOHPS %in% c(1,2) & (is.na(VARPS) | VARPS==0) & !is.na(VARECOLOHPS)]<-VARECOLOHPS[VARECOLOHPS %in% c(1,2) & (is.na(VARPS) | VARPS==0) & !is.na(VARECOLOHPS)]
	VARECOLO[VARECOLOHPS==0 & (is.na(VARPS) | VARPS==0) & !is.na(VARECOLOHPS)]<-0
		
	VARECOLO[VARECOLOHPS %in% c(3,4,5)]<-VARECOLOHPS[VARECOLOHPS %in% c(3,4,5)]
	VARECOLO
}

## Etat ECOLO hors polluants spécifiques (appel de la fonction)
if (BASSIN=="AESN") {
	EECOLO$ETATECOLOHPS<-FCTEECOLOHPS_AESN(VARBIO=EECOLO$ETATBIO, VARPCH=EECOLO$ETATPCH)
} else {
	EECOLO$ETATECOLOHPS<-FCTEECOLOHPS(VARBIO=EECOLO$ETATBIO, VARPCH=EECOLO$ETATPCH)
}
gc()

## Pour Plan d'eau, Etat ECOLO hors polluants spécifiques et hors bilanO2
if(CEPE == "PE"){
	if (BASSIN=="AESN") {
		EECOLO$ETATECOLOHPSetBO2<-FCTEECOLOHPS_AESN(VARBIO=EECOLO$ETATBIO, VARPCH=EECOLO$ETATPCH_SSBILANO2)
	} else {
		EECOLO$ETATECOLOHPSetBO2<-FCTEECOLOHPS(VARBIO=EECOLO$ETATBIO, VARPCH=EECOLO$ETATPCH_SSBILANO2)
	}
}
gc()

## Etat ECOLO avec polluants spécifiques (appel de la fonction)
if (SEEEPS=="oui" & BASSIN!="AESN" ) {
	EECOLO<-merge(EECOLO, RLT_POLSPE[,c("STATION", ETATPS_NAMES)], by="STATION", all.x=TRUE)
	EECOLO$ETATECOLO<-FCTEECOLO(VARECOLOHPS=EECOLO$ETATECOLOHPS, VARPS=EECOLO$ETATPS)
} else if (SEEEPS=="oui" & BASSIN=="AESN" ) {
	EECOLO<-merge(EECOLO, RLT_POLSPE[,c("STATION", ETATPS_NAMES)], by="STATION", all.x=TRUE)
	EECOLO$ETATECOLO<-FCTEECOLO_AESN(VARECOLOHPS=EECOLO$ETATECOLOHPS, VARPS=EECOLO$ETATPS)
} else {
	EECOLO$ETATECOLO<-EECOLO$ETATECOLOHPS
}
gc()

## Pour Plan d'eau, Etat ECOLO avec polluants spécifiques et hors bilanO2
if(CEPE == "PE"){
	if (SEEEPS=="oui" & BASSIN!="AESN" ) {
		EECOLO$ETATECOLOHBO2<-FCTEECOLO(VARECOLOHPS=EECOLO$ETATECOLOHPSetBO2, VARPS=EECOLO$ETATPS)
	} else if (SEEEPS=="oui" & BASSIN=="AESN" ) {
		EECOLO$ETATECOLOHBO2<-FCTEECOLO_AESN(VARECOLOHPS=EECOLO$ETATECOLOHPSetBO2, VARPS=EECOLO$ETATPS)
	} else {
		EECOLO$ETATECOLOHBO2<-EECOLO$ETATECOLOHPSetBO2
	}
}
gc()

## Mise en forme table finale
PCHASSOUP_BIO_NAMES<-c(PCHASSOUP_BIO_NAMES, ASSOUPLI_NAMES, ETATECOLOHPS_NAMES, ETATPS_NAMES, ETATECOLO_NAMES)
EECOLO<-EECOLO[,PCHASSOUP_BIO_NAMES]
flush.console()
	
###################################
# CALCUL ETAT ECOLO - MASSE D'EAU
###################################
if (SEEEECOLOME=="oui" & CEPE == "CE") {
	TEMPEECOLOME<-merge(EECOLO[,c("STATION", "ETATPCH", "ETATBIO", "IBD", ETATPS_NAMES, "ETATECOLO", "ASSOUPLI")], STATION[,c("STATION", "EUCD", "REPRESENTATIVE")], by="STATION", all.x=TRUE)
	TEMPEECOLOME<-merge(TEMPEECOLOME, MASSEDEAU[,c("EUCD", "TYPEME")], by="EUCD")
	#TEMPEECOLOME<-TEMPEECOLOME[TEMPEECOLOME$EUCD == "FRHR_T05-I8161000",]
## Préparation des données pour calcul l'état ecolo en tenant compte du cas MEFM/MEA
	TEMPEECOLOME$ETATBIOME[TEMPEECOLOME$TYPEME=="MEN"]<-TEMPEECOLOME$ETATBIO[TEMPEECOLOME$TYPEME=="MEN"]
	TEMPEECOLOME$ETATBIOME[!(TEMPEECOLOME$TYPEME=="MEN")]<-TEMPEECOLOME$IBD[!(TEMPEECOLOME$TYPEME=="MEN")]
	TEMPEECOLOME$ETATPCHME<-TEMPEECOLOME$ETATPCH #data identique entre analyse à la station et ME, mais c'est pour etre cohérent avec la bio
	if (SEEEPS=="oui") {
		TEMPEECOLOME$ETATPSME<-TEMPEECOLOME$ETATPS #data identique entre analyse à la station et ME, mais c'est pour etre cohérent avec la bio
	}

## Etat ECOLO hors polluants spécifiques (appel de la fonction)
	if (BASSIN=="AESN") {
		TEMPEECOLOME$ETATECOLOHPSME<-FCTEECOLOHPS_AESN(VARBIO=TEMPEECOLOME$ETATBIOME, VARPCH=TEMPEECOLOME$ETATPCHME)
	} else {
		TEMPEECOLOME$ETATECOLOHPSME<-FCTEECOLOHPS(VARBIO=TEMPEECOLOME$ETATBIOME, VARPCH=TEMPEECOLOME$ETATPCHME)
	}
		
## Etat ECOLO avec polluants spécifiques (appel de la fonction)
	if (SEEEPS=="oui" & BASSIN!="AESN" ) {
		TEMPEECOLOME$ETATECOLOME<-FCTEECOLO(VARECOLOHPS=TEMPEECOLOME$ETATECOLOHPSME, VARPS=TEMPEECOLOME$ETATPSME)
	} else if (SEEEPS=="oui" & BASSIN=="AESN" ) {
		TEMPEECOLOME$ETATECOLOME<-FCTEECOLO_AESN(VARECOLOHPS=TEMPEECOLOME$ETATECOLOHPSME, VARPS=TEMPEECOLOME$ETATPSME)
	} else {
		TEMPEECOLOME$ETATECOLOME<-TEMPEECOLOME$ETATECOLOHPSME
	}
	
## Extrapolation à la ME via la station représentative ou temporairement représentative
	EECOLOME_REP<-aggregate(ETATECOLOME ~ EUCD + REPRESENTATIVE, data = TEMPEECOLOME[TEMPEECOLOME$REPRESENTATIVE=="oui",], max)

	# traitement si présence de station temporairement représentative dans les données
	EECOLOME_REPTEMPO1<-TEMPEECOLOME[TEMPEECOLOME$REPRESENTATIVE=="temporaire" & !(TEMPEECOLOME$EUCD %in% c(EECOLOME_REP$EUCD)),]
	if (nrow(EECOLOME_REPTEMPO1) > 0) {
		EECOLOME_REPTEMPO2<-aggregate(ETATECOLOME ~ EUCD + REPRESENTATIVE, data = EECOLOME_REPTEMPO1, max)
		EECOLOME<-rbind(EECOLOME_REP,EECOLOME_REPTEMPO2)
		rm(EECOLOME_REP, EECOLOME_REPTEMPO1,EECOLOME_REPTEMPO2)
	} else {
		EECOLOME<-EECOLOME_REP
		rm(EECOLOME_REP)
	}
	EECOLOME_NAMES<-names(EECOLOME) #utile pour rappel ultérieur des noms de champs
gc()

## Mise en forme tableau final
	EECOLOME$ID<-paste (EECOLOME$EUCD, "EE",EECOLOME$ETATECOLOME, EECOLOME$REPRESENTATIVE, sep="")
	TEMPEECOLOME$ID<-paste (TEMPEECOLOME$EUCD, "EE",TEMPEECOLOME$ETATECOLOME, TEMPEECOLOME$REPRESENTATIVE, sep="")

	TEMPEECOLOME_NAMES<-names(TEMPEECOLOME[,c("STATION", "ETATBIOME", "ETATPCHME", ETATPSME_NAMES, "ETATECOLOHPSME", "TYPEME")])
	EECOLOME<-merge(EECOLOME, TEMPEECOLOME[,c(TEMPEECOLOME_NAMES, "ID")], by="ID")
	EECOLOME<-EECOLOME[,c(EECOLOME_NAMES, TEMPEECOLOME_NAMES)]
	rm(TEMPEECOLOME,TEMPEECOLOME_NAMES)
	EECOLOME_NAMES<-names(EECOLOME) #utile pour rappel ultérieur des noms de champs
	flush.console()

###################################
# CALCUL NIVEAU DE CONFIANCE
###################################
	if (!(BASSIN=="AELB")) {
		NIVCONF<-merge(EECOLOME, EECOLO,by="STATION")
	} else {
		NIVCONF<-merge(EECOLOME, EECOLO,by="STATION")
		NIVCONF<-merge(NIVCONF, MASSEDEAU[,c("EUCD", "RGLOBAL", "RMACRO", "RNIT", "RPEST", "RMICRO", "RMORPHO", "RHYDRO")],by="EUCD")
	}
	NIVCONF$NIVEAU<-as.numeric(NA)
	NIVCONF$CAS<-as.character(NA)

# Calcul des indicateurs GLOBAUX nécessaires au calcul du niveau de confiance
	# IBG ou IBGA est disponible
	if (nrow(PARAMETREBIO[PARAMETREBIO$PARAGROUP=="6951",])>0) {
		NIVCONF$IBGIBGA[!is.na(NIVCONF$IBG) | !is.na(NIVCONF$IBGA)]<-"dispo"
		NIVCONF$IBGIBGA[is.na(NIVCONF$IBG) & is.na(NIVCONF$IBGA)]<-"non dispo"
	} else {
		NIVCONF$IBGIBGA[!is.na(NIVCONF$IBG)]<-"dispo"
		NIVCONF$IBGIBGA[is.na(NIVCONF$IBG)]<-"non dispo"
	}

	# détermination de la robustesse : écart entre l'état bio et état PCH à la masse d'eau --> pour éviter le quid des MEFM/MEA pour la BIO calculer uniquement avec IBD!!
	NIVCONF$ROBUSTESSE[is.na(NIVCONF$ETATBIOME) | is.na(NIVCONF$ETATPCHME)]<-as.numeric(9)
	condROB<-!(is.na(NIVCONF$ETATBIOME)) & !(is.na(NIVCONF$ETATPCHME))
	NIVCONF$ROBUSTESSE[condROB]<-abs(NIVCONF$ETATBIOME[condROB]-NIVCONF$ETATPCHME[condROB])
	rm(condROB)
gc()

# Calcul des indicateurs BASSINS nécessaires au calcul du niveau de confiance
	if (!(BASSIN=="AELB")) {
	# au moins 1 végétal est disponible
		NIVCONF$VEGETAL[!(is.na(NIVCONF$IBD)) | !(is.na(NIVCONF$IBMR))]<-"oui"
		NIVCONF$VEGETAL[is.na(NIVCONF$IBD) & is.na(NIVCONF$IBMR)]<-"non"

	# au moins 1 animal est disponible
		NIVCONF$ANIMAL[NIVCONF$IBGIBGA=="dispo" | !(is.na(NIVCONF$IPR))]<-"oui"
		NIVCONF$ANIMAL[NIVCONF$IBGIBGA=="non dispo" & is.na(NIVCONF$IPR)]<-"non"

	# ANCIEN ALGO - détermination de la robustesse : écart entre l'état bio et état PCH à la STATION ! quid des MEFM/MEA pour la BIO calculer uniquement avec IBD!!
		#NIVCONF$ROBUSTESSE[is.na(NIVCONF$ETATBIO) | is.na(NIVCONF$ETATPCH)]<-as.numeric(9)
		#condROB<-!(is.na(NIVCONF$ETATBIO)) & !(is.na(NIVCONF$ETATPCH))
		#NIVCONF$ROBUSTESSE[condROB]<-abs(NIVCONF$ETATBIO[condROB]-NIVCONF$ETATPCH[condROB])
		#rm(condROB)
	} else {
	# tous les éléments bio sont disponibles
		NIVCONF$BIOALL[NIVCONF$NBINDICBIO>=3 & NIVCONF$TYPEME=="MEN"]<-"oui"
		NIVCONF$BIOALL[NIVCONF$NBINDICBIO<3 & NIVCONF$TYPEME=="MEN"]<-"non"
		NIVCONF$BIOALL[is.na(NIVCONF$IBD) & !(NIVCONF$TYPEME=="MEN")]<-"non"
		NIVCONF$BIOALL[!is.na(NIVCONF$IBD) & !(NIVCONF$TYPEME=="MEN")]<-"oui"
		#NIVCONF$BIOALL[((is.na(NIVCONF$IBD) | NIVCONF$IBGIBGA=="non dispo" | is.na(NIVCONF$IBMR) | is.na(NIVCONF$IPR)) & NIVCONF$TYPEME=="MEN") | (is.na(NIVCONF$IBD) & !(NIVCONF$TYPEME=="MEN"))]<-"non"

	# cohérence données milieux et pression (risque global = état écolo ME)
		NIVCONF$COHERENCE[(NIVCONF$RGLOBAL %in% (-1:0) & NIVCONF$ETATECOLOME %in% (3:5)) | (NIVCONF$RGLOBAL==1 & NIVCONF$ETATECOLOME %in% (1:2))]<-"coherent"
		NIVCONF$COHERENCE[(NIVCONF$RGLOBAL %in% (-1:0) & NIVCONF$ETATECOLOME %in% (1:2)) | (NIVCONF$RGLOBAL==1 & NIVCONF$ETATECOLOME %in% (3:5))]<-"incoherent"
		NIVCONF$COHERENCE[NIVCONF$ETATECOLOME==0]<-"NQ"

	# Données sensibles disponibles si risque MACRO
		condRMACRO1<-!is.na(NIVCONF$ETATPCHME) & NIVCONF$IBGIBGA=="dispo" & NIVCONF$RMACRO %in% (-1:0)
		condRMACRO2<-!is.na(NIVCONF$ETATPCHME) & !is.na(NIVCONF$IBD)  & NIVCONF$RMACRO %in% (-1:0)
		condRMACRO3<-!is.na(NIVCONF$ETATPCHME) & !is.na(NIVCONF$IBMR)  & NIVCONF$RMACRO %in% (-1:0)
		NIVCONF$DATAMACRO[condRMACRO1 | condRMACRO2 | condRMACRO3]<-"1" # données dispo
		NIVCONF$DATAMACRO[!(condRMACRO1 | condRMACRO2 | condRMACRO3)]<-"2" # données non dispo
		NIVCONF$DATAMACRO[NIVCONF$RMACRO=="1"]<-"0" # nécessaire de mettre cette ligne en dernier pour éviter d'avoir le (respect) en non dispo --> traité à cause du ! (cas inverse)
		rm(condRMACRO1, condRMACRO2,condRMACRO3)

	# Données sensibles disponibles si risque NITRATE
		condRNIT1<-!is.na(NIVCONF$ETATPCHME) & !is.na(NIVCONF$no3) & NIVCONF$IBGIBGA=="dispo" & NIVCONF$RNIT %in% (-1:0)
		condRNIT2<-!is.na(NIVCONF$ETATPCHME) & !is.na(NIVCONF$no3) & !is.na(NIVCONF$IBD)  & NIVCONF$RNIT %in% (-1:0)
		NIVCONF$DATANIT[condRNIT1 | condRNIT2]<-"1" # données dispo
		NIVCONF$DATANIT[!(condRNIT1 | condRNIT2)]<-"2" # données non dispo
		NIVCONF$DATANIT[NIVCONF$RNIT=="1"]<-"0" # nécessaire de mettre cette ligne en dernier pour éviter d'avoir le (respect) en non dispo --> traité à cause du ! (cas inverse)
		rm(condRNIT1, condRNIT2)

	# Données sensibles disponibles si risque PEST
		condRPEST1<- NIVCONF$IBGIBGA=="dispo" & NIVCONF$RPEST %in% (-1:0)
		condRPEST2<- !is.na(NIVCONF$IBD)  & NIVCONF$RPEST %in% (-1:0)
		NIVCONF$DATAPEST[condRPEST1 | condRPEST2]<-"1" # données dispo
		NIVCONF$DATAPEST[!(condRPEST1 | condRPEST2)]<-"2" # données non dispo
		NIVCONF$DATAPEST[NIVCONF$RPEST=="1"]<-"0" # nécessaire de mettre cette ligne en dernier pour éviter d'avoir le (respect) en non dispo --> traité à cause du ! (cas inverse)
		rm(condRPEST1, condRPEST2)

	# Données sensibles disponibles si risque MORPHO
		condRMORPHO<- !is.na(NIVCONF$IPR) & NIVCONF$RMORPHO %in% (-1:0)
		NIVCONF$DATAMORPHO[condRMORPHO]<-"1" # données dispo
		NIVCONF$DATAMORPHO[!condRMORPHO]<-"2" # données non dispo
		NIVCONF$DATAMORPHO[NIVCONF$RMORPHO %in% (1:2)]<-"0" # nécessaire de mettre cette ligne en dernier pour éviter d'avoir le (respect) en non dispo --> traité à cause du ! (cas inverse)
		rm(condRMORPHO)

	# Données sensibles disponibles si risque HYDRO
		condRHYDRO<- !is.na(NIVCONF$IPR) & NIVCONF$RHYDRO %in% (-1:0)
		NIVCONF$DATAHYDRO[condRHYDRO]<-"1" # données dispo
		NIVCONF$DATAHYDRO[!condRHYDRO]<-"2" # données non dispo
		NIVCONF$DATAHYDRO[NIVCONF$RHYDRO %in% (1:2)]<-"0" # nécessaire de mettre cette ligne en dernier pour éviter d'avoir le(respect) en non dispo --> traité à cause du ! (cas inverse)
		rm(condRHYDRO)

	# Synthèse des données sensibles disponibles 
		NIVCONF$DATARISK<-paste(NIVCONF$DATAMACRO, NIVCONF$DATANIT, NIVCONF$DATAPEST, NIVCONF$DATAMORPHO, NIVCONF$DATAHYDRO, sep="")  # 11111 = données sensibles tjs présentes ; 00000 = ME en respect ; 2 dans une serie = data non présente
		NIVCONF$DATASENSIBLE[(regexpr("2",NIVCONF$DATARISK))>=1]<-"non dispo" #regpexr indique la position du caractere recherché (ici le 2), si -1 = caractere n'est pas dans la chaine.
		NIVCONF$DATASENSIBLE[(regexpr("1",NIVCONF$DATARISK))>=1 & is.na(NIVCONF$DATASENSIBLE)]<-"dispo" # is.na est nécessaire pour éviter d'écraser la condition précédente
		NIVCONF$DATASENSIBLE[NIVCONF$DATARISK=="00000"]<-"nc"
	}
gc()

# Calcul du niveau de confiance
	if (BASSIN %in% c("AEAG", "AERMC", "AERM", "AEAP", "ODE971", "ODE972", "ODE973", "ODE974", "ODE976")) {
		condQ1<-(NIVCONF$VEGETAL=="oui" & NIVCONF$ANIMAL=="oui" & NIVCONF$ROBUSTESSE<=1 & NIVCONF$TYPEME=="MEN") | (NIVCONF$VEGETAL=="oui" & !(NIVCONF$TYPEME=="MEN") & NIVCONF$ROBUSTESSE<=1) 
		condQ2<-(NIVCONF$VEGETAL=="oui" & NIVCONF$ANIMAL=="oui" & NIVCONF$ROBUSTESSE>1 & NIVCONF$TYPEME=="MEN") | (NIVCONF$VEGETAL=="oui" & !(NIVCONF$TYPEME=="MEN") & NIVCONF$ROBUSTESSE>1)
		condQ3<-!(NIVCONF$VEGETAL=="oui" & NIVCONF$ANIMAL=="oui") & NIVCONF$ROBUSTESSE<=1 & NIVCONF$TYPEME=="MEN" # cas impossible pour MEFM, c'est directement Q5
		condQ4<-!(NIVCONF$VEGETAL=="oui" & NIVCONF$ANIMAL=="oui") & NIVCONF$ROBUSTESSE>1 & NIVCONF$TYPEME=="MEN" # cas impossible pour MEFM, c'est directement Q5
		condQ5<-(NIVCONF$VEGETAL=="non" & NIVCONF$ANIMAL=="non" & NIVCONF$TYPEME=="MEN") | (NIVCONF$VEGETAL=="non" & !(NIVCONF$TYPEME=="MEN")) # le cas non/non est traitée en Q3 et Q4, mais corrigé en Q5 pour simplifier l'écriture des conditions Q3 et Q4 (ça évite d'écrire les combinaisons oui/non)
		condQ20<-(NIVCONF$ETATECOLOME==0)

		NIVCONF$NIVEAU[condQ1]<-3
		NIVCONF$CAS[condQ1]<-"Q1"
		NIVCONF$NIVEAU[condQ2]<-2
		NIVCONF$CAS[condQ2]<-"Q2"
		NIVCONF$NIVEAU[condQ3]<-2
		NIVCONF$CAS[condQ3]<-"Q3"
		NIVCONF$NIVEAU[condQ4]<-2
		NIVCONF$CAS[condQ4]<-"Q4"
		NIVCONF$NIVEAU[condQ5]<-1
		NIVCONF$CAS[condQ5]<-"Q5"
		NIVCONF$NIVEAU[condQ20]<-0
		NIVCONF$CAS[condQ20]<-"Q20"
		rm(condQ1,condQ2,condQ3,condQ4,condQ5,condQ20)
	} else if (BASSIN=="AESN") {
		condQ1<-(NIVCONF$MODELISE=="non" & NIVCONF$ETATBIOME>0 & NIVCONF$ETATPCHME>0 & NIVCONF$ETATPSME>0 & NIVCONF$NBINDICBIO>=2 & NIVCONF$TYPEME =="MEN" & !is.na(NIVCONF$ETATBIOME) & !is.na(NIVCONF$ETATPCHME) & !is.na(NIVCONF$ETATPSME) & !is.na(NIVCONF$NBINDICBIO) ) 
		condQ1MEFM<-(NIVCONF$MODELISE=="non" & NIVCONF$IBD>0 & NIVCONF$ETATPCHME>0 & NIVCONF$ETATPSME>0 & NIVCONF$TYPEME !="MEN" & !is.na(NIVCONF$IBD) & !is.na(NIVCONF$ETATPCHME) & !is.na(NIVCONF$ETATPSME) ) 
		condQ2<-(NIVCONF$MODELISE=="non" & NIVCONF$ETATBIOME>0 & NIVCONF$ETATPCHME>0 & NIVCONF$ETATPSME>0 & NIVCONF$NBINDICBIO==1 & NIVCONF$TYPEME =="MEN" & !is.na(NIVCONF$ETATBIOME) & !is.na(NIVCONF$ETATPCHME) & !is.na(NIVCONF$ETATPSME) & !is.na(NIVCONF$NBINDICBIO) )  # cas impossible pour MEFM
		condQ3<-(NIVCONF$MODELISE=="non" & NIVCONF$ETATBIOME>0 & NIVCONF$ETATPCHME>0 & (NIVCONF$ETATPSME ==0 | is.na(NIVCONF$ETATPSME)) & NIVCONF$TYPEME =="MEN" & !is.na(NIVCONF$ETATBIOME) & !is.na(NIVCONF$ETATPCHME) ) 
		condQ3MEFM<-(NIVCONF$MODELISE=="non" & NIVCONF$IBD>0 & NIVCONF$ETATPCHME>0 & (NIVCONF$ETATPSME ==0 | is.na(NIVCONF$ETATPSME)) & NIVCONF$TYPEME !="MEN" & !is.na(NIVCONF$IBD) & !is.na(NIVCONF$ETATPCHME) ) 
		condQ4<-(NIVCONF$MODELISE=="non" & (NIVCONF$ETATBIOME ==0 | is.na(NIVCONF$ETATBIOME)) )  # distinction MEFM pas utile
		condQ5<-(NIVCONF$MODELISE=="non" & NIVCONF$ETATBIOME>0 & (NIVCONF$ETATPCHME ==0 | is.na(NIVCONF$ETATPCHME)) & NIVCONF$NBINDICBIO>=2 & NIVCONF$TYPEME =="MEN" & !is.na(NIVCONF$ETATBIOME) ) 
		condQ5MEFM<-(NIVCONF$MODELISE=="non" & NIVCONF$IBD>0 & (NIVCONF$ETATPCHME ==0 | is.na(NIVCONF$ETATPCHME)) & NIVCONF$TYPEME !="MEN" & !is.na(NIVCONF$IBD) )
		condQ6<-((NIVCONF$MODELISE=="non" | is.na(NIVCONF$MODELISE)) & NIVCONF$ETATBIOME>0 & (NIVCONF$ETATPCHME ==0 | is.na(NIVCONF$ETATPCHME)) & NIVCONF$NBINDICBIO==1 & NIVCONF$TYPEME =="MEN" & !is.na(NIVCONF$ETATBIOME) ) # cas impossible pour MEFM
		condQ7<-(NIVCONF$MODELISE=="oui" & NIVCONF$ETATBIOME >0 & !is.na(NIVCONF$ETATBIOME) & NIVCONF$TYPEME =="MEN" ) 
		condQ7MEFM<-(NIVCONF$MODELISE=="oui" & NIVCONF$IBD >0 & !is.na(NIVCONF$IBD) & NIVCONF$TYPEME !="MEN" )
		condQ8<-(NIVCONF$MODELISE=="oui" & (NIVCONF$ETATBIOME ==0 | is.na(NIVCONF$ETATBIOME)))
		
		NIVCONF$NIVEAU[condQ1]<-3
		NIVCONF$CAS[condQ1]<-"Q1"
		NIVCONF$NIVEAU[condQ1MEFM]<-3
		NIVCONF$CAS[condQ1MEFM]<-"Q1"
		NIVCONF$NIVEAU[condQ2]<-2
		NIVCONF$CAS[condQ2]<-"Q2"
		NIVCONF$NIVEAU[condQ3]<-2
		NIVCONF$CAS[condQ3]<-"Q3"		
		NIVCONF$NIVEAU[condQ3MEFM]<-2
		NIVCONF$CAS[condQ3MEFM]<-"Q3"
		NIVCONF$NIVEAU[condQ4]<-1
		NIVCONF$CAS[condQ4]<-"Q4"
		NIVCONF$NIVEAU[condQ5]<-2
		NIVCONF$CAS[condQ5]<-"Q5"
		NIVCONF$NIVEAU[condQ5MEFM]<-2
		NIVCONF$CAS[condQ5MEFM]<-"Q5"
		NIVCONF$NIVEAU[condQ6]<-1
		NIVCONF$CAS[condQ6]<-"Q6"
		NIVCONF$NIVEAU[condQ7]<-2
		NIVCONF$CAS[condQ7]<-"Q7"
		NIVCONF$NIVEAU[condQ7MEFM]<-2
		NIVCONF$CAS[condQ7MEFM]<-"Q7"
		NIVCONF$NIVEAU[condQ8]<-1
		NIVCONF$CAS[condQ8]<-"Q8"
		rm(condQ1,condQ1MEFM,condQ2,condQ3,condQ3MEFM,condQ4,condQ5,condQ5MEFM,condQ6,condQ7,condQ7MEFM,condQ8)
	} else if (BASSIN=="AELB") {
		condQ1<-NIVCONF$BIOALL=="oui" & NIVCONF$ROBUSTESSE<=1 # BIOALL tient déjà compte des MEFM
		condQ2<-NIVCONF$BIOALL=="oui" & NIVCONF$ROBUSTESSE>1 & NIVCONF$COHERENCE=="coherent"
		condQ3<-NIVCONF$BIOALL=="oui" & NIVCONF$ROBUSTESSE>1 & NIVCONF$COHERENCE=="incoherent"
		condQ4<-NIVCONF$BIOALL=="non" & NIVCONF$DATASENSIBLE=="dispo" & NIVCONF$ROBUSTESSE<=1
		condQ5<-NIVCONF$BIOALL=="non" & NIVCONF$DATASENSIBLE=="dispo" & NIVCONF$ROBUSTESSE>1 & NIVCONF$COHERENCE=="coherent"
		condQ6<-NIVCONF$BIOALL=="non" & NIVCONF$DATASENSIBLE=="dispo" & NIVCONF$ROBUSTESSE>1 & NIVCONF$COHERENCE=="incoherent"
		condQ7a<-NIVCONF$BIOALL=="non" & NIVCONF$DATASENSIBLE=="non dispo" & NIVCONF$ROBUSTESSE<=1 & NIVCONF$COHERENCE=="coherent"
		condQ7b<-NIVCONF$BIOALL=="non" & NIVCONF$DATASENSIBLE=="non dispo" & NIVCONF$ROBUSTESSE<=1 & NIVCONF$COHERENCE=="incoherent"
		condQ8<-NIVCONF$BIOALL=="non" & NIVCONF$DATASENSIBLE=="non dispo" & NIVCONF$ROBUSTESSE>1
		condQ9a<-NIVCONF$BIOALL=="non" & NIVCONF$DATASENSIBLE=="nc" & NIVCONF$ETATECOLOME %in% (1:2) & NIVCONF$RGLOBAL=="1"
		condQ9b<-NIVCONF$BIOALL=="non" & NIVCONF$DATASENSIBLE=="nc" & !(NIVCONF$ETATECOLOME %in% (1:2) & NIVCONF$RGLOBAL=="1")
		condQ10<-!is.na(NIVCONF$ETATPCHME) & is.na(NIVCONF$ETATBIOME)
		condQ20<-NIVCONF$ETATECOLOME==0

		NIVCONF$NIVEAU[condQ1]<-3
		NIVCONF$CAS[condQ1]<-"Q1"
		NIVCONF$NIVEAU[condQ2]<-3
		NIVCONF$CAS[condQ2]<-"Q2"
		NIVCONF$NIVEAU[condQ3]<-2
		NIVCONF$CAS[condQ3]<-"Q3"
		NIVCONF$NIVEAU[condQ4]<-3
		NIVCONF$CAS[condQ4]<-"Q4"
		NIVCONF$NIVEAU[condQ5]<-2
		NIVCONF$CAS[condQ5]<-"Q5"
		NIVCONF$NIVEAU[condQ6]<-1
		NIVCONF$CAS[condQ6]<-"Q6"
		NIVCONF$NIVEAU[condQ7a]<-2
		NIVCONF$CAS[condQ7a]<-"Q7a"
		NIVCONF$NIVEAU[condQ7b]<-1
		NIVCONF$CAS[condQ7b]<-"Q7b"
		NIVCONF$NIVEAU[condQ8]<-1
		NIVCONF$CAS[condQ8]<-"Q8"
		NIVCONF$NIVEAU[condQ9a]<-2
		NIVCONF$CAS[condQ9a]<-"Q9a"
		NIVCONF$NIVEAU[condQ9b]<-1
		NIVCONF$CAS[condQ9b]<-"Q9b"
		NIVCONF$NIVEAU[condQ10]<-NA  # niveau de confiance attributé en concertation
		NIVCONF$CAS[condQ10]<-"Q10"
		NIVCONF$NIVEAU[condQ20]<-0
		NIVCONF$CAS[condQ20]<-"Q20"
		rm(condQ1,condQ2,condQ3,condQ4,condQ5,condQ6,condQ7a,condQ7b,condQ8,condQ9a,condQ9b,condQ10,condQ20)
		gc()
	}
	EECOLOME_NAMES<-c(EECOLOME_NAMES, "NIVEAU", "CAS")
	#RLT_EECOLOME<-NIVCONF[,EECOLOME_NAMES]
	EECOLOME<-NIVCONF[,EECOLOME_NAMES]
	
	gc() ## compacte R
	flush.console()
}
#########

if (SEEEECOLOME=="oui" & CEPE == "PE") {
	EECOLOME<-merge(EECOLO,STATION[,c("STATION","EUCD","REPRESENTATIVE")],by="STATION",all.x = TRUE)
	EECOLOME<-merge(EECOLOME[EECOLOME$REPRESENTATIVE == "oui",],MASSEDEAU,by="EUCD",all.x = TRUE)
	if (EXCLURE_IIL == "oui") {
		condmeafm<-EECOLOME$TYPEME %in% c("MEA","MEFM"  )
		PARAMETREBIO_NAMES_HIIL<-PARAMETREBIO_NAMES[PARAMETREBIO_NAMES != "IIL"]
		EECOLOME$ETATBIO[condmeafm]<-apply(EECOLOME[condmeafm,PARAMETREBIO_NAMES_HIIL],1,max,na.rm = TRUE)
		EECOLOME$ETATBIO[!condmeafm]<-as.numeric(NA)
		
	if (BASSIN=="AESN") {
		EECOLOME$ETATECOLOHPS<-FCTEECOLOHPS_AESN(VARBIO=EECOLOME$ETATBIO, VARPCH=EECOLOME$ETATPCH)
	} else {
		EECOLOME$ETATECOLOHPS<-FCTEECOLOHPS(VARBIO=EECOLOME$ETATBIO, VARPCH=EECOLOME$ETATPCH)
	}
		
	if (BASSIN=="AESN") {
		EECOLOME$ETATECOLOHPSetBO2<-FCTEECOLOHPS_AESN(VARBIO=EECOLOME$ETATBIO, VARPCH=EECOLOME$ETATPCH_SSBILANO2)
	} else {
		EECOLOME$ETATECOLOHPSetBO2<-FCTEECOLOHPS(VARBIO=EECOLOME$ETATBIO, VARPCH=EECOLOME$ETATPCH_SSBILANO2)
	}	
	
	## Etat ECOLO avec polluants spécifiques (appel de la fonction)
	if (SEEEPS=="oui" & BASSIN!="AESN" ) {
		EECOLOME<-merge(EECOLOME, RLT_POLSPE[,c("STATION", ETATPS_NAMES)], by="STATION", all.x=TRUE)
		EECOLOME$ETATECOLO<-FCTEECOLO(VARECOLOHPS=EECOLOME$ETATECOLOHPS, VARPS=EECOLOME$ETATPS)
	} else if (SEEEPS=="oui" & BASSIN=="AESN" ) {
		EECOLOME<-merge(EECOLOME, RLT_POLSPE[,c("STATION", ETATPS_NAMES)], by="STATION", all.x=TRUE)
		EECOLOME$ETATECOLO<-FCTEECOLO_AESN(VARECOLOHPS=EECOLOME$ETATECOLOHPS, VARPS=EECOLOME$ETATPS)
	} else {
		EECOLOME$ETATECOLO<-EECOLOME$ETATECOLOHPS
	}

	## Pour Plan d'eau, Etat ECOLO avec polluants spécifiques et hors bilanO2
		if (SEEEPS=="oui" & BASSIN!="AESN" ) {
			EECOLOME$ETATECOLOHBO2<-FCTEECOLO(VARECOLOHPS=EECOLOME$ETATECOLOHPSetBO2, VARPS=EECOLOME$ETATPS)
		} else if (SEEEPS=="oui" & BASSIN=="AESN" ) {
			EECOLOME$ETATECOLOHBO2<-FCTEECOLO_AESN(VARECOLOHPS=EECOLOME$ETATECOLOHPSetBO2, VARPS=EECOLOME$ETATPS)
		} else {
			EECOLOME$ETATECOLOHBO2<-EECOLOME$ETATECOLOHPSetBO2
		}

		EECOLOME$RESTRICTION[condmeafm]<-"calcul etatbio et etateco sans IIL"
		ETATECOLOMEPE_NAMES<-c(ETATECOLOMEPE_NAMES,"RESTRICTION")
	}
}
#########
gc()