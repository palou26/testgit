###########################################
## CONFORMITE : TABLES DE PARAMETRES
## script créé en mars 2013
###########################################

#SAVE()
tcl("update")
###création d'une fonction pour dire si la données est vide et ferme alors l'application
test_data_vide<-function(donnee, msg="La table de données est vide.") {
	if (nrow(donnee) == 0) {
		tkmessageBox(title = "Info", message = paste(msg,"\n L'application va se fermer automatiquement. Veuillez corriger et lancer un nouveau traitement.",sep=""), icon = "info", type = "ok")
		tcl("update")
	source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE)	}
}

### Création d'un guide pour le bon report des extension de fichiers fonction du module
fileExtension <- data.frame(MODULE=c("REEE2010","REEE2016","REEE2018","REEE2021"),fileExtension=c("","_2c","_2c","_3c"))
fE.MOD <- as.character(fileExtension[which(as.character(fileExtension$MODULE)==MODULE),2])
if (MODULE == "CONTAMINATION") {fE.MOD <- "_2c"}

### choix des tables de paramètres
if (CHOIXPARAM != "perso") {
	tcl("update")
	ch_param<-paste(racine,"/PARAMETRES/DEFAUT/",sep="")
} else {
	ch_param<-paste(racine,"/PARAMETRES/PERSONNALISE/",sep="")
}



if (BASSIN== "AESN") {
# gestion de l'erreur si absence du fichier
	if (file.exists(paste(ch_param,"PARAMETREPCH_NO3aesn.csv",sep="")) ){
		PARAMNO3AESN<-read.csv2(paste(ch_param,"PARAMETREPCH_NO3aesn.csv",sep=""))
		PARAMNO3AESN$SEUIL<-as.numeric(as.character(PARAMNO3AESN$SEUIL))
	} else {
		tkmessageBox(title = "Info", message = paste(ch_param,"PARAMETREPCH_NO3aesn.csv n'existe pas.\n L'application va se fermer automatiquement. Veuillez réparer l'erreur et lancer un nouveau traitement.",sep=""), icon = "info", type = "ok")
		tcl("update")
		source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
	}
}

### PARAMETREPCH
if (file.exists(paste(ch_param,"PARAMETREPCH.csv",sep="")) ){
PARAMETREPCH<-read.csv2(paste(ch_param,"PARAMETREPCH.csv",sep=""))
PARAMETREPCH$PARAMETRE<-as.character(PARAMETREPCH$PARAMETRE)
PARAMETREPCH$NOM<-as.character(PARAMETREPCH$NOM)
PARAMETREPCH$ELTQUALITE<-as.character(PARAMETREPCH$ELTQUALITE)
PARAMETREPCH$FRACTION<-as.character(PARAMETREPCH$FRACTION)
PARAMETREPCH$IDTRI<-as.numeric(as.character(PARAMETREPCH$IDTRI))
PARAMETREPCH$SUPB<-as.numeric(as.character(PARAMETREPCH$SUPB))
PARAMETREPCH$INFB<-as.numeric(as.character(PARAMETREPCH$INFB))
PARAMETREPCH$INFV<-as.numeric(as.character(PARAMETREPCH$INFV))
PARAMETREPCH$INFJ<-as.numeric(as.character(PARAMETREPCH$INFJ))
PARAMETREPCH$INFO<-as.numeric(as.character(PARAMETREPCH$INFO))
PARAMETREPCH$INFR<-as.numeric(as.character(PARAMETREPCH$INFR))
PARAMETREPCHCE<-PARAMETREPCH
} else {
tkmessageBox(title = "Info", message = paste(ch_param,"PARAMETREPCH.csv n'existe pas.\n L'application va se fermer automatiquement. Veuillez réparer l'erreur et lancer un nouveau traitement.",sep=""), icon = "info", type = "ok")
tcl("update")
source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
}

if (CEPE == "PE") {
	if (file.exists(paste(ch_param,"PARAMETREPCH_PE",fE.MOD,".csv",sep="")) ){
	PARAMETREPCH<-read.csv2(paste(ch_param,"PARAMETREPCH_PE",fE.MOD,".csv",sep=""))
	PARAMETREPCH$PARAMETRE<-as.character(PARAMETREPCH$PARAMETRE)
	PARAMETREPCH$NOM<-as.character(PARAMETREPCH$NOM)
	PARAMETREPCH$ELTQUALITE<-as.character(PARAMETREPCH$ELTQUALITE)
	PARAMETREPCH$FRACTION<-as.character(PARAMETREPCH$FRACTION)
	PARAMETREPCH$IDTRI<-as.numeric(as.character(PARAMETREPCH$IDTRI))
	for (colo in names(PARAMETREPCH)[substr(names(PARAMETREPCH),1,3) %in% c("SUP","INF")]){
		PARAMETREPCH[,colo]<-as.numeric(as.character(PARAMETREPCH[,colo]))	
	}
	} else {
	tkmessageBox(title = "Info", message = paste(ch_param,"PARAMETREPCH.csv n'existe pas.\n L'application va se fermer automatiquement. Veuillez réparer l'erreur et lancer un nouveau traitement.",sep=""), icon = "info", type = "ok")
	tcl("update")
	source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
	}
}

### PARAMETREUNITE : table des unités pour l'ensemble des parametres (PCH, PS et CHIM)
if (file.exists(paste(ch_param,"PARAMETREUNITE",fE.MOD,".csv",sep="")) ){
PARAMETREUNITE<-read.csv2(paste(ch_param,"PARAMETREUNITE",fE.MOD,".csv",sep=""))
PARAMETREUNITE$PARAMETRE<-as.character(PARAMETREUNITE$PARAMETRE)
PARAMETREUNITE$NOM<-as.character(PARAMETREUNITE$NOM)
PARAMETREUNITE$UNITE<-as.character(PARAMETREUNITE$UNITE)
PARAMETREUNITE$UNITELIB<-as.character(PARAMETREUNITE$UNITELIB)
PARAMETREUNITE$ETAT<-as.character(PARAMETREUNITE$ETAT)
} else {
tkmessageBox(title = "Info", message = paste(ch_param,"PARAMETREUNITE",fE.MOD,".csv n'existe pas.\n L'application va se fermer automatiquement. Veuillez réparer l'erreur et lancer un nouveau traitement.",sep=""), icon = "info", type = "ok")
tcl("update")
source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
}


### PARAMETREUNITE : table des unités pour l'ensemble des parametres utilisé dans module Contamination
if (CONTA == "oui") {
if (file.exists(paste(ch_param,"PARAMETREUNITE_CONTA.csv",sep="")) ){
	PARAMETREUNITE<-read.csv2(paste(ch_param,"PARAMETREUNITE_CONTA.csv",sep=""))
	PARAMETREUNITE$PARAMETRE<-as.character(PARAMETREUNITE$PARAMETRE)
	PARAMETREUNITE$NOM<-as.character(PARAMETREUNITE$NOM)
	PARAMETREUNITE$UNITE<-as.character(PARAMETREUNITE$UNITE)
	PARAMETREUNITE$UNITELIB<-as.character(PARAMETREUNITE$UNITELIB)
	} else {
	tkmessageBox(title = "Info", message = paste(ch_param,"PARAMETREUNITE_CONTA.csv n'existe pas.\n L'application va se fermer automatiquement. Veuillez réparer l'erreur et lancer un nouveau traitement.",sep=""), icon = "info", type = "ok")
	tcl("update")
	source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
	}
}

### PARAMETREBIO
if (file.exists(paste(ch_param,"PARAMETREBIO",fE.MOD,".csv",sep="")) ){
PARAMETREBIO<-read.csv2(paste(ch_param,"PARAMETREBIO",fE.MOD,".csv",sep=""))
	if(MODULE=="REEE2018") {PARAMETREBIO<-read.csv2(paste(ch_param,"PARAMETREBIO",fE.MOD,"REEE2018.csv",sep=""))}
PARAMETREBIO$PARAMETRE<-as.character(PARAMETREBIO$PARAMETRE)
PARAMETREBIO$PARAMETRELIB<-as.character(PARAMETREBIO$PARAMETRELIB)
PARAMETREBIO$PARAGROUP<-as.character(PARAMETREBIO$PARAGROUP)
PARAMETREBIO$PARALIBGROUP<-as.character(PARAMETREBIO$PARALIBGROUP)
PARAMETREBIO$IDTRI<-as.character(PARAMETREBIO$IDTRI)
	# on retire I2M2 (7613) et IPR+ (7614) si REEE2016
if (MODULE == "REEE2016" ) {PARAMETREBIO<-PARAMETREBIO[! (PARAMETREBIO$PARAMETRE %in% c("7614","7613")),]} 
} else {
tkmessageBox(title = "Info", message = paste(ch_param,"PARAMETREBIO",fE.MOD,".csv n'existe pas.\n L'application va se fermer automatiquement. Veuillez réparer l'erreur et lancer un nouveau traitement.",sep=""), icon = "info", type = "ok")
source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
}



# AESN : IBGA est intégré avec IBG, on retire ce paramètre de la table PARAMETREBIO pour éviter d'avoir les colonnes dans les exports
#if (BASSIN =="AESN") {
#	PARAMETREBIO<-PARAMETREBIO[PARAMETREBIO$PARAGROUP!="6951",]
#}

if (MODULE =="REEE2018"){
	if(  file.exists(paste(ch_param,"PARAMBIO_TYPEFR_REEE2018.csv",sep=""))) {
	PARAMBIOTYPEFR<-read.csv2(paste(ch_param,"PARAMBIO_TYPEFR_REEE2018.csv",sep=""))
	} else {
	tkmessageBox(title = "Info", message = paste(ch_param,"PARAMBIO_TYPEFR_REEE2018.csv n'existe pas.\n L'application va se fermer automatiquement. Veuillez réparer l'erreur et lancer un nouveau traitement.",sep=""), icon = "info", type = "ok")
	source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
	}
}


### PARAMETREBIO PE
if (CEPE == "PE"  ) {
if (file.exists(paste(ch_param,"PARAMETREBIO_PE",fE.MOD,".csv",sep="")) ){
PARAMETREBIO<-read.csv2(paste(ch_param,"PARAMETREBIO_PE",fE.MOD,".csv",sep=""))
PARAMETREBIO$EQ<-as.character(PARAMETREBIO$EQ)
PARAMETREBIO$PARAMETRE<-as.character(PARAMETREBIO$PARAMETRE)
PARAMETREBIO$PARAMETRELIB<-as.character(PARAMETREBIO$PARAMETRELIB)
PARAMETREBIO$PARALIBGROUP<-as.character(PARAMETREBIO$PARALIBGROUP)
PARAMETREBIO$IDTRI<-as.character(PARAMETREBIO$IDTRI)
PARAMETREBIO$INFB<-as.numeric(as.character(PARAMETREBIO$INFB))
PARAMETREBIO$INFV<-as.numeric(as.character(PARAMETREBIO$INFV))
PARAMETREBIO$INFJ<-as.numeric(as.character(PARAMETREBIO$INFJ))
PARAMETREBIO$INFO<-as.numeric(as.character(PARAMETREBIO$INFO))
PARAMETREBIO$INFR<-as.numeric(as.character(PARAMETREBIO$INFR))
} else {
tkmessageBox(title = "Info", message = paste(ch_param,"PARAMETREBIO",fE.MOD,".csv n'existe pas.\n L'application va se fermer automatiquement. Veuillez réparer l'erreur et lancer un nouveau traitement.",sep=""), icon = "info", type = "ok")
source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
}


if(MODULE== "REEE2016" & CEPE == "PE" ) {
PARAMETREBIO<-PARAMETREBIO[PARAMETREBIO$PARAGROUP != "IPL",]
}


###Cas particulier des tableaux
if (file.exists(paste(ch_param,"PECasparticulierCycle1_chloA.csv",sep="")) ){
PECasPartiChloA<-read.csv2(paste(ch_param,"PECasparticulierCycle1_chloA.csv",sep=""))
PECasPartiChloA$EU_CD<-as.character(PECasPartiChloA$EU_CD)
PECasPartiChloA$NOMME<-as.character(PECasPartiChloA$NOMME)
PECasPartiChloA$TYPE<-as.character(PECasPartiChloA$TYPE)
PECasPartiChloA$TBB<-as.numeric(as.character(PECasPartiChloA$TBB))
PECasPartiChloA$Bmo<-as.numeric(as.character(PECasPartiChloA$Bmo))
PECasPartiChloA$MoMe<-as.numeric(as.character(PECasPartiChloA$MoMe))
PECasPartiChloA$MeMa<-as.numeric(as.character(PECasPartiChloA$MeMa))
} else {
tkmessageBox(title = "Info", message = paste(ch_param,"PECasparticulierCycle1_chloA.csv n'existe pas.\n L'application va se fermer automatiquement. Veuillez réparer l'erreur et lancer un nouveau traitement.",sep=""), icon = "info", type = "ok")
source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
}

}


###LISTECODERQE
if (file.exists(paste(ch_param,"LISTECODERQE.csv",sep="")) ){
LISTECODERQE<-read.csv2(paste(ch_param,"LISTECODERQE.csv",sep=""))
LISTECODERQE$REMARQUE<-as.character(LISTECODERQE$REMARQUE)
LISTECODERQE$REMARQUELIB<-as.character(LISTECODERQE$REMARQUELIB)
LISTECODERQE$QUANTIFIE<-as.character(LISTECODERQE$QUANTIFIE)
} else {
tkmessageBox(title = "Info", message = paste(ch_param,"LISTECODERQE.csv n'existe pas.\n L'application va se fermer automatiquement. Veuillez réparer l'erreur et lancer un nouveau traitement.",sep=""), icon = "info", type = "ok")
tcl("update")
source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
}

##GRILLEIBD
if (file.exists(paste(ch_param,"GRILLEIBD",fE.MOD,".csv",sep="")) ){
GRILLEIBD<-read.csv2(paste(ch_param,"GRILLEIBD",fE.MOD,".csv",sep=""))
GRILLEIBD$TYPEFR<-as.character(GRILLEIBD$TYPEFR)
GRILLEIBD$INFB<-as.numeric(as.character(GRILLEIBD$INFB))
GRILLEIBD$INFV<-as.numeric(as.character(GRILLEIBD$INFV))
GRILLEIBD$INFJ<-as.numeric(as.character(GRILLEIBD$INFJ))
GRILLEIBD$INFO<-as.numeric(as.character(GRILLEIBD$INFO))
GRILLEIBD$INFR<-as.numeric(as.character(GRILLEIBD$INFR))
if(MODULE == "REEE2016" | MODULE == "REEE2018"){ # modifié le 10/12/14
GRILLEIBD$VALREF<-as.numeric(as.character(GRILLEIBD$VALREF))
GRILLEIBD$VALMIN<-as.numeric(as.character(GRILLEIBD$VALMIN))
}
} else {
tkmessageBox(title = "Info", message = paste(ch_param,"GRILLEIBD",fE.MOD,".csv n'existe pas.\n L'application va se fermer automatiquement. Veuillez réparer l'erreur et lancer un nouveau traitement.",sep=""), icon = "info", type = "ok")
source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
}


##GRILLEIBD (pour les BV sup 10 000 km2
if (MODULE == "REEE2018") {
if ( file.exists(paste(ch_param,"GRILLEIBD_2cBVsup10000km2.csv",sep="")) ){
GRILLEIBD_GRANDBV<-read.csv2(paste(ch_param,"GRILLEIBD_2cBVsup10000km2.csv",sep=""))
GRILLEIBD_GRANDBV$TYPEFR<-as.character(GRILLEIBD_GRANDBV$TYPEFR)
GRILLEIBD_GRANDBV$INFB<-as.numeric(as.character(GRILLEIBD_GRANDBV$INFB))
GRILLEIBD_GRANDBV$INFV<-as.numeric(as.character(GRILLEIBD_GRANDBV$INFV))
GRILLEIBD_GRANDBV$INFJ<-as.numeric(as.character(GRILLEIBD_GRANDBV$INFJ))
GRILLEIBD_GRANDBV$INFO<-as.numeric(as.character(GRILLEIBD_GRANDBV$INFO))
GRILLEIBD_GRANDBV$INFR<-as.numeric(as.character(GRILLEIBD_GRANDBV$INFR))

} else {
tkmessageBox(title = "Info", message = paste(ch_param,"GRILLEIBD_2cBVsup10000km2.csv n'existe pas.\n L'application va se fermer automatiquement. Veuillez réparer l'erreur et lancer un nouveau traitement.",sep=""), icon = "info", type = "ok")
source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
}
}

##GRILLEIPR
if (file.exists(paste(ch_param,"GRILLEIPR",fE.MOD,".csv",sep="")) ){
GRILLEIPR<-read.csv2(paste(ch_param,"GRILLEIPR",fE.MOD,".csv",sep=""))
GRILLEIPR$INFB<-as.numeric(as.character(GRILLEIPR$INFB))
GRILLEIPR$INFV<-as.numeric(as.character(GRILLEIPR$INFV))
GRILLEIPR$INFJ<-as.numeric(as.character(GRILLEIPR$INFJ))
GRILLEIPR$INFO<-as.numeric(as.character(GRILLEIPR$INFO))
GRILLEIPR$INFR<-as.numeric(as.character(GRILLEIPR$INFR))

if(MODULE == "REEE2016" | MODULE == "REEE2018"){ # modifié le 10/12/14
GRILLEIPR$TYPEFR<-as.character(GRILLEIPR$TYPEFR)
GRILLEIPR$VALALTIV<-as.numeric(as.character(GRILLEIPR$VALALTIV))
}
} else {
tkmessageBox(title = "Info", message = paste(ch_param,"GRILLEIPR",fE.MOD,".csv n'existe pas.\n L'application va se fermer automatiquement. Veuillez réparer l'erreur et lancer un nouveau traitement.",sep=""), icon = "info", type = "ok")
tcl("update")
source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
}

##GRILLEIPRPLUS IPR+ (rajouté 25/01/2018)
if (file.exists(paste(ch_param,"GRILLEIPRPLUS.csv",sep="")) ){
GRILLEIPRPLUS<-read.csv2(paste(ch_param,"GRILLEIPRPLUS.csv",sep=""))
GRILLEIPRPLUS$INFB<-as.numeric(as.character(GRILLEIPRPLUS$INFB))
GRILLEIPRPLUS$INFV<-as.numeric(as.character(GRILLEIPRPLUS$INFV))
GRILLEIPRPLUS$INFJ<-as.numeric(as.character(GRILLEIPRPLUS$INFJ))
GRILLEIPRPLUS$INFO<-as.numeric(as.character(GRILLEIPRPLUS$INFO))
GRILLEIPRPLUS$INFR<-as.numeric(as.character(GRILLEIPRPLUS$INFR))

if(MODULE == "REEE2016" | MODULE == "REEE2018"){ 
GRILLEIPRPLUS$TYPEFR<-as.character(GRILLEIPRPLUS$TYPEFR)
GRILLEIPRPLUS$VALALTIV<-as.numeric(as.character(GRILLEIPRPLUS$VALALTIV))
}
} else {
tkmessageBox(title = "Info", message = paste(ch_param,"GRILLEIPRPLUS.csv n'existe pas.\n L'application va se fermer automatiquement. Veuillez réparer l'erreur et lancer un nouveau traitement.",sep=""), icon = "info", type = "ok")
tcl("update")
source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
}


##GRILLEIBG
if (file.exists(paste(ch_param,"GRILLEIBG",fE.MOD,".csv",sep="")) ){
GRILLEIBG<-read.csv2(paste(ch_param,"GRILLEIBG",fE.MOD,".csv",sep=""))
GRILLEIBG$TYPEFR<-as.character(GRILLEIBG$TYPEFR)
GRILLEIBG$INFB<-as.numeric(as.character(GRILLEIBG$INFB))
GRILLEIBG$INFV<-as.numeric(as.character(GRILLEIBG$INFV))
GRILLEIBG$INFJ<-as.numeric(as.character(GRILLEIBG$INFJ))
GRILLEIBG$INFO<-as.numeric(as.character(GRILLEIBG$INFO))
GRILLEIBG$INFR<-as.numeric(as.character(GRILLEIBG$INFR))
	if(MODULE == "REEE2016" | MODULE == "REEE2018"){ # modifié le 10/12/14
	GRILLEIBG$VALREF<-as.numeric(as.character(GRILLEIBG$VALREF))
	}
} else {
tkmessageBox(title = "Info", message = paste(ch_param,"GRILLEIBG",fE.MOD,".csv n'existe pas.\n L'application va se fermer automatiquement. Veuillez réparer l'erreur et lancer un nouveau traitement.",sep=""), icon = "info", type = "ok")
source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
}

##GRILLEI2M2
if (MODULE %in% c("REEE2018","REEE2021")) {
	if ( file.exists(paste(ch_param,"GRILLEI2M2",fE.MOD,".csv",sep="")) ){
	GRILLEI2M2<-read.csv2(paste(ch_param,"GRILLEI2M2",fE.MOD,".csv",sep=""))
	GRILLEI2M2$TYPEFR<-as.character(GRILLEI2M2$TYPEFR)
	GRILLEI2M2$INFB<-as.numeric(as.character(GRILLEI2M2$INFB))
	GRILLEI2M2$INFV<-as.numeric(as.character(GRILLEI2M2$INFV))
	GRILLEI2M2$INFJ<-as.numeric(as.character(GRILLEI2M2$INFJ))
	GRILLEI2M2$INFO<-as.numeric(as.character(GRILLEI2M2$INFO))
	GRILLEI2M2$INFR<-as.numeric(as.character(GRILLEI2M2$INFR))
		
	} else {
	tkmessageBox(title = "Info", message = paste(ch_param,"GRILLEI2M2",fE.MOD,".csv n'existe pas.\n L'application va se fermer automatiquement. Veuillez réparer l'erreur et lancer un nouveau traitement.",sep=""), icon = "info", type = "ok")
	source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
	}
}

##GRILLEIBMR
## bloc IBMR rajouté le 10/12/14
if (MODULE != "REEE2010"){
if (file.exists(paste(ch_param,"GRILLEIBMR",fE.MOD,".csv",sep="")) ){
GRILLEIBMR<-read.csv2(paste(ch_param,"GRILLEIBMR",fE.MOD,".csv",sep=""))
GRILLEIBMR$TYPEFR<-as.character(GRILLEIBMR$TYPEFR)
GRILLEIBMR$INFB<-as.numeric(as.character(GRILLEIBMR$INFB))
GRILLEIBMR$INFV<-as.numeric(as.character(GRILLEIBMR$INFV))
GRILLEIBMR$INFJ<-as.numeric(as.character(GRILLEIBMR$INFJ))
GRILLEIBMR$INFO<-as.numeric(as.character(GRILLEIBMR$INFO))
GRILLEIBMR$INFR<-as.numeric(as.character(GRILLEIBMR$INFR))
# modifié le 10/12/14
GRILLEIBMR$VALREF<-as.numeric(as.character(GRILLEIBMR$VALREF))
} else {
tkmessageBox(title = "Info", message = paste(ch_param,"GRILLEIBMR",fE.MOD,".csv n'existe pas.\n L'application va se fermer automatiquement. Veuillez réparer l'erreur et lancer un nouveau traitement.",sep=""), icon = "info", type = "ok")
tcl("update")
source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
}
}

##PARAMETREPCHEXCEPT
if (file.exists(paste(ch_param,"PARAMETREPCHEXCEPT.csv",sep="")) ){
PARAMETREPCHEXCEPT<-read.csv2(paste(ch_param,"PARAMETREPCHEXCEPT.csv",sep=""))
PARAMETREPCHEXCEPT$PARAMETRE<-as.character(PARAMETREPCHEXCEPT$PARAMETRE)
PARAMETREPCHEXCEPT$NOM<-as.character(PARAMETREPCHEXCEPT$NOM)
PARAMETREPCHEXCEPT$CONTEXTE_PISCICOLE<-as.character(PARAMETREPCHEXCEPT$CONTEXTE_PISCICOLE)
PARAMETREPCHEXCEPT$SUPB<-as.numeric(as.character(PARAMETREPCHEXCEPT$SUPB))
PARAMETREPCHEXCEPT$INFB<-as.numeric(as.character(PARAMETREPCHEXCEPT$INFB))
PARAMETREPCHEXCEPT$INFV<-as.numeric(as.character(PARAMETREPCHEXCEPT$INFV))
PARAMETREPCHEXCEPT$INFJ<-as.numeric(as.character(PARAMETREPCHEXCEPT$INFJ))
PARAMETREPCHEXCEPT$INFO<-as.numeric(as.character(PARAMETREPCHEXCEPT$INFO))
PARAMETREPCHEXCEPT$INFR<-as.numeric(as.character(PARAMETREPCHEXCEPT$INFR))
} else {
tkmessageBox(title = "Info", message = paste(ch_param,"PARAMETREPCHEXCEPT.csv n'existe pas.\n L'application va se fermer automatiquement. Veuillez réparer l'erreur et lancer un nouveau traitement.",sep=""), icon = "info", type = "ok")
tcl("update")
source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
}

####PARAMETREPOLSPE
if (file.exists(paste(ch_param,"PARAMETREPOLSPE",fE.MOD,".csv",sep="")) ){
PARAMETREPOLSPE<-read.csv2(paste(ch_param,"PARAMETREPOLSPE",fE.MOD,".csv",sep=""))
PARAMETREPOLSPE$PARAMETRE<-as.character(PARAMETREPOLSPE$PARAMETRE)
PARAMETREPOLSPE$PARAMETRELIB<-as.character(PARAMETREPOLSPE$PARAMETRELIB)
PARAMETREPOLSPE$FRACTION<-as.character(PARAMETREPOLSPE$FRACTION)
PARAMETREPOLSPE$POLLUANT<-as.character(PARAMETREPOLSPE$POLLUANT)
PARAMETREPOLSPE$NQEMA<-as.numeric(as.character(PARAMETREPOLSPE$NQEMA))
PARAMETREPOLSPE$IDTRI<-as.numeric(as.character(PARAMETREPOLSPE$IDTRI))
PARAMETREPOLSPE$PRIORITE<-as.numeric(as.character(PARAMETREPOLSPE$PRIORITE))
# modifié le 16/03/15
PARAMETREPOLSPE$NQEMA2<-as.numeric(as.character(PARAMETREPOLSPE$NQEMA2))
if(MODULE == "REEE2016" | MODULE == "REEE2018"){ # modifié le 10/12/14
PARAMETREPOLSPE$AEAG<-as.character(PARAMETREPOLSPE$AEAG)
PARAMETREPOLSPE$AEAP<-as.character(PARAMETREPOLSPE$AEAP)
PARAMETREPOLSPE$AELB<-as.character(PARAMETREPOLSPE$AELB)
PARAMETREPOLSPE$AERM<-as.character(PARAMETREPOLSPE$AERM)
PARAMETREPOLSPE$AERMC<-as.character(PARAMETREPOLSPE$AERMC)
PARAMETREPOLSPE$AESN<-as.character(PARAMETREPOLSPE$AESN)
PARAMETREPOLSPE$ODE971<-as.character(PARAMETREPOLSPE$ODE971)
PARAMETREPOLSPE$ODE972<-as.character(PARAMETREPOLSPE$ODE972)
PARAMETREPOLSPE$ODE973<-as.character(PARAMETREPOLSPE$ODE973)
PARAMETREPOLSPE$ODE974<-as.character(PARAMETREPOLSPE$ODE974)
PARAMETREPOLSPE$ODE976<-as.character(PARAMETREPOLSPE$ODE976)
PARAMETREPOLSPE$LQSEUIL<-as.numeric(gsub(",",".",as.character(PARAMETREPOLSPE$LQSEUIL)))
}
} else {
tkmessageBox(title = "Info", message = paste(ch_param,"PARAMETREPOLSPE",fE.MOD,".csv n'existe pas.\n L'application va se fermer automatiquement. Veuillez réparer l'erreur et lancer un nouveau traitement.",sep=""), icon = "info", type = "ok")
tcl("update")
source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
}
if(MODULE == "REEE2016" | MODULE == "REEE2018"){
## Sélection des polspe selon le bassin
## créé le 12/12/14
	if (BASSIN =="AEAG") {
		PARAMETREPOLSPE<-PARAMETREPOLSPE[PARAMETREPOLSPE$AEAG=="oui",]
	} else if (BASSIN =="AEAP") {
		PARAMETREPOLSPE<-PARAMETREPOLSPE[PARAMETREPOLSPE$AEAP=="oui",]
	} else if (BASSIN =="AELB") {
		PARAMETREPOLSPE<-PARAMETREPOLSPE[PARAMETREPOLSPE$AELB=="oui",]
	} else if (BASSIN =="AERM") {
		PARAMETREPOLSPE<-PARAMETREPOLSPE[PARAMETREPOLSPE$AERM=="oui",]
	} else if (BASSIN =="AERMC") {
		PARAMETREPOLSPE<-PARAMETREPOLSPE[PARAMETREPOLSPE$AERMC=="oui",]	
	} else if (BASSIN =="AESN") {
		PARAMETREPOLSPE<-PARAMETREPOLSPE[PARAMETREPOLSPE$AESN=="oui",]
	} else if (BASSIN =="ODE971") {
		PARAMETREPOLSPE<-PARAMETREPOLSPE[PARAMETREPOLSPE$ODE971=="oui",]
	} else if (BASSIN =="ODE972") {
		PARAMETREPOLSPE<-PARAMETREPOLSPE[PARAMETREPOLSPE$ODE972=="oui",]
	} else if (BASSIN =="ODE973") {
		PARAMETREPOLSPE<-PARAMETREPOLSPE[PARAMETREPOLSPE$ODE973=="oui",]	
	} else if (BASSIN =="ODE974") {
		PARAMETREPOLSPE<-PARAMETREPOLSPE[PARAMETREPOLSPE$ODE974=="oui",]	
	} else if (BASSIN =="ODE976") {
		PARAMETREPOLSPE<-PARAMETREPOLSPE[PARAMETREPOLSPE$ODE976=="oui",]	
	}
}


####PARAMETRECHIM
if (file.exists(paste(ch_param,"PARAMETRECHIM",fE.MOD,".csv",sep="")) ){
	PARAMETRECHIM<-read.csv2(paste(ch_param,"PARAMETRECHIM",fE.MOD,".csv",sep=""))
	PARAMETRECHIM$PARAMETRE<-as.character(PARAMETRECHIM$PARAMETRE)
	PARAMETRECHIM$PARAMETRELIB<-as.character(PARAMETRECHIM$PARAMETRELIB)
	PARAMETRECHIM$FRACTION<-as.character(PARAMETRECHIM$FRACTION)
	PARAMETRECHIM$PRIORITE<-as.numeric(PARAMETRECHIM$PRIORITE)
	PARAMETRECHIM$PARAGROUP1<-as.character(PARAMETRECHIM$PARAGROUP1)
	PARAMETRECHIM$PARAGROUP2<-as.character(PARAMETRECHIM$PARAGROUP2)
	if(MODULE == "REEE2016" | MODULE == "REEE2018"){	
		PARAMETRECHIM$LQSEUIL<-as.numeric(gsub(",",".",as.character(PARAMETRECHIM$LQSEUIL)))
	}
	
	####LIAISONCHIM
	LIAISONCHIM1<-PARAMETRECHIM[,c("PARAMETRE","PARAGROUP1")]
	names(LIAISONCHIM1)[2]<-"PARAGROUP"
	LIAISONCHIM2<-PARAMETRECHIM[c(PARAMETRECHIM$PARAGROUP2!=""),c("PARAMETRE","PARAGROUP2")]
	names(LIAISONCHIM2)[2]<-"PARAGROUP"
	LIAISONCHIM<-rbind(LIAISONCHIM1,LIAISONCHIM2)
	rm(LIAISONCHIM1,LIAISONCHIM2)
} else {
	tkmessageBox(title = "Info", message = paste(ch_param,"PARAMETRECHIM",fE.MOD,".csv n'existe pas.\n L'application va se fermer automatiquement. Veuillez réparer l'erreur et lancer un nouveau traitement.",sep=""), icon = "info", type = "ok")
	tcl("update")
	source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
}

####PARAGROUPCHIM
if (file.exists(paste(ch_param,"PARAGROUPCHIM",fE.MOD,".csv",sep="")) ){
	PARAGROUPCHIM<-read.csv2(paste(ch_param,"PARAGROUPCHIM",fE.MOD,".csv",sep=""))
	PARAGROUPCHIM$PARAGROUP<-as.character(PARAGROUPCHIM$PARAGROUP)
	PARAGROUPCHIM$PARAGROUPLIB<-as.character(PARAGROUPCHIM$PARAGROUPLIB)
	PARAGROUPCHIM$PARAGROUPLIBCOURT<-as.character(PARAGROUPCHIM$PARAGROUPLIBCOURT)
	PARAGROUPCHIM$GROUPE<-as.character(PARAGROUPCHIM$GROUPE)
	PARAGROUPCHIM$FAMILLE<-as.character(PARAGROUPCHIM$FAMILLE)
	PARAGROUPCHIM$NQEMA<-as.numeric(as.character(PARAGROUPCHIM$NQEMA))
	PARAGROUPCHIM$NQECMA<-as.numeric(as.character(PARAGROUPCHIM$NQECMA))
	PARAGROUPCHIM$IDTRI<-as.numeric(as.character(PARAGROUPCHIM$IDTRI))
	
	# modifié le 12/06/15
	PARAGROUPCHIM$NQEMA2<-as.numeric(as.character(PARAGROUPCHIM$NQEMA2))	
	PARAGROUPCHIM$NQEMA3<-as.numeric(as.character(PARAGROUPCHIM$NQEMA3))
	PARAGROUPCHIM$NQEMA4<-as.numeric(as.character(PARAGROUPCHIM$NQEMA4))
	PARAGROUPCHIM$NQEMA5<-as.numeric(as.character(PARAGROUPCHIM$NQEMA5))
	PARAGROUPCHIM$NQECMA2<-as.numeric(as.character(PARAGROUPCHIM$NQECMA2))	
	PARAGROUPCHIM$NQECMA3<-as.numeric(as.character(PARAGROUPCHIM$NQECMA3))
	PARAGROUPCHIM$NQECMA4<-as.numeric(as.character(PARAGROUPCHIM$NQECMA4))
	PARAGROUPCHIM$NQECMA5<-as.numeric(as.character(PARAGROUPCHIM$NQECMA5))
	
} else {
	tkmessageBox(title = "Info", message = paste(ch_param,"PARAGROUPCHIM",fE.MOD,".csv n'existe pas.\n L'application va se fermer automatiquement. Veuillez réparer l'erreur et lancer un nouveau traitement.",sep=""), icon = "info", type = "ok")
	tcl("update")
	source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
	}

gc()


####PARAMETRECONTA
if(CONTA == "oui") {
if (file.exists(paste(ch_param,"GRILLECONTAMINATION.csv",sep="")) ){
	if(CHOIXSEUIL != "LIBRE"){
	PARAMETRECONTA<-read.csv2(paste(ch_param,"GRILLECONTAMINATION.csv",sep=""))
	} else {PARAMETRECONTA <- PARAMETRECONTALIBRE}
	PARAMETRECONTA$PARAMETRE<-as.character(PARAMETRECONTA$PARAMETRE)
	PARAMETRECONTA$NOMCOURT<-gsub("'","",(gsub(" ","_",gsub(",","_",as.character(PARAMETRECONTA$NOMCOURT)))))
	PARAMETRECONTA$FRACTION<-as.character(PARAMETRECONTA$FRACTION)
	PARAMETRECONTA$UNITE<-as.character(PARAMETRECONTA$UNITE)
	

	if ( CHOIXSEUIL ==  "LIBRE"  ) {
		if ( paste(as.character(PARAMETRECONTA$SEUIL_LIBRE4),collapse = "") == "" ) { NBSEUIL<-4 } else { NBSEUIL<-5 }
		PARAMETRECONTA$SEUIL_LIBRE1<-as.numeric(gsub(",",".",as.character(PARAMETRECONTA$SEUIL_LIBRE1)))
		PARAMETRECONTA$SEUIL_LIBRE2<-as.numeric(gsub(",",".",as.character(PARAMETRECONTA$SEUIL_LIBRE2)))
		PARAMETRECONTA$SEUIL_LIBRE3<-as.numeric(gsub(",",".",as.character(PARAMETRECONTA$SEUIL_LIBRE3)))
		if (NBSEUIL==5){
		PARAMETRECONTA$SEUIL_LIBRE4<-as.numeric(gsub(",",".",as.character(PARAMETRECONTA$SEUIL_LIBRE4)))} else { PARAMETRECONTA$SEUIL_LIBRE4<- 99999999}

		condNA<-!is.na(PARAMETRECONTA$SEUIL_LIBRE1) 
		pbseuil1<-nrow(PARAMETRECONTA[ condNA & PARAMETRECONTA$SEUIL_LIBRE1 >= PARAMETRECONTA$SEUIL_LIBRE2,])
		pbseuil2<-nrow(PARAMETRECONTA[ condNA & PARAMETRECONTA$SEUIL_LIBRE2 >= PARAMETRECONTA$SEUIL_LIBRE3,])
		pbseuil3<-0
		if (NBSEUIL==5){
		pbseuil3<-nrow(PARAMETRECONTA[ condNA & PARAMETRECONTA$SEUIL_LIBRE3 >= PARAMETRECONTA$SEUIL_LIBRE4,])
		}
		if (pbseuil1 + pbseuil2 + pbseuil3 > 0) {
			tkmessageBox(title = "Info", message = "Certains seuils libres ne sont pas ordonnés du plus petit au plus grand. Les résultats risquent de manquer de cohérence", icon = "info", type = "ok")
			tcl("update")
		}
		
	} else {

	PARAMETRECONTA$NQEMA<-as.numeric(gsub(",",".",as.character(PARAMETRECONTA$NQEMA)))
	PARAMETRECONTA$NQECMA<-as.numeric(gsub(",",".",as.character(PARAMETRECONTA$NQECMA)))
	PARAMETRECONTA$VGEMA<-as.numeric(gsub(",",".",as.character(PARAMETRECONTA$VGEMA)))
	PARAMETRECONTA$VGECMA<-as.numeric(gsub(",",".",as.character(PARAMETRECONTA$VGECMA)))
	PARAMETRECONTA$PNEC<-as.numeric(gsub(",",".",as.character(PARAMETRECONTA$PNEC)))
	PARAMETRECONTA$NQEMA<-as.numeric(gsub(",",".",as.character(PARAMETRECONTA$NQEMA)))
	
	
	if ("COEFFTOX" %in% names(PARAMETRECONTA) ) {PARAMETRECONTA$COEFFTOX<-as.numeric(gsub(",",".",as.character(PARAMETRECONTA$COEFFTOX))) }
	
	}	

} else {
	tkmessageBox(title = "Info", message = paste(ch_param,"GRILLECONTAMINATION.csv n'existe pas.\n L'application va se fermer automatiquement. Veuillez réparer l'erreur et lancer un nouveau traitement.",sep=""), icon = "info", type = "ok")
	source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
}
}



########
if( CEPE == "PE") {
PARAMPE<-data.frame(
					STATION=as.character(),
					ZMOY=as.numeric(),
					TPSSEJOUR=as.numeric(),
					FIABILITSEJOUR=as.character(),
					COMPARTIMENT=as.character(),
					PARAMETRE=as.character(),
					INFB=as.numeric(),
					INFV=as.numeric(),
					INFJ=as.numeric(),
					INFO=as.numeric()
					)
					
				}


#SAVE()
###########################################
## CONFORMITE : table STATION
###########################################
STATION<-read.csv2(XLS_STATION)
test_data_vide(STATION,"La table STATION ne contient aucune donnée valide.")
## comparaison avec les noms de colonnes attendus


if (CEPE == "CE") {
	
		NAMES_ATTENDU<-c("STATION", "CONTEXTE_PISCICOLE", "TYPEFR", "ALTITUDE", "EUCD", "REPRESENTATIVE", "EXCEPT_FROID", "EXCEPT_CHAUD", "EXCEPT_ACID", "EXCEPT_MO", "EXCEPT_TOURB", "EXCEPT_O2","ECHELLESTA","FONDGEO_ARSENIC","FONDGEO_CHROME","FONDGEO_CUIVRE","FONDGEO_ZINC","FONDGEO_CADMIUM","FONDGEO_PLOMB","FONDGEO_MERCURE","FONDGEO_NICKEL", "DURETE")
		NAMES_ATTENDU_LISTTXT<-paste(NAMES_ATTENDU, collapse = ",")
	if ( MODULE != "REEE2018" ) {
		NAMES_ATTENDU<-c("STATION","LIBELLE", "CONTEXTE_PISCICOLE", "TYPEFR","TYPESTAT", "ALTITUDE", "EUCD", "REPRESENTATIVE", "EXCEPT_FROID", "EXCEPT_CHAUD", "EXCEPT_ACID", "EXCEPT_MO", "EXCEPT_TOURB", "EXCEPT_O2","ECHELLESTA","FONDGEO_ARSENIC","FONDGEO_CHROME","FONDGEO_CUIVRE","FONDGEO_ZINC","FONDGEO_CADMIUM","FONDGEO_PLOMB","FONDGEO_MERCURE","FONDGEO_NICKEL", "DURETE")
		NAMES_ATTENDU_LISTTXT<-paste(NAMES_ATTENDU, collapse = ",")
	} else if ( MODULE == "REEE2018" ) {
		NAMES_ATTENDU<-c("STATION","LIBELLE", "CONTEXTE_PISCICOLE", "TYPEFR","TYPESTAT", "ME_BV_SUP10000KM2", "ALTITUDE", "EUCD", "REPRESENTATIVE", "EXCEPT_FROID", "EXCEPT_CHAUD", "EXCEPT_ACID", "EXCEPT_MO", "EXCEPT_TOURB", "EXCEPT_O2","EXCEPTLOC_COD","EXCEPTLOC_O2SATO2","NC_EXCEPTLOC","ECHELLESTA","FONDGEO_ARSENIC","FONDGEO_CHROME","FONDGEO_CUIVRE","FONDGEO_ZINC","FONDGEO_CADMIUM","FONDGEO_PLOMB","FONDGEO_MERCURE","FONDGEO_NICKEL", "DURETE")
		NAMES_ATTENDU_LISTTXT<-paste(NAMES_ATTENDU, collapse = ",")
	}
	names(STATION)<-toupper(names(STATION))
	NAMES_STATION<-names(STATION)
	NAMES_STATION<-NAMES_STATION[NAMES_STATION %in% NAMES_ATTENDU]
	if (length(NAMES_STATION) != length(NAMES_ATTENDU)){
		NAMESTAT_MANQ<-paste(subset(NAMES_ATTENDU,!(NAMES_ATTENDU %in% NAMES_STATION)),collapse = ", ")
		tkmessageBox(title = "Info", message = paste("Les noms de colonnes de STATION.csv ne sont pas ceux attendus\n Les colonnes attendues sont:\n",NAMES_ATTENDU_LISTTXT,"\n\n Il manque les colonnes : ", NAMESTAT_MANQ , sep=" "), icon = "info", type = "ok")
		source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
		}
	#STATION<-STATION[,NAMES_ATTENDU] # n'est pas utile , surtout si on souhaite conserver certaines colonnes du fichier STATION en plus
	STATION$STATION<-as.character(STATION$STATION)
	
	#7/12/2017 : uniformiser le code station Agence
	cond<-nchar(STATION$STATION) == 7 & regexpr("[0-9][0-9][0-9][0-9][0-9][0-9][0-9]",STATION$STATION)
	STATION$STATION[cond]<-paste0("0",STATION$STATION[cond])
	
	#
	STATION$CONTEXTE_PISCICOLE<-as.character(STATION$CONTEXTE_PISCICOLE)
	STATION$TYPEFR<-as.character(STATION$TYPEFR)
	STATION$ALTITUDE<-as.numeric(as.character(STATION$ALTITUDE))
	STATION$EUCD<-as.character(STATION$EUCD)
	STATION$REPRESENTATIVE<-tolower(gsub(" ","",as.character(STATION$REPRESENTATIVE)))
	STATION$EXCEPT_FROID<-tolower(gsub(" ","",as.character(STATION$EXCEPT_FROID)))
	STATION$EXCEPT_CHAUD<-tolower(gsub(" ","",as.character(STATION$EXCEPT_CHAUD)))
	STATION$EXCEPT_ACID<-tolower(gsub(" ","",as.character(STATION$EXCEPT_ACID)))
	STATION$EXCEPT_MO<-tolower(gsub(" ","",as.character(STATION$EXCEPT_MO)))
	STATION$EXCEPT_TOURB<-tolower(gsub(" ","",as.character(STATION$EXCEPT_TOURB)))
	STATION$EXCEPT_O2<-tolower(gsub(" ","",as.character(STATION$EXCEPT_O2)))
	
	if (MODULE == "REEE2018") {
		STATION$ME_BV_SUP10000KM2<-tolower(gsub(" ","",as.character(STATION$ME_BV_SUP10000KM2)))
	}
	
	if (BASSIN =="AESN" & MODULE == "REEE2018") { # Exception locale
		STATION$EXCEPTLOC_COD<-tolower(gsub(" ","",as.character(STATION$EXCEPTLOC_COD)))
		STATION$EXCEPTLOC_O2SATO2<-tolower(gsub(" ","",as.character(STATION$EXCEPTLOC_O2SATO2)))
		STATION$NC_EXCEPTLOC<-tolower(gsub(" ","",as.character(STATION$NC_EXCEPTLOC)))
		
		
		if(nrow(STATION[STATION$EXCEPTLOC_COD == "oui" |STATION$EXCEPTLOC_O2SATO2 == "oui" ,]) > 0) { EXCEPTLOC <- "oui"}
		
	}
	
	# modifié 16/02/15 : ajout de colonnes
	STATION$FONDGEO_ARSENIC<-as.numeric(gsub(",",".",as.character(STATION$FONDGEO_ARSENIC)))
	STATION$FONDGEO_CHROME<-as.numeric(gsub(",",".",as.character(STATION$FONDGEO_CHROME)))
	STATION$FONDGEO_CUIVRE<-as.numeric(gsub(",",".",as.character(STATION$FONDGEO_CUIVRE)))
	STATION$FONDGEO_ZINC<-as.numeric(gsub(",",".",as.character(STATION$FONDGEO_ZINC)))
	STATION$DURETE<-as.numeric(as.character(STATION$DURETE))

	# modifié 12/06/15 : ajout de colonnes
	STATION$FONDGEO_CADMIUM<-as.numeric(gsub(",",".",as.character(STATION$FONDGEO_CADMIUM)))
	STATION$FONDGEO_PLOMB<-as.numeric(gsub(",",".",as.character(STATION$FONDGEO_PLOMB)))
	STATION$FONDGEO_MERCURE<-as.numeric(gsub(",",".",as.character(STATION$FONDGEO_MERCURE)))
	STATION$FONDGEO_NICKEL<-as.numeric(gsub(",",".",as.character(STATION$FONDGEO_NICKEL)))
	

	if (BASSIN =="AESN") {  # spécifique à AESN colonne supplémentaire
		STATION$LIBELLE<-as.character(STATION$LIBELLE)
		STATION$TYPESTAT<-as.character(STATION$TYPESTAT)
	}
	
	#CONTEXTE_PISCICOLE à retraduire
	STATION$CONTEXTE_PISCICOLE<-tolower(STATION$CONTEXTE_PISCICOLE)
	STATION$CONTEXTE_PISCICOLE[substr(STATION$CONTEXTE_PISCICOLE,1,1) == "c"]<-"cyprinicole"
	STATION$CONTEXTE_PISCICOLE[substr(STATION$CONTEXTE_PISCICOLE,1,1) == "i"]<-"intermediaire"
	STATION$CONTEXTE_PISCICOLE[substr(STATION$CONTEXTE_PISCICOLE,1,1) == "s"]<-"salmonicole"
	print(table(STATION$CONTEXTE_PISCICOLE))

	#REPRESENTATITIVE "temporaire" à retraduire
	STATION$REPRESENTATIVE<-tolower(STATION$REPRESENTATIVE)
	STATION$REPRESENTATIVE[substr(STATION$REPRESENTATIVE,1,1) == "t"]<-"temporaire"
	print(table(STATION$REPRESENTATIVE))


}


if (CEPE == "PE") {

	NAMES_ATTENDU<-c("STATION" , "NOMSTATION" , "CODENATPE" , "TYPEPE" , "TYPEFR" , "TYPECUVETTE" , "EUCD" , "NOMME" , 
	"TYPEME" , "REPRESENTATIVE" , "ZMOY" , "ZMAX" , "ALTITUDE" , "VOLUME" , "STRATIFIE" , "MARNAGE" , "TPSSEJOUR" , "FIABILITSEJOUR" , 
	"EXCEPT_TRANS" , "EXCEPT_O2" , "EXCEPT_NIT" , "ECHELLESTA", 
	"FONDGEO_ARSENIC", "FONDGEO_CHROME","FONDGEO_CUIVRE","FONDGEO_ZINC","FONDGEO_CADMIUM","FONDGEO_PLOMB","FONDGEO_MERCURE","FONDGEO_NICKEL","DURETE")
	if (MODULE != "REEE2010") {NAMES_ATTENDU<-c(NAMES_ATTENDU,"EXCEPT_IBML")}
	NAMES_ATTENDU_LISTTXT<-paste(NAMES_ATTENDU, collapse = ",")
	
	NAMES_STATION<-names(STATION)
	NAMES_STATION<-NAMES_STATION[NAMES_STATION %in% NAMES_ATTENDU]
	if (length(NAMES_STATION) != length(NAMES_ATTENDU)){
		NAMESTAT_MANQ<-paste(subset(NAMES_ATTENDU,!(NAMES_ATTENDU %in% NAMES_STATION)),collapse = ", ")
		tkmessageBox(title = "Info", message = paste("Les noms de colonnes de STATION.csv ne sont pas ceux attendus\n Les colonnes attendues sont:\n",NAMES_ATTENDU_LISTTXT,"\n\n Il manque les colonnes : ", NAMESTAT_MANQ , sep=" "), icon = "info", type = "ok")
		source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
		}
	#STATION<-STATION[,NAMES_ATTENDU]
	STATION$STATION<-as.character(STATION$STATION)
	
	#7/12/2017 : uniformiser le code station Agence
	cond<-nchar(STATION$STATION) == 7 & regexpr("[0-9][0-9][0-9][0-9][0-9][0-9][0-9]",STATION$STATION)
	STATION$STATION[cond]<-paste0("0",STATION$STATION[cond])
	
	
	STATION$NOMSTATION<-as.character(STATION$NOMSTATION)
	STATION$CODENATPE<-as.character(STATION$CODENATPE)
	STATION$TYPEPE<-as.numeric(as.character(STATION$TYPEPE))
	STATION$TYPEFR<-as.character(STATION$TYPEFR)
	STATION$TYPECUVETTE<-as.character(STATION$TYPECUVETTE)
	STATION$EUCD<-as.character(STATION$EUCD)
	STATION$NOMME<-as.character(STATION$NOMME)
	STATION$TYPEME<-as.character(STATION$TYPEME)
	STATION$REPRESENTATIVE<-tolower(as.character(STATION$REPRESENTATIVE))
	STATION$STRATIFIE<-tolower(as.character(STATION$STRATIFIE))
	STATION$MARNAGE<-tolower(as.character(STATION$MARNAGE))
	
	STATION$FIABILITSEJOUR<-tolower(as.character(STATION$FIABILITSEJOUR))
	STATION$EXCEPT_TRANS<-tolower(as.character(STATION$EXCEPT_TRANS))
	STATION$EXCEPT_O2<-tolower(as.character(STATION$EXCEPT_O2))
	STATION$EXCEPT_NIT<-tolower(as.character(STATION$EXCEPT_NIT))
	
	if (MODULE != "REEE2010") {STATION$EXCEPT_IBML<-tolower(as.character(STATION$EXCEPT_IBML))}
	
	STATION$ECHELLESTA<-as.character(STATION$ECHELLESTA)

	
	STATION$FONDGEO_ARSENIC<-as.numeric(gsub(",",".",as.character(STATION$FONDGEO_ARSENIC)))
	STATION$FONDGEO_CHROME<-as.numeric(gsub(",",".",as.character(STATION$FONDGEO_CHROME)))
	STATION$FONDGEO_CUIVRE<-as.numeric(gsub(",",".",as.character(STATION$FONDGEO_CUIVRE)))
	STATION$FONDGEO_ZINC<-as.numeric(gsub(",",".",as.character(STATION$FONDGEO_ZINC)))
	STATION$DURETE<-as.numeric(as.character(STATION$DURETE))

	STATION$FONDGEO_CADMIUM<-as.numeric(gsub(",",".",as.character(STATION$FONDGEO_CADMIUM)))
	STATION$FONDGEO_PLOMB<-as.numeric(gsub(",",".",as.character(STATION$FONDGEO_PLOMB)))
	STATION$FONDGEO_MERCURE<-as.numeric(gsub(",",".",as.character(STATION$FONDGEO_MERCURE)))
	STATION$FONDGEO_NICKEL<-as.numeric(gsub(",",".",as.character(STATION$FONDGEO_NICKEL)))

	
	STATION$ZMOY<-as.numeric(gsub(",",".",as.character(STATION$ZMOY)))
	STATION$ZMAX<-as.numeric(gsub(",",".",as.character(STATION$ZMAX)))
	STATION$ALTITUDE<-as.numeric(gsub(",",".",as.character(STATION$ALTITUDE)))
	STATION$VOLUME<-as.numeric(gsub(",",".",as.character(STATION$VOLUME)))
	STATION$TPSSEJOUR<-as.numeric(gsub(",",".",as.character(STATION$TPSSEJOUR)))
	STATION$FIABILITSEJOUR[STATION$TPSSEJOUR == 0]<-"non"
	tcl("update")
}


#controle qu'il n'y a pas de doublons dans la table STATION
if ( nrow(STATION) != length(unique(STATION$STATION))  ){
	STATIONDUPLIQ<-data.frame(table(STATION$STATION))[table(STATION$STATION)>1,]
	names(STATIONDUPLIQ)<-c("STATION","FREQ")
	
	CSV<-paste(CH_ERREUR,"DoublonsSTATIONS_",SEEE_DEBformat,".csv",sep="")
	write.csv2(STATIONDUPLIQ,CSV)
	tkmessageBox(title = "Info", message = paste("Il y a des doublons de numéros de Station dans la table STATION.csv \n Les doublons ont été exportés dans : \n", CSV,
	"\n L'application va se fermer automatiquement",sep=" "), icon = "info", type = "ok")
	tcl("update")
	source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit

}


#####Liste des stations manquantes dans le fichier STATION
liststatmanQ<-as.character()
 ## création du dossier /RESULTATS/ERREURS/
#if (!file.exists(paste(racine,"/RESULTATS/",sep=""))) {dir.create(paste(racine,"/RESULTATS/",sep=""))}
#if (!file.exists(paste(CH_ERREUR,"",sep=""))) {dir.create(paste(CH_ERREUR,"",sep=""))}

gc()

###########################################
## CONFORMITE : table DATAPCH généralité (PCH / PS / CHIM)
###############################################
if (SEEEPCH == "oui"  | SEEEPS == "oui" | SEEECHIM == "oui" | CONTA == "oui" ) {
	#DATAPCH<-rbind(DATAPCH,DATAPCH,DATAPCH,DATAPCH)
	#DATAPCH$DATEPRELEV<-paste(DATAPCH$DATEPRELEV, 1:nrow(DATAPCH))
	gc()
	print(paste("nb datapch",nrow(DATAPCH)))

	names(DATAPCH)<-NAMESPCH
	print(dim(DATAPCH))
	print(names(DATAPCH))
	print(summary(DATAPCH))
	DATAPCH$STATION<-as.character(DATAPCH$STATION)
	
	
	#7/12/2017 : uniformiser le code station Agence
	cond<-nchar(DATAPCH$STATION) == 7 & regexpr("[0-9][0-9][0-9][0-9][0-9][0-9][0-9]",DATAPCH$STATION)
	DATAPCH$STATION[cond]<-paste0("0",DATAPCH$STATION[cond])
	gc()
	
	DATAPCH$PARAMETRE<-tolower(as.character(DATAPCH$PARAMETRE))
	DATAPCH$FRACTION<-as.character(as.numeric(DATAPCH$FRACTION))
	DATAPCH$REMARQUE<-as.character(as.numeric(DATAPCH$REMARQUE))
	DATAPCH$DATEPRELEV<-as.character(DATAPCH$DATEPRELEV)
	DATAPCH$HEUREPRELEV<-as.character(DATAPCH$HEUREPRELEV)
	gc()
	
	if ( paste(as.character(DATAPCH$LQ),collapse = "") == "" ) { LQ_NONDISPO<-"oui" } else { LQ_NONDISPO<-"non" } 
	DATAPCH$LQ<-as.numeric(gsub(",",".",as.character(DATAPCH$LQ)))
	DATAPCH$LQ<-as.numeric(as.character(DATAPCH$LQ))
		
	test_data_vide(DATAPCH,"Aucune donnée physicochimique n'a pu être extraite du fichier importé. Vérifiez la conformité de vos données.")

###DATAPCH : CORRECTION de RESULTAT si on a le signe "<"
	DATAPCH$RESULTAT<-as.character(DATAPCH$RESULTAT)
	index_signeinf<-grep("<",substr(DATAPCH$RESULTAT,1,1))
	DATAPCH$RESULTAT[index_signeinf]<-gsub("<","",DATAPCH$RESULTAT[index_signeinf])
	DATAPCH$REMARQUE[index_signeinf]<-"10"
	DATAPCH$RESULTAT<-as.numeric(gsub(",",".",as.character(DATAPCH$RESULTAT)))
	gc()
	
### On enlève les données null
	nblignepch_avant<-nrow(DATAPCH)
	DATAPCH<-DATAPCH[!is.na(DATAPCH$RESULTAT),]
	nblignepch_apres<-nrow(DATAPCH)
	nb_null_pch<-nblignepch_avant - nblignepch_apres	
	gc()
	
### Extraire l'année

	DATAPCH$ANNEE<-substr(DATAPCH$DATEPRELEV,7,10)
	if(min(as.numeric(DATAPCH$ANNEE),na.rm = TRUE)<1970){
		for ( i in 1970:2030) {
			DATAPCH$ANNEE[as.numeric(regexpr(as.character(i),DATAPCH$DATEPRELEV)) > 0]<-i
		}
		if (sum(DATAPCH$ANNEE) == 0) {
			tkmessageBox(title = "Info", message = paste("Attention! Aucun format de date n'est conforme pour extraire les années.\n Le résultat globalise l'ensemble des années pour le calcul de la moyenne\n avant la comparaison à la NQE.",sep=" "), icon = "info", type = "ok")
			tcl("update")
		} else if (min(DATAPCH$ANNEE) == 0){
			tkmessageBox(title = "Info", message = paste("Attention! Le format 'DATE' de certaines stations n'est pas conforme.\n Pour celles-ci le résultat globalise l'ensemble des années pour le calcul de la moyenne\n avant la comparaison à la NQE.",sep=" "), icon = "info", type = "ok")
			tcl("update")
		}
	}
	
	gc()
	
## Pour Plan d'eau, on ne conserve que les échantillon intégrés et on fait des conversions d'unité si besoin, 
	if(CEPE == "PE"){
		#Extraire l'échantillon selon la profondeur (1 = intégré, 2 = fond ) 
		# On retiendra 
		if ( paste(as.character(DATAPCH$INTEGRE),collapse = "") == "" ) { INTEGRE_NONDISPO<-"oui" } else { INTEGRE_NONDISPO<-"non" } 
		if (	INTEGRE_NONDISPO == "non") {
			DATAPCH$INTEGRE<-as.character(DATAPCH$INTEGRE)
			NROWINTEGREOK<-nrow(DATAPCH[DATAPCH$INTEGRE %in% c("1","2"),])
			PBINTEGRE<-nrow(DATAPCH) - NROWINTEGREOK 
			if(PBINTEGRE > 0) {
			YESNO<-tkmessageBox(message = paste( PBINTEGRE,"echantillons PCH/PS n'ont pas d'info sur la profondeur \n 1= Intégré\n 2 = Fond\nOui pour poursuivre en considérant ces échantillons\n comme intégrés"),
						icon = "question", type = "yesno", default = "yes")
					tcl("update")
					
					if (tclvalue(YESNO) == "no") {
						tkdestroy(tt)
						source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
					}
			}
			
			if (PROFONDEUR == "integre") {
			DATAPCH<-DATAPCH[DATAPCH$INTEGRE != "2",]
			} 
			if (PROFONDEUR == "fond") {
			DATAPCH<-DATAPCH[DATAPCH$INTEGRE == "2",]
			}
			
			
		}
		
		### Conversion de la transparence en mètre (code SANDRE = 111) si exprimée en centimètre
		condunitetranp<-DATAPCH$PARAMETRE == "1332" & DATAPCH$UNITE == "13"
		DATAPCH$RESULTAT[ condunitetranp]<- DATAPCH$RESULTAT[condunitetranp ]/100
		DATAPCH$UNITE[ condunitetranp]<- "111"
	}	

	####Conversion unité Microgramme vers milligramme (NH4, NO3, PHOS, NO2)
	condunite<-DATAPCH$PARAMETRE %in% c("1335","1340","1350","1339") & DATAPCH$UNITE %in% c("378","582","583","584","133")
	DATAPCH$RESULTAT[ condunite]<- DATAPCH$RESULTAT[condunite ]/1000
	DATAPCH$UNITE[ condunite]<- "162"

	#Extraire le Mois
	DATAPCH$MOIS<-substr(DATAPCH$DATEPRELEV,regexpr("/{0-9}{0-9}/",DATAPCH$DATEPRELEV)+1,regexpr("/{0-9}{0-9}/",DATAPCH$DATEPRELEV)+2)
	DATAPCH$MOIS<-as.numeric(as.character(DATAPCH$MOIS))
	gc()
	
	###Données pour état des polluants spécifiques
	if (SEEEPS == "oui"){
		DATAPOLSPE<-DATAPCH
		LISTPARAM_PS_AVANT<-sort(unique(DATAPOLSPE$PARAMETRE))
	}
	
	###Données pour état chimique
	if (SEEECHIM == "oui"){
		DATACHIM<-DATAPCH
		LISTPARAM_CHIM_AVANT<-sort(unique(DATACHIM$PARAMETRE))

	}
	
	
	###Données pour état chimique
	if (CONTA == "oui"){
		DATACONTA<-DATAPCH
		LISTPARAM_CONTA_AVANT<-sort(unique(DATACONTA$PARAMETRE))

	}

}
gc()
###########################################################
## CONFORMITE : table DATAPCH spéficique à traitement PCH
###########################################################


nbstatfalsepch<-" - "
if (SEEEPCH == "oui" ) {

### création des parametres 1302min et 1302max (modifié PPL 11/04/2016)
	DATAPCH_1302min<-DATAPCH[DATAPCH$PARAMETRE == "1302",]
	DATAPCH_1302max<-DATAPCH_1302min
	DATAPCH_1302min$PARAMETRE[DATAPCH_1302max$PARAMETRE == "1302"]<-"1302min"
	DATAPCH_1302max$PARAMETRE[DATAPCH_1302max$PARAMETRE == "1302"]<-"1302max"
	if(nrow(DATAPCH_1302max) > 0){
		DATAPCH<-DATAPCH[DATAPCH$PARAMETRE != "1302",]
		DATAPCH<-rbind(DATAPCH,DATAPCH_1302min,DATAPCH_1302max)
		rm(DATAPCH_1302max, DATAPCH_1302min)
		gc()
	}

#####selection dans DATAPCH selon le code fraction et le parametre
	if (FRACTIONOKPCH == "oui"){
		DATAPCH$FRACTION<-as.character(as.numeric(DATAPCH$FRACTION))
		PARAMETREPCH$FRACTION<-as.character(as.numeric(PARAMETREPCH$FRACTION))
		print(paste("nb lignes DATAPCH avant sélection des PARAMETRES et code FRACTION : ",nrow(DATAPCH)))
		DATAPCH<-DATAPCH[paste(DATAPCH$FRACTION,DATAPCH$PARAMETRE,sep="_") %in% paste(PARAMETREPCH$FRACTION,PARAMETREPCH$PARAMETRE,sep="_"),]
		print(paste("nb lignes DATAPCH après sélection des PARAMETRES et code FRACTION : ",nrow(DATAPCH)))
	}  else {
		print(paste("nb lignes DATAPCH avant sélection des PARAMETRES et code FRACTION : ",nrow(DATAPCH)))
		DATAPCH<-DATAPCH[paste(DATAPCH$FRACTION,DATAPCH$PARAMETRE,sep="_") %in% c(paste(3,PARAMETREPCH$PARAMETRE,sep="_"),paste(23,PARAMETREPCH$PARAMETRE,sep="_")) ,]
		print(paste("nb lignes DATAPCH après sélection des PARAMETRES et code FRACTION : ",nrow(DATAPCH)))
	}
	test_data_vide(DATAPCH,"Il n'y a pas de données PCH après l'extraction selon la fraction et le paramètre. \n Vérifiez la conformité des codes Fractions et Paramètres.")
	
#####selection dans DATAPCH selon l'unité du parametre
	if (UNITEOKPCH == "oui"){
		DATAPCH$UNITE<-as.character(as.numeric(DATAPCH$UNITE))
		PARAMETREUNITE$UNITE<-as.character(as.numeric(PARAMETREUNITE$UNITE))
		print(paste("nb lignes DATAPCH avant sélection de l'unité : ",nrow(DATAPCH)))
		DATAPCH<-DATAPCH[paste(DATAPCH$UNITE,DATAPCH$PARAMETRE,sep="_") %in% paste(PARAMETREUNITE$UNITE,PARAMETREUNITE$PARAMETRE,sep="_"),]
		print(paste("nb lignes DATAPCH après sélection de l'unité : ",nrow(DATAPCH)))
	}	
	test_data_vide(DATAPCH,"Il n'y a pas de données PCH après l'extraction selon l'unité. Vérifiez la conformité des unités.")
	
### Choix et respect du code REMARQUE
	DATAPCH<-DATAPCH[DATAPCH$REMARQUE %in% LISTECODERQE$REMARQUE,]
    print(paste("nb lignes DATAPCH après sélection du code REMARQUE : ",nrow(DATAPCH)))
	test_data_vide(DATAPCH,"Il n'y a pas de données PCH après l'extraction selon le code remarque. Vérifiez la conformité du code remarque.")

##vérif que les stations sont bien présentes dans la table STATION
	nbstat<-DATAPCH[!(DATAPCH$STATION %in% STATION$STATION),]
	nbstatfalsepch<-length(unique(nbstat$STATION))
	if (nbstatfalsepch>0 & nrow(nbstat) != nrow(DATAPCH)){
	liststatmanQ<-unique(c(liststatmanQ,unique(nbstat$STATION)))
	print(paste("PCH : il manque ", nbstatfalsepch, " stations dans la table STATION",sep=""))
	tcl("update")
	}
	if (nrow(nbstat) == nrow(DATAPCH) ){
	MSGBOX <- tkmessageBox(title = "Info", message = paste("Aucun code station des données PCH n'est conforme à la table STATION.csv\n L'application va se fermer automatiquement. Veuillez corriger et lancer un nouveau traitement.",sep=""), icon = "info", type = "ok")
	source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
	tcl("update")
	}
	gc()
	
### Extraire le mois et la SAISON
	if( CEPE == "PE") {
		DATAPCH$SAISON<-"ETE"
		DATAPCH$MOIS<-substr(DATAPCH$DATEPRELEV,regexpr("/{0-9}{0-9}/",DATAPCH$DATEPRELEV)+1,regexpr("/{0-9}{0-9}/",DATAPCH$DATEPRELEV)+2)
		DATAPCH$MOIS<-as.numeric(as.character(DATAPCH$MOIS))
		

		
		# Si la date est un nombre
		# Possibilité de le convertir (non implémenté)
		#Ex : as.character(format(as.Date(40255, origin = "1900-01-01"), format = "%d/%m/%Y"))
		conderror<-(!is.na(as.numeric(as.character(DATAPCH$DATEPRELEV))))
		pbDatenumeric<-nrow(DATAPCH[conderror,])
		if(pbDatenumeric > 0) {
		CSV<-paste(CH_ERREUR,"DATAPCH_PbMois_",SEEE_DEBformat,".csv",sep="")
		write.csv2(DATAPCH[conderror,],CSV)
		DATAPCH$MOIS[conderror]<-7
		YESNO<-tkmessageBox(message = paste("Pour ",pbDatenumeric ," prélèvements PCH, le format de la colonne date semble être un Nombre.\n Le mois et la saison seront pas défaut 'JUILLET' et 'ETE' \n Oui pour poursuivre et considérer \n tous les prélévement comme estivaux. \n Non pour stopper et visualiser les problèmes. \n (Formaliser la date comme ceci : 'jj/mm/aaaa')"),
				icon = "question", type = "yesno", default = "yes")
			tcl("update")
			
			if (tclvalue(YESNO) == "no") {
				tkdestroy(tt)
				browseURL(CSV)
				source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
			}
		}

		
		conderror<-is.na(DATAPCH$MOIS) | regexpr("/{0-9}{0-9}/",DATAPCH$DATEPRELEV) == -1
		NpbMois<-nrow(DATAPCH[conderror,])
		if(NpbMois > 0 & pbDatenumeric == 0) {
		CSV<-paste(CH_ERREUR,"DATAPCH_PbMois_",SEEE_DEBformat,".csv",sep="")
		write.csv2(DATAPCH[conderror,],CSV)
		DATAPCH$MOIS[conderror]<-7
		YESNO<-tkmessageBox(message = paste("Pour ",NpbMois ," prélèvements PCH, il n'a pas été possible d'extraire le Mois. Le mois et la saison seront par défaut 'JUILLET' et 'ETE' \n Oui pour poursuivre et considérer \n tous les prélévement comme estivaux. \n Non pour stopper et visualiser les problèmes. \n (Formaliser la date comme ceci : 'jj/mm/aaaa')"),
				icon = "question", type = "yesno", default = "yes")
			tcl("update")
			
			if (tclvalue(YESNO) == "no") {
				tkdestroy(tt)
				browseURL(CSV)
				source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
			}
		}
		
		
		# Saison hiver : Novembre inclu à Avril inclu
		DATAPCH$SAISON[DATAPCH$MOIS<5 | DATAPCH$MOIS>10]<-"HIVER"
	}	
	
	
	}
###############################
## CONFORMITE : table DATAPOLSPE
###############################
gc()
if (SEEEPS == "oui"){
	test_data_vide(DATAPOLSPE,"Aucune donnée des polluants spécifiques n'a pu être extraite du fichier importé. Vérifiez la conformité de vos données.")
	DATAPOLSPE$FRACTION<-as.character(as.numeric(DATAPOLSPE$FRACTION))
	PARAMETREPOLSPE$FRACTION<-as.character(as.numeric(PARAMETREPOLSPE$FRACTION))
	 if (FRACTIONOKPS == "oui"){
		print(paste("nb lignes DATAPOLSPE avant selection des PARAMETRES et code FRACTION : ",nrow(DATAPOLSPE)))
		DATAPOLSPE<-DATAPOLSPE[paste(DATAPOLSPE$FRACTION,DATAPOLSPE$PARAMETRE,sep="_") %in% paste(PARAMETREPOLSPE$FRACTION,PARAMETREPOLSPE$PARAMETRE,sep="_"),]
		print(paste("nb lignes DATAPOLSPE après selection des PARAMETRES et code FRACTION : ",nrow(DATAPOLSPE)))
			
	 } else {
		print(paste("nb lignes DATAPOLSPE avant sélection des PARAMETRES et code FRACTION : ",nrow(DATAPOLSPE)))
		DATAPOLSPE<-DATAPOLSPE[paste(DATAPOLSPE$FRACTION,DATAPOLSPE$PARAMETRE,sep="_") %in% c(paste(3,PARAMETREPOLSPE$PARAMETRE,sep="_"),paste(23,PARAMETREPOLSPE$PARAMETRE,sep="_")) ,]
		print(paste("nb lignes DATAPOLSPE après sélection des PARAMETRES et code FRACTION : ",nrow(DATAPOLSPE)))
	}	
	test_data_vide(DATAPOLSPE,"Il n'y a pas de données PS après l'extraction selon la fraction et le paramètre. \n Vérifiez la conformité des codes Fractions et Paramètres.")

	# On enlève les paramètres du xylène qui sont en substance indiciduelle. 
	PARAMETREPOLSPE<-PARAMETREPOLSPE[PARAMETREPOLSPE$PRIORITE == 2,names(PARAMETREPOLSPE) != "PRIORITE"]
	
	
#####selection selon l'unité du parametre
	if (UNITEOKPS == "oui"){
		DATAPOLSPE$UNITE<-as.character(as.numeric(DATAPOLSPE$UNITE))
		PARAMETREUNITE$UNITE<-as.character(as.numeric(PARAMETREUNITE$UNITE))
		print(paste("nb lignes DATAPOLSPE avant sélection de l'unité : ",nrow(DATAPOLSPE)))
		DATAPOLSPE<-DATAPOLSPE[paste(DATAPOLSPE$UNITE,DATAPOLSPE$PARAMETRE,sep="_") %in% paste(PARAMETREUNITE$UNITE,PARAMETREUNITE$PARAMETRE,sep="_"),]
		print(paste("nb lignes DATAPOLSPE après sélection de l'unité : ",nrow(DATAPOLSPE)))
	}	
	test_data_vide(DATAPOLSPE,"Il n'y a pas de données PS après l'extraction selon l'unité. Vérifiez la conformité des unités.")
	
### Choix et respect du code REMARQUE	
	DATAPOLSPE<-DATAPOLSPE[DATAPOLSPE$REMARQUE %in% LISTECODERQE$REMARQUE,]	
	test_data_vide(DATAPOLSPE,"Il n'y a pas de données PS après l'extraction selon le code remarque. Vérifiez la conformité des codes remarque.")

# retrait des LQ non renseignées si option LQ dispo est choisie car bug potentiel
	if (LQ_NONDISPO=="non") {
		NPS<-nrow(DATAPOLSPE)
		NPS2<-nrow(DATAPOLSPE[!is.na(DATAPOLSPE$LQ),])
		NBLQMANQ_PS<-NPS - NPS2
		if ( NPS - NPS2 != 0) {
		PBLQ_DATAPOLSPE<-DATAPOLSPE[is.na(DATAPOLSPE$LQ),]
		DATAPOLSPE<-DATAPOLSPE[!is.na(DATAPOLSPE$LQ),]
		MSGBOX <- tkmessageBox(title = "Info", message = paste(NPS - NPS2, " prélèvements n'ont pas de LQ renseignée pour le calcul de l'état PS\n Ces prélèvements ne seront pas utilisés dans le calcul",sep=""), icon = "info", type = "ok")
		CSV<-paste(CH_ERREUR,"Prélevements_LQ_Manq_",SEEE_DEBformat,".csv",sep="")
		write.csv2(PBLQ_DATAPOLSPE,CSV)
		MSGBOX <- tkmessageBox(title = "Info", message = paste("Les prélèvements sans LQ ont été exportés : \n",CSV,sep=""), icon = "info", type = "ok")
		tcl("update")		
		}
	}
	
##vérif que les stations sont bien présente dans la table STATION
	nbstatps<-DATAPOLSPE[!(DATAPOLSPE$STATION %in% STATION$STATION),]
	nbstatfalseps<-length(unique(nbstatps$STATION))
	if (nbstatfalseps>0 & nrow(nbstatps) != nrow(DATAPOLSPE)){
	liststatmanQ<-unique(c(liststatmanQ,unique(nbstatps$STATION)))
	print(paste("PS : il manque ", nbstatfalseps, " stations dans la table STATION",sep=""))
	tcl("update")
	}
	if (nrow(nbstatps) == nrow(DATAPOLSPE) ){
	MSGBOX <- tkmessageBox(title = "Info", message = paste("Aucun code station des données PS n'est conforme à la table STATION.csv\n L'application va se fermer automatiquement.Veuillez corriger et lancer un nouveau traitement.",sep=""), icon = "info", type = "ok")
	source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
	}
	gc()
}

###############################
## CONFORMITE : table DATABIO
###############################
gc()
tcl("update")
nbstatfalsebio<-" - "
if (SEEEBIO=="oui") {
	#DATABIO<-DATABIO[,CBBIOVALUE2]
	names(DATABIO)<-NAMESBIO
	print(dim(DATABIO))
	print(names(DATABIO))
	print(summary(DATABIO))
	DATABIO$STATION<-as.character(DATABIO$STATION)
	
	
		#7/12/2017 : uniformiser le code station Agence
	cond<-nchar(DATABIO$STATION) == 7 & regexpr("[0-9][0-9][0-9][0-9][0-9][0-9][0-9]",DATABIO$STATION)
	DATABIO$STATION[cond]<-paste0("0",DATABIO$STATION[cond])
	
	
	
	DATABIO$PARAMETRE<-as.character(DATABIO$PARAMETRE)
	DATABIO$DATEPRELEV<-as.character(DATABIO$DATEPRELEV)
	DATABIO$HEUREPRELEV<-as.character(DATABIO$HEUREPRELEV)
	
	#INDICEEQR
	if ( MODULE == "REEE2018" & CEPE == "CE") {
	DATABIO$INDICEEQR<-as.character(DATABIO$INDICEEQR)
	NBDATAssINDICEEQR<-nrow(DATABIO[is.na(DATABIO$INDICEEQR) | DATABIO$INDICEEQR == "" |  (toupper(substr(DATABIO$INDICEEQR,1,1)) != "I" & toupper(substr(DATABIO$INDICEEQR,1,1)) != "E")  ,])

	if ( NBDATAssINDICEEQR>0) {
	tkmessageBox(title = "Info", message = paste("Pour " , NBDATAssINDICEEQR,  "lignes dans DATABIO, le champ INDICEEQR n'est pas renseigné\n Attendu : 'INDICE' ou 'EQR'.\n PAr défaut, S3R considèrera que ce sont des Indices \n Sauf pour I2M2 où S3R considèrera que ce sont des EQR",sep=" "), icon = "info", type = "ok")
	}
	DATABIO$INDICEEQR[is.na(DATABIO$INDICEEQR) | DATABIO$INDICEEQR == "" |  (toupper(substr(DATABIO$INDICEEQR,1,1)) != "I" & toupper(substr(DATABIO$INDICEEQR,1,1)) != "E")]<-"INDICE"
	DATABIO$INDICEEQR[toupper(substr(DATABIO$INDICEEQR,1,1)) == "I"]<-"INDICE"
	DATABIO$INDICEEQR[toupper(substr(DATABIO$INDICEEQR,1,1)) == "E"]<-"EQR"
	}
	
	test_data_vide(DATABIO,"Aucune donnée biologique n'a pu être extraite du fichier importé. Vérifiez la conformité de vos données.")

###DATABIO : CORRECTION de RESULTAT si on a une virgule
	DATABIO$RESULTAT<-as.character(DATABIO$RESULTAT)
	#index_signeinf<-grep("<",substr(DATABIO$RESULTAT,1,1))
	#DATABIO$RESULTAT[index_signeinf]<-gsub("<","",DATABIO$RESULTAT[index_signeinf])
	DATABIO$RESULTAT<-as.numeric(gsub(",",".",as.character(DATABIO$RESULTAT)))

#####selection dans DATABIO selon  le parametre
	print(paste("nb lignes DATABIO avant selection des PARAMETRES : ",nrow(DATABIO)))
	NAME_OF_DATABIO<-names(DATABIO)
	DATABIO<-merge(DATABIO, PARAMETREBIO, by = "PARAMETRE")
	DATABIO<-DATABIO[,c(NAME_OF_DATABIO,"PARAMETRELIB","PARAGROUP")]
	print(paste("nb lignes DATABIO après selection des PARAMETRES  : ",nrow(DATABIO)))
	test_data_vide(DATABIO,"Il n'y a pas de données biologiques après l'extraction selon le code paramètre. Vérifiez la conformité des codes paramètres.")

### On enlève les données null
	nblignebio_avant<-nrow(DATABIO)
	DATABIO<-DATABIO[!is.na(DATABIO$RESULTAT),]
	nblignebio_apres<-nrow(DATABIO)
	nb_null_bio<-nblignebio_avant - nblignebio_apres
	test_data_vide(DATABIO,"Les données biologiques ne contiennent aucune valeur de résultat. Vérifiez la colonne 'résultat'.")

### Extraire l'année
## rajouté le 06/01/15 pour tous les cycles
	
	DATABIO$ANNEE<-substr(DATABIO$DATEPRELEV,7,10)
	if(min(as.numeric(DATABIO$ANNEE),na.rm = TRUE)<1970){
		for ( i in 1970:2030) {
				DATABIO$ANNEE[as.numeric(regexpr(as.character(i),DATABIO$DATEPRELEV)) > 0]<-i
			}
			if (sum(DATABIO$ANNEE) == 0) {
				tkmessageBox(title = "Info", message = paste("Attention! Aucun format de date n'est conforme pour extraire les années.\n Pour celles-ci le résultat globalise l'ensemble des années pour le calcul de la moyenne\n.",sep=" "), icon = "info", type = "ok")
			} else if (min(DATABIO$ANNEE) == 0){
				tkmessageBox(title = "Info", message = paste("Attention! Le format 'DATE' de certaines stations n'est pas conforme.\n Pour celles-ci le résultat globalise l'ensemble des années pour le calcul de la moyenne\n.",sep=" "), icon = "info", type = "ok")
		}
	}
		tcl("update")
	
### Extraire le mois et la SAISON  
	# (REEE2010)	+ retirer Chlo-A en profondeur de fond (intégré = 2)
	if( CEPE == "PE") {
		DATABIO$SAISON<-"ETE"
		DATABIO$MOIS<-substr(DATABIO$DATEPRELEV,regexpr("/{0-9}{0-9}/",DATABIO$DATEPRELEV)+1,regexpr("/{0-9}{0-9}/",DATABIO$DATEPRELEV)+2)
		DATABIO$MOIS<-as.numeric(as.character(DATABIO$MOIS))
		NpbMois<-nrow(DATABIO[is.na(DATABIO$MOIS),])
		if(NpbMois > 0) {
		CSV<-paste(CH_ERREUR,"DATABIO_PbMois_",SEEE_DEBformat,".csv",sep="")
		write.csv2(DATABIO,CSV)
		YESNO<-tkmessageBox(message = paste("Pour ",NpbMois ," prélèvements BIO, il n'a pas été possible d'extraire le Mois. \n Oui pour poursuivre et considérer \n tous les prélévement comme estivaux. \n Non pour stopper et visualiser les problèmes. \n (Formaliser la date comme ceci : 'jj/mm/aaaa')"),
				icon = "question", type = "yesno", default = "yes")
			tcl("update")
			
			if (tclvalue(YESNO) == "no") {
				tkdestroy(tt)
				browseURL(CSV)
				source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
			}
		}
		
		DATABIO$SAISON[DATABIO$MOIS<5 | DATABIO$MOIS>10]<-"HIVER"
	
	
	if(MODULE == "REEE2010"){
		#Extraire l'échantillon selon la profondeur pour ChloA(1 = intégré, 2 = fond ) 
		DATABIO$INTEGRE<-as.character(DATABIO$INTEGRE)
		# On retiendra 
		if ( paste(as.character(DATABIO$INTEGRE),collapse = "") == "" ) { INTEGRE_NONDISPO<-"oui" } else { INTEGRE_NONDISPO<-"non" } 
		if (	INTEGRE_NONDISPO == "non") {
			DATABIO$INTEGRE<-as.character(DATABIO$INTEGRE)
			NROWINTEGREOK<-nrow(DATABIO[DATABIO$INTEGRE %in% c("1","2") & DATABIO$PARAMETRE == "1439",])
			PBINTEGRE<-nrow(DATABIO[ DATABIO$PARAMETRE == "1439",]) - NROWINTEGREOK 
			if(PBINTEGRE > 0) {
			YESNO<-tkmessageBox(message = paste( PBINTEGRE,"echantillons Bio(Chlo-a) n'ont pas d'info sur la profondeur \n 1= Intégré\n 2 = Fond\nOui pour poursuivre en supprimant les prélèvements de fond (Intégré = 2)"),
						icon = "question", type = "yesno", default = "yes")
					tcl("update")
					
					if (tclvalue(YESNO) == "no") {
						tkdestroy(tt)
						source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
					}
			}
			DATABIO<-DATABIO[!(DATABIO$INTEGRE == "2" & DATABIO$PARAMETRE == "1439"),]
			
			
		}
	
	}
	
	
	}


	
##vérif que les stations sont bien présente dans la table STATION
	nbstat<-DATABIO[!(DATABIO$STATION %in% STATION$STATION),]
	nbstatfalsebio<-length(unique(nbstat$STATION))
	if (nbstatfalsebio>0 & nrow(nbstat) != nrow(DATABIO)){
		liststatmanQ<-unique(c(liststatmanQ,unique(nbstat$STATION)))
			print(paste("Bio : il manque ", nbstatfalsebio, " stations dans la table STATION.csv",sep=""))
			tcl("update")
	}
	if (nrow(nbstat) == nrow(DATABIO) ){
	MSGBOX <- tkmessageBox(title = "Info", message = paste("Aucun code station des données bio n'est conforme à la table STATION.csv\n L'application va se fermer automatiquement.Veuillez corriger et lancer un nouveau traitement.",sep=""), icon = "info", type = "ok")
	tcl("update")
	source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
	}
}

gc()


#############################Ì
#### VERIFICATION DU TYPEFR
#############################
NB_STATIONS<-nrow(STATION)
if (SEEEBIO == "oui" & CEPE == "CE"){
    STATION$CONF_TYPFR<-"" 
	STATION$CONF_TYPFR[(STATION$TYPEFR %in% GRILLEIBD$TYPEFR ) & STATION$STATION %in% DATABIO$STATION]<-"oui"
	STATION$CONF_TYPFR[!(STATION$TYPEFR %in% GRILLEIBD$TYPEFR ) & STATION$STATION %in% DATABIO$STATION ]<-"non"
	STATION$CONF_TYPFR[ (is.na(STATION$TYPEFR) | STATION$TYPEFR == "") & STATION$STATION %in% DATABIO$STATION ]<-"vide"  ##potentiellement en NQ
	NB_STATIONS_NCONF<-nrow(STATION[STATION$CONF_TYPFR %in% c("vide","non"),])
	NB_STATIONS_POTNQ<-nrow(STATION[STATION$CONF_TYPFR == "vide",])


	if (NB_STATIONS_NCONF > 0 ){ # S'il y a des non-conformes
		YESNO<-tkmessageBox(message = paste(NB_STATIONS_NCONF,"stations ont un TypeFR non conforme aux grilles IBD-IBG-IPR-IBMR, dont ",NB_STATIONS_POTNQ," avec un TypeFR non renseigné\n\n Oui pour poursuivre et ignorer. \n\n Non pour stopper et modifier le fichier STATION.csv "),
			icon = "question", type = "yesno", default = "yes")
			tcl("update")
			if (tclvalue(YESNO) == "no") { # ...l'utilisateur peut décider d'arreter et exporter la table des doublons
				tkdestroy(tt)
				write.csv2(STATION,XLS_STATION)
				MSGBOX <- tkmessageBox(title = "Info", message = paste("Le fichier à modifier est dans \n",XLS_STATION,"\n Ouverture automatique. \n Pensez à enregistrer au format CSV",sep=""), icon = "info", type = "ok")
				tcl("update")
				browseURL(XLS_STATION)
				source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
			} else {
			
				STATION_NCONF<-STATION[STATION$CONF_TYPFR %in% c("vide","non"), c("STATION","TYPEFR","CONF_TYPFR")]
				
				CSV<-paste(CH_ERREUR,"PB_TYPEFR",SEEE_DEBformat,".csv",sep="")
				write.csv2(STATION_NCONF,CSV)
				MSGBOX <- tkmessageBox(title = "Info", message = paste("Le fichier avec l'identification des stations\n ayant un TYPE-FR non conforme\n a été exporté dans : \n",CSV,sep=""), icon = "info", type = "ok")
				tcl("update")
			}
	} 
}
gc()

###############################
## CONFORMITE : table DATAPEGASE
###############################
gc()
nbstatfalsepeg<-" - "
if (SEEEPEGASE == "oui") {
	#DATAPEGASE<-DATAPEGASE[,CBPEGVALUE2]
	names(DATAPEGASE)<-NAMESPEG
	print(dim(DATAPEGASE))
	print(names(DATAPEGASE))
	print(summary(DATAPEGASE))
	
	##as.numeric(as.character(DATAPEGASE$PEGO2)) --> permet de gérer la class factor, sinon modifie le numeric
	DATAPEGASE$STATION<-as.character(DATAPEGASE$STATION) 
	DATAPEGASE$PEGO2<-as.numeric(gsub(",",".",as.character(DATAPEGASE$PEGO2)))
	DATAPEGASE$PEGSATO2<-as.numeric(gsub(",",".",as.character(DATAPEGASE$PEGSATO2)))
	DATAPEGASE$PEGDBO5<-as.numeric(gsub(",",".",as.character(DATAPEGASE$PEGDBO5)))
	DATAPEGASE$PEGCOD<-as.numeric(gsub(",",".",as.character(DATAPEGASE$PEGCOD)))
	DATAPEGASE$PEGPO43<-as.numeric(gsub(",",".",as.character(DATAPEGASE$PEGPO43)))
	DATAPEGASE$PEGPHOS<-as.numeric(gsub(",",".",as.character(DATAPEGASE$PEGPHOS)))
	DATAPEGASE$PEGNH4<-as.numeric(gsub(",",".",as.character(DATAPEGASE$PEGNH4)))
	DATAPEGASE$PEGNO2<-as.numeric(gsub(",",".",as.character(DATAPEGASE$PEGNO2)))
	DATAPEGASE$PEGNO3<-as.numeric(gsub(",",".",as.character(DATAPEGASE$PEGNO3)))
	DATAPEGASE$PEGTEMPE<-as.numeric(gsub(",",".",as.character(DATAPEGASE$PEGTEMPE)))
	DATAPEGASE$PEGPHMIN<-as.numeric(gsub(",",".",as.character(DATAPEGASE$PEGPHMIN)))
	DATAPEGASE$PEGPHMAX<-as.numeric(gsub(",",".",as.character(DATAPEGASE$PEGPHMAX)))

	test_data_vide(DATAPEGASE,"La table importée ne contient aucune donnée PEGASE.")
}


###############################
## CONFORMITE : table DATACHIM
###############################
gc()
if (SEEECHIM == "oui" ){
	test_data_vide(DATACHIM,"Aucune donnée chimique n'a pu être extraite du fichier importé. Vérifiez la conformité de vos données.")
		
### Sélection selon code paramètre et code fraction	
		print(FRACTIONOKCHIM)
	DATACHIM$FRACTION<-as.character(as.numeric(DATACHIM$FRACTION))
	PARAMETRECHIM$FRACTION<-as.character(as.numeric(PARAMETRECHIM$FRACTION))
	 if (FRACTIONOKCHIM == "oui"){
		print(paste("nb lignes DATACHIM avant selection des PARAMETRES et code FRACTION : ",nrow(DATACHIM)))
		DATACHIM<-DATACHIM[paste(DATACHIM$FRACTION,DATACHIM$PARAMETRE,sep="_") %in% paste(PARAMETRECHIM$FRACTION,PARAMETRECHIM$PARAMETRE,sep="_"),]
		print(paste("nb lignes DATACHIM après selection des PARAMETRES et code FRACTION : ",nrow(DATACHIM)))
			
	 } else {
		print(paste("nb lignes DATACHIM avant sélection des PARAMETRES et code FRACTION : ",nrow(DATACHIM)))
		DATACHIM<-DATACHIM[paste(DATACHIM$FRACTION,DATACHIM$PARAMETRE,sep="_") %in% c(paste(3,PARAMETRECHIM$PARAMETRE,sep="_"),paste(23,PARAMETRECHIM$PARAMETRE,sep="_")) ,]
		print(paste("nb lignes DATACHIM après sélection des PARAMETRES et code FRACTION : ",nrow(DATACHIM)))
	}
	test_data_vide(DATACHIM,"Il n'y a pas de donnée chimique après l'extraction selon la fraction et le paramètre. \n Vérifiez la conformité des codes Fractions et Paramètres.")
	gc()
	
#####selection selon l'unité du parametre
	if (UNITEOKCHIM == "oui"){
		DATACHIM$UNITE<-as.character(as.numeric(DATACHIM$UNITE))
		PARAMETREUNITE$UNITE<-as.character(as.numeric(PARAMETREUNITE$UNITE))
		print(paste("nb lignes DATACHIM avant sélection de l'unité : ",nrow(DATACHIM)))
		DATACHIM<-DATACHIM[paste(DATACHIM$UNITE,DATACHIM$PARAMETRE,sep="_") %in% paste(PARAMETREUNITE$UNITE,PARAMETREUNITE$PARAMETRE,sep="_"),]
		print(paste("nb lignes DATACHIM après sélection de l'unité : ",nrow(DATACHIM)))
	}	
	test_data_vide(DATACHIM,"Il n'y a pas de donnée chimique après l'extraction selon l'unité. Vérifiez la conformité des unités.")
	gc()
	
### Choix et respect du code REMARQUE	
	DATACHIM<-DATACHIM[DATACHIM$REMARQUE %in% LISTECODERQE$REMARQUE,]	
	test_data_vide(DATACHIM,"Il n'y a pas de donnée chimique après l'extraction selon le code remarque. Vérifiez la conformité des codes remarque.")

### Retrait des prélèvement avec LQ manquantes
	if (LQ_NONDISPO=="non") {
		NCHIM<-nrow(DATACHIM)
		NCHIM2<-nrow(DATACHIM[!is.na(DATACHIM$LQ),])
		NBLQMANQ_CHIM<-NCHIM - NCHIM2
		if ( NCHIM - NCHIM2 != 0) {
		PBLQ_DATAPOLSPE<-DATACHIM[is.na(DATACHIM$LQ),]
		DATACHIM<-DATACHIM[!is.na(DATACHIM$LQ),]
		MSGBOX <- tkmessageBox(title = "Info", message = paste(NCHIM - NCHIM2, " prélèvements n'ont pas de LQ renseignée pour le calcul de l'état Chimique\n Ces prélèvements ne seront pas utilisés dans le calcul",sep=""), icon = "info", type = "ok")
		tcl("update")
		CSV<-paste(CH_ERREUR,"Prélevements_LQ_Manq_",SEEE_DEBformat,".csv",sep="")
		write.csv2(PBLQ_DATAPOLSPE,CSV)
		MSGBOX <- tkmessageBox(title = "Info", message = paste("Les prélèvements sans LQ ont été exportés : \n",CSV,sep=""), icon = "info", type = "ok")	
		tcl("update")
		}
	}
	gc()
##vérif que les stations sont bien présentes dans la table STATION
	nbstatchim<-DATACHIM[!(DATACHIM$STATION %in% STATION$STATION),]
	nbstatfalsechim<-length(unique(nbstatchim$STATION))
	if (nbstatfalsechim>0 & nrow(nbstatchim) != nrow(DATACHIM)){
	liststatmanQ<-unique(c(liststatmanQ,unique(nbstatchim$STATION)))
	print(paste("CHIMIE : il manque ", nbstatfalsechim, " stations dans la table STATION",sep=""))
	tcl("update")
	}
	if (nrow(nbstatchim) == nrow(DATACHIM) ){
		MSGBOX <- tkmessageBox(title = "Info", message = paste("Aucun code station des données chimiques n'est conforme à la table STATION.csv\n L'application va se fermer automatiquement.Veuillez corriger et lancer un nouveau traitement.",sep=""), icon = "info", type = "ok")
		tcl("update")
		source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
	}
}
gc()

if (CONTA == "oui" ){
	test_data_vide(DATACONTA,"Aucune donnée chimique n'a pu être extraite du fichier importé. Vérifiez la conformité de vos données.")
		
### Sélection selon code paramètre et code fraction	
		print(FRACTIONOKCONTA)
	DATACONTA$FRACTION<-as.character(as.numeric(DATACONTA$FRACTION))
	PARAMETRECONTA$FRACTION<-as.character(as.numeric(PARAMETRECONTA$FRACTION))
	 if (FRACTIONOKCONTA == "oui"){
		print(paste("nb lignes DATACONTA avant selection des PARAMETRES et code FRACTION : ",nrow(DATACONTA)))
		DATACONTA<-DATACONTA[paste(DATACONTA$FRACTION,DATACONTA$PARAMETRE,sep="_") %in% paste(PARAMETRECONTA$FRACTION,PARAMETRECONTA$PARAMETRE,sep="_"),]
		print(paste("nb lignes DATACONTA après selection des PARAMETRES et code FRACTION : ",nrow(DATACONTA)))
		LISTPARAM_CONTA_APRES<-sort(unique(DATACONTA$PARAMETRE))
		if( 	length(LISTPARAM_CONTA_APRES) < length(LISTPARAM_CONTA_AVANT)) {
			MSGBOX <- tkmessageBox(title = "Info", message = paste("Le fichier des données brutes contient ",length(LISTPARAM_CONTA_AVANT) , 
			" paramètres \n Seuls ", length(LISTPARAM_CONTA_APRES), " seront traités après mise en cohérence avec GRILLECONTAMINATION.csv 
			(PARAMETRE et FRACTION) ",sep=""), icon = "info", type = "ok")
		}
	 } else {
		print(paste("nb lignes DATACONTA avant sélection des PARAMETRES et code FRACTION : ",nrow(DATACONTA)))
		DATACONTA<-DATACONTA[paste(DATACONTA$FRACTION,DATACONTA$PARAMETRE,sep="_") %in% c(paste(3,PARAMETRECONTA$PARAMETRE,sep="_"),paste(23,PARAMETRECONTA$PARAMETRE,sep="_")) ,]
		print(paste("nb lignes DATACONTA après sélection des PARAMETRES et code FRACTION : ",nrow(DATACONTA)))
		LISTPARAM_CONTA_APRES<-sort(unique(DATACONTA$PARAMETRE))
		if( 	length(LISTPARAM_CONTA_APRES) < length(LISTPARAM_CONTA_AVANT)) {
			MSGBOX <- tkmessageBox(title = "Info", message = paste("Le fichier des données brutes contient ",length(LISTPARAM_CONTA_AVANT) , 
			" paramètres \n Seuls ", length(LISTPARAM_CONTA_APRES), " seront traités après mise en cohérence avec GRILLECONTAMINATION.csv 
			",sep=""), icon = "info", type = "ok")
		}
	}
	test_data_vide(DATACONTA,"Il n'y a pas de donnée chimique après l'extraction selon la fraction et le paramètre. \n Vérifiez la conformité des codes Fractions et Paramètres.")
	gc()
#####selection selon l'unité du parametre

	if (UNITEOKCONTA == "oui") {
		if (file.exists(paste(ch_param,"UNITE_REF_EAU.csv",sep="")) ){
		UNITE_REF_EAU<-read.csv2(paste(ch_param,"UNITE_REF_EAU.csv",sep=""))
		}

		NAMESDATACONTA<-names(DATACONTA)
		DATACONTA<-merge(DATACONTA,UNITE_REF_EAU[,c("CDUNITESANDRE","GPUNITE")], by.x = "UNITE", by.y="CDUNITESANDRE", all.x=TRUE)

		condna<-!is.na(DATACONTA$GPUNITE)
		DATACONTA$RESULTAT[ condna & DATACONTA$GPUNITE == 'KG']<-DATACONTA$RESULTAT[ condna & DATACONTA$GPUNITE == 'KG']*10^9
		DATACONTA$RESULTAT[ condna & DATACONTA$GPUNITE == 'G']<-DATACONTA$RESULTAT[ condna & DATACONTA$GPUNITE == 'G']*10^6
		DATACONTA$RESULTAT[ condna & DATACONTA$GPUNITE == 'MG']<-DATACONTA$RESULTAT[ condna & DATACONTA$GPUNITE == 'MG']*10^3
		DATACONTA$RESULTAT[ condna & DATACONTA$GPUNITE == 'NG']<-DATACONTA$RESULTAT[ condna & DATACONTA$GPUNITE == 'NG']*10^(-3)
		DATACONTA$UNITE[ DATACONTA$GPUNITE %in% c('KG','G','MG','NG')]<-'133'
		print("répartition par unité")
		print(table(DATACONTA$GPUNITE, exclude = NULL))
		gc()
		
		##Nombre de données d'analyse ayant subie une conversion
		UNITE_PAS_MICROG<-nrow(DATACONTA[condna & DATACONTA$GPUNITE %in% c('KG','G','MG','NG'),])
		if ( UNITE_PAS_MICROG > 0) {

			MSGBOX <- tkmessageBox(title = "Info", message = paste(UNITE_PAS_MICROG, "analyses  ont été converties  en microgramme/L (133)" ), icon = "info", type = "ok")	
			tcl("update")
			}

		##Unité mécones qui ne sont pas dans UNITE_REF_EAU.csv.	
		UNITENONCONNUES<-unique(	DATACONTA$UNITE[is.na(DATACONTA$GPUNITE)])
			if ( length(UNITENONCONNUES) > 0) {

			MSGBOX <- tkmessageBox(title = "Info", message = paste(UNITENONCONNUES, " ne sont pas connues\n cf PARAMETRES/UNITE_REF_EAU.csv \n Elles seront prises en compte comme des microgrammes/L" ), icon = "info", type = "ok")	
			tcl("update")
			}		
		DATACONTA<-DATACONTA[,NAMESDATACONTA]
	}
### Choix et respect du code REMARQUE	
	DATACONTA<-DATACONTA[DATACONTA$REMARQUE %in% LISTECODERQE$REMARQUE,]
	LISTPARAM_CONTA_APRES2<-sort(unique(DATACONTA$PARAMETRE))	
	test_data_vide(DATACONTA,"Il n'y a pas de donnée chimique après l'extraction selon le code remarque. Vérifiez la conformité des codes remarque.")

### Retrait des prélèvement avec LQ manquantes
	if (LQ_NONDISPO=="non") {
		NCHIM<-nrow(DATACONTA)
		NCHIM2<-nrow(DATACONTA[!is.na(DATACONTA$LQ),])
		NBLQMANQ_CHIM<-NCHIM - NCHIM2
		if ( NCHIM - NCHIM2 != 0) {
		PBLQ_DATAPOLSPE<-DATACONTA[is.na(DATACONTA$LQ),]
		DATACONTA<-DATACONTA[!is.na(DATACONTA$LQ),]
		MSGBOX <- tkmessageBox(title = "Info", message = paste(NCHIM - NCHIM2, " prélèvements n'ont pas de LQ renseignée\n Ces prélèvements ne seront pas utilisés dans le calcul",sep=""), icon = "info", type = "ok")
		tcl("update")
		CSV<-paste(CH_ERREUR,"Prélevements_LQ_Manq_",SEEE_DEBformat,".csv",sep="")
		write.csv2(PBLQ_DATAPOLSPE,CSV)
		MSGBOX <- tkmessageBox(title = "Info", message = paste("Les prélèvements sans LQ ont été exportés : \n",CSV,sep=""), icon = "info", type = "ok")	
		tcl("update")
		}
	}
gc()	
##vérif que les stations sont bien présentes dans la table STATION

	STATION$STATION[substr(STATION$STATION,1,1) == "0" & nchar(STATION$STATION) > 4]<-as.character(as.numeric(STATION$STATION[substr(STATION$STATION,1,1) == "0" & nchar(STATION$STATION) > 4]))
	DATACONTA$STATION[substr(DATACONTA$STATION,1,1) == "0" & nchar(DATACONTA$STATION) > 4]<-as.character(as.numeric(DATACONTA$STATION[substr(DATACONTA$STATION,1,1) == "0" & nchar(DATACONTA$STATION) > 4]))

	nbstatchim<-DATACONTA[!(DATACONTA$STATION %in% STATION$STATION),]
	nbstatfalsechim<-length(unique(nbstatchim$STATION))
	if (nbstatfalsechim>0 & nrow(nbstatchim) != nrow(DATACONTA)){
	liststatmanQ<-unique(c(liststatmanQ,unique(nbstatchim$STATION)))
	print(paste("CHIMIE : il manque ", nbstatfalsechim, " stations dans la table STATION",sep=""))
	tcl("update")
	}
	if (nrow(nbstatchim) == nrow(DATACONTA) ){
		MSGBOX <- tkmessageBox(title = "Info", message = paste("Aucun code station des données brutes n'est conforme à la table STATION.csv\n L'application va se fermer automatiquement.Veuillez corriger et lancer un nouveau traitement.",sep=""), icon = "info", type = "ok")
		tcl("update")
		source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
	}
}
gc()


###############################
## Chargement de MASSEDEAU
###############################
if ((SEEEECOLOME=="oui" | SEEECHIMME=="oui"  |  CONTAME == "oui") & CEPE == "CE") {
MASSEDEAU <- read.csv2(XLS_MASSEDEAU)
test_data_vide(MASSEDEAU,"La table MASSEDEAU ne contient aucune donnée valide..")

 # vérifie les noms de colonnes
NAMES_ATTENDU<-c("EUCD", "NAME", "MODIFIED", "ARTIFICIAL","ECHELLEME")
NAMES_ATTENDU_LISTTXT<-paste("EUCD", "NAME", "MODIFIED", "ARTIFICIAL","ECHELLEME", sep = ",")
NAMES_MASSEDEAU<-names(MASSEDEAU)
NAMES_MASSEDEAU<-NAMES_MASSEDEAU[NAMES_MASSEDEAU %in% NAMES_ATTENDU]
if (length(NAMES_MASSEDEAU) != length(NAMES_ATTENDU)){
	tkmessageBox(title = "Info", message = paste("Les noms de colonnes de MASSEDEAU.csv ne sont pas ceux attendus\n Les colonnes attendues sont:\n",NAMES_ATTENDU_LISTTXT,sep=" "), icon = "info", type = "ok")
	tcl("update")
	source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
	}

MASSEDEAU$EUCD<-as.character(MASSEDEAU$EUCD)
MASSEDEAU$NAME<-as.character(MASSEDEAU$NAME)
MASSEDEAU$MODIFIED<-as.character(MASSEDEAU$MODIFIED)
MASSEDEAU$ARTIFICIAL<-as.character(MASSEDEAU$ARTIFICIAL)
MASSEDEAU$ECHELLEME<-gsub("DT","",toupper(as.character(MASSEDEAU$ECHELLEME)))

MASSEDEAU<-MASSEDEAU[, names(MASSEDEAU) %in% c("EUCD", "NAME", "MODIFIED", "ARTIFICIAL", "RGLOBAL", "RMACRO", "RNIT", "RPEST", "RMICRO", "RMORPHO", "RHYDRO", "RPHOS","ECHELLEME")]

if (BASSIN=="AELB") {
		# vérifie les noms de colonnes
		NAMES_ATTENDU<-c("EUCD", "NAME", "MODIFIED", "ARTIFICIAL", "RGLOBAL", "RMACRO", "RNIT", "RPEST", "RMICRO", "RMORPHO", "RHYDRO", "RPHOS","ECHELLEME")
		NAMES_ATTENDU_LISTTXT<-paste("EUCD", "NAME", "MODIFIED", "ARTIFICIAL", "RGLOBAL", "RMACRO", "RNIT", "RPEST", "RMICRO", "RMORPHO", "RHYDRO", "RPHOS","ECHELLEME", sep = ",")
		NAMES_MASSEDEAU<-names(MASSEDEAU)
		NAMES_MASSEDEAU<-NAMES_MASSEDEAU[NAMES_MASSEDEAU %in% NAMES_ATTENDU]
		if (length(NAMES_MASSEDEAU) != length(NAMES_ATTENDU)){
			tkmessageBox(title = "Info", message = paste("Les noms de colonnes de MASSEDEAU.csv ne sont pas ceux attendus\n Sur le bassin de l'AELB, Les colonnes attendues sont:\n",NAMES_ATTENDU_LISTTXT,sep=" "), icon = "info", type = "ok")
			tcl("update")
			source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
			}

	MASSEDEAU$RGLOBAL<-as.character(MASSEDEAU$RGLOBAL)
	MASSEDEAU$RMACRO<-as.character(MASSEDEAU$RMACRO)
	MASSEDEAU$RNIT<-as.character(MASSEDEAU$RNIT)
	MASSEDEAU$RPEST<-as.character(MASSEDEAU$RPEST)
	MASSEDEAU$RMICRO<-as.character(MASSEDEAU$RMICRO)
	MASSEDEAU$RMORPHO<-as.character(MASSEDEAU$RMORPHO)
	MASSEDEAU$RHYDRO<-as.character(MASSEDEAU$RHYDRO)
	MASSEDEAU$RPHOS<-as.character(MASSEDEAU$RPHOS)
}

MASSEDEAU$TYPEME[tolower(substr(MASSEDEAU$MODIFIED,1,1)) %in% c("y","o")]<-"MEFM"
MASSEDEAU$TYPEME[tolower(substr(MASSEDEAU$MODIFIED,1,1)) %in% c("n") & tolower(substr(MASSEDEAU$ARTIFICIAL,1,1)) %in% c("y","o")]<-"MEA"
MASSEDEAU$TYPEME[tolower(substr(MASSEDEAU$MODIFIED,1,1)) %in% c("n") & tolower(substr(MASSEDEAU$ARTIFICIAL,1,1)) %in% c("n") ]<-"MEN"
gc()

	#controle qu'il n'y a pas de doublons dans la table MASSEDEAU
	if ( nrow(MASSEDEAU) != length(unique(MASSEDEAU$EUCD))  ){
		STATIONDUPLIQ<-data.frame(table(MASSEDEAU$EUCD))[table(MASSEDEAU$EUCD)>1,]
		names(STATIONDUPLIQ)<-c("EUCD","FREQ")
		
		CSV<-paste(CH_ERREUR,"DoublonsME_",SEEE_DEBformat,".csv",sep="")
		write.csv2(STATIONDUPLIQ,CSV)
		tkmessageBox(title = "Info", message = paste("Il y a des doublons de numéros EUCD dans la table MASSEDEAU.csv \n Les doublons ont été exportés dans : \n", CSV,
		"\n L'application va se fermer automatiquement",sep=" "), icon = "info", type = "ok")
		tcl("update")
		source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit

	}
}

if ((SEEEECOLOME=="oui" | SEEECHIMME=="oui" | CONTAME == "oui") & CEPE == "PE") {
	MASSEDEAU <- STATION[!duplicated(STATION[,"EUCD"]),c("EUCD","NOMME","TYPEME","ECHELLESTA")]
	names(MASSEDEAU)<-c("EUCD", "NAME","TYPEME","ECHELLEME")

	typeattendu<-c("MEN","MEA","MEFM",NA,"")
	LSTTYPME<-MASSEDEAU$TYPEME
	if (length(LSTTYPME[!(LSTTYPME %in% typeattendu)]) > 0) {
			ERRORMETYPE<-paste(unique(LSTTYPME[!(LSTTYPME %in% typeattendu)]),collapse = ",")
			tkmessageBox(title = "Info", message = paste("Il y a des type de masse d'eau innatendu \n", ERRORMETYPE ," \n",
			"Les types attendus sont MEN, MEA et MEFM. \n Le risque d'erreur est possible pour l'agrégation à la masse d'eau" , sep=" "), icon = "info", type = "ok")
			tcl("update")
			
	}

	MASSEDEAU$MODIFIED<-"non"
	MASSEDEAU$ARTIFICIAL<-"non"
	MASSEDEAU$MODIFIED[MASSEDEAU$TYPEME == "MEFM"]<-"oui"
	MASSEDEAU$MODIFIED[MASSEDEAU$ARTIFICIAL == "MEA"]<-"oui"


}




####################################################
## VECTEURS DE SELECTION DES CHAMPS
## Astuce nécessaire au niveau de l'état ecolo selon les colonnes calculées (et donc non créées--> bugs) & pour l'export final des résultats.
#####################################################
# VECTEUR STATION
STATION_NAMES<-names(STATION[,2:ncol(STATION)]) # on conserve l'ensemble des colonnes, sauf la 1ere qui est le code STATION.

# VECTEUR MASSE D'EAU
if (SEEEECOLOME=="oui") {
MASSEDEAU_NAMES<-names(MASSEDEAU[,3:ncol(MASSEDEAU)]) # on conserve l'ensemble des colonnes, sauf les 2 premieres (EUCD et NAME).
}

# VECTEURS GLOBAUX pour le tri/affichage des colonnes. Si le vecteur est vide, la colonne ne sera pas exportée. Les vecteurs sont "remplis" selon les IF.
PARAMETREPCH_NAMES<-as.character()
vPARAMETREPCH_NAMES<-as.character() # valeur moyenne
iPARAMETREPCH_NAMES<-as.character() # indice
fPARAMETREPCH_NAMES<-as.character() # frequence prelevement
fdPARAMETREPCH_NAMES<-as.character() # frequence prelevement declassement
PARAMETREPCHELT_NAMES<-as.character()
iPARAMETREPCHELT_NAMES<-as.character()
ETATPCH_NAMES<-as.character()
iETATPCH_NAMES<-as.character()
PCHDECLASS_NAMES<-as.character() # liste des PCH déclassants
N_PCHDECLASS_NAMES<-as.character() # nombre des parametres PCH déclassants
PCHDECLASSASSOUP_NAMES<-as.character() # liste des PS déclassants assouplis
N_PCHASSOUP_NAMES<-as.character() # nombre des parametres PCH déclassants assouplis
MODELISE_NAMES<-as.character()
PARAMETREBIO_NAMES<-as.character()
vPARAMETREBIO_NAMES<-as.character() # valeur moyenne
eqrPARAMETREBIO_NAMES<-as.character() # valeur eqr
iPARAMETREBIO_NAMES<-as.character() # indice
fPARAMETREBIO_NAMES<-as.character() # frequence prelevement
QUARTCLASSE_NAMES<-as.character()
ETATBIO_NAMES<-as.character()
iETATBIO_NAMES<-as.character()
NBBIO_NAMES<-as.character()  # nb indicateur bio et liste
PARAMETREPOLLUANT_NAMES<-as.character()
PARAMETREPS_NAMES<-as.character()
vPARAMETREPS_NAMES<-as.character() # valeur moyenne
iPARAMETREPS_NAMES<-as.character() # indice
fPARAMETREPS_NAMES<-as.character() # frequence prelevement
aPARAMETREPS_NAMES<-as.character() # année retenue pour moyenne
ETATPS_NAMES<-as.character()
iETATPS_NAMES<-as.character()
ETATECOLOHPS_NAMES<-as.character()
iETATECOLO_NAMES<-as.character() # indice de l'état ecolo HPS (spécifique AESN)
iETATECOLOHPS_NAMES<-as.character() # indice de l'état ecolo HPS (spécifique AESN)
PSDECLASS_NAMES<-as.character() # liste des PS déclassants
N_PSDECLASS_NAMES<-as.character() # nombre de parametres PS déclassants
BIODECLASS_NAMES<-as.character() # liste des Bio déclassants
N_BIODECLASS_NAMES<-as.character() # nombre de parametres Bio déclassants
ETATECOLO_NAMES<-as.character()
ETATECOLOME_NAMES<-as.character()
ETATECOLOMEPE_NAMES<-as.character()
ASSOUPLI_NAMES<-as.character()
ECOLODECLASS_NAMES<-as.character()
N_ECODECLASS_NAMES<-as.character()
ECOLOORIGINDECLASS_NAMES<-as.character()
N_ECOLOORIGINDECLASS_NAMES<-as.character()
ETATECOLOME_NAMES<-as.character()
NIVEAU_NAMES<-as.character()
CAS_NAMES<-as.character()
ETATBIOME_NAMES<-as.character()
ETATPCHME_NAMES<-as.character()
ETATPSME_NAMES<-as.character()
ETATECOLOHPSME_NAMES<-as.character()
tcl("update")
gc()
#############
## ETAT PCH
#############
if (SEEEPCH=="oui") {
	# Table des ELEMENTS QUALITE
	TABLEELT<-aggregate(IDTRI ~ ELTQUALITE, data = PARAMETREPCH , min)
	TABLEELT<-TABLEELT[order(TABLEELT$IDTRI),]
	iPARAMETREPCHELT_NAMES<-paste("i",toupper(TABLEELT$ELTQUALITE),sep="")

	#VECTEURS PCH pour sélection des CHAMPS (ETAT ECOLO & EXPORT FINAL)
	PARAMETREPCH<-PARAMETREPCH[order(PARAMETREPCH$IDTRI),] # tri uniquement pour affichage des colonnes dans le bon ordre
	if (CEPE == "CE") PARAMETREPCH_NAMES<-PARAMETREPCH$NOM
	if (CEPE == "PE") PARAMETREPCH_NAMES<-unique(PARAMETREPCH$PARAGROUP)
	vPARAMETREPCH_NAMES<-paste("v",toupper(PARAMETREPCH_NAMES),sep="")
	iPARAMETREPCH_NAMES<-paste("i",toupper(PARAMETREPCH_NAMES),sep="")
	fPARAMETREPCH_NAMES<-paste("f",toupper(PARAMETREPCH_NAMES),sep="")
	fdPARAMETREPCH_NAMES<-paste("fd",toupper(PARAMETREPCH_NAMES),sep="")
	PARAMETREPCHELT_NAMES<-TABLEELT$ELTQUALITE
	if (CEPE == "CE") ETATPCH_NAMES<-"ETATPCH" 
	if (CEPE == "CE" & EXCEPTLOC == "oui") ETATPCH_NAMES<-c("ETATPCH" ,"ETATPCHssExceptloc")
	if (CEPE == "PE") ETATPCH_NAMES<-c("ETATPCH" ,"ETATPCH_SSBILANO2")
	if (CEPE == "CE") MODELISE_NAMES<-"MODELISE"
}
gc()
##############################
## ETAT POLLUANTS SPECIFIQUES
##############################
if (SEEEPS=="oui") {
	# Table des ELEMENTS POLLUANTS SPECIFIQUES
	TABLEPOL<-aggregate(IDTRI ~ POLLUANT, data = PARAMETREPOLSPE , min)
	TABLEPOL<-TABLEPOL[order(TABLEPOL$IDTRI),]
	
	# VECTEURS PS pour sélection des CHAMPS (ETAT ECOLO & EXPORT FINAL)
	PARAMETREPOLLUANT_NAMES<-TABLEPOL$POLLUANT
	PARAMETREPOLSPE<-PARAMETREPOLSPE[order(PARAMETREPOLSPE$IDTRI),] # tri uniquement pour affichage des colonnes dans le bon ordre
	PARAMETREPS_NAMES<-PARAMETREPOLSPE$PARAMETRELIB
	vPARAMETREPS_NAMES<-paste("v",toupper(PARAMETREPOLSPE$PARAMETRELIB),sep="")
	iPARAMETREPS_NAMES<-paste("i",toupper(PARAMETREPOLSPE$PARAMETRELIB),sep="")
	fPARAMETREPS_NAMES<-paste("f",toupper(PARAMETREPOLSPE$PARAMETRELIB),sep="")
	aPARAMETREPS_NAMES<-paste("a",toupper(PARAMETREPOLSPE$PARAMETRELIB),sep="")
	ETATPS_NAMES<-"ETATPS"
	ETATPSME_NAMES<-"ETATPSME" # sert uniquement si ETAT ECOLO ME est à faire !
}
gc()
####################
## ETAT BIOLOGIQUE
####################
if (SEEEBIO=="oui") {
	# Table des INDICATEURS BIO
	TABLEBIO<-aggregate(IDTRI ~ PARAGROUP + PARALIBGROUP, data = PARAMETREBIO , min)
	# modifié le 10/12/14 pour intégrer IBMR en 5eme position
	TABLEBIO<-TABLEBIO[order(TABLEBIO$IDTRI),]
	
	
	if (BASSIN %in% c("AESN","AERC","AELB","AEAG") ){
	TABLEBIO<-TABLEBIO[as.character(TABLEBIO$PARAGROUP) != "IBMA",]
	}
	if (MODULE == "REEE2018" & CEPE == "CE" ){
	TABLEBIO<-TABLEBIO[ TABLEBIO$PARALIBGROUP %in% names(PARAMBIOTYPEFR),]
	}
	
	#VECTEURS BIO pour sélection des CHAMPS (ETAT ECOLO & EXPORT FINAL)
	PARAMETREBIO_NAMES<-TABLEBIO$PARALIBGROUP
	vPARAMETREBIO_NAMES<-paste("v",toupper(TABLEBIO$PARALIBGROUP),sep="")
	if(MODULE == "REEE2016" | MODULE == "REEE2018"){
	eqrPARAMETREBIO_NAMES<-paste("eqr",toupper(TABLEBIO$PARALIBGROUP),sep="")
	}
	iPARAMETREBIO_NAMES<-paste("i",toupper(TABLEBIO$PARALIBGROUP),sep="")
	fPARAMETREBIO_NAMES<-paste("f",toupper(TABLEBIO$PARALIBGROUP),sep="")
	ETATBIO_NAMES<-"ETATBIO"
}
gc()
####################
## ETAT ECOLOGIQUE
####################
if (SEEEBIO=="oui" & SEEEPCH=="oui" ) {
# VECTEURS ETATECOLO pour sélection des CHAMPS (ETAT ECOLO & EXPORT FINAL)
 ETATECOLOHPS_NAMES<-"ETATECOLOHPS"
 ETATECOLO_NAMES<-"ETATECOLO"
	if (CEPE == "PE") {
		ETATECOLO_NAMES<-c("ETATECOLO","ETATECOLOHBO2")
		ETATECOLOHPS_NAMES<-c("ETATECOLOHPS","ETATECOLOHPSetBO2")
		}
	if (CEPE == "CE") {ASSOUPLI_NAMES<-"ASSOUPLI"}

# VECTEURS ETATECOLO ME pour sélection des CHAMPS (ETAT ECOLO & EXPORT FINAL)
 	if (SEEEECOLOME=="oui") {
		ETATECOLOME_NAMES<-"ETATECOLOME"
		ETATECOLOMEPE_NAMES<-"ETATECOLO"
		NIVEAU_NAMES<-"NIVEAU"
		CAS_NAMES<-"CAS"
		ETATBIOME_NAMES<-"ETATBIOME"
		ETATPCHME_NAMES<-"ETATPCHME"
		if (EXCEPTLOC == "oui") {
		ETATPCHME_NAMES<-c("ETATPCHME","ETATPCHMEssExceptloc")
		}
		ETATECOLOHPSME_NAMES<-"ETATECOLOHPSME"
	}
}
gc()
####################
## ETAT CHIMIQUE
####################
if (SEEECHIM=="oui") {
	# Table des FAMILLES
	TABLEFAMILLE<-aggregate(IDTRI ~ FAMILLE, data = PARAGROUPCHIM , min)
	TABLEFAMILLE<-TABLEFAMILLE[order(TABLEFAMILLE$IDTRI),]
	
}
gc()
tcl("update")
###########################################
### EXPORT DES STATIONS MANQUANTES 
############################################
nbstatmanQ<-length(liststatmanQ)
if (nbstatmanQ > 0) {
		YESNO<-tkmessageBox(message = paste(length(liststatmanQ)," stations issues des données importées ne sont pas dans la table STATION.csv. \n Oui pour poursuivre et ignorer l'erreur. \n Non pour stopper et visualiser la liste des stations"),
				icon = "question", type = "yesno", default = "yes")
			tcl("update")
			#export des stations manquantes même si l'utilisateur veut poursuivre le traitement
			liststatmanQ_DF<-data.frame(sort(liststatmanQ))
			names(liststatmanQ_DF)<-"STATION"
			
			CSV<-paste(CH_ERREUR,"StationsManquantes_",SEEE_DEBformat,".csv",sep="")
			write.csv2(liststatmanQ_DF,CSV)
			MSGBOX <- tkmessageBox(title = "Info", message = paste("La liste des stations manquantes a été exportée : \n",CSV,sep=""), icon = "info", type = "ok")
			tcl("update")
			
			if (tclvalue(YESNO) == "no") { # ...l'utilisateur peut décider d'arreter et exporter la liste des stations manquantes
				tkdestroy(tt)
				browseURL(CSV)
				source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
			}
}
gc()
tcl("update")
###########################################################################
### Recherche de doublons.      ndoubl = nombre de doublons
###########################################################################

if (SEEEPCH=="oui" ) {
			gc()
			# on réordonne le résultat pour conserver le plus declassant après suppresion des doublons
			condexeptpch<-DATAPCH$PARAMETRE  %in%  c("1311", "1312")
			DATAPCH$RESULTAT[condexeptpch]<-DATAPCH$RESULTAT[condexeptpch] * (-1) #exeption 02, sat02
			DATAPCH<-DATAPCH[order(DATAPCH$STATION,DATAPCH$PARAMETRE,DATAPCH$DATEPRELEV, DATAPCH$HEUREPRELEV, -DATAPCH$RESULTAT),]
			condexeptpch<-DATAPCH$PARAMETRE  %in%  c("1311", "1312")
			DATAPCH$RESULTAT[condexeptpch]<-DATAPCH$RESULTAT[condexeptpch] * (-1)
			# on indentifie des doublons
			DATAPCH_doublons<-DATAPCH
			DATAPCH_doublons$ID<-as.numeric(as.factor(paste(DATAPCH_doublons$STATION,DATAPCH_doublons$PARAMETRE,DATAPCH_doublons$DATEPRELEV,DATAPCH_doublons$HEUREPRELEV,DATAPCH_doublons$FRACTION)  ))
			DATAPCH_doublons$DOUBLON<-""
			DATAPCH_doublons$DOUBLON[duplicated(DATAPCH[,c("STATION","PARAMETRE","DATEPRELEV","HEUREPRELEV","FRACTION"  )])]<-"2. oui" 
			IDDOUBLON<-unique(DATAPCH_doublons$ID[DATAPCH_doublons$DOUBLON == "2. oui"] )
			DATAPCH_doublons$DOUBLON[DATAPCH_doublons$DOUBLON == "" & DATAPCH_doublons$ID %in% IDDOUBLON ]<-"1. oui à conserver ? " 
			DATAPCH_doublons$ID[DATAPCH_doublons$DOUBLON == "" ]<-NA
			DATAPCH_doublons<-DATAPCH_doublons[order(DATAPCH_doublons$STATION,DATAPCH_doublons$PARAMETRE,DATAPCH_doublons$DATEPRELEV,
			DATAPCH_doublons$HEUREPRELEV,DATAPCH_doublons$FRACTION,DATAPCH_doublons$DOUBLON),]
			ndoubl<-nrow(DATAPCH) - nrow(DATAPCH_doublons[DATAPCH_doublons$DOUBLON == "",])
			print(nrow(DATAPCH))
			print(nrow(DATAPCH_doublons))
			print(nrow(DATAPCH_doublons[as.character(DATAPCH_doublons$DOUBLON) == "",]))

			if (ndoubl> 0 ){ # S'il ya des doublons...
			YESNO<-tkmessageBox(message = paste(ndoubl,"doublons ont été identifiés dans les données physicochimiques. \n Oui pour poursuivre et supprimer automatiquement les doublons. \n Non pour stopper et visualiser les doublons."),
				icon = "question", type = "yesno", default = "yes")
				tcl("update")
				#export du csv des doublons même si l'utilisateur veut continuer
				
				CSV<-paste(CH_ERREUR,"DoublonsPCH_",SEEE_DEBformat,".csv",sep="")
				write.csv2(DATAPCH_doublons,CSV)
				MSGBOX <- tkmessageBox(title = "Info", message = paste("Les données avec l'identification des doublons ont été exportées : \n",CSV,sep=""), icon = "info", type = "ok")
				tcl("update")
				
				if (tclvalue(YESNO) == "no") { # ...l'utilisateur peut décider d'arreter et exporter la table des doublons
					tkdestroy(tt)
					browseURL(CSV)
					source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
				}
			}
			ndoublpch<-ndoubl  ## permettra d'afficher le nombre de doublons dans le fichier Excel final : voir SEEE.r
			DATAPCH<-DATAPCH[!duplicated(DATAPCH[,c("STATION","PARAMETRE","DATEPRELEV","HEUREPRELEV","FRACTION"  )]),]
			rm(DATAPCH_doublons)	
			tcl("update")
}
gc()

if (SEEEPS=="oui" ) {
			# on réordonne le résultat pour conserver le plus declassant après suppresion des doublons
			DATAPOLSPE<-DATAPOLSPE[order(DATAPOLSPE$STATION,DATAPOLSPE$PARAMETRE,DATAPOLSPE$DATEPRELEV, DATAPOLSPE$HEUREPRELEV, -DATAPOLSPE$RESULTAT),]
			# on indentifie des doublons
			gc()
			DATAPOLSPE_doublons<-DATAPOLSPE
			DATAPOLSPE_doublons$ID<-as.numeric(as.factor(paste(DATAPOLSPE_doublons$STATION,DATAPOLSPE_doublons$PARAMETRE,DATAPOLSPE_doublons$DATEPRELEV,DATAPOLSPE_doublons$HEUREPRELEV,DATAPOLSPE_doublons$FRACTION)  ))
			DATAPOLSPE_doublons$DOUBLON<-""
			DATAPOLSPE_doublons$DOUBLON[duplicated(DATAPOLSPE[,c("STATION","PARAMETRE","DATEPRELEV","HEUREPRELEV","FRACTION"  )])]<-"2. oui" 
			IDDOUBLON<-unique(DATAPOLSPE_doublons$ID[DATAPOLSPE_doublons$DOUBLON == "2. oui"] )
			DATAPOLSPE_doublons$DOUBLON[DATAPOLSPE_doublons$DOUBLON == "" & DATAPOLSPE_doublons$ID %in% IDDOUBLON ]<-"1. oui à conserver ? " 
			DATAPOLSPE_doublons$ID[DATAPOLSPE_doublons$DOUBLON == "" ]<-NA
			DATAPOLSPE_doublons<-DATAPOLSPE_doublons[order(DATAPOLSPE_doublons$STATION,DATAPOLSPE_doublons$PARAMETRE,DATAPOLSPE_doublons$DATEPRELEV,
			DATAPOLSPE_doublons$HEUREPRELEV,DATAPOLSPE_doublons$FRACTION,DATAPOLSPE_doublons$DOUBLON),]
			ndoubl<-nrow(DATAPOLSPE) - nrow(DATAPOLSPE_doublons[DATAPOLSPE_doublons$DOUBLON == "",])
			print(nrow(DATAPOLSPE))
			print(nrow(DATAPOLSPE_doublons))
			print(nrow(DATAPOLSPE_doublons[as.character(DATAPOLSPE_doublons$DOUBLON) == "",]))			

			if (ndoubl> 0 ){ # S'il ya des doublons...
			YESNO<-tkmessageBox(message = paste(ndoubl,"doublons ont été identifiés dans les données des polluants spécifiques. \n Oui pour poursuivre et supprimer automatiquement les doublons. \n Non pour stopper et visualiser les doublons."),
				icon = "question", type = "yesno", default = "yes")
				
				tcl("update")
				#export du csv des doublons même si l'utilisateur veut continuer
				CSV<-paste(CH_ERREUR,"DoublonsPS_",SEEE_DEBformat,".csv",sep="")
				write.csv2(DATAPOLSPE_doublons,CSV)
				MSGBOX <- tkmessageBox(title = "Info", message = paste("Les données avec l'identification des doublons ont été exportées : \n",CSV,sep=""), icon = "info", type = "ok")
				
				if (tclvalue(YESNO) == "no") { # ...l'utilisateur peut décider d'arreter et exporter la table des doublons
					tkdestroy(tt)
					browseURL(CSV)
					source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit 
				}
			}
			ndoublps<-ndoubl  ## permettra d'afficher le nombre de doublons dans le fichier Excel final : voir SEEE.r
			DATAPOLSPE<-DATAPOLSPE[!duplicated(DATAPOLSPE[,c("STATION","PARAMETRE","DATEPRELEV","HEUREPRELEV","FRACTION"  )]),]
			rm(DATAPOLSPE_doublons)
			tcl("update")
}
gc()

if (SEEECHIM=="oui" ) {
			# on réordonne le résultat pour conserver le plus declassant après suppresion des doublons
			DATACHIM<-DATACHIM[order(DATACHIM$STATION,DATACHIM$PARAMETRE,DATACHIM$DATEPRELEV, DATACHIM$HEUREPRELEV, -DATACHIM$RESULTAT),]
			# on indentifie des doublons
			gc()
			DATACHIM_doublons<-DATACHIM
			DATACHIM_doublons$ID<-as.numeric(as.factor(paste(DATACHIM_doublons$STATION,DATACHIM_doublons$PARAMETRE,DATACHIM_doublons$DATEPRELEV,DATACHIM_doublons$HEUREPRELEV,DATACHIM_doublons$FRACTION)  ))
			DATACHIM_doublons$DOUBLON<-""
			DATACHIM_doublons$DOUBLON[duplicated(DATACHIM[,c("STATION","PARAMETRE","DATEPRELEV","HEUREPRELEV","FRACTION"  )])]<-"2. oui" 
			IDDOUBLON<-unique(DATACHIM_doublons$ID[DATACHIM_doublons$DOUBLON == "2. oui"] )
			DATACHIM_doublons$DOUBLON[DATACHIM_doublons$DOUBLON == "" & DATACHIM_doublons$ID %in% IDDOUBLON ]<-"1. oui à conserver ? " 
			DATACHIM_doublons$ID[DATACHIM_doublons$DOUBLON == "" ]<-NA
			DATACHIM_doublons<-DATACHIM_doublons[order(DATACHIM_doublons$STATION,DATACHIM_doublons$PARAMETRE,DATACHIM_doublons$DATEPRELEV,
			DATACHIM_doublons$HEUREPRELEV,DATACHIM_doublons$FRACTION,DATACHIM_doublons$DOUBLON),]
			ndoubl<-nrow(DATACHIM) - nrow(DATACHIM_doublons[DATACHIM_doublons$DOUBLON == "",])
			print(nrow(DATACHIM))
			print(nrow(DATACHIM_doublons))
			print(nrow(DATACHIM_doublons[as.character(DATACHIM_doublons$DOUBLON) == "",]))

			if (ndoubl> 0 ){ # S'il ya des doublons...
			YESNO<-tkmessageBox(message = paste(ndoubl,"doublons ont été identifiés dans le TABLE CHIMIE. \n Oui pour poursuivre et supprimer automatiquement les doublons. \n Non pour stopper et visualiser les doublons"),
				icon = "question", type = "yesno", default = "yes")
				tcl("update")
				# export du csv des doublons même si l'utilisateur veut continuer
				
				CSV<-paste(CH_ERREUR,"DoublonsCHIM_",SEEE_DEBformat,".csv",sep="")
				write.csv2(DATACHIM_doublons,CSV)
				MSGBOX <- tkmessageBox(title = "Info", message = paste("Le fichier avec identification des doublons\n a été exporté dans \n",CSV,sep=""), icon = "info", type = "ok")
				tcl("update")
				
				if (tclvalue(YESNO) == "no") { # ...l'utilisateur peut décider d'arreter et exporter la table des doublons
					tkdestroy(tt)
					browseURL(CSV)
				    
				}
			}
			ndoublchim<-ndoubl  ## permettra d'afficher le nombre de doublons dans le fichier Excel final : voir SEEE.r
			DATACHIM<-DATACHIM[!duplicated(DATACHIM[,c("STATION","PARAMETRE","DATEPRELEV","HEUREPRELEV","FRACTION"  )]),]
			rm(DATACHIM_doublons)
			tcl("update")
			}
gc()
if (CONTA=="oui" ) {
			# on réordonne le résultat pour conserver le plus declassant après suppresion des doublons
			DATACONTA<-DATACONTA[order(DATACONTA$STATION,DATACONTA$PARAMETRE,DATACONTA$DATEPRELEV, DATACONTA$HEUREPRELEV, -DATACONTA$RESULTAT),]
			# on indentifie des doublons
			gc()
			DATACONTA_doublons<-DATACONTA
			DATACONTA_doublons$ID<-as.numeric(as.factor(paste(DATACONTA_doublons$STATION,DATACONTA_doublons$PARAMETRE,DATACONTA_doublons$DATEPRELEV,DATACONTA_doublons$HEUREPRELEV,DATACONTA_doublons$FRACTION)  ))
			DATACONTA_doublons$DOUBLON<-""
			DATACONTA_doublons$DOUBLON[duplicated(DATACONTA[,c("STATION","PARAMETRE","DATEPRELEV","HEUREPRELEV","FRACTION"  )])]<-"2. oui" 
			IDDOUBLON<-unique(DATACONTA_doublons$ID[DATACONTA_doublons$DOUBLON == "2. oui"] )
			DATACONTA_doublons$DOUBLON[DATACONTA_doublons$DOUBLON == "" & DATACONTA_doublons$ID %in% IDDOUBLON ]<-"1. oui à conserver ? " 
			DATACONTA_doublons$ID[DATACONTA_doublons$DOUBLON == "" ]<-NA
			DATACONTA_doublons<-DATACONTA_doublons[order(DATACONTA_doublons$STATION,DATACONTA_doublons$PARAMETRE,DATACONTA_doublons$DATEPRELEV,
			DATACONTA_doublons$HEUREPRELEV,DATACONTA_doublons$FRACTION,DATACONTA_doublons$DOUBLON),]
			ndoubl<-nrow(DATACONTA) - nrow(DATACONTA_doublons[DATACONTA_doublons$DOUBLON == "",])
			print(nrow(DATACONTA))
			print(nrow(DATACONTA_doublons))
			print(nrow(DATACONTA_doublons[as.character(DATACONTA_doublons$DOUBLON) == "",]))

			if (ndoubl> 0 ){ # S'il ya des doublons...
			YESNO<-tkmessageBox(message = paste(ndoubl,"doublons ont été identifiés dans les données brutes. \n Oui pour poursuivre et supprimer automatiquement les doublons. \n Non pour stopper et visualiser les doublons"),
				icon = "question", type = "yesno", default = "yes")
				tcl("update")
				# export du csv des doublons même si l'utilisateur veut continuer
				
				CSV<-paste(CH_ERREUR,"DoublonsCONTA_",SEEE_DEBformat,".csv",sep="")
				write.csv2(DATACONTA_doublons,CSV)
				MSGBOX <- tkmessageBox(title = "Info", message = paste("Le fichier avec identification des doublons\n a été exporté dans \n",CSV,sep=""), icon = "info", type = "ok")
				tcl("update")
				
				if (tclvalue(YESNO) == "no") { # ...l'utilisateur peut décider d'arreter et exporter la table des doublons
					tkdestroy(tt)
					browseURL(CSV)
				    
				}
			}
			ndoublchim<-ndoubl  ## permettra d'afficher le nombre de doublons dans le fichier Excel final : voir SEEE.r
			DATACONTA<-DATACONTA[!duplicated(DATACONTA[,c("STATION","PARAMETRE","DATEPRELEV","HEUREPRELEV","FRACTION"  )]),]
			rm(DATACONTA_doublons)
			tcl("update")
			}
gc()


if (SEEEBIO=="oui" ) {
			gc()
			# on réordonne le résultat pour conserver le plus declassant après suppresion des doublons
			condbio<-(!DATABIO$PARAMETRE %in%  c("2906","7036") ) 
			DATABIO$RESULTAT[condbio]<-DATABIO$RESULTAT[condbio] * (-1) #exeption pour les non IPR
			DATABIO<-DATABIO[order(DATABIO$STATION,DATABIO$PARAMETRE,DATABIO$DATEPRELEV, DATABIO$HEUREPRELEV, -DATABIO$RESULTAT),]
			if (MODULE == "REEE2018" & CEPE == "CE") { #on prend preferenciellement les EQR par rapport au indice
				DATABIO<-DATABIO[order(DATABIO$STATION,DATABIO$PARAMETRE,DATABIO$DATEPRELEV, DATABIO$HEUREPRELEV, DATABIO$INDICEEQR ,  -DATABIO$RESULTAT),]
			}
			condbio<-(!DATABIO$PARAMETRE %in%  c("2906","7036") ) 
			DATABIO$RESULTAT[condbio]<-DATABIO$RESULTAT[condbio] * (-1)
			# on indentifie des doublons
			DATABIO_doublons<-DATABIO
			DATABIO_doublons$ID<-as.numeric(as.factor(paste(DATABIO_doublons$STATION,DATABIO_doublons$PARAMETRE,DATABIO_doublons$DATEPRELEV,DATABIO_doublons$HEUREPRELEV,DATABIO_doublons$FRACTION)  ))
			DATABIO_doublons$DOUBLON<-""
			DATABIO_doublons$DOUBLON[duplicated(DATABIO[,c("STATION","PARAMETRE","DATEPRELEV","HEUREPRELEV" )])]<-"2. oui" 
			IDDOUBLON<-unique(DATABIO_doublons$ID[DATABIO_doublons$DOUBLON == "2. oui"] )
			DATABIO_doublons$DOUBLON[DATABIO_doublons$DOUBLON == "" & DATABIO_doublons$ID %in% IDDOUBLON ]<-"1. oui à conserver ? " 
			DATABIO_doublons$ID[DATABIO_doublons$DOUBLON == "" ]<-NA
			DATABIO_doublons<-DATABIO_doublons[order(DATABIO_doublons$STATION,DATABIO_doublons$PARAMETRE,DATABIO_doublons$DATEPRELEV,
			DATABIO_doublons$HEUREPRELEV,DATABIO_doublons$DOUBLON),]
			ndoubl<-nrow(DATABIO) - nrow(DATABIO_doublons[DATABIO_doublons$DOUBLON == "",])
			print(nrow(DATABIO))
			print(nrow(DATABIO_doublons))
			print(nrow(DATABIO_doublons[as.character(DATABIO_doublons$DOUBLON) == "",]))
			
			if (ndoubl> 0 ){ # S'il ya des doublons...
			YESNO<-tkmessageBox(message = paste(ndoubl,"doublons ont été identifiés dans les données biologiques. \n Oui pour poursuivre et supprimer automatiquement les doublons. \n Non pour stopper et visualiser les doublons."),
				icon = "question", type = "yesno", default = "yes")
				tcl("update")
				
				#export du csv des doublons même si l'utilisateur veut continuer
				CSV<-paste(CH_ERREUR,"DoublonsBIO_",SEEE_DEBformat,".csv",sep="")
				write.csv2(DATABIO_doublons,CSV)
				MSGBOX <- tkmessageBox(title = "Info", message = paste("Les données avec l'identification des doublons ont été exportées : \n",CSV,sep=""), icon = "info", type = "ok")
				tcl("update")
				
				if (tclvalue(YESNO) == "no") { # ...l'utilisateur peut décider d'arreter et exporter la table des doublons
					tkdestroy(tt)
					browseURL(CSV)
					source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
				}
			}
			ndoublbio<-ndoubl  ## permettra d'afficher le nombre de doublons dans le fichier Excel final : voir SEEE.r
			DATABIO<-DATABIO[!duplicated(DATABIO[,c("STATION","PARAMETRE","DATEPRELEV","HEUREPRELEV" )]),]
			rm(DATABIO_doublons)
			tcl("update")
			}

if (SEEEPEGASE=="oui" ) {
			gc()
			DATAPEGASE_doublons<-DATAPEGASE
			DATAPEGASE_doublons$DOUBLON<-""
			DATAPEGASE_doublons$DOUBLON[duplicated(DATAPEGASE[,c("STATION")])]<-"oui" 
			DATAPEGASE_doublons<-DATAPEGASE_doublons[order(DATAPEGASE_doublons$STATION,DATAPEGASE_doublons$DOUBLON),]
			ndoubl<-nrow(DATAPEGASE) - nrow(DATAPEGASE_doublons[DATAPEGASE_doublons$DOUBLON == "",])
			print(nrow(DATAPEGASE))
			print(nrow(DATAPEGASE_doublons))
			print(nrow(DATAPEGASE_doublons[as.character(DATAPEGASE_doublons$DOUBLON) != "oui",]))

			if (ndoubl> 0 ){ # S'il ya des doublons...
			YESNO<-tkmessageBox(message = paste(ndoubl,"doublons ont été identifiés dans les données PEGASE. \n Oui pour poursuivre et supprimer automatiquement les doublons. \n Non pour stopper et visualiser les doublons"),
				icon = "question", type = "yesno", default = "yes")
				tcl("update")
				#export du csv des doublons même si l'utilisateur veut continuer
				
				CSV<-paste(CH_ERREUR,"DoublonsPEGASE",SEEE_DEBformat,".csv",sep="")
				write.csv2(DATAPEGASE_doublons,CSV)
				MSGBOX <- tkmessageBox(title = "Info", message = paste("Le fichier avec l'identification des doublons\n a été exporté dans \n",CSV,sep=""), icon = "info", type = "ok")
				tcl("update")
				
				if (tclvalue(YESNO) == "no") { # ...l'utilisateur peut décider d'arreter et exporter la table des doublons
					tkdestroy(tt)
					browseURL(CSV)
					source(paste(pathS,"msgerror.r",sep="") , echo = TRUE, print.eval = TRUE) #=quit
				}
			}
			ndoublpeg<-ndoubl  ## permettra d'afficher le nombre de doublons dans le fichier Excel final : voir SEEE.r
			DATAPEGASE<-DATAPEGASE[!duplicated(DATAPEGASE[,c("STATION")]),]
			rm(DATAPEGASE_doublons)
			tcl("update")
			}

gc()
###########################################################################################
#Fin du module de conformité : on exporte les données brutes qui ont été mise en conformité

CH_DATABRUTE<-paste0(CH_OUTPUT,"/DONNEES_BRUTES/")
dir.create(CH_DATABRUTE)

if (OPTIONEXPORTDATABRUT=="oui"){
		#DATAPCH
	CSVDATAPCH<-paste0(CH_DATABRUTE,"DATAPCH_conforme.csv")
	if (SEEEPCH == "oui") {write.csv2(DATAPCH,CSVDATAPCH)}
		#DATAPOLSPE
	CSVDATAPS<-paste0(CH_DATABRUTE,"DATAPOLSPE_conforme.csv")
	if (SEEEPS == "oui") {write.csv2(DATAPOLSPE,CSVDATAPS)}
		#DATABIO
	CSVDATABIO<-paste0(CH_DATABRUTE,"DATABIO_conforme.csv")
	if (SEEEBIO == "oui") {write.csv2(DATABIO,CSVDATABIO)}
		#DATACHIM
	CSVDATACHIM<-paste0(CH_DATABRUTE,"DATACHIM_conforme.csv")
	if (SEEECHIM == "oui") {write.csv2(DATACHIM,CSVDATACHIM)}
		#CONTA
	CSVDATACONTA<-paste0(CH_DATABRUTE,"DATACONTA_conforme.csv")
	if (CONTA == "oui") {write.csv2(DATACONTA,CSVDATACONTA)}
		#SEEEPEGASE
	CSVDATAPEG<-paste0(CH_DATABRUTE,"DATAMODELISEE_conforme.csv")
	if (SEEEPEGASE == "oui") {write.csv2(DATAPEGASE,CSVDATAPEG)}

}



gc()
