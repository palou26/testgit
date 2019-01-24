##############################################
## SEEE - COURS D'EAU : état polluants spécifiques
## Application de l'arrêté de janvier 2010
##############################################


##############################
## Mise en forme des données
##############################
gc()

DATAPOLSPE$IDAN<-as.character(paste0(DATAPOLSPE$STATION,DATAPOLSPE$PARAMETRE,DATAPOLSPE$ANNEE))

######################################################################
## METHODE : choix de la méthode pour la preparation des données brutes
# ANNEERECENTE : extraction des données de l'année la + récente pour chaque paramètre de la station
# ANNEECHRONIQUE : extraction des données d'une station pour l'ensemble des paramètres ayant la chronique la plus complète
# NOFILTREANNEE : on ne fait aucun filtre sur l'année et on prend en compte tout le jeu de données
######################################################################

if (METHODEMOYPS =="NOFILTREANNEE") {
	PSANNEEmax<-aggregate(ANNEE ~ STATION + PARAMETRE, data = DATAPOLSPE , max)
	PSANNEEmin<-aggregate(ANNEE ~ STATION + PARAMETRE, data = DATAPOLSPE , min)
	PSANNEEmax$IDAN<-as.character(paste0(PSANNEEmax$STATION,PSANNEEmax$PARAMETRE))
	PSANNEEmin$IDAN<-as.character(paste0(PSANNEEmin$STATION,PSANNEEmin$PARAMETRE))
	PSANNEE<-merge(PSANNEEmin,PSANNEEmax[,c("IDAN","ANNEE")], by = "IDAN")
	PSANNEE$ANNEE<-PSANNEE$ANNEE.x
	condAnnee<-PSANNEE$ANNEE.x != PSANNEE$ANNEE.y
	PSANNEE$ANNEE[condAnnee]<-paste0(PSANNEE$ANNEE.x[condAnnee],"-",PSANNEE$ANNEE.y[condAnnee])
}

if (METHODEMOYPS =="ANNEERECENTE") {
	PSANNEE<-aggregate(ANNEE ~ STATION + PARAMETRE, data = DATAPOLSPE , max)
	PSANNEE$IDAN<-as.character(paste0(PSANNEE$STATION,PSANNEE$PARAMETRE,PSANNEE$ANNEE))
	DATAPOLSPE<-DATAPOLSPE[DATAPOLSPE$IDAN %in% c(PSANNEE$IDAN),]
} else if (METHODEMOYPS =="ANNEECHRONIQUE") {
	PSANNEE<-aggregate(RESULTAT ~ STATION + PARAMETRE + ANNEE, data = DATAPOLSPE , length)
	PSANNEE$IDAN<-as.character(paste0(PSANNEE$STATION,PSANNEE$PARAMETRE,PSANNEE$ANNEE))
	PSANNEE$IDFREQ<-as.character(paste0(PSANNEE$STATION,PSANNEE$PARAMETRE,PSANNEE$RESULTAT))
	PSANNEE<-PSANNEE[order(PSANNEE$IDAN),]
	
	TEMPPSANNEE1<-aggregate(RESULTAT ~ STATION + PARAMETRE, data = PSANNEE , max) # colonne RESULTAT non renommée mais contient le nb de prelevement
	TEMPPSANNEE1$IDFREQ<-as.character(paste0(TEMPPSANNEE1$STATION,TEMPPSANNEE1$PARAMETRE,TEMPPSANNEE1$RESULTAT))
	PSANNEE<-PSANNEE[PSANNEE$IDFREQ %in% c(TEMPPSANNEE1$IDFREQ),]
	
	#extraction de l'année la + récente si plusieurs années ont les mêmes fréquences
	TEMPPSANNEE2<-aggregate(ANNEE ~ STATION + PARAMETRE, data = PSANNEE , max)
	TEMPPSANNEE2$IDAN<-as.character(paste0(TEMPPSANNEE2$STATION,TEMPPSANNEE2$PARAMETRE,TEMPPSANNEE2$ANNEE))
	DATAPOLSPE<-DATAPOLSPE[DATAPOLSPE$IDAN %in% c(TEMPPSANNEE2$IDAN),]
	
	#on recrée PSANNEE depuis DATAPOLSPE filtré pour calculer les fréquences. PSANNEE est ainsi identique à "ANNEERECENTE" dans FREQUENCE.r --> ça facilite le script et les éventuels modif.
	PSANNEE<-aggregate(ANNEE ~ STATION + PARAMETRE, data = DATAPOLSPE , max)
	
	rm(TEMPPSANNEE1,TEMPPSANNEE2)
}

##############################
## Calcul de l'état des polluants spécifiques
##############################
# requete préparatoire
POLSPE<-merge(DATAPOLSPE,LISTECODERQE[,c("REMARQUE","QUANTIFIE")],by="REMARQUE")
POLSPE<-merge(POLSPE,PARAMETREPOLSPE[,c("PARAMETRE","NQEMA","NQEMA2")],by="PARAMETRE")
gc()
POLSPE<-merge(POLSPE,STATION[,c("STATION","FONDGEO_ARSENIC","FONDGEO_CHROME","FONDGEO_CUIVRE","FONDGEO_ZINC","DURETE")],by="STATION", all.x=TRUE)
POLSPE$ID<-paste(POLSPE$STATION, POLSPE$PARAMETRE, sep="_")
gc()

# Détermination de NQEMA selon durete et Fond géochim
POLSPE$NQEMA[POLSPE$PARAMETRE=="1383" & POLSPE$DURETE>24 & !is.na(POLSPE$DURETE)]<-POLSPE$NQEMA2[POLSPE$PARAMETRE=="1383" & POLSPE$DURETE>24 & !is.na(POLSPE$DURETE)]

condarsenic<-POLSPE$PARAMETRE=="1369" & !is.na(POLSPE$FONDGEO_ARSENIC)
POLSPE$NQEMA[condarsenic]<-POLSPE$NQEMA[condarsenic]+POLSPE$FONDGEO_ARSENIC[condarsenic]
condchrome<-POLSPE$PARAMETRE=="1389" & !is.na(POLSPE$FONDGEO_CHROME)
POLSPE$NQEMA[condchrome]<-POLSPE$NQEMA[condchrome]+POLSPE$FONDGEO_CHROME[condchrome]
condcuivre<-POLSPE$PARAMETRE=="1392" & !is.na(POLSPE$FONDGEO_CUIVRE)
POLSPE$NQEMA[condcuivre]<-POLSPE$NQEMA[condcuivre]+POLSPE$FONDGEO_CUIVRE[condcuivre]
condzinc<-POLSPE$PARAMETRE=="1383" & !is.na(POLSPE$FONDGEO_ZINC)
POLSPE$NQEMA[condzinc]<-POLSPE$NQEMA[condzinc]+POLSPE$FONDGEO_ZINC[condzinc]
rm(condarsenic,condchrome,condcuivre,condzinc)

# Préparation des valeurs pour calcul de la moyenne
# if (LQ_NONDISPO =="oui") { # si LQ non dispo on prend les valeurs du champ RESULTAT lorsque non quantifié --> car c'est la LQ
	# POLSPE$MOY[POLSPE$QUANTIFIE=="1"]<-POLSPE$RESULTAT[POLSPE$QUANTIFIE=="1"] # pour valeur quantifiée on conserve en l'état, car risque de ne pas avoir de LQ si quantifiée tt l'année
	# POLSPE$MOYMIN[POLSPE$QUANTIFIE=="1"]<-POLSPE$RESULTAT[POLSPE$QUANTIFIE=="1"]
	# POLSPE$MOYMAX[POLSPE$QUANTIFIE=="1"]<-POLSPE$RESULTAT[POLSPE$QUANTIFIE=="1"]
	
	# condLQ2<-POLSPE$QUANTIFIE=="0" & POLSPE$RESULTAT <= (POLSPE$NQEMA*0.33)
	# condLQ0<-POLSPE$QUANTIFIE=="0" & POLSPE$RESULTAT > (POLSPE$NQEMA*0.33)
	# condLQ<-POLSPE$QUANTIFIE=="0" & POLSPE$RESULTAT > (POLSPE$NQEMA*0.33)
# } else {
	# condLQ2<-POLSPE$LQ <= (POLSPE$NQEMA*0.33)
	# condLQ0<-POLSPE$LQ > (POLSPE$NQEMA*0.33)
	# condLQ<-POLSPE$LQ > (POLSPE$NQEMA*0.33)
	
	# POLSPE$MOY<-POLSPE$RESULTAT
	# POLSPE$MOYMIN<-POLSPE$RESULTAT
	# POLSPE$MOYMAX<-POLSPE$RESULTAT
# }

## modifié le 16/07/15 suite mail AESN
if (LQ_NONDISPO =="oui") { # si LQ non dispo on prend les valeurs du champ RESULTAT lorsque non quantifié --> car c'est la LQ
	condLQ2<-POLSPE$QUANTIFIE=="0" & POLSPE$RESULTAT <= (POLSPE$NQEMA*0.33)
	condLQ0<-POLSPE$QUANTIFIE=="0" & POLSPE$RESULTAT > (POLSPE$NQEMA*0.33)
	condLQ<-POLSPE$QUANTIFIE=="0" & POLSPE$RESULTAT > (POLSPE$NQEMA*0.33)
} else {
	condLQ2<-POLSPE$QUANTIFIE=="0" & POLSPE$LQ <= (POLSPE$NQEMA*0.33)
	condLQ0<-POLSPE$QUANTIFIE=="0" & POLSPE$LQ > (POLSPE$NQEMA*0.33)
	condLQ<-POLSPE$QUANTIFIE=="0" & POLSPE$LQ > (POLSPE$NQEMA*0.33)
}
# modifé le 15/07/15 suite mail AESN
POLSPE$MOY<-POLSPE$RESULTAT
POLSPE$MOYMIN<-POLSPE$RESULTAT
POLSPE$MOYMAX<-POLSPE$RESULTAT

POLSPE$MOY[condLQ2]<-POLSPE$RESULTAT[condLQ2]/2
POLSPE$MOYMIN[condLQ0]<-0
POLSPE$MOYMAX[condLQ]<-POLSPE$RESULTAT[condLQ]

POLSPE_NQEMA<-POLSPE[!duplicated(POLSPE[,c("STATION","PARAMETRE")]),c("STATION","PARAMETRE","NQEMA")]
POLSPE_NQEMA$ID<-paste0(POLSPE_NQEMA$STATION,"_",POLSPE_NQEMA$PARAMETRE)
rm(condLQ2,condLQ0,condLQ)

# Calcul de la moyenne
POLSPE_MOY<-aggregate(cbind(MOY, MOYMIN, MOYMAX)~ STATION + PARAMETRE, data = POLSPE , mean)
POLSPE_MOY$MOY<-round(POLSPE_MOY$MOY,3)
POLSPE_MOY$MOYMIN<-round(POLSPE_MOY$MOYMIN,3)
POLSPE_MOY$MOYMAX<-round(POLSPE_MOY$MOYMAX,3)
POLSPE_MOY$ID<-paste0(POLSPE_MOY$STATION,"_",POLSPE_MOY$PARAMETRE)
gc()
POLSPE_MOY<-merge(POLSPE_MOY,POLSPE_NQEMA[,c("ID","NQEMA")],by="ID")
POLSPE_MOY<-merge(POLSPE_MOY,PARAMETREPOLSPE[,c("PARAMETRE","POLLUANT")],by="PARAMETRE")
gc()

# Extraction de la LQMAX par station pour comparaison à la MA
if (LQ_NONDISPO =="oui") { # recherche LQmax dans colonne resultat si colonne LQ non dispo
	LQMAX<-POLSPE[POLSPE$QUANTIFIE=="0", c("STATION","PARAMETRE","QUANTIFIE","RESULTAT")]
	LQMAX<-aggregate(RESULTAT~ STATION + PARAMETRE, data = LQMAX , max)
	names(LQMAX)[3]<-"LQMAX"
	LQMAX$LQMAX<-round(LQMAX$LQMAX,5)
	LQMAX$ID<-paste(LQMAX$STATION, LQMAX$PARAMETRE, sep="_")
} else { # recherche LQmax dans colonne LQ
	#LQMAX<-POLSPE[POLSPE$QUANTIFIE=="0", c("STATION","PARAMETRE","QUANTIFIE","RESULTAT")]
	LQMAX<-aggregate(LQ ~ STATION + PARAMETRE, data = POLSPE , max)
	names(LQMAX)[3]<-"LQMAX"
	LQMAX$ID<-paste(LQMAX$STATION, LQMAX$PARAMETRE, sep="_")
}

POLSPE_MOY<-merge(POLSPE_MOY,LQMAX[,c("ID","LQMAX")],by="ID", all.x=TRUE)
gc()

# CAS 1 - Comparaison NQEMA LQmax <= 1/3 NQE
POLSPE_MOY$CLASSEPS<-as.numeric(NA)
POLSPE_MOY$CAS1<-as.character(NA)
POLSPE_MOY$CLASSEPS[(is.na(POLSPE_MOY$LQMAX) | POLSPE_MOY$LQMAX <= (POLSPE_MOY$NQEMA*0.33)) & POLSPE_MOY$MOY <=POLSPE_MOY$NQEMA ]<-2  # LQmax > 1/3 NQEMA, is.na(POLSPE_MOY$LQMAX) --> cas des prlvt tjs quantifié donc pas de LQMAX trouvé dans les data
POLSPE_MOY$CAS1[(is.na(POLSPE_MOY$LQMAX) | POLSPE_MOY$LQMAX <= (POLSPE_MOY$NQEMA*0.33)) & POLSPE_MOY$MOY <=POLSPE_MOY$NQEMA ]<-"oui"
POLSPE_MOY$CLASSEPS[(is.na(POLSPE_MOY$LQMAX) | POLSPE_MOY$LQMAX <= (POLSPE_MOY$NQEMA*0.33)) & POLSPE_MOY$MOY >POLSPE_MOY$NQEMA ]<-3
POLSPE_MOY$CAS1[(is.na(POLSPE_MOY$LQMAX) | POLSPE_MOY$LQMAX <= (POLSPE_MOY$NQEMA*0.33)) & POLSPE_MOY$MOY >POLSPE_MOY$NQEMA ]<-"oui"

# CAS 2 - Comparaison NQEMA LQmax > 1/3 NQE : encadremment avec MOYMIN
POLSPE_MOY$CAS2<-as.character(NA)
POLSPE_MOY$CLASSEPS[!is.na(POLSPE_MOY$LQMAX) & POLSPE_MOY$LQMAX > (POLSPE_MOY$NQEMA*0.33) & POLSPE_MOY$MOYMIN >POLSPE_MOY$NQEMA ]<-3 
POLSPE_MOY$CAS2[!is.na(POLSPE_MOY$LQMAX) & POLSPE_MOY$LQMAX > (POLSPE_MOY$NQEMA*0.33) & POLSPE_MOY$MOYMIN >POLSPE_MOY$NQEMA ]<-"oui"

# CAS 3 - Comparaison NQEMA LQmax > 1/3 NQE  : encadremment avec MOYMAX
POLSPE_MOY$CAS3<-as.character(NA)
POLSPE_MOY$CLASSEPS[!is.na(POLSPE_MOY$LQMAX) & POLSPE_MOY$LQMAX > (POLSPE_MOY$NQEMA*0.33) & POLSPE_MOY$MOYMIN <= POLSPE_MOY$NQEMA  & POLSPE_MOY$MOYMAX <= POLSPE_MOY$NQEMA  ]<-2
POLSPE_MOY$CAS3[!is.na(POLSPE_MOY$LQMAX) & POLSPE_MOY$LQMAX > (POLSPE_MOY$NQEMA*0.33) & POLSPE_MOY$MOYMIN <= POLSPE_MOY$NQEMA  & POLSPE_MOY$MOYMAX <= POLSPE_MOY$NQEMA  ]<-"oui" 
POLSPE_MOY$CLASSEPS[!is.na(POLSPE_MOY$LQMAX) & POLSPE_MOY$LQMAX > (POLSPE_MOY$NQEMA*0.33) & POLSPE_MOY$MOYMIN <= POLSPE_MOY$NQEMA & POLSPE_MOY$MOYMAX > POLSPE_MOY$NQEMA  ]<-0
POLSPE_MOY$CAS3[!is.na(POLSPE_MOY$LQMAX) & POLSPE_MOY$LQMAX > (POLSPE_MOY$NQEMA*0.33) & POLSPE_MOY$MOYMIN <= POLSPE_MOY$NQEMA & POLSPE_MOY$MOYMAX > POLSPE_MOY$NQEMA  ]<-"oui"

# Calcul de la frequence de prélèvement
POLSPE_FREQ<-aggregate(MOY ~ STATION + PARAMETRE, data = POLSPE , length)
names(POLSPE_FREQ)[3]<-"FREQ"
POLSPE_FREQ$ID<-as.character(paste0(POLSPE_FREQ$STATION,"_",POLSPE_FREQ$PARAMETRE))
POLSPE_FREQ<-POLSPE_FREQ[,c("ID","FREQ")]

POLSPE_MOY<-merge(POLSPE_MOY,POLSPE_FREQ,by="ID")
POLSPE_MOY<-POLSPE_MOY[order(POLSPE_MOY$STATION, POLSPE_MOY$PARAMETRE),]
rm(POLSPE_FREQ)

## Respect de la fréquence minimale de prélèvement
if (FREQOKPS == "oui") {
	POLSPE_MOY$CLASSEPS[POLSPE_MOY$FREQ<NBANFREQ]<-0
}

# #### SCRIPT INITIAL
# # calcul de la moyenne en tenant compte des valeurs non quantifiées
# POLSPE<-merge(DATAPOLSPE,LISTECODERQE[,c("REMARQUE","QUANTIFIE")],by="REMARQUE")
# POLSPE$RESULTATMOY<-POLSPE$RESULTAT
# POLSPE$RESULTATMOY[POLSPE$QUANTIFIE=="0"]<-POLSPE$RESULTAT[POLSPE$QUANTIFIE=="0"] / 2
# POLSPE_MOY<-aggregate(RESULTATMOY ~ STATION + PARAMETRE, data = POLSPE , mean)
# POLSPE_MOY$ID<-as.character(paste0(POLSPE_MOY$STATION,POLSPE_MOY$PARAMETRE))
# POLSPE_MOY$RESULTATMOY<-round(POLSPE_MOY$RESULTATMOY,3)

# # Calcul de la frequence de prélèvement
# POLSPE_FREQ<-aggregate(RESULTATMOY ~ STATION + PARAMETRE, data = POLSPE , length)
# names(POLSPE_FREQ)[3]<-"FREQ"
# POLSPE_FREQ$ID<-as.character(paste0(POLSPE_FREQ$STATION,POLSPE_FREQ$PARAMETRE))
# POLSPE_FREQ<-POLSPE_FREQ[,c("ID","FREQ")]

# POLSPE_MOY<-merge(POLSPE_MOY,POLSPE_FREQ,by="ID")
# POLSPE_MOY<-POLSPE_MOY[order(POLSPE_MOY$STATION, POLSPE_MOY$PARAMETRE),]
# rm(POLSPE_FREQ)

# ## Calcul de l'état des paramètres selon la moyenne des valeurs
# POLSPE_MOY$CLASSEPS<-as.numeric("")
# POLSPE_MOY<-merge(POLSPE_MOY,PARAMETREPOLSPE,by="PARAMETRE")
# POLSPE_MOY<-merge(POLSPE_MOY,STATION[,c("STATION","FONDGEO_ARSENIC","FONDGEO_CHROME","FONDGEO_CUIVRE","FONDGEO_ZINC","DURETE")],by="STATION", all.x=TRUE)

# ## Adaptation de NQEMA du zinc selon dureté
# POLSPE_MOY$NQEMA[POLSPE_MOY$PARAMETRE=="1383" & POLSPE_MOY$DURETE>24 & !is.na(POLSPE_MOY$DURETE)]<-POLSPE_MOY$NQEMA2[POLSPE_MOY$PARAMETRE=="1383" & POLSPE_MOY$DURETE>24 & !is.na(POLSPE_MOY$DURETE)]

# # déclaration des conditions
# condbon<-"POLSPE_MOY$RESULTATMOY<=POLSPE_MOY$NQEMA"
# condpasbon<-"POLSPE_MOY$RESULTATMOY>POLSPE_MOY$NQEMA"
# condarsenic<- "+ POLSPE_MOY$FONDGEO_ARSENIC & POLSPE_MOY$PARAMETRE=='1369'"
# condchrome<-"+ POLSPE_MOY$FONDGEO_CHROME & POLSPE_MOY$PARAMETRE=='1389'"
# condcuivre<-"+ POLSPE_MOY$FONDGEO_CUIVRE & POLSPE_MOY$PARAMETRE=='1392'" 
# condzinc<-"+ POLSPE_MOY$FONDGEO_ZINC & POLSPE_MOY$PARAMETRE=='1383'"
# condautreparam<- "& !(POLSPE_MOY$PARAMETRE %in% c('1369','1389','1392','1383'))"

# POLSPE_MOY$CLASSEPS[eval(parse(text = paste0(condbon,condarsenic)))]<-2 # eval parse permet de transformer une chaine de caractere en requete true/false
# POLSPE_MOY$CLASSEPS[eval(parse(text = paste0(condbon,condchrome)))]<-2
# POLSPE_MOY$CLASSEPS[eval(parse(text = paste0(condbon,condcuivre)))]<-2
# POLSPE_MOY$CLASSEPS[eval(parse(text = paste0(condbon,condzinc)))]<-2
# POLSPE_MOY$CLASSEPS[eval(parse(text = paste0(condbon,condautreparam)))]<-2

# POLSPE_MOY$CLASSEPS[eval(parse(text = paste0(condpasbon,condarsenic)))]<-3
# POLSPE_MOY$CLASSEPS[eval(parse(text = paste0(condpasbon,condchrome)))]<-3
# POLSPE_MOY$CLASSEPS[eval(parse(text = paste0(condpasbon,condcuivre)))]<-3
# POLSPE_MOY$CLASSEPS[eval(parse(text = paste0(condpasbon,condzinc)))]<-3
# POLSPE_MOY$CLASSEPS[eval(parse(text = paste0(condpasbon,condautreparam)))]<-3
# rm(condbon,condpasbon,condarsenic,condchrome,condcuivre,condzinc,condautreparam)


# Calcul de l'état par type de polluant
POLSPE_POL<-aggregate(CLASSEPS ~ STATION + POLLUANT, data = POLSPE_MOY, max)
names(POLSPE_POL)[3]<-"ETATPOL" #renomme la colonne CLASSEPS sur laquelle il y a eu le max car correspond à l'état par type de polluants
POLSPE_POL<-POLSPE_POL[order(POLSPE_POL$STATION, POLSPE_POL$POLLUANT),]

# Calcul de l'état global des polluants spécifiques
# si les polluants NS sont exclus, l'état des PS est uniquement déterminé par les polluants synthétiques
if (EXCLU_POLNS=="oui") {
	RLT_POLSPE<-POLSPE_POL[POLSPE_POL$POLLUANT=="synth",c("STATION", "ETATPOL")]
	} else {
	RLT_POLSPE<-aggregate(ETATPOL ~ STATION, data = POLSPE_POL, max) # Résultat final des polluants spécifiques
	}
names(RLT_POLSPE)[2]<-"ETATPS" #renomme la colonne ETATPOL sur laquelle il y a eu le max car correspond à l'état global des polluants spécifiques
flush.console()

##############################
## Mise en forme tableau final
##############################
# Ajout colonnes des polluants 
for (i in  1:nrow(TABLEPOL)  ) {
	TEMPP<-POLSPE_POL[POLSPE_POL$POLLUANT==TABLEPOL$POLLUANT[i],c("STATION", "ETATPOL")]
	names(TEMPP)[2]<-TABLEPOL$POLLUANT[i]
	RLT_POLSPE<-merge(RLT_POLSPE,TEMPP,by="STATION", all.x=TRUE)
	rm(TEMPP)
}

# Ajout colonnes des paramètres 
PARAMETREPOLSPE<-PARAMETREPOLSPE[order(PARAMETREPOLSPE$IDTRI),]

for (i in  1:nrow(PARAMETREPOLSPE)  ) {
	TEMPP<-POLSPE_MOY[POLSPE_MOY$PARAMETRE==PARAMETREPOLSPE$PARAMETRE[i],c("STATION", "CLASSEPS")]
	names(TEMPP)[2]<-PARAMETREPOLSPE$PARAMETRELIB[i]
	RLT_POLSPE<-merge(RLT_POLSPE,TEMPP,by="STATION", all.x=TRUE)
	rm(TEMPP)
}

gc() ## compacte R
flush.console()
