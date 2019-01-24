##############################################
## SEEE - COURS D'EAU : état physicochimique
## Application de l'arrêté de janvier 2010
##############################################
#save.image("D:/_ETUDES/E4309_AESN_S3R_Adapt2016/S3R_DEV/REpch.Rdata")


#EPCHDEB<-date()
# DATAPCH$STATION<-as.character(DATAPCH$STATION)
# DATAPCH$PARAMETRE<-as.character(DATAPCH$PARAMETRE)
# DATAPCH$FRACTION<-as.character(DATAPCH$FRACTION)
# DATAPCH$RESULTAT<-as.numeric(DATAPCH$RESULTAT)
# DATAPCH$REMARQUE<-as.character(DATAPCH$REMARQUE)
# DATAPCH$DATEPRELEV<-as.character(DATAPCH$DATEPRELEV)
# DATAPCH$HEUREPRELEV<-as.character(DATAPCH$HEUREPRELEV)

gc() ## compacte R
PCH<-merge(DATAPCH,STATION,by.x="STATION",by.y="STATION")
gc() ## compacte R
PCH$CLASSEPCH<- 0
PCH$INDICE<- -1
PCH$IDTRI<-as.character(paste(PCH$STATION,PCH$PARAMETRE,sep=""))
gc() ## compacte R

##############################
## Calcul de la classe d'état PCH
##############################
PCH$RESULTAT<-round(PCH$RESULTAT,3)
PCH<-merge(PCH,PARAMETREPCH[,c("PARAMETRE", "SUPB", "INFB", "INFV", "INFJ", "INFO", "INFR")],by="PARAMETRE", all.x=TRUE, sort = FALSE)
gc() ## compacte R

## PARAMETRE ordre croissant : Valeur seuil de l'état PCH
# Attention les valeurs seuils dans la table PARAMETREPCH (colonne INFR) a été attribuées pour le calcul de l'indice et non la mise en classe
PCH$CLASSEPCH[PCH$PARAMETRE %in% c("1313","1841","1433","1350","1335","1339","1340","1302max") & !is.na(PCH$RESULTAT) & PCH$RESULTAT<=PCH$INFB]<-1
PCH$CLASSEPCH[PCH$PARAMETRE %in% c("1313","1841","1433","1350","1335","1339","1340","1302max") & !is.na(PCH$RESULTAT) & PCH$RESULTAT>PCH$INFB & PCH$RESULTAT<=PCH$INFV]<-2
PCH$CLASSEPCH[PCH$PARAMETRE %in% c("1313","1841","1433","1350","1335","1339","1340","1302max") & !is.na(PCH$RESULTAT) & PCH$RESULTAT>PCH$INFV & PCH$RESULTAT<=PCH$INFJ]<-3
PCH$CLASSEPCH[PCH$PARAMETRE %in% c("1313","1841","1433","1350","1335","1339","1340","1302max") & !is.na(PCH$RESULTAT) & PCH$RESULTAT>PCH$INFJ & PCH$RESULTAT<=PCH$INFO]<-4
PCH$CLASSEPCH[PCH$PARAMETRE %in% c("1313","1841","1433","1350","1335","1339","1340","1302max") & !is.na(PCH$RESULTAT) & PCH$RESULTAT>PCH$INFO]<-5
PCH$CLASSEPCH[PCH$PARAMETRE %in% c("1313","1841","1433","1350","1335","1339","1340","1302max") & !is.na(PCH$RESULTAT) & PCH$RESULTAT<0]<-99 

# Cas particulier des NO3 (3 classes). On réaffecte les classes 4 & 5. Les seuils des classes 4 et 5 ont été définis uniquement pour le calcul de l'indice dans la table PARAMETREPCH
PCH$CLASSEPCH[PCH$PARAMETRE %in% c("1340") & !is.na(PCH$RESULTAT) & PCH$CLASSEPCH>=4]<-3

## PARAMETRE ordre décroissant : Valeur seuil de l'état PCH
# Attention les valeurs seuils dans la table PARAMETREPCH (colonne INFR) a été attribuées pour le calcul de l'indice et non la mise en classe
PCH$CLASSEPCH[PCH$PARAMETRE %in% c("1311", "1312", "1302min") & !is.na(PCH$RESULTAT) & PCH$RESULTAT>=PCH$INFB]<-1
PCH$CLASSEPCH[PCH$PARAMETRE %in% c("1311", "1312", "1302min") & !is.na(PCH$RESULTAT) & PCH$RESULTAT>=PCH$INFV & PCH$RESULTAT<PCH$INFB]<-2
PCH$CLASSEPCH[PCH$PARAMETRE %in% c("1311", "1312", "1302min") & !is.na(PCH$RESULTAT) & PCH$RESULTAT>=PCH$INFJ & PCH$RESULTAT<PCH$INFV]<-3
PCH$CLASSEPCH[PCH$PARAMETRE %in% c("1311", "1312", "1302min") & !is.na(PCH$RESULTAT) & PCH$RESULTAT>=PCH$INFO & PCH$RESULTAT<PCH$INFJ]<-4
PCH$CLASSEPCH[PCH$PARAMETRE %in% c("1311", "1312", "1302min") & !is.na(PCH$RESULTAT) & PCH$RESULTAT<PCH$INFO]<-5
PCH$CLASSEPCH[PCH$PARAMETRE %in% c("1311", "1312", "1302min") & !is.na(PCH$RESULTAT) & PCH$RESULTAT<0]<-99 

PCH<-PCH[,c("STATION", "DATEPRELEV", "HEUREPRELEV","PARAMETRE", "FRACTION", "RESULTAT", "REMARQUE", "CLASSEPCH", "INDICE", "IDTRI")]
PCH<-merge(PCH, STATION, by="STATION")
gc() ## compacte R

## PARAMETRE Température (prise en compte du domaine) : Valeur seuil de l'état PCH
PCH_1301<-PCH[(PCH$PARAMETRE=="1301" & PCH$CONTEXTE_PISCICOLE %in% c("salmonicole", "intermediaire", "cyprinicole") & !is.na(PCH$CONTEXTE_PISCICOLE)) ,]
PCH_1301na<-PCH[PCH$PARAMETRE=="1301" & is.na(PCH$CONTEXTE_PISCICOLE),] #sert uniquement dans le rbind pour recréer le tableau final
PCH_AUTRE<-PCH[!(PCH$PARAMETRE=="1301"),] #sert uniquement dans le rbind pour recréer le tableau final

PCH_1301$PARATEMP<-paste(PCH_1301$PARAMETRE,PCH_1301$CONTEXTE_PISCICOLE,sep="")
PARAMETREPCHEXCEPT$PARATEMP<-paste(PARAMETREPCHEXCEPT$PARAMETRE,PARAMETREPCHEXCEPT$CONTEXTE_PISCICOLE,sep="")
PCH_1301<-merge(PCH_1301, PARAMETREPCHEXCEPT[,c("PARATEMP", "SUPB", "INFB", "INFV", "INFJ", "INFO", "INFR")], by="PARATEMP")
gc() ## compacte R

PCH_1301$CLASSEPCH[!is.na(PCH_1301$RESULTAT) & PCH_1301$RESULTAT<=PCH_1301$INFB]<-1
PCH_1301$CLASSEPCH[!is.na(PCH_1301$RESULTAT) & PCH_1301$RESULTAT>PCH_1301$INFB & PCH_1301$RESULTAT<=PCH_1301$INFV]<-2
PCH_1301$CLASSEPCH[!is.na(PCH_1301$RESULTAT) & PCH_1301$RESULTAT>PCH_1301$INFV & PCH_1301$RESULTAT<=PCH_1301$INFJ]<-3
PCH_1301$CLASSEPCH[!is.na(PCH_1301$RESULTAT) & PCH_1301$RESULTAT>PCH_1301$INFJ & PCH_1301$RESULTAT<=PCH_1301$INFO]<-4
PCH_1301$CLASSEPCH[!is.na(PCH_1301$RESULTAT) & PCH_1301$RESULTAT>PCH_1301$INFO]<-5
#PCH_1301$CLASSEPCH[!is.na(PCH_1301$RESULTAT) & PCH_1301$RESULTAT<0]<-99 # valeur négative possible

PCH_1301<-PCH_1301[,c("STATION", "DATEPRELEV", "HEUREPRELEV","PARAMETRE", "FRACTION", "RESULTAT", "REMARQUE", "CLASSEPCH", "INDICE", "IDTRI")]
PCH_1301<-merge(PCH_1301, STATION, by="STATION")
PCH<-rbind(PCH_1301,PCH_1301na,PCH_AUTRE)
rm(PCH_1301, PCH_1301na, PCH_AUTRE)
gc() ## compacte R
flush.console()

##############################
## Calcul de la valeur au rang 90
##############################
## PARAMETRE ordre décroissant : Sélection et tri des paramètres
PCHDESC<-PCH[PCH$PARAMETRE %in% c("1311", "1312", "1302min") ,]
PCHDESC<-PCHDESC[order(PCHDESC$RESULTAT, decreasing = TRUE),]
PCHDESC<-PCHDESC[order(PCHDESC$IDTRI),]
##rownames(PCHDESC)<-1:nrow(PCHDESC)
## PARAMETRE ordre décroissant : Calcul du rang et de la valeur de référence au rang90
if(nrow(PCHDESC)>0){
	PCHDESC$UN<-1
	PCHDESC$RANG<-c(lapply(split(PCHDESC, PCHDESC$IDTRI) , function(x) cumsum(x$UN)), recursive=T)
	NBENREG_DESC<-data.frame(table(PCHDESC$IDTRI))
	PCHDESC<-merge(PCHDESC,NBENREG_DESC,by.x="IDTRI",by.y="Var1")
	PCHDESC$R90<-round((PCHDESC$Freq*0.9)+0.5)
} else {
	PCHDESC$UN<-as.numeric()
	PCHDESC$RANG<-as.numeric()
	PCHDESC$Freq<-as.numeric()
	PCHDESC$R90<-as.numeric()
}
gc() ## compacte R

## PARAMETRE ordre croissant : Sélection et tri des paramètres
PCHASC<-PCH[PCH$PARAMETRE %in% c("1313","1841","1433","1350","1335","1339","1340","1301", "1302max") ,]
PCHASC<-PCHASC[order(PCHASC$RESULTAT, decreasing = FALSE),]
PCHASC<-PCHASC[order(PCHASC$IDTRI),]
##rownames(PCHASC)<-1:nrow(PCHASC)
## PARAMETRE ordre croissant : Calcul du rang et de la valeur de référence au rang90
if(nrow(PCHASC)>0){
	PCHASC$UN<-1
	PCHASC$RANG<-c(lapply(split(PCHASC, PCHASC$IDTRI) , function(x) cumsum(x$UN)), recursive=T)
	NBENREG_ASC<-data.frame(table(PCHASC$IDTRI))
	PCHASC<-merge(PCHASC,NBENREG_ASC,by.x="IDTRI",by.y="Var1")
	PCHASC$R90<-round((PCHASC$Freq*0.9)+0.5)
} else {
	PCHASC$UN<-as.numeric()
	PCHASC$RANG<-as.numeric()
	PCHASC$Freq<-as.numeric()
	PCHASC$R90<-as.numeric()
}
gc() ## compacte R

RLT_PCHDESC<-PCHDESC[PCHDESC$RANG == PCHDESC$R90,] ## *** Résultat Final : Valeur R90 pour paramètres décroissants ***
RLT_PCHASC<-PCHASC[PCHASC$RANG == PCHASC$R90,] ## *** Résultat Final : Valeur R90 pour paramètres croissants ***

## Respect de la fréquence minimale de prélèvement
if (FREQOKPCH == "oui") {
	RLT_PCHDESC$CLASSEPCH[RLT_PCHDESC$Freq<NBANFREQ]<-0
	RLT_PCHASC$CLASSEPCH[RLT_PCHASC$Freq<NBANFREQ]<-0
}

RLT_PCH<-rbind(RLT_PCHDESC,RLT_PCHASC) ## *** Résultat Final : Valeur R90 pour l'ensemble des paramètres ***
rm(NBENREG_DESC,NBENREG_ASC)

##############################
## Calcul de l'état PCH à la station & mis en forme du résultat avec les éléments de qualité
##############################
PCH_ELTQUALITE<-merge(RLT_PCH,PARAMETREPCH[,c("PARAMETRE","ELTQUALITE")],by="PARAMETRE")
PCH_ELTQUALITE<-aggregate(CLASSEPCH ~ STATION + ELTQUALITE, data = PCH_ELTQUALITE , max)
PCH_ELTQUALITE<-PCH_ELTQUALITE[order(PCH_ELTQUALITE$STATION, PCH_ELTQUALITE$ELTQUALITE),]
names(PCH_ELTQUALITE)[3]<-"ETATELT" #renomme la colonne CLASSEPCH sur laquelle il y a eu le max car correspond à l'état de element qualité

PCH_STATION<-aggregate(ETATELT ~ STATION, data = PCH_ELTQUALITE , max)
PCH_STATION<-PCH_STATION[order(PCH_STATION$STATION),]
names(PCH_STATION)[2]<-"ETATPCH" #renomme la colonne ETATELT sur laquelle il y a eu le max car correspond à l'état de la station

RLT_PCHFINAL<-PCH_STATION

for (i in  1:nrow(TABLEELT)) {
	TEMPELT<-PCH_ELTQUALITE[PCH_ELTQUALITE$ELTQUALITE==TABLEELT$ELTQUALITE[i],c("STATION", "ETATELT")]
	names(TEMPELT)[2]<-TABLEELT$ELTQUALITE[i]
	RLT_PCHFINAL<-merge(RLT_PCHFINAL,TEMPELT,by="STATION", all.x=TRUE)
	rm(TEMPELT)
}
flush.console()

##########################################
## Traitement des exceptions typologiques - Adaptations des classes d'état
##########################################
PCHEXCEPT<-rbind(PCHASC,PCHDESC)
PARAMETREPCHEXCEPT_v2<-PARAMETREPCHEXCEPT[!(PARAMETREPCHEXCEPT$PARAMETRE=="1301"),]
PCHEXCEPT<-merge(PCHEXCEPT, PARAMETREPCHEXCEPT_v2[,c("PARAMETRE", "SUPB", "INFB", "INFV", "INFJ", "INFO", "INFR")], by="PARAMETRE", all.x=TRUE, sort = FALSE)

## Exception naturellement FROID
PCHEXCEPT$CLASSEPCH[PCHEXCEPT$EXCEPT_FROID=="oui" & PCHEXCEPT$PARAMETRE %in% c("1335") & !is.na(PCHEXCEPT$RESULTAT) & PCHEXCEPT$RESULTAT>PCHEXCEPT$INFB & PCHEXCEPT$RESULTAT<=PCHEXCEPT$INFV]<-2
PCHEXCEPT$CLASSEPCH[PCHEXCEPT$EXCEPT_FROID=="oui" & PCHEXCEPT$PARAMETRE %in% c("1335") & !is.na(PCHEXCEPT$RESULTAT) & PCHEXCEPT$RESULTAT>PCHEXCEPT$INFV & PCHEXCEPT$RESULTAT<=PCHEXCEPT$INFJ]<-3

## Exception naturellement MO (COD adapté)
PCHEXCEPT$CLASSEPCH[PCHEXCEPT$EXCEPT_MO=="oui" & PCHEXCEPT$PARAMETRE %in% c("1841") & !is.na(PCHEXCEPT$RESULTAT) & PCHEXCEPT$RESULTAT<=PCHEXCEPT$INFB]<-1
PCHEXCEPT$CLASSEPCH[PCHEXCEPT$EXCEPT_MO=="oui" & PCHEXCEPT$PARAMETRE %in% c("1841") & !is.na(PCHEXCEPT$RESULTAT) & PCHEXCEPT$RESULTAT>PCHEXCEPT$INFB & PCHEXCEPT$RESULTAT<=PCHEXCEPT$INFV]<-2
PCHEXCEPT$CLASSEPCH[PCHEXCEPT$EXCEPT_MO=="oui" & PCHEXCEPT$PARAMETRE %in% c("1841") & !is.na(PCHEXCEPT$RESULTAT) & PCHEXCEPT$RESULTAT>PCHEXCEPT$INFV & PCHEXCEPT$RESULTAT<=PCHEXCEPT$INFJ]<-3

## Exception naturellement O2
PCHEXCEPT$CLASSEPCH[PCHEXCEPT$EXCEPT_O2=="oui" & PCHEXCEPT$PARAMETRE %in% c("1311", "1312") & !is.na(PCHEXCEPT$RESULTAT) & PCHEXCEPT$RESULTAT>=PCHEXCEPT$INFB]<-1
PCHEXCEPT$CLASSEPCH[PCHEXCEPT$EXCEPT_O2=="oui" & PCHEXCEPT$PARAMETRE %in% c("1311", "1312") & !is.na(PCHEXCEPT$RESULTAT) & PCHEXCEPT$RESULTAT>=PCHEXCEPT$INFV & PCHEXCEPT$RESULTAT<PCHEXCEPT$INFB]<-2
PCHEXCEPT$CLASSEPCH[PCHEXCEPT$EXCEPT_O2=="oui" & PCHEXCEPT$PARAMETRE %in% c("1311", "1312") & !is.na(PCHEXCEPT$RESULTAT) & PCHEXCEPT$RESULTAT>=PCHEXCEPT$INFJ & PCHEXCEPT$RESULTAT<PCHEXCEPT$INFV]<-3

## Exception naturellement ACID
# sur AELB le pH est retiré lorsqu'il déclasse au lieu d'être adapté comme dans l'arrêté
if (BASSIN=="AELB") {
	PCHEXCEPT<-PCHEXCEPT[!(PCHEXCEPT$EXCEPT_ACID=="oui" & PCHEXCEPT$PARAMETRE %in% c("1302min","1302max")),]
	} else {
	PCHEXCEPT$CLASSEPCH[PCHEXCEPT$EXCEPT_ACID=="oui" & PCHEXCEPT$PARAMETRE %in% c("1302min") & !is.na(PCHEXCEPT$RESULTAT) & PCHEXCEPT$RESULTAT>=PCHEXCEPT$INFB]<-1
	PCHEXCEPT$CLASSEPCH[PCHEXCEPT$EXCEPT_ACID=="oui" & PCHEXCEPT$PARAMETRE %in% c("1302min") & !is.na(PCHEXCEPT$RESULTAT) & PCHEXCEPT$RESULTAT>=PCHEXCEPT$INFV & PCHEXCEPT$RESULTAT<PCHEXCEPT$INFB]<-2
	PCHEXCEPT$CLASSEPCH[PCHEXCEPT$EXCEPT_ACID=="oui" & PCHEXCEPT$PARAMETRE %in% c("1302min") & !is.na(PCHEXCEPT$RESULTAT) & PCHEXCEPT$RESULTAT>=PCHEXCEPT$INFJ & PCHEXCEPT$RESULTAT<PCHEXCEPT$INFV]<-3
}

## Exception naturellement CHAUD, (retrait TEMPERATURE) extraction des données après la mise à jour des autres exceptions
PCHEXCEPT<-PCHEXCEPT[!(PCHEXCEPT$EXCEPT_CHAUD=="oui" & PCHEXCEPT$PARAMETRE=="1301"),]

## Exception naturellement TOURB (retrait du COD), extraction des données après la mise à jour des autres exceptions
PCHEXCEPT<-PCHEXCEPT[!(PCHEXCEPT$EXCEPT_TOURB=="oui" & PCHEXCEPT$PARAMETRE=="1841"),]

##########################################
## Traitement des exceptions typologiques - Calcul de l'état PCH
##########################################
#Sélection du rang 90
PCHEXCEPT_R90<-PCHEXCEPT[PCHEXCEPT$RANG == PCHEXCEPT$R90,]

## Respect de la fréquence minimale de prélèvement pour les stations en exception typologique

if (FREQOKPCH == "oui") {
	PCHEXCEPT_R90$CLASSEPCH[PCHEXCEPT_R90$Freq<NBANFREQ]<-0
}

PCHEXCEPT_ELTQUALITE<-merge(PCHEXCEPT_R90,PARAMETREPCH[,c("PARAMETRE","ELTQUALITE")],by="PARAMETRE")
PCHEXCEPT_ELTQUALITE<-aggregate(CLASSEPCH ~ STATION + ELTQUALITE, data = PCHEXCEPT_ELTQUALITE , max)
PCHEXCEPT_ELTQUALITE<-PCHEXCEPT_ELTQUALITE[order(PCHEXCEPT_ELTQUALITE$STATION, PCHEXCEPT_ELTQUALITE$ELTQUALITE),]
names(PCHEXCEPT_ELTQUALITE)[3]<-"ETATELT" #renomme la colonne CLASSEPCH sur laquelle il y a eu le max car correspond à l'état de element qualité

PCHEXCEPT_STATION<-aggregate(ETATELT ~ STATION, data = PCHEXCEPT_ELTQUALITE , max)
PCHEXCEPT_STATION<-PCHEXCEPT_STATION[order(PCHEXCEPT_STATION$STATION),]
names(PCHEXCEPT_STATION)[2]<-"ETATPCH" #renomme la colonne ETATELT sur laquelle il y a eu le max car correspond à l'état de la station
gc() ## compacte R

##########################################
## Traitement des exceptions typologiques - Mise en forme du tableau pour les éléments de qualité
##########################################
RLT_PCHFINALEXCEPT<-PCHEXCEPT_STATION

for (i in  1:nrow(TABLEELT)  ) {
	TEMPELT<-PCHEXCEPT_ELTQUALITE[PCHEXCEPT_ELTQUALITE$ELTQUALITE==TABLEELT$ELTQUALITE[i],c("STATION", "ETATELT")]
	names(TEMPELT)[2]<-TABLEELT$ELTQUALITE[i]
	RLT_PCHFINALEXCEPT<-merge(RLT_PCHFINALEXCEPT,TEMPELT,by="STATION", all.x=TRUE)
	rm(TEMPELT)
}
RLT_PCHFINALEXCEPT$MODELISE<-"non" # résultat hors PCH modélisée (PEGASE)
RLT_PCHFINALEXCEPT_NAMES<-names(RLT_PCHFINALEXCEPT) # utile pour la mise en forme des tableaux finaux
PCHFINALEXCEPT_ASSOUP<-RLT_PCHFINALEXCEPT #utile pour assouplissement pour calculer nb de paramètre déclassants + table complétée avec PEGASE si nécessaire
#PCHFINALEXCEPT_ASSOUP$MODELISE<-"non" # résultat hors PCH modélisée (PEGASE)
#PCHFINALEXCEPT<-RLT_PCHFINALEXCEPT #utile pour assouplissement pour assouplir les elt qualité et l'état
flush.console()

##########################################
## Traitement des exceptions typologiques - Mise en forme du tableau pour les paramètres
## On affiche les résultats par paramètre sans tenir compte des exceptions typo. L'exception intervient dans la valeur de l'élément de qualité
##########################################
PARAMETREPCH<-PARAMETREPCH[order(PARAMETREPCH$IDTRI),]

for (i in  1:nrow(PARAMETREPCH)) {
	TEMPP<-RLT_PCH[RLT_PCH$PARAMETRE==PARAMETREPCH$PARAMETRE[i],c("STATION", "CLASSEPCH")]
	names(TEMPP)[2]<-PARAMETREPCH$NOM[i]
	RLT_PCHFINALEXCEPT<-merge(RLT_PCHFINALEXCEPT,TEMPP,by="STATION", all.x=TRUE)
	rm(TEMPP)
}

# Tableau utile uniquement pour le calcul de l'assouplissement en affichant les paramètres en exception typo, contrairement au tableau précédent
for (i in  1:nrow(PARAMETREPCH)) {
	TEMPP<-PCHEXCEPT_R90[PCHEXCEPT_R90$PARAMETRE==PARAMETREPCH$PARAMETRE[i],c("STATION", "CLASSEPCH")]
	names(TEMPP)[2]<-PARAMETREPCH$NOM[i]
	PCHFINALEXCEPT_ASSOUP<-merge(PCHFINALEXCEPT_ASSOUP,TEMPP,by="STATION", all.x=TRUE)
	rm(TEMPP)
}
PCHFINALEXCEPT_ASSOUP_NAMES<-names(PCHFINALEXCEPT_ASSOUP) # nom de champ dans un vecteur pour les rappeler ultérieurement
flush.console()

### OPTION AESN : grille NO3 spécifique
if (BASSIN=="AESN" & OPTIONNO3 == "oui") {
	PCH_NO3AESN<-RLT_PCH[RLT_PCH$PARAMETRE=="1340",c("STATION","PARAMETRE","RESULTAT", "CLASSEPCH")]
	PCH_NO3AESN$no3v2[!is.na(PCH_NO3AESN$RESULTAT) & PCH_NO3AESN$RESULTAT<=PARAMNO3AESN$SEUIL[1]]<-1
	PCH_NO3AESN$no3v2[!is.na(PCH_NO3AESN$RESULTAT) & PCH_NO3AESN$RESULTAT>PARAMNO3AESN$SEUIL[1] & PCH_NO3AESN$RESULTAT<=PARAMNO3AESN$SEUIL[2]]<-2
	PCH_NO3AESN$no3v2[!is.na(PCH_NO3AESN$RESULTAT) & PCH_NO3AESN$RESULTAT>PARAMNO3AESN$SEUIL[2] & PCH_NO3AESN$RESULTAT<=PARAMNO3AESN$SEUIL[3]]<-3
	PCH_NO3AESN$no3v2[!is.na(PCH_NO3AESN$RESULTAT) & PCH_NO3AESN$RESULTAT>PARAMNO3AESN$SEUIL[3] & PCH_NO3AESN$RESULTAT<=PARAMNO3AESN$SEUIL[4]]<-4
	PCH_NO3AESN$no3v2[!is.na(PCH_NO3AESN$RESULTAT) & PCH_NO3AESN$RESULTAT>PARAMNO3AESN$SEUIL[4]]<-5
	PCH_NO3AESN$no3v2[!is.na(PCH_NO3AESN$RESULTAT) & PCH_NO3AESN$RESULTAT<0]<-99
	PCH_NO3AESN$no3v2[!is.na(PCH_NO3AESN$RESULTAT) & PCH_NO3AESN$CLASSEPCH ==0]<-0  ## pour etre cohérent avec Frequence de prélèvement lors elle n'est pas respectée
	
	RLT_PCHFINALEXCEPT<-merge(RLT_PCHFINALEXCEPT,PCH_NO3AESN[,c("STATION","no3v2")],by="STATION",all.x=TRUE)
	rm(PCH_NO3AESN)
	# Ajout du parametre NO3v2 aux noms de colonnes des résultats
	PARAMETREPCH_NAMES<-c(PARAMETREPCH_NAMES,"no3v2")
}
gc() ## compacte R

#####################################
# PEGASE : intégration PCH modélisée
#####################################
if (SEEEPEGASE=="oui") {
	
# Selection des stations n'étant pas dans les résultats d'ETATPCH et ayant des données modélisées
	DATAPEGASE<-DATAPEGASE[!(DATAPEGASE$STATION %in% c(PCHFINALEXCEPT_ASSOUP$STATION)),]
	
	# on fait les calculs PEGASE uniquement s'il y a des données à traiter n'étant pas déjà monitorées
	if (nrow(DATAPEGASE)>0) {
	
		if (SEEEPEGASEDATA=="valeur") {
	# Mise en forme d'un nouveau tableau en ligne pour faciliter le calcul (basé sur le même principe de calcul que les autres scripts)
			PEGTEMPO2<-DATAPEGASE[,c("STATION","PEGO2")]
			names(PEGTEMPO2)[2]<-"RESULTAT"
			PEGTEMPO2$PARAMETRE<-as.character("1311")
		
			PEGTEMPSATO2<-DATAPEGASE[,c("STATION","PEGSATO2")]
			names(PEGTEMPSATO2)[2]<-"RESULTAT"
			PEGTEMPSATO2$PARAMETRE<-as.character("1312")
		
			PEGTEMPDBO5<-DATAPEGASE[,c("STATION","PEGDBO5")]
			names(PEGTEMPDBO5)[2]<-"RESULTAT"
			PEGTEMPDBO5$PARAMETRE<-as.character("1313")
		
			PEGTEMPCOD<-DATAPEGASE[,c("STATION","PEGCOD")]
			names(PEGTEMPCOD)[2]<-"RESULTAT"
			PEGTEMPCOD$PARAMETRE<-as.character("1841")
		
			PEGTEMPPO43<-DATAPEGASE[,c("STATION","PEGPO43")]
			names(PEGTEMPPO43)[2]<-"RESULTAT"
			PEGTEMPPO43$PARAMETRE<-as.character("1433")
		
			PEGTEMPPHOS<-DATAPEGASE[,c("STATION","PEGPHOS")]
			names(PEGTEMPPHOS)[2]<-"RESULTAT"
			PEGTEMPPHOS$PARAMETRE<-as.character("1350")
		
			PEGTEMPNH4<-DATAPEGASE[,c("STATION","PEGNH4")]
			names(PEGTEMPNH4)[2]<-"RESULTAT"
			PEGTEMPNH4$PARAMETRE<-as.character("1335")
		
			PEGTEMPNO2<-DATAPEGASE[,c("STATION","PEGNO2")]
			names(PEGTEMPNO2)[2]<-"RESULTAT"
			PEGTEMPNO2$PARAMETRE<-as.character("1339")
		
			PEGTEMPNO3<-DATAPEGASE[,c("STATION","PEGNO3")]
			names(PEGTEMPNO3)[2]<-"RESULTAT"
			PEGTEMPNO3$PARAMETRE<-as.character("1340")
		
			PEGTEMPTEMPE<-DATAPEGASE[,c("STATION","PEGTEMPE")]
			names(PEGTEMPTEMPE)[2]<-"RESULTAT"
			PEGTEMPTEMPE$PARAMETRE<-as.character("1301")
		
			PEGTEMPPHMIN<-DATAPEGASE[,c("STATION","PEGPHMIN")]
			names(PEGTEMPPHMIN)[2]<-"RESULTAT"
			PEGTEMPPHMIN$PARAMETRE<-as.character("1302min")
		
			PEGTEMPPHMAX<-DATAPEGASE[,c("STATION","PEGPHMAX")]
			names(PEGTEMPPHMAX)[2]<-"RESULTAT"
			PEGTEMPPHMAX$PARAMETRE<-as.character("1302max")
		
			PEGASEMEF<-rbind(PEGTEMPO2, PEGTEMPSATO2, PEGTEMPDBO5, PEGTEMPCOD, PEGTEMPPO43, PEGTEMPPHOS, PEGTEMPNH4, PEGTEMPNO2, PEGTEMPNO3, PEGTEMPTEMPE, PEGTEMPPHMIN, PEGTEMPPHMAX)
			#rm(PEGTEMPO2, PEGTEMPSATO2, PEGTEMPDBO5, PEGTEMPCOD, PEGTEMPPO43, PEGTEMPPHOS, PEGTEMPNH4, PEGTEMPNO2, PEGTEMPNO3, PEGTEMPTEMPE, PEGTEMPPHMIN, PEGTEMPPHMAX)
			PEGASEMEF<-merge(PEGASEMEF,PARAMETREPCH[,c("PARAMETRE", "SUPB", "INFB", "INFV", "INFJ", "INFO", "INFR")],by="PARAMETRE", all.x=TRUE, sort = FALSE)

	## PEGASE : PARAMETRE ordre croissant (Valeur seuil de l'état PEGASE)
		# Attention les valeurs seuils dans la table PARAMETREPCH (colonne INFR) ont été attribuées pour le calcul de l'indice et non la mise en classe
			PEGASEMEF$CLASSEPCH[PEGASEMEF$PARAMETRE %in% c("1313","1841","1433","1350","1335","1339","1340","1302max") & !is.na(PEGASEMEF$RESULTAT) & PEGASEMEF$RESULTAT<=PEGASEMEF$INFB]<-1
			PEGASEMEF$CLASSEPCH[PEGASEMEF$PARAMETRE %in% c("1313","1841","1433","1350","1335","1339","1340","1302max") & !is.na(PEGASEMEF$RESULTAT) & PEGASEMEF$RESULTAT>PEGASEMEF$INFB & PEGASEMEF$RESULTAT<=PEGASEMEF$INFV]<-2
			PEGASEMEF$CLASSEPCH[PEGASEMEF$PARAMETRE %in% c("1313","1841","1433","1350","1335","1339","1340","1302max") & !is.na(PEGASEMEF$RESULTAT) & PEGASEMEF$RESULTAT>PEGASEMEF$INFV & PEGASEMEF$RESULTAT<=PEGASEMEF$INFJ]<-3
			PEGASEMEF$CLASSEPCH[PEGASEMEF$PARAMETRE %in% c("1313","1841","1433","1350","1335","1339","1340","1302max") & !is.na(PEGASEMEF$RESULTAT) & PEGASEMEF$RESULTAT>PEGASEMEF$INFJ & PEGASEMEF$RESULTAT<=PEGASEMEF$INFO]<-4
			PEGASEMEF$CLASSEPCH[PEGASEMEF$PARAMETRE %in% c("1313","1841","1433","1350","1335","1339","1340","1302max") & !is.na(PEGASEMEF$RESULTAT) & PEGASEMEF$RESULTAT>PEGASEMEF$INFO]<-5
			PEGASEMEF$CLASSEPCH[PEGASEMEF$PARAMETRE %in% c("1313","1841","1433","1350","1335","1339","1340","1302max") & !is.na(PEGASEMEF$RESULTAT) & PEGASEMEF$RESULTAT<0]<-99 

		# PEGASE : Cas particulier des NO3 (3 classes). On réaffecte les classes 4 & 5. Les seuils des classes 4 et 5 ont été définis uniquement pour le calcul de l'indice dans la table PARAMETREPCH
			PEGASEMEF$CLASSEPCH[PEGASEMEF$PARAMETRE %in% c("1340") & !is.na(PEGASEMEF$RESULTAT) & PEGASEMEF$CLASSEPCH>=4]<-3

	## PEGASE : PARAMETRE ordre décroissant (Valeur seuil de l'état PEGASE)
		# Attention les valeurs seuils dans la table PARAMETREPCH (colonne INFR) ont été attribuées pour le calcul de l'indice et non la mise en classe
			PEGASEMEF$CLASSEPCH[PEGASEMEF$PARAMETRE %in% c("1311", "1312", "1302min") & !is.na(PEGASEMEF$RESULTAT) & PEGASEMEF$RESULTAT>=PEGASEMEF$INFB]<-1
			PEGASEMEF$CLASSEPCH[PEGASEMEF$PARAMETRE %in% c("1311", "1312", "1302min") & !is.na(PEGASEMEF$RESULTAT) & PEGASEMEF$RESULTAT>=PEGASEMEF$INFV & PEGASEMEF$RESULTAT<PEGASEMEF$INFB]<-2
			PEGASEMEF$CLASSEPCH[PEGASEMEF$PARAMETRE %in% c("1311", "1312", "1302min") & !is.na(PEGASEMEF$RESULTAT) & PEGASEMEF$RESULTAT>=PEGASEMEF$INFJ & PEGASEMEF$RESULTAT<PEGASEMEF$INFV]<-3
			PEGASEMEF$CLASSEPCH[PEGASEMEF$PARAMETRE %in% c("1311", "1312", "1302min") & !is.na(PEGASEMEF$RESULTAT) & PEGASEMEF$RESULTAT>=PEGASEMEF$INFO & PEGASEMEF$RESULTAT<PEGASEMEF$INFJ]<-4
			PEGASEMEF$CLASSEPCH[PEGASEMEF$PARAMETRE %in% c("1311", "1312", "1302min") & !is.na(PEGASEMEF$RESULTAT) & PEGASEMEF$RESULTAT<PEGASEMEF$INFO]<-5
			PEGASEMEF$CLASSEPCH[PEGASEMEF$PARAMETRE %in% c("1311", "1312", "1302min") & !is.na(PEGASEMEF$RESULTAT) & PEGASEMEF$RESULTAT<0]<-99 

	## PEGASE : Traitement de la température
			PEGASEMEF<-PEGASEMEF[,c("STATION", "PARAMETRE", "RESULTAT", "CLASSEPCH")]
			PEGASEMEF<-merge(PEGASEMEF, STATION[,c("STATION", "CONTEXTE_PISCICOLE", "EXCEPT_FROID", "EXCEPT_CHAUD", "EXCEPT_ACID", "EXCEPT_MO", "EXCEPT_TOURB", "EXCEPT_O2")], by="STATION", all.x=TRUE)
			cond1301<-PEGASEMEF$PARAMETRE=="1301"
			condAUTRE<-!(PEGASEMEF$PARAMETRE=="1301")
			PEGASEMEF$PARATEMP[cond1301]<-as.character(paste(PEGASEMEF$PARAMETRE[cond1301],PEGASEMEF$CONTEXTE_PISCICOLE[cond1301],sep=""))
			PEGASEMEF$PARATEMP[condAUTRE]<-as.character(paste(PEGASEMEF$PARAMETRE[condAUTRE],"nc",sep=""))
			rm(cond1301, condAUTRE)
			PEGASEMEF<-merge(PEGASEMEF, PARAMETREPCHEXCEPT[,c("PARATEMP", "INFB", "INFV", "INFJ", "INFO", "INFR")], by="PARATEMP", all.x=TRUE)

			PEGASEMEF$CLASSEPCH[PEGASEMEF$PARAMETRE %in% c("1301") & !is.na(PEGASEMEF$RESULTAT) & PEGASEMEF$RESULTAT<=PEGASEMEF$INFB]<-1
			PEGASEMEF$CLASSEPCH[PEGASEMEF$PARAMETRE %in% c("1301") & !is.na(PEGASEMEF$RESULTAT) & PEGASEMEF$RESULTAT>PEGASEMEF$INFB & PEGASEMEF$RESULTAT<=PEGASEMEF$INFV]<-2
			PEGASEMEF$CLASSEPCH[PEGASEMEF$PARAMETRE %in% c("1301") & !is.na(PEGASEMEF$RESULTAT) & PEGASEMEF$RESULTAT>PEGASEMEF$INFV & PEGASEMEF$RESULTAT<=PEGASEMEF$INFJ]<-3
			PEGASEMEF$CLASSEPCH[PEGASEMEF$PARAMETRE %in% c("1301") & !is.na(PEGASEMEF$RESULTAT) & PEGASEMEF$RESULTAT>PEGASEMEF$INFJ & PEGASEMEF$RESULTAT<=PEGASEMEF$INFO]<-4
			PEGASEMEF$CLASSEPCH[PEGASEMEF$PARAMETRE %in% c("1301") & !is.na(PEGASEMEF$RESULTAT) & PEGASEMEF$RESULTAT>PEGASEMEF$INFO]<-5
			PEGASEMEF$CLASSEPCH[PEGASEMEF$PARAMETRE %in% c("1841") & !is.na(PEGASEMEF$RESULTAT) & PEGASEMEF$RESULTAT<0]<-99 # retrait du paramètre 1301 car <0 est possible !
			PEGASEMEF_XLS<-PEGASEMEF # utile uniquement pour l'export sous Excel des données. On exporte en conservant les paramètres sans tenir compte des exceptions typo
		
	## PEGASE : Exception typologique
		## PEGASE : Exception naturellement FROID
			PEGASEMEF$CLASSEPCH[PEGASEMEF$EXCEPT_FROID=="oui" & PEGASEMEF$PARAMETRE %in% c("1335") & !is.na(PEGASEMEF$RESULTAT) & PEGASEMEF$RESULTAT>PEGASEMEF$INFB & PEGASEMEF$RESULTAT<=PEGASEMEF$INFV]<-2
			PEGASEMEF$CLASSEPCH[PEGASEMEF$EXCEPT_FROID=="oui" & PEGASEMEF$PARAMETRE %in% c("1335") & !is.na(PEGASEMEF$RESULTAT) & PEGASEMEF$RESULTAT>PEGASEMEF$INFV & PEGASEMEF$RESULTAT<=PEGASEMEF$INFJ]<-3

		## PEGASE : Exception naturellement MO (COD adapté)
			PEGASEMEF$CLASSEPCH[PEGASEMEF$EXCEPT_MO=="oui" & PEGASEMEF$PARAMETRE %in% c("1841") & !is.na(PEGASEMEF$RESULTAT) & PEGASEMEF$RESULTAT<=PEGASEMEF$INFB]<-1
			PEGASEMEF$CLASSEPCH[PEGASEMEF$EXCEPT_MO=="oui" & PEGASEMEF$PARAMETRE %in% c("1841") & !is.na(PEGASEMEF$RESULTAT) & PEGASEMEF$RESULTAT>PEGASEMEF$INFB & PEGASEMEF$RESULTAT<=PEGASEMEF$INFV]<-2
			PEGASEMEF$CLASSEPCH[PEGASEMEF$EXCEPT_MO=="oui" & PEGASEMEF$PARAMETRE %in% c("1841") & !is.na(PEGASEMEF$RESULTAT) & PEGASEMEF$RESULTAT>PEGASEMEF$INFV & PEGASEMEF$RESULTAT<=PEGASEMEF$INFJ]<-3

		## PEGASE : Exception naturellement O2
			PEGASEMEF$CLASSEPCH[PEGASEMEF$EXCEPT_O2=="oui" & PEGASEMEF$PARAMETRE %in% c("1311", "1312") & !is.na(PEGASEMEF$RESULTAT) & PEGASEMEF$RESULTAT>=PEGASEMEF$INFB]<-1
			PEGASEMEF$CLASSEPCH[PEGASEMEF$EXCEPT_O2=="oui" & PEGASEMEF$PARAMETRE %in% c("1311", "1312") & !is.na(PEGASEMEF$RESULTAT) & PEGASEMEF$RESULTAT>=PEGASEMEF$INFV & PEGASEMEF$RESULTAT<PEGASEMEF$INFB]<-2
			PEGASEMEF$CLASSEPCH[PEGASEMEF$EXCEPT_O2=="oui" & PEGASEMEF$PARAMETRE %in% c("1311", "1312") & !is.na(PEGASEMEF$RESULTAT) & PEGASEMEF$RESULTAT>=PEGASEMEF$INFJ & PEGASEMEF$RESULTAT<PEGASEMEF$INFV]<-3

		## PEGASE : Exception naturellement ACID
		# sur AELB le pH est retiré lorsqu'il déclasse au lieu d'être adapté comme dans l'arrêté. ici on reprend la même valeur, on le retire au niveau du calcul de l'élt de qualité
				if (BASSIN=="AELB") {
				PEGASEMEF$CLASSEPCH[PEGASEMEF$EXCEPT_ACID=="oui" & PEGASEMEF$PARAMETRE %in% c("1302min") & !is.na(PEGASEMEF$RESULTAT)]<-PEGASEMEF$CLASSEPCH[PEGASEMEF$EXCEPT_ACID=="oui" & PEGASEMEF$PARAMETRE %in% c("1302min") & !is.na(PEGASEMEF$RESULTAT)]
				} else {
				PEGASEMEF$CLASSEPCH[PEGASEMEF$EXCEPT_ACID=="oui" & PEGASEMEF$PARAMETRE %in% c("1302min") & !is.na(PEGASEMEF$RESULTAT) & PEGASEMEF$RESULTAT>=PEGASEMEF$INFB]<-1
				PEGASEMEF$CLASSEPCH[PEGASEMEF$EXCEPT_ACID=="oui" & PEGASEMEF$PARAMETRE %in% c("1302min") & !is.na(PEGASEMEF$RESULTAT) & PEGASEMEF$RESULTAT>=PEGASEMEF$INFV & PEGASEMEF$RESULTAT<PEGASEMEF$INFB]<-2
				PEGASEMEF$CLASSEPCH[PEGASEMEF$EXCEPT_ACID=="oui" & PEGASEMEF$PARAMETRE %in% c("1302min") & !is.na(PEGASEMEF$RESULTAT) & PEGASEMEF$RESULTAT>=PEGASEMEF$INFJ & PEGASEMEF$RESULTAT<PEGASEMEF$INFV]<-3
				}
		
	## PEGASE : Mise en forme du tableau de classe d'état		
			PEGASE<-data.frame(DATAPEGASE[,c("STATION")])
			names(PEGASE)[1]<-"STATION"
				for (i in  1:nrow(PARAMETREPCH)  ) {
				TEMPPEG<-PEGASEMEF[PEGASEMEF$PARAMETRE==PARAMETREPCH$PARAMETRE[i],c("STATION", "CLASSEPCH")]
				names(TEMPPEG)[2]<-paste("PEG",toupper(PARAMETREPCH$NOM[i]), sep="")
				PEGASE<-merge(PEGASE,TEMPPEG,by="STATION", all.x=TRUE)
				rm(TEMPPEG)
				}
			#rm(PEGASEMEF)	
			
			# Mise en forme du tableau pour l'export des données sous Excel
			PEGASE_XLS<-data.frame(DATAPEGASE[,c("STATION")])
			names(PEGASE_XLS)[1]<-"STATION"
				for (i in  1:nrow(PARAMETREPCH)  ) {
				TEMPPEGXLS<-PEGASEMEF_XLS[PEGASEMEF_XLS$PARAMETRE==PARAMETREPCH$PARAMETRE[i],c("STATION", "CLASSEPCH")]
				names(TEMPPEGXLS)[2]<-paste("PEG",toupper(PARAMETREPCH$NOM[i]), sep="")
				PEGASE_XLS<-merge(PEGASE_XLS,TEMPPEGXLS,by="STATION", all.x=TRUE)
				rm(TEMPPEGXLS)
				}
		} else {
			PEGASE<-DATAPEGASE
			PEGASE_XLS<-DATAPEGASE # utile uniquement pour l'export sous Excel des données. On exporte en conservant les paramètres sans tenir compte des exceptions typo
		}

	## PEGASE : Calcul des état par élèments de qualité	
		PEGASE_NAMES<-names(PEGASE)
		PEGASE<-merge(PEGASE, STATION[,c("STATION", "EXCEPT_FROID", "EXCEPT_CHAUD", "EXCEPT_ACID", "EXCEPT_MO", "EXCEPT_TOURB", "EXCEPT_O2")], by="STATION", all.x=TRUE)

		# Bilan O2 : calcul avec exception typologique pour les paramètres retirés
		PEGASE$PEGBILANO2<-pmax(PEGASE$PEGO2, PEGASE$PEGSATO2, PEGASE$PEGDBO5, PEGASE$PEGCOD, na.rm=TRUE)
		condB02<-PEGASE$EXCEPT_TOURB=="oui"
		PEGASE$PEGBILANO2[condB02]<-pmax(PEGASE$PEGO2[condB02], PEGASE$PEGSATO2[condB02], PEGASE$PEGDBO5[condB02], na.rm=TRUE)
		rm(condB02)

		# Nutriment : pas de retrait de paramètre avec exception typologique
		PEGASE$PEGNUT<-pmax(PEGASE$PEGPO43, PEGASE$PEGPHOS, PEGASE$PEGNH4, PEGASE$PEGNO2,PEGASE$PEGNO3, na.rm=TRUE)

		# Température : calcul avec exception typologique pour les paramètres retirés
		PEGASE$PEGTEMP<-PEGASE$PEGTEMPE
		condTEMP<-PEGASE$EXCEPT_CHAUD=="oui"
		PEGASE$PEGTEMP[condTEMP]<-NA

		# Acidification : calcul avec exception typologique (cas uniquement sur AELB)
		if (BASSIN=="AELB") {
			PEGASE$PEGACID<-NA #sur AELB le pH est retiré si exception typologique
			} else {
			PEGASE$PEGACID<-pmax(PEGASE$PEGPHMIN, PEGASE$PEGPHMAX, na.rm=TRUE)
			}

	## PEGASE : Calcul de l'état PCH
		PEGASE$PEGETATPCH<-pmax(PEGASE$PEGBILANO2, PEGASE$PEGNUT, PEGASE$PEGTEMP, PEGASE$PEGACID, na.rm=TRUE)
		
	# PEGASE : intégration de PEGASE au résultat non modélisé
		# mise en conformité des noms de colonnes pour le RBIND
		names(PEGASE)<-tolower(gsub("PEG","",names(PEGASE)))
		names(PEGASE)[1]<-"STATION"
		names(PEGASE)[24]<-"ETATPCH"
		PEGASE<-PEGASE[,c("STATION", PARAMETREPCH_NAMES[1:12],PARAMETREPCHELT_NAMES,ETATPCH_NAMES)] # retrait du no3v2 dans names pour éviter bug du select
		PEGASE$MODELISE<-"oui"
		PCHFINALEXCEPT_ASSOUP<-rbind(PCHFINALEXCEPT_ASSOUP,PEGASE)
		

		flush.console()
	} else {
	SEEEPEGASE<-"non"
	tkmessageBox(message ="Aucune donnée PEGASE n'est disponible pour les stations sans état physicochimique\n le calcul poursuivra sans considérer les données PEGASE")
	}
}
# PEGASE : Fin de calcul
#########################
gc() ## compacte R
flush.console()

