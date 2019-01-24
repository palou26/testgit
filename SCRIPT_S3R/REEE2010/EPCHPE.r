##############################################
## SEEE - COURS D'EAU : état physicochimique
## Application de l'arrêté de janvier 2010
##############################################
#EPCHDEB<-date()
# DATAPCH$STATION<-as.character(DATAPCH$STATION)
# DATAPCH$PARAMETRE<-as.character(DATAPCH$PARAMETRE)
# DATAPCH$FRACTION<-as.character(DATAPCH$FRACTION)
# DATAPCH$RESULTAT<-as.numeric(DATAPCH$RESULTAT)
# DATAPCH$REMARQUE<-as.character(DATAPCH$REMARQUE)
# DATAPCH$DATEPRELEV<-as.character(DATAPCH$DATEPRELEV)
# DATAPCH$HEUREPRELEV<-as.character(DATAPCH$HEUREPRELEV)

############################################
# recalcul des valeurs de paramètres #######
############################################
DATAPCH$RESULTAT2<-DATAPCH$RESULTAT
###NH4 -> conversion masse moléculaire pour avoir Azote minéral
DATAPCH$RESULTAT2[DATAPCH$PARAMETRE == "1335"]<-DATAPCH$RESULTAT[DATAPCH$PARAMETRE == "1335"] / 1.29
###NO3-> conversion masse moléculaire pour avoir Azote minéral
DATAPCH$RESULTAT2[DATAPCH$PARAMETRE == "1340"]<-DATAPCH$RESULTAT[DATAPCH$PARAMETRE == "1340"] / 4.43
###PO43-> conversion masse moléculaire pour avoir Phosphore
DATAPCH$RESULTAT2[DATAPCH$PARAMETRE == "1350"]<-DATAPCH$RESULTAT[DATAPCH$PARAMETRE == "1350"] /3.07

### Choix de prendre Azote minéral d'hiver si temps de sejour > 2 mois
## ou de prendre l'Azote minéral d'été si temps de sejour < 2 mois
DATAPCH<-merge(DATAPCH,STATION[,c("STATION","NOMSTATION","EUCD","ZMOY","STRATIFIE","TPSSEJOUR","FIABILITSEJOUR","EXCEPT_NIT")],by="STATION")


##On somme par paragroup pour avec le N mineral et selection des OK
DATAPCH<-merge(DATAPCH,PARAMETREPCH[,c("PARAMETRE","PARAGROUP", "ELTQUALITE")],by="PARAMETRE")
cond_exeptnit<-!(DATAPCH$EXCEPT_NIT == "oui" & DATAPCH$PARAMETRE == "1340") #prise en compte de l'expetion Nitrates
DATAPCHPARAGROUP<-aggregate(RESULTAT2 ~ STATION + PARAGROUP + ELTQUALITE + DATEPRELEV + HEUREPRELEV + TPSSEJOUR+ FIABILITSEJOUR + SAISON, data = DATAPCH[cond_exeptnit,], sum)
#On conserve dans un coin la piste des parametres
LSTPARAMGROUP<-sort(unique(DATAPCHPARAGROUP$PARAGROUP))
### On prend la valeur maximale (pour tous les parametres, sauf pour transparence où on prend la moyenne)
PCH<-data.frame(
				STATION = as.character(), 
				PARAGROUP= as.character() , 
				TPSSEJOUR= as.character()  ,
				FIABILITSEJOUR=as.character()  ,
				SAISON= as.character() ,
				RESULTAT2=as.numeric()
				)
if (	nrow(DATAPCHPARAGROUP[DATAPCHPARAGROUP$ELTQUALITE == "nut",]) > 0 ) {			
PCH_MAX_nut<-aggregate(RESULTAT2 ~ STATION + PARAGROUP + TPSSEJOUR+ FIABILITSEJOUR + SAISON , data = DATAPCHPARAGROUP[DATAPCHPARAGROUP$ELTQUALITE == "nut",] , max)
} else {
PCH_MAX_nut<-PCH
}
if (	nrow(DATAPCHPARAGROUP[DATAPCHPARAGROUP$ELTQUALITE == "transpa",]) > 0 ) {			
PCH_MOY_trans<-aggregate(RESULTAT2 ~ STATION + PARAGROUP + TPSSEJOUR+ FIABILITSEJOUR + SAISON , data = DATAPCHPARAGROUP[DATAPCHPARAGROUP$ELTQUALITE == "transpa",] , max)
} else {
PCH_MOY_trans<-PCH
}
if (	nrow(DATAPCHPARAGROUP[DATAPCHPARAGROUP$ELTQUALITE == "bilano2",]) > 0 ) {			
PCH_MED_bilanO2<-aggregate(RESULTAT2 ~ STATION + PARAGROUP + TPSSEJOUR+ FIABILITSEJOUR + SAISON , data = DATAPCHPARAGROUP[DATAPCHPARAGROUP$ELTQUALITE == "bilano2",] , max)
} else {
PCH_MED_bilanO2<-PCH
}

PCH<-rbind(PCH_MAX_nut,PCH_MOY_trans,PCH_MED_bilanO2)
names(PCH)[6]<-"RESULTAT"
##CLASSE PCH
PARAGROUPPCH<-PARAMETREPCH[!duplicated(PARAMETREPCH[,"PARAGROUP"]),c("PARAGROUP", "ELTQUALITE",  "IDTRI" ,"SUPB" ,"INFB" ,"INFV" ,"INFJ" ,"INFO", "INFR" ) ]
PCH<-merge(PCH,PARAGROUPPCH,by="PARAGROUP")
names(PCH)[1]<-"PARAMETRE"



gc() ## compacte R
PCH$CLASSEPCH<- 0
PCH$INDICE<- -1
PCH$IDTRI<-as.character(paste(PCH$STATION,PCH$PARAMETRE,sep=""))
gc() ## compacte R

##############################
## Calcul de la classe d'état PCH
##############################
PCH$RESULTAT<-round(PCH$RESULTAT,4)
gc() ## compacte R

## PARAMETRE ordre croissant : Valeur seuil de l'état PCH
# Attention les valeurs seuils dans la table PARAMETREPCH (colonne INFR) a été attribuées pour le calcul de l'indice et non la mise en classe
PCH$CLASSEPCH[PCH$PARAMETRE  != "transp" & !is.na(PCH$RESULTAT) & PCH$RESULTAT<=PCH$INFB]<-1
PCH$CLASSEPCH[PCH$PARAMETRE  !="transp" & !is.na(PCH$RESULTAT) & PCH$RESULTAT>PCH$INFB & PCH$RESULTAT<=PCH$INFV]<-2
PCH$CLASSEPCH[PCH$PARAMETRE  !="transp" & !is.na(PCH$RESULTAT) & PCH$RESULTAT>PCH$INFV & PCH$RESULTAT<=PCH$INFJ]<-3
PCH$CLASSEPCH[PCH$PARAMETRE  !="transp" & !is.na(PCH$RESULTAT) & PCH$RESULTAT>PCH$INFJ & PCH$RESULTAT<=PCH$INFO]<-4
PCH$CLASSEPCH[PCH$PARAMETRE  !="transp"  & !is.na(PCH$RESULTAT) & PCH$RESULTAT>PCH$INFO]<-5
PCH$CLASSEPCH[PCH$PARAMETRE != "transp" & !is.na(PCH$RESULTAT) & PCH$RESULTAT<0]<-99 


## PARAMETRE ordre décroissant : Valeur seuil de l'état PCH
# Attention les valeurs seuils dans la table PARAMETREPCH (colonne INFR) a été attribuées pour le calcul de l'indice et non la mise en classe
PCH$CLASSEPCH[PCH$PARAMETRE  == "transp" & !is.na(PCH$RESULTAT) & PCH$RESULTAT>=PCH$INFB]<-1
PCH$CLASSEPCH[PCH$PARAMETRE == "transp" & !is.na(PCH$RESULTAT) & PCH$RESULTAT>=PCH$INFV & PCH$RESULTAT<PCH$INFB]<-2
PCH$CLASSEPCH[PCH$PARAMETRE == "transp" & !is.na(PCH$RESULTAT) & PCH$RESULTAT>=PCH$INFJ & PCH$RESULTAT<PCH$INFV]<-3
PCH$CLASSEPCH[PCH$PARAMETRE == "transp" & !is.na(PCH$RESULTAT) & PCH$RESULTAT>=PCH$INFO & PCH$RESULTAT<PCH$INFJ]<-4
PCH$CLASSEPCH[PCH$PARAMETRE == "transp" & !is.na(PCH$RESULTAT) & PCH$RESULTAT<PCH$INFO]<-5
PCH$CLASSEPCH[PCH$PARAMETRE == "transp" & !is.na(PCH$RESULTAT) & PCH$RESULTAT<0]<-99 

##Cas particulier de l'ilox
PCH$CLASSEPCH[PCH$PARAMETRE %in% c("ilox","bilano2") & PCH$CLASSEPCH >=2 ]<-2
PCH$CLASSEPCH[PCH$PARAMETRE %in% c("ilox","bilano2") & PCH$CLASSEPCH <=3 ]<-3

##on ordine
PCH<-PCH[order(PCH$STATION,PCH$PARAMETRE, PCH$SAISON),]
table(PCH$CLASSEPCH, PCH$PARAMETRE)


###SAUVEGARDE DES PARAMETRES
PCH$COMPARTIMENT<-"PCH"
PCH<-merge(PCH,STATION[,c("STATION","ZMOY")],by="STATION",all.x = TRUE)
COLONNESPARAM<-c("STATION","ZMOY","TPSSEJOUR", "FIABILITSEJOUR", "COMPARTIMENT","PARAMETRE" ,"INFB","INFV","INFJ","INFO")
PARAMPEPCH<-PCH[!duplicated(PCH[,COLONNESPARAM]),COLONNESPARAM]
names(PARAMPEPCH)<-names(PARAMPE)
PARAMPE<-merge(PARAMPE,PARAMPEPCH, all = TRUE)


### RESPECT DE LA FREQ DE PRELEVEMENT 
DATAPCH$Freq<-1
FREQPRELEVPCH<-aggregate(Freq ~ STATION + PARAGROUP +PARAMETRE + SAISON , data = DATAPCH , length)
FREQPRELEVPCH<-FREQPRELEVPCH[!duplicated(FREQPRELEVPCH[,c("STATION","PARAGROUP","SAISON")]),c("STATION","PARAGROUP","SAISON","Freq")]
FREQPRELEVPCH$ID<-paste(FREQPRELEVPCH$STATION, FREQPRELEVPCH$PARAGROUP, FREQPRELEVPCH$SAISON,sep="-")
PCH$ID<-paste(PCH$STATION, PCH$PARAMETRE, PCH$SAISON,sep="-")
PCH<-merge(PCH,FREQPRELEVPCH[,c("ID","Freq")],by="ID")

## Respect de la fréquence minimale de prélèvement
if (FREQOKPCH == "oui") {
	PCH$CLASSEPCH[PCH$Freq<NBANFREQ & !(PCH$PARAMETRE %in% c("ilox","bilano2"))]<-0
}

##Mise forme des résultat en gardant la distinction des saison


	## NUTRIMENT  : NMINERAL, PHOS et PO43
	#Choix de la saison en fonction du tps de résour et de la faibilité du temps de séjour
	### Choix de prendre Azote minéral d'hiver si temps de sejour > 2 mois
	## ou de prendre l'Azote minéral d'été si temps de sejour < 2 mois

COLONNES<-c("PARAMETRE",  "STATION", "SAISON", "ELTQUALITE", "Freq", "RESULTAT","CLASSEPCH")
#Tps de sjour >= 60
NUT_TPSSEJSUP2MOIS<-PCH[PCH$PARAMETRE %in% c("NMINERAL", "phos","po43") & PCH$TPSSEJOUR >= 60 & PCH$FIABILITSEJOUR != "non" & PCH$SAISON == "HIVER",COLONNES]
#Tps de sjour < 60
NUT_TPSSEJINF2MOIS<-PCH[PCH$PARAMETRE %in% c("NMINERAL", "phos","po43") & PCH$TPSSEJOUR < 60 & PCH$FIABILITSEJOUR != "non" & PCH$SAISON == "ETE",COLONNES]
#Tps de sjour non fiable , on prend le parametre le plus déclassant
PCH<-PCH[order(PCH$STATION,PCH$PARAMETRE, -PCH$CLASSEPCH),]
NUT_NONFIABLE<-PCH[PCH$PARAMETRE %in% c("NMINERAL", "phos","po43") & PCH$FIABILITSEJOUR == "non",COLONNES]
NUT_NONFIABLE<-NUT_NONFIABLE[!duplicated( NUT_NONFIABLE[,c("STATION","PARAMETRE" )]),COLONNES]

NUT<-rbind(NUT_TPSSEJSUP2MOIS,NUT_TPSSEJINF2MOIS,NUT_NONFIABLE)
	
	## TRANSPRARENCE
TRANSP<-PCH[PCH$PARAMETRE == "transp" &  PCH$SAISON == "ETE",COLONNES]
	## BILANO2
BILANO2<-PCH[PCH$PARAMETRE %in% c("bilano2","ilox") &  PCH$SAISON == "ETE",COLONNES]
	
RLT_PCH<-rbind(NUT,TRANSP,BILANO2)

#### REGROUPEMENT et mise en forme du tableau final (d'abord par élément de qualité)
PCH_ELTQUALITE<-aggregate(CLASSEPCH ~ STATION + ELTQUALITE, data = RLT_PCH, max)

PCH_ELTQUALITE<-PCH_ELTQUALITE[order(PCH_ELTQUALITE$STATION, PCH_ELTQUALITE$ELTQUALITE),]
names(PCH_ELTQUALITE)[3]<-"ETATELT" #renomme la colonne CLASSEPCH sur laquelle il y a eu le max car correspond à l'état de element qualité


## regroupement pour calculer l'état PCH ss bilan O2 (ilox) (Attention exeption typologique transparence)
PCH_STATION<-data.frame(STATION=as.character(), ETATELT=as.numeric())
PCH_ELTQUALITE<-merge(PCH_ELTQUALITE,STATION[,c("STATION","EXCEPT_TRANS")],by="STATION")
PASEXEPTION<-PCH_ELTQUALITE[PCH_ELTQUALITE$ELTQUALITE != "bilano2" & PCH_ELTQUALITE$EXCEPT_TRANS != "oui",]
if (nrow(PASEXEPTION) > 0){
	PCH_STATION_SS_EXEPT<-aggregate(ETATELT ~ STATION, data =  PASEXEPTION, max)
	PCH_STATION<-rbind(PCH_STATION,PCH_STATION_SS_EXEPT)
}

EXCEPTIONTRANSP<-PCH_ELTQUALITE[PCH_ELTQUALITE$ELTQUALITE != "bilano2" & PCH_ELTQUALITE$EXCEPT_TRANS == "oui"  & PCH_ELTQUALITE$ELTQUALITE != "transpa",]
if (nrow(EXCEPTIONTRANSP) > 0){
	PCH_STATION_AV_EXEPT<-aggregate(ETATELT ~ STATION, data =EXCEPTIONTRANSP  , max)
	PCH_STATION<-rbind(PCH_STATION,PCH_STATION_AV_EXEPT)
}


PCH_STATION<-PCH_STATION[order(PCH_STATION$STATION),]
names(PCH_STATION)[2]<-"ETATPCH_SSBILANO2" #renomme la colonne ETATELT sur laquelle il y a eu le max car correspond à l'état de la station



RLT_PCHFINAL<-PCH_STATION

for (i in  1:nrow(TABLEELT)) {
	TEMPELT<-PCH_ELTQUALITE[PCH_ELTQUALITE$ELTQUALITE==TABLEELT$ELTQUALITE[i],c("STATION", "ETATELT")]
	names(TEMPELT)[2]<-TABLEELT$ELTQUALITE[i]
	RLT_PCHFINAL<-merge(RLT_PCHFINAL,TEMPELT,by="STATION", all.x=TRUE)
	rm(TEMPELT)
}
flush.console()

# on déclasse à 3 si bilanO2 = 3 et etatpch = 1ou 2
RLT_PCHFINAL$ETATPCH[RLT_PCHFINAL$ETATPCH_SSBILANO2 %in% c(1,2) & !is.na(RLT_PCHFINAL$bilano2) & RLT_PCHFINAL$bilano2 ==3]<-3

##Puis par parametre (= paragroup)
PARAMETRES<-unique(PARAMETREPCH$PARAGROUP)
for (i in  PARAMETRES) {
	TEMP<-RLT_PCH[RLT_PCH$PARAMETRE==i,c("STATION","CLASSEPCH")]
	names(TEMP)[2]<-i
	RLT_PCHFINAL<-merge(RLT_PCHFINAL,TEMP,by="STATION", all.x=TRUE)
	rm(TEMP)
}
NAMESRLT<-names(RLT_PCHFINAL)



gc() ## compacte R
flush.console()

