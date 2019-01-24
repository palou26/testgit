##############################################
## SEEE - COURS D'EAU : état physicochimique
## Application de l'arrêté de janvier 2010
##############################################
#SAVE()


#Si  on est pas quantifié , Resultat = LQ/2
DATAPCH<-merge(DATAPCH, LISTECODERQE[,c("REMARQUE","QUANTIFIE")], by="REMARQUE")

if (LQ_NONDISPO =="oui") {
	cond<- DATAPCH$QUANTIFIE=="0"  
	DATAPCH$RESULTAT[cond]<-DATAPCH$RESULTAT[cond]/2
} else {
	cond<- (DATAPCH$QUANTIFIE=="0"  | DATAPCH$RESULTAT <= DATAPCH$LQ) 
	DATAPCH$RESULTAT[cond]<-DATAPCH$LQ[cond]/2
}


##on veut des microgrammes par litre
	####Conversion unité milligramme vers microgrammes (NH4, NO3, PHOS, NO2)
	condunite<-DATAPCH$PARAMETRE %in% c("1335","1340","1350","1339") & DATAPCH$UNITE %in% c("169","173","177","171","133")
	DATAPCH$RESULTAT[ condunite]<- DATAPCH$RESULTAT[condunite ]*1000
	DATAPCH$UNITE[ condunite]<- "162"



### Choix de prendre Azote minéral d'hiver si temps de sejour > 2 mois
## ou de prendre l'Azote minéral d'été si temps de sejour < 2 mois
DATAPCH<-merge(DATAPCH,STATION[,c("STATION","NOMSTATION","EUCD","ZMOY","STRATIFIE","TPSSEJOUR","FIABILITSEJOUR","EXCEPT_NIT")],by="STATION")


##on retire la période hivernale pour le bilan O2 et si pas sstratifié
DATAPCH<-DATAPCH[!( DATAPCH$PARAMETRE == "ilox" & DATAPCH$SAISON == "HIVER") ,]
DATAPCH<-DATAPCH[!( DATAPCH$PARAMETRE == "ilox" & DATAPCH$STRATIFIE != "oui") ,]

##On fait la médiane des résultats pour phosphore et transparence et maximum pour NH4 et NO3
cond_exeptnit<-!(DATAPCH$EXCEPT_NIT == "oui" & DATAPCH$PARAMETRE == "1340") #prise en compte de l'expetion Nitrates
PCH_MAX_NO3NH4<-aggregate(RESULTAT ~ STATION + EUCD + PARAMETRE + TPSSEJOUR+ FIABILITSEJOUR +ZMOY, 
						data = DATAPCH[cond_exeptnit & DATAPCH$PARAMETRE %in%  c("1340","1335"),] , max)
PCH_MED_trans_PHOS_ilox<-aggregate(RESULTAT ~ STATION + EUCD + PARAMETRE + TPSSEJOUR+ FIABILITSEJOUR +ZMOY , 
						data = DATAPCH[ DATAPCH$PARAMETRE %in%  c("1332","1350","ilox"),] , median)
												
PCH<-rbind(PCH_MAX_NO3NH4,PCH_MED_trans_PHOS_ilox)

##CALCUL DES BORNES SEUIL
PCH<-merge(PCH,PARAMETREPCH,by="PARAMETRE")

	# PHOS, AMMONIUM et TRANSP formule : minimum/maximum entre [a*Zmoy^b] et [c*(Zmoy+1)^d]
		#MINIMUM pour PHOS & AMONIUM 
		i<-PCH$PARAMETRE %in% c("1335", "1350")
		PCH$INFB[i]<-pmin(PCH$INFBa[i]*(PCH$ZMOY[i]^PCH$INFBb[i]) ,  PCH$INFBc[i]*((PCH$ZMOY[i]+1)^PCH$INFBd[i]) )
		PCH$INFV[i]<-pmin(PCH$INFVa[i]*(PCH$ZMOY[i]^PCH$INFVb[i]) ,  PCH$INFVc[i]*((PCH$ZMOY[i]+1)^PCH$INFVd[i]) )
		PCH$INFJ[i]<-pmin(PCH$INFJa[i]*(PCH$ZMOY[i]^PCH$INFJb[i]) ,  PCH$INFJc[i]*((PCH$ZMOY[i]+1)^PCH$INFJd[i]) )
		PCH$INFO[i]<-pmin(PCH$INFOa[i]*(PCH$ZMOY[i]^PCH$INFOb[i]) ,  PCH$INFOc[i]*((PCH$ZMOY[i]+1)^PCH$INFOd[i]) )

		#MAXIMUM pour transparence 
		i<-PCH$PARAMETRE %in% c("1332")
		PCH$INFB[i]<-pmax(PCH$INFBa[i]*(PCH$ZMOY[i]^PCH$INFBb[i]) ,  PCH$INFBc[i]*((PCH$ZMOY[i]+1)^PCH$INFBd[i]) )
		PCH$INFV[i]<-pmax(PCH$INFVa[i]*(PCH$ZMOY[i]^PCH$INFVb[i]) ,  PCH$INFVc[i]*((PCH$ZMOY[i]+1)^PCH$INFVd[i]) )
		PCH$INFJ[i]<-pmax(PCH$INFJa[i]*(PCH$ZMOY[i]^PCH$INFJb[i]) ,  PCH$INFJc[i]*((PCH$ZMOY[i]+1)^PCH$INFJd[i]) )
		PCH$INFO[i]<-pmax(PCH$INFOa[i]*(PCH$ZMOY[i]^PCH$INFOb[i]) ,  PCH$INFOc[i]*((PCH$ZMOY[i]+1)^PCH$INFOd[i]) )
		
	#NITRATES (en fonction de la profondeur moyenne est > ou < à 15m)
	i<-PCH$PARAMETRE %in% c("1340") & PCH$ZMOY <= 15
	PCH$INFB[i]<-PCH$INFBa[i]
	PCH$INFV[i]<-PCH$INFVa[i]
	PCH$INFJ[i]<-PCH$INFJa[i]
	PCH$INFO[i]<-PCH$INFOa[i]

	i<-PCH$PARAMETRE %in% c("1340") & PCH$ZMOY > 15
	PCH$INFB[i]<-PCH$INFBb[i]
	PCH$INFV[i]<-PCH$INFVb[i]
	PCH$INFJ[i]<-PCH$INFJb[i]
	PCH$INFO[i]<-PCH$INFOb[i]

	
	##Pour les nitrates, si le temps de résidence est inférieur à 30 jours, il faut prendre les seuils des cours d'eau
	i<-PCH$PARAMETRE %in% c("1340") & PCH$TPSSEJOUR < 30 
	j<-PARAMETREPCHCE$PARAMETRE %in% c("1340")
	PCH$INFB[i]<-PARAMETREPCHCE$INFB[j] * 1000
	PCH$INFV[i]<-PARAMETREPCHCE$INFV[j]* 1000
	PCH$INFJ[i]<-PARAMETREPCHCE$INFJ[j]* 1000
	PCH$INFO[i]<-PARAMETREPCHCE$INFO[j]* 1000
	
	#BILAN O2
	i<-PCH$PARAMETRE %in% c("ilox") 
	PCH$INFV[i]<-PCH$INFVa[i]
	
	
	
	
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
PCH$CLASSEPCH[PCH$PARAMETRE  != "1332" & !is.na(PCH$RESULTAT) & PCH$RESULTAT<=PCH$INFB]<-1
PCH$CLASSEPCH[PCH$PARAMETRE  !="1332" & !is.na(PCH$RESULTAT) & PCH$RESULTAT>PCH$INFB & PCH$RESULTAT<=PCH$INFV]<-2
PCH$CLASSEPCH[PCH$PARAMETRE  !="1332" & !is.na(PCH$RESULTAT) & PCH$RESULTAT>PCH$INFV & PCH$RESULTAT<=PCH$INFJ]<-3
PCH$CLASSEPCH[PCH$PARAMETRE  !="1332" & !is.na(PCH$RESULTAT) & PCH$RESULTAT>PCH$INFJ & PCH$RESULTAT<=PCH$INFO]<-4
PCH$CLASSEPCH[PCH$PARAMETRE  !="1332"  & !is.na(PCH$RESULTAT) & PCH$RESULTAT>PCH$INFO]<-5
PCH$CLASSEPCH[PCH$PARAMETRE != "1332" & !is.na(PCH$RESULTAT) & PCH$RESULTAT<0]<-99 


## PARAMETRE ordre décroissant : Valeur seuil de l'état PCH
# Attention les valeurs seuils dans la table PARAMETREPCH (colonne INFR) a été attribuées pour le calcul de l'indice et non la mise en classe
PCH$CLASSEPCH[PCH$PARAMETRE  == "1332" & !is.na(PCH$RESULTAT) & PCH$RESULTAT>=PCH$INFB]<-1
PCH$CLASSEPCH[PCH$PARAMETRE == "1332" & !is.na(PCH$RESULTAT) & PCH$RESULTAT>=PCH$INFV & PCH$RESULTAT<PCH$INFB]<-2
PCH$CLASSEPCH[PCH$PARAMETRE == "1332" & !is.na(PCH$RESULTAT) & PCH$RESULTAT>=PCH$INFJ & PCH$RESULTAT<PCH$INFV]<-3
PCH$CLASSEPCH[PCH$PARAMETRE == "1332" & !is.na(PCH$RESULTAT) & PCH$RESULTAT>=PCH$INFO & PCH$RESULTAT<PCH$INFJ]<-4
PCH$CLASSEPCH[PCH$PARAMETRE == "1332" & !is.na(PCH$RESULTAT) & PCH$RESULTAT<PCH$INFO]<-5
PCH$CLASSEPCH[PCH$PARAMETRE == "1332" & !is.na(PCH$RESULTAT) & PCH$RESULTAT<0]<-99 


####PARAMETRE ilox (bilan O2)
PCH$CLASSEPCH[PCH$PARAMETRE == "ilox" & !is.na(PCH$RESULTAT) & PCH$RESULTAT<=PCH$INFV]<-2
PCH$CLASSEPCH[PCH$PARAMETRE == "ilox" & !is.na(PCH$RESULTAT) & PCH$RESULTAT>PCH$INFV]<-3


PCH<-PCH[order(PCH$STATION,PCH$PARAMETRE),]
table(PCH$CLASSEPCH, PCH$PARAMETRE)



###SAUVEGARDE DES PARAMETRES
PCH$COMPARTIMENT<-"PCH"
COLONNESPARAM<-c("STATION","ZMOY","TPSSEJOUR", "FIABILITSEJOUR","COMPARTIMENT","PARAMETRE" ,"INFB","INFV","INFJ","INFO")
PARAMPEPCH<-PCH[!duplicated(PCH[,COLONNESPARAM]),COLONNESPARAM]
names(PARAMPEPCH)<-names(PARAMPE)
PARAMPE<-merge(PARAMPE,PARAMPEPCH, all = TRUE)


###RESPECT DE LA FREQ DE PRELEVEMENT 
DATAPCH$Freq<-1
FREQPRELEVPCH<-aggregate(Freq ~ STATION + PARAMETRE  , data = DATAPCH , length)
FREQPRELEVPCH<-FREQPRELEVPCH[!duplicated(FREQPRELEVPCH[,c("STATION","PARAMETRE")]),c("STATION","PARAMETRE","Freq")]
FREQPRELEVPCH$ID<-paste(FREQPRELEVPCH$STATION, FREQPRELEVPCH$PARAMETRE,sep="-")
PCH$ID<-paste(PCH$STATION, PCH$PARAMETRE,sep="-")
PCH<-merge(PCH,FREQPRELEVPCH[,c("ID","Freq")],by="ID")

## Respect de la fréquence minimale de prélèvement
if (FREQOKPCH == "oui") {
	PCH$CLASSEPCH[PCH$Freq<NBANFREQ & !(PCH$PARAMETRE %in% c("ilox","bilano2"))]<-0
}
table(PCH$CLASSEPCH, PCH$PARAMETRE)

##Mise forme des résultat en gardant la distinction des saison
	
RLT_PCH<-PCH

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
RLT_PCHFINAL$ETATPCH[RLT_PCHFINAL$ETATPCH_SSBILANO2 %in% c(1,2) & !is.na(RLT_PCHFINAL$bilano2) & RLT_PCHFINAL$bilano2 >=3]<-3

##Puis par parametre (= paragroup)
PARAMETRES<-unique(PARAMETREPCH$PARAMETRE)
for (i in  PARAMETRES) {
	TEMP<-RLT_PCH[RLT_PCH$PARAMETRE==i,c("STATION","CLASSEPCH")]
	names(TEMP)[2]<-PARAMETREPCH$NOM[PARAMETREPCH$PARAMETRE == i][1]
	RLT_PCHFINAL<-merge(RLT_PCHFINAL,TEMP,by="STATION", all.x=TRUE)
	rm(TEMP)
}
NAMESRLT<-names(RLT_PCHFINAL)



gc() ## compacte R
flush.console()

