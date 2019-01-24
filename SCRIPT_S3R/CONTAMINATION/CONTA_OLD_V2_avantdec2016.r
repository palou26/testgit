##############################################
## SEEE - COURS D'EAU : �tat FAMILLEluants sp�cifiques
## Application de l'arr�t� de janvier 2016
##############################################

##############################
## Mise en forme des donn�es
##############################
gc()

###CHOIX DES SEUILS : ON PREPARE LES SEUIL SELON LE TYPE DE CAUL CHOISI: AIGUE, CHRONIQUE, IMPREGNATION

# AIGUE : On prend dans l'ordre NQECMA puis  VGECMA 
if (TYPECONTA == 1) {

	PARAMETRECONTA$VALSEUIL1<-PARAMETRECONTA$NQECMA * CONTASEUIL1
	PARAMETRECONTA$VALSEUIL2<-PARAMETRECONTA$NQECMA * CONTASEUIL2
	PARAMETRECONTA$VALSEUIL3<-PARAMETRECONTA$NQECMA * CONTASEUIL3
	PARAMETRECONTA$SEUIL<-"NQECMA"
	PARAMETRECONTA$NORME<-PARAMETRECONTA$NQECMA

	cond<-is.na(PARAMETRECONTA$VALSEUIL1)
	PARAMETRECONTA$VALSEUIL1[cond]<-PARAMETRECONTA$VGECMA[cond] * CONTASEUIL1
	PARAMETRECONTA$VALSEUIL2[cond]<-PARAMETRECONTA$VGECMA[cond] * CONTASEUIL2
	PARAMETRECONTA$VALSEUIL3[cond]<-PARAMETRECONTA$VGECMA[cond] * CONTASEUIL3
	PARAMETRECONTA$SEUIL[cond]<-"VGECMA"
	PARAMETRECONTA$NORME[cond]<-PARAMETRECONTA$VGECMA[cond]

	cond<-is.na(PARAMETRECONTA$VALSEUIL1)
	PARAMETRECONTA$SEUIL[cond]<-"abs"
	PARAMETRECONTA$NORME[cond]<-NA
	PARAMETRECONTA$VALSEUIL4<-NA
}

# CHRONIQUE : On prend dans l'ordre NQEMA puis  VGEMA puis PNEC
if (TYPECONTA == 2) {

	PARAMETRECONTA$VALSEUIL1<-PARAMETRECONTA$NQEMA * CONTASEUIL1
	PARAMETRECONTA$VALSEUIL2<-PARAMETRECONTA$NQEMA * CONTASEUIL2
	PARAMETRECONTA$VALSEUIL3<-PARAMETRECONTA$NQEMA * CONTASEUIL3
	PARAMETRECONTA$SEUIL<-"NQEMA"
	PARAMETRECONTA$NORME<-PARAMETRECONTA$NQEMA

	cond<-is.na(PARAMETRECONTA$VALSEUIL1)
	PARAMETRECONTA$VALSEUIL1[cond]<-PARAMETRECONTA$VGEMA[cond] * CONTASEUIL1
	PARAMETRECONTA$VALSEUIL2[cond]<-PARAMETRECONTA$VGEMA[cond] * CONTASEUIL2
	PARAMETRECONTA$VALSEUIL3[cond]<-PARAMETRECONTA$VGEMA[cond] * CONTASEUIL3
	PARAMETRECONTA$SEUIL[cond]<-"VGEMA"
	PARAMETRECONTA$NORME[cond]<-PARAMETRECONTA$VGEMA[cond]


	cond<-is.na(PARAMETRECONTA$VALSEUIL2)
	PARAMETRECONTA$VALSEUIL1[cond]<-PARAMETRECONTA$PNEC[cond] * CONTASEUIL1
	PARAMETRECONTA$VALSEUIL2[cond]<-PARAMETRECONTA$PNEC[cond] * CONTASEUIL2
	PARAMETRECONTA$VALSEUIL3[cond]<-PARAMETRECONTA$PNEC[cond] * CONTASEUIL3
	PARAMETRECONTA$SEUIL[cond]<-"PNEC"
	PARAMETRECONTA$NORME[cond]<-PARAMETRECONTA$PNEC[cond]

	cond<-is.na(PARAMETRECONTA$VALSEUIL1)
	PARAMETRECONTA$SEUIL[cond]<-"abs"
	#seuil 4 fictif
	PARAMETRECONTA$VALSEUIL4<-NA
	PARAMETRECONTA$NORME[cond]<-NA
}

#Impr�gantion (on comparera soit � la LQ
if (TYPECONTA == 3 & CHOIXSEUIL == "LIBRE") {

	PARAMETRECONTA$VALSEUIL1<-PARAMETRECONTA$SEUIL_LIBRE1
	PARAMETRECONTA$VALSEUIL2<-PARAMETRECONTA$SEUIL_LIBRE2
	PARAMETRECONTA$VALSEUIL3<-PARAMETRECONTA$SEUIL_LIBRE3
	if (NBSEUIL == 4) {
		PARAMETRECONTA$VALSEUIL4<-PARAMETRECONTA$SEUIL_LIBRE4
	} else {
		PARAMETRECONTA$VALSEUIL4<-NA

	}
	PARAMETRECONTA$SEUIL<-"LIBRE"
	PARAMETRECONTA$NORME<-PARAMETRECONTA$VALSEUIL3

}
#Impr�gantion (on comparera � des seuils d�fini
if (TYPECONTA == 3 & CHOIXSEUIL == "LQMAX") {

PARAMETRECONTA$VALSEUIL1<-CONTASEUIL1
PARAMETRECONTA$VALSEUIL2<-CONTASEUIL2
PARAMETRECONTA$VALSEUIL3<-CONTASEUIL3
PARAMETRECONTA$SEUIL<-"LQMAX"
PARAMETRECONTA$NORME<-NA

}

##########################################
## Calcul du module de contamination
##########################################
# requete pr�paratoire
CONTAM<-DATACONTA[DATACONTA$STATION %in% STATION$STATION,]
CONTAM$ID<-paste(CONTAM$STATION, CONTAM$PARAMETRE, sep="_")
gc()
CONTAM<-merge(CONTAM,LISTECODERQE[,c("REMARQUE","QUANTIFIE")],by="REMARQUE")
gc()

# On divise par 2 si Resultat <= LQ ou si pas qualtifi�
CONTAM$RESULTAT2<-CONTAM$RESULTAT
#Si  on est pas quantifi� 
if (LQ_NONDISPO =="oui") {
	cond<- CONTAM$QUANTIFIE=="0" 
	CONTAM$RESULTAT2[cond]<-CONTAM$RESULTAT[cond]/2
} else {
	cond<- CONTAM$QUANTIFIE=="0" | CONTAM$RESULTAT <= CONTAM$LQ
	CONTAM$RESULTAT2[cond]<-CONTAM$LQ[cond]/2
}
gc()

CONTAM$QUANTIF<-as.numeric(CONTAM$QUANTIFIE)
##On souhaite savoir s'il ya au moins une valeur quantifi�e
NBQUANTI<-aggregate(QUANTIF~ STATION + PARAMETRE, data = CONTAM , max)
NBQUANTI$ID<-paste(NBQUANTI$STATION, NBQUANTI$PARAMETRE, sep="_")

#######ON FAIT LA MOYENNE OU LE MAX DES CONCENTRATIONS SELON le Mode de calcul (Aggr�gation = CONTAAGG)
if (TYPECONTA == 1) {
		CONTAAGG<-aggregate(RESULTAT2~ STATION + PARAMETRE, data = CONTAM , max)
}
if (TYPECONTA == 2) {
		CONTAAGG<-aggregate(RESULTAT2~ STATION + PARAMETRE, data = CONTAM , mean)
}

if (TYPECONTA ==3 & CALCCONTA == "MAX") {
		CONTAAGG<-aggregate(RESULTAT2~ STATION + PARAMETRE, data = CONTAM , max)
}

if (TYPECONTA ==3 & CALCCONTA == "MOY") {
		CONTAAGG<-aggregate(RESULTAT2~ STATION + PARAMETRE, data = CONTAM , mean)
}
gc()

# Extraction de la LQMAX 
if (LQ_NONDISPO =="oui") { # recherche LQmax dans colonne resultat si colonne LQ non dispo
	LQMAX<-CONTAM[CONTAM$QUANTIFIE=="0", c("STATION","PARAMETRE","QUANTIFIE","RESULTAT")]
	LQMAX<-aggregate(RESULTAT~ STATION + PARAMETRE, data = LQMAX , max)
	names(LQMAX)[3]<-"LQMAX"
	LQMAX$LQMAX<-round(LQMAX$LQMAX,5)
	LQMAX$ID<-paste(LQMAX$STATION, LQMAX$PARAMETRE, sep="_")
} else { # recherche LQmax dans colonne LQ
	#LQMAX<-CONTAM[CONTAM$QUANTIFIE=="0", c("STATION","PARAMETRE","QUANTIFIE","RESULTAT")]
	LQMAX<-aggregate(LQ ~ STATION + PARAMETRE, data = CONTAM , max)
	names(LQMAX)[3]<-"LQMAX"
	LQMAX$ID<-paste(LQMAX$STATION, LQMAX$PARAMETRE, sep="_")
}

CONTAAGG$ID<-paste(CONTAAGG$STATION, CONTAAGG$PARAMETRE, sep="_")
gc()
CONTAAGG<-merge(CONTAAGG,LQMAX[,c("ID","LQMAX")],by="ID", all.x=TRUE)
CONTAAGG<-merge(CONTAAGG,NBQUANTI[,c("ID","QUANTIF")],by="ID", all.x=TRUE)
gc()


#on rassemble les info de PARAMETRES
if(NBSEUIL == 3) {
CONTAAGG<-merge(CONTAAGG,PARAMETRECONTA[,c("PARAMETRE","NOMCOURT","FAMILLE","VALSEUIL1","VALSEUIL2","VALSEUIL3","SEUIL","NORME")],by="PARAMETRE")
} else {
CONTAAGG<-merge(CONTAAGG,PARAMETRECONTA[,c("PARAMETRE","NOMCOURT","FAMILLE","VALSEUIL1","VALSEUIL2","VALSEUIL3","VALSEUIL4","SEUIL","NORME")],by="PARAMETRE")


}


gc()

###Si Seuil en fonction de LQMAX
if (TYPECONTA == 3 & CHOIXSEUIL == "LQMAX") {

	CONTAAGG$VALSEUIL1<-CONTAAGG$VALSEUIL1 * CONTAAGG$LQMAX
	CONTAAGG$VALSEUIL2<-CONTAAGG$VALSEUIL2 * CONTAAGG$LQMAX
	CONTAAGG$VALSEUIL3<-CONTAAGG$VALSEUIL3 * CONTAAGG$LQMAX
	CONTAAGG$SEUIL<-"LQMAX"
	CONTAAGG$NORME<-CONTAAGG$LQMAX

}


# Calcul de la frequence de pr�l�vement
CONTA_FREQ<-aggregate(RESULTAT2 ~ STATION + PARAMETRE, data = CONTAM , length)
names(CONTA_FREQ)[3]<-"FREQ"
CONTA_FREQ$ID<-as.character(paste0(CONTA_FREQ$STATION,"_",CONTA_FREQ$PARAMETRE))
CONTA_FREQ<-CONTA_FREQ[,c("ID","FREQ")]

CONTAAGG<-merge(CONTAAGG,CONTA_FREQ,by="ID")
CONTAAGG<-CONTAAGG[order(CONTAAGG$STATION, CONTAAGG$PARAMETRE),]
rm(CONTA_FREQ)

#initialisation de CALSSECONTA et CASCONTA
CONTAAGG$CLASSECONTA<-as.numeric(NA)
CONTAAGG$CAS<-as.character(NA)



#Mise en Classe de qualit�
condcrit1<-CONTAAGG$LQMAX <= CONTAAGG$NORME & !is.na(CONTAAGG$NORME)
	CONTAAGG$CLASSECONTA[condcrit1 & CONTAAGG$RESULTAT2<=CONTAAGG$VALSEUIL1]<-1
	CONTAAGG$CAS[condcrit1 & CONTAAGG$RESULTAT2<=CONTAAGG$VALSEUIL1]<-"1a"
	CONTAAGG$CLASSECONTA[condcrit1 & CONTAAGG$RESULTAT2>CONTAAGG$VALSEUIL1 & CONTAAGG$RESULTAT2<=CONTAAGG$VALSEUIL2]<-2  
	CONTAAGG$CAS[condcrit1 & CONTAAGG$RESULTAT2>CONTAAGG$VALSEUIL1 & CONTAAGG$RESULTAT2<=CONTAAGG$VALSEUIL2]<-"2a"
	CONTAAGG$CLASSECONTA[condcrit1 & CONTAAGG$RESULTAT2>CONTAAGG$VALSEUIL2 & CONTAAGG$RESULTAT2<=CONTAAGG$VALSEUIL3]<-3  
	CONTAAGG$CAS[condcrit1 & CONTAAGG$RESULTAT2>CONTAAGG$VALSEUIL2 & CONTAAGG$RESULTAT2<=CONTAAGG$VALSEUIL3]<-"3a"
	CONTAAGG$CLASSECONTA[condcrit1 & CONTAAGG$RESULTAT2>CONTAAGG$VALSEUIL3 ]<-4 
	CONTAAGG$CAS[condcrit1 & CONTAAGG$RESULTAT2>CONTAAGG$VALSEUIL3 ]<-"4a"
	CONTAAGG$CLASSECONTA[condcrit1 & CONTAAGG$LQMAX>CONTAAGG$VALSEUIL3 ]<- -1 
	if (NBSEUIL == 4) {
	CONTAAGG$CLASSECONTA[condcrit1 & CONTAAGG$RESULTAT2>CONTAAGG$VALSEUIL4 ]<-5
	CONTAAGG$CAS[condcrit1 & CONTAAGG$RESULTAT2>CONTAAGG$VALSEUIL4 ]<-"5a"
	CONTAAGG$CLASSECONTA[condcrit1 & CONTAAGG$LQMAX>CONTAAGG$VALSEUIL4 ]<- -1	
	}
	
condcrit2<-!condcrit1 & CONTAAGG$QUANTIF==1 & !is.na(CONTAAGG$NORME)
	CONTAAGG$CLASSECONTA[condcrit2 & CONTAAGG$RESULTAT2<=CONTAAGG$VALSEUIL1]<-1
	CONTAAGG$CAS[condcrit2 & CONTAAGG$RESULTAT2<=CONTAAGG$VALSEUIL1]<-"1b"
	CONTAAGG$CLASSECONTA[condcrit2 & CONTAAGG$RESULTAT2>CONTAAGG$VALSEUIL1 & CONTAAGG$RESULTAT2<=CONTAAGG$VALSEUIL2]<-2  
	CONTAAGG$CAS[condcrit2 & CONTAAGG$RESULTAT2>CONTAAGG$VALSEUIL1 & CONTAAGG$RESULTAT2<=CONTAAGG$VALSEUIL2]<-"2b"
	CONTAAGG$CLASSECONTA[condcrit2 & CONTAAGG$RESULTAT2>CONTAAGG$VALSEUIL2 & CONTAAGG$RESULTAT2<=CONTAAGG$VALSEUIL3]<-3  
	CONTAAGG$CAS[condcrit2 & CONTAAGG$RESULTAT2>CONTAAGG$VALSEUIL2 & CONTAAGG$RESULTAT2<=CONTAAGG$VALSEUIL3]<-"3b"
	CONTAAGG$CLASSECONTA[condcrit2 & CONTAAGG$RESULTAT2>CONTAAGG$VALSEUIL3 ]<-4 
	CONTAAGG$CAS[condcrit2 & CONTAAGG$RESULTAT2>CONTAAGG$VALSEUIL3 ]<-"4b"
	CONTAAGG$CLASSECONTA[condcrit2 & CONTAAGG$LQMAX>CONTAAGG$VALSEUIL3 ]<- 4    ### ou ind�termin� (-1) ? 
	if (NBSEUIL == 4) {
	CONTAAGG$CLASSECONTA[condcrit2 & CONTAAGG$RESULTAT2>CONTAAGG$VALSEUIL4 ]<-5
	CONTAAGG$CAS[condcrit2 & CONTAAGG$RESULTAT2>CONTAAGG$VALSEUIL4 ]<-"5b"
	CONTAAGG$CLASSECONTA[condcrit2 & CONTAAGG$LQMAX>CONTAAGG$VALSEUIL4 ]<- 5	### ou ind�termin� (-1) ? 
	}
	#ind�termin�
CONTAAGG$CLASSECONTA[!condcrit1 & CONTAAGG$QUANTIF==0]<--1
	CONTAAGG$CAS[!condcrit1 & CONTAAGG$QUANTIF==0]<-"Ind"
	
## Respect de la fr�quence minimale de pr�l�vement
if (FREQOKCONTA == "oui") {
	CONTAAGG$CLASSECONTA[CONTAAGG$FREQ<NBANFREQ]<-0
	CONTAAGG$CAS[CONTAAGG$FREQ<NBANFREQ]<-"Freqbasse"
}

#verif
table(CONTAAGG$CLASSECONTA)
table(CONTAAGG$CAS)
CONTACASCLASS<-data.frame(table(CONTAAGG$CLASSECONTA, CONTAAGG$CAS))
names(CONTACASCLASS)<-c("ClasseQualit�","Cas","effectif")
CONTACASCLASS<-CONTACASCLASS[CONTACASCLASS$effectif > 0,]
CONTACASCLASS<-CONTACASCLASS[order(CONTACASCLASS$ClasseQualit�),]

# Calcul de l'�tat par type de FAMILLEluant
CONTA_FAMILLE<-aggregate(CLASSECONTA ~ STATION + FAMILLE, data = CONTAAGG, max)
names(CONTA_FAMILLE)[3]<-"ETATFAMILLE" #renomme la colonne CLASSECONTA sur laquelle il y a eu le max car correspond � l'�tat par type de FAMILLEluants
CONTA_FAMILLE<-CONTA_FAMILLE[order(CONTA_FAMILLE$STATION, CONTA_FAMILLE$FAMILLE),]
CONTA_FAMILLE$FAMILLE<-as.character(CONTA_FAMILLE$FAMILLE)
# Calcul de l'�tat global des FAMILLE

RLT_CONTA<-aggregate(ETATFAMILLE ~ STATION, data = CONTA_FAMILLE, max) # R�sultat final des FAMILLEluants sp�cifiques
names(RLT_CONTA)[2]<-"CONTAMINATION" #renomme la colonne ETATFAMILLE sur laquelle il y a eu le max car correspond � l'�tat global des FAMILLEluants sp�cifiques
RLT_LQMAX<-RLT_CONTA
RLT_CAS<-RLT_CONTA
TABLEFAMILLE<-data.frame(FAMILLE = as.character(unique(toupper(as.character(CONTA_FAMILLE$FAMILLE)))))
TABLEFAMILLE$FAMILLE<-as.character(TABLEFAMILLE$FAMILLE)



##############################
## Mise en forme tableau final
##############################
# Ajout colonnes des FAMILLE
for (i in  1:nrow(TABLEFAMILLE)  ) {
	TEMPP<-CONTA_FAMILLE[toupper(CONTA_FAMILLE$FAMILLE)==TABLEFAMILLE$FAMILLE[i],c("STATION", "ETATFAMILLE")]
	names(TEMPP)[2]<-TABLEFAMILLE$FAMILLE[i]
	RLT_CONTA<-merge(RLT_CONTA,TEMPP,by="STATION", all.x=TRUE)
	rm(TEMPP)
}

# Ajout colonnes des param�tres 
PARAMETRECONTA_ETUDIE<-PARAMETRECONTA[PARAMETRECONTA$PARAMETRE %in% CONTAAGG$PARAMETRE,]
for (i in  1:nrow(PARAMETRECONTA_ETUDIE)  ) {
	TEMPP<-CONTAAGG[CONTAAGG$PARAMETRE==PARAMETRECONTA_ETUDIE$PARAMETRE[i],c("STATION", "CLASSECONTA")]
	names(TEMPP)[2]<-PARAMETRECONTA_ETUDIE$NOMCOURT[i]
	RLT_CONTA<-merge(RLT_CONTA,TEMPP,by="STATION", all.x=TRUE)
	rm(TEMPP)
}

##Export de la LQMAX
PARAMETRECONTA_ETUDIE<-PARAMETRECONTA[PARAMETRECONTA$PARAMETRE %in% CONTAAGG$PARAMETRE,]
for (i in  1:nrow(PARAMETRECONTA_ETUDIE)  ) {
	TEMPP<-CONTAAGG[CONTAAGG$PARAMETRE==PARAMETRECONTA_ETUDIE$PARAMETRE[i],c("STATION", "LQMAX")]
	names(TEMPP)[2]<-PARAMETRECONTA_ETUDIE$NOMCOURT[i]
	RLT_LQMAX<-merge(RLT_LQMAX,TEMPP,by="STATION", all.x=TRUE)
	rm(TEMPP)
}

##Export du CAS
PARAMETRECONTA_ETUDIE<-PARAMETRECONTA[PARAMETRECONTA$PARAMETRE %in% CONTAAGG$PARAMETRE,]
for (i in  1:nrow(PARAMETRECONTA_ETUDIE)  ) {
	TEMPP<-CONTAAGG[CONTAAGG$PARAMETRE==PARAMETRECONTA_ETUDIE$PARAMETRE[i],c("STATION", "CAS")]
	names(TEMPP)[2]<-PARAMETRECONTA_ETUDIE$NOMCOURT[i]
	RLT_CAS<-merge(RLT_CAS,TEMPP,by="STATION", all.x=TRUE)
	rm(TEMPP)
}

##Export des parametres limitants

	### Fonction pour sortir les parametres limitant 
	FUNC_LIMITANT<-function(RLT_FINAL, ETAT, LIST_PARAM) {
		COLETAT<-RLT_FINAL[,ETAT]
		MAT<-as.matrix(RLT_FINAL[,LIST_PARAM])
		colnames(MAT)<-LIST_PARAM
		C1<-MAT >= COLETAT & COLETAT > 1
		INDICE<-which( C1  ,  arr.ind=TRUE)  
		MAT2<-matrix(data="",nrow = nrow(MAT), ncol=ncol(MAT))
		MAT2[INDICE] <- colnames(MAT)[INDICE[,2]]
		DECLASS<-apply(MAT2, 1, function(x) paste( x,collapse = ";"))
		A<- sum(nchar(DECLASS))+1
		
		while (A - sum(nchar(DECLASS))   > 0 ) { #boucle pour supprimer les ";" inutiles
			A<-sum(nchar(DECLASS))
			DECLASS<-gsub(";;",";",DECLASS)
			cond1<-substr(DECLASS,1,1) == ";"
			DECLASS[cond1]<-substr(DECLASS[cond1],2,nchar(DECLASS[cond1]))
			cond2<-substr(DECLASS,nchar(DECLASS),nchar(DECLASS) ) == ";"
			DECLASS[cond2]<-substr(DECLASS[cond2],1,nchar(DECLASS[cond2])-1)
			}
		return(DECLASS)
	}	

names_RLT_CONTA<-names(RLT_CONTA)	
NAME_ETAT<-"CONTAMINATION"
RLT_CONTA$PARAMContaminant<-FUNC_LIMITANT(RLT_CONTA,NAME_ETAT, PARAMETRECONTA_ETUDIE$NOMCOURT)


#nb de declass
temp<- unlist(lapply(gregexpr(";",RLT_CONTA$PARAMContaminant,TRUE),length))
temp2<-unlist(gregexpr(";",RLT_CONTA$PARAMContaminant[temp == 1],TRUE)) == -1
RLT_CONTA$N_PARAMContaminant<-temp+1
RLT_CONTA$N_PARAMContaminant[temp == 1][temp2]<- 1
RLT_CONTA$N_PARAMContaminant[RLT_CONTA$PARAMContaminant == "" ]<-0

RLT_CONTA_PARAMLIMITANT<-RLT_CONTA[,c("STATION","CONTAMINATION","N_PARAMContaminant","PARAMContaminant")]
RLT_CONTA<-RLT_CONTA[,names_RLT_CONTA]
gc() ## compacte R


if (CONTAME == "oui"){


	# Classe de qualit� � la masse d'eau
		TEMPECONTAME<-merge(CONTAAGG[,c("STATION", "PARAMETRE","CLASSECONTA")], STATION[,c("STATION", "EUCD", "REPRESENTATIVE")], by="STATION")
		TEMPECONTAME_OUI<-TEMPECONTAME[TEMPECONTAME$REPRESENTATIVE %in% c("oui"),]
		TEMPECONTAME_TEMPO<-TEMPECONTAME[TEMPECONTAME$REPRESENTATIVE %in% c("temporaire"),]
		
		# Calcul de l'�tat ME pour les stations repr�sentatives
		TEMPECONTAME_CAS1<-aggregate(CLASSECONTA ~ EUCD + REPRESENTATIVE + STATION, data = TEMPECONTAME_OUI, max)
		
		# Calcul de l'�tat ME pour les stations temporaires s'il n'y a pas de station repr�sentative
		if (nrow(TEMPECONTAME_TEMPO)>0) {
			TEMPECONTAME_CAS2<-aggregate(CLASSECONTA ~ EUCD + REPRESENTATIVE+ STATION, data = TEMPECONTAME_TEMPO, max)
			CONTAAGGME<-rbind(TEMPECONTAME_CAS1,TEMPECONTAME_CAS2[!(TEMPECONTAME_CAS2$EUCD %in% c(TEMPECONTAME_CAS1$EUCD)),])
			rm(TEMPECONTAME_CAS2)
		} else {
			CONTAAGGME<-TEMPECONTAME_CAS1
		}
	
		names(CONTAAGGME)[4]<-"CONTAMINATIONME"
		
		
		

	
	# Rappatriment des donn�es de la Station repr�sentative et si plusieurs station, on garde la plus d�classante
	RLT_CONTAAGGME<-CONTAAGGME[,c("EUCD","CONTAMINATIONME","STATION")]
	RLT_CONTAAGGME<-merge(RLT_CONTAAGGME,RLT_CONTA, by="STATION",all.x = TRUE)
	RLT_CONTAAGGME<-RLT_CONTAAGGME[order(RLT_CONTAAGGME$EUCD, -RLT_CONTAAGGME$CONTAMINATIONME) ,]
	RLT_CONTAAGGME<-RLT_CONTAAGGME[!duplicated(RLT_CONTAAGGME[,"EUCD"]),]
	RLT_CONTAAGGME<-RLT_CONTAAGGME[,c("EUCD", "CONTAMINATIONME", "CONTAMINATION", names(RLT_CONTA)[3:ncol(RLT_CONTA)])]
	gc()
	
	
}
