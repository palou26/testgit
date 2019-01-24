 ##############################################
## SEEE - COURS D'EAU : état physicochimique
## Application de l'arrêté de janvier 2010
##############################################

###################
## PCH : FREQUENCE
###################
#Plan d'eau
if (SEEEPCH=="oui" & CEPE == "PE") {
## PCH : FREQUENCE PRELEVEMENT		
	RLT_PCHFREQ<-data.frame(STATION = sort(unique(RLT_PCH$STATION)))

	for (i in  1:nrow(PARAMETREPCH)) {
		TEMPP<-RLT_PCH[RLT_PCH$PARAMETRE==PARAMETREPCH$PARAMETRE[i],c("STATION","Freq")]
		names(TEMPP)[2]<-paste("f",toupper(PARAMETREPCH$NOM[i]), sep="")
		RLT_PCHFREQ<-merge(RLT_PCHFREQ,TEMPP,by="STATION", all.x=TRUE)
		rm(TEMPP)
		gc()
	}	
TEMPDECLAS<-data.frame(COLONNE=as.character())

}

#Cours d'eau
if (SEEEPCH=="oui" & CEPE == "CE") {
## PCH : FREQUENCE PRELEVEMENT
	RLT_PCHFREQ<-data.frame(RLT_PCHFINAL[,c("STATION")])
	names(RLT_PCHFREQ)[1]<-"STATION"
		for (i in  1:nrow(PARAMETREPCH)) {
			TEMPP<-RLT_PCH[RLT_PCH$PARAMETRE==PARAMETREPCH$PARAMETRE[i],c("STATION","Freq")]
			names(TEMPP)[2]<-paste("f",toupper(PARAMETREPCH$NOM[i]), sep="")
			RLT_PCHFREQ<-merge(RLT_PCHFREQ,TEMPP,by="STATION", all.x=TRUE)
			rm(TEMPP)
		}

## PCH : FREQUENCE DE DECLASSEMENT (hors PCH assouplie)
	RLT_PCHFREQDECLAS<-RLT_PCHFINALEXCEPT[,c("STATION", "ETATPCH")]
	RLT_PCHFREQDECLAS$IDDECLAS<-paste(RLT_PCHFREQDECLAS$STATION,"_",RLT_PCHFREQDECLAS$ETATPCH,sep="")

	# selection des données <=R90 pour déterminer le nb de prélèvements déclassants (Fréquence de déclassement se calcule au sein des valeurs <=R90)
	PCHASCFREQ<-PCHASC[,c("STATION", "PARAMETRE", "RESULTAT", "CLASSEPCH", "RANG", "R90")]
	PCHDESCFREQ<-PCHDESC[,c("STATION", "PARAMETRE", "RESULTAT", "CLASSEPCH", "RANG", "R90")]
	PCHFREQ<-rbind(PCHASCFREQ,PCHDESCFREQ)
	rm(PCHASCFREQ,PCHDESCFREQ)
	PCHFREQ<-PCHFREQ[PCHFREQ$RANG<=PCHFREQ$R90,]
	
	# selection des prelevements déclassants (déclassant = même état que la station) par parametre dans la table initiale
	#TEMPDECLAS<-PCH[,c("STATION", "PARAMETRE", "CLASSEPCH")]
	#TEMPDECLAS<-PCHFREQ
	#TEMPDECLAS$IDDECLAS<-paste(TEMPDECLAS$STATION,"_",TEMPDECLAS$CLASSEPCH,sep="")
	#TEMPDECLAS<-TEMPDECLAS[TEMPDECLAS$IDDECLAS %in%(RLT_PCHFREQDECLAS$IDDECLAS),c("STATION", "PARAMETRE", "CLASSEPCH","IDDECLAS")]
	
	# selection des prelevements déclassants (déclassant = état 3 à 5)
	TEMPDECLAS<-PCHFREQ[,c("STATION", "PARAMETRE", "CLASSEPCH")]
	TEMPDECLAS$DECLASS<-as.numeric(0)
	TEMPDECLAS$DECLASS[TEMPDECLAS$CLASSEPCH %in%(3:5)]<-1

	# comptage par parametre du nb de prélevements déclassants
	TEMPFREQDECLAS<-data.frame(table(TEMPDECLAS$STATION,TEMPDECLAS$PARAMETRE,TEMPDECLAS$DECLASS))
	names(TEMPFREQDECLAS)<-c("STATION","PARAMETRE","DECLASS","Freq")
	TEMPFREQDECLAS$DECLASS<-as.numeric(as.character(TEMPFREQDECLAS$DECLASS))
	TEMPFREQDECLAS$Freq[TEMPFREQDECLAS$DECLASS == 0]<-0
	TEMPFREQDECLAS<-TEMPFREQDECLAS[order(TEMPFREQDECLAS$STATION, TEMPFREQDECLAS$STATION, -TEMPFREQDECLAS$DECLASS),]
	TEMPFREQDECLAS<-TEMPFREQDECLAS[!duplicated(TEMPFREQDECLAS[,c("STATION","PARAMETRE")]),]

	# mise en forme du tableau final pour fréquence de déclassement
	RLT_PCHFREQDECLAS<-RLT_PCHFREQDECLAS[,c("STATION", "ETATPCH")]
		for (i in  1:nrow(PARAMETREPCH)) {
			TEMPP<-TEMPFREQDECLAS[TEMPFREQDECLAS$PARAMETRE==PARAMETREPCH$PARAMETRE[i],c("STATION","Freq")]
			names(TEMPP)[2]<-paste("fd",toupper(PARAMETREPCH$NOM[i]), sep="")
			RLT_PCHFREQDECLAS<-merge(RLT_PCHFREQDECLAS,TEMPP,by="STATION", all.x=TRUE)
			rm(TEMPP)
		}	
}

####################################
## BIOLOGIE : FREQUENCE PRELEVEMENT
## Fréquence de déclassement non calculée car on détermine une moyenne (sans intéret) !
####################################
if (SEEEBIO=="oui") {
	BIOFREQ<-data.frame(table(DATABIO$STATION,DATABIO$PARAGROUP))
	names(BIOFREQ)[1]<-"STATION"
	names(BIOFREQ)[2]<-"PARAGROUP"
	
	RLT_BIOFREQ<-data.frame(RLT_BIO[,c("STATION")])
	names(RLT_BIOFREQ)[1]<-"STATION"
		for (i in  1:nrow(TABLEBIO)  ) {
			TEMPBIO<-BIOFREQ[BIOFREQ$PARAGROUP==TABLEBIO$PARAGROUP[i],c("STATION", "Freq")]
			names(TEMPBIO)[2]<-paste("f",TABLEBIO$PARALIBGROUP[i],sep="")
			RLT_BIOFREQ<-merge(RLT_BIOFREQ,TEMPBIO,by="STATION", all.x=TRUE)
			rm(TEMPBIO)
		}
	rm(BIOFREQ)
}

#########################
## POLLUANTS SPECIFIQUES : FREQUENCE PRELEVEMENT
# Fréquence de déclassement non calculée car on détermine une moyenne (sans intéret) !
#########################
if (SEEEPS=="oui") {
	PSFREQ<-data.frame(table(DATAPOLSPE$STATION,DATAPOLSPE$PARAMETRE))
	names(PSFREQ)[1]<-"STATION"
	names(PSFREQ)[2]<-"PARAMETRE"
	
	RLT_POLSPEFREQ<-data.frame(RLT_POLSPE[,c("STATION")])
	names(RLT_POLSPEFREQ)[1]<-"STATION"
		# mise en forme tableau pour la fréquence
		for (i in  1:nrow(PARAMETREPOLSPE)  ) {
			TEMPP<-PSFREQ[PSFREQ$PARAMETRE==PARAMETREPOLSPE$PARAMETRE[i],c("STATION", "Freq")]
			names(TEMPP)[2]<-paste("f",toupper(PARAMETREPOLSPE$PARAMETRELIB[i]), sep="")
			RLT_POLSPEFREQ<-merge(RLT_POLSPEFREQ,TEMPP,by="STATION", all.x=TRUE)
			rm(TEMPP)
		}
		
		# mise en forme tableau pour année retenue pour le calcul de la moyenne
		for (i in  1:nrow(PARAMETREPOLSPE)  ) {
			TEMPP<-PSANNEE[PSANNEE$PARAMETRE==PARAMETREPOLSPE$PARAMETRE[i],c("STATION", "ANNEE")]
			names(TEMPP)[2]<-paste("a",toupper(PARAMETREPOLSPE$PARAMETRELIB[i]), sep="")
			RLT_POLSPEFREQ<-merge(RLT_POLSPEFREQ,TEMPP,by="STATION", all.x=TRUE)
			rm(TEMPP)
		}
	rm(PSFREQ,PSANNEE)
}

#########################
## ETAT CHIMIQUE : FREQUENCE PRELEVEMENT
# Fréquence de déclassement non calculée car on détermine une moyenne (sans intéret) !
#########################
if (SEEECHIM=="oui") {
	CHIMFREQ<-data.frame(table(DATACHIMFREQ$STATION,DATACHIMFREQ$PARAGROUP))
	names(CHIMFREQ)[1]<-"STATION"
	names(CHIMFREQ)[2]<-"PARAGROUP"
	
	RLT_CHIMFREQ<-data.frame(RLT_CHIMSTATION[,c("STATION")])
	names(RLT_CHIMFREQ)[1]<-"STATION"
		# mise en forme tableau pour la fréquence
		for (i in  1:nrow(PARAGROUPCHIM)  ) {
			TEMPP<-CHIMFREQ[CHIMFREQ$PARAGROUP==PARAGROUPCHIM$PARAGROUP[i],c("STATION", "Freq")]
			names(TEMPP)[2]<-paste("f",toupper(PARAGROUPCHIM$PARAGROUPLIBCOURT[i]), sep="")
			RLT_CHIMFREQ<-merge(RLT_CHIMFREQ,TEMPP,by="STATION", all.x=TRUE)
			rm(TEMPP)
		}
		
		# mise en forme tableau pour année retenue pour le calcul de la moyenne
		CHIMANNEE<-merge(CHIMANNEE,LIAISONCHIM,by="PARAMETRE")
		CHIMANNEE<-aggregate(ANNEE ~ STATION + PARAGROUP, data = CHIMANNEE , min) # extraction de l'année min s'il y a plusieurs années au sein d'un groupe de parametre. A priori cas improbable! (RAS sur data AEAG)
				
		for (i in  1:nrow(PARAGROUPCHIM)  ) {
			TEMPP<-CHIMANNEE[CHIMANNEE$PARAGROUP==PARAGROUPCHIM$PARAGROUP[i],c("STATION", "ANNEE")]
			names(TEMPP)[2]<-paste("a",toupper(PARAGROUPCHIM$PARAGROUPLIBCOURT[i]), sep="")
			RLT_CHIMFREQ<-merge(RLT_CHIMFREQ,TEMPP,by="STATION", all.x=TRUE)
			rm(TEMPP)
		}

	
	
	RLT_CHIMFREQ_NAMES<-names(RLT_CHIMFREQ)
	if (nchar(as.character(RLT_CHIMFREQ$STATION))[1] == 7 & !is.na(RLT_CHIMFREQ$STATION[1])) {
			RLT_CHIMFREQ$STATIONTXT<-as.character(RLT_CHIMFREQ$STATION)
			RLT_CHIMFREQ$STATIONTXT[nchar(as.character(RLT_CHIMFREQ$STATION)) == 7 & !is.na(RLT_CHIMFREQ$STATION)]<-paste("0",RLT_CHIMFREQ$STATION[nchar(as.character(RLT_CHIMFREQ$STATION)) == 7 & !is.na(RLT_CHIMFREQ$STATION)],sep="")
			RLT_CHIMFREQ<-RLT_CHIMFREQ[,c("STATIONTXT",RLT_CHIMFREQ_NAMES)]
			RLT_CHIMFREQ_NAMES<-names(RLT_CHIMFREQ)
	}
	

	if(exists("DATACHIMFREQQUANTI")) {  ##Ajout 20/04/2018 : calcul de frequence de mesures quantifiées
	
		CHIMFREQQUANTI<-data.frame(table(DATACHIMFREQQUANTI$STATION,DATACHIMFREQQUANTI$PARAGROUP))
		names(CHIMFREQQUANTI)[1]<-"STATION"
		names(CHIMFREQQUANTI)[2]<-"PARAGROUP"
		CHIMFREQQUANTI<-merge(CHIMFREQQUANTI, CHIMFREQ[,c("STATION","PARAGROUP")], all = TRUE)
		if(nrow(DATACHIMFREQQUANTI)>0) {
		CHIMFREQQUANTI$Freq[is.na(CHIMFREQQUANTI$Freq)]<-0
		}
		
		
		RLT_CHIMFREQQUANTI<-data.frame(RLT_CHIMSTATION[,c("STATION")])
		names(RLT_CHIMFREQQUANTI)[1]<-"STATION"
			# mise en forme tableau pour la fréquence
		if(nrow(DATACHIMFREQQUANTI)>0) {	
				for (i in  1:nrow(PARAGROUPCHIM)  ) {
					TEMPP<-CHIMFREQQUANTI[CHIMFREQQUANTI$PARAGROUP==PARAGROUPCHIM$PARAGROUP[i],c("STATION", "Freq")]
					names(TEMPP)[2]<-paste("fq",toupper(PARAGROUPCHIM$PARAGROUPLIBCOURT[i]), sep="")
					RLT_CHIMFREQQUANTI<-merge(RLT_CHIMFREQQUANTI,TEMPP,by="STATION", all.x=TRUE)
					rm(TEMPP)
				}
				

			
			RLT_CHIMFREQQUANTI_NAMES<-names(RLT_CHIMFREQQUANTI)
			if (nchar(as.character(RLT_CHIMFREQQUANTI$STATION))[1] == 7 & !is.na(RLT_CHIMFREQQUANTI$STATION[1])) {
					RLT_CHIMFREQQUANTI$STATIONTXT<-as.character(RLT_CHIMFREQQUANTI$STATION)
					RLT_CHIMFREQQUANTI$STATIONTXT[nchar(as.character(RLT_CHIMFREQQUANTI$STATION)) == 7 & !is.na(RLT_CHIMFREQQUANTI$STATION)]<-paste("0",RLT_CHIMFREQQUANTI$STATION[nchar(as.character(RLT_CHIMFREQQUANTI$STATION)) == 7 & !is.na(RLT_CHIMFREQQUANTI$STATION)],sep="")
					RLT_CHIMFREQQUANTI<-RLT_CHIMFREQQUANTI[,c("STATIONTXT",RLT_CHIMFREQQUANTI_NAMES)]
					RLT_CHIMFREQQUANTI_NAMES<-names(RLT_CHIMFREQQUANTI)
			}
		}	
	
	
	
	rm(CHIMFREQ,CHIMANNEE)
	}
	
	
	
}
