##############################################
## SEEE - COURS D'EAU : �tat physicochimique
## Application de l'arr�t� de janvier 2010

## script cr�e en mars 2013
## script modifi� en d�cembre 2014 pour int�grer les r�gles du 2nd cycle
##############################################

##############################################
## CALCUL DES RANG 90 / VALEUR MOYENNE / INDICE (type SEQ-eau)

## PCH
# principe de calcul de l'indice
# (((r�sultat - valeur inf borne)*20) / (valeur sup borne - valeur inf borne)) + indice borne inf

### BIO
# principe de calcul de l'indice par ordre croissant (IBD, IBG, IBGA)
# (((r�sultat - valeur inf borne)*20) / (valeur sup borne - valeur inf borne)) + indice borne inf

# principe de calcul de l'indice par ordre d�croissant (IPR)
# (((valeur sup borne - r�sultat)*20) / (valeur sup borne - valeur inf borne)) + indice borne inf

### NOTA :
# La fonction "merge" retrie le vecteur. la commande "sort" ne semble pas fonctionner --> pourquoi ?
# on d�clare donc les conditions "condXXX" apr�s chaque "merge" pour s'assurer que le true/false cr�� avec les conditions est toujours dans le m�me ordre que le merge.
# sinon le r�sultat ne correspond pas � la condition.
# On aurait pu rester comme initialement, avec 1 condition pour les param�tres et 1 autre pour les classes et la combinaison des 2 donne la condition globale,
# mais comme on les d�clarait avant les merge �a obligait � d�finir un ID pour tjs trier les enregistrements dans le m�me sens et faire un "order" apr�s chaque "merge".
# on a jug� cette solution moins robuste que de r�p�ter les conditions apr�s chaque merge, m�me si �a engendre un code avec plus de lignes.

#SAVE()

#########################################
# PCH INDICE : Param�tre ordre croissant
if (SEEEPCH=="oui") {
	gc()
	RLT_PCH<-merge(RLT_PCH,PARAMETREPCH[,c("PARAMETRE", "SUPB", "INFB", "INFV", "INFJ", "INFO", "INFR")],by="PARAMETRE", all.x=TRUE, sort = FALSE)
	FORMULEINDICE<- function (DAT=RLT_PCH) {
		DAT$INDICE[condPCH1]<-round((((DAT$RESULTAT[condPCH1]-DAT$INFB[condPCH1])*20)/(DAT$SUPB[condPCH1]-DAT$INFB[condPCH1]))+80) 
		DAT$INDICE[condPCH2]<-round((((DAT$RESULTAT[condPCH2]-DAT$INFV[condPCH2])*20)/(DAT$INFB[condPCH2]-DAT$INFV[condPCH2]))+60)
		DAT$INDICE[condPCH3]<-round((((DAT$RESULTAT[condPCH3]-DAT$INFJ[condPCH3])*20)/(DAT$INFV[condPCH3]-DAT$INFJ[condPCH3]))+40)
		DAT$INDICE[condPCH4]<-round((((DAT$RESULTAT[condPCH4]-DAT$INFO[condPCH4])*20)/(DAT$INFJ[condPCH4]-DAT$INFO[condPCH4]))+20)
		DAT$INDICE[condPCH5]<-round((((DAT$RESULTAT[condPCH5]-DAT$INFR[condPCH5])*20)/(DAT$INFO[condPCH5]-DAT$INFR[condPCH5]))+0)
		DAT$INDICE[DAT$INDICE>100]<-100
		DAT$INDICE[DAT$INDICE<0]<-0
		DAT
	}

	# A cause des NO3 en 3 classes, on est oblig� de situer le r�sultat entre les bornes, plutot que d'utiliser la classe PCH pour d�finir la bonne formule
	# sinon l'indice renvoy� n'est pas conforme. Cela oblige � dissocier les param�tres par ordre croissant et d�croissant, car les signes ne sont pas �quivalents
	condPCH1<-RLT_PCH$PARAMETRE %in% c("1313","1841","1433","1350","1335","1339","1340","1302max") & !is.na(RLT_PCH$CLASSEPCH) & RLT_PCH$RESULTAT<=RLT_PCH$INFB
	condPCH2<-RLT_PCH$PARAMETRE %in% c("1313","1841","1433","1350","1335","1339","1340","1302max") & !is.na(RLT_PCH$CLASSEPCH) & RLT_PCH$RESULTAT>RLT_PCH$INFB & RLT_PCH$RESULTAT<=RLT_PCH$INFV
	condPCH3<-RLT_PCH$PARAMETRE %in% c("1313","1841","1433","1350","1335","1339","1340","1302max") & !is.na(RLT_PCH$CLASSEPCH) & RLT_PCH$RESULTAT>RLT_PCH$INFV & RLT_PCH$RESULTAT<=RLT_PCH$INFJ
	condPCH4<-RLT_PCH$PARAMETRE %in% c("1313","1841","1433","1350","1335","1339","1340","1302max") & !is.na(RLT_PCH$CLASSEPCH) & RLT_PCH$RESULTAT>RLT_PCH$INFJ & RLT_PCH$RESULTAT<=RLT_PCH$INFO
	condPCH5<-RLT_PCH$PARAMETRE %in% c("1313","1841","1433","1350","1335","1339","1340","1302max") & !is.na(RLT_PCH$CLASSEPCH) & RLT_PCH$RESULTAT>RLT_PCH$INFO & RLT_PCH$RESULTAT<=RLT_PCH$INFR

	RLT_PCH<-FORMULEINDICE(DAT = RLT_PCH)
	rm(condPCH1, condPCH2, condPCH3, condPCH4, condPCH5)
	gc()
	flush.console()

# PCH INDICE : Param�tre ordre d�croissant
# A cause des NO3 en 3 classes, on est oblig� de situer le r�sultat entre les bornes, plutot que d'utiliser la classe PCH pour d�finir la bonne formule
# sinon l'indice renvoy� n'est pas conforme. Cela oblige � dissocier les param�tres par ordre croissant et d�croissant, car les signes ne sont pas �quivalents
	condPCH1<-RLT_PCH$PARAMETRE %in% c("1311", "1312", "1302min") & !is.na(RLT_PCH$CLASSEPCH) & RLT_PCH$RESULTAT>=RLT_PCH$INFB
	condPCH2<-RLT_PCH$PARAMETRE %in% c("1311", "1312", "1302min") & !is.na(RLT_PCH$CLASSEPCH) & RLT_PCH$RESULTAT>=RLT_PCH$INFV & RLT_PCH$RESULTAT<RLT_PCH$INFB
	condPCH3<-RLT_PCH$PARAMETRE %in% c("1311", "1312", "1302min") & !is.na(RLT_PCH$CLASSEPCH) & RLT_PCH$RESULTAT>=RLT_PCH$INFJ & RLT_PCH$RESULTAT<RLT_PCH$INFV
	condPCH4<-RLT_PCH$PARAMETRE %in% c("1311", "1312", "1302min") & !is.na(RLT_PCH$CLASSEPCH) & RLT_PCH$RESULTAT>=RLT_PCH$INFO & RLT_PCH$RESULTAT<RLT_PCH$INFJ
	condPCH5<-RLT_PCH$PARAMETRE %in% c("1311", "1312", "1302min") & !is.na(RLT_PCH$CLASSEPCH) & RLT_PCH$RESULTAT>=RLT_PCH$INFR & RLT_PCH$RESULTAT<RLT_PCH$INFO

	RLT_PCH<-FORMULEINDICE(DAT = RLT_PCH)
	rm(condPCH1, condPCH2, condPCH3, condPCH4, condPCH5)

	RLT_PCH<-RLT_PCH[,c("IDTRI","STATION", "DATEPRELEV", "HEUREPRELEV","PARAMETRE", "FRACTION", "RESULTAT", "REMARQUE", "CLASSEPCH", "INDICE", "RANG", "Freq", "R90")]
	RLT_PCH<-merge(RLT_PCH, STATION, by="STATION")
	gc()
	flush.console()
	
# PCH INDICE : Temp�rature (prise en compte du domaine)
	RLT_PCH_1301<-RLT_PCH[(RLT_PCH$PARAMETRE=="1301" & RLT_PCH$CONTEXTE_PISCICOLE %in% c("salmonicole", "intermediaire", "cyprinicole") & !is.na(RLT_PCH$CONTEXTE_PISCICOLE)) ,]
	RLT_PCH_1301na<-RLT_PCH[RLT_PCH$PARAMETRE=="1301" & is.na(RLT_PCH$CONTEXTE_PISCICOLE),] #sert uniquement dans le rbinb pour recr�er le tableau final
	RLT_PCH_AUTRE<-RLT_PCH[!(RLT_PCH$PARAMETRE=="1301"),] #sert uniquement dans le rbinb pour recr�er le tableau final

	RLT_PCH_1301$PARATEMP<-paste(RLT_PCH_1301$PARAMETRE,RLT_PCH_1301$CONTEXTE_PISCICOLE,sep="")
	PARAMETREPCHEXCEPT$PARATEMP<-paste(PARAMETREPCHEXCEPT$PARAMETRE,PARAMETREPCHEXCEPT$CONTEXTE_PISCICOLE,sep="")
	RLT_PCH_1301<-merge(RLT_PCH_1301, PARAMETREPCHEXCEPT[,c("PARATEMP", "SUPB", "INFB", "INFV", "INFJ", "INFO", "INFR")], by="PARATEMP")
	gc()
	condPCH1<-RLT_PCH_1301$CLASSEPCH==1
	condPCH2<-RLT_PCH_1301$CLASSEPCH==2
	condPCH3<-RLT_PCH_1301$CLASSEPCH==3
	condPCH4<-RLT_PCH_1301$CLASSEPCH==4
	condPCH5<-RLT_PCH_1301$CLASSEPCH==5

	RLT_PCH_1301<-FORMULEINDICE(DAT = RLT_PCH_1301)
	rm(condPCH1, condPCH2, condPCH3, condPCH4, condPCH5)

	RLT_PCH_1301<-RLT_PCH_1301[,c("IDTRI","STATION", "DATEPRELEV", "HEUREPRELEV","PARAMETRE", "FRACTION", "RESULTAT", "REMARQUE", "CLASSEPCH", "INDICE", "RANG", "Freq", "R90")]
	RLT_PCH_1301<-merge(RLT_PCH_1301, STATION, by="STATION")
	gc()
	RLT_PCH<-rbind(RLT_PCH_1301,RLT_PCH_1301na,RLT_PCH_AUTRE)
	rm(RLT_PCH_1301, RLT_PCH_1301na, RLT_PCH_AUTRE)
	gc()
	flush.console()

	## Si l'�tat est Ind�termin� alors on met Vide
	RLT_PCH$INDICE[RLT_PCH$CLASSEPCH == 0]<-NA

	
# TABLEAU FINAL : mise en forme
	PARAMETREPCH<-PARAMETREPCH[order(PARAMETREPCH$IDTRI),]
	
	# Tableau pour les valeurs au rang 90
	PCHR90<-data.frame(PCHEXCEPT_STATION[,c("STATION")])
	names(PCHR90)<-"STATION"
		for (i in  1:nrow(PARAMETREPCH)) {
		TEMPP<-RLT_PCH[RLT_PCH$PARAMETRE==PARAMETREPCH$PARAMETRE[i],c("STATION","RESULTAT")]
		names(TEMPP)[2]<-paste("v",toupper(PARAMETREPCH$NOM[i]), sep="")
		PCHR90<-merge(PCHR90,TEMPP,by="STATION", all.x=TRUE)
		rm(TEMPP)
		gc()
	}	

	# Tableau pour les indices par param�tre
	PCHINDICE<-data.frame(PCHEXCEPT_STATION[,c("STATION")])
	names(PCHINDICE)<-"STATION"
	for (i in  1:nrow(PARAMETREPCH)) {
		TEMPP<-RLT_PCH[RLT_PCH$PARAMETRE==PARAMETREPCH$PARAMETRE[i],c("STATION","INDICE")]
		names(TEMPP)[2]<-paste("i",toupper(PARAMETREPCH$NOM[i]), sep="")
		PCHINDICE<-merge(PCHINDICE,TEMPP,by="STATION", all.x=TRUE)
		rm(TEMPP)
		gc()
	}	
			
	# Tableau pour les indices par �l�ment de qualit�
	RLT_PCH<-merge(RLT_PCH,PARAMETREPCH[,c("PARAMETRE","ELTQUALITE")],by="PARAMETRE", all.x=TRUE)
	
	PCHELT_temp<-aggregate(INDICE ~ STATION + ELTQUALITE, data = RLT_PCH , min, na.rm = TRUE) # calcul de l'indice d�classant par �l�ment
	names(PCHELT_temp)[3]<-"ETATELT"
			
	PCHELTINDICE<-data.frame(PCHEXCEPT_STATION[,c("STATION")])
	names(PCHELTINDICE)<-"STATION"
	for (i in  1:nrow(TABLEELT)  ) {
		TEMPELT<-PCHELT_temp[PCHELT_temp$ELTQUALITE==TABLEELT$ELTQUALITE[i],c("STATION", "ETATELT")]
		names(TEMPELT)[2]<-paste("i",toupper(TABLEELT$ELTQUALITE[i]), sep="")
		PCHELTINDICE<-merge(PCHELTINDICE,TEMPELT,by="STATION", all.x=TRUE)
		rm(TEMPELT)
	}
	
	# Tableau avec Indice ETAT PCH - demande sp�cifique AESN	
	if (BASSIN=="AESN") {
		PCHETATINDICE<-aggregate(ETATELT ~ STATION , data = PCHELT_temp , min, na.rm = TRUE) # calcul de l'indice de l'�tat PCH
		names(PCHETATINDICE)[2]<-"iETATPCH"
		iETATPCH_NAMES<-"iETATPCH"
		RLT_PCHINDICE<-merge(PCHR90, PCHETATINDICE,by="STATION", all= TRUE)
		RLT_PCHINDICE<-merge(RLT_PCHINDICE, PCHELTINDICE,by="STATION", all= TRUE)
		RLT_PCHINDICE<-merge(RLT_PCHINDICE, PCHINDICE,by="STATION", all= TRUE)
		rm(PCHR90, PCHINDICE,PCHELT_temp, PCHETATINDICE)
	} else {
		RLT_PCHINDICE<-merge(PCHR90, PCHELTINDICE,by="STATION", all= TRUE)
		RLT_PCHINDICE<-merge(RLT_PCHINDICE, PCHINDICE,by="STATION", all= TRUE)
		rm(PCHR90, PCHINDICE,PCHELT_temp)
	}	
	gc()
}

######################################
## BIOLOGIE : VALEUR MOYENNE & INDICE 
## modifi� le 30/01/15 (2nd cycle): indice est calcul� depuis l'EQR et non plus la note bio !!
######################################
if (SEEEBIO=="oui") {
	BIO_MOY$INDICEBIO<-as.numeric("")
#calcul indice IBD
# modifi� le 02/02/15 pour calcul sur EQR
	BIO_MOY<-merge(BIO_MOY,GRILLEIBD,by="TYPEFR", all.x=TRUE, sort = FALSE)
	condIBD1<-BIO_MOY$PARAGROUP=="5856" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==1
	condIBD2<-BIO_MOY$PARAGROUP=="5856" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==2
	condIBD3<-BIO_MOY$PARAGROUP=="5856" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==3
	condIBD4<-BIO_MOY$PARAGROUP=="5856" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==4
	condIBD5<-BIO_MOY$PARAGROUP=="5856" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==5
	condIBD100<-BIO_MOY$PARAGROUP=="5856" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==1 & BIO_MOY$EQRBIO>1.159 # seuil EQR max d�fini par J.MOY (EQR max sur la p�riode 2011-2013)
	
	BIO_MOY$INDICEBIO[condIBD1]<-round((((BIO_MOY$EQRBIO[condIBD1]-BIO_MOY$INFB[condIBD1])*20)/(1.159-BIO_MOY$INFB[condIBD1]))+80) # seuil max � modifier si n�cessaire 
	BIO_MOY$INDICEBIO[condIBD2]<-round((((BIO_MOY$EQRBIO[condIBD2]-BIO_MOY$INFV[condIBD2])*20)/(BIO_MOY$INFB[condIBD2]-BIO_MOY$INFV[condIBD2]))+60)
	BIO_MOY$INDICEBIO[condIBD3]<-round((((BIO_MOY$EQRBIO[condIBD3]-BIO_MOY$INFJ[condIBD3])*20)/(BIO_MOY$INFV[condIBD3]-BIO_MOY$INFJ[condIBD3]))+40)
	BIO_MOY$INDICEBIO[condIBD4]<-round((((BIO_MOY$EQRBIO[condIBD4]-BIO_MOY$INFO[condIBD4])*20)/(BIO_MOY$INFJ[condIBD4]-BIO_MOY$INFO[condIBD4]))+20)
	BIO_MOY$INDICEBIO[condIBD5]<-round((((BIO_MOY$EQRBIO[condIBD5]-BIO_MOY$INFR[condIBD5])*20)/(BIO_MOY$INFO[condIBD5]-BIO_MOY$INFR[condIBD5]))+0)
	BIO_MOY$INDICEBIO[condIBD100]<-100
	BIO_MOY<-BIO_MOY[,c(BIO_MOY_NAMES, "INDICEBIO")]
	rm(condIBD1, condIBD2, condIBD3, condIBD4, condIBD5,condIBD100)

#calcul indice IBG & IBGA
# modifi� le 02/02/15 pour calcul sur EQR
	BIO_MOY<-merge(BIO_MOY,GRILLEIBG,by="TYPEFR", all.x=TRUE, sort = FALSE)
	condIBG1<-BIO_MOY$PARAGROUP %in% c("5910","6951") & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==1
	condIBG2<-BIO_MOY$PARAGROUP %in% c("5910","6951") & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==2
	condIBG3<-BIO_MOY$PARAGROUP %in% c("5910","6951") & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==3
	condIBG4<-BIO_MOY$PARAGROUP %in% c("5910","6951") & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==4
	condIBG5<-BIO_MOY$PARAGROUP %in% c("5910","6951") & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==5
	condIBG100<-BIO_MOY$PARAGROUP %in% c("5910","6951") & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==1 & BIO_MOY$EQRBIO>1.358 # seuil EQR max d�fini par J.MOY (EQR max sur la p�riode 2011-2013)

	BIO_MOY$INDICEBIO[condIBG1]<-round((((BIO_MOY$EQRBIO[condIBG1]-BIO_MOY$INFB[condIBG1])*20)/(1.358-BIO_MOY$INFB[condIBG1]))+80) # seuil max � modifier si n�cessaire 
	BIO_MOY$INDICEBIO[condIBG2]<-round((((BIO_MOY$EQRBIO[condIBG2]-BIO_MOY$INFV[condIBG2])*20)/(BIO_MOY$INFB[condIBG2]-BIO_MOY$INFV[condIBG2]))+60)
	BIO_MOY$INDICEBIO[condIBG3]<-round((((BIO_MOY$EQRBIO[condIBG3]-BIO_MOY$INFJ[condIBG3])*20)/(BIO_MOY$INFV[condIBG3]-BIO_MOY$INFJ[condIBG3]))+40)
	BIO_MOY$INDICEBIO[condIBG4]<-round((((BIO_MOY$EQRBIO[condIBG4]-BIO_MOY$INFO[condIBG4])*20)/(BIO_MOY$INFJ[condIBG4]-BIO_MOY$INFO[condIBG4]))+20)
	BIO_MOY$INDICEBIO[condIBG5]<-round((((BIO_MOY$EQRBIO[condIBG5]-BIO_MOY$INFR[condIBG5])*20)/(BIO_MOY$INFO[condIBG5]-BIO_MOY$INFR[condIBG5]))+0)
	BIO_MOY$INDICEBIO[condIBG100]<-100
	BIO_MOY<-BIO_MOY[,c(BIO_MOY_NAMES, "INDICEBIO")]
	rm(condIBG1, condIBG2, condIBG3, condIBG4, condIBG5,condIBG100)

# calcul indice IPR
# modifi� le 02/02/15 pour grille HER, alti
	BIO_MOY<-merge(BIO_MOY,GRILLEIPR,by="TYPEFR", all.x=TRUE, sort = FALSE)
	# MAJ borne inferieure Verte si alti >=500m. Si alti non renseign�e, INFV ne change pas.
	BIO_MOY$INFV[BIO_MOY$ALTITUDE>=500 & !(is.na(BIO_MOY$ALTITUDE))]<-BIO_MOY$VALALTIV[BIO_MOY$ALTITUDE>=500 & !(is.na(BIO_MOY$ALTITUDE))]
	
	condIPR1<-BIO_MOY$PARAGROUP=="7036" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==1
	condIPR2<-BIO_MOY$PARAGROUP=="7036" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==2
	condIPR3<-BIO_MOY$PARAGROUP=="7036" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==3
	condIPR4<-BIO_MOY$PARAGROUP=="7036" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==4
	condIPR5<-BIO_MOY$PARAGROUP=="7036" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==5

	BIO_MOY$INDICEBIO[condIPR1]<-round((((BIO_MOY$INFB[condIPR1]-BIO_MOY$RESULTAT[condIPR1])*20)/(BIO_MOY$INFB[condIPR1]-0))+80)  
	BIO_MOY$INDICEBIO[condIPR2]<-round((((BIO_MOY$INFV[condIPR2]-BIO_MOY$RESULTAT[condIPR2])*20)/(BIO_MOY$INFV[condIPR2]-BIO_MOY$INFB[condIPR2]))+60)
	BIO_MOY$INDICEBIO[condIPR3]<-round((((BIO_MOY$INFJ[condIPR3]-BIO_MOY$RESULTAT[condIPR3])*20)/(BIO_MOY$INFJ[condIPR3]-BIO_MOY$INFV[condIPR3]))+40)
	BIO_MOY$INDICEBIO[condIPR4]<-round((((BIO_MOY$INFO[condIPR4]-BIO_MOY$RESULTAT[condIPR4])*20)/(BIO_MOY$INFO[condIPR4]-BIO_MOY$INFJ[condIPR4]))+20)
	BIO_MOY$INDICEBIO[condIPR5]<-round((((BIO_MOY$INFR[condIPR5]-BIO_MOY$RESULTAT[condIPR5])*20)/(BIO_MOY$INFR[condIPR5]-BIO_MOY$INFO[condIPR5]))+0)
	BIO_MOY$INDICEBIO[BIO_MOY$RESULTAT>BIO_MOY$INFR & BIO_MOY$CLASSEBIO==5]<-0
	
	BIO_MOY<-BIO_MOY[,c(BIO_MOY_NAMES, "INDICEBIO")]
	rm(condIPR1, condIPR2, condIPR3, condIPR4, condIPR5)
	
#calcul indice IBMR : ajout� le 10/12/14
	BIO_MOY<-merge(BIO_MOY,GRILLEIBMR,by="TYPEFR", all.x=TRUE, sort = FALSE)
	condIBMR1<-BIO_MOY$PARAGROUP %in% c("2928") & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==1
	condIBMR2<-BIO_MOY$PARAGROUP %in% c("2928") & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==2
	condIBMR3<-BIO_MOY$PARAGROUP %in% c("2928") & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==3
	condIBMR4<-BIO_MOY$PARAGROUP %in% c("2928") & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==4
	condIBMR5<-BIO_MOY$PARAGROUP %in% c("2928") & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==5
	condIBMR100<-BIO_MOY$PARAGROUP %in% c("2928") & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==1 & BIO_MOY$EQRBIO>1.384 # seuil EQR max d�fini par J.MOY (EQR max sur la p�riode 2011-2013)

	BIO_MOY$INDICEBIO[condIBMR1]<-round((((BIO_MOY$EQRBIO[condIBMR1]-BIO_MOY$INFB[condIBMR1])*20)/(1.384-BIO_MOY$INFB[condIBMR1]))+80) # seuil max � modifier si n�cessaire 
	BIO_MOY$INDICEBIO[condIBMR2]<-round((((BIO_MOY$EQRBIO[condIBMR2]-BIO_MOY$INFV[condIBMR2])*20)/(BIO_MOY$INFB[condIBMR2]-BIO_MOY$INFV[condIBMR2]))+60)
	BIO_MOY$INDICEBIO[condIBMR3]<-round((((BIO_MOY$EQRBIO[condIBMR3]-BIO_MOY$INFJ[condIBMR3])*20)/(BIO_MOY$INFV[condIBMR3]-BIO_MOY$INFJ[condIBMR3]))+40)
	BIO_MOY$INDICEBIO[condIBMR4]<-round((((BIO_MOY$EQRBIO[condIBMR4]-BIO_MOY$INFO[condIBMR4])*20)/(BIO_MOY$INFJ[condIBMR4]-BIO_MOY$INFO[condIBMR4]))+20)
	BIO_MOY$INDICEBIO[condIBMR5]<-round((((BIO_MOY$EQRBIO[condIBMR5]-BIO_MOY$INFR[condIBMR5])*20)/(BIO_MOY$INFO[condIBMR5]-BIO_MOY$INFR[condIBMR5]))+0)
	BIO_MOY$INDICEBIO[condIBMR100]<-100
	BIO_MOY<-BIO_MOY[,c(BIO_MOY_NAMES, "INDICEBIO")]
	rm(condIBMR1, condIBMR2, condIBMR3, condIBMR4, condIBMR5,condIBMR100)
	flush.console()	

# On met vide si Etat vide
BIO_MOY$INDICEBIO[BIO_MOY$CLASSEBIO == 0]<-NA

	
# TABLEAU FINAL : mise en forme
	# Tableau pour les valeurs moyennes
	TEMP_BIOMOY<-data.frame(RLT_BIO[,"STATION"])
	names(TEMP_BIOMOY)[1]<-"STATION"
	for (i in  1:nrow(TABLEBIO)  ) {
		TEMPBIO<-BIO_MOY[BIO_MOY$PARAGROUP==TABLEBIO$PARAGROUP[i],c("STATION", "RESULTAT")]
		names(TEMPBIO)[2]<-paste ("v",TABLEBIO$PARALIBGROUP[i], sep="")
		TEMP_BIOMOY<-merge(TEMP_BIOMOY,TEMPBIO,by="STATION", all.x=TRUE)
		rm(TEMPBIO)
	}
	
	# Tableau pour les valeurs EQR
	TEMP_BIOEQR<-data.frame(RLT_BIO[,"STATION"])
	names(TEMP_BIOEQR)[1]<-"STATION"
	for (i in  1:nrow(TABLEBIO)  ) {
		TEMPBIO<-BIO_MOY[BIO_MOY$PARAGROUP==TABLEBIO$PARAGROUP[i],c("STATION", "EQRBIO")]
		names(TEMPBIO)[2]<-paste ("eqr",TABLEBIO$PARALIBGROUP[i], sep="")
		TEMP_BIOEQR<-merge(TEMP_BIOEQR,TEMPBIO,by="STATION", all.x=TRUE)
		rm(TEMPBIO)
	}
		
	# Tableau pour les indices
	TEMP_BIOINDICE<-data.frame(RLT_BIO[,"STATION"])
	names(TEMP_BIOINDICE)[1]<-"STATION"
	for (i in  1:nrow(TABLEBIO)  ) {
		TEMPBIO<-BIO_MOY[BIO_MOY$PARAGROUP==TABLEBIO$PARAGROUP[i],c("STATION", "INDICEBIO")]
		names(TEMPBIO)[2]<-paste ("i",TABLEBIO$PARALIBGROUP[i], sep="")
		TEMP_BIOINDICE<-merge(TEMP_BIOINDICE,TEMPBIO,by="STATION", all.x=TRUE)
		rm(TEMPBIO)
	}
	
	# Mise en forme et Tableau pour l'indice ETATBIO : demande sp�cifique AESN
	if (BASSIN=="AESN") {
		TEMP_BIOINDICEETAT<-aggregate(INDICEBIO ~ STATION , data = BIO_MOY , min, na.rm = TRUE) # calcul de l'indice de l'�tat BIO
		names(TEMP_BIOINDICEETAT)[2]<-"iETATBIO"
		iETATBIO_NAMES<-"iETATBIO"
		RLT_BIOINDICE<-merge(TEMP_BIOMOY,TEMP_BIOEQR,by="STATION", all= TRUE)
		RLT_BIOINDICE<-merge(RLT_BIOINDICE,TEMP_BIOINDICEETAT,by="STATION", all= TRUE)
		RLT_BIOINDICE<-merge(RLT_BIOINDICE,TEMP_BIOINDICE,by="STATION", all= TRUE)
		rm(TEMP_BIOMOY, TEMP_BIOINDICE, TEMP_BIOINDICEETAT,TEMP_BIOEQR)
	} else {
		RLT_BIOINDICE<-merge(TEMP_BIOMOY,TEMP_BIOEQR,by="STATION", all= TRUE)
		RLT_BIOINDICE<-merge(RLT_BIOINDICE,TEMP_BIOINDICE,by="STATION", all= TRUE)
		rm(TEMP_BIOMOY, TEMP_BIOINDICE, TEMP_BIOEQR)	
	}	
		
#### SIMULATION POUR LE 1/4 de classe biologique
# assouplissement de la bio si 1 seul indicateur est J alors que les autres B ou V
# il faut 3 indicateurs pr�sents minimum pour pouvoir assouplir. on ne tient pas compte de la PCH.
# l'indice de l'indicateur J doit etre dans le 1/4 sup�rieur de la classe J --> entre [55 et 60[
	if (SIMULQUARTCLASSE=="oui") {
		# calcul du nb d'indicateur bio pr�sent ; IBGA ne compte pas comme un indicateur bio, il est confondu avec IBG
		TEMPSIMUL<-RLT_BIO
		TEMPSIMUL$NBINDICBIO<-as.numeric(0)
		TEMPSIMUL$NBINDICBIO[!is.na(TEMPSIMUL$IBD)]<-TEMPSIMUL$NBINDICBIO[!is.na(TEMPSIMUL$IBD)]+1
		TEMPSIMUL$NBINDICBIO[!is.na(TEMPSIMUL$IPR)]<-TEMPSIMUL$NBINDICBIO[!is.na(TEMPSIMUL$IPR)]+1
		TEMPSIMUL$NBINDICBIO[!is.na(TEMPSIMUL$IBMR)]<-TEMPSIMUL$NBINDICBIO[!is.na(TEMPSIMUL$IBMR)]+1	
		if (BASSIN =="AESN"){ # AESN moyenne IBGA et IBG --> ligne sp�cifique sinon condidation marche pas
			TEMPSIMUL$NBINDICBIO[!is.na(TEMPSIMUL$IBG)]<-TEMPSIMUL$NBINDICBIO[!is.na(TEMPSIMUL$IBG)]+1 
		} else {
			TEMPSIMUL$NBINDICBIO[!is.na(TEMPSIMUL$IBG) | !is.na(TEMPSIMUL$IBGA)]<-TEMPSIMUL$NBINDICBIO[!is.na(TEMPSIMUL$IBG) | !is.na(TEMPSIMUL$IBGA)]+1 
		}
		
		# calcul du nb d'indicateur J quand l'�tat bio est J
		TEMPSIMUL$NBBIOJAUNE<-as.numeric(0)	
		TEMPSIMUL$NBBIOJAUNE[TEMPSIMUL$ETATBIO ==3 & TEMPSIMUL$IBD==3 & !is.na(TEMPSIMUL$IBD)]<-TEMPSIMUL$NBBIOJAUNE[TEMPSIMUL$ETATBIO ==3 & TEMPSIMUL$IBD==3 & !is.na(TEMPSIMUL$IBD)]+1
		TEMPSIMUL$NBBIOJAUNE[TEMPSIMUL$ETATBIO ==3 & TEMPSIMUL$IPR==3 & !is.na(TEMPSIMUL$IPR)]<-TEMPSIMUL$NBBIOJAUNE[TEMPSIMUL$ETATBIO ==3 & TEMPSIMUL$IPR==3 & !is.na(TEMPSIMUL$IPR)]+1
		TEMPSIMUL$NBBIOJAUNE[TEMPSIMUL$ETATBIO ==3 & TEMPSIMUL$IBMR==3 & !is.na(TEMPSIMUL$IBMR)]<-TEMPSIMUL$NBBIOJAUNE[TEMPSIMUL$ETATBIO ==3 & TEMPSIMUL$IBMR==3 & !is.na(TEMPSIMUL$IBMR)]+1
		if (BASSIN =="AESN"){ # AESN moyenne IBGA et IBG --> ligne sp�cifique sinon condidation marche pas
			TEMPSIMUL$NBBIOJAUNE[TEMPSIMUL$ETATBIO ==3 & TEMPSIMUL$IBG==3 & !is.na(TEMPSIMUL$IBG)]<-TEMPSIMUL$NBBIOJAUNE[TEMPSIMUL$ETATBIO ==3 & TEMPSIMUL$IBG==3 & !is.na(TEMPSIMUL$IBG)]+1
		} else {
			TEMPSIMUL$NBBIOJAUNE[TEMPSIMUL$ETATBIO ==3 & ((TEMPSIMUL$IBG==3 & !is.na(TEMPSIMUL$IBG)) | ((TEMPSIMUL$IBGA==3 & !is.na(TEMPSIMUL$IBGA))))]<-TEMPSIMUL$NBBIOJAUNE[TEMPSIMUL$ETATBIO ==3 & ((TEMPSIMUL$IBG==3 & !is.na(TEMPSIMUL$IBG)) | ((TEMPSIMUL$IBGA==3 & !is.na(TEMPSIMUL$IBGA))))]+1
		}
		
		# d�termine si quart de classe peut s'appliquer
		QUARTCLASSE<-merge(RLT_BIOINDICE[,c("STATION",iPARAMETREBIO_NAMES)],TEMPSIMUL,by="STATION",all.x=TRUE)
		condinitial<-QUARTCLASSE$ETATBIO==3 & QUARTCLASSE$NBINDICBIO>=3 & QUARTCLASSE$NBBIOJAUNE==1
		QUARTCLASSE$QUARTCLASSE<-as.character("non")
		QUARTCLASSE$QUARTCLASSE[condinitial & QUARTCLASSE$iIBD >=55 & QUARTCLASSE$iIBD <60 & !is.na(QUARTCLASSE$iIBD) ]<-"oui"
		QUARTCLASSE$QUARTCLASSE[condinitial & QUARTCLASSE$iIBG >=55 & QUARTCLASSE$iIBG <60 & !is.na(QUARTCLASSE$iIBG) ]<-"oui"
		QUARTCLASSE$QUARTCLASSE[condinitial & QUARTCLASSE$iIBGA >=55 & QUARTCLASSE$iIBGA <60 & !is.na(QUARTCLASSE$iIBGA) ]<-"oui" # if pas n�cessaire si bassin ==AESN
		QUARTCLASSE$QUARTCLASSE[condinitial & QUARTCLASSE$iIPR >=55 & QUARTCLASSE$iIPR <60 & !is.na(QUARTCLASSE$iIPR) ]<-"oui"
		QUARTCLASSE$QUARTCLASSE[condinitial & QUARTCLASSE$iIBMR >=55 & QUARTCLASSE$iIBMR <60 & !is.na(QUARTCLASSE$iIBMR) ]<-"oui"
		#rm(condinitial,TEMPSIMUL)
		
		# ajout de la colonne QUARTCLASSE au r�sultat
		RLT_BIOINDICE<-merge(RLT_BIOINDICE,QUARTCLASSE[,c("STATION","QUARTCLASSE")],by="STATION",all.x=TRUE)
		QUARTCLASSE_NAMES<-"QUARTCLASSE" # utile pour affiche colonne dans export
		}
} 

###########################################
## ETAT ECOLOGIQUE : INDICE (sp�cifique AESN)
###########################################
if (BASSIN =="AESN" & SEEEBIO=="oui" & SEEEPCH=="oui" ) {
	ETATECOLOINDICE<-merge(RLT_PCHINDICE[,c("STATION","iETATPCH")],RLT_BIOINDICE[,c("STATION","iETATBIO")],by="STATION",all.x=TRUE,all.y=TRUE)
	ETATECOLOINDICE$iETATECOLOHPS<-pmin(ETATECOLOINDICE$iETATPCH,ETATECOLOINDICE$iETATBIO,na.rm = TRUE) #na.rm permet de ne pas tenir compte des NA
	ETATECOLOHPSINDICE<-ETATECOLOINDICE[,c("STATION","iETATECOLOHPS")]
	iETATECOLOHPS_NAMES<-"iETATECOLOHPS"
}

###########################################
## POLLUANTS SPECIFIQUES : VALEUR MOYENNE
# Extraction uniquement de la valeur moyenne. 
# Indice calcul� � partir d'une formule transmise par AESN
###########################################
# principe retenu : valeur de l'indice = 60 si moyenne concentration = NQE. 
# valeur = 80 si NQE/10
# valeur = 100 si NQE/100 ou moins
# valeur = 40 si 10xNQE
# valeur = 20 si 100x NQE
# valeur= 0 si > 1000xNQE
# formule : indice = (log10(NQE/concent moyenne) + 3) x 20  avec les indices > 100 ou <0 ramen�s � 100 ou 0 

if (SEEEPS=="oui") {
	RLT_POLSPEVALEUR<-data.frame(RLT_POLSPE[,c("STATION")])
	names(RLT_POLSPEVALEUR)[1]<-"STATION"
	for (i in  1:nrow(PARAMETREPOLSPE)  ) {
		TEMPP<-POLSPE_MOY[POLSPE_MOY$PARAMETRE==PARAMETREPOLSPE$PARAMETRE[i],c("STATION", "MOY")]
		names(TEMPP)[2]<-paste("v",toupper(PARAMETREPOLSPE$PARAMETRELIB[i]), sep="")
		RLT_POLSPEVALEUR<-merge(RLT_POLSPEVALEUR,TEMPP,by="STATION", all.x=TRUE)
		rm(TEMPP)
	}
	
	# CALCUL DES INDICES (9/03/2016 Adaptation AESN)
	POLSPE_MOY$INDICEPOLSPE<-round((log10(POLSPE_MOY$NQEMA/POLSPE_MOY$MOY)+3)*20,3)
	POLSPE_MOY$INDICEPOLSPE[POLSPE_MOY$INDICEPOLSPE>100]<-100
	POLSPE_MOY$INDICEPOLSPE[POLSPE_MOY$INDICEPOLSPE<0]<-0

	#On met Vide si Etat ind�termin�
	POLSPE_MOY$iETATPS[POLSPE_MOY$CLASSEPS == 0]<-NA
	
	# Tableau pour les indices
	RLT_POLSPEINDICE<-data.frame(RLT_POLSPE[,"STATION"])
	names(RLT_POLSPEINDICE)[1]<-"STATION"
	for (i in  1:nrow(PARAMETREPOLSPE)  ) {
		TEMPPOLSPE<-POLSPE_MOY[POLSPE_MOY$PARAMETRE==PARAMETREPOLSPE$PARAMETRE[i],c("STATION", "INDICEPOLSPE")]
		names(TEMPPOLSPE)[2]<-paste ("i",toupper(PARAMETREPOLSPE$PARAMETRELIB[i]), sep="")
		RLT_POLSPEINDICE<-merge(RLT_POLSPEINDICE,TEMPPOLSPE,by="STATION", all.x=TRUE)
		rm(TEMPPOLSPE)
	}
	
		# Indice ETAT PS - demande sp�cifique AESN
		#Indice du parametre ayant le moins bon indice (MIN) hors �tat ind�termin�
	if (BASSIN=="AESN") {
		indPS<-as.matrix(RLT_POLSPEINDICE[,2:ncol(RLT_POLSPEINDICE)])
		MAT<-RLT_POLSPE[,5:ncol(RLT_POLSPE)]
		MAT[MAT > 0 & !is.na(MAT)]<-1
		MAT[MAT == 0 & !is.na(MAT)]<-NA
		indPS<-indPS * MAT
		cond<-RLT_POLSPE$ETATPS != 0 & !is.na(RLT_POLSPE$ETATPS)
		RLT_POLSPEINDICE$iETATPS<-as.numeric(NA)
		RLT_POLSPEINDICE$iETATPS[cond]<-apply(indPS[cond,],1, min, na.rm=TRUE)
		rm(indPS,MAT)
		iETATPS_NAMES<-"iETATPS"
	} 	
	gc()

	

	
	#On rassemble valeur et indice
	RLT_POLSPEINDICE<-merge(RLT_POLSPEVALEUR,RLT_POLSPEINDICE,by="STATION")
	
}

###########################################
## ETAT CHIMIQUE : VALEUR MOYENNE
# Extraction uniquement de la valeur moyenne. Indice non calcul� car non demand� par les clients pour le moment
###########################################

if (SEEECHIM=="oui") {
# Mise en forme du tableau avec les classes d'�tat & valeurs moyennes (MOY, MOYMIN, MOYMAX)
	RLT_CHIMCMAMA<-CHIMCMAMA[,c(2:(ncol(CHIMCMAMA)))]
	RLT_CHIMCMAMA<-merge(RLT_CHIMCMAMA, PARAGROUPCHIM[,c("PARAGROUP","PARAGROUPLIBCOURT","FAMILLE","PARAGROUPLIB","HAPUBI")],by="PARAGROUP")
	RLT_CHIMCMAMA<-RLT_CHIMCMAMA[,c("STATION","PARAGROUP","PARAGROUPLIBCOURT", "FAMILLE", "HAPUBI","QUANTIFEAN", "RESULTATMAX", "NQECMA","CLASSECMA", "MOY", "LQMAX","NQEMA","CLASSEMA", "CLASSEETAT","PARAGROUPLIB", "GROUPE")]
	RLT_CHIMCMAMA<-RLT_CHIMCMAMA[order(RLT_CHIMCMAMA$STATION,RLT_CHIMCMAMA$PARAGROUP),]
	
# 22/03/2016 PPL: Calcul d'un indice comme pour PS (si NA c'est normal et c'est corrig� par la suite gr�ce � l'argument na.ram = T)	
	RLT_CHIMCMAMA$IndiceCMA<-round((log10(RLT_CHIMCMAMA$NQECMA/RLT_CHIMCMAMA$RESULTATMAX)+3)*20,3)
	RLT_CHIMCMAMA$IndiceCMA[RLT_CHIMCMAMA$RESULTATMAX == 0 & RLT_CHIMCMAMA$CLASSECMA == 1]<-100
	
	RLT_CHIMCMAMA$IndiceMA<-round((log10(RLT_CHIMCMAMA$NQEMA/RLT_CHIMCMAMA$MOY)+3)*20,3)
	RLT_CHIMCMAMA$IndiceMA[RLT_CHIMCMAMA$MOY == 0 & RLT_CHIMCMAMA$CLASSEMA == 1]<-100

	RLT_CHIMCMAMA$IndiceChim<-pmin(RLT_CHIMCMAMA$IndiceMA,RLT_CHIMCMAMA$IndiceCMA, na.rm=TRUE)
	
	RLT_CHIMCMAMA$IndiceMA[RLT_CHIMCMAMA$IndiceMA>100]<-100
	RLT_CHIMCMAMA$IndiceMA[RLT_CHIMCMAMA$IndiceMA<0]<-0
	RLT_CHIMCMAMA$IndiceCMA[RLT_CHIMCMAMA$IndiceCMA>100]<-100
	RLT_CHIMCMAMA$IndiceCMA[RLT_CHIMCMAMA$IndiceCMA<0]<-0
	RLT_CHIMCMAMA$IndiceChim[RLT_CHIMCMAMA$IndiceChim>100]<-100
	RLT_CHIMCMAMA$IndiceChim[RLT_CHIMCMAMA$IndiceChim<0]<-0
	
	# On met vide si Ind�termin�
	RLT_CHIMCMAMA$IndiceChim[RLT_CHIMCMAMA$CLASSEETAT ==0 ]<-NA
	
	#calul de l'�tat chimique � la station (valaur minimale hors indetermin�)
	# on regarde dabord les parametres d�termin�s puis indetermin�s
	INDICECHIMSTAT_det<-aggregate(IndiceChim ~ STATION, data = RLT_CHIMCMAMA[RLT_CHIMCMAMA$CLASSEETAT != 0,], min, na.rm = TRUE)
	condindeterm<-RLT_CHIMCMAMA$CLASSEETAT ==0 &  !(RLT_CHIMCMAMA$STATION %in% INDICECHIMSTAT_det$STATION)
	if( nrow(RLT_CHIMCMAMA[condindeterm,])> 0) {
	INDICECHIMSTAT_indet<-data.frame(STATION = unique(RLT_CHIMCMAMA$STATION[condindeterm], IndiceChim = NA))
	INDICECHIMSTAT<-merge(INDICECHIMSTAT_det,INDICECHIMSTAT_indet, all = TRUE)
	rm(INDICECHIMSTAT_indet,INDICECHIMSTAT_det)
	} else {
	INDICECHIMSTAT<-INDICECHIMSTAT_det
	rm(INDICECHIMSTAT_det)
	}
	names(INDICECHIMSTAT)[2]<-"IndiceChim_Station"
	RLT_CHIMCMAMA<-merge(RLT_CHIMCMAMA,INDICECHIMSTAT,by="STATION",all.x = TRUE)
	gc()
	
	## m�me chose : On calcule un indice ss hubiquiste
	INDICECHIMSTAT_det<-aggregate(IndiceChim ~ STATION, data = RLT_CHIMCMAMA[RLT_CHIMCMAMA$CLASSEETAT != 0 & RLT_CHIMCMAMA$HAPUBI != "oui",], min, na.rm = TRUE)
	condindeterm<-RLT_CHIMCMAMA$CLASSEETAT ==0 &  !(RLT_CHIMCMAMA$STATION %in% INDICECHIMSTAT_det$STATION) & RLT_CHIMCMAMA$HAPUBI != "oui"
	if( nrow(RLT_CHIMCMAMA[condindeterm,])> 0) {
	INDICECHIMSTAT_indet<-data.frame(STATION = unique(RLT_CHIMCMAMA$STATION[condindeterm], IndiceChim = NA))
	INDICECHIMSTAT<-merge(INDICECHIMSTAT_det,INDICECHIMSTAT_indet, all = TRUE)
	rm(INDICECHIMSTAT_indet,INDICECHIMSTAT_det)
	} else {
	INDICECHIMSTAT<-INDICECHIMSTAT_det
	rm(INDICECHIMSTAT_det)
	}
	names(INDICECHIMSTAT)[2]<-"IndiceChim_Station_horsHAPUBI"
	RLT_CHIMCMAMA<-merge(RLT_CHIMCMAMA,INDICECHIMSTAT,by="STATION",all.x = TRUE)
	gc()
	
	
	## RAJOUT STATIONTXT	
	RLT_CHIMCMAMA_NAMES<-names(RLT_CHIMCMAMA)
	if (nchar(as.character(RLT_CHIMCMAMA$STATION))[1] == 7 & !is.na(RLT_CHIMCMAMA$STATION[1])) {
			RLT_CHIMCMAMA$STATIONTXT<-as.character(RLT_CHIMCMAMA$STATION)
			RLT_CHIMCMAMA$STATIONTXT[nchar(as.character(RLT_CHIMCMAMA$STATION)) == 7 & !is.na(RLT_CHIMCMAMA$STATION)]<-paste("0",RLT_CHIMCMAMA$STATION[nchar(as.character(RLT_CHIMCMAMA$STATION)) == 7 & !is.na(RLT_CHIMCMAMA$STATION)],sep="")
			RLT_CHIMCMAMA<-RLT_CHIMCMAMA[,c("STATIONTXT",RLT_CHIMCMAMA_NAMES)]
			RLT_CHIMCMAMA_NAMES<-names(RLT_CHIMCMAMA)
	}
	
	#On r�ordonne
	RLT_CHIMCMAMA<-RLT_CHIMCMAMA[order(RLT_CHIMCMAMA$STATION,RLT_CHIMCMAMA$PARAGROUP),]
}