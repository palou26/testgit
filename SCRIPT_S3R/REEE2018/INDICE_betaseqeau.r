##############################################
## SEEE - COURS D'EAU : état physicochimique
## Application de l'arrêté de janvier 2010
## script crée en mars 2013
## script modifié en décembre 2014 pour intégrer les règles du 2nd cycle

### ATTENTION #####################################################
## SCRIPT arrêté en cours de développement car nous n'avons pas les coef A et B pour certains paramètres
## script qui fonctionne mais résultat faux pour certains parametres à cause des coef A et B
## script non utilisé par S3R mais conservé au cas où agence retrouve les coef.

##############################################

##############################################
## CALCUL DES RANG 90 / VALEUR MOYENNE / INDICE (type SEQ-eau)

## PCH
# principe de calcul de l'indice
# (((résultat - valeur inf borne)*20) / (valeur sup borne - valeur inf borne)) + indice borne inf

### BIO
# principe de calcul de l'indice par ordre croissant (IBD, IBG, IBGA, IBMR)
# (((résultat - valeur inf borne)*20) / (valeur sup borne - valeur inf borne)) + indice borne inf

# principe de calcul de l'indice par ordre décroissant (IPR)
# (((valeur sup borne - résultat)*20) / (valeur sup borne - valeur inf borne)) + indice borne inf

### NOTA :
# La fonction "merge" retrie le vecteur. la commande "sort" ne semble pas fonctionner --> pourquoi ?
# on déclare donc les conditions "condXXX" après chaque "merge" pour s'assurer que le true/false créé avec les conditions est toujours dans le même ordre que le merge.
# sinon le résultat ne correspond pas à la condition.
# On aurait pu rester comme initialement, avec 1 condition pour les paramètres et 1 autre pour les classes et la combinaison des 2 donne la condition globale,
# mais comme on les déclarait avant les merge ça obligait à définir un ID pour tjs trier les enregistrements dans le même sens et faire un "order" après chaque "merge".
# on a jugé cette solution moins robuste que de répéter les conditions après chaque merge, même si ça engendre un code avec plus de lignes.


## MODIF 2nd cycle
## MISE EN FORME
PARAMETREPCHCOEF<-read.csv2(paste(ch_param,"PARAMETREPCH_INDICECOEFF.csv",sep=""))
PARAMETREPCHCOEF$PARAMETRE<-as.character(PARAMETREPCHCOEF$PARAMETRE)
PARAMETREPCHCOEF$NOM<-as.character(PARAMETREPCHCOEF$NOM)
PARAMETREPCHCOEF$COEFA_B<-as.numeric(as.character(PARAMETREPCHCOEF$COEFA_B))
PARAMETREPCHCOEF$COEFA_V<-as.numeric(as.character(PARAMETREPCHCOEF$COEFA_V))
PARAMETREPCHCOEF$COEFA_J<-as.numeric(as.character(PARAMETREPCHCOEF$COEFA_J))
PARAMETREPCHCOEF$COEFA_O<-as.numeric(as.character(PARAMETREPCHCOEF$COEFA_O))
PARAMETREPCHCOEF$COEFA_R<-as.numeric(as.character(PARAMETREPCHCOEF$COEFA_R))
PARAMETREPCHCOEF$COEFB_B<-as.numeric(as.character(PARAMETREPCHCOEF$COEFB_B))
PARAMETREPCHCOEF$COEFB_V<-as.numeric(as.character(PARAMETREPCHCOEF$COEFB_V))
PARAMETREPCHCOEF$COEFB_J<-as.numeric(as.character(PARAMETREPCHCOEF$COEFB_J))
PARAMETREPCHCOEF$COEFB_O<-as.numeric(as.character(PARAMETREPCHCOEF$COEFB_O))
PARAMETREPCHCOEF$COEFB_R<-as.numeric(as.character(PARAMETREPCHCOEF$COEFB_R))

##FIN MISE EN FORME


#########################################
# PCH INDICE : Paramètre ordre croissant
if (SEEEPCH=="oui") {
	RLT_PCH<-merge(RLT_PCH,PARAMETREPCH[,c("PARAMETRE", "SUPB", "INFB", "INFV", "INFJ", "INFO", "INFR")],by="PARAMETRE", all.x=TRUE, sort = FALSE)
	RLT_PCH<-merge(RLT_PCH,PARAMETREPCHCOEF,by="PARAMETRE", all.x=TRUE, sort = FALSE)
	
	# # Equation du SEQ-eau
	# # 3 conditions possibles pour 1ere equation (classe verte à orange)
	# EQUATION1v<-a[condPCH2]*C[condPCH2]+b[condPCH2]
	# EQUATION1j<-a[condPCH3]*C[condPCH3]+b[condPCH3]
	# EQUATION1o<-a[condPCH4]*C[condPCH4]+b[condPCH4]
	# # 1 condition possible pour 2eme équation (classe rouge)
	# EQUATION2<-a[condPCH5]*(C[condPCH5]^b[condPCH5])
	# # 1 condition possible pour 2eme équation (classe bleue)
	# EQUATION3<-100-(a[condPCH1]*(C[condPCH1]^b[condPCH1]))
	# # 1 condition possible pour 2eme équation (classe bleue pour pH)
	# EQUATION4<-a[condPCH1ph]*(P[condPCH1ph]-C[condPCH1ph])+b[condPCH1ph]
	
	COEFA<- function (DAT=RLT_PCH) {
		DAT$COEFA[condPCH1]<-DAT$COEFA_B[condPCH1]
		DAT$COEFA[condPCH1ph]<-DAT$COEFA_B[condPCH1ph] # cas du ph, même condition que cond1 mais traité à part pour cohérence dans traitement equation 4
		DAT$COEFA[condPCH2]<-DAT$COEFA_V[condPCH2]
		DAT$COEFA[condPCH3]<-DAT$COEFA_J[condPCH3]
		DAT$COEFA[condPCH4]<-DAT$COEFA_O[condPCH4]
		DAT$COEFA[condPCH5]<-DAT$COEFA_R[condPCH5]
		DAT
	}
	
	COEFB<- function (DAT=RLT_PCH) {
		DAT$COEFB[condPCH1]<-DAT$COEFB_B[condPCH1]
		DAT$COEFB[condPCH1ph]<-DAT$COEFB_B[condPCH1ph] # cas du ph, même condition que cond1 mais traité à part pour cohérence dans traitement equation 4
		DAT$COEFB[condPCH2]<-DAT$COEFB_V[condPCH2]
		DAT$COEFB[condPCH3]<-DAT$COEFB_J[condPCH3]
		DAT$COEFB[condPCH4]<-DAT$COEFB_O[condPCH4]
		DAT$COEFB[condPCH5]<-DAT$COEFB_R[condPCH5]
		DAT
	}
	
	# fonction pour calcul indice SeqEau	
	FORMULEINDICESEQ<- function (DAT=RLT_PCH) {
		DAT$INDICE[condPCH1]<-round(100-(a[condPCH1]*(C[condPCH1]^b[condPCH1])))  #EQUATION3
		DAT$INDICE[condPCH1ph]<-round(a[condPCH1ph]*(P-C[condPCH1ph])+b[condPCH1ph])  #EQUATION4 cas du pH
		DAT$INDICE[condPCH2]<-round(a[condPCH2]*C[condPCH2]+b[condPCH2]) #EQUATION1v
		DAT$INDICE[condPCH3]<-round(a[condPCH3]*C[condPCH3]+b[condPCH3]) #EQUATION1j
		DAT$INDICE[condPCH4]<-round(a[condPCH4]*C[condPCH4]+b[condPCH4]) #EQUATION1o
		DAT$INDICE[condPCH5]<-round(a[condPCH5]*(C[condPCH5]^b[condPCH5])) #EQUATION2
		#DAT$INDICE[DAT$INDICE>100 & DAT$CLASSEPCH>=1]<-100
		#DAT$INDICE[DAT$INDICE<0 & DAT$CLASSEPCH>=1]<-0
		DAT
	}

	# fonction pour calcul indice linéaire (écrit par asconit)
	FORMULEINDICE<- function (DAT=RLT_PCH) {
		DAT$INDICE[condPCH1]<-round((((DAT$RESULTAT[condPCH1]-DAT$INFB[condPCH1])*20)/(DAT$SUPB[condPCH1]-DAT$INFB[condPCH1]))+80) 
		DAT$INDICE[condPCH2]<-round((((DAT$RESULTAT[condPCH2]-DAT$INFV[condPCH2])*20)/(DAT$INFB[condPCH2]-DAT$INFV[condPCH2]))+60)
		DAT$INDICE[condPCH3]<-round((((DAT$RESULTAT[condPCH3]-DAT$INFJ[condPCH3])*20)/(DAT$INFV[condPCH3]-DAT$INFJ[condPCH3]))+40)
		DAT$INDICE[condPCH4]<-round((((DAT$RESULTAT[condPCH4]-DAT$INFO[condPCH4])*20)/(DAT$INFJ[condPCH4]-DAT$INFO[condPCH4]))+20)
		DAT$INDICE[condPCH5]<-round((((DAT$RESULTAT[condPCH5]-DAT$INFR[condPCH5])*20)/(DAT$INFO[condPCH5]-DAT$INFR[condPCH5]))+0)
		DAT$INDICE[DAT$INDICE>100 & DAT$CLASSEPCH>=1]<-100
		DAT$INDICE[DAT$INDICE<0 & DAT$CLASSEPCH>=1]<-0
		DAT
	}

	# A cause des NO3 en 3 classes, on est obligé de situer le résultat entre les bornes, plutot que d'utiliser la classe PCH pour définir la bonne formule
	# sinon l'indice renvoyé n'est pas conforme. Cela oblige à dissocier les paramètres par ordre croissant et décroissant, car les signes ne sont pas équivalents
	condPCH1<- RLT_PCH$PARAMETRE  %in% c("1313","1841","1433","1350","1335","1339","1340") & !is.na(RLT_PCH$CLASSEPCH) & RLT_PCH$RESULTAT<=RLT_PCH$INFB
	condPCH1ph<-RLT_PCH$PARAMETRE %in% c("1302max") & !is.na(RLT_PCH$CLASSEPCH) & RLT_PCH$RESULTAT<=RLT_PCH$INFB
	condPCH2<-RLT_PCH$PARAMETRE %in% c("1302max","1313","1841","1433","1350","1335","1339","1340") & !is.na(RLT_PCH$CLASSEPCH) & RLT_PCH$RESULTAT>RLT_PCH$INFB & RLT_PCH$RESULTAT<=RLT_PCH$INFV
	condPCH3<-RLT_PCH$PARAMETRE %in% c("1302max","1313","1841","1433","1350","1335","1339","1340") & !is.na(RLT_PCH$CLASSEPCH) & RLT_PCH$RESULTAT>RLT_PCH$INFV & RLT_PCH$RESULTAT<=RLT_PCH$INFJ
	condPCH4<-RLT_PCH$PARAMETRE %in% c("1302max","1313","1841","1433","1350","1335","1339","1340") & !is.na(RLT_PCH$CLASSEPCH) & RLT_PCH$RESULTAT>RLT_PCH$INFJ & RLT_PCH$RESULTAT<=RLT_PCH$INFO
	condPCH5<-RLT_PCH$PARAMETRE %in% c("1302max","1313","1841","1433","1350","1335","1339","1340") & !is.na(RLT_PCH$CLASSEPCH) & RLT_PCH$RESULTAT>RLT_PCH$INFO # & RLT_PCH$RESULTAT<=RLT_PCH$INFR  # partie plus utile avec formule seqeau
	
	# attributation des coef par parametres
	RLT_PCH<-COEFA(DAT = RLT_PCH)
	RLT_PCH<-COEFB(DAT = RLT_PCH)
	
	# déclaration valeur pour équation
	C<-RLT_PCH$RESULTAT # valeur au R90
	a<-RLT_PCH$COEFA
	b<-RLT_PCH$COEFB
	# Déclaration de P pour pH max avant calcul
	P<-8.2 # valeur du pH max
	
	# calcul de l'indice pour ordre croissant
	RLT_PCH<-FORMULEINDICESEQ(DAT = RLT_PCH)
	
	# extraction des indices extremes pour verif résultat !! erreur à corriger !!!!
	verif<-RLT_PCH[RLT_PCH$INDICE< 0 & RLT_PCH$INDICE != -999  & !(is.na(RLT_PCH$INDICE)) ,c("STATION","PARAMETRE","RESULTAT", "CLASSEPCH","INDICE", "COEFA", "COEFB")]
	verif2<-RLT_PCH[RLT_PCH$INDICE> 100 & !(is.na(RLT_PCH$INDICE)) ,c("STATION","PARAMETRE","RESULTAT", "CLASSEPCH","INDICE", "COEFA", "COEFB")]
	verif3<-RLT_PCH[RLT_PCH$INDICE==-999 & !(is.na(RLT_PCH$INDICE)) ,c("STATION","PARAMETRE","RESULTAT", "CLASSEPCH","INDICE", "COEFA", "COEFB")]
	verif3<-RLT_PCH[RLT_PCH$INDICE==-999 & !(is.na(RLT_PCH$INDICE)) ,c("STATION","PARAMETRE","RESULTAT", "CLASSEPCH","INDICE")]
	verifNA<-RLT_PCH[is.na(RLT_PCH$INDICE),c("STATION","PARAMETRE","RESULTAT", "CLASSEPCH","INDICE","COEFA", "COEFB")]
	verifPH<-RLT_PCH[RLT_PCH$PARAMETRE=="1302max",c("STATION","PARAMETRE","RESULTAT", "CLASSEPCH","INDICE", "COEFA", "COEFB")]
	
	verifindiceseq<-read.csv2("D:/S3Rlocal - 2nd cycle/S3R1.0/IndiceTS_RESOS_2013.csv",sep=";")
	verifindiceseq<-verifindiceseq[verifindiceseq$Periode=="Année 2013",c("Code.station","PI_ACID","PI_AZOT","PI_MOOX","PI_PHOS","PI_TEMP","Periode")]
	veriftot<-merge(RLT_PCH[,c("STATION","PARAMETRE","RESULTAT","CLASSEPCH","INDICE")],verifindiceseq,by.x="STATION",by.y="Code.station", x.all=TRUE)
	veriftot[veriftot$STATION=="4000100",]
	
	rm(condPCH1, condPCH2, condPCH3, condPCH4, condPCH5)
	flush.console() 

# PCH INDICE : Paramètre ordre décroissant
# A cause des NO3 en 3 classes, on est obligé de situer le résultat entre les bornes, plutot que d'utiliser la classe PCH pour définir la bonne formule
# sinon l'indice renvoyé n'est pas conforme. Cela oblige à dissocier les paramètres par ordre croissant et décroissant, car les signes ne sont pas équivalents
	condPCH1<-RLT_PCH$PARAMETRE %in% c("1311", "1312") & !is.na(RLT_PCH$CLASSEPCH) & RLT_PCH$RESULTAT>=RLT_PCH$INFB
	condPCH1ph<-RLT_PCH$PARAMETRE %in% c("1302min") & !is.na(RLT_PCH$CLASSEPCH) & RLT_PCH$RESULTAT>=RLT_PCH$INFB
	condPCH2<-RLT_PCH$PARAMETRE %in% c("1311", "1312", "1302min") & !is.na(RLT_PCH$CLASSEPCH) & RLT_PCH$RESULTAT>=RLT_PCH$INFV & RLT_PCH$RESULTAT<RLT_PCH$INFB
	condPCH3<-RLT_PCH$PARAMETRE %in% c("1311", "1312", "1302min") & !is.na(RLT_PCH$CLASSEPCH) & RLT_PCH$RESULTAT>=RLT_PCH$INFJ & RLT_PCH$RESULTAT<RLT_PCH$INFV
	condPCH4<-RLT_PCH$PARAMETRE %in% c("1311", "1312", "1302min") & !is.na(RLT_PCH$CLASSEPCH) & RLT_PCH$RESULTAT>=RLT_PCH$INFO & RLT_PCH$RESULTAT<RLT_PCH$INFJ
	condPCH5<-RLT_PCH$PARAMETRE %in% c("1311", "1312", "1302min") & !is.na(RLT_PCH$CLASSEPCH) & RLT_PCH$RESULTAT>=RLT_PCH$INFR & RLT_PCH$RESULTAT<RLT_PCH$INFO

	# attributation des coef par parametres
	RLT_PCH<-COEFA(DAT = RLT_PCH)
	RLT_PCH<-COEFB(DAT = RLT_PCH)
	
	# déclaration valeur pour équation, on les répete --> pquoi ? ne sais pas ! car ils ne sont pas supprimés précédemment
	C<-RLT_PCH$RESULTAT # valeur au R90
	a<-RLT_PCH$COEFA
	b<-RLT_PCH$COEFB
	# Déclaration de P pour pH min avant calcul
	P<-6.5 # valeur du pH min
	
	# calcul de l'indice pour ordre décroissant	
	RLT_PCH<-FORMULEINDICESEQ(DAT = RLT_PCH)
	rm(condPCH1, condPCH2, condPCH3, condPCH4, condPCH5)

	RLT_PCH<-RLT_PCH[,c("IDTRI","STATION", "DATEPRELEV", "HEUREPRELEV","PARAMETRE", "FRACTION", "RESULTAT", "REMARQUE", "CLASSEPCH", "INDICE", "RANG", "Freq", "R90")]
	RLT_PCH<-merge(RLT_PCH, STATION, by="STATION")
	flush.console()
	
# PCH INDICE : Température (prise en compte du domaine)
	RLT_PCH_1301<-RLT_PCH[(RLT_PCH$PARAMETRE=="1301" & RLT_PCH$CONTEXTE_PISCICOLE %in% c("salmonicole", "intermediaire", "cyprinicole") & !is.na(RLT_PCH$CONTEXTE_PISCICOLE)) ,]
	RLT_PCH_1301na<-RLT_PCH[RLT_PCH$PARAMETRE=="1301" & is.na(RLT_PCH$CONTEXTE_PISCICOLE),] #sert uniquement dans le rbinb pour recréer le tableau final
	RLT_PCH_AUTRE<-RLT_PCH[!(RLT_PCH$PARAMETRE=="1301"),] #sert uniquement dans le rbinb pour recréer le tableau final

	RLT_PCH_1301$PARATEMP<-paste(RLT_PCH_1301$PARAMETRE,RLT_PCH_1301$CONTEXTE_PISCICOLE,sep="")
	PARAMETREPCHEXCEPT$PARATEMP<-paste(PARAMETREPCHEXCEPT$PARAMETRE,PARAMETREPCHEXCEPT$CONTEXTE_PISCICOLE,sep="")
	RLT_PCH_1301<-merge(RLT_PCH_1301, PARAMETREPCHEXCEPT[,c("PARATEMP", "SUPB", "INFB", "INFV", "INFJ", "INFO", "INFR")], by="PARATEMP")

	condPCH1<-RLT_PCH_1301$CLASSEPCH==1
	condPCH2<-RLT_PCH_1301$CLASSEPCH==2
	condPCH3<-RLT_PCH_1301$CLASSEPCH==3
	condPCH4<-RLT_PCH_1301$CLASSEPCH==4
	condPCH5<-RLT_PCH_1301$CLASSEPCH==5

	RLT_PCH_1301<-FORMULEINDICE(DAT = RLT_PCH_1301)
	rm(condPCH1, condPCH2, condPCH3, condPCH4, condPCH5)

	RLT_PCH_1301<-RLT_PCH_1301[,c("IDTRI","STATION", "DATEPRELEV", "HEUREPRELEV","PARAMETRE", "FRACTION", "RESULTAT", "REMARQUE", "CLASSEPCH", "INDICE", "RANG", "Freq", "R90")]
	RLT_PCH_1301<-merge(RLT_PCH_1301, STATION, by="STATION")
	RLT_PCH<-rbind(RLT_PCH_1301,RLT_PCH_1301na,RLT_PCH_AUTRE)
	rm(RLT_PCH_1301, RLT_PCH_1301na, RLT_PCH_AUTRE)
	flush.console()
	
	verif<-RLT_PCH[RLT_PCH$INDICE< 0 & RLT_PCH$INDICE != -999  & !(is.na(RLT_PCH$INDICE)) ,c("STATION","PARAMETRE","RESULTAT", "CLASSEPCH","INDICE")]
	verif2<-RLT_PCH[RLT_PCH$INDICE> 100 & !(is.na(RLT_PCH$INDICE)) ,c("STATION","PARAMETRE","RESULTAT", "CLASSEPCH","INDICE")]
	verif3<-RLT_PCH[RLT_PCH$INDICE==-999 & !(is.na(RLT_PCH$INDICE)) ,c("STATION","PARAMETRE","RESULTAT", "CLASSEPCH","INDICE")]
	verifNA<-RLT_PCH[is.na(RLT_PCH$INDICE),c("STATION","PARAMETRE","RESULTAT", "CLASSEPCH","INDICE")]
	
	
	
# TABLEAU FINAL : mise en forme
	PARAMETREPCH<-PARAMETREPCH[order(PARAMETREPCH$IDTRI),]
	
	# Tableau pour les valeurs au rang 90
	PCHR90<-data.frame(PCHEXCEPT_STATION[,c("STATION")])
	names(PCHR90)[1]<-"STATION"
	for (i in  1:nrow(PARAMETREPCH)) {
		TEMPP<-RLT_PCH[RLT_PCH$PARAMETRE==PARAMETREPCH$PARAMETRE[i],c("STATION","RESULTAT")]
		names(TEMPP)[2]<-paste("v",toupper(PARAMETREPCH$NOM[i]), sep="")
		PCHR90<-merge(PCHR90,TEMPP,by="STATION", all.x=TRUE)
		rm(TEMPP)
	}	
	# Tableau pour les indices
	PCHINDICE<-data.frame(PCHEXCEPT_STATION[,c("STATION")])
	names(PCHINDICE)[1]<-"STATION"
	for (i in  1:nrow(PARAMETREPCH)) {
		TEMPP<-RLT_PCH[RLT_PCH$PARAMETRE==PARAMETREPCH$PARAMETRE[i],c("STATION","INDICE")]
		names(TEMPP)[2]<-paste("i",toupper(PARAMETREPCH$NOM[i]), sep="")
		PCHINDICE<-merge(PCHINDICE,TEMPP,by="STATION", all.x=TRUE)
		rm(TEMPP)
	}	
	
	RLT_PCHINDICE<-merge(PCHR90, PCHINDICE,by="STATION")
	rm(PCHR90, PCHINDICE)	
}

######################################
## BIOLOGIE : VALEUR MOYENNE & INDICE 
######################################
##CALCUL A REVOIR CAR LES VALEURS DES GRILLES ONT CHANGE !!!!!!!!!!!!!

if (SEEEBIO=="oui") {
	BIO_MOY$INDICEBIO<-as.numeric("")
#calcul indice IBD
	BIO_MOY<-merge(BIO_MOY,GRILLEIBD,by="TYPEFR", all.x=TRUE, sort = FALSE)
	condIBD1<-BIO_MOY$PARAGROUP=="5856" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==1
	condIBD2<-BIO_MOY$PARAGROUP=="5856" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==2
	condIBD3<-BIO_MOY$PARAGROUP=="5856" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==3
	condIBD4<-BIO_MOY$PARAGROUP=="5856" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==4
	condIBD5<-BIO_MOY$PARAGROUP=="5856" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==5

	BIO_MOY$INDICEBIO[condIBD1]<-round((((BIO_MOY$RESULTAT[condIBD1]-BIO_MOY$INFB[condIBD1])*20)/(20-BIO_MOY$INFB[condIBD1]))+80) 
	BIO_MOY$INDICEBIO[condIBD2]<-round((((BIO_MOY$RESULTAT[condIBD2]-BIO_MOY$INFV[condIBD2])*20)/(BIO_MOY$INFB[condIBD2]-BIO_MOY$INFV[condIBD2]))+60)
	BIO_MOY$INDICEBIO[condIBD3]<-round((((BIO_MOY$RESULTAT[condIBD3]-BIO_MOY$INFJ[condIBD3])*20)/(BIO_MOY$INFV[condIBD3]-BIO_MOY$INFJ[condIBD3]))+40)
	BIO_MOY$INDICEBIO[condIBD4]<-round((((BIO_MOY$RESULTAT[condIBD4]-BIO_MOY$INFO[condIBD4])*20)/(BIO_MOY$INFJ[condIBD4]-BIO_MOY$INFO[condIBD4]))+20)
	BIO_MOY$INDICEBIO[condIBD5]<-round((((BIO_MOY$RESULTAT[condIBD5]-BIO_MOY$INFR[condIBD5])*20)/(BIO_MOY$INFO[condIBD5]-BIO_MOY$INFR[condIBD5]))+0)
	BIO_MOY<-BIO_MOY[,c("STATION", "TYPEFR", "PARAGROUP", "RESULTAT", "CLASSEBIO", "INDICEBIO" )]
	rm(condIBD1, condIBD2, condIBD3, condIBD4, condIBD5)

#calcul indice IBG & IBGA
	BIO_MOY<-merge(BIO_MOY,GRILLEIBG,by="TYPEFR", all.x=TRUE, sort = FALSE)
	condIBG1<-BIO_MOY$PARAGROUP %in% c("5910","6951") & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==1
	condIBG2<-BIO_MOY$PARAGROUP %in% c("5910","6951") & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==2
	condIBG3<-BIO_MOY$PARAGROUP %in% c("5910","6951") & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==3
	condIBG4<-BIO_MOY$PARAGROUP %in% c("5910","6951") & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==4
	condIBG5<-BIO_MOY$PARAGROUP %in% c("5910","6951") & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==5

	BIO_MOY$INDICEBIO[condIBG1]<-round((((BIO_MOY$RESULTAT[condIBG1]-BIO_MOY$INFB[condIBG1])*20)/(20-BIO_MOY$INFB[condIBG1]))+80) 
	BIO_MOY$INDICEBIO[condIBG2]<-round((((BIO_MOY$RESULTAT[condIBG2]-BIO_MOY$INFV[condIBG2])*20)/(BIO_MOY$INFB[condIBG2]-BIO_MOY$INFV[condIBG2]))+60)
	BIO_MOY$INDICEBIO[condIBG3]<-round((((BIO_MOY$RESULTAT[condIBG3]-BIO_MOY$INFJ[condIBG3])*20)/(BIO_MOY$INFV[condIBG3]-BIO_MOY$INFJ[condIBG3]))+40)
	BIO_MOY$INDICEBIO[condIBG4]<-round((((BIO_MOY$RESULTAT[condIBG4]-BIO_MOY$INFO[condIBG4])*20)/(BIO_MOY$INFJ[condIBG4]-BIO_MOY$INFO[condIBG4]))+20)
	BIO_MOY$INDICEBIO[condIBG5]<-round((((BIO_MOY$RESULTAT[condIBG5]-BIO_MOY$INFR[condIBG5])*20)/(BIO_MOY$INFO[condIBG5]-BIO_MOY$INFR[condIBG5]))+0)
	BIO_MOY<-BIO_MOY[,c("STATION", "TYPEFR", "PARAGROUP", "RESULTAT", "CLASSEBIO", "INDICEBIO" )]
	rm(condIBG1, condIBG2, condIBG3, condIBG4, condIBG5)
	
#calcul indice IPR
############################################################
##ADAPTER LE CALCUL AVEC NOUVELLE GRILLE + BORNE V POUR ALTI
	condIPR1<-BIO_MOY$PARAGROUP=="7036" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==1
	condIPR2<-BIO_MOY$PARAGROUP=="7036" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==2
	condIPR3<-BIO_MOY$PARAGROUP=="7036" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==3
	condIPR4<-BIO_MOY$PARAGROUP=="7036" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==4
	condIPR5<-BIO_MOY$PARAGROUP=="7036" & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==5

	BIO_MOY$INDICEBIO[condIPR1]<-round((((7-BIO_MOY$RESULTAT[condIPR1])*20)/(7-0))+80)
	BIO_MOY$INDICEBIO[condIPR2]<-round((((16-BIO_MOY$RESULTAT[condIPR2])*20)/(16-7))+60)
	BIO_MOY$INDICEBIO[condIPR3]<-round((((25-BIO_MOY$RESULTAT[condIPR3])*20)/(25-16))+40)
	BIO_MOY$INDICEBIO[condIPR4]<-round((((36-BIO_MOY$RESULTAT[condIPR4])*20)/(36-25))+20)
	BIO_MOY$INDICEBIO[condIPR5]<-round((((45-BIO_MOY$RESULTAT[condIPR5])*20)/(45-36))+0)
	BIO_MOY$INDICEBIO[BIO_MOY$RESULTAT>45]<-0
	rm(condIPR1, condIPR2, condIPR3, condIPR4, condIPR5)
	flush.console()

#calcul indice IBMR : ajouté le 10/12/14
	BIO_MOY<-merge(BIO_MOY,GRILLEIBMR,by="TYPEFR", all.x=TRUE, sort = FALSE)
	condIBMR1<-BIO_MOY$PARAGROUP %in% c("2928") & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==1
	condIBMR2<-BIO_MOY$PARAGROUP %in% c("2928") & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==2
	condIBMR3<-BIO_MOY$PARAGROUP %in% c("2928") & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==3
	condIBMR4<-BIO_MOY$PARAGROUP %in% c("2928") & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==4
	condIBMR5<-BIO_MOY$PARAGROUP %in% c("2928") & !is.na(BIO_MOY$CLASSEBIO) & BIO_MOY$CLASSEBIO==5

	BIO_MOY$INDICEBIO[condIBMR1]<-round((((BIO_MOY$RESULTAT[condIBMR1]-BIO_MOY$INFB[condIBMR1])*20)/(20-BIO_MOY$INFB[condIBMR1]))+80) 
	BIO_MOY$INDICEBIO[condIBMR2]<-round((((BIO_MOY$RESULTAT[condIBMR2]-BIO_MOY$INFV[condIBMR2])*20)/(BIO_MOY$INFB[condIBMR2]-BIO_MOY$INFV[condIBMR2]))+60)
	BIO_MOY$INDICEBIO[condIBMR3]<-round((((BIO_MOY$RESULTAT[condIBMR3]-BIO_MOY$INFJ[condIBMR3])*20)/(BIO_MOY$INFV[condIBMR3]-BIO_MOY$INFJ[condIBMR3]))+40)
	BIO_MOY$INDICEBIO[condIBMR4]<-round((((BIO_MOY$RESULTAT[condIBMR4]-BIO_MOY$INFO[condIBMR4])*20)/(BIO_MOY$INFJ[condIBMR4]-BIO_MOY$INFO[condIBMR4]))+20)
	BIO_MOY$INDICEBIO[condIBMR5]<-round((((BIO_MOY$RESULTAT[condIBMR5]-BIO_MOY$INFR[condIBMR5])*20)/(BIO_MOY$INFO[condIBMR5]-BIO_MOY$INFR[condIBMR5]))+0)
	BIO_MOY<-BIO_MOY[,c("STATION", "TYPEFR", "PARAGROUP", "RESULTAT", "CLASSEBIO", "INDICEBIO" )]
	rm(condIBMR1, condIBMR2, condIBMR3, condIBMR4, condIBMR5)

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
	
	# Tableau pour les indices
	TEMP_BIOINDICE<-data.frame(RLT_BIO[,"STATION"])
	names(TEMP_BIOINDICE)[1]<-"STATION"
	for (i in  1:nrow(TABLEBIO)  ) {
		TEMPBIO<-BIO_MOY[BIO_MOY$PARAGROUP==TABLEBIO$PARAGROUP[i],c("STATION", "INDICEBIO")]
		names(TEMPBIO)[2]<-paste ("i",TABLEBIO$PARALIBGROUP[i], sep="")
		TEMP_BIOINDICE<-merge(TEMP_BIOINDICE,TEMPBIO,by="STATION", all.x=TRUE)
		rm(TEMPBIO)
	}
		
	RLT_BIOINDICE<-merge(TEMP_BIOMOY,TEMP_BIOINDICE,by="STATION")
	rm(TEMP_BIOMOY, TEMP_BIOINDICE)	

}

###########################################
## POLLUANTS SPECIFIQUES : VALEUR MOYENNE
# Extraction uniquement de la valeur moyenne. Indice non calculé car non demandé par les clients pour le moment
###########################################
if (SEEEPS=="oui") {
	RLT_POLSPEINDICE<-data.frame(RLT_POLSPE[,c("STATION")])
	names(RLT_POLSPEINDICE)[1]<-"STATION"
	for (i in  1:nrow(PARAMETREPOLSPE)  ) {
		TEMPP<-POLSPE_MOY[POLSPE_MOY$PARAMETRE==PARAMETREPOLSPE$PARAMETRE[i],c("STATION", "RESULTATMOY")]
		names(TEMPP)[2]<-paste("v",toupper(PARAMETREPOLSPE$PARAMETRELIB[i]), sep="")
		RLT_POLSPEINDICE<-merge(RLT_POLSPEINDICE,TEMPP,by="STATION", all.x=TRUE)
		rm(TEMPP)
	}
}

###########################################
## ETAT CHIMIQUE : VALEUR MOYENNE
# Extraction uniquement de la valeur moyenne. Indice non calculé car non demandé par les clients pour le moment
###########################################
if (SEEECHIM=="oui") {
# Mise en forme du tableau avec les classes d'état & valeurs moyennes (MOY, MOYMIN, MOYMAX)
	RLT_CHIMCMAMA<-CHIMCMAMA[,c(2:(ncol(CHIMCMAMA)))]
	RLT_CHIMCMAMA<-merge(RLT_CHIMCMAMA, PARAGROUPCHIM,by="PARAGROUP")
	RLT_CHIMCMAMA<-RLT_CHIMCMAMA[,c("STATION","PARAGROUP","PARAGROUPLIBCOURT", "FAMILLE", "QUANTIFEAN", "RESULTATMAX", "NQECMA","CLASSECMA", "MOY", "MOYMIN", "MOYMAX", "LQMAX","NQEMA","CLASSEMA","CLASSEMARQ", "CLASSEETAT","PARAGROUPLIB", "GROUPE")]
	RLT_CHIMCMAMA<-RLT_CHIMCMAMA[order(RLT_CHIMCMAMA$STATION,RLT_CHIMCMAMA$PARAGROUP),]
	
	## RAJOUT STATIONTXT	
	RLT_CHIMCMAMA_NAMES<-names(RLT_CHIMCMAMA)
	if (nchar(as.character(RLT_CHIMCMAMA$STATION))[1] == 7 & !is.na(RLT_CHIMCMAMA$STATION[1])) {
			RLT_CHIMCMAMA$STATIONTXT<-as.character(RLT_CHIMCMAMA$STATION)
			RLT_CHIMCMAMA$STATIONTXT[nchar(as.character(RLT_CHIMCMAMA$STATION)) == 7 & !is.na(RLT_CHIMCMAMA$STATION)]<-paste("0",RLT_CHIMCMAMA$STATION[nchar(as.character(RLT_CHIMCMAMA$STATION)) == 7 & !is.na(RLT_CHIMCMAMA$STATION)],sep="")
			RLT_CHIMCMAMA<-RLT_CHIMCMAMA[,c("STATIONTXT",RLT_CHIMCMAMA_NAMES)]
			RLT_CHIMCMAMA_NAMES<-names(RLT_CHIMCMAMA)
	}

	
	
}
