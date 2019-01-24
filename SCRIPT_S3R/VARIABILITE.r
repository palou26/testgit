##############################################
## SEEE - COURS D'EAU : état physicochimique
##############################################

## script crée en Avril 2016

# fonction premeir et troisième quartile 
Q1<-function(X, na.rm=TRUE){
quantile(X,0.25, type=1, na.rm= na.rm)
}
Q3<-function(X, na.rm=TRUE){
quantile(X,0.75, type=1, na.rm= na.rm)
}


###########################
### PCH  ##################
###########################
if (SEEEPCH=="oui") {
	
	if (CEPE == "PE" & MODULE == "REEE2010") { 
		PCH<-DATAPCHPARAGROUP
		PCH$PARAMETRE<-PCH$PARAGROUP
	}
	
	if (CEPE == "PE" & MODULE == "REEE2010") { 
		PCH<-DATAPCH
	}

	
		#table de correspondance
	CORRESP_ID<-PCH[!duplicated(PCH[,c("STATION","PARAMETRE")]),c("STATION","PARAMETRE")]
	CORRESP_ID$ID<-paste(CORRESP_ID$STATION, CORRESP_ID$PARAMETRE, sep="-")
	PCH$ID<-paste(PCH$STATION, PCH$PARAMETRE, sep="-")
	RLT_PCH$ID<-paste(RLT_PCH$STATION, RLT_PCH$PARAMETRE, sep="-")
	
	#Calculs des min, Q1, mediane, Q3, max, Moyenne						
	VARIA_PCH<-data.frame(
					COMPARTIMENT = "PCH",
					ID = sort(unique(PCH$ID)), 
					MIN = round(tapply(PCH$RESULTAT, PCH$ID , min, na.rm = T),3),
					Q1 = round(tapply(PCH$RESULTAT, PCH$ID , Q1, na.rm = T),3),
					Mediane = round(tapply(PCH$RESULTAT, PCH$ID , median, na.rm = T),3),
					Q3 = round(tapply(PCH$RESULTAT, PCH$ID , Q3, na.rm = T),3),
					MAX = round(tapply(PCH$RESULTAT, PCH$ID , max, na.rm = T),3),
					MOYENNE = round(tapply(PCH$RESULTAT, PCH$ID , mean, na.rm = T),3),
					FREQ = round(tapply(PCH$RESULTAT, PCH$ID , length))
					)					
						
	names(VARIA_PCH)<-c("COMPARTIMENT","ID","MIN","Q1","Mediane","Q3","MAX","MOYENNE","FREQ")

	VARIA_PCH<-merge(VARIA_PCH,CORRESP_ID,by="ID",all.x = TRUE)
	VARIA_PCH<-merge(VARIA_PCH,RLT_PCH[,c("ID","RESULTAT")],by="ID")
	VARIA_PCH<-merge(VARIA_PCH,PARAMETREPCH[,c("PARAMETRE", "NOM")],by="PARAMETRE",all.x = TRUE)
	
	VARIA_PCH<-VARIA_PCH[,c("COMPARTIMENT","STATION","PARAMETRE","NOM","RESULTAT",  "MIN","Q1","Mediane" ,"Q3", "MAX" ,"MOYENNE" ,"FREQ" )]
	names(VARIA_PCH)<-c("COMPARTIMENT","STATION","PARAMETRE","PARAMETRELIB","P90_RESULTAT",  "MIN","Q1","Mediane" ,"Q3", "MAX" ,"MOYENNE" ,"FREQ")
	
	gc()
}


###########################
### BIO  ##################
###########################

if (SEEEBIO=="oui") {

	#table de correspondance
	CORRESP_ID<-DATABIO[!duplicated(DATABIO[,c("STATION","PARAGROUP")]),c("STATION","PARAGROUP")]
	CORRESP_ID$ID<-paste(CORRESP_ID$STATION, CORRESP_ID$PARAGROUP, sep="-")
	DATABIO$ID<-paste(DATABIO$STATION, DATABIO$PARAGROUP, sep="-")
	BIO_MOY$ID<-paste(BIO_MOY$STATION, BIO_MOY$PARAGROUP, sep="-")
	
	#Calculs des min, Q1, mediane, Q3, max, Moyenne											
		VARIA_BIO<-data.frame(
					COMPARTIMENT = "BIO",
					ID = sort(unique(DATABIO$ID)), 
					MIN = round(tapply(DATABIO$RESULTAT, DATABIO$ID , min, na.rm = T),3),
					Q1 = round(tapply(DATABIO$RESULTAT, DATABIO$ID , Q1, na.rm = T),3),
					Mediane = round(tapply(DATABIO$RESULTAT, DATABIO$ID , median, na.rm = T),3),
					Q3 = round(tapply(DATABIO$RESULTAT, DATABIO$ID , Q3, na.rm = T),3),
					MAX = round(tapply(DATABIO$RESULTAT, DATABIO$ID , max, na.rm = T),3),
					MOYENNE = round(tapply(DATABIO$RESULTAT, DATABIO$ID , mean, na.rm = T),3),
					FREQ = round(tapply(DATABIO$RESULTAT, DATABIO$ID , length))
					)					
										
	VARIA_BIO<-merge(VARIA_BIO,CORRESP_ID,by="ID",all.x = TRUE)
	VARIA_BIO<-merge(VARIA_BIO,BIO_MOY[,c("ID","RESULTAT")])
	VARIA_BIO<-merge(VARIA_BIO,TABLEBIO[,c("PARAGROUP", "PARALIBGROUP")],by="PARAGROUP",all.x = TRUE)
	
	VARIA_BIO<-VARIA_BIO[,c("COMPARTIMENT","STATION","PARAGROUP","PARALIBGROUP","RESULTAT",  "MIN","Q1","Mediane" ,"Q3", "MAX" ,"MOYENNE" ,"FREQ" )]
	names(VARIA_BIO)<-c("COMPARTIMENT","STATION","PARAMETRE","PARAMETRELIB","MOY_RESULTAT",  "MIN","Q1","Mediane" ,"Q3", "MAX" ,"MOYENNE" ,"FREQ")
	gc()

}

###########################
### PS  ##################
###########################
if (SEEEPS=="oui") {
	
	#table de correspondance
	CORRESP_ID<-POLSPE[!duplicated(POLSPE[,c("STATION","PARAMETRE")]),c("STATION","PARAMETRE")]
	CORRESP_ID$ID<-paste(CORRESP_ID$STATION, CORRESP_ID$PARAMETRE, sep="-")
	POLSPE$ID<-paste(POLSPE$STATION, POLSPE$PARAMETRE, sep="-")
	POLSPE_MOY$ID<-paste(POLSPE_MOY$STATION, POLSPE_MOY$PARAMETRE, sep="-")
	
#Calculs des min, Q1, mediane, Q3, max, Moyenne												
		VARIA_POLSPE<-data.frame(
					COMPARTIMENT = "POLSPE",
					ID = sort(unique(POLSPE$ID)), 
					MIN = round(tapply(POLSPE$RESULTAT, POLSPE$ID , min, na.rm = T),3),
					Q1 = round(tapply(POLSPE$RESULTAT, POLSPE$ID , Q1, na.rm = T),3),
					Mediane = round(tapply(POLSPE$RESULTAT, POLSPE$ID , median, na.rm = T),3),
					Q3 = round(tapply(POLSPE$RESULTAT, POLSPE$ID , Q3, na.rm = T),3),
					MAX = round(tapply(POLSPE$RESULTAT, POLSPE$ID , max, na.rm = T),3),
					MOYENNE = round(tapply(POLSPE$RESULTAT, POLSPE$ID , mean, na.rm = T),3),
					FREQ = round(tapply(POLSPE$RESULTAT, POLSPE$ID , length))
					)					
										
	VARIA_POLSPE<-merge(VARIA_POLSPE,CORRESP_ID,by="ID",all.x = TRUE)
	if (MODULE == "REEE2010") {POLSPE_MOY$RESULTAT<-POLSPE_MOY$MOY}
	VARIA_POLSPE<-merge(VARIA_POLSPE,POLSPE_MOY[,c("ID","RESULTAT")],by="ID")
	VARIA_POLSPE<-merge(VARIA_POLSPE,PARAMETREPOLSPE[,c("PARAMETRE", "PARAMETRELIB")],by="PARAMETRE",all.x = TRUE)
	
	head(VARIA_POLSPE)
	
	VARIA_POLSPE<-VARIA_POLSPE[,c("COMPARTIMENT","STATION","PARAMETRE","PARAMETRELIB","RESULTAT",  "MIN","Q1","Mediane" ,"Q3", "MAX" ,"MOYENNE" ,"FREQ" )]
	names(VARIA_POLSPE)<-c("COMPARTIMENT","STATION","PARAMETRE","PARAMETRELIB","MOY_RESULTAT",  "MIN","Q1","Mediane" ,"Q3", "MAX" ,"MOYENNE" ,"FREQ")

	gc()

	
}

###########################
### CHIM  ##################
###########################

if (SEEECHIM=="oui") {


#table de correspondance
	CORRESP_ID<-DATACHIM[!duplicated(DATACHIM[,c("STATION","PARAGROUP")]),c("STATION","PARAGROUP")]
	CORRESP_ID$ID<-paste(CORRESP_ID$STATION, CORRESP_ID$PARAGROUP, sep="-")
	DATACHIM$ID<-paste(DATACHIM$STATION, DATACHIM$PARAGROUP, sep="-")
	CHIMMA$ID<-paste(CHIMMA$STATION, CHIMMA$PARAGROUP, sep="-")
	CHIMCMAMA$ID<-paste(CHIMCMAMA$STATION, CHIMCMAMA$PARAGROUP, sep="-")
	
#Calculs des min, Q1, mediane, Q3, max, Moyenne						
		VARIA_CHIM<-data.frame(
					COMPARTIMENT = "CHIM",
					ID = sort(unique(DATACHIM$ID)), 
					MIN = round(tapply(DATACHIM$RESULTAT, DATACHIM$ID , min, na.rm = T),3),
					Q1 = round(tapply(DATACHIM$RESULTAT, DATACHIM$ID , Q1, na.rm = T),3),
					Mediane = round(tapply(DATACHIM$RESULTAT, DATACHIM$ID , median, na.rm = T),3),
					Q3 = round(tapply(DATACHIM$RESULTAT, DATACHIM$ID , Q3, na.rm = T),3),
					MAX = round(tapply(DATACHIM$RESULTAT, DATACHIM$ID , max, na.rm = T),3),
					MOYENNE = round(tapply(DATACHIM$RESULTAT, DATACHIM$ID , mean, na.rm = T),3),
					FREQ2 = round(tapply(DATACHIM$RESULTAT, DATACHIM$ID , length))
					)					
										
	VARIA_CHIM<-merge(VARIA_CHIM,CORRESP_ID,by="ID",all.x = TRUE)
	VARIA_CHIM<-merge(VARIA_CHIM,CHIMCMAMA[,c("ID","RESULTATMAX","MOY")],by="ID")
	VARIA_CHIM<-merge(VARIA_CHIM,CHIMMA[,c("ID","FREQ")],by="ID",all.x = TRUE)
	summary(VARIA_CHIM)
	
	VARIA_CHIM$FREQ[is.na(VARIA_CHIM$FREQ)]<-VARIA_CHIM$FREQ2[is.na(VARIA_CHIM$FREQ)]
	VARIA_CHIM<-merge(VARIA_CHIM,PARAGROUPCHIM[,c("PARAGROUP", "PARAGROUPLIBCOURT")],by="PARAGROUP",all.x = TRUE)
	
	head(VARIA_CHIM)
	
	VARIA_CHIM<-VARIA_CHIM[,c("COMPARTIMENT","STATION","PARAGROUP","PARAGROUPLIBCOURT","RESULTATMAX","MOY",  "MIN","Q1","Mediane" ,"Q3", "MAX" ,"MOYENNE" ,"FREQ" )]
	names(VARIA_CHIM)<-c("COMPARTIMENT","STATION","PARAMETRE","PARAMETRELIB","MAX_RESULTAT",  "MOY_RESULTAT","MIN","Q1","Mediane" ,"Q3", "MAX" ,"MOYENNE" ,"FREQ")
	VARIA_CHIM<-VARIA_CHIM[order(VARIA_CHIM$STATION,VARIA_CHIM$COMPARTIMENT,VARIA_CHIM$PARAMETRE),]

	gc()

}

##############################################
##On assemble les différents data.frame VARIA
##############################################

# Dataframe vide
VARIA<-data.frame(
					COMPARTIMENT = as.character(),
					STATION = as.character(),
					PARAMETRE = as.character(),
					PARAMETRELIB = as.character(),
					MOY_RESULTAT = as.numeric(),
					P90_RESULTAT = as.numeric(),
					MAX_RESULTAT = as.numeric(),
					MIN = as.numeric(),
					Q1 = as.numeric(),
					Mediane = as.numeric(),
					Q3 = as.numeric(),
					MAX = as.numeric(),
					MOYENNE = as.numeric(),
					FREQ= as.numeric()
					)
					
# on assememble					
if(exists("VARIA_BIO")){
VARIA<-merge(VARIA,VARIA_BIO,all=TRUE)
}					

					
if(exists("VARIA_PCH")){
VARIA<-merge(VARIA,VARIA_PCH,all=TRUE)
}
					
if(exists("VARIA_POLSPE")){
VARIA<-merge(VARIA,VARIA_POLSPE,all=TRUE)
}
					
#if(exists("VARIA_CHIM")){
#VARIA<-merge(VARIA,VARIA_CHIM,all=TRUE)
#}

# tri final
VARIA<-VARIA[order(VARIA$STATION,VARIA$COMPARTIMENT,VARIA$PARAMETRE),]




########################################
######## INDICATEUR DE FIABILITE #######
########################################



###PCH 
if (SEEEPCH=="oui" & CEPE=="CE") { #test uniquement dévoloppé pour la PCH 
	#PCHEXCEPT_R90<-PCHEXCEPT[PCHEXCEPT$RANG == PCHEXCEPT$R90,]


	PCHEXCEPTPRELEV<-PCHEXCEPT
	PCHCLASS<-PCHEXCEPT_R90

	PCHCLASS$CLASSPCHFINAL<-PCHCLASS$CLASSEPCH
	PCHEXCEPTPRELEV<-merge(PCHEXCEPTPRELEV[,c("IDTRI","STATION","PARAMETRE","CLASSEPCH")],PCHCLASS[,c("IDTRI","CLASSPCHFINAL")],by="IDTRI")
	PCHEXCEPTPRELEV$OK<-0
	PCHEXCEPTPRELEV$OK[PCHEXCEPTPRELEV$CLASSEPCH == PCHEXCEPTPRELEV$CLASSPCHFINAL]<-1

	FIABILITE_PCH<-data.frame(
						COMPARTIMENT = "PCH",
						IDTRI = sort(unique(PCHEXCEPTPRELEV$IDTRI)), 
						CLASSEPCH = round(tapply(PCHEXCEPTPRELEV$CLASSPCHFINAL, PCHEXCEPTPRELEV$IDTRI , mean)),
						NBPRELEV = round(tapply(PCHEXCEPTPRELEV$OK, PCHEXCEPTPRELEV$IDTRI , length)),
						NBCLASSOK = round(tapply(PCHEXCEPTPRELEV$OK, PCHEXCEPTPRELEV$IDTRI , sum))
						)					
	FIABILITE_PCH$pFIABILITE<-round(FIABILITE_PCH$NBCLASSOK * 100 / 	FIABILITE_PCH$NBPRELEV,1)
	FIABILITE_PCH$pFIABILITE[FIABILITE_PCH$CLASSEPCH == 0]<- (-1)
	FIABILITE_PCH$cFIABILITE[FIABILITE_PCH$pFIABILITE == (-1)]<- 0
	FIABILITE_PCH$cFIABILITE[FIABILITE_PCH$pFIABILITE > (-1)]<- 3
	FIABILITE_PCH$cFIABILITE[FIABILITE_PCH$pFIABILITE >= 50]<- 2
	FIABILITE_PCH$cFIABILITE[FIABILITE_PCH$pFIABILITE >= 75]<- 1
	#Pour éviter d’avoir « 100% » de fiabilité 
	#s’’il n’y a qu’un seul prélèvement, la classe de fiabilité est « 3 – peu fiable » , 
	#lorsque le nombre de prélèvements est strictement inférieur à 3. 
	FIABILITE_PCH$cFIABILITE[FIABILITE_PCH$NBPRELEV %in% c(1,2)  & FIABILITE_PCH$CLASSEPCH != 0]<- 3

	FIABILITE_PCH<-merge(FIABILITE_PCH,PCHCLASS[,c("IDTRI","STATION","PARAMETRE")],by= "IDTRI")
	FIABILITE_PCH<-FIABILITE_PCH[order(FIABILITE_PCH$STATION,FIABILITE_PCH$PARAMETRE),c("STATION","PARAMETRE","CLASSEPCH","NBPRELEV","NBCLASSOK","pFIABILITE","cFIABILITE")]


	PARAMETREPCH<-PARAMETREPCH[order(PARAMETREPCH$IDTRI),]
	RLT_pFIABILITE_PCH<-data.frame(STATION = sort(unique(PCHCLASS$STATION)))
	for (i in  1:nrow(PARAMETREPCH)) {
		TEMPP<-FIABILITE_PCH[FIABILITE_PCH$PARAMETRE==PARAMETREPCH$PARAMETRE[i],c("STATION", "pFIABILITE")]
		names(TEMPP)[2]<-paste0("p_",PARAMETREPCH$NOM[i])
		RLT_pFIABILITE_PCH<-merge(RLT_pFIABILITE_PCH,TEMPP,by="STATION", all.x=TRUE)
		rm(TEMPP)
	}


	RLT_FIABILITE_PCH<-RLT_pFIABILITE_PCH
	for (i in  1:nrow(PARAMETREPCH)) {
		TEMPP<-FIABILITE_PCH[FIABILITE_PCH$PARAMETRE==PARAMETREPCH$PARAMETRE[i],c("STATION", "cFIABILITE")]
		names(TEMPP)[2]<-paste0("c_",PARAMETREPCH$NOM[i])
		RLT_FIABILITE_PCH<-merge(RLT_FIABILITE_PCH,TEMPP,by="STATION", all.x=TRUE)
		rm(TEMPP)
	}


}










