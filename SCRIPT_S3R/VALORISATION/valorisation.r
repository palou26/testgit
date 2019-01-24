##############MODULE DE VALORISATION


###OPTION D'EXPORT Par défaut
if(!exists("EXPORTGRAPH")) { EXPORTGRAPH<- "AUCUN" }
#liste des etat et elements de qualité
ETATELTSQUAL<-c( "ETATECOLO", "ETATBIO","ETATPCH","ETATPS","IBD","IBG","IPR","BILANO2","NUT","TEMP","ACID","NONSYNTH","SYNTH","NIVEAU","CAS","TRANSPA")
if (MODULE != "REEE2010" ) {ETATELTSQUAL <-c(ETATELTSQUAL,"IBMR")}

COULEURCAS<-c("aquamarine4","coral2","bisque4","blue4","brown1","chartreuse3","brown4","chartreuse3","cadetblue","chocolate1")



###########################################################
##### CREATION DES GRAPHS ET DU FICHIER EXCEL D'AGGREGATION
###########################################################


if (EXPORTGRAPH != "AUCUN" ) {

	##############
	##### STATIONS
	##############

	##Tableau Valorisation_Station.xls vide
	DATAAGG_ALL<-data.frame(
							ECHELLESTA = as.character(),
							PARAMETAT = as.character(),
							ETAT = as.character(), 
							Freq = as.numeric(),
							PART = as.numeric()
							)


	if (SEEEBIO=="oui" | SEEEPCH=="oui" | SEEEPS=="oui") {

		#creation d'un repertoire "VALORISATION"
		CH_VALO<-paste0(CH_OUTPUT,"VALORISATION")
		dir.create(CH_VALO)
		XLS<-paste0(CH_VALO,"/Valorisation_Station.xls")

		#creation d'un repertoire "STATIONS"
		CH_VALOSTAT<-paste0(CH_VALO,"/STATIONS")
		dir.create(CH_VALOSTAT)
		
		 NAMESFINAL<-unique(names(RLT_FINALSTATION))
		 NAMESFINAL<-subset(NAMESFINAL,!(NAMESFINAL%in% c("STATION","STATIONTXT" ,"CONTEXTE_PISCICOLE" , "TYPEFR" , 
		 "EUCD"  , "REPRESENTATIVE","EXCEPT_FROID",
		 "EXCEPT_CHAUD" ,"EXCEPT_ACID" , "EXCEPT_MO" ,"EXCEPT_TOURB" , "EXCEPT_O2" ,
		 "ECHELLESTA", "ALTITUDE","ASSOUPLI","MODELISE","FONDGEO_ZINC","FONDGEO_CUIVRE","FONDGEO_CHROME","FONDGEO_ARSENIC",
		 "DURETE","LIBELLE",   "LISTINDICBIO" ,"N_PCHASSOUP", "N_PCHDECLASS" , "N_PSDECLASS","NBINDICBIO","PCHDECLASS",
		 "PCHDECLASSASSOUP","PSDECLASS","TYPESTAT","BIODECLASS","N_BIODECLASS","N_ECO_ORIGINDECLASS","N_ECODECLASS",names(STATION)
		 
		 )) )

		UNIK_ECHELLE<-c("TTES_STATIONS",sort(unique(RLT_FINALSTATION$ECHELLESTA)))
		UNIK_ECHELLE<-gsub(" ","",UNIK_ECHELLE)
		UNIK_ECHELLE<-gsub("[:?:]","_",UNIK_ECHELLE)
		UNIK_ECHELLE<-gsub(" ","",UNIK_ECHELLE)
		UNIK_ECHELLE<-unique(UNIK_ECHELLE)
		print("UNIK_ECHELLE")
		print(UNIK_ECHELLE)
		
		# Boucle sur les différentes échelles spatiales
		 for (u in 1:length(UNIK_ECHELLE) ) {
		 
		 UNIK_ECHELLE_u<-UNIK_ECHELLE[u]
			 CH_GRAPH<-paste0(CH_VALOSTAT,"/",u,"_",UNIK_ECHELLE_u)  
			 dir.create(CH_GRAPH)
		
			#Agrégation par parametre		
			 for( i in NAMESFINAL) {

			 if (UNIK_ECHELLE_u == "TTES_STATIONS") {
				 DATAAGG<-data.frame(table(RLT_FINALSTATION[,i]))
				 } else {
				 DATAAGG<-data.frame(table(RLT_FINALSTATION[RLT_FINALSTATION$ECHELLESTA == UNIK_ECHELLE_u,i]))			 
				}
				if ( nrow(DATAAGG) > 0 ) {	
				 names(DATAAGG)<-c(i,"Freq")
				 DATAAGG[,i]<-as.numeric(as.character(DATAAGG[,i]))
				} else {   
				DATAAGG<-data.frame(col1 = 1, col2 = 0)
				names(DATAAGG)<-c(i,"Freq")
				}
				 
				 
				 ORDRE<-data.frame(c("1","2","3","4","5","0"))
				 names(ORDRE)<-i
				 DATAAGG<-merge(DATAAGG,ORDRE, by = i, all.y = TRUE, sort = FALSE)
				 DATAAGG[,i]<-as.numeric(as.character(DATAAGG[,i]))
				 DATAAGG<-DATAAGG[order(DATAAGG[,i]),]
				 DATAAGG<-rbind( DATAAGG[DATAAGG[,i]>0,] , DATAAGG[DATAAGG[,i]==0,] )
				 DATAAGG$Freq[ is.na(DATAAGG$Freq) ] <- 0	 
			
			##	création de la table de synthèse statistique	
			 if (UNIK_ECHELLE_u == "TTES_STATIONS") {
			 DATAAGG_TEMP<-cbind(UNIK_ECHELLE_u,i,DATAAGG)
			names(DATAAGG_TEMP)<-c("ECHELLESTA","PARAMETAT","ETAT","Freq")
			DATAAGG_TEMP$PART<-round(DATAAGG_TEMP$Freq * 100 /sum(DATAAGG_TEMP$Freq),2)
			DATAAGG_ALL<-rbind(DATAAGG_ALL,DATAAGG_TEMP)
			 } else { 
			DATAAGG_TEMP<-cbind(UNIK_ECHELLE_u,i,DATAAGG)
			names(DATAAGG_TEMP)<-c("ECHELLESTA","PARAMETAT","ETAT","Freq")
			DATAAGG_TEMP$PART<-round(DATAAGG_TEMP$Freq * 100 /sum(DATAAGG_TEMP$Freq),2)
			
			DATAAGG_ALL<-rbind(DATAAGG_ALL,DATAAGG_TEMP)
			 }


			 
			if ( EXPORTGRAPH == "TOUS" |  i %in% ETATELTSQUAL  ) { 
			## PLOT	 
				jpeg(filename = paste0(CH_GRAPH,"/",i,".jpeg") ,
					 width = 800, height = 500, quality = 90, pointsize = 15, type = "windows")

				barplot(DATAAGG$Freq,
						names.arg = c("Très Bon","Bon","Moyen","Médiocre", "Mauvais","Ind."),
						main = gsub(" de la station","",lexiQselection$NOM_LONG[lexiQselection$NOM_COURT == i]),
						ylab = 'nombre de stations',
						col = c("cyan","green","yellow","orange","red","grey"),
						axes = TRUE,
						ylim = c(0,max(DATAAGG$Freq)*1.2)
						)

				## effectif et pourcentage
				POS<-c(0.695,1.891,3.052,4.276,5.449,6.696)		
				for (j in 1:6){		
				text(POS[j],	DATAAGG$Freq[j]+0.05*max(DATAAGG$Freq),	paste0(DATAAGG$Freq[j]," (", round(DATAAGG$Freq[j]*100/sum(DATAAGG$Freq),1),"%)"), cex = 0.8  )
				}


				dev.off()		 
				}} # fin de boucle sur chaque parametat NAMESFINAL
			
			} # fin de boucle sur les ECHELLESTA
		
		
		#Export Fichier Excel
		XLSload <- loadWorkbook(XLS, create = TRUE)
		createSheet(XLSload, name = "Station")
		print(SEEE_DEBformat)
		setStyleAction(XLSload, XLC$"STYLE_ACTION.NONE")
		writeWorksheet(XLSload, DATAAGG_ALL, sheet = "Station", startRow = 1, startCol = 1 , header = TRUE)
		saveWorkbook(XLSload)


		}	


	##############
	##### ME
	##############

	if ( ( SEEEBIO=="oui" | SEEEPCH=="oui" | SEEEPS=="oui") & SEEEECOLOME =="oui") {

		#creation d'un repertoire "VALORISATION"
		CH_VALO<-paste0(CH_OUTPUT,"VALORISATION")
		dir.create(CH_VALO)
		XLS<-paste0(CH_VALO,"/Valorisation_MasseEau.xls")

		#creation d'un repertoire "STATIONS"
		CH_VALOME<-paste0(CH_VALO,"/MASSE_EAU")
		dir.create(CH_VALOME)
		
		 NAMESFINAL<-unique(names(RLT_FINALME))
		 NAMESFINAL<-subset(NAMESFINAL,!(NAMESFINAL %in% c("NAME","EUCD" ,"CONTEXTE_PISCICOLE" , "STATION","STATIONTXT" ,
		 "TYPEFR" ,"ETATECOLO", "ETATECOLOHPS", "ETATBIO","ETATPCH","ETATPS","REPRESENTATIVE","EXCEPT_FROID",
		 "EXCEPT_CHAUD" ,"EXCEPT_ACID" , "EXCEPT_MO" ,"EXCEPT_TOURB" , "EXCEPT_O2" ,
		 "ASSOUPLI" ,"MODELISE" ,"NBINDICBIO" ,"LISTINDICBIO" ,"PCHDECLASS" ,     
		 "N_PCHDECLASS" , "PCHDECLASSASSOUP","N_PCHASSOUP" , "PSDECLASS" , "N_PSDECLASS" , "LIBELLE" , "CONTEXTE_PISCICOLE" , "TYPEFR",          
		"TYPESTAT" ,"ALTITUDE" , "REPRESENTATIVE", "EXCEPT_FROID" , "EXCEPT_CHAUD" , "EXCEPT_ACID","EXCEPT_MO" , "EXCEPT_TOURB" ,   
		"EXCEPT_O2" , "ECHELLESTA"  ,"FONDGEO_ARSENIC" ,"FONDGEO_CHROME" ,  "FONDGEO_CUIVRE" , "FONDGEO_ZINC" , "DURETE"  , "MODIFIED" ,       
		"ARTIFICIAL"  ,"ECHELLEME"   ,"TYPEME"  ,
		 "ECHELLEME", "ALTITUDE","ASSOUPLI","MODELISE","TYPEME","N_ECO_ORIGINDECLASS","N_ECODECLASS",names(STATION),names(MASSEDEAU) )) )

		 UNIK_ECHELLE<-c("TTES_ME",sort(unique(RLT_FINALME$ECHELLEME)))
		UNIK_ECHELLE<-gsub(" ","",UNIK_ECHELLE)
		UNIK_ECHELLE<-gsub("[:?:]","_",UNIK_ECHELLE)
		
		 for (u in 1:length(UNIK_ECHELLE) ) {
		 
		 UNIK_ECHELLE_u<-UNIK_ECHELLE[u]
			 CH_GRAPH<-paste0(CH_VALOME,"/",u,"_",UNIK_ECHELLE_u)  
			 dir.create(CH_GRAPH)
			 
			 for( i in NAMESFINAL) {

			 #Valorisation NIVEAU
			 if ( i == "NIVEAU") {
			 
			 
				 if (UNIK_ECHELLE_u == "TTES_ME") {
					 DATAAGGNIV<-data.frame(table(RLT_FINALME[,i]))
					 } else {
					 DATAAGGNIV<-data.frame(table(RLT_FINALME[RLT_FINALME$ECHELLEME == UNIK_ECHELLE_u,i]))			 
					}
					if ( nrow(DATAAGGNIV) > 0 ) {	
					 names(DATAAGGNIV)<-c(i,"Freq")
					 DATAAGGNIV[,i]<-as.numeric(as.character(DATAAGGNIV[,i]))
					} else {   
					DATAAGGNIV<-data.frame(col1 = 1, col2 = 0)
					names(DATAAGGNIV)<-c(i,"Freq")
					}
									 
					 ORDRE<-data.frame(c("1","2","3"))
					 names(ORDRE)<-i
					 DATAAGGNIV<-merge(DATAAGGNIV,ORDRE, by = i, all.y = TRUE, sort = FALSE)
					 DATAAGGNIV[,i]<-as.numeric(as.character(DATAAGGNIV[,i]))
					 DATAAGGNIV<-DATAAGGNIV[order(DATAAGGNIV[,i]),]
					 DATAAGGNIV<-rbind( DATAAGGNIV[DATAAGGNIV[,i]>0,] , DATAAGGNIV[DATAAGGNIV[,i]==0,] )
					 DATAAGGNIV$Freq[ is.na(DATAAGGNIV$Freq) ] <- 0	 
				
				##	création de la table de synthèse statistique	
				 if (UNIK_ECHELLE_u == "TTES_ME") {
				 DATAAGGNIV_ALL<-cbind(UNIK_ECHELLE_u,DATAAGGNIV)
				names(DATAAGGNIV_ALL)<-c("ECHELLEME","NIVEAU","Freq")
				DATAAGGNIV_ALL$PART<-round(DATAAGGNIV_ALL$Freq * 100 /sum(DATAAGGNIV_ALL$Freq),2)
				 } else { 
				DATAAGGNIV_TEMP<-cbind(UNIK_ECHELLE_u,DATAAGGNIV)
				names(DATAAGGNIV_TEMP)<-c("ECHELLEME","NIVEAU","Freq")
				DATAAGGNIV_TEMP$PART<-round(DATAAGGNIV_TEMP$Freq * 100 /sum(DATAAGGNIV_TEMP$Freq),2)
				
				DATAAGGNIV_ALL<-rbind(DATAAGGNIV_ALL,DATAAGGNIV_TEMP)
				 }

				 
				## PLOT	
				if ( EXPORTGRAPH == "TOUS" |  i %in% ETATELTSQUAL  ) { 
				
					jpeg(filename = paste0(CH_GRAPH,"/",i,".jpeg") ,
						 width = 800, height = 500, quality = 90, pointsize = 15, type = "windows")

					TITR<-"NIVEAU DE CONFIANCE"
					
					barplot(DATAAGGNIV$Freq,
							names.arg = c("1","2","3"),
							main = TITR,
							ylab = "nombre de Masses d'eau",
							col = c("#9FE855","#dce76f","#FF5E4D"),
							axes = TRUE,
							ylim = c(0,max(DATAAGGNIV$Freq)*1.2)
							)

					## effectif et pourcentage
					POS<-c(0.695,1.891,3.052)	
					for (j in 1:3){		
					text(POS[j],	DATAAGGNIV$Freq[j]+0.05*max(DATAAGGNIV$Freq),	paste0(DATAAGGNIV$Freq[j]," (", round(DATAAGGNIV$Freq[j]*100/sum(DATAAGGNIV$Freq),1),"%)"), cex = 0.8  )
					}


					dev.off()
					
				}
			 
			 
			 } else {
			 
			 #Valorisation CAS
			 if ( i == "CAS") {
			 
			 			 
				 if (UNIK_ECHELLE_u == "TTES_ME") {
					 DATAAGGCAS<-data.frame(table(RLT_FINALME[,i]))
					 } else {
					 DATAAGGCAS<-data.frame(table(RLT_FINALME[RLT_FINALME$ECHELLEME == UNIK_ECHELLE_u,i]))			 
					}
					if ( nrow(DATAAGGCAS) > 0 ) {	
					 names(DATAAGGCAS)<-c(i,"Freq")
					 DATAAGGCAS[,i]<-as.character(DATAAGGCAS[,i])
					} else {   
					DATAAGGCAS<-data.frame(col1 = 1, col2 = 0)
					names(DATAAGGCAS)<-c(i,"Freq")
					}
									 
					 ORDRE<-data.frame(c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8"))
					 names(ORDRE)<-i
					 DATAAGGCAS<-merge(DATAAGGCAS,ORDRE, by = i, all.y = TRUE, sort = FALSE)
					 DATAAGGCAS[,i]<-as.character(DATAAGGCAS[,i])
					 DATAAGGCAS<-DATAAGGCAS[order(DATAAGGCAS[,i]),]
					 DATAAGGCAS<-rbind( DATAAGGCAS[DATAAGGCAS[,i]>0,] , DATAAGGCAS[DATAAGGCAS[,i]==0,] )
					 DATAAGGCAS$Freq[ is.na(DATAAGGCAS$Freq) ] <- 0	 
				
				##	création de la table de synthèse statistique	
				 if (UNIK_ECHELLE_u == "TTES_ME") {
				 DATAAGGCAS_ALL<-cbind(UNIK_ECHELLE_u,DATAAGGCAS)
				names(DATAAGGCAS_ALL)<-c("ECHELLEME","CAS","Freq")
				DATAAGGCAS_ALL$PART<-round(DATAAGGCAS_ALL$Freq * 100 /sum(DATAAGGCAS_ALL$Freq),2)
				 } else { 
				DATAAGGCAS_TEMP<-cbind(UNIK_ECHELLE_u,DATAAGGCAS)
				names(DATAAGGCAS_TEMP)<-c("ECHELLEME","CAS","Freq")
				DATAAGGCAS_TEMP$PART<-round(DATAAGGCAS_TEMP$Freq * 100 /sum(DATAAGGCAS_TEMP$Freq),2)
				
				DATAAGGCAS_ALL<-rbind(DATAAGGCAS_ALL,DATAAGGCAS_TEMP)
				 }

				 
				## PLOT	
				if ( EXPORTGRAPH == "TOUS" |  i %in% ETATELTSQUAL  ) { 
				
					jpeg(filename = paste0(CH_GRAPH,"/",i,".jpeg") ,
						 width = 800, height = 500, quality = 90, pointsize = 15, type = "windows")

					TITR<-gsub(" de la masse d'eau","",lexiQselection$NOM_LONG[lexiQselection$NOM_COURT == i])
					TITR<-gsub(" de la station","",TITR)
					TITR<-gsub("(identique à l'état PCH)","",TITR)
					TITR<-gsub("(identique à l'état PS)","",TITR)
					TITR<-gsub("[:(:]","",TITR)
					TITR<-gsub("[:):]","",TITR)
					
					barplot(DATAAGGCAS$Freq,
							names.arg = c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8"),
							main = TITR,
							ylab = "nombre de Masses d'eau",
							col = COULEURCAS[1:8],
							axes = TRUE,
							ylim = c(0,max(DATAAGGCAS$Freq)*1.2)
							)

					## effectif et pourcentage
					POS<-c(0.695,1.891,3.052,4.276,5.449,6.696,7.87,11.21)	
					for (j in 1:8){		
					text(POS[j],	DATAAGGCAS$Freq[j]+0.05*max(DATAAGGCAS$Freq),	paste0(DATAAGGCAS$Freq[j]," (", round(DATAAGGCAS$Freq[j]*100/sum(DATAAGGCAS$Freq),1),"%)"), cex = 0.8  )
					}


					dev.off()
					
				}
			 } else {
			 
				 if (UNIK_ECHELLE_u == "TTES_ME") {
					 DATAAGG<-data.frame(table(RLT_FINALME[,i]))
					 } else {
					 DATAAGG<-data.frame(table(RLT_FINALME[RLT_FINALME$ECHELLEME == UNIK_ECHELLE_u,i]))			 
					}
					if ( nrow(DATAAGG) > 0 ) {	
					 names(DATAAGG)<-c(i,"Freq")
					 DATAAGG[,i]<-as.numeric(as.character(DATAAGG[,i]))
					} else {   
					DATAAGG<-data.frame(col1 = 1, col2 = 0)
					names(DATAAGG)<-c(i,"Freq")
					}
									 
					 ORDRE<-data.frame(c("1","2","3","4","5","0"))
					 names(ORDRE)<-i
					 DATAAGG<-merge(DATAAGG,ORDRE, by = i, all.y = TRUE, sort = FALSE)
					 DATAAGG[,i]<-as.numeric(as.character(DATAAGG[,i]))
					 DATAAGG<-DATAAGG[order(DATAAGG[,i]),]
					 DATAAGG<-rbind( DATAAGG[DATAAGG[,i]>0,] , DATAAGG[DATAAGG[,i]==0,] )
					 DATAAGG$Freq[ is.na(DATAAGG$Freq) ] <- 0	 
				
				##	création de la table de synthèse statistique	
				 if (UNIK_ECHELLE_u == "TTES_ME") {
				 DATAAGG_ALL<-cbind(UNIK_ECHELLE_u,i,DATAAGG)
				names(DATAAGG_ALL)<-c("ECHELLEME","PARAMETAT","ETAT","Freq")
				DATAAGG_ALL$PART<-round(DATAAGG_ALL$Freq * 100 /sum(DATAAGG_ALL$Freq),2)
				 } else { 
				DATAAGG_TEMP<-cbind(UNIK_ECHELLE_u,i,DATAAGG)
				names(DATAAGG_TEMP)<-c("ECHELLEME","PARAMETAT","ETAT","Freq")
				DATAAGG_TEMP$PART<-round(DATAAGG_TEMP$Freq * 100 /sum(DATAAGG_TEMP$Freq),2)
				
				DATAAGG_ALL<-rbind(DATAAGG_ALL,DATAAGG_TEMP)
				 }

				 
				## PLOT	
				if ( EXPORTGRAPH == "TOUS" |  i %in% ETATELTSQUAL  ) { 
				
					jpeg(filename = paste0(CH_GRAPH,"/",i,".jpeg") ,
						 width = 800, height = 500, quality = 90, pointsize = 15, type = "windows")

					TITR<-gsub(" de la masse d'eau","",lexiQselection$NOM_LONG[lexiQselection$NOM_COURT == i])
					TITR<-gsub(" de la station","",TITR)
					TITR<-gsub("(identique à l'état PCH)","",TITR)
					
					barplot(DATAAGG$Freq,
							names.arg = c("Très Bon","Bon","Moyen","Médiocre", "Mauvais","Ind."),
							main = TITR,
							ylab = "nombre de Masses d'eau",
							col = c("cyan","green","yellow","orange","red","grey"),
							axes = TRUE,
							ylim = c(0,max(DATAAGG$Freq)*1.2)
							)

					## effectif et pourcentage
					POS<-c(0.695,1.891,3.052,4.276,5.449,6.696)	
					for (j in 1:6){		
					text(POS[j],	DATAAGG$Freq[j]+0.05*max(DATAAGG$Freq),	paste0(DATAAGG$Freq[j]," (", round(DATAAGG$Freq[j]*100/sum(DATAAGG$Freq),1),"%)"), cex = 0.8  )
					}


					dev.off()
					
				}
				}
				}# fin de boucle sur chaque parametat NAMESFINAL
			
			} # fin de boucle sur les ECHELLEME
		
		
		 #Export Fichier Excel
		XLSload <- loadWorkbook(XLS, create = TRUE)
		createSheet(XLSload, name = "MasseEau")
		createSheet(XLSload, name = "NiveauConfiance")
		createSheet(XLSload, name = "Cas")
		print(SEEE_DEBformat)
		setStyleAction(XLSload, XLC$"STYLE_ACTION.NONE")
		writeWorksheet(XLSload, DATAAGG_ALL, sheet = "MasseEau", startRow = 1, startCol = 1 , header = TRUE)
		writeWorksheet(XLSload, DATAAGGNIV_ALL, sheet = "NiveauConfiance", startRow = 1, startCol = 1 , header = TRUE)
		writeWorksheet(XLSload, DATAAGGCAS_ALL, sheet = "Cas", startRow = 1, startCol = 1 , header = TRUE)

		saveWorkbook(XLSload)

		
		}
		}
	
}	
	