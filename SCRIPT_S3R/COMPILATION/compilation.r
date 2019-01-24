
##Permet de compiler plusieurs fichiers ayant la m�me structure en un m�me fichier



library(tcltk) 


ok<-0

# racine<-"P:/E3930_AESN_S3R/S3R_AESN_dev/S3R/"
CH_REPFILES<-tclvalue(tkchooseDirectory(initialdir = racine, title = "Choisir un r�pertoire contenant les fichiers � assembler : "))
tkconfigure(tt,cursor="watch") 
if (!nchar(CH_REPFILES)) { #choix du dossier
		tkmessageBox(message = "Aucun dossier s�lectionn�", icon = "warning")
		tkconfigure(tt,cursor="arrow")
		} else {
		LISTFILE<-list.files(CH_REPFILES)
	if (length(LISTFILE) == 0 ) {
			tkmessageBox(message = "Aucun fichier dans le dossier s�lectionn�", icon = "warning")
			tkconfigure(tt,cursor="arrow")
			} else {
			
			

		if (exists("FILECOMPIL")) { rm(FILECOMPIL)} #r�initialisation en supprimant le fichier assembl�
			
			
		for ( i in LISTFILE ) {
			CH_FILE<-paste0(CH_REPFILES,'/',i) # on lit le fichier CSV
			donnee<-read.csv2(CH_FILE)
				if (nrow(donnee) == 0) {
					ok<-0
					tkmessageBox(title = "Info", message = paste(i, "est vide "))
					tkconfigure(tt,cursor="arrow")				} else { #v�rif le fichier CSV est plein
					
				if (length(names(donnee)) < 5) {
					ok<-0
					tkmessageBox(title = "Info", message = paste(i, "n'a pas un nombre de colonnes suffisants "))
					tkconfigure(tt,cursor="arrow")} else { #v�rif le fichier CSV a au moins 5 colonnes
				
				if (!exists("FILECOMPIL")) { 
				ok<-1
				FILECOMPIL<-donnee  } else { 
				
				matchnames<-length(which(names(donnee) %in% names(FILECOMPIL)))  #on v�rifie que le deuxi�me CSV a les m�mes colonnes que le premier

				
					if (matchnames >=  length(names(FILECOMPIL)) ) {
					donnee<-donnee[,names(FILECOMPIL)]
					FILECOMPIL<-rbind(FILECOMPIL,donnee)  
					ok<-1
					gc()
					} else {
					ok<-0
					tkmessageBox(message = "Les colonnes ne correspondent pas entre les fichiers", icon = "warning")
					tkconfigure(tt,cursor="arrow")
					}
				}
				
				}}			
		}

	}

print("compilation OK, le fichier est pr�t � �tre exporter au format CSV")

	if (ok == 1) {
				FILEEXPORT<-tclvalue(tkgetSaveFile(title = "Choisir un fichier de sortie", #choix du fichier de sortie
				filetypes = " {{CSV Files} {.csv}} ",
				initialdir = racine
				))
				
				if( !(gregexpr(".csv",FILEEXPORT) > 0 )) {
					FILEEXPORT<-paste0(FILEEXPORT,".csv")
				}
				
				tkconfigure(tt,cursor="watch") 
				write.csv2(FILECOMPIL, file = FILEEXPORT)  #Export
				print("Export OK")

				tkconfigure(tt,cursor="arrow")
				tkmessageBox(message = "Le fichier a �t� cr�� et export�")
				
				
				}
			
}	

