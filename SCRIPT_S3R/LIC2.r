
################################################
###verification dates et CLE d'UTILISATION #####
################################################



### Selon les utilisateurs, mettre la clé et la date

##Formatage de la date
LOGINDATE<-read.csv2(paste(pathS,"login_psw.csv",sep=""))
LOGINDATE$jour<-as.character(LOGINDATE$jour)
LOGINDATE$mois<-as.character(LOGINDATE$mois)
LOGINDATE$annee<-as.character(LOGINDATE$annee)

LOGINDATE$jour[as.numeric(LOGINDATE$jour)<10]<-paste("0",LOGINDATE$jour[as.numeric(LOGINDATE$jour)<10],sep="")
LOGINDATE$mois[as.numeric(LOGINDATE$mois)<10]<-paste("0",LOGINDATE$mois[as.numeric(LOGINDATE$mois)<10],sep="")
LOGINDATE$annee[as.numeric(LOGINDATE$annee)<10]<-paste("0",LOGINDATE$annee[as.numeric(LOGINDATE$annee)<10],sep="")
LOGINDATE$date_end<-paste(LOGINDATE$annee,LOGINDATE$mois,LOGINDATE$jour,sep="-")


NAMEORDI<-toupper(as.character(Sys.info()[4])) ## récuppère le nom de l'ordi

LOGINDATE<-LOGINDATE[LOGINDATE$cle == CLE & LOGINDATE$mail == LOGIN & toupper(LOGINDATE$nameordi) %in% c(NAMEORDI,"FLOTTANTE") , ]

############SCRIPT (NE PAS TOUCHER)

today <- Sys.Date()

validok<-"oui"
infodate<-" /"

if (substr(pathS,1,4) == 'http') {
	if (nrow(LOGINDATE) > 0){
	datelim <- as.Date(LOGINDATE$date_end[1]) 
	nbjourestant<-as.numeric(datelim-today)
	infodate<-paste("Licence épuisée dans " , nbjourestant, " j")
	validok<-"oui"
	}else{
	if(!exists("nbjourestant") ) {nbjourestant<-0}
	}

	if (nbjourestant <= 0) {
	validok<-"non"
	infodate<-"Licence épuisée ou Utilisateur/Clé/Machine non valide.  \nVeuillez contacter ASCONIT Consultants"
	tkmessageBox(message = infodate)
	}
}


