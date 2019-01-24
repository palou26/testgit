#########################
## LOG FINAL ou D'ERREUR
########################

## Export en Archive Rdata
try(save.image(file= paste(racine,"/LOGR/SAVE_RDATA/imageFIN.Rdata",sep="")))



###msgerror.r

library(tcltk)
MSG<-geterrmessage()
print(traceback() )
if(MSG[1] != "" ) {
#gestion de certains type de messages
MSG[1][as.numeric(regexpr("XLC",MSG[1])) >0]<-"La librairie XLConnect n'arrive pas à se lancer\n Ce problème peut être lié à Java et à l'utilisation \n d'applications telle que Outlook\n Info : S3R utilise une version Java 32bits > 1.6\n Si besoin, redémarrer l'ordinateur"
MSG[1][as.numeric(regexpr("rJava",MSG[1])) >0]<-"La librairie rJava n'arrive pas à se lancer\n Ce problème peut être lié à Java (ou à un mauvais parametrage\n des varaiables d'environnement JAVA_HOME)\n et/ou à l'utilisation \n d'application telle que Outlook\n Info : S3R utilise une version Java 32bits > 1.6\n Si besoin, redémarrer l'ordinateur"
MSG[1][as.numeric(regexpr("ouvrir la connexion",MSG[1])) >0]<-"S3R tente d'exporter un fichier csv ou xls\n soit le chemin du fichier est invalide, soit il est déjà ouvert"

print(MSG)

print("#############################################################################################")
print("#############################################################################################")
print("#############################################################################################")
print("#############################################################################################")
print("#######################################  FIN   ##############################################")
print("#############################################################################################")
print("#############################################################################################")
print("#############################################################################################")
print("#############################################################################################")


try(tkmessageBox(message = paste("Erreur",FILESCRIPT, MSG[1],sep="\n"), icon = "error", type = "ok"))
#try(tkmessageBox(message = paste("Erreur",traceback() ,sep="\n"), icon = "error", type = "ok"))




}

print(FILESCRIPT)
print(MSG[1])
quit()

