#########################
## LOG FINAL ou D'ERREUR
########################

## Export en Archive Rdata
try(save.image(file= paste(racine,"/LOGR/SAVE_RDATA/imageFIN.Rdata",sep="")))



###msgerror.r

library(tcltk)
MSG<-geterrmessage()

if(MSG[1] != "" ) {
print(MSG)
#gestion de certains type de messages
MSG[1][as.numeric(regexpr("XLC",MSG[1])) >0]<-"La librairie XLConnect n'arrive pas à se lancer\n Ce problème peut être lié à Java et à l'utilisation \n d'applications telle que Outlook\n Info : S3R utilise une version Java 32bits > 1.6\n Si besoin, redémarrer l'ordinateur"
MSG[1][as.numeric(regexpr("rJava",MSG[1])) >0]<-"La librairie rJava n'arrive pas à se lancer\n Ce problème peut être lié à Java et à l'utilisation \n d'application telle que Outlook\n Info : S3R utilise une version Java 32bits > 1.6\n Si besoin, redémarrer l'ordinateur"
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

#if(sum(unlist(gregexpr("Error in structure",MSG)))<0){
try(tkmessageBox(message = paste("Erreur",MSG[1],sep="\n"), icon = "error", type = "ok"))
#}
}
#### on supprime les objets qui ne servent à rien
listobjet<-ls()
rm(list=listobjet[as.numeric(regexpr("but",listobjet)) >0])
listobjet<-ls()
rm(list=listobjet[as.numeric(regexpr("frame",listobjet)) >0])
listobjet<-ls()
rm(list=listobjet[as.numeric(regexpr("cb",listobjet)) >0])
listobjet<-ls()
rm(list=listobjet[as.numeric(regexpr("lab",listobjet)) >0])
listobjet<-ls()
rm(list=listobjet[as.numeric(regexpr("cond",listobjet)) >0])
listobjet<-ls()
rm(list=listobjet[as.numeric(regexpr("onOK",listobjet)) >0])
listobjet<-ls()
rm(list=listobjet[as.numeric(regexpr("mod_",listobjet)) >0])
listobjet<-ls()
rm(list=listobjet[as.numeric(regexpr("font",listobjet)) >0])
listobjet<-ls()
rm(list=listobjet[as.numeric(regexpr("entry",listobjet)) >0])
listobjet<-ls()
rm(list=listobjet[as.numeric(regexpr("comboBox",listobjet)) >0])
gc()

quit()

