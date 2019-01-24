###lancement
## indique le numéro de version
if (substr(pathS,1,4) == "http") {
numversion<-"V5.0 (online)"} else {numversion<-"V5.0"}


##Fonction qui enregistre l'image Rdata à un moment précis (ne sert que pour la phase de dev)
SAVE<-function(...) {
	DATE<-gsub(":","_",gsub(" ","-",date()))
	save.image(file= paste(racine,"/image_",DATE,".Rdata",sep=""))
}

## on modifie la fonction "source" pour quelle soit utilisable avec un paramettre alléatoire
SRCE1<-source
source <- function(file, echo = TRUE, print.eval = TRUE){
if (regexpr("msg",file) <= 0 ) {FILESCRIPT<<-file  
print(FILESCRIPT)}
param<-sample(1:1000000,1)
if (substr(file,1,4) == 'http') {
URL<-paste0(file,"?r=",param )} else {URL<-file}
print(URL)
SRCE1(file=URL, echo = echo, )
}


## on modifie la fonction "read.csv2" pour quelle soit utilisable avec un paramettre alléatoire
RCSV2<-read.csv2
read.csv2 <- function(file){
param<-sample(1:1000000,1)
if (substr(file,1,4) == 'http') {
URL<-paste0(file,"?r=",param )} else {URL<-file}
print(URL)
#RCSV2(file=URL)
read.table(file=URL, header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, comment.char = "",
          stringsAsFactors = FALSE,  as.is = FALSE, colClasses="character" )
}

## on modifie la fonction write.csv2 pour que par défaut NA soit écrit par du vide
if (!exists("CHWRTECSV")) {
writecsv2<-write.csv2
write.csv2<-function(x, file = "", row.names = FALSE) { write.table(x, file=file, na="", row.names = row.names, dec="." , col.names = TRUE, sep = ";") }
}
CHWRTECSV<-"ok"


##on modifie la fonction file.exists pour qu'elle prenne en compe le "/" final
FEXIST<-file.exists
file.exists<-function(file) {
RETURN<-FALSE
file<-gsub("//","/",file)
if(substr(file,nchar(file),nchar(file)) == "/") { file<-substr(file,1,nchar(file)-1)}
if(FEXIST(file)) {RETURN<-TRUE}
return(RETURN)
}


ifelse2<-ifelse
ifelse<-function (test, yes, no) 
{
    if (is.atomic(test)) {
        if (typeof(test) != "logical") 
            storage.mode(test) <- "logical"
        if (length(test) == 1 && is.null(attributes(test))) {
            if (is.na(test)) 
                return(NA)
            else if (test) {
                
                  return(yes)
            }
       
                return(no)
        }
    }
    else test <- if (isS4(test)) 
        as(test, "logical")
    else as.logical(test)
    ans <- test
    ok <- !(nas <- is.na(test))
    if (any(test[ok])) 
        ans[test & ok] <- rep(yes, length.out = length(ans))[test & 
            ok]
    if (any(!test[ok])) 
        ans[!test & ok] <- rep(no, length.out = length(ans))[!test & 
            ok]
    ans[nas] <- NA
    ans
}


#### on réécrit la fonction d'arrondi
#roundtemp<-round
#round<-function(X,decimal=0){
#x<-roundtemp(X,decimal)
#COND<-roundtemp(X,decimal) != roundtemp(X+10^(-decimal-2),decimal)
#x[COND]<-roundtemp(X[COND]+10^(-decimal-2),decimal)
#return(x)
#}

roundtemp<-round
round <- function (x, N=0) {
x<-x*10^N
x<-ifelse(x-trunc(x)>=0.5,trunc(x)+1,trunc(x))
x<-x*10^(-N)
return(x)
}

### on lance GUI_tab1
pathsite<-paste(pathS,"/",sep="")
racine<-paste(getwd(),"/S3R",sep="")
print(racine)
.libPaths(paste(racine,"R/library/",sep=""))  #indique clairement le chemins des librairies

# cherche les parametres de mot de passe par défault et le chemin de resultat
if (file.exists(paste(racine,"/PARAMETRES/INI/ini.DAT",sep=""))) {load(paste(racine,"/PARAMETRES/INI/ini.DAT",sep=""))}
if (!file.exists(paste(racine,"/PARAMETRES/INI/",sep=""))) {dir.create(paste(racine,"/PARAMETRES/INI/",sep=""))}
if (file.exists(paste(racine,"/PARAMETRES/INI/CH_OUTPUT.DAT",sep=""))) {load(paste(racine,"/PARAMETRES/INI/CH_OUTPUT.DAT",sep=""))}




##Déclaration d'un certain nombre de variable

MODULE<-as.character()
SEEEPCH<-"non"
SEEEBIO<-"non"
SEEEPS<-"non"
SEEEECOLO<-"non"
SEEECHIM<-"non"
CONTA<-"non"
SEEEPEGASE<-"non"
EXCLU_POLNS<-"non"
SEEEECOLOME<-"non"
SIMULQUARTCLASSE<-"non"
ASSOUPTBE<-"non"
FRACTIONOKPCH<-"non"
UNITEOKPCH<-"non"
FREQOKPCH<-"non"
FRACTIONOKPS<-"non"
UNITEOKPS<-"non"
FREQOKPS<-"non"
LQSUPNQEMAPS<-"non"
SUPPR_LQ_ABERRANTE_PS<-"non"
FRACTIONOKCHIM<-"non"
UNITEOKCHIM<-"non"
FREQOKCHIM<-"non"
SEEECHIMME<-"non"
LQSUPNQEMACHIM<-"non"
SUPPR_LQ_ABERRANTE_CHIM<-"non"
INDICE<-"non"
FREQPRELEV<-"non"
MISECOULEUR<-"non"
STATISTIQUE<-"non"
OPTIONNO3<-"non"
EXPORTGRAPH<-"non"
#pour module contatmination
CONTA<-"non"
FRACTIONOKCONTA<-"non"
UNITEOKCONTA<-"non"
FREQOKCONTA<-"non"
CHOIXSEUIL<-" - "
CONTAME<-""
NBSEUIL<-4
CONTARETRAITNONQUANTI<- "non"
TYPECONTA <- 1
EXCEPTLOC <- "non"
nbgroupeDATACHIM<-0
LQPROGRESSIVE<-1
OPTIONEXPORTDATABRUT<-"non"
g<-1

##Sourcer vers la première fenêtre d'interface graphique
try(save.image(file= paste(racine,"/LOGR/SAVE_RDATA/Depart.Rdata",sep="")))
try(source(paste(pathsite,"preinterface.r",sep="") , echo = TRUE, print.eval = TRUE))
try(source(paste(pathsite,"GUI_tab0.r",sep="") , echo = TRUE, print.eval = TRUE))
print(geterrmessage())


