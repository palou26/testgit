
#### FONCTION POUR MISE EN COULEUR DU TABLEAU EXCEL
##nomxls = nom du fichier EXCEL
##createxls = TRUE si le fichier Excel n'est pas créé
##nameonglet = nom de l'onglet
##createsheet = TRUE si on souhaite créer l'index
## dfr = dataframe à exporter en Excel
## numcol = vecteur avec le numero des colonne où il faut faire la mise en couleur. si numcol = 0 alors toute les colonne aura la mise en couleur
## depR = numéro de la ligne où le tableau va être importé (par defaut 1
## depC = numéro de la colonne où le tableau va être importer (par defaut 1)
## entete = TRUE si on veut importer le nom des colonne
## couleur = 1, on prend les couleurs qui sont dans le fichier EXCEL. Si =0, on prend les couleurs par defaut

colorXLS<-function( nomxls, createxls = TRUE, nameonglet = "NEWONGLET", createsheet = TRUE,  dfr, numcol = 0, depR =1 , depC=1, entete = TRUE, couleur=1,nbclasses = 5) { 
tcl("wm", "attributes", tt, topmost=FALSE) ## fenetre  en 1er plan
progressBar <- tkProgressBar("Export tableau", "",0, 100, 0)
tcl("update")
print(paste("mise en couleur de ", nomxls))
Sys.sleep(0.3)
nomXLS<-nomxls

## charger un fichier Excel et rajouter un onglet avec des valeurs
####
DF<-dfr
print("création du xls et de l'onglet")

if (createxls == TRUE){
	#création du template
	XLSload <- loadWorkbook(nomXLS, create = TRUE)
	#création de l'onglet
	createSheet(XLSload, name = nameonglet)
	saveWorkbook(XLSload)
} else {
#chargement du template
XLSload <- loadWorkbook(nomXLS, create = FALSE)
	if (createsheet == TRUE){
	#création de l'onglet
	createSheet(XLSload, name = nameonglet)
	saveWorkbook(XLSload)
	}}


### EXPORT du dataframe vers le fichier Excel
print("Export du dataframe vers le fichier Excel")
setTkProgressBar(progressBar, 0, paste("Export : ", nameonglet), "0% réalisé")
XLSload <- loadWorkbook(nomXLS) 
setTkProgressBar(progressBar, 25, paste("Export : ", nameonglet), "25% réalisé")
writeWorksheet(XLSload, DF, sheet = nameonglet, startRow = depR, startCol = depC , header = entete)
setTkProgressBar(progressBar, 50, paste("Export : ", nameonglet), "50% réalisé")
setColumnWidth(XLSload,nameonglet,1:ncol(DF),-1)
setTkProgressBar(progressBar, 75, paste("Export : ", nameonglet), "75% réalisé")
Sys.sleep(0.5)
### largeur des colonnes en fonction du contenu
##SAVE
saveWorkbook(XLSload)
setTkProgressBar(progressBar, 100, paste("Export : ", nameonglet), "100% réalisé")
tcl("update")
Sys.sleep(0.25)

#######################
### MISE EN COULEUR ##
#######################
if(numcol[1] == 0) { ALLCOLO<-"oui"}else  {ALLCOLO<-"non"}
if(ALLCOLO=="oui"  ) {numcol <- 2:ncol(DF)}


if (MISECOULEUR=="oui"){
print("mise en couleur")
setTkProgressBar(progressBar, 0, paste("Mise en couleur de l'onglet : ", nameonglet), "")
tcl("update")
###chargement du fichier Excel
XLSload <- loadWorkbook(nomXLS) 

#On déclare les couleurs
if (nbclasses == 4) {VECTCOLOR<-c("GREY", "GREEN","YELLOW","ORANGE","RED")}
if (nbclasses == 5) {VECTCOLOR<-c("GREY", "BLUE","GREEN","YELLOW","ORANGE","RED")}


if (exists("BLUE")) { rm(BLUE)}
if (exists("RED")) { rm(RED)}
if (exists("YELLOW")) { rm(YELLOW)}
if (exists("GREEN")) { rm(GREEN)}
if (exists("ORANGE")) { rm(ORANGE)}
if (exists("GREY")) { rm(GREY)}

### dans ce cas, les styles ont été créés dans le template (or ca marche pas très bien)
if (couleur ==1 ) {
BLUE<-getCellStyle(XLSload,"blue_e")
RED<-getCellStyle(XLSload,"red_e")
ORANGE<-getCellStyle(XLSload,"orange_e")
GREEN<-getCellStyle(XLSload,"green_e")
YELLOW<-getCellStyle(XLSload,"yellow_e")


}

### dans ce cas, on prend les couleurs par défaut qui existe dans library XLConnect
if (couleur ==0 ) {
BLUE <- createCellStyle(XLSload, name = paste("BLUE3",nameonglet,sep="") )
setFillPattern(BLUE, fill = XLC$FILL.SOLID_FOREGROUND)
setFillForegroundColor(BLUE, color = XLC$COLOR.SKY_BLUE)

RED <- createCellStyle(XLSload, name = paste("RED3",nameonglet,sep="") )
setFillPattern(RED, fill = XLC$FILL.SOLID_FOREGROUND)
setFillForegroundColor(RED, color = XLC$COLOR.RED)

YELLOW <- createCellStyle(XLSload, name = paste("YELLOW3",nameonglet,sep="") )
setFillPattern(YELLOW, fill = XLC$FILL.SOLID_FOREGROUND)
setFillForegroundColor(YELLOW, color = XLC$COLOR.YELLOW)

GREEN <- createCellStyle(XLSload, name = paste("GREEN3",nameonglet,sep="") )
setFillPattern(GREEN, fill = XLC$FILL.SOLID_FOREGROUND)
setFillForegroundColor(GREEN, color = XLC$COLOR.BRIGHT_GREEN)

ORANGE <- createCellStyle(XLSload, name = paste("ORANGE3",nameonglet,sep="") )
setFillPattern(ORANGE, fill = XLC$FILL.SOLID_FOREGROUND)
setFillForegroundColor(ORANGE, color = XLC$COLOR.LIGHT_ORANGE)

GREY <- createCellStyle(XLSload, name = paste("GREY3",nameonglet,sep="") )
setFillPattern(GREY, fill = XLC$FILL.SOLID_FOREGROUND)
setFillForegroundColor(GREY, color = XLC$COLOR.GREY_40_PERCENT)
}

##### On calcul le nombre total d'itération
nbcol<-length(VECTCOLOR)
iter<-0
somiter<-length(numcol)*nbcol 

row.names(DF)<-1:nrow(DF)
##boucle sur le nombre de colonne et en fonction du nombre de couleurs
for ( n in numcol){
for ( v in 1:nbcol){
rowscolor<-as.numeric(row.names(DF)[DF[,n]== v-1])
rowscolor<-rowscolor[!is.na(rowscolor)]
if (length(rowscolor) > 0) {
setCellStyle(XLSload, sheet = nameonglet, row = rowscolor + depR, col = n,cellstyle = get(VECTCOLOR[v]) ) }

	# barre de progression
	iter<-iter+1
	iterP<-round(iter*100/somiter)
	info <- paste(iterP, "% réalisé")
	setTkProgressBar(progressBar, iterP, paste("Mise en couleur de l'onglet : ", nameonglet), info)
	tcl("update")
	
Sys.sleep(0.02)
}}
print("enregistrement des couleurs")
saveWorkbook(XLSload)
print("fin enregistrement des couleurs")
}

close(progressBar)
tcl("update")
Sys.sleep(0.25)
rm(BLUE, ORANGE, YELLOW, GREEN,RED)
gc()
tcl("wm", "attributes", tt, topmost=TRUE)



}




colorXLSCHIM<-function( nomxls, createxls = TRUE, nameonglet = "NEWONGLET", createsheet = TRUE,  dfr, numcol = 0, depR =1 , depC=1, entete = TRUE, couleur=1) { 
tcl("wm", "attributes", tt, topmost=FALSE) ## fenetre  en 1er plan
progressBar <- tkProgressBar("Export tableau", "",0, 100, 0)
tcl("update")
print(paste("mise en couleur de ", nomxls))
Sys.sleep(0.01)
nomXLS<-nomxls

## charger un fichier Excel et rajouter un onglet avec des valeurs
####
DF<-dfr
print("création du xls et de l'onglet")

if (createxls == TRUE){
	#création du template
	XLSload <- loadWorkbook(nomXLS, create = TRUE)
	#création de l'onglet
	createSheet(XLSload, name = nameonglet)
	saveWorkbook(XLSload)
} else {
#chargement du template
XLSload <- loadWorkbook(nomXLS, create = FALSE)
	if (createsheet == TRUE){
	#création de l'onglet
	createSheet(XLSload, name = nameonglet)
	saveWorkbook(XLSload)
	}}


### EXPORT du dataframe vers le fichier Excel
print("Export du dataframe vers le fichier Excel")
setTkProgressBar(progressBar, 0, paste("Export : ", nameonglet), "0% réalisé")
XLSload <- loadWorkbook(nomXLS) 
setTkProgressBar(progressBar, 25, paste("Export : ", nameonglet), "25% réalisé")
writeWorksheet(XLSload, DF, sheet = nameonglet, startRow = depR, startCol = depC , header = entete)
setTkProgressBar(progressBar, 50, paste("Export : ", nameonglet), "50% réalisé")
setColumnWidth(XLSload,nameonglet,1:ncol(DF),-1)
setTkProgressBar(progressBar, 75, paste("Export : ", nameonglet), "75% réalisé")
Sys.sleep(0.55)
### largeur des colonnes en fonction du contenu
##SAVE
saveWorkbook(XLSload)
setTkProgressBar(progressBar, 100, paste("Export : ", nameonglet), "100% réalisé")
tcl("update")
Sys.sleep(0.25)

#######################
### MISE EN COULEUR ##
#######################

if (MISECOULEUR=="oui"){
print("mise en couleur")
setTkProgressBar(progressBar, 0, paste("Mise en couleur de l'onglet : ", nameonglet), "")
tcl("update")
###chargement du fichier Excel
XLSload <- loadWorkbook(nomXLS) 

#On déclare les couleurs
VECTCOLOR<-c("GREY", "BLUE","RED")

if (exists("BLUE")) { rm(BLUE)}
if (exists("RED")) { rm(RED)}
if (exists("GREY")) { rm(GREY)}

### dans ce cas, les styles ont été créés dans le template (or ca marche pas très bien)
if (couleur ==1 ) {
BLUE<-getCellStyle(XLSload,"blue_e")
RED<-getCellStyle(XLSload,"red_e")
}

### dans ce cas, on prend les couleurs par défaut qui existe dans library XLConnect
if (couleur ==0 ) {
BLUE <- createCellStyle(XLSload, name = paste("BLUE3",nameonglet,sep="") )
setFillPattern(BLUE, fill = XLC$FILL.SOLID_FOREGROUND)
setFillForegroundColor(BLUE, color = XLC$COLOR.SKY_BLUE)

RED <- createCellStyle(XLSload, name = paste("RED3",nameonglet,sep="") )
setFillPattern(RED, fill = XLC$FILL.SOLID_FOREGROUND)
setFillForegroundColor(RED, color = XLC$COLOR.RED)

GREY <- createCellStyle(XLSload, name = paste("GREY3",nameonglet,sep="") )
setFillPattern(GREY, fill = XLC$FILL.SOLID_FOREGROUND)
setFillForegroundColor(GREY, color = XLC$COLOR.GREY_40_PERCENT)
}

##### On calcul le nombre total d'itération

print("names DF")
print(names(DF))
iter<-0
if (numcol[1] == 0) {
numcol <- 2:ncol(DF)
somiter<-ncol(DF)*3
} else {
somiter<-length(numcol)*3 }

row.names(DF)<-1:nrow(DF)
##boucle sur le nombre de colonne et en fonction du nombre de couleurs
for ( n in numcol){
for ( v in 1:3){
rowscolor<-as.numeric(row.names(DF)[DF[,n]== v-1])
rowscolor<-rowscolor[!is.na(rowscolor)]
if (length(rowscolor) > 0) {
setCellStyle(XLSload, sheet = nameonglet, row = rowscolor + depR, col = n,cellstyle = get(VECTCOLOR[v]) ) }

	# barre de progression
	iter<-iter+1
	iterP<-round(iter*100/somiter)
	info <- paste(iterP, "% réalisé")
	setTkProgressBar(progressBar, iterP, paste("Mise en couleur de l'onglet : ", nameonglet), info)
	tcl("update")
	
Sys.sleep(0.01)
}}
saveWorkbook(XLSload)
}

close(progressBar)
tcl("update")
Sys.sleep(0.25)
rm(BLUE, ORANGE, YELLOW, GREEN,RED)
gc()
tcl("wm", "attributes", tt, topmost=TRUE)
}

#######################################################################
##Fonction pour la coloration de la fiabilité en 3 classe + indéterminé
########################################################################


colorXLSFia<-function( nomxls, createxls = TRUE, nameonglet = "NEWONGLET", createsheet = TRUE,  dfr, numcol = 0, depR =1 , depC=1, entete = TRUE, couleur=1) { 
tcl("wm", "attributes", tt, topmost=FALSE) ## fenetre  en 1er plan
progressBar <- tkProgressBar("Export tableau", "",0, 100, 0)
tcl("update")
print(paste("mise en couleur de ", nomxls))
Sys.sleep(0.01)
nomXLS<-nomxls

## charger un fichier Excel et rajouter un onglet avec des valeurs
####
DF<-dfr
print("création du xls et de l'onglet")

if (createxls == TRUE){
	#création du template
	XLSload <- loadWorkbook(nomXLS, create = TRUE)
	#création de l'onglet
	createSheet(XLSload, name = nameonglet)
	saveWorkbook(XLSload)
} else {
#chargement du template
XLSload <- loadWorkbook(nomXLS, create = FALSE)
	if (createsheet == TRUE){
	#création de l'onglet
	createSheet(XLSload, name = nameonglet)
	saveWorkbook(XLSload)
	}}


### EXPORT du dataframe vers le fichier Excel
print("Export du dataframe vers le fichier Excel")
setTkProgressBar(progressBar, 0, paste("Export : ", nameonglet), "0% réalisé")
XLSload <- loadWorkbook(nomXLS) 
setTkProgressBar(progressBar, 25, paste("Export : ", nameonglet), "25% réalisé")
writeWorksheet(XLSload, DF, sheet = nameonglet, startRow = depR, startCol = depC , header = entete)
setTkProgressBar(progressBar, 50, paste("Export : ", nameonglet), "50% réalisé")
setColumnWidth(XLSload,nameonglet,1:ncol(DF),-1)
setTkProgressBar(progressBar, 75, paste("Export : ", nameonglet), "75% réalisé")
Sys.sleep(0.75)
### largeur des colonnes en fonction du contenu
##SAVE
saveWorkbook(XLSload)
setTkProgressBar(progressBar, 100, paste("Export : ", nameonglet), "100% réalisé")
Sys.sleep(0.5)

#######################
### MISE EN COULEUR ##
#######################

if (MISECOULEUR=="oui"){
print("mise en couleur")
setTkProgressBar(progressBar, 0, paste("Mise en couleur de l'onglet : ", nameonglet), "")
###chargement du fichier Excel
XLSload <- loadWorkbook(nomXLS) 

#On déclare les couleurs
VECTCOLOR<-c("GREY", "VERT","JAUNE","ROSE")

if (exists("VERT")) { rm(BLUE)}
if (exists("JAUNE")) { rm(RED)}
if (exists("ROSE")) { rm(YELLOW)}
if (exists("GREY")) { rm(GREY)}

### dans ce cas, les styles ont été créés dans le template (or ca marche pas très bien)
if (couleur ==1 ) {
VERT<-getCellStyle(XLSload,"VERT_e")
ROSE<-getCellStyle(XLSload,"ROSE_e")
JAUNE<-getCellStyle(XLSload,"JAUNE_e")



}

### dans ce cas, on prend les couleurs par défaut qui existe dans library XLConnect
if (couleur ==0 ) {
VERT <- createCellStyle(XLSload, name = paste("VERT3",nameonglet,sep="") )
setFillPattern(VERT, fill = XLC$FILL.SOLID_FOREGROUND)
setFillForegroundColor(VERT, color = XLC$COLOR.LIGHT_GREEN)

ROSE <- createCellStyle(XLSload, name = paste("ROSE3",nameonglet,sep="") )
setFillPattern(ROSE, fill = XLC$FILL.SOLID_FOREGROUND)
setFillForegroundColor(ROSE, color = XLC$COLOR.ROSE)

JAUNE <- createCellStyle(XLSload, name = paste("JAUNE3",nameonglet,sep="") )
setFillPattern(JAUNE, fill = XLC$FILL.SOLID_FOREGROUND)
setFillForegroundColor(JAUNE, color = XLC$COLOR.LEMON_CHIFFON)

GREY <- createCellStyle(XLSload, name = paste("GREY3",nameonglet,sep="") )
setFillPattern(GREY, fill = XLC$FILL.SOLID_FOREGROUND)
setFillForegroundColor(GREY, color = XLC$COLOR.GREY_40_PERCENT)
}

##### On calcul le nombre total d'itération
iter<-0
if (numcol[1] == 0) {
numcol <- 2:ncol(DF)
somiter<-ncol(DF)*4
} else {
somiter<-length(numcol)*4 }

row.names(DF)<-1:nrow(DF)
##boucle sur le nombre de colonne et en fonction du nombre de couleurs
for ( n in numcol){
for ( v in 1:4){
rowscolor<-as.numeric(row.names(DF)[DF[,n]== v-1])
rowscolor<-rowscolor[!is.na(rowscolor)]
if (length(rowscolor) > 0) {
setCellStyle(XLSload, sheet = nameonglet, row = rowscolor + depR, col = n,cellstyle = get(VECTCOLOR[v]) ) }

	# barre de progression
	iter<-iter+1
	iterP<-round(iter*100/somiter)
	info <- paste(iterP, "% réalisé")
	setTkProgressBar(progressBar, iterP, paste("Mise en couleur de l'onglet : ", nameonglet), info)
	tcl("update")
	
Sys.sleep(0.01)
}}
saveWorkbook(XLSload)
}

close(progressBar)
tcl("update")
Sys.sleep(0.25)
rm(VERT, JAUNE, YELLOW, GREEN,ROSE)
gc()
tcl("wm", "attributes", tt, topmost=TRUE)
}



