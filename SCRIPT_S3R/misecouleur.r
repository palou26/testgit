
#### FONCTION POUR MISE EN COULEUR DU TABLEAU EXCEL
##nomxls = nom du fichier EXCEL
##createxls = TRUE si le fichier Excel n'est pas cr��
##nameonglet = nom de l'onglet
##createsheet = TRUE si on souhaite cr�er l'index
## dfr = dataframe � exporter en Excel
## numcol = vecteur avec le numero des colonne o� il faut faire la mise en couleur. si numcol = 0 alors toute les colonne aura la mise en couleur
## depR = num�ro de la ligne o� le tableau va �tre import� (par defaut 1
## depC = num�ro de la colonne o� le tableau va �tre importer (par defaut 1)
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
print("cr�ation du xls et de l'onglet")

if (createxls == TRUE){
	#cr�ation du template
	XLSload <- loadWorkbook(nomXLS, create = TRUE)
	#cr�ation de l'onglet
	createSheet(XLSload, name = nameonglet)
	saveWorkbook(XLSload)
} else {
#chargement du template
XLSload <- loadWorkbook(nomXLS, create = FALSE)
	if (createsheet == TRUE){
	#cr�ation de l'onglet
	createSheet(XLSload, name = nameonglet)
	saveWorkbook(XLSload)
	}}


### EXPORT du dataframe vers le fichier Excel
print("Export du dataframe vers le fichier Excel")
setTkProgressBar(progressBar, 0, paste("Export : ", nameonglet), "0% r�alis�")
XLSload <- loadWorkbook(nomXLS) 
setTkProgressBar(progressBar, 25, paste("Export : ", nameonglet), "25% r�alis�")
writeWorksheet(XLSload, DF, sheet = nameonglet, startRow = depR, startCol = depC , header = entete)
setTkProgressBar(progressBar, 50, paste("Export : ", nameonglet), "50% r�alis�")
setColumnWidth(XLSload,nameonglet,1:ncol(DF),-1)
setTkProgressBar(progressBar, 75, paste("Export : ", nameonglet), "75% r�alis�")
Sys.sleep(0.2)
### largeur des colonnes en fonction du contenu
##SAVE
saveWorkbook(XLSload)
setTkProgressBar(progressBar, 100, paste("Export : ", nameonglet), "100% r�alis�")
tcl("update")
Sys.sleep(0.05)

#######################
### MISE EN COULEUR ##
#######################

if (MISECOULEUR=="oui"){
print("mise en couleur")
setTkProgressBar(progressBar, 0, paste("Mise en couleur de l'onglet : ", nameonglet), "")
tcl("update")
###chargement du fichier Excel
XLSload <- loadWorkbook(nomXLS) 

#On d�clare les couleurs
if (nbclasses == 4) {VECTCOLOR<-c("GREY", "GREEN","YELLOW","ORANGE","RED")}
if (nbclasses == 5) {VECTCOLOR<-c("GREY", "BLUE","GREEN","YELLOW","ORANGE","RED")}
if (nbclasses == 6) {VECTCOLOR<-c("GREY", "BLUE","GREEN","YELLOW","ORANGE","RED","BROWN")}


if (exists("BLUE")) { rm(BLUE)}
if (exists("RED")) { rm(RED)}
if (exists("YELLOW")) { rm(YELLOW)}
if (exists("GREEN")) { rm(GREEN)}
if (exists("ORANGE")) { rm(ORANGE)}
if (exists("GREY")) { rm(GREY)}

### dans ce cas, les styles ont �t� cr��s dans le template (or ca marche pas tr�s bien)
if (couleur ==1 ) {
BLUE<-getCellStyle(XLSload,"blue_e")
RED<-getCellStyle(XLSload,"red_e")
ORANGE<-getCellStyle(XLSload,"orange_e")
GREEN<-getCellStyle(XLSload,"green_e")
YELLOW<-getCellStyle(XLSload,"yellow_e")
BROWN<-getCellStyle(XLSload,"brown_e")


}

### dans ce cas, on prend les couleurs par d�faut qui existe dans library XLConnect
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

BROWN <- createCellStyle(XLSload, name = paste("BROWN3",nameonglet,sep="") )
setFillPattern(BROWN, fill = XLC$FILL.SOLID_FOREGROUND)
setFillForegroundColor(BROWN, color = XLC$COLOR.BROWN)

GREY <- createCellStyle(XLSload, name = paste("GREY3",nameonglet,sep="") )
setFillPattern(GREY, fill = XLC$FILL.SOLID_FOREGROUND)
setFillForegroundColor(GREY, color = XLC$COLOR.GREY_40_PERCENT)
}

##### On calcul le nombre total d'it�ration
nbcol<-length(VECTCOLOR)
iter<-0
if (numcol[1] == 0) {
numcol <- 2:ncol(DF)
somiter<-ncol(DF)*nbcol
} else {
somiter<-length(numcol)*nbcol }

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
	info <- paste(iterP, "% r�alis�")
	setTkProgressBar(progressBar, iterP, paste("Mise en couleur de l'onglet : ", nameonglet), info)
	tcl("update")
	
Sys.sleep(0.005)
}}
saveWorkbook(XLSload)
}

close(progressBar)
tcl("update")
Sys.sleep(0.1)
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
print("cr�ation du xls et de l'onglet")

if (createxls == TRUE){
	#cr�ation du template
	XLSload <- loadWorkbook(nomXLS, create = TRUE)
	#cr�ation de l'onglet
	createSheet(XLSload, name = nameonglet)
	saveWorkbook(XLSload)
} else {
#chargement du template
XLSload <- loadWorkbook(nomXLS, create = FALSE)
	if (createsheet == TRUE){
	#cr�ation de l'onglet
	createSheet(XLSload, name = nameonglet)
	saveWorkbook(XLSload)
	}}


### EXPORT du dataframe vers le fichier Excel
print("Export du dataframe vers le fichier Excel")
setTkProgressBar(progressBar, 0, paste("Export : ", nameonglet), "0% r�alis�")
XLSload <- loadWorkbook(nomXLS) 
setTkProgressBar(progressBar, 25, paste("Export : ", nameonglet), "25% r�alis�")
writeWorksheet(XLSload, DF, sheet = nameonglet, startRow = depR, startCol = depC , header = entete)
setTkProgressBar(progressBar, 50, paste("Export : ", nameonglet), "50% r�alis�")
setColumnWidth(XLSload,nameonglet,1:ncol(DF),-1)
setTkProgressBar(progressBar, 75, paste("Export : ", nameonglet), "75% r�alis�")
Sys.sleep(0.1)
### largeur des colonnes en fonction du contenu
##SAVE
saveWorkbook(XLSload)
setTkProgressBar(progressBar, 100, paste("Export : ", nameonglet), "100% r�alis�")
tcl("update")
Sys.sleep(0.1)

#######################
### MISE EN COULEUR ##
#######################

if (MISECOULEUR=="oui"){
print("mise en couleur")
setTkProgressBar(progressBar, 0, paste("Mise en couleur de l'onglet : ", nameonglet), "")
tcl("update")
###chargement du fichier Excel
XLSload <- loadWorkbook(nomXLS) 

#On d�clare les couleurs
VECTCOLOR<-c("GREY", "BLUE","RED")

if (exists("BLUE")) { rm(BLUE)}
if (exists("RED")) { rm(RED)}
if (exists("GREY")) { rm(GREY)}

### dans ce cas, les styles ont �t� cr��s dans le template (or ca marche pas tr�s bien)
if (couleur ==1 ) {
BLUE<-getCellStyle(XLSload,"blue_e")
RED<-getCellStyle(XLSload,"red_e")
}

### dans ce cas, on prend les couleurs par d�faut qui existe dans library XLConnect
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

##### On calcul le nombre total d'it�ration

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
	info <- paste(iterP, "% r�alis�")
	setTkProgressBar(progressBar, iterP, paste("Mise en couleur de l'onglet : ", nameonglet), info)
	tcl("update")
	
Sys.sleep(0.01)
}}
saveWorkbook(XLSload)
}

close(progressBar)
tcl("update")
Sys.sleep(0.05)
rm(BLUE, ORANGE, YELLOW, GREEN,RED)
gc()
tcl("wm", "attributes", tt, topmost=TRUE)
}

#######################################################################
##Fonction pour la coloration de la fiabilit� en 3 classe + ind�termin�
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
print("cr�ation du xls et de l'onglet")

if (createxls == TRUE){
	#cr�ation du template
	XLSload <- loadWorkbook(nomXLS, create = TRUE)
	#cr�ation de l'onglet
	createSheet(XLSload, name = nameonglet)
	saveWorkbook(XLSload)
} else {
#chargement du template
XLSload <- loadWorkbook(nomXLS, create = FALSE)
	if (createsheet == TRUE){
	#cr�ation de l'onglet
	createSheet(XLSload, name = nameonglet)
	saveWorkbook(XLSload)
	}}


### EXPORT du dataframe vers le fichier Excel
print("Export du dataframe vers le fichier Excel")
setTkProgressBar(progressBar, 0, paste("Export : ", nameonglet), "0% r�alis�")
XLSload <- loadWorkbook(nomXLS) 
setTkProgressBar(progressBar, 25, paste("Export : ", nameonglet), "25% r�alis�")
writeWorksheet(XLSload, DF, sheet = nameonglet, startRow = depR, startCol = depC , header = entete)
setTkProgressBar(progressBar, 50, paste("Export : ", nameonglet), "50% r�alis�")
setColumnWidth(XLSload,nameonglet,1:ncol(DF),-1)
setTkProgressBar(progressBar, 75, paste("Export : ", nameonglet), "75% r�alis�")
Sys.sleep(0.1)
### largeur des colonnes en fonction du contenu
##SAVE
saveWorkbook(XLSload)
setTkProgressBar(progressBar, 100, paste("Export : ", nameonglet), "100% r�alis�")
Sys.sleep(0.1)

#######################
### MISE EN COULEUR ##
#######################

if (MISECOULEUR=="oui"){
print("mise en couleur")
setTkProgressBar(progressBar, 0, paste("Mise en couleur de l'onglet : ", nameonglet), "")
###chargement du fichier Excel
XLSload <- loadWorkbook(nomXLS) 

#On d�clare les couleurs
VECTCOLOR<-c("GREY", "VERT","JAUNE","ROSE")

if (exists("VERT")) { rm(VERT)}
if (exists("JAUNE")) { rm(JAUNE)}
if (exists("ROSE")) { rm(ROSE)}
if (exists("GREY")) { rm(GREY)}

### dans ce cas, les styles ont �t� cr��s dans le template (or ca marche pas tr�s bien)
if (couleur ==1 ) {
VERT<-getCellStyle(XLSload,"VERT_e")
ROSE<-getCellStyle(XLSload,"ROSE_e")
JAUNE<-getCellStyle(XLSload,"JAUNE_e")



}

### dans ce cas, on prend les couleurs par d�faut qui existe dans library XLConnect
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

##### On calcul le nombre total d'it�ration
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
	info <- paste(iterP, "% r�alis�")
	setTkProgressBar(progressBar, iterP, paste("Mise en couleur de l'onglet : ", nameonglet), info)
	tcl("update")
	
Sys.sleep(0.01)
}}
saveWorkbook(XLSload)
}

close(progressBar)
tcl("update")
Sys.sleep(0.01)
rm(VERT, JAUNE, YELLOW, GREEN,ROSE)
gc()
tcl("wm", "attributes", tt, topmost=TRUE)
}



colorOuiNon<-function( nomxls, createxls = TRUE, nameonglet = "NEWONGLET", createsheet = TRUE,  dfr, numcol = 0, depR =1 , depC=1, entete = TRUE, couleur=1) { 
tcl("wm", "attributes", tt, topmost=FALSE) ## fenetre  en 1er plan
progressBar <- tkProgressBar("Export tableau", "",0, 100, 0)
tcl("update")
print(paste("mise en couleur de ", nomxls))
Sys.sleep(0.01)
nomXLS<-nomxls

## charger un fichier Excel et rajouter un onglet avec des valeurs
####
DF<-dfr
print("cr�ation du xls et de l'onglet")

if (createxls == TRUE){
	#cr�ation du template
	XLSload <- loadWorkbook(nomXLS, create = TRUE)
	#cr�ation de l'onglet
	createSheet(XLSload, name = nameonglet)
	saveWorkbook(XLSload)
} else {
#chargement du template
XLSload <- loadWorkbook(nomXLS, create = FALSE)
	if (createsheet == TRUE){
	#cr�ation de l'onglet
	createSheet(XLSload, name = nameonglet)
	saveWorkbook(XLSload)
	}}


### EXPORT du dataframe vers le fichier Excel
print("Export du dataframe vers le fichier Excel")
setTkProgressBar(progressBar, 0, paste("Export : ", nameonglet), "0% r�alis�")
XLSload <- loadWorkbook(nomXLS) 
setTkProgressBar(progressBar, 25, paste("Export : ", nameonglet), "25% r�alis�")
writeWorksheet(XLSload, DF, sheet = nameonglet, startRow = depR, startCol = depC , header = entete)
setTkProgressBar(progressBar, 50, paste("Export : ", nameonglet), "50% r�alis�")
setColumnWidth(XLSload,nameonglet,1:ncol(DF),-1)
setTkProgressBar(progressBar, 75, paste("Export : ", nameonglet), "75% r�alis�")
Sys.sleep(0.1)
### largeur des colonnes en fonction du contenu
##SAVE
saveWorkbook(XLSload)
setTkProgressBar(progressBar, 100, paste("Export : ", nameonglet), "100% r�alis�")
Sys.sleep(0.1)

#######################
### MISE EN COULEUR ##
#######################

if (MISECOULEUR=="oui"){
print("mise en couleur")
setTkProgressBar(progressBar, 0, paste("Mise en couleur de l'onglet : ", nameonglet), "")
###chargement du fichier Excel
XLSload <- loadWorkbook(nomXLS) 

#On d�clare les couleurs
VECTCOLOR<-c( "VERT","ROSE")

if (exists("VERT")) { rm(BLUE)}
if (exists("JAUNE")) { rm(YELLOW)}
if (exists("ROSE")) { rm(RED)}
if (exists("GREY")) { rm(GREY)}

### dans ce cas, les styles ont �t� cr��s dans le template (or ca marche pas tr�s bien)
if (couleur ==1 ) {
VERT<-getCellStyle(XLSload,"VERT_e")
ROSE<-getCellStyle(XLSload,"ROSE_e")

}

### dans ce cas, on prend les couleurs par d�faut qui existe dans library XLConnect
if (couleur ==0 ) {
VERT <- createCellStyle(XLSload, name = paste("VERT3",nameonglet,sep="") )
setFillPattern(VERT, fill = XLC$FILL.SOLID_FOREGROUND)
setFillForegroundColor(VERT, color = XLC$COLOR.LIGHT_GREEN)

ROSE <- createCellStyle(XLSload, name = paste("ROSE3",nameonglet,sep="") )
setFillPattern(ROSE, fill = XLC$FILL.SOLID_FOREGROUND)
setFillForegroundColor(ROSE, color = XLC$COLOR.ROSE)
}


##### On calcul le nombre total d'it�ration
iter<-0
if (numcol[1] == 0) {
numcol <- 2:ncol(DF)
somiter<-ncol(DF)*2
} else {
somiter<-length(numcol)*2 }

row.names(DF)<-1:nrow(DF)
##boucle sur le nombre de colonne et en fonction du nombre de couleurs
for ( n in numcol){
for ( v in c("oui","non")){
rowscolor<-as.numeric(row.names(DF)[DF[,n]== v])
rowscolor<-rowscolor[!is.na(rowscolor)]
if (length(rowscolor) > 0) {
if (v == "oui") {setCellStyle(XLSload, sheet = nameonglet, row = rowscolor + depR, col = n,cellstyle = get(VECTCOLOR[1]) ) }
if (v == "non") {setCellStyle(XLSload, sheet = nameonglet, row = rowscolor + depR, col = n,cellstyle = get(VECTCOLOR[2]) ) }

}

	# barre de progression
	iter<-iter+1
	iterP<-round(iter*100/somiter)
	info <- paste(iterP, "% r�alis�")
	setTkProgressBar(progressBar, iterP, paste("Mise en couleur de l'onglet : ", nameonglet), info)
	tcl("update")
	
Sys.sleep(0.005)
}}
saveWorkbook(XLSload)
}

close(progressBar)
tcl("update")
Sys.sleep(0.01)
rm(VERT, ROSE)
gc()
tcl("wm", "attributes", tt, topmost=TRUE)
}


