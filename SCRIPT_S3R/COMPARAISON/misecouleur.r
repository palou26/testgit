
library(tcltk)   ## library qui sert � faire les interface
library(tcltk2)
MISECOULEUR<-"oui"
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


colorXLS<-function( nomxls, createxls = TRUE, nameonglet = "NEWONGLET", createsheet = TRUE,  dfr, numcol = 0, depR =1 , depC=1, entete = TRUE, couleur=1) { 

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
Sys.sleep(0.75)
### largeur des colonnes en fonction du contenu
##SAVE
saveWorkbook(XLSload)
setTkProgressBar(progressBar, 100, paste("Export : ", nameonglet), "100% r�alis�")
Sys.sleep(0.5)

#######################
### MISE EN COULEUR ##
#######################
if (val_abs == "oui"){
	if (MISECOULEUR=="oui"){
	print("mise en couleur")
	setTkProgressBar(progressBar, 0, paste("Mise en couleur de l'onglet : ", nameonglet), "")
	###chargement du fichier Excel
	XLSload <- loadWorkbook(nomXLS) 

	#On d�clare les couleurs
	VECTCOLOR<-c("V1","V2","V3","V4")


	if (exists("V1")) { rm(RED)}
	if (exists("V2")) { rm(YELLOW)}
	if (exists("V3")) { rm(GREEN)}
	if (exists("V4")) { rm(ORANGE)}


	### dans ce cas, les styles ont �t� cr��s dans le template (or ca marche pas tr�s bien)
	if (couleur ==1 ) {
		# � faire

	}

	### dans ce cas, on prend les couleurs par d�faut qui existe dans library XLConnect
	if (couleur ==0 ) {

	V1 <- createCellStyle(XLSload, name = paste("V1",nameonglet,sep="") )
	setFillPattern(V1, fill = XLC$FILL.SOLID_FOREGROUND)
	setFillForegroundColor(V1, color = XLC$COLOR.LIGHT_YELLOW)

	V2 <- createCellStyle(XLSload, name = paste("V2",nameonglet,sep="") )
	setFillPattern(V2, fill = XLC$FILL.SOLID_FOREGROUND)
	setFillForegroundColor(V2, color = XLC$COLOR.YELLOW)

	V3 <- createCellStyle(XLSload, name = paste("V3",nameonglet,sep="") )
	setFillPattern(V3, fill = XLC$FILL.SOLID_FOREGROUND)
	setFillForegroundColor(V3, color = XLC$COLOR.ORANGE)

	V4 <- createCellStyle(XLSload, name = paste("V4",nameonglet,sep="") )
	setFillPattern(V4, fill = XLC$FILL.SOLID_FOREGROUND)
	setFillForegroundColor(V4, color = XLC$COLOR.RED)

	}
	print("mise en couleur")
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
	rowscolor<-as.numeric(row.names(DF)[abs(DF[,n])== v])
	rowscolor<-rowscolor[!is.na(rowscolor)]
	if (length(rowscolor) > 0) {
	setCellStyle(XLSload, sheet = nameonglet, row = rowscolor + depR, col = n,cellstyle = get(VECTCOLOR[v]) ) }

		# barre de progression
		iter<-iter+1
		iterP<-round(iter*100/somiter)
		info <- paste(iterP, "% r�alis�")
		setTkProgressBar(progressBar, iterP, paste("Mise en couleur de l'onglet : ", nameonglet), info)
		tcl("update")
		
	Sys.sleep(0.02)
	}}
	saveWorkbook(XLSload)

	}

}

if (val_abs != "oui"){
	if (MISECOULEUR=="oui"){
	print("mise en couleur")
	setTkProgressBar(progressBar, 0, paste("Mise en couleur de l'onglet : ", nameonglet), "")
	###chargement du fichier Excel
	XLSload <- loadWorkbook(nomXLS) 

	#On d�clare les couleurs
	VECTCOLOR<-c("Vm4","Vm3","Vm2","Vm1","V0","V1","V2","V3","V4")


	if (exists("V1")) { rm(RED)}
	if (exists("V2")) { rm(YELLOW)}
	if (exists("V3")) { rm(GREEN)}
	if (exists("V4")) { rm(ORANGE)}


	### dans ce cas, les styles ont �t� cr��s dans le template (or ca marche pas tr�s bien)
	if (couleur ==1 ) {
		# � faire

	}

	### dans ce cas, on prend les couleurs par d�faut qui existe dans library XLConnect
	if (couleur ==0 ) {

	Vm1 <- createCellStyle(XLSload, name = paste("Vm1",nameonglet,sep="") )
	setFillPattern(Vm1, fill = XLC$FILL.SOLID_FOREGROUND)
	setFillForegroundColor(Vm1, color = XLC$COLOR.LIGHT_GREEN)

	Vm2 <- createCellStyle(XLSload, name = paste("Vm2",nameonglet,sep="") )
	setFillPattern(Vm2, fill = XLC$FILL.SOLID_FOREGROUND)
	setFillForegroundColor(Vm2, color = XLC$COLOR.LIGHT_BLUE)

	Vm3 <- createCellStyle(XLSload, name = paste("Vm3",nameonglet,sep="") )
	setFillPattern(Vm3, fill = XLC$FILL.SOLID_FOREGROUND)
	setFillForegroundColor(Vm3, color = XLC$COLOR.ROYAL_BLUE)

	Vm4 <- createCellStyle(XLSload, name = paste("Vm4",nameonglet,sep="") )
	setFillPattern(Vm4, fill = XLC$FILL.SOLID_FOREGROUND)
	setFillForegroundColor(Vm4, color = XLC$COLOR.BLUE)
	
	V1 <- createCellStyle(XLSload, name = paste("V1",nameonglet,sep="") )
	setFillPattern(V1, fill = XLC$FILL.SOLID_FOREGROUND)
	setFillForegroundColor(V1, color = XLC$COLOR.LIGHT_YELLOW)

	V2 <- createCellStyle(XLSload, name = paste("V2",nameonglet,sep="") )
	setFillPattern(V2, fill = XLC$FILL.SOLID_FOREGROUND)
	setFillForegroundColor(V2, color = XLC$COLOR.YELLOW)

	V3 <- createCellStyle(XLSload, name = paste("V3",nameonglet,sep="") )
	setFillPattern(V3, fill = XLC$FILL.SOLID_FOREGROUND)
	setFillForegroundColor(V3, color = XLC$COLOR.ORANGE)

	V4 <- createCellStyle(XLSload, name = paste("V4",nameonglet,sep="") )
	setFillPattern(V4, fill = XLC$FILL.SOLID_FOREGROUND)
	setFillForegroundColor(V4, color = XLC$COLOR.RED)

	}
	print("mise en couleur")
	##### On calcul le nombre total d'it�ration
	iter<-0
	if (numcol[1] == 0) {
	numcol <- 2:ncol(DF)
	somiter<-ncol(DF)*8
	} else {
	somiter<-length(numcol)*8 }

	row.names(DF)<-1:nrow(DF)
	##boucle sur le nombre de colonne et en fonction du nombre de couleurs
	for ( n in numcol){
	for ( v in c(-4,-3,-2,-1,1,2,3,4) ){
	rowscolor<-as.numeric(row.names(DF)[(DF[,n])== v])
	rowscolor<-rowscolor[!is.na(rowscolor)]
	if (length(rowscolor) > 0) {
	setCellStyle(XLSload, sheet = nameonglet, row = rowscolor + depR, col = n,cellstyle = get(VECTCOLOR[5+v]) ) }

		# barre de progression
		iter<-iter+1
		iterP<-round(iter*100/somiter)
		info <- paste(iterP, "% r�alis�")
		setTkProgressBar(progressBar, iterP, paste("Mise en couleur de l'onglet : ", nameonglet), info)
		tcl("update")
		
	Sys.sleep(0.02)
	}}
	saveWorkbook(XLSload)

	}

}



print("export et mise en couleur ok")
close(progressBar)
Sys.sleep(0.5)
rm(V1, V2, V3, V4)
gc()
tcl("wm", "attributes", tt, topmost=TRUE)
xlcFreeMemory()
gc()
}






