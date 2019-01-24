

###########################
# INTERFACE ETAPE 2 COMPARAISON ######
##########################


### déclaration de la fenêtre
tt <- tktoplevel()
tcl("wm", "attributes", tt, topmost=TRUE) ## fenetre  en 1er plan
tkwm.state(tt,"withdrawn")  #mode caché
###icone
icone<-paste(racine,"/TEMPLATE/IMAGE/icone.ico", sep="")
if (file.exists(icone)) {tk2ico.setFromFile(tt, icone)}
####
tkwm.geometry(tt, "575x495") #tailel de la fenêtre
tkwm.geometry(tt, "+180+150") #placement sur l'écran
tkwm.resizable(tt, FALSE, FALSE) ## non extensible
tkwm.title(tt,"Module de comparaison") #titre de la fenêtre
tkconfigure(tt, bg=colorbg) ##configure la couleur de fond

## choix des police
fontchemin<- tkfont.create(family="calibri",size=10)

####TITRE = import de l'image en GIF
frametitre <- tkframe(tt)
tkconfigure(frametitre, bg=colorbg)
tkgrid(frametitre)
imagtitre <- tclVar()
fileimagetitre<-paste(racine,"/TEMPLATE/IMAGE/S3R.gif", sep="")
if (file.exists(fileimagetitre)) {tcl("image","create","photo",imagtitre,file=fileimagetitre)
	imgtitreAsLabel <- tklabel2(frametitre,image=imagtitre,bg="white")
	tkgrid(imgtitreAsLabel ,  sticky="w")
}
tcl("update")
tkgrid(tklabel2(tt,text="    ", font = fontvide))
tkgrid(tklabel2(tt,text="    ", font = fontvide))

###frame formulaire
frameform <- tkframe(tt)
tkconfigure(frameform, bg=colorbg)
tkgrid(frameform)
tkgrid(tklabel2(frameform,text="    ", font = fontvide))
tkgrid(tklabel2(frameform,text="    ", font = fontvide))

### Choix du fichier file1 . Le nom du fichier sera attribué à NAMErdata1
if (!exists("NAMErdata1")) {NAMErdata1<-""}

	
	getfilefile1 <- function() {
	tcl("wm", "attributes", tt, topmost=FALSE)
	NAMErdata1 <<- tclvalue(tkgetOpenFile(
	filetypes = " {{Rdata Files} {.Rdata}} "))
		if (!nchar(NAMErdata1)) {
		tkmessageBox(message = "Aucun fichier sélectionné", icon = "warning")
		} else {
			tkconfigure(entrychfile1,textvariable=tclVar(NAMErdata1))  # permet de mettre à jour le label avec le chemin du fichier csv
			tcl("update")
		}
		tcl("wm", "attributes", tt, topmost=TRUE) ## fenetre  en 1er plan
	}
	 ## label et bouton
	labselectfile<-tklabel3(frameform,text="FICHIER 1: ",font=fontlabrad )
	buttonselfile1 <- tkbutton2(frameform, text = " ... ", command = getfilefile1 )
	tkgrid(labselectfile,  sticky="w")
		
	##permet de mettre le nom du chemin en vert
	entrychfile1 <- tkentry(frameform, width = 73,textvariable = NAMErdata1)
	tkconfigure(entrychfile1,textvariable=tclVar(NAMErdata1))
	tkgrid( entrychfile1, tklabel2(frameform,text="   "), buttonselfile1 ,sticky="w")
	tkconfigure(buttonselfile1 ,cursor="hand2")
	tkgrid(tklabel2(frameform,text="    ", font = fontvide))
	tkgrid(tklabel2(frameform,text="    ", font = fontvide))
	tkgrid(tklabel2(frameform,text="    ", font = fontvide))




if (!exists("NAMErdata2")) {NAMErdata2<-""}

		
		getfilefile2 <- function() {
		tcl("wm", "attributes", tt, topmost=FALSE)
			NAMErdata2 <<- tclvalue(tkgetOpenFile(
			filetypes = " {{Rdata Files} {.Rdata}} "))
			if (!nchar(NAMErdata2)) {
				tkmessageBox(message = "Aucun fichier sélectionné" , icon = "warning")
			} else {
				tkconfigure(entrychfile2,textvariable=tclVar(NAMErdata2))  # permet de mettre à jour le label avec le chemin du fichier csv
				tcl("update")

			}
		tcl("wm", "attributes", tt, topmost=TRUE) ## fenetre  en 1er plan
		}

		  ## label et bouton
		labselectfile<-tklabel3(frameform,text="FICHIER 2: ",font=fontlabrad )
		buttonselfile2 <- tkbutton2(frameform, text = " ... ", command = getfilefile2 )
		tkgrid(labselectfile,  sticky="w")
		
		  ##permet de mettre le nom du chemin en vert
		entrychfile2 <- tkentry(frameform, width = 73,textvariable = NAMErdata2)
		tkconfigure(entrychfile2,textvariable=tclVar(NAMErdata2))
		tkgrid( entrychfile2, tklabel2(frameform,text="   "), buttonselfile2 ,sticky="w")
		tkconfigure(buttonselfile2 ,cursor="hand2")
		tkgrid(tklabel2(frameform,text="    ", font = fontvide))
		tkgrid(tklabel2(frameform,text="    ", font = fontvide))
		tkgrid(tklabel2(frameform,text="    ", font = fontvide))

		
		### check box OUI/non pour faire donner la valeur absole du résultat de la différence
		cb1 <- tkcheckbutton2(frameform)
		cbValue1 <- tclVar(0)
		tkconfigure(cb1,variable=cbValue1)
		tkgrid( tklabel2(frameform,text="  Exporte les différences en valeur absolue    ") , 
		tklabel2(frameform,text="    ", font = fontvide),
		cb1, sticky="w")
		tkgrid(tklabel2(frameform,text="    ", font = fontvide))
		tkgrid(tklabel2(frameform,text="    ", font = fontvide))


###FRAME BOUTONS
## affichage des bouton
framebut <- tkframe(tt)
tkconfigure(framebut, bg=colorbg)


### FONCTION BOUTON
OnOK_comparaison <- function()  ## enregistress les options dans des objets et va lire les CSV. Vérifie que le fichier a un nombe de colonne cohérent
{
	NR<-nchar(paste(NAMErdata1,NAMErdata2,sep=""))
	print(NR)
	sortie<-1
	pathsitecompa<<-pathsite
	if (as.character((tclvalue(cbValue1))) == "1") { val_abs <<- "oui" } else { val_abs <<- "non"  }
	
		if (NAMErdata1 != "" ) {  ## si un fichier a bien été sélectionné
			tkconfigure(tt,cursor="watch")
			load(NAMErdata1, .GlobalEnv)
			for (i in listobject_to_save){
			assign(paste0(i,"_1"), get(i), envir = .GlobalEnv)
			rm(list=i, envir = .GlobalEnv)
			}
			gc()
		} else {tkmessageBox(message = "Veuillez importer le fichier 1") ; sortie <-0}

		if (NAMErdata2 != "" ) {  ## si un fichier a bien été sélectionné
			tkconfigure(tt,cursor="watch")
			load(NAMErdata2, .GlobalEnv)
			for (i in listobject_to_save){
			assign(paste0(i,"_2"), get(i), envir = .GlobalEnv)
			rm(list=i, envir = .GlobalEnv)
			}
			gc()
		} else {tkmessageBox(message = "Veuillez importer le fichier 2") ; sortie <-0}

		if(sortie==1){
		tkconfigure(tt,cursor="arrow")
		gc()
		

		
		source(paste(pathsitecompa,"comparaison.r",sep="") , echo = TRUE, print.eval = TRUE)
		gc()
		rm(tt)
		tkdestroy(tt)
		
		pathsite<<-gsub("COMPARAISON/","",pathsitecompa)
		source(paste(pathsite,"GUI_tab1.r",sep="") , echo = TRUE, print.eval = TRUE)
		gc()

		} 
	
}
fontbouton <- tkfont.create(family="calibri",size=15,weight="bold")
OKbut <- tkbutton2(framebut,text="  >  ",command=OnOK_comparaison, font  = fontbouton ) 

## Fonction pour bouton PRECEDENT
Retour<-function()  ## enregistress les options dans des objets et va lire les CSV
{
tkdestroy(tt)
pathsite<<-gsub("COMPARAISON/","",pathsite)
source(paste(pathsite,"GUI_tab1.r",sep="") , echo = TRUE, print.eval = TRUE)
}
returnbut <- tkbutton2(framebut,text="  <  ",command=Retour, font  = fontbouton ) 


##bouton Aide
fct_Aide <-function() { browseURL(paste(AidePath,"/comparaison.htm",sep=""))}
AideBut <- tkbutton2(framebut,text="  ?  ",command=fct_Aide, font  = fontbouton) 



tkgrid(framebut)
tkgrid(returnbut,OKbut,tklabel2(framebut,text="", font = fontvide), AideBut)
tkconfigure(OKbut ,cursor="hand2")
tkconfigure(returnbut ,cursor="hand2")
tkconfigure(AideBut ,cursor="hand2")
tkgrid(tklabel2(framebut,text="    ", font = fontvide))
tkgrid(tklabel2(framebut,text="    ", font = fontvide))
tkgrid(tklabel2(framebut,text="    ", font = fontvide))



tkwm.state(tt,"normal")
tcl("wm", "attributes", tt, topmost=FALSE)
tkwait.window(tt)
#tkfocus(tt)





