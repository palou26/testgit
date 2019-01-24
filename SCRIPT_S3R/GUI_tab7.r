###########################
# INTERFACE DE RESULTATS  ######
##########################


## Creation de la fenetre
DEBUTGUI(Titre = "S3R Résultats")


###frame formulaire
frameform <- tkframe(tt)
tkconfigure(frameform, bg=colorbg)
tkgrid(frameform)
tkgrid(tklabel2(frameform,text="    ", font = fontvide))
tkgrid(tklabel2(frameform,text="    ", font = fontvide))

#############################
### boutons pour visualiser
############################
##fonction
voirExcelECOLO <- function(){ if (file.exists(XLSECOLO)) { 
tcl("wm", "attributes", tt, topmost=FALSE) ## fenetre  en 1er plan
browseURL(XLSECOLO)  
} else {tkmessageBox(message = "Le fichier Excel n'existe pas")} }

voirExcelCHIM <- function(){ if (file.exists(XLSCHIM)) { 
tcl("wm", "attributes", tt, topmost=FALSE) ## fenetre  en 1er plan
browseURL(XLSCHIM)  
} else {tkmessageBox(message = "Le fichier Excel n'existe pas")} }

voirExcelCONTA <- function(){ if (file.exists(XLSCONTA)) { 
tcl("wm", "attributes", tt, topmost=FALSE) ## fenetre  en 1er plan
browseURL(XLSCONTA)  
} else {tkmessageBox(message = "Le fichier Excel n'existe pas")} }


voirRepResultats <- function(){ 
print(CH_OUTPUT)
tcl("wm", "attributes", tt, topmost=FALSE) ## fenetre  en 1er plan
try(browseURL(CH_OUTPUT))  
} 



butRestart <- function(){ 
	if (file.exists(XLSECOLO) & file.exists(XLSCHIM)) { tkmessageBox(message = paste("Les résultats ont été exportés dans :\n",XLSECOLO,XLSCHIM )) 
	} else if (file.exists(XLSECOLO)) { tkmessageBox(message = paste("Les résultats ont été exportés dans :\n",XLSECOLO ))  
	} else if (file.exists(XLSCHIM)) { tkmessageBox(message = paste("Les résultats ont été exportés dans :\n",XLSCHIM )) }
	tkdestroy(tt)
	pathsite<-paste(pathS,"/",sep="")
	#on preprend la date
	SEEE_DEBformat<<-Sys.time()
	SEEE_DEBformat<<-gsub(" CET","",SEEE_DEBformat)
	SEEE_DEBformat<<-gsub("-","_",SEEE_DEBformat)
	SEEE_DEBformat<<-gsub(":","_",SEEE_DEBformat)
	SEEE_DEBformat<<-gsub(" ","_",SEEE_DEBformat)
	source(paste(pathS,'GUI_tab1.r',sep='')) 
}

butSortir <- function(){ 
	if (file.exists(XLSECOLO) & file.exists(XLSCHIM)) { tkmessageBox(message = paste("Les résultats ont été exportés dans :\n",XLSECOLO,XLSCHIM )) 
	} else if (file.exists(XLSECOLO)) { tkmessageBox(message = paste("Les résultats ont été exportés dans :\n",XLSECOLO ))  
	} else if (file.exists(XLSCHIM)) { tkmessageBox(message = paste("Les résultats ont été exportés dans :\n",XLSCHIM )) } 
	tkdestroy(tt)
	#source(paste(pathsite,"GUI.r",sep="") , echo = TRUE, print.eval = TRUE)
}
#chargement des images
logoXLS <- tclVar()
tcl("image","create","photo",logoXLS,file=paste(racine,"/TEMPLATE/IMAGE/excel_logo.gif", sep=""))
logoREP <- tclVar()
tcl("image","create","photo",logoREP,file=paste(racine,"/TEMPLATE/IMAGE/rep_logo.gif", sep=""))
logorestart<- tclVar()
tcl("image","create","photo",logorestart,file=paste(racine,"/TEMPLATE/IMAGE/Restart_logo.gif", sep=""))
logoend<- tclVar()
tcl("image","create","photo",logoend,file=paste(racine,"/TEMPLATE/IMAGE/End.gif", sep=""))

##création bouton
fontbouton <- tkfont.create(family="calibri",size=13,weight="bold")
voirExcelECOLO.but <- tkbutton(frameform,text=" Excel ",command=voirExcelECOLO, font  = fontbouton, image = logoXLS) 
voirExcelCHIM.but <- tkbutton(frameform,text=" Excel ",command=voirExcelCHIM, font  = fontbouton, image = logoXLS) 
voirExcelCONTA.but <- tkbutton(frameform,text=" Excel ",command=voirExcelCONTA, font  = fontbouton, image = logoXLS) 
voirRepResultats.but <- tkbutton(frameform,text=" Rep ",command=voirRepResultats, font  = fontbouton, image = logoREP) 
butRestart.but <- tkbutton(frameform,text=" RECOMMENCER ",command=butRestart, font  = fontbouton, image = logorestart) 
butSortir.but <- tkbutton(frameform,text=" TERMINER ",command=butSortir, font  = fontbouton, image = logoend) 

##Affichage bouton
tkgrid(tklabel2(frameform,text="    ", font = fontvide))
tkgrid(tklabel3(frameform,text=paste(" RESULTATS :  "), font=fontlabrad),sticky="w")
tkgrid(tklabel2(frameform,text="    ", font = fontvide))

if(SEEEPCH=="oui" | SEEEPS=="oui" | SEEEBIO=="oui"){
	tkgrid(tklabel2(frameform,text="    ", font = fontvide))
	tkgrid(tklabel2(frameform,text=paste(" - Etat Ecologique (PCH, PS, BIO) :  "), font=fontbold2),voirExcelECOLO.but,sticky="w")
}
if(SEEECHIM=="oui"){
	tkgrid(tklabel2(frameform,text="    ", font = fontvide))
	tkgrid(tklabel2(frameform,text=paste(" - Etat Chimique:  "), font=fontbold2),voirExcelCHIM.but,sticky="w")
}
if(CONTA=="oui"){
	tkgrid(tklabel2(frameform,text="    ", font = fontvide))
	tkgrid(tklabel2(frameform,text=paste(" - Module Contamination:  "), font=fontbold2),voirExcelCONTA.but,sticky="w")
}
tkgrid(tklabel2(frameform,text="    ", font = fontvide))
tkgrid(tklabel2(frameform,text=paste(" - Dossier RESULTATS:  "), font=fontbold2),voirRepResultats.but,sticky="w")
tkgrid(tklabel2(frameform,text="    ", font = fontvide))
tkgrid(tklabel2(frameform,text=paste(" - Relancer l'Application :          "), font=fontbold2),butRestart.but,sticky="w")
tkgrid(tklabel2(frameform,text="    ", font = fontvide))
tkgrid(tklabel2(frameform,text=paste(" - Sortir de l'Application :          "), font=fontbold2),butSortir.but,sticky="w")




tkconfigure(butRestart.but ,cursor="hand2")     
tkconfigure(butSortir.but ,cursor="hand2")                                                           
tkconfigure(voirExcelECOLO.but ,cursor="hand2")                                                            
tkconfigure(voirExcelCHIM.but ,cursor="hand2")                                                            

###frame formulaire
frameform2 <- tkframe(tt)
tkconfigure(frameform2, bg=colorbg)
tkgrid(frameform2)
tkgrid(tklabel2(frameform2,text="    ", font = fontvide))
tkgrid(tklabel2(frameform2,text="    ", font = fontvide))
tkgrid(tklabel2(frameform2,text="    ", font = fontvide))

##############AFFICHER L'HEURE ET DATE DE FIN

tkgrid(tklabel2(frameform2,text="    ", font = fontvide))
tkgrid(tklabel2(frameform2,text=paste(" Durée du traitement : ", DUREE_CALC_min, " minutes ", DUREE_CALC_sec, " sec."), font=fontlabrad), sticky="e")
tkgrid(tklabel2(frameform2,text="    ", font = fontvide))
tkgrid(tklabel2(frameform2,text="    ", font = fontvide))
tkgrid(tklabel2(frameform2,text="    ", font = fontvide))
tkconfigure(frameform2,cursor="arrow")
tkwm.state(tt,"normal")
tcl("update")


tcl("wm", "attributes", tt, topmost=FALSE)
tkwait.window(tt)
