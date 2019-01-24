## suppression des anciennes listes
#rm(list=ls())
######################################
# PARTIE A PARAMETRER PAR UTILISATEUR
######################################


.libPaths(paste(racine,"R/library/",sep=""))


###########################
# INTERFACE ETAPE 1  ######
##########################




 ### Demande quels sont les indicateurs à calculer
### déclaration de l'interface et de la police des labels
tt2 <- tktoplevel()
tkwm.state(tt2,"withdrawn") 
tcl("update")
###icone
icone<-paste(racine,"/TEMPLATE/IMAGE/icone.ico", sep="")
if (file.exists(icone)) {tk2ico.setFromFile(tt2, icone)}
####
tkwm.geometry(tt2, "580x430" )
tkwm.geometry(tt2, "+55+45")
tkwm.resizable(tt2, FALSE, FALSE)
tkwm.title(tt2,"A propos")
tkconfigure(tt2, bg=colorbg)
fontlab <- tkfont.create(family="calibri",size=11,weight="bold",slant="italic")
fontvide <-tkfont.create(family="calibri",size=1,weight="bold",slant="italic")
fontversion <- tkfont.create(family="calibri",size=8,weight="bold",slant="italic")




####TITRE
frametitre <- tkframe(tt2)
tkconfigure(frametitre, bg=colorbg)
tkgrid(frametitre)
imagtitre <- tclVar()
fileimagetitre<-paste(racine,"/TEMPLATE/IMAGE/S3R.gif", sep="")
if (file.exists(fileimagetitre)) {tcl("image","create","photo",imagtitre,file=fileimagetitre)
	imgtitreAsLabel <- tklabel2(frametitre,image=imagtitre,bg="white")
	tkgrid(imgtitreAsLabel ,  sticky="w")
}

###FRAME2
tt2fr2 <- tkframe(tt2)
tkconfigure(tt2fr2, bg=colorbg)
tkgrid(tt2fr2)

tkgrid(tklabel2(tt2fr2,text="S3R a été conçu par Pascal PLUVINET (2013-2018) et Rémy MARTIN (2013-2014)",font=fontlab ) )
tkgrid(tklabel2(tt2fr2,text=" Il est le fruit d'une expérience",font=fontlab ) )
tkgrid(tklabel2(tt2fr2,text="de plusieurs années sur l'évaluation des états écologiques et chimiques",font=fontlab ) )
tkgrid(tklabel2(tt2fr2,text="des masses d'eau cours d'eau pour les acteurs majeurs du domaine.",font=fontlab ) )
tkgrid(tklabel2(tt2fr2,text="(Agences de l'eau, Offices de l'eau, Conseils généraux...)", font = fontlab))
tkgrid(tklabel2(tt2fr2,text="S3R tient compte des spécificités par bassin et offre plusieurs options avancées de calculs.", font = fontlab))
tkgrid(tklabel2(tt2fr2,text="    ", font = fontvide))
tkgrid(tklabel2(tt2fr2,text="    ", font = fontvide))
tkgrid(tklabel2(tt2fr2,text=paste("Vous utilisez la version ",numversion),font=fontlab ) )
tkgrid(tklabel2(tt2fr2,text=paste("Ce logiciel a été développé entièrement sous R (2.15.1)"),font=fontlab ) )
tcl("update")

tkgrid(tklabel2(tt2fr2,text="Contact : ppluvinet@yahoo.fr",font=fontlab ) )
tkgrid(tklabel2(tt2fr2,text="    ", font = fontvide))
	 

###FRAME3
tt2fr3 <- tkframe(tt2)
tkconfigure(tt2fr3, bg=colorbg)
tkgrid(tt2fr3)

# image1 <- tclVar()
# fileimage<-paste(racine,"/TEMPLATE/IMAGE/logo_Asco.gif", sep="")
# if (file.exists(fileimage)) {tcl("image","create","photo",image1,file=fileimage)
	# imgAsLabel <- tklabel2(tt2fr3,image=image1,bg="white" )
	# tkgrid(imgAsLabel ,  column=3, sticky="e")
# }




tkgrid(tklabel2(tt2fr3,text="    ", font = fontvide))
tkwm.state(tt2,"normal")
tkwait.window(tt2)






