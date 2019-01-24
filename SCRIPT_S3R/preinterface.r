###PREINTERFACE
###############

#######################################################################################################
###Ce script parmet de parametrer les interfaces utilisateur (GUI) et de créer les fonctions adéquates
#######################################################################################################


## Import de la librairie permettant de faire les interface 
library(tcltk2)

#Si la taille de l'écran (résolution ) est faible , on réduit le scaling
#if (as.numeric(substr(shell("wmic desktopmonitor get screenheight", intern = T)[2],1,6)) < 1000){
#.Tcl("tk scaling 0.976")  # au cas où on souhaite rétrécir la fenetre
#}

################ C'est ici qu'on paramètre certains code couleur (fond, boutond, couleur de la police (colorblue). 
colorbg<-"#d4e6eb"     
colorbgbut<-"#dbe4ea"
colorblue<-"#514e57"  
colortitre<-"#aa1352" 
colorduS<-"#0092a5"  


## Déclalation des Polices des labels

fontnormal<-tkfont.create(family="calibri",size=11)
fontlabrad <- tkfont.create(family="calibri",size=13,weight="bold",slant="italic")
fontvide <-tkfont.create(family="Arial Narrow",size=1)
fontvide2 <-tkfont.create(family="Arial Narrow",size=5)
fontversion <- tkfont.create(family="calibri",size=8,weight="bold",slant="italic")
fontboutonp <- tkfont.create(family="calibri",size=10,weight="bold")
fontbouton <- tkfont.create(family="calibri",size=15,weight="bold")
fontsstitre <- tkfont.create(family="calibri",size=13,weight="bold",slant="italic")
fontbold <- tkfont.create(family="calibri",size=9,weight="bold")
fontbolditalic <- tkfont.create(family="calibri",size=10,weight="bold",slant="italic")
fontsmallitalic <- tkfont.create(family="calibri",size=8,slant="italic")
fontchemin<- tkfont.create(family="calibri",size=10)
fontbold2 <- tkfont.create(family="calibri",size=11,weight="bold")


## on crée des nouvelles fonctions (label, bouton, ... qui integregrerons les fond en couleur. Sinon par défault gris.
tklabel2<-function(...){tklabel(...,bg=colorbg)}
tklabel3<-function(...){tklabel2(...,foreground=colortitre )}

tkcheckbutton2<-function(...){tkcheckbutton(...,bg=colorbg)}
tkbutton2<-function(...){tkbutton(...,bg=colorbgbut)}   #,  relief = "flat"


DEBUTGUI<-function(Titre = "", geom = "") {
	### déclaration de l'interface 
	tt <<- tktoplevel()
	tcl("wm", "attributes", tt, topmost=TRUE) ## fenetre  en 1er plan
	## fenetre  en 1er plan
	tkwm.state(tt,"withdrawn") ## l'interface se crée en mode caché

	### petite icone de la fenêtre

	icone<<-paste(racine,"/TEMPLATE/IMAGE/icone.ico", sep="")
	tkwm.iconbitmap(tt, "-default", icone)
	if (file.exists(icone)) {tk2ico.setFromFile(tt, icone)}

	#### quelques paramètrs de la fenêtre
	if ( geom != "")  {tkwm.geometry(tt, geom ) } 
	tkwm.geometry(tt, "+180+150") #position à l'écran
	tkwm.resizable(tt, FALSE, FALSE) ## ne permet pas de modifier la taille de la fenêtre = NON extensible
	tkwm.title(tt,Titre) ## titre de la fenêtre
	tkconfigure(tt, bg=colorbg)  ## afffecte la couleurs de fond

	####TITRE = importation d'une image
	frametitre <- tkframe(tt)  # création d'un frame
	tkconfigure(frametitre, bg=colorbg)
	tkgrid(frametitre)
	imagtitre <- tclVar()
	if (BASSININI == "AESN") fileimagetitre<-paste(racine,"/TEMPLATE/IMAGE/S3RAESN.gif", sep="")
	if (BASSININI != "AESN") fileimagetitre<-paste(racine,"/TEMPLATE/IMAGE/S3R.gif", sep="")
	if (file.exists(fileimagetitre)) {
		tcl("image","create","photo",imagtitre,file=fileimagetitre)
		imgtitreAsLabel <- tklabel2(frametitre,image=imagtitre,bg="white")
		tkgrid(imgtitreAsLabel ,  sticky="w")
	}
	tcl("update")
	gc()
}

# Permettre d'avoir une icone S3R
tkmessageBox2<-tkmessageBox
tkmessageBox<-function(...) {tkmessageBox2(...,parent = tt) }


##On change un petit peu la fonction  de bare de défilement


tkProgressBar<-function (title = "R progress bar", label = "", min = 0, max = 1, 
    initial = 0, width = 300) 
{
    useText <- FALSE
    have_ttk <- as.character(tcl("info", "tclversion")) >= "8.5"
    if (!have_ttk && as.character(tclRequire("PBar")) == "FALSE") 
        useText <- TRUE
	.win <- tktoplevel()
	tkwm.iconbitmap(.win, "-default", icone)
    .val <- initial
    .killed <- FALSE
    tkwm.geometry(.win, sprintf("%dx80", width + 40))
    tkwm.title(.win, title)
    fn <- tkfont.create(family = "helvetica", size = 12)
    if (useText) {
        .lab <- tklabel(.win, text = label, font = fn, padx = 20)
        tkpack(.lab, side = "left")
        fn2 <- tkfont.create(family = "helvetica", size = 16)
        .vlab <- tklabel(.win, text = "0%", font = fn2, padx = 20)
        tkpack(.vlab, side = "right")
        up <- function(value) {
            if (!is.finite(value) || value < min || value > max) 
                return()
            .val <<- value
            tkconfigure(.vlab, text = sprintf("%d%%", round(100 * 
                (value - min)/(max - min))))
        }
    }
    else {
        .lab <- tklabel(.win, text = label, font = fn, pady = 10)
        .tkval <- tclVar(0)
        tkpack(.lab, side = "top")
        tkpack(tklabel(.win, text = "", font = fn), side = "bottom")
        pBar <- if (have_ttk) 
            ttkprogressbar(.win, length = width, variable = .tkval)
        else tkwidget(.win, "ProgressBar", width = width, variable = .tkval)
        tkpack(pBar, side = "bottom")
        up <- function(value) {
            if (!is.finite(value) || value < min || value > max) 
                return()
            .val <<- value
            tclvalue(.tkval) <<- 100 * (value - min)/(max - min)
        }
    }
    getVal <- function() .val
    kill <- function() if (!.killed) {
        tkdestroy(.win)
        .killed <<- TRUE
    }
    title <- function(title) tkwm.title(.win, title)
    lab <- function(label) tkconfigure(.lab, text = label)
    tkbind(.win, "<Destroy>", kill)
    up(initial)
    structure(list(getVal = getVal, up = up, title = title, label = lab, 
        kill = kill), class = "tkProgressBar")
}


