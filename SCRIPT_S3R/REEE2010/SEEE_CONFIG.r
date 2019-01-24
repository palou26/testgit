## suppression des anciennes listes
rm(list=ls())

######################################
# PARTIE A PARAMETRER PAR UTILISATEUR
######################################

# Mentionner le chemin d'accès aux données à traiter - il s'agit d'un fichier MDB
CHEMINDATA="W:/D4 - Coordination/OUTIL/S3E/v3 beta/AEAG SEEE 2009-2010/"  

# Mentionner le nom de la base access contenant les données
FICHIERDATA="DCE_DATA_R.mdb" 

# Mentionner sur quel bassin se trouve les stations à traiter : AELB, AEAG, AERM, AERMC, AESN, AEAP, ODE971, ODE972, ODE973, ODE974
BASSIN="AEAG"

# Mentionner si l'état PCH est à faire --> "oui" / "non"
SEEEPCH="oui"

# Mentionner si l'état des polluants spécifiques est à faire --> "oui" / "non"
SEEEPS="oui"

# Exclure les polluants non synthétiques de l'état PS final en raison de l'absence des fonds géochimiques --> "oui" / "non"
EXCLU_POLNS="oui"

# Mentionner si l'état biologique est à calculer --> "oui" / "non"
SEEEBIO="oui"

# Mentionner si l'état écologique à la ME est à calculer --> "oui" / "non"
SEEEECOLOME="oui"

# Mentionner si l'état chimique est à calculer --> "oui" / "non"
SEEECHIM="oui"

###########################
# PARTIE NON PARAMETRABLE
##########################

source("W:\\D4 - Coordination\\OUTIL\\S3E\\v3 beta\\SEEE_CE.r")

