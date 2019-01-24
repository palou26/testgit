## suppression des anciennes listes
rm(list=ls())

######################################
# PARTIE A PARAMETRER PAR UTILISATEUR
######################################

# Mentionner le chemin d'acc�s aux donn�es � traiter - il s'agit d'un fichier MDB
CHEMINDATA="W:/D4 - Coordination/OUTIL/S3E/v3 beta/AEAG SEEE 2009-2010/"  

# Mentionner le nom de la base access contenant les donn�es
FICHIERDATA="DCE_DATA_R.mdb" 

# Mentionner sur quel bassin se trouve les stations � traiter : AELB, AEAG, AERM, AERMC, AESN, AEAP, ODE971, ODE972, ODE973, ODE974
BASSIN="AEAG"

# Mentionner si l'�tat PCH est � faire --> "oui" / "non"
SEEEPCH="oui"

# Mentionner si l'�tat des polluants sp�cifiques est � faire --> "oui" / "non"
SEEEPS="oui"

# Exclure les polluants non synth�tiques de l'�tat PS final en raison de l'absence des fonds g�ochimiques --> "oui" / "non"
EXCLU_POLNS="oui"

# Mentionner si l'�tat biologique est � calculer --> "oui" / "non"
SEEEBIO="oui"

# Mentionner si l'�tat �cologique � la ME est � calculer --> "oui" / "non"
SEEEECOLOME="oui"

# Mentionner si l'�tat chimique est � calculer --> "oui" / "non"
SEEECHIM="oui"

###########################
# PARTIE NON PARAMETRABLE
##########################

source("W:\\D4 - Coordination\\OUTIL\\S3E\\v3 beta\\SEEE_CE.r")

