

#import des données
GRILLE<-read.csv2("D:/_ETUDES/E4309_AESN_S3R_Adapt2016/S3R_DEV/S3R/PARAMETRES/DEFAUT/GRILLECONTAMINATION.csv")
SIRIS<-read.csv2("D:/_ETUDES/E4309_AESN_S3R_Adapt2016/S3R_DEV/S3R/PARAMETRES/DEFAUT/siris_2012.csv")

table(GRILLE$PESTICIDE)


GRILLE$PESTICIDE<-""
GRILLE$PESTICIDE[GRILLE$PARAMETRE %in% SIRIS$Code.Sandre[!is.na(SIRIS$Code.Sandre)]  & !is.na(GRILLE$CAS)  ]<-"oui"
table(GRILLE$PESTICIDE)
head(GRILLE)


GRILLE$PESTICIDE[GRILLE$CAS %in% SIRIS$CAS.SIRIS.2012[!is.na(SIRIS$CAS.SIRIS.2012)] & !is.na(GRILLE$CAS) & GRILLE$CAS != ""    ]<-"oui"
table(GRILLE$PESTICIDE)
head(GRILLE)


write.csv2(GRILLE, "D:/_ETUDES/E4309_AESN_S3R_Adapt2016/S3R_DEV/S3R/PARAMETRES/DEFAUT/GRILLECONTAMINATION.csv")



