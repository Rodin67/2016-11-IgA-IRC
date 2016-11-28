#-----------------------------------------------------------------------------------Data-----------------------------------------------------
library("survival", lib.loc="C:/Program Files/R/R-3.3.2/library")
library("readxl", lib.loc="~/R/win-library/3.3")
library("tidyr", lib.loc="~/R/win-library/3.3")
library("dplyr", lib.loc="~/R/win-library/3.3")
library("prettyR", lib.loc="~/R/win-library/3.3")
library("ggplot2", lib.loc="~/R/win-library/3.3")
library("corrplot", lib.loc="~/R/win-library/3.3")
library("epitools", lib.loc="~/R/win-library/3.3")

.pardefault <- par(no.readonly = T)
options(max.print = 99999999)

## _iga----
iga <- read_excel("~/IRC/dataIgA/donnes patients IgA.xlsx", 
                  col_types = c("text", "text", "text", 
                                "text", "numeric", "text", "numeric", 
                                "text", "text", "numeric", "text", 
                                "text", "text", "text", "text", "text", 
                                "numeric", "text", "text", "text", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "text", "text", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "text", "numeric", "text", "text", 
                                "text", "text", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "text", "text", "text", 
                                "text", "text", "text", "numeric", 
                                "numeric", "text", "text", "text", 
                                "text", "numeric", "text", "text", 
                                "numeric", "numeric", "text", "text", 
                                "text", "text", "date", "date", "date", 
                                "date", "date", "date", "date", 
                                "date", "date", "date"))
iga<-iga[!iga$anirt<2010,] # On supprime les patients qui ont eu une leur 1er ttt de supppléance avant 2010
iga$anirt<-as.factor(iga$anirt)
iga$age<-as.numeric(iga$age)
iga<-iga[!iga$age<16,] # On supprime les patients inf à 16 ans
iga$DATE_EVT<-as.Date(iga$DATE_EVT, origin = "1899-12-30")
iga$FAV_DATE<-as.Date(iga$FAV_DATE, origin = "1899-12-30")
iga$DDIRT<-as.Date(iga$DDIRT, origin = "1899-12-30")
iga$DNAIS<-as.Date(iga$DNAIS, origin = "1899-12-30")
iga$DINSCMED<-as.Date(iga$DINSCMED, origin = "1899-12-30")
iga$dgrf<-as.Date(iga$dgrf, origin = "1899-12-30")
iga$dsvr<-as.Date(iga$dsvr, origin = "1899-12-30")
iga$ddc<-as.Date(iga$ddc, origin = "1899-12-30")
iga$dpdv<-as.Date(iga$dpdv, origin = "1899-12-30")
iga$DATE_DERNOUV<-as.Date(iga$DATE_DERNOUV, origin = "1899-12-30")
iga$delai_dernouv<-as.numeric(iga$delai_dernouv)
iga$delailna<-as.numeric(iga$delailna)
iga$delaitx<-as.numeric(iga$delaitx)
iga$delaidc<-as.numeric(iga$delaidc)
iga$RES_DEP_LIB<-as.factor(iga$RES_DEP_LIB)
iga$RES_REG_LIB<-as.factor(iga$RES_REG_LIB)
iga$bmi<-as.numeric(iga$bmi)
iga$classage<-as.factor(iga$classage)
iga$classage<-cut(iga$age, breaks = c(16,seq(20,95,5)), right = F)
iga$agegp<-as.factor(iga$agegp)
iga$AUTCOMOR3_LIB<-as.factor(iga$AUTCOMOR3_LIB)
iga$AUTCOMOR3_COD<-as.factor(iga$AUTCOMOR3_COD)
iga$AUTCOMOR2_LIB<-as.factor(iga$AUTCOMOR2_LIB)
iga$AUTCOMOR2_COD<-as.factor(iga$AUTCOMOR2_COD)
iga$AUTCOMOR1_LIB<-as.factor(iga$AUTCOMOR1_LIB)
iga$AUTCOMOR1_COD<-as.factor(iga$AUTCOMOR1_COD)
iga$HB<-as.numeric(iga$HB)
iga$HBINI<-as.numeric(iga$HBINI)
iga$gr_HBINI<-NA
iga$gr_HBINI[iga$sex==1]<-cut(iga$HBINI[iga$sex==1], breaks = c(0,13,max(iga$HBINI, na.rm = T)+1), right = F)
iga$gr_HBINI[iga$sex==2]<-cut(iga$HBINI[iga$sex==2], breaks = c(0,12,max(iga$HBINI, na.rm = T)+1), right = F)
iga$gr_HBINI<-factor(iga$gr_HBINI, levels = c(1,2), labels = c(1,0))
#table(iga$gr_HBINI, useNA = "always")
iga$ALBI_MTH<-factor(iga$ALBI_MTH, labels = c("Automate","Electrophorèse","ND","Néphélémétrie","Colorimétrique "))
iga$ALB_MTH<-factor(iga$ALB_MTH, labels = c("Automate","Electrophorèse","ND","Néphélémétrie","Colorimétrique "))
iga$ALB<-as.numeric(iga$ALB)
iga$PDS<-as.numeric(iga$PDS)
iga$METHOn<-factor(iga$METHOn, labels = c("Hémodialyse","Dialyse péritonéale","Greffe"))

#iga$nephgp<-NULL # N'a que le facteur "gnc" et pas de donnée manquante
#iga$liste_longue<-NULL # N'a que le facteur "Néphropathie à dépôts d'IgA" et pas de donnée manquante
iga$ALBI_MTH<-as.factor(iga$ALBI_MTH)
iga$ALBINI<-as.numeric(iga$ALBINI)
iga$CAUSENEPH2_LIB<-as.factor(iga$CAUSENEPH2_LIB)
iga$CAUSENEPH2_COD<-as.factor(iga$CAUSENEPH2_COD)
iga$CAUSENEPH1_LIB<-as.factor(iga$CAUSENEPH1_LIB)
iga$CAUSENEPH1_COD<-as.factor(iga$CAUSENEPH1_COD)
iga$NEPH_LIB<-as.factor(iga$NEPH_LIB)
iga$NEPH_COD<-as.factor(iga$NEPH_COD)
iga$EQD_REG_COD<-as.factor(iga$EQD_REG_COD)
#iga$EQD_PAYS_LIB<-NULL # N'a que le facteur "France" et pas de donnée manquante
iga$EQD_DEP_LIB<-as.factor(iga$EQD_DEP_LIB)
iga$EQD_DEP_COD<-as.factor(iga$EQD_DEP_COD)
iga$EQD_REG_LIB<-as.factor(iga$EQD_REG_LIB)
iga$RREC_COD<-as.factor(iga$RREC_COD)
iga$CAUSDCP_LIB<-as.factor(iga$CAUSDCP_LIB)
iga$CAUSDCP_COD<-as.factor(iga$CAUSDCP_COD)
iga$CAUSEDCP_A_LIB<-as.factor(iga$CAUSEDCP_A_LIB)
iga$CAUSEDCP_A_COD<-as.factor(iga$CAUSEDCP_A_COD)
iga$TABACn <- factor(iga$TABACn, labels = c("NF","Fumeur","EX Fumeur"))
iga$VAVn <- factor(iga$VAVn, labels = c("FAV native","Cathéter tunnélisé","Pontage","Autre"))
iga$ACTIVn<-factor(iga$ACTIVn, labels = c("Actif temps plein","Actif temps partiel","Actif en milieu protégé","Retraité","Au chômage","Au foyer","Scolarisé, étudiant","Arrêt longue maladie","Inactif en invalidité","Inactif autre"))

## _greffe----
greffe <- read_excel("~/IRC/dataIgA/greffe IgA.xlsx", col_types = c("numeric", "text", "date", "date", "date", "date", "date", "numeric", "numeric",
                                                                    "date", "date", "text", "text", "numeric", "text", "text", "text", "text",
                                                                    "text", "text", "numeric"))
greffe$ARF1<-as.Date(greffe$ARF1, origin = "1899-12-30")
greffe$ARF2<-as.Date(greffe$ARF2, origin = "1899-12-30")
greffe$GRF1<-as.Date(greffe$GRF1, origin = "1899-12-30")
greffe$GRF2<-as.Date(greffe$GRF2, origin = "1899-12-30")
greffe$GRF3<-as.Date(greffe$GRF3, origin = "1899-12-30")
greffe$DECES1<-as.Date(greffe$DECES1, origin = "1899-12-30")
greffe$DECES2<-as.Date(greffe$DECES2, origin = "1899-12-30")
greffe$NEFG<-factor(greffe$NEFG)
greffe$LMALADI<-factor(greffe$LMALADI)
greffe$DELAINOUV3<-NULL # Ne comporte aucune donnée
greffe$NOUV3<-NULL # Ne comporte aucune donnée
greffe$DELAINOUV1<-as.numeric(greffe$DELAINOUV1)
greffe$DELAINOUV2<-as.numeric(greffe$DELAINOUV2)
greffe$DELAIARF1<-as.numeric(greffe$DELAIARF1)
greffe$DELAIARF2<-as.numeric(greffe$DELAIARF2)
greffe$DELAIDC1<-as.numeric(greffe$DELAIDC1)
greffe$DELAIDC2<-as.numeric(greffe$DELAIDC2)
greffe$NOUV1<-factor(greffe$NOUV1)
greffe$NOUV2<-factor(greffe$NOUV2)
greffe$kmdc<-ifelse(is.na(greffe$DECES1),0,1) # Pour faire un Kaplan-Meier
greffe$kmdelaidc<- ifelse(is.na(greffe$DECES1),as.Date(c("2016-11-01"), origin = "1899-12-30")-greffe$GRF1,greffe$DECES1-greffe$GRF1) # Pour faire un Kaplan-Meier : délai avant évènement (ou suivi jusqu'au 1-11-2016)



## _gnc----
gnc <- read_excel("~/IRC/dataIgA/donnees GNC.xlsx", 
                  col_types = c("text", "text", "text", 
                                "text", "numeric", "text", "numeric", 
                                "text", "text", "numeric", "text", 
                                "text", "text", "text", "text", "text", 
                                "numeric", "text", "text", "text", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "text", "text", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "text", "numeric", "text", "text", 
                                "text", "text", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "text", "text", "text", 
                                "text", "text", "text", "numeric", 
                                "numeric", "text", "text", "text", 
                                "text", "numeric", "text", "text", 
                                "numeric", "numeric", "text", "text", 
                                "text", "text", "date", "date", "date", 
                                "date", "date", "date", "date", 
                                "date", "date", "date"))
gnc<-gnc[!gnc$anirt<2010,] # On supprime les patients qui ont eu une leur 1er ttt de supppléance avant 2010
gnc$anirt<-as.factor(gnc$anirt)
gnc$age<-as.numeric(gnc$age)
gnc<-gnc[!gnc$age<16,] # On supprime les patients inf à 16 ans
gnc$DATE_EVT<-as.Date(gnc$DATE_EVT, origin = "1899-12-30")
gnc$FAV_DATE<-as.Date(gnc$FAV_DATE, origin = "1899-12-30")
gnc$DDIRT<-as.Date(gnc$DDIRT, origin = "1899-12-30")
gnc$DNAIS<-as.Date(gnc$DNAIS, origin = "1899-12-30")
gnc$DINSCMED<-as.Date(gnc$DINSCMED, origin = "1899-12-30")
gnc$dgrf<-as.Date(gnc$dgrf, origin = "1899-12-30")
gnc$dsvr<-as.Date(gnc$dsvr, origin = "1899-12-30")
gnc$ddc<-as.Date(gnc$ddc, origin = "1899-12-30")
gnc$dpdv<-as.Date(gnc$dpdv, origin = "1899-12-30")
gnc$DATE_DERNOUV<-as.Date(gnc$DATE_DERNOUV, origin = "1899-12-30")
gnc$delai_dernouv<-as.numeric(gnc$delai_dernouv)
gnc$delailna<-as.numeric(gnc$delailna)
gnc$delaitx<-as.numeric(gnc$delaitx)
gnc$delaidc<-as.numeric(gnc$delaidc)
gnc$RES_DEP_LIB<-as.factor(gnc$RES_DEP_LIB)
gnc$RES_REG_LIB<-as.factor(gnc$RES_REG_LIB)
gnc$bmi<-as.numeric(gnc$bmi)
gnc$classage<-as.factor(gnc$classage)
gnc$agegp<-as.factor(gnc$agegp)
gnc$AUTCOMOR3_LIB<-as.factor(gnc$AUTCOMOR3_LIB)
gnc$AUTCOMOR3_COD<-as.factor(gnc$AUTCOMOR3_COD)
gnc$AUTCOMOR2_LIB<-as.factor(gnc$AUTCOMOR2_LIB)
gnc$AUTCOMOR2_COD<-as.factor(gnc$AUTCOMOR2_COD)
gnc$AUTCOMOR1_LIB<-as.factor(gnc$AUTCOMOR1_LIB)
gnc$AUTCOMOR1_COD<-as.factor(gnc$AUTCOMOR1_COD)
gnc$HB<-as.numeric(gnc$HB)
gnc$HBINI<-as.numeric(gnc$HBINI)
gnc$gr_HBINI<-NA
gnc$gr_HBINI[gnc$sex==1]<-cut(gnc$HBINI[gnc$sex==1], breaks = c(0,13,max(gnc$HBINI, na.rm = T)+1), right = F)
gnc$gr_HBINI[gnc$sex==2]<-cut(gnc$HBINI[gnc$sex==2], breaks = c(0,12,max(gnc$HBINI, na.rm = T)+1), right = F)
gnc$gr_HBINI<-factor(gnc$gr_HBINI, levels = c(1,2), labels = c(1,0))
gnc$ALBI_MTH<-factor(gnc$ALBI_MTH, labels = c("Automate","Electrophorèse","ND","Néphélémétrie","Colorimétrique "))
gnc$ALB_MTH<-factor(gnc$ALB_MTH, labels = c("Automate","Electrophorèse","ND","Néphélémétrie","Colorimétrique "))
gnc$ALB<-as.numeric(gnc$ALB)
gnc$PDS<-as.numeric(gnc$PDS)
#gnc$nephgp<-NULL # N'a que le facteur "gnc" et pas de donnée manquante
#gnc$liste_longue<-NULL # N'a que le facteur "Néphropathie à dépôts d'gnc" et pas de donnée manquante
gnc$ALBI_MTH<-as.factor(gnc$ALBI_MTH)
gnc$ALBINI<-as.numeric(gnc$ALBINI)
gnc$CAUSENEPH2_LIB<-as.factor(gnc$CAUSENEPH2_LIB)
gnc$CAUSENEPH2_COD<-as.factor(gnc$CAUSENEPH2_COD)
gnc$CAUSENEPH1_LIB<-as.factor(gnc$CAUSENEPH1_LIB)
gnc$CAUSENEPH1_COD<-as.factor(gnc$CAUSENEPH1_COD)
gnc$NEPH_LIB<-as.factor(gnc$NEPH_LIB)
gnc$NEPH_COD<-as.factor(gnc$NEPH_COD)
gnc$EQD_REG_COD<-as.factor(gnc$EQD_REG_COD)
#gnc$EQD_PAYS_LIB<-NULL # N'a que le facteur "France" et pas de donnée manquante
gnc$EQD_DEP_LIB<-as.factor(gnc$EQD_DEP_LIB)
gnc$EQD_DEP_COD<-as.factor(gnc$EQD_DEP_COD)
gnc$EQD_REG_LIB<-as.factor(gnc$EQD_REG_LIB)
gnc$RREC_COD<-as.factor(gnc$RREC_COD)
gnc$CAUSDCP_LIB<-as.factor(gnc$CAUSDCP_LIB)
gnc$CAUSDCP_COD<-as.factor(gnc$CAUSDCP_COD)
gnc$CAUSEDCP_A_LIB<-as.factor(gnc$CAUSEDCP_A_LIB)
gnc$CAUSEDCP_A_COD<-as.factor(gnc$CAUSEDCP_A_COD)
gnc$TABACn <- factor(gnc$TABACn, labels = c("NF","Fumeur","EX Fumeur"))
gnc$VAVn <- factor(gnc$VAVn, labels = c("FAV native","Cathéter tunnélisé","Pontage","Autre"))
gnc$ACTIVn<-factor(gnc$ACTIVn, labels = c("Actif temps plein","Actif temps partiel","Actif en milieu protégé","Retraité","Au chômage","Au foyer","Scolarisé, étudiant","Arrêt longue maladie","Inactif en invalidité","Inactif autre"))
gnc$METHOn<-factor(gnc$METHOn, labels = c("Hémodialyse","Dialyse péritonéale","Greffe"))

## _diab----
diab <- read_excel("~/IRC/dataIgA/donneesdiabete.xlsx", 
                   col_types = c("text", "text", "text", 
                                 "text", "numeric", "text", "numeric", 
                                 "text", "text", "numeric", "text", 
                                 "text", "text", "text", "text", "text", 
                                 "numeric", "text", "text", "text", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "text", "text", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "text", "numeric", "text", "text", 
                                 "text", "text", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "text", "text", "text", 
                                 "text", "text", "text", "numeric", 
                                 "numeric", "text", "text", "text", 
                                 "text", "numeric", "text", "text", 
                                 "numeric", "numeric", "text", "text", 
                                 "text", "text", "date", "date", "date", 
                                 "date", "date", "date", "date", 
                                 "date", "date", "date"))
diab<-diab[!diab$anirt<2010,] # On supprime les patients qui ont eu une leur 1er ttt de supppléance avant 2010
diab$anirt<-as.factor(diab$anirt)
diab$age<-as.numeric(diab$age)
diab<-diab[!diab$age<16,] # On supprime les patients inf à 16 ans
diab$DATE_EVT<-as.Date(diab$DATE_EVT, origin = "1899-12-30")
diab$FAV_DATE<-as.Date(diab$FAV_DATE, origin = "1899-12-30")
diab$DDIRT<-as.Date(diab$DDIRT, origin = "1899-12-30")
diab$DNAIS<-as.Date(diab$DNAIS, origin = "1899-12-30")
diab$DINSCMED<-as.Date(diab$DINSCMED, origin = "1899-12-30")
diab$dgrf<-as.Date(diab$dgrf, origin = "1899-12-30")
diab$dsvr<-as.Date(diab$dsvr, origin = "1899-12-30")
diab$ddc<-as.Date(diab$ddc, origin = "1899-12-30")
diab$dpdv<-as.Date(diab$dpdv, origin = "1899-12-30")
diab$DATE_DERNOUV<-as.Date(diab$DATE_DERNOUV, origin = "1899-12-30")
diab$delai_dernouv<-as.numeric(diab$delai_dernouv)
diab$delailna<-as.numeric(diab$delailna)
diab$delaitx<-as.numeric(diab$delaitx)
diab$delaidc<-as.numeric(diab$delaidc)
diab$RES_DEP_LIB<-as.factor(diab$RES_DEP_LIB)
diab$RES_REG_LIB<-as.factor(diab$RES_REG_LIB)
diab$bmi<-as.numeric(diab$bmi)
diab$classage<-as.factor(diab$classage)
diab$agegp<-as.factor(diab$agegp)
diab$AUTCOMOR3_LIB<-as.factor(diab$AUTCOMOR3_LIB)
diab$AUTCOMOR3_COD<-as.factor(diab$AUTCOMOR3_COD)
diab$AUTCOMOR2_LIB<-as.factor(diab$AUTCOMOR2_LIB)
diab$AUTCOMOR2_COD<-as.factor(diab$AUTCOMOR2_COD)
diab$AUTCOMOR1_LIB<-as.factor(diab$AUTCOMOR1_LIB)
diab$AUTCOMOR1_COD<-as.factor(diab$AUTCOMOR1_COD)
diab$HB<-as.numeric(diab$HB)
diab$HBINI<-as.numeric(diab$HBINI)
diab$gr_HBINI<-NA
diab$gr_HBINI[diab$sex==1]<-cut(diab$HBINI[diab$sex==1], breaks = c(0,13,max(diab$HBINI, na.rm = T)+1), right = F)
diab$gr_HBINI[diab$sex==2]<-cut(diab$HBINI[diab$sex==2], breaks = c(0,12,max(diab$HBINI, na.rm = T)+1), right = F)
diab$gr_HBINI<-factor(diab$gr_HBINI, levels = c(1,2), labels = c(1,0))
diab$ALBI_MTH<-factor(diab$ALBI_MTH, labels = c("Automate","Electrophorèse","ND","Néphélémétrie","Colorimétrique "))
diab$ALB_MTH<-factor(diab$ALB_MTH, labels = c("Automate","Electrophorèse","ND","Néphélémétrie","Colorimétrique "))
diab$ALB<-as.numeric(diab$ALB)
diab$PDS<-as.numeric(diab$PDS)
#diab$nephgp<-NULL # N'a que le facteur "gnc" et pas de donnée manquante
#diab$liste_longue<-NULL # N'a que le facteur "Néphropathie à dépôts d'gnc" et pas de donnée manquante
diab$ALBI_MTH<-as.factor(diab$ALBI_MTH)
diab$ALBINI<-as.numeric(diab$ALBINI)
diab$CAUSENEPH2_LIB<-as.factor(diab$CAUSENEPH2_LIB)
diab$CAUSENEPH2_COD<-as.factor(diab$CAUSENEPH2_COD)
diab$CAUSENEPH1_LIB<-as.factor(diab$CAUSENEPH1_LIB)
diab$CAUSENEPH1_COD<-as.factor(diab$CAUSENEPH1_COD)
diab$NEPH_LIB<-as.factor(diab$NEPH_LIB)
diab$NEPH_COD<-as.factor(diab$NEPH_COD)
diab$EQD_REG_COD<-as.factor(diab$EQD_REG_COD)
#diab$EQD_PAYS_LIB<-NULL # N'a que le facteur "France" et pas de donnée manquante
diab$EQD_DEP_LIB<-as.factor(diab$EQD_DEP_LIB)
diab$EQD_DEP_COD<-as.factor(diab$EQD_DEP_COD)
diab$EQD_REG_LIB<-as.factor(diab$EQD_REG_LIB)
diab$RREC_COD<-as.factor(diab$RREC_COD)
diab$CAUSDCP_LIB<-as.factor(diab$CAUSDCP_LIB)
diab$CAUSDCP_COD<-as.factor(diab$CAUSDCP_COD)
diab$CAUSEDCP_A_LIB<-as.factor(diab$CAUSEDCP_A_LIB)
diab$CAUSEDCP_A_COD<-as.factor(diab$CAUSEDCP_A_COD)
diab$TABACn <- factor(diab$TABACn, labels = c("NF","Fumeur","EX Fumeur"))
diab$VAVn <- factor(diab$VAVn, labels = c("FAV native","Cathéter tunnélisé","Pontage","Autre"))
diab$ACTIVn<-factor(diab$ACTIVn, labels = c("Actif temps plein","Actif temps partiel","Actif en milieu protégé","Retraité","Au chômage","Au foyer","Scolarisé, étudiant","Arrêt longue maladie","Inactif en invalidité","Inactif autre"))
diab$METHOn<-factor(diab$METHOn, labels = c("Hémodialyse","Dialyse péritonéale","Greffe"))

## _global----
# On créé un fichier contenant les tableaux iga, diab et gnc (greffe est à part et pkrd n'est pas exploitable en l'état)
global<-bind_rows(iga, gnc, diab)
global$DATE_EVT<-as.Date(global$DATE_EVT, origin = "1899-12-30")
global$FAV_DATE<-as.Date(global$FAV_DATE, origin = "1899-12-30")
global$DDIRT<-as.Date(global$DDIRT, origin = "1899-12-30")
global$DNAIS<-as.Date(global$DNAIS, origin = "1899-12-30")
global$DINSCMED<-as.Date(global$DINSCMED, origin = "1899-12-30")
global$dgrf<-as.Date(global$dgrf, origin = "1899-12-30")
global$dsvr<-as.Date(global$dsvr, origin = "1899-12-30")
global$ddc<-as.Date(global$ddc, origin = "1899-12-30")
global$dpdv<-as.Date(global$dpdv, origin = "1899-12-30")
global$DATE_DERNOUV<-as.Date(global$DATE_DERNOUV, origin = "1899-12-30")
global$delai_dernouv<-as.numeric(global$delai_dernouv)
global$delailna<-as.numeric(global$delailna)
global$delaitx<-as.numeric(global$delaitx)
global$delaidc<-as.numeric(global$delaidc)
global$RES_DEP_LIB<-as.factor(global$RES_DEP_LIB)
global$RES_REG_LIB<-as.factor(global$RES_REG_LIB)
global$bmi<-as.numeric(global$bmi)
global$age<-as.numeric(global$age)
global$classage<-as.factor(global$classage)
global$agegp<-as.factor(global$agegp)
global$anirt<-as.factor(global$anirt)
global$AUTCOMOR3_LIB<-as.factor(global$AUTCOMOR3_LIB)
global$AUTCOMOR3_COD<-as.factor(global$AUTCOMOR3_COD)
global$AUTCOMOR2_LIB<-as.factor(global$AUTCOMOR2_LIB)
global$AUTCOMOR2_COD<-as.factor(global$AUTCOMOR2_COD)
global$AUTCOMOR1_LIB<-as.factor(global$AUTCOMOR1_LIB)
global$AUTCOMOR1_COD<-as.factor(global$AUTCOMOR1_COD)
global$HB<-as.numeric(global$HB)
global$HBINI<-as.numeric(global$HBINI)
global$gr_HBINI<-factor(global$gr_HBINI)
global$ALBI_MTH<-factor(global$ALBI_MTH, labels = c("Automate","Electrophorèse","ND","Néphélémétrie","Colorimétrique "))
global$ALB_MTH<-factor(global$ALB_MTH, labels = c("Automate","Electrophorèse","ND","Néphélémétrie","Colorimétrique "))
global$ALB<-as.numeric(global$ALB)
global$PDS<-as.numeric(global$PDS)
#global$nephgp<-NULL # N'a que le facteur "gnc" et pas de donnée manquante
#global$liste_longue<-NULL # N'a que le facteur "Néphropathie à dépôts d'gnc" et pas de donnée manquante
global$ALBI_MTH<-as.factor(global$ALBI_MTH)
global$ALBINI<-as.numeric(global$ALBINI)
global$CAUSENEPH2_LIB<-as.factor(global$CAUSENEPH2_LIB)
global$CAUSENEPH2_COD<-as.factor(global$CAUSENEPH2_COD)
global$CAUSENEPH1_LIB<-as.factor(global$CAUSENEPH1_LIB)
global$CAUSENEPH1_COD<-as.factor(global$CAUSENEPH1_COD)
global$NEPH_LIB<-as.factor(global$NEPH_LIB)
global$NEPH_COD<-as.factor(global$NEPH_COD)
global$EQD_REG_COD<-as.factor(global$EQD_REG_COD)
#global$EQD_PAYS_LIB<-NULL # N'a que le facteur "France" et pas de donnée manquante
global$EQD_DEP_LIB<-as.factor(global$EQD_DEP_LIB)
global$EQD_DEP_COD<-as.factor(global$EQD_DEP_COD)
global$EQD_REG_LIB<-as.factor(global$EQD_REG_LIB)
global$RREC_COD<-as.factor(global$RREC_COD)
global$CAUSDCP_LIB<-as.factor(global$CAUSDCP_LIB)
global$CAUSDCP_COD<-as.factor(global$CAUSDCP_COD)
global$CAUSEDCP_A_LIB<-as.factor(global$CAUSEDCP_A_LIB)
global$CAUSEDCP_A_COD<-as.factor(global$CAUSEDCP_A_COD)

#-----------------------------------------------------------------------------------Fichier greffe-------------------------------------------
# Les points "." ont été gérés comme une donnée manquante "NA"

greffe$NEFG[duplicated(greffe[,c("NEFG")])]
greffe$NEFG[duplicated(greffe[,c("NEFG","LMALADI")])]
for (i in greffe$NEFG[duplicated(greffe[,c("NEFG","LMALADI")])]) print(table(is.na(greffe[greffe$NEFG==i,])[2,3:17])) # On regarde si les patients avec le même NEFG et la même maladie ont une ligne vide
for (i in greffe$NEFG[duplicated(greffe[,c("NEFG")])]) print(table(is.na(greffe[greffe$NEFG==i,])[2,2:17])) # On regarde si les patients avec le même NEFG ont une ligne vide
setdiff(greffe$NEFG[duplicated(greffe[,c("NEFG")])],greffe$NEFG[duplicated(greffe[,c("NEFG","LMALADI")])])
for (i in setdiff(greffe$NEFG[duplicated(greffe[,c("NEFG")])],greffe$NEFG[duplicated(greffe[,c("NEFG","LMALADI")])])) print(greffe[greffe$NEFG==i,1:2]) # On regarde les pathologies rénales des patients en double
greffe<-greffe[!(greffe$NEFG==140081 & greffe$LMALADI==c("Inconnue ou indéterminée")),] #Supprime car maladie inconnue
greffe<-greffe[!(greffe$NEFG==153968 & greffe$LMALADI==c("Inconnue ou indéterminée")),] #Supprime car maladie inconnue
#Les patients ayant à la fois les diagnostics de néphropathie à dépôts d'IgA et maladie de Berger ne sont pas supprimé car dépôt IgA pas forcément une maladie de Berger

##Statistiques
table(duplicated(greffe$NEFG))["FALSE"] # Nombres de patients unique

sort(table(greffe$LMALADI),decreasing = T) # Répartition des pathologies rénales
sort(round(table(greffe$LMALADI)*100/table(duplicated(greffe$NEFG))["FALSE"],1),decreasing = T) # Pourcentage des diagnostics sur le nombre de patients

## _Nombre de greffe par année----
greffe<-separate(greffe, GRF1, c("annee1","mois1","jour"), remove = F)
greffe$jour<-NULL
addmargins(table(greffe$annee1)) # Nombre de greffe par année (1ère greffe)
round(prop.table(table(greffe$annee1))*100,1)

greffe<-separate(greffe, GRF2, c("annee2","mois2","jour"), remove = F)
greffe$jour<-NULL
addmargins(table(greffe$annee2)) # Nombre de greffe par année (2e greffe)
round(prop.table(table(greffe$annee2))*100,1)

greffe<-separate(greffe, GRF3, c("annee3","mois3","jour"), remove = F)
greffe$jour<-NULL
addmargins(table(greffe$annee3)) # Nombre de greffe par année (3e greffe)

greffe_annee<-gather(greffe, "a","annee",c(3,5,7))
greffe_annee<-greffe_annee[,"annee"]
greffe_annee$annee<-factor(greffe_annee$annee)
addmargins(table(greffe_annee)) # Nombre de greffe par année (total de greffe)
round(prop.table(table(greffe_annee$annee))*100,1)
barplot(prop.table(table(greffe_annee$annee))*100, 
        xlab = "Années", 
        ylab = "Pourcentage", 
        main = "Répartition du nombre de greffe total par année", 
        ylim = c(0,12),
        col = "darkturquoise")

## _Arrêt de fonction de chaque greffe----
sum(table(greffe$ARF1)) # Arrêt de fonction lors de la 1ère greffe
describe(greffe$DELAIARF1, num.desc=c("mean","median", "min", "max", "sd","valid.n")) # Temps avant arrêt de fonction (après 1ère greffe) en mois. 
hist(greffe$DELAIARF1/12,
     xlab = "Temps (en années)", 
     ylab = "Effectif", 
     main = "Temps avant arrêt de fonction (après 1ère greffe)",
     col = "darkturquoise")

sum(table(greffe$ARF2)) # Arrêt de fonction lors de la 2e greffe
describe(greffe$DELAIARF2, num.desc=c("mean","median", "min", "max", "sd","valid.n")) # Temps avant arrêt de fonction (après 2e greffe) en mois.
hist(greffe$DELAIARF2/12,
     xlab = "Temps (en années)", 
     ylab = "Effectif", 
     main = "Temps avant arrêt de fonction (après 1ère greffe)",
     col = "darkturquoise")

## _Nombre de décès----
sum(table(greffe$DECES1)) # Décès lors de la 1ère greffe
describe(greffe$DELAIDC1, num.desc=c("mean","median", "min", "max", "sd","valid.n")) # Temps avant décès (après 1ère greffe) en mois.
hist(greffe$DELAIDC1/12,
     xlab = "Temps (en années)", 
     ylab = "Effectif", 
     main = "Temps avant décès",
     col = "darkturquoise")

sum(table(greffe$DECES2)) # Décès lors de la 2e greffe
describe(greffe$DELAIDC2, num.desc=c("mean","median", "min", "max", "sd","valid.n")) # Temps avant décès (après 2e greffe) en mois. Ne suit pas une loi normale

plot(survfit(Surv(greffe$kmdelaidc/365.25,greffe$kmdc)~1), 
     main ="Coure de survie après la première greffe (évènement = décès)",
     xlab = "Durée de survie post-greffe (en année)") # Courbe de survie de Kaplan-Meier

table(greffe$NOUV1) # Nouveaux évènements (moins de décès)

#-----------------------------------------------------------------------------------Fichier patients IgA-----------------------------------------
## Fichier avec tous les patients ayant une IgA
table(duplicated(iga)) # Pas de doublons donc 3249 patients avec une néphropathie à dépôts d'IgA
## _Région, départements----
sort(table(iga$EQD_REG_LIB), decreasing = T) # Répartition par régions
par(mar=c(5,12,5,12))
barplot(sort(table(iga$EQD_REG_LIB)),
        horiz = T,
        las = 1,
        xlab = "Effectif",
        main = "IRC par dépôts d'IgA selon la région",
        col = "darkturquoise")
par(.pardefault)

sort(table(iga$EQD_DEP_LIB), decreasing = T) # Répartition par départements
par(mar=c(5,8,2,8))
barplot(sort(table(iga$EQD_DEP_LIB)),
        horiz = T,
        las = 1,
        xlim = c(0,200),
        xlab = "Effectif",
        main = "IRC par dépôts d'IgA selon la région",
        cex.names = 0.7)
par(.pardefault)

## _Types de néphropathie à IgA----
sort(table(iga$NEPH_LIB, useNA = "always")) 
round(sort(prop.table(table(iga$NEPH_LIB))*100),1)

## _Sexe----
table(iga$sex,useNA = "always")
round(prop.table(table(iga$sex))*100,1)
pie(table(iga$sex),
    col = c("cadetblue1","indianred1"),
    labels = c("Hommes","Femmes"))

## _Poids, taille, bmi----
summary(iga$PDS)
iga[iga$PDS<35 & !is.na(iga$PDS),] # On regarde les patients avec un poids faible
hist(iga$PDS,
     las = 1,
     xlab = "Poids",
     ylab = "Fréquence",
     main = "Répartition des poids")
summary(iga$TAIL)
hist(iga$TAIL,
     las = 1,
     xlim = c(100,200),
     xlab = "Taille",
     ylab = "Fréquence",
     main = "Répartition des tailles")
table(iga$bmi, useNA = "always")
iga$gr_bmi<-cut(iga$bmi, 
                breaks = c(min(iga$bmi,na.rm = T),18.5,25,30,35,40,max(iga$bmi,na.rm = T)+1), 
                right = F, 
                labels = c("<18.5","18.5-24.9","25-29.9","30-34.9","35-39.9",">40"))
barplot(prop.table(table(iga$gr_bmi))*100,
        las = 1,
        xlab = "IMC",
        ylab = "Proportion",
        main = "Répartition des IMC")


## _Néphropathies associées----
summary(iga$CAUSENEPH1_LIB) # Trop de données manquantes

## _Créatininémie initiale----
summary(iga$CREATINI)
hist(iga$CREATINI)

## _Albuminémie----
summary(iga$ALBINI) # Albuminémie initiale (g/L)
hist(iga$ALBINI)
table(iga$ALBI_MTH) # Méthode de mesure de l'albumine initiale
barplot(sort(table(iga$ALBI_MTH), decreasing = T))

summary(iga$ALB) # Albuminémie (g/L)
hist(iga$ALB)
summary(iga$ALB_MTH) # Méthode de mesure de l'albumine
barplot(sort(table(iga$ALB_MTH), decreasing = T))

## _Hémoglobinémie----
summary(iga$HBINI) # Hémoglobinémie initiale
hist(iga$HBINI)
boxplot(iga$HBINI~iga$sex)
abline(v = 13, col = "blue") # Limite anémie hommes
abline(v = 12, col = "red") # Limite anémie femmes

table(iga$gr_HBINI)
round(prop.table(table(iga$gr_HBINI))*100,1) # Nombre d'anémiques

table(iga$sex,iga$gr_HBINI, deparse.level = 2) # Nombre d'anémiques selon le sexe
round(prop.table(table(iga$sex,iga$gr_HBINI, deparse.level = 2),1)*100,1)
chisq.test(table(iga$sex,iga$gr_HBINI),correct = F) # Chi2 test

summary(iga$HB) # Hémoglobinémie
hist(iga$HB)

## _Nombre de consultation néphrologique dans l'année précédente----
summary(iga$NBCS)
table(iga$NBCS)
plot(as.factor(iga$NBCS))

## _Premier traitement en urgence----
summary(iga$URGn)
table(iga$URGn)
round(prop.table(table(iga$URGn))*100,1)
pie(table(iga$URGn), col = c("lightblue","pink"))

## _Erythropoïétine----
table(iga$EPOn,useNA = "always")
round(prop.table(table(iga$EPOn))*100,1)
pie(table(iga$EPOn),col = c("lightblue","pink"))

summary(iga$EPOINIn) # Traitement par Erythropoïétine
table(iga$EPOINIn)
round(prop.table(table(iga$EPOINIn))*100,1)
pie(table(iga$EPOINIn), col = c("lightblue","pink"))

## _Premier traitement en réa----
summary(iga$REAn)
table(iga$REAn)
round(prop.table(table(iga$REAn))*100,1)
pie(table(iga$REAn), col = c("lightblue","pink"))

## _Ponction Biopsie rénale----

table(iga$PBRn, useNA = "always")
round(prop.table(table(iga$PBRn))*100,1)
pie(table(iga$PBRn), col = c("lightblue","pink"))

## _Activité au début de l'IRT----
table(iga$ACTIVn, useNA = "always")
round(prop.table(table(iga$ACTIVn))*100,1)
par(mar=c(5,10,5,3))
barplot(sort(table(iga$ACTIVn)),
        las = 1,
        horiz = T,
        xlab = "Fréquence",
        main = "Activité au début de l'IRT")
par(.pardefault)

## _Nombre de séances d'hémodialyse----
summary(iga$NBSCEAN)
table(iga$NBSCEAN)
round(prop.table(table(iga$NBSCEAN))*100,1)
barplot(table(iga$NBSCEAN), las = 1)

## _Volume de liquide infusée par jour pour effectuer la dialyse péritonéale----
summary(iga$VOLDP)
table(iga$VOLDP)
round(prop.table(table(iga$VOLDP))*100,1)
barplot(table(iga$VOLDP), las = 1)

## _Type de traitement de l'IRCT----
summary(iga$METHOn)
table(iga$METHOn, useNA = "always")
round(prop.table(table(iga$METHOn))*100,1) # Greffe = greffé sans être passé par la dialyse
barplot(table(iga$METHOn), las = 1)

## _Voie d'abord vasculaire----
# NA = zéro abord ? 
summary(iga$VAVn)
table(iga$VAVn, useNA = "always")
round(prop.table(table(iga$VAVn))*100,1)
barplot(table(iga$VAVn), las = 1)

## _Traitement (concaténation TECHN et MODAL)----
summary(iga$traitement)
table(iga$traitement)
round(prop.table(table(iga$traitement))*100,1)
barplot(table(iga$traitement), las = 1)

## _Durée séance d'hémodialyse en minutes----
describe(iga$DRSCmin, num.desc = c("min","mean","median","max","sd","valid.n"))
table(iga$DRSCmin)
barplot(table(iga$DRSCmin), las = 1)

## _Assistance par infirmière diplômée d'Etat----
table(iga$IDEn,useNA = "always")
round(prop.table(table(iga$IDEn))*100,1)
pie(table(iga$IDEn),col = c("lightblue","pink"))

## _Diabète----
table(iga$DIABn,useNA = "always")
round(prop.table(table(iga$DIABn))*100,1)
pie(table(iga$DIABn),col = c("lightblue","pink"))

table(iga$TYPDIABn) # Type de diabète
round(prop.table(table(iga$TYPDIABn[!iga$TYPDIABn==0]))*100,1)
pie(table(iga$TYPDIABn[!iga$TYPDIABn==0]))

## _Insuline----
table(iga$INSULn,useNA = "always")
round(prop.table(table(iga$INSULn))*100,1)
pie(table(iga$INSULn))

## _Insuffisance respiratoire chronique----
table(iga$IRCn,useNA = "always")
round(prop.table(table(iga$IRCn))*100,1)
pie(table(iga$IRCn))

## _Oxygénothérapie----
table(iga$O2n,useNA = "always")
round(prop.table(table(iga$O2n))*100,1)
pie(table(iga$O2n))

## _Insuffisance cardiaque----
table(iga$ICn,useNA = "always")
round(prop.table(table(iga$ICn))*100,1)
pie(table(iga$ICn))

table(iga$STADICn,useNA = "always") # Stade de l'insuffisance cardiaque ???
round(prop.table(table(iga$STADICn))*100,1)
pie(table(iga$STADICn))

## _Insuffisance coronarienne----
table(iga$ICOROn,useNA = "always")
round(prop.table(table(iga$ICOROn))*100,1)
pie(table(iga$ICOROn))

## _Infarctus du myocarde----
table(iga$IDMn,useNA = "always")
round(prop.table(table(iga$IDMn))*100,1)
pie(table(iga$IDMn))

## _Troubles du rythme----
table(iga$RYTHMn,useNA = "always")
round(prop.table(table(iga$RYTHMn))*100,1)
pie(table(iga$RYTHMn))

## _Anevrysme de l'aorte abdominale----
table(iga$ANEVn,useNA = "always")
round(prop.table(table(iga$ANEVn))*100,1)
pie(table(iga$ANEVn))

## _Artérite des membres inférieurs----
table(iga$AMIn,useNA = "always")
round(prop.table(table(iga$AMIn))*100,1)
pie(table(iga$AMIn))

table(iga$STDAMIn, useNA = "always") # Stade de l'artérite des membres inférieurs
round(prop.table(table(iga$STDAMIn))*100,1)
pie(table(iga$STDAMIn))

## _Accident vasculaire cérébral----
table(iga$AVCn,useNA = "always")
round(prop.table(table(iga$AVCn))*100,1)
pie(table(iga$AVCn))

table(iga$AITn,useNA = "always") # Accident ischémique transitoire
round(prop.table(table(iga$AITn))*100,1)
pie(table(iga$AITn))

table(iga$AVCAITn,useNA = "always") # Variable composite de AVC et AIT
round(prop.table(table(iga$AVCAITn))*100,1)
pie(table(iga$AVCAITn))

addmargins(table(iga$AVCn, iga$AITn, useNA = "always", deparse.level = 2))# Ne correspond pas exactement
round(prop.table(table(iga$AVCn, iga$AITn, deparse.level = 2))*100,1)

## _Cancer évolutif----
table(iga$KCn,useNA = "always")
round(prop.table(table(iga$KCn))*100,1)
pie(table(iga$KCn))

## _Ag HBS positif----
table(iga$VHBn,useNA = "always")
round(prop.table(table(iga$VHBn))*100,1)
pie(table(iga$VHBn))

## _PCR VHC positif----
table(iga$VHCn,useNA = "always")
round(prop.table(table(iga$VHCn))*100,1)
pie(table(iga$VHCn))

## _Cirrhose----
table(iga$CIRHn,useNA = "always")
round(prop.table(table(iga$CIRHn))*100,1)
pie(table(iga$CIRHn))

table(iga$STDCIRHn, useNA = "always") # Stade de la cirrhose
round(prop.table(table(iga$STDCIRHn))*100,1)
pie(table(iga$STDCIRHn))

## _VIH----
table(iga$VIHn,useNA = "always")
round(prop.table(table(iga$VIHn))*100,1)
pie(table(iga$VIHn))

table(iga$SIDAn, useNA = "always") # SIDA
round(prop.table(table(iga$SIDAn))*100,1)
pie(table(iga$SIDAn))


## _Transplantation autre que rein----
table(iga$TX_AUTRE, useNA = "always")
round(prop.table(table(iga$TX_AUTRE))*100,1)
pie(table(iga$TX_AUTRE))

par(mfrow = c(3,3))
for (i in c( "ORG_Cn", "ORG_CPn","ORG_Pn","ORG_Fn","ORG_PAn","ORG_ILn","ORG_In","ORG_MOn")) 
{
  print(i)
  print(table(iga[,i], useNA = "always"))
  print(round(prop.table(table(iga[,i]))*100,1))
  barplot(table(iga[,i]), main = i, xlab = paste("0 =",table(iga[,i])[1],";", "1 =",table(iga[,i])[2]))
  
}
par(mfrow = c(1,1))

## _Au moins un handicap----
table(iga$HANDn, useNA = "always")
round(prop.table(table(iga$HANDn))*100,1)
pie(table(iga$HANDn))

## _Amputation membres inférieurs----
table(iga$AMPn, useNA = "always")
round(prop.table(table(iga$AMPn))*100,1)
pie(table(iga$AMPn))

## _Paraplégie/Hémiplégie----
table(iga$PLEGn, useNA = "always")
round(prop.table(table(iga$PLEGn))*100,1)
pie(table(iga$PLEGn))

## _Cécité----
table(iga$CECITEn, useNA = "always")
round(prop.table(table(iga$CECITEn))*100,1)
pie(table(iga$CECITEn))

## _Troubles du comportement----
table(iga$COMPORTn, useNA = "always")
round(prop.table(table(iga$COMPORTn))*100,1)
pie(table(iga$COMPORTn))

## _Inscription sur liste d'attente (selon REIN)----
table(iga$INSCn, useNA = "always")
round(prop.table(table(iga$INSCn))*100,1)
pie(table(iga$INSCn), col = c("lightblue","pink"))

## _Motif de non inscription sur liste d'attente de greffe----
table(iga$NOINSCn, useNA = "always")
round(prop.table(table(iga$NOINSCn))*100,1)
barplot(table(iga$NOINSCn))

## _Statut tabagique----

table(iga$TABACn, useNA = "always")
round(prop.table(table(iga$TABACn))*100,1)
pie(table(iga$TABACn))

## _Marche----
table(iga$MARCHn, useNA = "always")
round(prop.table(table(iga$MARCHn))*100,1)
pie(table(iga$MARCHn))

## _Autres comorbidités----
sort(table(iga$AUTCOMOR1_LIB, useNA = "always"))
round(prop.table(sort(table(iga$AUTCOMOR1_LIB)))*100,1)

sort(table(iga$AUTCOMOR2_LIB, useNA = "always"))
round(prop.table(sort(table(iga$AUTCOMOR2_LIB)))*100,1)

sort(table(iga$AUTCOMOR3_LIB, useNA = "always"))
round(prop.table(sort(table(iga$AUTCOMOR3_LIB)))*100,1)

autcomor_lib<-gather(iga[,c("patient","AUTCOMOR1_LIB","AUTCOMOR2_LIB","AUTCOMOR3_LIB")], "num_comor", "autcomor", 2:4)
sort(table(autcomor_lib$autcomor, useNA = "always"))
round(prop.table(sort(table(autcomor_lib$autcomor)))*100,1)

## _Année de démarrage du 1er traitement de suppléance----
table(iga$anirt, useNA = "always")
round(prop.table(table(iga$anirt))*100,1)
barplot(table(iga$anirt))
table(iga$RES_REG_LIB,iga$anirt) # Par région

## _Age à l'initiation du traitement de suppléance----
table(iga$age, useNA = "always")
summary(iga$age)
hist(iga$age,
     las = 1,
     xlab = "Age",
     ylab = "Fréquence",
     main = "Age à l'initiation du traitement de suppléance")

table(iga$agegp, useNA = "always")
round(prop.table(table(iga$agegp))*100,1)
barplot(table(iga$agegp),
        las = 1,
        xlab = "Age",
        ylab = "Fréquence",
        main = "Age à l'initiation du traitement de suppléance")

table(iga$classage, useNA = "always") # Il y a deux classes aberrantes (étaient sous forme de date dans le fichier Excel)
View(iga[iga$classage==c("41913","42618"), c("age","agegp","classage")]) # Correspodent à des enfants de moins de 15 ans
iga$classage<-cut(iga$age, breaks = c(seq(5,95,5)), right = F)
round(prop.table(table(iga$classage))*100,1)
barplot(table(iga$classage),
        las = 1,
        xlab = "Age",
        ylab = "Fréquence",
        main = "Age à l'initiation du traitement de suppléance")

## _Décès----
table(iga$dc, useNA = "always")
round(prop.table(table(iga$dc))*100,1)
pie(table(iga$dc), col = c("lightblue","pink"))

summary(iga$ddc) #9 Date de décès
table(iga$ddc, useNA = "always")
barplot(table(iga$ddc))

summary(iga$delaidc) # Délai entre 1er tt et décès
hist(iga$delaidc)
nrow(iga[iga$dc==0 & !is.na(iga$delaidc),c("dc","delaidc")]) # On a des patients qui sont noté comme non DCD alors qu'ils ont un délai avant décès

## _Greffé----
table(iga$tx, useNA = "always")
round(prop.table(table(iga$tx))*100,1)
pie(table(iga$tx))

summary(iga$delaitx) # Délai entre 1er tt et greffe
hist(iga$delaitx)
nrow(iga[iga$tx==0 & !is.na(iga$delaitx),c("tx","delaitx")]) # On a des patients qui sont noté comme non greffé alors qu'ils ont un délai avant greffe

## _Date de greffe----
summary(iga$dgrf)
table(iga$dgrf, useNA = "always")
barplot(table(iga$dgrf))

## _Délai entre 1er tt et inscription sur la Liste Nationale d'Attente (LNA)----
summary(iga$delailna)
table(iga$delailna, useNA = "always")
hist(iga$delailna)

## _Date inscription sur liste d'attente de greffe----
summary(iga$DINSCMED)
table(iga$DINSCMED, useNA = "always")
barplot(table(iga$DINSCMED))

## _Délai entre 1er tt et dernières nouvelles----
summary(iga$delai_dernouv)
table(iga$delai_dernouv, useNA = "always")
hist(iga$delai_dernouv)

## _Date Inclusion dans REIN----
summary(iga$gr_DATE_EVT)
table(iga$gr_DATE_EVT, useNA = "always")
barplot(table(iga$gr_DATE_EVT))

## _Date Fistule Artério veineuse----
summary(iga$FAV_DATE)
table(iga$FAV_DATE, useNA = "always")
barplot(table(iga$FAV_DATE))

## _Date de premier tt de suppléance----
summary(iga$DDIRT)
table(iga$DDIRT, useNA = "always")
barplot(table(iga$DDIRT))


## _Date de naissance----
summary(iga$DNAIS)
table(iga$DNAIS, useNA = "always")
barplot(table(iga$DNAIS)) ## On a des données aberrantes (né après 2016)
iga$DNAIS[iga$DNAIS>"2016-12-01"]<-NA
iga$gr_DNAIS<-cut(iga$DNAIS, breaks = as.Date(paste(1930:2016, "-01-01", sep = ""))) # On regroupe par année
barplot(table(iga$gr_DNAIS))
summary(iga$gr_DNAIS)

## _Date de sevrage de la dialyse----
summary(iga$dsvr)
table(iga$dsvr, useNA = "always")
barplot(table(iga$dsvr))

## _Date de perdu de vue----
summary(iga$dpdv)
table(iga$dpdv, useNA = "always") # Trop de donénes manquantes
barplot(table(iga$dpdv))

## _Date des dernières nouvelles----
summary(iga$DATE_DERNOUV)
table(iga$DATE_DERNOUV, useNA = "always") # Majorité des PDV le 01-12-2014
barplot(table(iga$DATE_DERNOUV))

#--------------------------------------------------------------- Incidence spatiale --------------------------

## Tableaux du nombre de cas d'IgA par âge et sexe selon la région
nbev_iga_region<-iga[,c("sex","classage","RES_REG_LIB")]
nbev_iga_region<-unite(nbev_iga_region, "sex_age", 1:2, sep = "-")
nbev_iga_region<-as.data.frame.matrix(table(nbev_iga_region$sex_age,nbev_iga_region$RES_REG_LIB))
nbev_iga_region$ETRANGER<-NULL 
nbev_iga_region$`POLYNESIE FRANCAISE`<-NULL # Suppression de la Polynésie française qui n'est pas dans les statistiques de l'INSEE
nbev_iga_region$Mayotte<-NULL

## Tableaux de l'effectif par région, pour la catégorie d'âge 16-19 ans, les données INSEE étant de 15 à 19 ans, muliplié par 0.8
eff_region <- read.csv2("~/IRC/data/eff_region.csv", header = T, ";")
nbev_iga_region<-data.frame(eff_region[,1],nbev_iga_region)
colnames(nbev_iga_region)[1]<-"classage"

## Tableaux de l'effectif en France
eff_france <- read.csv2("~/IRC/data/eff_france.csv", header = T,";")
eff_france$age<-cut(eff_france$age, breaks = c(16, seq(20,95,5)), right = F, include.lowest = T)
eff_france<-gather(eff_france, "sex","effectif", 2:3)
eff_france$sex<-factor(eff_france$sex, levels = c("h","f"), labels = c(1,2))
eff_france<-unite(eff_france, "classage", 2:1, sep = "-")
eff_france$classage<-factor(eff_france$classage)

## Calcul par standardisation directe sur l'âge et le sexe pour 100 000 habitants
standdirect<-matrix(nrow = 4, ncol = 25, dimnames = list(c("Ratio brut","Ratio ajuste","IC inf","IC sup"), c(colnames(nbev_iga_region[,-1]))))
for (i in colnames(eff_region[,-1])) standdirect[,i]<-as.matrix(round(ageadjust.direct(nbev_iga_region[,i], eff_region[,i], stdpop = eff_france[,2])*10^5,1))
standdirect<-t(standdirect)
standdirect<-data.frame(standdirect)
standdirect[order(standdirect$Ratio.ajuste, decreasing = T),]

#--------------------------------------------------------------- incidence temporelle --------------------------

table(iga$anirt, useNA = "always")
round(prop.table(table(iga$anirt))*100,1)
addmargins(table(iga$RES_REG_LIB,iga$anirt)) # Par région
sum(nbev_iga_region$Alsace)
round(prop.table(table(iga$RES_REG_LIB,iga$anirt),1)*100,0)


#------------------------------------------------- Etude des caractéristiques cliniques et du devenir de ces patients --------------------------

summary(iga$age)
hist(iga$age,
     las = 1,
     xlab = "Age",
     ylab = "Fréquence",
     main = "Age à l'initiation du traitement de suppléance")

ave(iga$age, iga$anirt, summary)
tapply(iga$age, iga$anirt, summary)


#------------------------------------------------- Divers --------------------------

table(!is.na(iga$ddc))[2]
summary(iga$delaidc)
table(iga$dc,!is.na(iga$ddc), deparse.level = 2)
























