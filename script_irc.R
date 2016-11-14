#-------------------------------------------------Data------------------------------------------------------------------------------
library("survival", lib.loc="C:/Program Files/R/R-3.3.2/library")
library("readxl", lib.loc="~/R/win-library/3.3")
library("tidyr", lib.loc="~/R/win-library/3.3")
library("dplyr", lib.loc="~/R/win-library/3.3")
library("prettyR", lib.loc="~/R/win-library/3.3")
library("ggplot2", lib.loc="~/R/win-library/3.3")

.pardefault <- par(no.readonly = T)

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
                                "text", "numeric", "numeric", "numeric", 
                                "numeric", "text", "text", "text", 
                                "text", "numeric", "text", "text", 
                                "numeric", "numeric", "text", "text", 
                                "text", "text", "date", "date", "date", 
                                "date", "date", "date", "date", 
                                "date", "date", "date"))
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
iga$age<-as.numeric(iga$age)
iga$classage<-as.factor(iga$classage)
iga$agegp<-as.factor(iga$agegp)
iga$anirt<-as.factor(iga$anirt)
iga$AUTCOMOR3_LIB<-NULL # Vide
iga$AUTCOMOR3_COD<-NULL # Vide
iga$AUTCOMOR2_LIB<-as.factor(iga$AUTCOMOR2_LIB)
iga$AUTCOMOR2_COD<-as.factor(iga$AUTCOMOR2_COD)
iga$AUTCOMOR1_LIB<-as.factor(iga$AUTCOMOR1_LIB)
iga$AUTCOMOR1_COD<-as.factor(iga$AUTCOMOR1_COD)
iga$HB<-as.numeric(iga$HB)
iga$ALBI_MTH<-factor(iga$ALBI_MTH, labels = c("Automate","Electrophorèse","ND","Néphélémétrie","Colorimétrique "))
iga$ALB_MTH<-factor(iga$ALB_MTH, labels = c("Automate","Electrophorèse","ND","Néphélémétrie","Colorimétrique "))
iga$ALB<-as.numeric(iga$ALB)
iga$PDS<-as.numeric(iga$PDS)
iga$nephgp<-NULL # N'a que le facteur "gnc" et pas de donnée manquante
iga$liste_longue<-NULL # N'a que le facteur "Néphropathie à dépôts d'IgA" et pas de donnée manquante
iga$HBINI<-as.numeric(iga$HBINI)
iga$ALBI_MTH<-as.factor(iga$ALBI_MTH)
iga$ALBINI<-as.numeric(iga$ALBINI)
iga$CAUSENEPH2_LIB<-as.factor(iga$CAUSENEPH2_LIB)
iga$CAUSENEPH2_COD<-as.factor(iga$CAUSENEPH2_COD)
iga$CAUSENEPH1_LIB<-as.factor(iga$CAUSENEPH1_LIB)
iga$CAUSENEPH1_COD<-as.factor(iga$CAUSENEPH1_COD)
iga$NEPH_LIB<-as.factor(iga$NEPH_LIB)
iga$NEPH_COD<-as.factor(iga$NEPH_COD)
iga$EQD_REG_COD<-as.factor(iga$EQD_REG_COD)
iga$EQD_PAYS_LIB<-NULL # N'a que le facteur "France" et pas de donnée manquante
iga$EQD_DEP_LIB<-as.factor(iga$EQD_DEP_LIB)
iga$EQD_DEP_COD<-as.factor(iga$EQD_DEP_COD)
iga$EQD_REG_LIB<-as.factor(iga$EQD_REG_LIB)
iga$RREC_COD<-as.factor(iga$RREC_COD)
iga$CAUSDCP_LIB<-as.factor(iga$CAUSDCP_LIB)
iga$CAUSDCP_COD<-as.factor(iga$CAUSDCP_COD)
iga$CAUSEDCP_A_LIB<-as.factor(iga$CAUSEDCP_A_LIB)
iga$CAUSEDCP_A_COD<-as.factor(iga$CAUSEDCP_A_COD)

gnc <- read_excel("~/IRC/dataIgA/donnees GNC.xlsx")
diab <- read_excel("~/IRC/dataIgA/donneesdiabete.xlsx")
pkrd <- read_excel("~/IRC/dataIgA/donneesPKRD.xlsx")

greffe <- read_excel("~/IRC/dataIgA/greffe IgA.xlsx", col_types = c("numeric", "text", "date", "date", "date", "date", "date", "numeric", "numeric",
                                                                        "date", "date", "text", "text", "numeric", "text", "text", "text", "numeric",
                                                                        "text", "text", "numeric"))
greffe$ARF1<-as.Date(greffe$ARF1, origin = "1899-12-30")
greffe$ARF2<-as.Date(greffe$ARF2, origin = "1899-12-30")
greffe$GRF1<-as.Date(greffe$GRF1, origin = "1899-12-30")
greffe$GRF2<-as.Date(greffe$GRF2, origin = "1899-12-30")
greffe$GRF3<-as.Date(greffe$GRF3, origin = "1899-12-30")
greffe$DECES1<-as.Date(greffe$DECES1, origin = "1899-12-30")
greffe$NEFG<-factor(greffe$NEFG)
greffe$LMALADI<-factor(greffe$LMALADI)
greffe$DELAINOUV3<-NULL # Ne comporte aucune donnée
greffe$DELAIDC2<-NULL # Ne comporte aucune donnée
greffe$NOUV3<-NULL # Ne comporte aucune donnée
greffe$DECES2<-NULL #Supprime car même date que dans DECES1
greffe$DELAINOUV1<-as.numeric(greffe$DELAINOUV1)
greffe$DELAINOUV2<-as.numeric(greffe$DELAINOUV2)
greffe$DELAIARF1<-as.numeric(greffe$DELAIARF1)
greffe$DELAIARF2<-as.numeric(greffe$DELAIARF2)
greffe$DELAIDC1<-as.numeric(greffe$DELAIDC1)
greffe$NOUV1<-factor(greffe$NOUV1)
greffe$NOUV2<-factor(greffe$NOUV2)
greffe$kmdc<-ifelse(is.na(greffe$DECES1),0,1) # Pour faire un Kaplan-Meier
greffe$kmdelaidc<- ifelse(is.na(greffe$DECES1),as.Date(c("2016-11-01"), origin = "1899-12-30")-greffe$GRF1,greffe$DECES1-greffe$GRF1) # Pour faire un Kaplan-Meier : délai avant évènement (ou suivi jusqu'au 1-11-2016)

#-------------------------------------------------Fichier greffe------------------------------------------------------------------------------
# Les points "." ont été gérés comme une donnée manquante "NA" en attedant de savoir à quoi cela correspond

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

sum(table(greffe$DECES1)) #Nombre de décès
describe(greffe$DELAIDC1, num.desc=c("mean","median", "min", "max", "sd","valid.n")) # Temps avant décès (après greffe ?) en mois. Ne suit pas une loi normale
hist(greffe$DELAIDC1/30,
     xlab = "Temps (en années)", 
     ylab = "Effectif", 
     main = "Temps avant décès",
     col = "darkturquoise")

plot(survfit(Surv(greffe$kmdelaidc/365.25,greffe$kmdc)~1), 
     main ="Coure de survie après la première greffe (évènement = décès)",
     xlab = "Durée desurvie post-greffe (en année)") # Courbe de survie de Kaplan-Meier

table(greffe$NOUV1) # Nouveaux évènements (moins de décès)

#-------------------------------------------------Fichier patients IgA------------------------------------------------------------------------------
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

## _Poids, taille----
table(iga$PDS, useNA = "always")
iga[iga$PDS<35 & !is.na(iga$PDS),] # On regarde les patients avec un poids faible
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
summary(iga$ALBI_MTH) # Méthode de mesure de l'albumine initiale
barplot(table(iga$ALBI_MTH))

summary(iga$ALB) # Albuminémie (g/L)
hist(iga$ALB)
summary(iga$ALB_MTH) # Méthode de mesure de l'albumine
barplot(table(iga$ALB_MTH))

## _Hémoglobinémie----
summary(iga$HBINI) # Hémoglobinémie initiale
hist(iga$HBINI)
boxplot(iga$HBINI~iga$sex)
abline(h = 13, col = "blue") # Limite anémie hommes
abline(h = 12, col = "red") # Limite anémie femmes

ifelse(iga$sex==1, 
       iga$gr_HBINI[iga$sex==1]<-cut(iga$HBINI[iga$sex==1], breaks = c(0,13,max(iga$HBINI, na.rm = T)+1), right = F, labels = c("Anémique","Non anémique")), 
       iga$gr_HBINI[iga$sex==2]<-cut(iga$HBINI[iga$sex==2], breaks = c(0,12,max(iga$HBINI, na.rm = T)+1), right = F, labels = c("Anémique","Non anémique")))

table(iga$gr_HBINI)
round(prop.table(table(iga$gr_HBINI))*100,1) # Nombre d'anémiques

table(iga$sex,iga$gr_HBINI) # Nombre d'anémiques selon le sexe
round(prop.table(table(iga$sex,iga$gr_HBINI, deparse.level = 1),1)*100,1)
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

## _Traitement par Erythropoïétine----
summary(iga$EPOINIn)
table(iga$EPOINIn)
round(prop.table(table(iga$EPOINIn))*100,1)
pie(table(iga$EPOINIn), col = c("lightblue","pink"))

## _Premier traitement en réa----
summary(iga$REAn)
table(iga$REAn)
round(prop.table(table(iga$REAn))*100,1)
pie(table(iga$REAn), col = c("lightblue","pink"))

## _Ponction Biopsie rénale----
summary(iga$PBRn)
table(iga$PBRn)
round(prop.table(table(iga$PBRn))*100,1)
pie(table(iga$PBRn), col = c("lightblue","pink"))

## _Activité au début de l'IRT----
summary(iga$ACTIVn)
iga$ACTIVn<-factor(iga$ACTIVn, labels = c("Actif temps plein","Actif temps partiel","Actif en milieu protégé","Retraité","Au chômage","Au foyer","Scolarisé, étudiant","Arrêt longue maladie","Inactif en invalidité","Inactif autre"))
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
table(iga$METHOn)
iga$METHOn<-factor(iga$METHOn, labels = c("Hémodialyse","Dialyse péritonéale","Greffe"))
round(prop.table(table(iga$METHOn))*100,1)
barplot(table(iga$METHOn), las = 1)

## _Voie d'abord vasculaire----
# NA = zéro abord ? 
summary(iga$VAVn)
table(iga$VAVn)
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
table(iga$IDEn)
round(prop.table(table(iga$IDEn))*100,1)
pie(table(iga$IDEn),col = c("lightblue","pink"))
