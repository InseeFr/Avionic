### Travail préparatoire, import de données

rm(list=ls())

chemin_data="C:/Users/letch/Documents/Travail/Insee/2019_conso_menages/data_avionic/FR_2015_38"
chemin_data_inter="C:/Users/letch/Documents/Travail/Insee/2019_conso_menages/data_avionic/OCDE"
chemin_data_agregation="C:/Users/letch/Documents/Travail/Insee/2019_conso_menages/data_avionic/agregation"

chemin_prog="C:/Users/letch/Documents/Travail/Insee/2019_conso_menages/programmes_avionic"



## imports de données
setwd(chemin_prog)
getwd()
source("fonctions.R")

## Données TES français##

setwd(dir = chemin_data)
tei_imp <- matrice(fichier = "tei_imp_2015.csv")
tei_dom <- matrice(fichier = "tei_dom_2015.csv")
prod <- matrice(fichier = "prod_dom_2015.csv")
ef_i <- matrice(fichier = "conso_imp_menage_2015.csv")
ef_d <- matrice(fichier = "conso_dom_menage_2015.csv")
df_d <- matrice(fichier = "dde_dom_2015.csv")
df_i <- matrice(fichier = "dde_imp_2015.csv")
emploi <- matrice(fichier = "emploi_sal_2015.csv")
var_df <- matrice(fichier = "var_df.csv")
coeff_trans <- matrice(fichier = "coeff_transmission_38.csv")


################################# Calculs au niveau national ############################################

setwd(chemin_prog)

### Calcul de contenus en importations, VA et emplois
source("contenus.R")

### Calcul des contenus en importations

res.contenus_imp <- contenus_imp(prod, tei_dom, tei_imp, ef_d, ef_i)

### Calcul des contenus en VA

res.contenus_va <- contenus_va(prod, tei_dom, tei_imp, ef_d, ef_i)

### Calcul des contenus en emplois (ici ETP)

res.contenus_emploi <- contenus_emploi(prod, tei_dom, ef_d, emploi)


### Réponse à une variation de demande finale
source("variation_df.R")

### Impact sur la production nationale

res.variation_prod <- variation_prod(prod, tei_dom, df_d, var_df)

### Impact sur les importations

res.variation_imp <- variation_imp(prod, tei_dom, tei_imp, df_d, df_i, var_df)

### Impact sur l'emploi

res.variation_emp <- variation_emploi(prod, tei_dom, df_d, var_df, emploi)

### Réponse à une variation de prix 

source("variation_prix.R")

### On peut ou non utiliser les coefficients de transmission, voir le document de travail sur insee.fr
res.variation_prix <- variation_prix(tei_dom, prod, 9, 50)
res.variation_prix_coeff <- variation_prix(tei_dom, prod, 9, 50, coeff_trans)


############################### Calculs au niveau international #########################################


### import des données internationales de l'OCDE

install.packages("stringr", dependencies=TRUE)   # package pour fonction str_sub
library(stringr)

setwd(chemin_data_inter)
load("ICIO2018Z.Rdata")
load("ICIO2018X.Rdata")
load("ICIO2018HFCE.Rdata")
load("ICIO2018NPISH.Rdata")
load("ICIO2018GGFC.Rdata")
tei_tiva <- ICIO2018econZ[6,,]
prod_tiva <- ICIO2018econX[6,]
conso_tiva <- ICIO2018econHFCE[6,,] + ICIO2018econNPISH[6,,] + ICIO2018econGGFC[6,,]

pays <-vector()
for(j in 1:69){
  pays[j] <- str_sub(names(tei_tiva[,1])[1+36*(j-1)], 1, 3)
}
pays_2 <- pays[1:64]

### Découpage des données en fichiers fins
setwd(chemin_prog)

source("decoupage_international.R")

path <- paste0(chemin_data_inter,"/2015/Stockage des TEI")
dir.create(path)

path <- paste0(chemin_data_inter,"/2015/Stockage des productions")
dir.create(path)

path <- paste0(chemin_data_inter,"/2015/Stockage des emplois finals")
dir.create(path)


decoupage_tei(tei_tiva, pays, 36, 69, paste0(chemin_data_inter,"/2015/Stockage des TEI"))
decoupage_prod(prod_tiva, pays, 36, 69,paste0(chemin_data_inter,"/2015/Stockage des productions"))
decoupage_ef(conso_tiva, pays, 36, 69, paste0(chemin_data_inter,"/2015/Stockage des emplois finals"))

### Calculs internationaux

### Calculs de contenus

setwd(chemin_prog)

source("contenus_international.R")
res.contenus_va_inter <- contenus_va_inter(prod_tiva, tei_tiva, conso_tiva)
res.contenus_imp_inter <- contenus_imp_inter(paste0(chemin_data_inter,"/2015"),"Stockage des TEI","Stockage des productions","Stockage des emplois finals",pays, pays_2)

### Réponse à une variation de demande finale

setwd(chemin_prog)

# création d'une matrice de variation de demande finale internationale
var_conso_inter <- matrix(0, nrow = nrow(conso_tiva), ncol = ncol(conso_tiva))
var_conso_inter[1,1]<- 10000

source("variation_df_international.R")
res.var_df_inter <- variation_prod_inter(prod_tiva, tei_tiva, conso_tiva, var_conso_inter) 


setwd(dir = chemin_data)

var_df_inter_fra <- matrice(fichier = "var_df_inter_fra.csv")

res.var_imp_inter <- var_imp_inter(paste0(chemin_data_inter,"/2015"),"Stockage des TEI","Stockage des productions","Stockage des emplois finals",pays, "FRA", var_df_inter_fra)

### Réponse à une variation de prix
setwd(chemin_prog)

source("variation_prix_international.R")
res.variation_prix_inter <- variation_prix_inter(prod_tiva, tei_tiva,1,10,36, 50)


#################### Agrégation et compte des ménages par catégories ################################

### import des tables de passages
setwd(dir = chemin_data_agregation)
getwd()

TabPass_foncdet_vers_fonc10 <- read.csv(file="TabPass_foncdet_vers_fonc10.csv", header=TRUE, sep=";")
TabPass_NA <- read.csv(file="TabPass_NA.csv", header=TRUE, sep=";")
TabPass_NA10_vers_fonc <- read.csv(file="TabPass_NA10_vers_fonc.csv", header=TRUE, sep=";")
TabPass_NA17_vers_fonc <- read.csv(file="TabPass_NA17_vers_fonc.csv", header=TRUE, sep=";")
TabPass_NA38_vers_fonc <- read.csv(file="TabPass_NA38_vers_fonc.csv", header=TRUE, sep=";")
TabPass_NA64_vers_fonc <- read.csv(file="TabPass_NA64_vers_fonc.csv", header=TRUE, sep=";")
TabPass_NA88_vers_fonc <- read.csv(file="TabPass_NA88_vers_fonc.csv", header=TRUE, sep=";")
TabPass_NA138_vers_fonc <- read.csv(file="TabPass_NA138_vers_fonc.csv", header=TRUE, sep=";")

tab_pass_FIGARO <- read.csv(file="tab_pass_FIGARO.csv", header=TRUE, sep=";")
tab_pass_TiVA <- read.csv(file="tab_pass_TiVA.csv", header=TRUE, sep=";")


install.packages("sqldf")
library(sqldf)# Library qui permet d executer des requetes SQL avec la clause GROUP BY
#Ce package est utilisé par le programme d'agregation et par celui sur le compte des menages par categories

setwd(dir = chemin_prog)

source("Avionic_agregation.R")

### Agregation d'une valeur à différents niveaux, en nomenclature agrégée puis en nomenclature fonctionnelle
setwd(dir = chemin_data_agregation)

# chargement d'une table de test
testAgreg <- read.csv(file="testAgreg.CSV", header=TRUE, sep=";")


# Agrégation 
Agreg_NA(testAgreg, "Valeur","NA129","NA17")


# Passage en nomenclature fonctionnelle
Agreg_NA_fonc("NA17",TabAfter,"VAL")


### Travaux sur le Compte des ménages par catégories (CMpC)

# chargement de la structure de la consommation des menages par categories
Tab_Struct_CMpC <- read.csv(file="Tab_Struct_CMpC.csv", header=TRUE, sep=";")

setwd(chemin_prog)

source("Avionic_CMpC.R")
# attention, le package "sqldf" est nécessaire aux fonctions de ce programme

Ventile_Vecteur(TabAfter_fonc_niv1et2COICOP)
resultat_CMpC(Tab_struct_CMpC_foisVecteur)



