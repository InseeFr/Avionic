#########################################################
#########################################################
#                                                       #
#    AVIONIC - Compte des ménages par catégories (CMpC) #
#                                                       #
#########################################################
#########################################################

#######################################################################################################
## FONCTION DE VENTILATION D UN VECTEUR SELON LE CMpC ############################################################
##
## Cette fonction permet de ventiler un vecteur de format fixe (COCIOP 1+2 digit) tel qu il ressort notamment des programmes d agregation.
#######################################################################################################

## On commence par charger la table de structure du CMpC qui est implicitement du niveau COCIOP 1+2 digit
tab_Struct_CMpC <- Tab_Struct_CMpC # si nécessaire : read.csv(file="Tab_Struct_CMpC.csv", header=TRUE, sep=";") # Chargement de la table de structure CMpC
 # stringsAsFactors = FALSE

# Creation d'une fonction pour agreger tout vecteur a tout niveau de nomenclature
# Variables : vecteur en entree = vecteur en sortie d AVIONIC (cf fonctions précédentes)
# A noter : la structure des donnees est relativement contrainte : structure du CMpC entre les colonnes 4:43 et vecteur en input se retrouve en colonne 47 et 48 lignes <- eventuellement a adapter si la structure du CMpC change

Ventile_Vecteur <- function(tablVectEntree)  {
  VecteurAajouter <- tablVectEntree   # On suppose que cette table est déjà dans R, notamment issue d'une précédente agrégation
  # Si cette table est n'est pas déjà dans R : VecteurAajouter <- read.csv(file="XYZ.csv", header=TRUE, sep=";")
  
  # On commence par faire une proc SQL pour ajouter le vecteur d entree a la table de structure du CMpC
  Tab_struct_CMpC_plusVecteur<<-sqldf("select * from tab_Struct_CMpC a, VecteurAajouter b where a.FONC1et2=b.Agrege") 
  
  ### Méthode avec des matrices 
  Tab_struct_CMpC_foisInterm<-as.matrix(Tab_struct_CMpC_plusVecteur)

  # Dans MAT1 on place les éléments de structure
  MAT1=as.matrix(Tab_struct_CMpC_foisInterm[,4:43])
 
   # Dans MAT2 on place les 40 réplication du vecteur du choc ou du contenu qui nous arrive en sortie d'AVIONIC
  MAT2<- matrix(nrow=47, ncol=40)

  # On convertit les éléments de structure en numérique pour pouvoir procéder à la multiplication
  for (i in 1:40) { 
    MAT2[,i]=Tab_struct_CMpC_foisInterm[,47]
  }
  # Export des 2 matrices pour en faciliter la lecture
  write.table(MAT1, "Mat1.xls") # Export 
  write.table(MAT2, "Mat2.xls") # Export 
  # On définit le format de la matrice résultat 
  PRODMAT12<- matrix(nrow=47, ncol=40)

  # Conversion en format numérique avant le produit matriciel terme à terme
  m1 <- mapply(MAT1, FUN=as.numeric)
  MAT1 <- matrix(data=m1, ncol=40, nrow=47)
  m2 <- mapply(MAT2, FUN=as.numeric)
  MAT2 <- matrix(data=m2, ncol=40, nrow=47)
  
  # Produit matriciel terme à terme
  PRODMAT12<-MAT1*MAT2
  write.table(PRODMAT12, "Mat12.xls") # Export 
  Tab_struct_CMpC_foisInterm[,4:43]<-PRODMAT12 # pour que ça reste en variable globale
  Tab_struct_CMpC_foisVecteur<<-Tab_struct_CMpC_foisInterm
  return
}

# Creation d'une fonction pour calculer la strucutre resultant du CMpC et la comparer à la structure moyenne
# Variables : table en entree = table en sortie de la fonction précédente
# A noter : la structure des donnees est relativement contrainte : structure du CMpC entre les colonnes 4:43 et vecteur en input se retrouve en colonne 47 et 48 lignes <- eventuellement a adapter si la structure du CMpC change

resultat_CMpC <- function(tablEntree)  {
   tabW <- tablEntree   # On suppose que cette table est déjà dans R, notamment issue d'une précédente agrégation
   # Si cette table est n'est pas déjà dans R : VecteurAajouter <- read.csv(file="XYZ.csv", header=TRUE, sep=";")
  
  # On commence par faire une proc SQL pour ajouter le vecteur d entree a la table de structure du CMpC
   f<-as.data.frame(Tab_struct_CMpC_foisVecteur)

   RESULTAT<-matrix(data=NA,nrow=3,ncol=40)
   for (i in 4:43) {
     f[,c(i)]<-as.numeric(as.character(f[,c(i)]))
     tempo<-aggregate(f[,c(i)] ~ NiveauCOICOP, f , sum) 
     tempo<-as.matrix(tempo)
     tempo<<-tempo
     RESULTAT[,i-3]<-tempo[,2]
   }
   TabAgreg<<-RESULTAT
   ### Lecture de TabAgreg
   # la ligne 1 de Tabagrer correspond à l'agrégation du niveau 1 COICOP
   # la ligne 2 au niveau 2 COCIOP 
   # la ligne 3 : uniquement directement le Total (sans ventilation au sein des décompositions du compte des ménages par catégories)
   
   return
}






























