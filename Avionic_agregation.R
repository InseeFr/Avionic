###############################################
###############################################
#                                             #
#    AVIONIC - Fonctions d'agrégations        #
#                                             #
###############################################
###############################################

## BLOC DES REFERENCES ################################################################################

#######################################################################################################
## FONCTION D AGREGATION EN NA - NAF Rev 2 ############################################################
##
## Cette fonction permet d agreger un vecteur de tout niveau NA a tout niveau moins fin NA
#######################################################################################################

## Si besoin, on peut charger la table de passage NA qui part implicitement du niveau A138 et propose les correspondances avec tous les niveaux plus agreges
# tabPass <- read.csv(file="TabPass_NA.csv", header=TRUE, sep=";") # Chargement de la table de passage nomenclature NA

# Creation d'une fonction pour agreger tout vecteur a tout niveau de nomenclature
# Variables : table/vecteur en entree ; Nom de la variable a agreger dans la table en entree ; niveau de nomenclature dans la table en entree, niveau de nomenclature dans la table de destination (plus agrege que la nomenclature dans la table en entree)
# Niveaux possibles : NA10, NA17, NA38, NA64, NA88, NA129
# A noter : le nom de la colonne de nomenclature a agreger doit porter le nom de sa nomenclature : par exemple "NA129"

Agreg_NA <- function(tablEntree, var_tablEntree , niveau_tablEntree, niveau_sortie)  {
  # Si nécessaire : tabPass <- read.csv(file="TabPass_NA.csv", header=TRUE, sep=";")
  tabPass <- TabPass_NA
  tabAagreger <- tablEntree
  Requete <- paste0("select * , sum(",var_tablEntree)
  Requete <- paste0(Requete,") as VAL from tabPass a, tabAagreger b where a.")  
  Requete <- paste0(Requete,niveau_tablEntree)
  Requete <- paste0(Requete,"=b.")  
  Requete <- paste0(Requete,niveau_tablEntree)  
  Requete <- paste0(Requete," group by ")  
  Requete <- paste0(Requete, "a.")
  Requete <- paste0(Requete, niveau_sortie)
  cat(Requete)
  TabAfter<<-sqldf(Requete)  # On définit une variable globale pour récupérer la table en sortie de la fonction
  # La table agregee se trouve dans TabAfter
  TabAfter<<-subset(TabAfter, select = c( niveau_sortie, "VAL" ))
  return
}


#######################################################################################################
## FONCTION AGREG FONCTIONNELLE #######################################################################
##
## Cette fonction permet d agreger un vecteur de tout niveau NA en nomenclature fonctionnelle 2 digit
#######################################################################################################
# Lorsqu'on dispose d'une table agrégrée (cf ci-dessus) ou d'une table directement à un niveau NA
# Niveaux possibles : A10, A17, A38, A64, A88, A129
# On va appliquer la table de passage ad'hoc pour passer d'un niveau NA a la nomenc fonctionnelle
# En nomenclature fonctionnelle on peut appliquer le compte des ménages par catégories pour obtenir des décompositions

# En toute logique, ce passage en fonctionnelle fait suite à une agrégation du type de la fonction précédente.
# La table a agréger en fonctionnelle doit au préalable être dans la var globale TabAfter
# Si beson de la charger : ajouter TabAfter <- read.csv(file="chemin", header=TRUE, sep=";")

# Variables : niveauNA_depart = niveau de nomenclature de départ en NA. Exemple : niveauNA_depart = "NA17". le niveau d'arrivée est connu par avance, c'est la fonctionnelle COICOP 2 digits. 


Agreg_NA_fonc <- function(niveauNA_depart,TabAfter, variable)  {
  Chemin <-  paste0("TabPass_", niveauNA_depart)
  Chemin <- paste0(Chemin, "_vers_fonc")  
  tabPass <- get(Chemin) 
  
  
  
  
  # si import depuis un fichier externe CSV :
  #Chemin <-  paste0("TabPass_", niveauNA_depart)
  #Chemin <- paste0(Chemin, "_vers_fonc.csv")  
  #tabPass <- read.csv(file=Chemin, header=TRUE, sep=";")
  
  tabAagreger <- TabAfter # A charger au préalable si besoin ; là on considère qu'elle est déjà là. Si besoin ajouter TabAfter <- read.csv(file="chemin", header=TRUE, sep=";")
  
  # Requête SQL d'agrégation : 
  Requete <- paste0("select a.Fonc , sum(a.Coef*b.",variable)
  Requete <- paste0(Requete,")as VALFONC from tabPass a, tabAagreger b where a.Pr_" , niveauNA_depart)
  Requete <- paste0(Requete, "=b.")
  Requete <- paste0(Requete, niveauNA_depart) 
  Requete <- paste0(Requete, " group by a.Fonc")
  cat(Requete)
  TabAfter_fonc<<-sqldf(Requete)  # On définit une variable globale pour récupérer la table en sortie de la fonction
  
  ## Passage ou on calcule le niveau 1 digit de la COICOP afin de disposer des niveaux 1 et 2 de la COICOP dans le fichier de sortie
  tabPass_niv1COICOP <- TabPass_foncdet_vers_fonc10 # si nécessaire : read.csv(file="TabPass_foncdet_vers_fonc10.csv", header=TRUE, sep=";")  
  tabAagreger <- TabAfter_fonc # Est issu de la variable globale calculée précédemment
  TabAfter_fonc_niv1et2COICOP<<-sqldf("select * , sum(VALFONC) as VAL_FONC from tabPass_niv1COICOP a, tabAagreger b where a.Detail=b.Fonc group by Agrege") 
  TabAfter_fonc_niv1et2COICOP<<-subset(TabAfter_fonc_niv1et2COICOP, select = c( "OrdreAffich", "Agrege","Libelle",  "VAL_FONC" )) 
  return
}

