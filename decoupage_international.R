#######################################
##  DECOUPAGE DES TES INTERNATIONAUX  ##
#######################################

#######################
##  DECOUPAGE DU TEI ##
######################

decoupage_tei <- function(tei_int, pays, nb_produits, nb_pays, dossiers_tei) {
  
  n <- nb_produits
  p <- nb_pays
  
  ## Calcul des fonctions de découpage selon le nombre de pays et de produits de la base
  
  TEI <- function(i,j){ 
    M <- matrix(data = 0:0,nrow = n, ncol = n)
    for(a in 1:n){
      for(b in 1:n){
        M[a,b] = tei_int[a+n*(i-1),b+n*(j-1)]
      }
    }
    return(M) 
  }
  
  TEIsommes <- function(j){ 
    M <- matrix(data = 0:0,nrow = n, ncol = n)
    for(i in 1:p){
      if(i!=j){
        M <- M + TEI(i,j)
      }
    }
    return (M)
  }
  
  
  ##  Export des TEI domestiques et importés par pays de provenance en csv
  
  for(j in 1:p){
    pays_etudie <- pays[j] 
    path <- paste(dossiers_tei,"/TEI ",pays_etudie,sep="")
    dir.create(path)
    setwd(path)  
    for (i in 1:p){
      if(i==j){  # cas domestique
        name <- paste("TEI","dom",pays_etudie,sep="_")
        name_csv <- paste(name,"csv",sep=".")
        write.table(TEI(i,j),file=name_csv,sep=";",dec=".",col.names = FALSE, row.names = FALSE)
      }
      else{   # cas importé
        pays_2 <- pays[i]
        name <- paste("TEI","imp",pays_etudie,pays_2,sep="_")
        name_csv <- paste(name,"csv",sep=".")
        write.table(TEI(i,j),file=name_csv,sep=";",dec=".",col.names = FALSE,row.names = FALSE)
      }
    }
    
  }
  
}

############################
##  DECOUPAGE PRODUCTION ##
###########################

decoupage_prod <- function(prod, pays, nb_produits, nb_pays, dossiers_prod) {
  
  n <- nb_produits
  p <- nb_pays
  
  ## Calcul des fonctions de découpage selon le nombre de pays et de produits de la base
  
  Proddom <- function(i){
    v <- numeric(n)
    for(a in 1:n){
      v[a] = prod[a+n*(i-1)]
    }
    return(v) 
  }
  
  ##  Export des productions en csv
  
  path <- dossiers_prod
  setwd(path) 
  for(i in 1:p){
    pays_etudie <- pays[i] 
    name <- paste("prod","dom",pays_etudie,sep="_")
    name_csv <- paste(name,"csv",sep=".")
    write.table(Proddom(i),file=name_csv,sep=";",dec=".",col.names = F, row.names = F)
  }
  
}

############################
##  DECOUPAGE EMPLOI FINAL ##
############################


decoupage_ef <- function(ef, pays, nb_produits, nb_pays, dossiers_ef) {
  
  n <- nb_produits
  p <- nb_pays
  
  ## Calcul des fonctions de découpage selon le nombre de pays et de produits de la base
  
  Vect <- function(i,j){
    M <- matrix(data = 0:0,nrow = n, ncol = 1)
    for(a in 1:n){
      M[a,1] = ef[a+n*(i-1),j]
    }
    return(M) 
  }
  
  Vect_tot <- function(j){
    M <- matrix(data = 0:0,nrow = n, ncol = 1)
    for(i in 1:p){
      if(i!=j){
        M <- M + Vect(i,j)
      }
    }
    return(M)
  }
  
  ##  Export des emplois finals domestiques et importés par pays de provenance en csv
  
  for(j in 1:p){
    pays_etudie <- pays[j]  
    path <- paste(dossiers_ef,"/EF ",pays_etudie,sep="")
    dir.create(path)
    setwd(path)  
    for (i in 1:p){
      if(i==j){  # cas domestique
        name <- paste("EF","dom",pays_etudie,sep="_")
        name_csv <- paste(name,"csv",sep=".")
        write.table(Vect(i,j),file=name_csv,sep=";",dec=".",col.names = F, row.names = F)
      }
      else{   # cas importé
        pays_2 <- pays[i]
        name <- paste("EF","imp",pays_etudie,pays_2,sep="_")
        name_csv <- paste(name,"csv",sep=".")
        write.table(Vect(i,j),file=name_csv,sep=";",dec=".",col.names = F,row.names = F)
      }
    }
    name <- paste("EF","tot",pays_etudie,sep="_")
    name_csv <- paste(name,"csv",sep=".")
    write.table(Vect_tot(j),file=name_csv,sep=";",dec=".",col.names = F, row.names = F)
  }
  
}



