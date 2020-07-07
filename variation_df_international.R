

###################################################################################################
##  Variation de la production mondiale par pays en fonction d'un choc exogène de demande finale ##
###################################################################################################


variation_prod_inter <- function (prod_inter, tei_inter, ef_inter, var_df) {
  
  ## 1) Calcul des coefficients techniques
  
  ctec <- coef_techniques (tei_inter, prod_inter)
  
  ## 2) Equation p = (I-A)^(-1)*EF 
  
  P <- inverse(identite(ctec))%*%ef_inter
  
  ## 3) Variation de la producation due à une variation des emplois finals (vecteur var_prod)
  
  ef_prime <- ef_inter + var_df
  P_prime <- inverse(identite(ctec))%*%ef_prime
  
  var_prod <- P_prime - P
  
  names(var_prod) <- names(ef_inter)
  rownames(var_prod) <- rownames(ef_inter)
  
  ## 4) Effet direct (I+A)
  
  
  ctdir <- direct(ctec)
  var_prod_direct <- ctdir%*%var_df
  names(var_prod_direct) <- names(ef_inter)
  rownames(var_prod_direct) <- rownames(ef_inter)
  
  ## 5) Effet indirect (I-A)^(-1)-I-A
  
  ctindir <- indirect(ctec)
  var_prod_indirect <- ctindir %*% var_df
  names(var_prod_indirect) <- names(ef_inter)
  rownames(var_prod_indirect) <- rownames(ef_inter)
  
  var_production_inter <- list(var_p = var_prod, var_p_dir = var_prod_direct, var_p_indir = var_prod_indirect, prod_bis = P_prime)
  
  return(var_production_inter)
  
}


###########################################################################################################
##  Variation des importations pour un  pays en fonction d'un choc exogène de demande finale de celui-ci ##
###########################################################################################################

var_imp_inter <- function (rep, dossier_tei, dossier_prod, dossier_ef, pays_1, pays_2,var_ef_inter) {
  
  var_ef=list()
  for (j in pays_2) {
    var_ef[[j]] <- var_ef_inter
  }
  
  ## 1) Tableau des entrées intermédiaires importées 
  
  tei_imp=list()
  
  for (j in pays_2) {
    
    setwd(dir = paste(rep,"/",dossier_tei,"/TEI ",j,sep=""))
    
    for(i in pays_1){ 
      if (i != j) {
        tei_imp[[j]][[i]] <- matrice(fichier = paste("TEI_imp_",j,"_",i,".csv",sep=""))  
        names(tei_imp[[j]][[i]]) <- paste ("TEI",j,i,sep="_")
      }
    }
  }
  
  
  ## 2) Tableau des entrées intermédiaires domestiques 
  
  tei_dom=list()
  
  for (j in pays_2) { 
    
    setwd(dir = paste(rep,"/",dossier_tei,"/TEI ",j,sep=""))
    
    tei_dom[[j]] <- matrice(fichier = paste("TEI_dom_",j,".csv",sep=""))  
    names(tei_dom[[j]]) <- paste ("TEI_dom_",j,sep="")
  }
  
  ## 3) Vecteur de production domestique 
  
  setwd(dir = paste(rep,"/",dossier_prod,sep=""))
  
  prod=list()
  
  for (j in pays_2) {
    
    prod[[j]] <- matrice(fichier = paste("prod_dom_",j,".csv",sep=""))  
    names(tei_dom[[j]]) <- paste ("prod_dom_",j,sep="")
  }
  
  ## 4) Vecteur d'un emploi final importé   # conso importée
  
  ef_i=list()
  
  for (j in pays_2) { 
    
    setwd(dir = paste(rep,"/",dossier_ef,"/EF ",j,sep=""))
    
    for(i in pays_1){ 
      if (i != j) {
        ef_i[[j]][[i]] <- matrice(fichier = paste("EF_imp_",j,"_",i,".csv",sep=""))  
        names(ef_i[[j]][[i]]) <- paste ("EF",j,i,sep="_")
      }
    }
  }
  
  ## 5) Vecteur d'un emploi final domestique 
  
  ef_d=list()
  
  for (j in pays_2) { ## on enlève le reste du monde
    
    setwd(dir = paste(rep,"/",dossier_ef,"/EF ",j,sep=""))
    
    ef_d[[j]] <- matrice(fichier = paste("EF_dom_",j,".csv",sep=""))  
    names(ef_d[[j]]) <- paste ("EF_dom_",j,sep="")
  }
  
  
  ## 6) Calculs des contenus directs en importations       ##
  
  
  contenus_directs <- ef_i
  
  
  
  ## 7) Calculs des contenus indirects en importations     
  
  
  
  #  Calcul des coefficients techniques
  
  ctec_imp=list()
  ctec_dom=list()
  
  for (j in pays_2) {
    
    for(i in pays_1){ 
      if (i != j) {
        ctec_imp[[j]][[i]] <- coef_techniques(A= tei_imp[[j]][[i]], B= prod[[j]])
      }
    }
  }
  
  
  for(i in pays_2){
    
    ctec_dom[[i]] <- coef_techniques(A= tei_dom[[i]], B= prod[[i]])
    
  }
  
  #  Equation contenus_indirects = A_imp(I-A_dom)^(-1)*EF_dom + EF_imp 
  
  cont_ind_detail = list()
  contenus_indirects = list()
  I_A = list()
  inv_I_A = list()
  diag_ef_d = list()
  
  for (j in pays_2) {
    
    I_A[[j]] <- identite(A=ctec_dom[[j]])
    inv_I_A[[j]] <- inverse(mat=I_A[[j]])
    
    diag_ef_d[[j]] <- diag(as.vector(ef_d[[j]]))
    
  }
  
  
  for (j in pays_2) {
    
    for (i in pays_1) {
      if (i != j) {
        cont_ind_detail[[j]][[i]] <- ctec_imp[[j]][[i]]%*%inv_I_A[[j]]%*% diag_ef_d[[j]]
        contenus_indirects[[j]][[i]]<- as.matrix(apply(cont_ind_detail[[j]][[i]], 2, sum))
      }
    }
    
  }
  
  ## 8)  Calculs des contenus totaux en importations      
  
  
  contenus_imp= list()
  
  for (j in pays_2) {
    
    for (i in pays_1) {
      if (i != j) {
        
        contenus_imp[[j]][[i]]<- contenus_directs[[j]][[i]] + contenus_indirects[[j]][[i]]
        
      }
    }
    
  }
  
  
  ## 8) Calculs des contenus indirects en importations après variation
  
  
  
  #  Equation contenus_indirects = A_imp(I-A_dom)^(-1)*var_ef
  
  cont_ind_detail_prime = list()
  contenus_indirects_prime = list()
  diag_var_ef = list()
  
  for (j in pays_2) {
    
    diag_var_ef[[j]] <- diag(as.vector(var_ef[[j]]))
    
  }
  
  for (j in pays_2) {
    
    for (i in pays_1) {
      if (i != j) {
        cont_ind_detail_prime[[j]][[i]] <- ctec_imp[[j]][[i]]%*%inv_I_A[[j]]%*% diag_var_ef[[j]]
        contenus_indirects_prime[[j]][[i]]<- as.matrix(apply(cont_ind_detail[[j]][[i]], 2, sum))
      }
    }
    
  }
  
  ## 8)  Calculs des contenus totaux en importations      
  
  
  contenus_imp_prime = list()
  
  for (j in pays_2) {
    
    for (i in pays_1) {
      if (i != j) {
        
        contenus_imp_prime[[j]][[i]]<- contenus_directs[[j]][[i]] + contenus_indirects[[j]][[i]] + contenus_indirects_prime[[j]][[i]]
        
      }
    }
    
  }
  
  var_imp_prime = list()
  
  for (j in pays_2) {
    
    for (i in pays_1) {
      if (i != j) {
        
        var_imp_prime[[j]][[i]]<- contenus_imp_prime[[j]][[i]] - contenus_imp[[j]][[i]]
        
      }
    }
    
  }
  
  contenus_imp_inter <- list(var_imp = var_imp_prime, imp = contenus_imp, imp_prime = contenus_imp_prime)
  return(contenus_imp_inter)
}


