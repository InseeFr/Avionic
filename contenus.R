
####################################################################
##  CONTENUS EN IMPORTATIONS DES COMPOSANTES DE LA DEMANDE FINALE ##
####################################################################

contenus_imp <- function(prod, tei_dom, tei_imp, ef_d, ef_i) {
  
  ##  Calculs des contenus directs en importations
  
  contenus_directs <- t(ef_i)
  prct_contenus_directs <- (contenus_directs / (t(ef_i) + t(ef_d)))*100
  
  ##  Calculs des contenuus indirects en importations
  
       ## 1) Calcul des coefficients techniques
  
  ctec_imp <- coef_techniques(A= tei_imp, B= prod)
  ctec_dom <- coef_techniques(A= tei_dom, B= prod)
  
       ## 2) Equation contenus_indirects = A_imp(I-A_dom)^(-1)*ef_dom + ef_imp 
  
  I_A <- identite(A=ctec_dom)
  inv_I_A <- inverse(mat=I_A)
  
  diag_ef_d <- diag(as.vector(ef_d))
  
  cont_ind_detail <- ctec_imp %*% inv_I_A %*% diag_ef_d 
  contenus_indirects <- as.matrix(apply(cont_ind_detail, 2, sum))
  prct_contenus_indirects <- (contenus_indirects / (t(ef_i) + t(ef_d)))*100
  
  ##  Calculs des contenuus totaux en importations
  
  diag_cont_directs <- diag(as.vector(contenus_directs))
  cont_imp_detail <- diag_cont_directs + cont_ind_detail
  contenus_imp <- contenus_directs + contenus_indirects 
  prct_contenus_imp <- (contenus_imp / (t(ef_i) + t(ef_d)))*100
  
  contenus_importations <- list(c_imp=contenus_imp, c_imp_dir=contenus_directs, c_imp_indir = contenus_indirects, #c_imp_det=cont_imp_detail, c_imp_ind_det=cont_ind_detail, 
                                prct_c_imp =prct_contenus_imp, prct_c_imp_dir=prct_contenus_directs,prct_c_imp_indir=prct_contenus_indirects)
  
  return(contenus_importations)
  
}

######################################################################
##  CONTENUS EN VALEUR AJOUTEE DES COMPOSANTES DE LA DEMANDE FINALE ##
######################################################################


contenus_va <- function(prod, tei_dom, tei_imp, ef_d, ef_i) {
  
     ## 1) Création d'une matrice de valeur ajoutée diagonale
  
  tot_ci <- as.matrix(apply(tei_dom, 2, sum))+as.matrix(apply(tei_imp, 2, sum))
  va_vect <- (prod - t(tot_ci))/prod
  for (j in 1:length(va_vect)){
    if (is.na(va_vect[j])==T){
      va_vect[j]=0
    }
  }
  
  
  mat_va <- diag(as.vector(va_vect))
  
     ## 2) Calcul des coefficients techniques
  
  ctec <- coef_techniques(A= tei_dom, B= prod)
  
     ## 3) Calcul du contenu en valeur ajoutée de l'emploi final considéré
  
  I_A <- identite(A=ctec)
  inv_I_A <- inverse(mat=I_A)
  
  diag_ef_d <- diag(as.vector(ef_d))
  
  cont_va_detail <- mat_va %*% inv_I_A %*% diag_ef_d
  contenus_va <- as.matrix(apply(cont_va_detail, 2, sum))
  
  ef_d <- ef_d + ef_i
  prct_contenus_va <- (contenus_va /t(ef_d))*100
  
  ##  Calculs des contenus directs et indirects
  
     ## 1) effet direct (I+A)
  
  ctdir <- direct(A = ctec)
  cont_va_direct_detail <- mat_va %*% ctdir %*% diag_ef_d
  contenus_va_direct <- as.matrix(apply(cont_va_direct_detail, 2, sum))
  prct_direct_va <- (contenus_va_direct/t(ef_d))*100
  
   
     ## 2) effet indirect (I-A)^(-1)-I-A
  
  ctindir <- indirect(A = ctec)
  cont_va_indirect_detail <- mat_va %*%ctindir %*% diag_ef_d
  contenus_va_indirect <- as.matrix(apply(cont_va_indirect_detail, 2, sum))
  prct_indirect_va <- (contenus_va_indirect/t(ef_d))*100
  
  contenus_va <- list(c_va=contenus_va, c_va_dir=contenus_va_direct, c_va_indir = contenus_va_indirect, #c_va_det=cont_va_detail, c_va_dir_det=cont_va_direct_detail, c_va_ind_det=cont_va_indirect_detail, 
                      prct_c_va =prct_contenus_va, prct_c_va_dir=prct_direct_va, prct_c_va_indir=prct_indirect_va)
  
  return(contenus_va)
  
}

##############################################################
##  CONTENUS EN EMPLOI DES COMPOSANTES DE LA DEMANDE FINALE ##
##############################################################

contenus_emploi <- function(prod, tei_dom, ef_d, emploi) {
  
  ##  Calculs des contenus emploi 
  
  
    ## 1) Création d'une matrice d'emploi diagonale
  
  emploi_vect <- emploi/prod
  for (j in 1:length(emploi_vect)){
    if (is.na(emploi_vect[j])==T){
      emploi_vect[j]=0
    }
  }
  for (j in 1:length(emploi_vect)){
    if (emploi_vect[j]==Inf){
      emploi_vect[j]=0
    }
  }
  
  mat_emploi <- diag(as.vector(emploi_vect))
  
    ## 2) Calcul des coefficients techniques
  
  ctec <- coef_techniques(A= tei_dom, B= prod)
  
    ## 3) Calcul du contenu en équivalent temps plein de l'emploi final considéré
  
  I_A <- identite(A=ctec)
  inv_I_A <- inverse(mat=I_A)
  
  diag_ef_d <- diag(as.vector(ef_d))
  
  cont_emploi_detail <- mat_emploi %*% inv_I_A %*% diag_ef_d
  contenus_emploi <- as.matrix(apply(cont_emploi_detail, 2, sum))
  
    ## 4) Calcul du contenu en équivalent temps plein par million de l'emploi considéré
  
  unit_contenu_emploi <- contenus_emploi /t(ef_d)
  
  ## Calculs des contenus directs et indirects
  
  
    ## 1) effet direct I
  
  cont_emploi_direct_detail <- mat_emploi %*% diag_ef_d
  contenus_emploi_direct <- as.matrix(apply(cont_emploi_direct_detail, 2, sum))
  unit_direct_emploi <- contenus_emploi_direct/t(ef_d) # contenu direct en emploi par million de l'emploi considéré
  
  
    ## 2) effet indirect (I-A)^(-1)-I
  
  
  ctindir <- indirect(A = ctec) + ctec 
  cont_emploi_indirect_detail <- mat_emploi %*%ctindir %*% diag_ef_d
  contenus_emploi_indirect <- as.matrix(apply(cont_emploi_indirect_detail, 2, sum))
  unit_indirect_emploi <- contenus_emploi_indirect/t(ef_d) # contenu indirect en emploi par million de l'emploi considéré
  
  contenus_emploi <- list(c_emp=contenus_emploi, c_emp_dir=contenus_emploi_direct, c_emp_indir = contenus_emploi_indirect, #c_emp_det=cont_emploi_detail, c_emp_dir_det=cont_emploi_direct_detail, c_emp_ind_det=cont_emploi_indirect_detail, 
                          unit_c_emp =unit_contenu_emploi, unit_c_emp_dir=unit_direct_emploi, unit_c_emp_indir=unit_indirect_emploi)
  
  return(contenus_emploi)
  
}
