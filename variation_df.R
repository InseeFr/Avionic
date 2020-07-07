
#################################################################################
##  Variation de la production en fonction d'un choc exogène de demande finale ##
#################################################################################

variation_prod <- function(prod, tei_dom, df_d, var_df){
  
  
  ## 1) Calcul des coefficients techniques
  
  ctec <- coef_techniques(A= tei_dom, B= prod)
  
  ## 2) Equation p = (I-A)^(-1)*EF 
  
  I_A <- identite(A=ctec)
  inv_I_A <- inverse(mat=I_A)
  p <- inv_I_A %*% t(df_d) 
  
  ## 3) Variation de la production due à une variation des emplois finals (vecteur var_prod)
  
  df_prime <- df_d + var_df
  p_prime <- inv_I_A %*% (t(df_prime)) 
  var_prod <- p_prime - p
  
  ## 4) Effet direct (I+A)
  
  ctdir <- direct(A = ctec)
  var_prod_direct <- ctdir %*% (t(var_df))
  
  
  ## 5) Effet indirect (I-A)^(-1)-I-A
  
  ctindir <- indirect(A = ctec)
  var_prod_indirect <- ctindir %*% (t(var_df))
  
  var_production <- list(var_p = var_prod, var_p_dir = var_prod_direct, var_p_indir = var_prod_indirect, prod_bis = p_prime)
  
  return(var_production)
  
}

#################################################################################
##  Variation des importations en fonction d'un choc exogène de demande finale ##
#################################################################################


variation_imp <- function(prod, tei_dom, tei_imp, df_d, df_i, var_df){
  
  ## 1) Calcul des coefficients techniques
  
  ctec_imp <- coef_techniques(A= tei_imp, B= prod)
  ctec_dom <- coef_techniques(A= tei_dom, B= prod)
  
  
  ## 2) Equation m = A_imp(I-A_dom)^(-1)*EF_dom + EF_imp 
  
  I_A <- identite(A=ctec_dom)
  inv_I_A <- inverse(mat=I_A)
  m <- ctec_imp %*% inv_I_A %*% t(df_d) + t(df_i) 
  
  ## 3) Variation des importations due à une variation des emplois finals (dom et imp) (vecteur var_imp)
  
  df_d_prime <- df_d + var_df
  m_prime <- ctec_imp %*% inv_I_A %*% t(df_d_prime) + t(df_i)  
  var_imp <- m_prime - m
  
  ## 4) Effet direct (A_m)
  
  var_imp_direct <- ctec_imp %*% (t(var_df))
  
  
  ## 5) Effet indirect (I-A)^(-1)-I-A
  
  ctindir_imp <- indirect_imp (A = ctec_dom, B=ctec_imp)
  var_imp_indirect <- ctindir_imp %*% (t(var_df))
  
  var_importations <- list(var_imp = var_imp, var_imp_dir = var_imp_direct, var_imp_indir = var_imp_indirect,# imp = m,
                           imp_bis= m_prime)
  
  return(var_importations)
  
}

############################################################################
##  Variation de l'emploi en fonction d'un choc exogène de demande finale ##
############################################################################

variation_emploi <- function(prod, tei_dom, df_d,emploi, var_df){
  
  
  ## 1) Création d'une matrice d'emploi diagonale
  
  emploi_vect <- emploi/prod
  
  mat_emploi <- diag(as.vector(emploi_vect))
  
  ## 2) Calcul des coefficients techniques
  
  ctec <- coef_techniques(A= tei_dom, B= prod)
  
  ## 3) Calcul de la variation des emploi
  
  I_A <- identite(A=ctec)
  inv_I_A <- inverse(mat=I_A)
  
  
  df_prime <- df_d + var_df
  emploi <- mat_emploi %*% inv_I_A %*% t(df_d)
  emploi_prime <- mat_emploi %*% inv_I_A %*% t(df_prime)
  var_emploi <- emploi_prime - emploi
  
  ## 4) Effet direct (I+A)
  
  ctdir <- direct(A = ctec)
  var_emploi_direct <- mat_emploi %*% ctdir %*% (t(var_df))
  
  
  ## 5) Effet indirect (I-A)^(-1)-I-A
  
  ctindir <- indirect(A = ctec)
  var_emploi_indirect <- mat_emploi %*% ctindir %*% (t(var_df))
  
  var_emp<- list(var_emp = var_emploi, var_emp_dir = var_emploi_direct, var_emp_indir = var_emploi_indirect, emp_bis=emploi_prime)
  
  return(var_emp)
  
}
  
  
















