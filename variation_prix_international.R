
#################################################################################
##    Variation du prix de production en fonction d'un choc exogène de prix    ##
#################################################################################

variation_prix_inter <- function(prod_inter, tei_inter, num_produit, num_pays, nb_produit, var){
  
  ## 1) Calcul des coefficients techniques
  
  ctec <- coef_techniques(A= tei_inter, B= prod_inter)
  
  ## 2) Créations des matrices en intermédiaires (amputées du produit variant)
  
  num <- (num_pays-1)*nb_produit + num_produit
  
  ctec_prime <- t(ctec[-num,-num])
  a <- as.matrix(ctec[num,])
  a_i <- as.matrix(a[-num])
  
  ## 3) Equation delta_p = (I-A)^(-1)*a_iT*delta_pi
  
  I_A <- identite(A=ctec_prime)
  inv_I_A <- inverse(mat=I_A)
  
  delta_p <- inv_I_A%*%a_i%*%(var/100)
  
  debut <- as.matrix(delta_p[1:num-1])
  fin <- as.matrix(delta_p[num:length(delta_p)])
  delta_p <- as.matrix(c(debut*100,var,fin*100))
  
  ## 4) Calcul de la variation totale du prix 
  
  valeur_x_var <- prod_inter*delta_p
  
  
  ## 5) Effet direct a_iT*delta_pi et effet indirect (I-A)^(-1)*a_iT*delta_pi*-a_iT*delta_pi
  
  delta_p_direct <- a_i%*%(var/100)
  
  debut_dir <- as.matrix(delta_p_direct[1:num-1])
  fin_dir <- as.matrix(delta_p_direct[num:length(delta_p_direct)])
  delta_p_direct <- as.matrix(c(debut_dir*100,var,fin_dir*100))
  
  delta_p_indirect = delta_p - delta_p_direct
  
  names(delta_p) <- names(prod_inter)
  names(delta_p_direct) <- names(prod_inter)
  names(delta_p_indirect) <- names(prod_inter)
  
  var_prix <- list(var_prix= delta_p, var_prix_dir=delta_p_direct, var_prix_indir=delta_p_indirect)
  
  return(var_prix)
  
  cat("ATTENTION \n Rappel, les résultats ne seront pas les mêmes en fonction de si vous avez entré un TEI seul domestique ou domestique et importé. Se reporter à la documentation")
} 
 







