# useful functions

#' hh
#' shorter for the here::here function
#' see here::here for more info
#' @param path path to file
#' @param ... 
#'
#' @return path to file
#' @export
hh <- function(path, ...){
  
  here::here(path, ...)
} # eo of hh

#' look_cor
#' This function orders the species according to their individual effect on the aesthetic score
#' @param all_id 
#' @param dat_cor 
#' @param respvar 
#' @param ord 
#' @export
#' 
look_cor <- function(all_id, dat_cor, respvar, ord){
  
  # all_id  = sp_names
  # dat_cor = data
  # respvar  = "esth_score"
  # ord     = "PERVAR"
  
  # linear regression between the target variable "respvar" and all the explaining 
  # variables "all_id"
  
  modall <- lm(as.formula(paste(respvar," ~ ", paste(all_id, collapse= "+"))), data = dat_cor,
               na.action = na.omit) 
  
  # Create a matrix of which number of rows is the number of explaining variables
  out_data           <- as.data.frame(matrix(NA, ncol = 4, nrow = length(all_id)))
  colnames(out_data) <- c("var","pervar","R2","pearson.p")
  out_data$var       <- all_id
  
  # Fill the matrix  
  for (i in 1:length(all_id)){
    # individual corelation between "respvar" and each explaining variable
    cort                  <- cor.test(dat_cor[,respvar],dat_cor[,all_id[i]]) 
    
    # get the pearson coefficient of the correlation between "respvar" and each explaining variable
    out_data$pearson.p[i] <- cort$p.value
    
    # get the % of variance of "respvar" explained by all the variables except one (variable i)
    modminus              <- lm(as.formula(paste(respvar," ~ ", paste(all_id[-i], collapse= "+"))),
                                data = dat_cor, na.action = na.omit) 
    # Rsquared of the lm between "respvar" and variable i
    modalone              <- lm(as.formula(paste(respvar," ~ ", all_id[i])), data = dat_cor,
                                na.action = na.omit) 
    
    # get the lost % of "respvar" vexplained variance if variable i is removed
    out_data$pervar[i]    <- (100-(summary(modminus)[[8]]/summary(modall)[[8]])*100) 
    
    # keep the R2 of the relation between "respvar" and the variable i alone
    out_data$R2[i]        <- summary(modalone)[[8]]
  } # eo for i
  
  if (ord == "R2") corres      <- out_data[order(-out_data$R2),]
  if (ord == "PERVAR") corres  <- out_data[order(-out_data$pervar),]
  if (ord == "PEARSON") corres <- out_data[order(-out_data$pearson.p),]
  
  return(corres)
} # eo look_cor


#' normalize
#' @param x 
#' @export
#' 
normalize <-  function(x){(x-mean(x))/sd(x)}


#' reduce_mod 
#' reduces the model by removing the non-significant species
#' @param sp_list 
#' @param respvar 
#' @param dat_cor 
#' @param thr 
#' @export
#' 
#' @return
reduce_mod <- function(sp_list, respvar, dat_cor, thr){
  
  # thr  = threshold of significativity
  # sp_list <- SPE_order
  # sp_list = SPE_order; respvar = "esth_score; dat_cor = data_num; thr = 0.05
  
  if(length(sp_list) == 1) return(sp_list) #stop("No more species")
  formu <- as.formula(paste0(respvar,"~", paste0(sp_list, collapse = "+")))
  mod_full                  <- lm(formula = formu, data = dat_cor, na.action = na.omit)
  mod_full_sum              <- summary(mod_full)
  mod_full_sum_coeff_sorted <- mod_full_sum$coefficients[
    order(mod_full_sum$coefficients[,"Pr(>|t|)"], decreasing = TRUE),]
  mod_full_sum_coeff_sorted <- mod_full_sum_coeff_sorted[!(rownames(mod_full_sum_coeff_sorted) %in%
                                                             c("sr","I(sr^2)","(Intercept)")),]
  
  least_sig <- rownames(mod_full_sum_coeff_sorted)[1]
  
  if(mod_full_sum_coeff_sorted[1,"Pr(>|t|)"] > thr) {
    cat("removing ",least_sig, "\n")
    sp_list <- sp_list[!(sp_list %in% least_sig)]
    
    Recall(sp_list = sp_list, respvar = respvar, dat_cor, thr)
  } else {
    return(sp_list)
  }
  
}#eo reduce_mod

#' rep.row
#' repeat row of matrix
#' @param x 
#' @param n 
#' @export
rep.row   <- function(x, n){matrix(rep(x,each = n), nrow = n)}


#' standefsize
#' This function generates nb null assemblages of each possible number of species in the data table 
#' computes the mean and sd of the div index on these assemblages
#' computes the deviation between the observed div and the SES_div
#' @param div       the diversity worked with : c(PD,FD)
#'                  if div = PD, hillfun = relab_hill_phylo
#'                  if div = FD, hillfun = FD_MLE
#' @param data      matrix of relative abundance. Quadrat_code must be rownames and Species_name
#'                  must be colnames for div = PD, colnames = id_name of the species, for div = TD
#'                  or FD colnames(data) = id_code of the species
#' @param tree      phylotree generated by ape::read_tree. Default is NULL. Only needed if div = PD
#' @param dij       distance matrix generated by ade4::dist.kab (Pavoine et al. 2009). Default is
#'                  NULL. Only needed if div = FD
#' @param tau       threshold for dij (Chao et al. 2019). Default is NULL, recommended value is
#'                  mean(dij). Only needed if div = FD
#' @param q         the value of q to use, same used to compute the "observed" PD or FD
#' @param hilldiv   vector of hillnumber (qPD or qFD) for all assemblages
#' @param log_trans logical. defaukt is FALSE. if TRUE, the SES if computed on the log of the index
#' @param nb        number of null models to generate (9999 is default)
#' @param mcores    number of cores to use
#'
#' @return a matrix of three columns : observed hillnumber, nb of species, SES hill number
#' @export
standefsize <- function(div, data, tree = NULL, dij = NULL, tau = NULL, q, hilldiv,
                        log_trans = FALSE, nb = 10, mcores = 20){
  # compute the random abundance matrices
  X  <- data
  nm <- vegan::nullmodel(x = X, method = "r2dtable") # create the model.
  # 1. non binary as working with abundances and no presence/absence
  # 2. conserves the sums of rows and columns
  # 3. non sequentiel so the simulated matrices are independant one from the other
  # 4. don't conserve the fill ie the number of empty (0) cells in the matrix. 
  sm <- stats::simulate(object = nm, nsim = nb)
  
  # get the hill numbers for all quadrats of all simulated matrices
  nuls_index <- c()
  ind <- do.call(cbind, parallel::mclapply(1:dim(sm)[3], function(i){ 
    # work on one matrix
    mat        <- sm[, , i]
    # transform abundance to relative abundance
    abondquad  <- as.matrix(apply(mat, 1, sum))    # count the number of indiv per quadrat
                                                   # considering all species
    mabondquad <- t(rep.row(abondquad, ncol(mat))) # create a matrix of same size to be able to
                                                   # divide the number of individual of each
                                                   # species per the number of individuals in the
                                                   # entire quadrat
    relab_quad <- mat/mabondquad                   # Relative abundance of each species for all
                                                   # quadrats 
    # if no sp division by zero so gives Nan ==> change Nan to zero
    if(length(which(is.na(relab_quad))) != 0){relab_quad[is.na(relab_quad)] <- 0} # eo if
    # compute the Hill number
    if (div == "PD"){ 
      nul_index <- relab_hill_phylo(comm = relab_quad, tree, q) 
    }# eo if PD
    if (div == "FD"){ 
      nul_index <- apply(relab_quad, 1, function(assemblage){ 
        FD_MLE(data = assemblage, dij = dij, tau = tau, q = q)
      })# eo function
    } # eo if FD
    if(log_trans == TRUE){nul_index <- log(nul_index)}
    nuls_index <- c(nuls_index, nul_index)
  }, mc.cores = mcores))
  
  # Compute the Z-transformed index
  hilldiv <- hilldiv[order(names(hilldiv))]
  if (length(setdiff(hilldiv$quadrat_code, rownames(ind))) != 0)
    stop("Not the same quadrats")
  if (length(setdiff(rownames(ind), hilldiv$quadrat_code)) !=0)
    stop("Not the same quadrats")
  
  # compute the mean and sd of the 1000 simulated indices for each of the null models
  mu    <- apply(ind, 1, mean)
  mu    <- mu[order(names(mu))]
  sigma <- apply(ind, 1, sd)
  sigma <- sigma[order(names(sigma))]
  
  # SES
  SES_hilldiv     <- data.frame(hilldiv = hilldiv[,1], mu = mu, sigma = sigma)
  # colnames(SES_hilldiv) <- c("hilldiv", "mu", "sigma")
  SES_hilldiv     <- as.data.frame(SES_hilldiv)
  SES_hilldiv$SES <- (SES_hilldiv$hilldiv - SES_hilldiv$mu)/SES_hilldiv$sigma
  # case where the hill number equals 1 and sigma is null create Nan (no division by 0) but if mu
  # is the same than observed index, SES is null
  SES_hilldiv$SES[which(SES_hilldiv$hilldiv == SES_hilldiv$mu & SES_hilldiv$sigma == 0)] <- 0
  
  # output
  SES_hilldiv
}

