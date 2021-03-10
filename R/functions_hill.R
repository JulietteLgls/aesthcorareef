# Hill numbers functions

# The functions of this scipt compute Hill numbers of given assemblages and determine which value
# of q maximizes the equation E~(D**q). D stands for TD (taxonomic diversity), PD (phylogenetic
# diversity) and FD (functional diversity). The article of Chao et al., 2014 is used as reference.                                                                 

#' hill_taxa
#' computes the taxonomic hill number of each row of a gic-ven matrix 
#' with a given value of q
#' @param comm a matrix with assemblages in row, species relative abundance in column
#' @param q 
#' @param MARGIN 
#' @param base 
#'
#' @return a vector of TD
#' @export
hill_taxa <- function(comm, q = 0, MARGIN = 1, base = exp(1)) { # adapted from hillR package
  
  # comm = as.matrix(data)
  # q = -1
  comm <- as.matrix(comm)
  if  (q == 0) {
    # richness
    if (length(dim(comm)) > 1) {
      hill <- apply(comm > 0, MARGIN, sum, na.rm = TRUE)
      # hill[which(hill == Inf)] <-0
    } else {
      hill <- sum(comm > 0, na.rm = TRUE)
      # hill[which(hill == Inf)] <-0
    }
  } else {
    if (q == 1) {
      # shannon
      comm <- -comm * log(comm, base)
      if (length(dim(comm)) > 1) {
        hill <- exp(apply(comm, MARGIN, sum, na.rm = TRUE))
        # hill[which(hill == Inf)] <-0
      } else {
        hill <- exp(sum(comm, na.rm = TRUE))
        # hill[which(hill == Inf)] <-0
      }
    } else {
      # q != 0,1, simpson, etc.
      comm <- comm**q  # p_i^q
      comm[which(comm == Inf)] <- 0
      if (length(dim(comm)) > 1) {
        hill <- (apply(comm, MARGIN, sum, na.rm = TRUE))^(1/(1 - q))
        # hill[which(hill == Inf)] <-0
      } else {
        hill <- (sum(comm, na.rm = TRUE))^(1/(1 - q))
        # hill[which(hill == Inf)] <-0
      }
    }
  }
  hill
} 

# PHYLO 
#' create_tree_chain
#' from cladistic information on several species, builds the tree chain
#' @param data a matrix containing cladistic inforamtion on the species
#'
#' @return a tree chain
#' @export
create_tree_chain <- function(data){
  genuses         <- c(lapply(unique(data$Genus), function(gen) paste0("(", paste(data$species[
    which(data$Genus %in% gen) ], collapse = ","), ")")))
  names(genuses)  <- unique(data$Genus)
  
  families        <- c(lapply(unique(data$Family), function(fam) paste0("(",paste(genuses[
    which(names(genuses) %in% data$Genus[which(data$Family %in% fam) ])], collapse = ","), ")")))
  names(families) <- unique(data$Family)
  
  orders          <- c(lapply(unique(data$Order), function(ord) paste0("(",paste(families[
    which(names(families) %in% data$Family[which(data$Order %in% ord)]) ], collapse = ","), ")")))
  names(orders)   <- unique(data$Order)
  
  classes         <- c(lapply(unique(data$Class), function(clas) paste0("(",paste(orders[
    which(names(orders) %in% data$Order[which(data$Class %in% clas)])], collapse = ","), ")")))
  names(classes)  <- unique(data$Class)
  
  phylums         <- c(lapply(unique(data$Phylum), function(phy) paste0("(",paste(classes[
    which(names(classes) %in% data$Class[which(data$Phylum %in% phy)])], collapse = ","), ")")))
  names(phylums)  <- unique(data$Phylum)
  
  kingdoms        <- paste(lapply(unique(data$Kingdom), function(phy) paste0("(",paste(phylums[
    which(names(phylums) %in% data$Phylum[which(data$Kingdom %in% phy)])], collapse = ","), ")")),
    collapse = ",")
} # eo create_tree_chain

#' dat_prep_phylo
#' intermediate function before computing hill numbers
#' sort and name the leaves and nodes of the tree
#' assign a legnth if there is no branch length in the tree
#' @param comm 
#' @param tree 
#'
#' @return
#' @export
dat_prep_phylo    <- function(comm, tree) { # from HillR package
  if (class(tree) == "phylo")
    tree    <- ape::write.tree(tree)
  phyloData <- ade4::newick2phylog(tree)
  comm      <- as.matrix(comm[, names(phyloData$leaves)])  # resort sp
  nodenames <- c(names(phyloData$leaves), names(phyloData$nodes))
  M         <- matrix(0, nrow = ncol(comm), ncol = length(nodenames), 
                      dimnames = list(names(phyloData$leaves), nodenames))
  for (i in 1:nrow(M)) {
    M[i, ][unlist(phyloData$paths[i])] <- 1
  } # eo for i
  phylo_comm  <- comm %*% M
  phyloLength <- c(phyloData$leaves, phyloData$nodes)
  treeH       <- sum(phyloLength * phylo_comm[1, ]/sum(comm[1, ]))
  
  return(list(pcomm = t(phylo_comm), pLength = phyloLength, treeH = treeH))
} # eo dat_prep_phylo

#' relab_hill_phylo
#' Computes the hill number of assemblages
#' @param comma matrix containing the relative abundance of the species in the assemblages
#' @param tree  phylo tree of all the species considered
#' @param q     value of q with which Hill numbers must be computed
#' @param base 
#' @param rel_then_pool 
#' @param show.warning 
#'
#' @return a vector of PD
#' @export
relab_hill_phylo  <- function(comm, tree, q = 0, base = exp(1), rel_then_pool = TRUE,
                              show.warning = TRUE) { # from HillR package
  if (any(comm < 0))
    stop("Negative value in comm data")
  # if(any(colSums(comm) == 0) & show.warning) warning('Some species in comm data were
  # not observed in any site,\n delete them...') comm = comm[, colSums(comm) != 0]
  
  comm_sp <- intersect(colnames(comm), tree$tip.label)
  
  if (class(tree) != "phylo")
    stop("tree must be an object with phylo as class")
  
  if (length(setdiff(tree$tip.label, comm_sp))) {
    if (show.warning)
      warning("Some species in the phylogeny but not in comm,
              \n remove them from the phylogeny...")
    tree <- ape::drop.tip(tree, tree$tip.label[!tree$tip.label %in% comm_sp])
  } # eo if 
  
  if (length(setdiff(colnames(comm), comm_sp))) {
    if (show.warning)
      warning("Some species in the comm but not in the phylogeny, \n remove them from the comm")
    comm <- comm[, comm_sp]
  } # eo if
  
  comm <- comm[, tree$tip.label]  # resort sp
  comm <- as.matrix(comm)
  
  # if (rel_then_pool) { # we already have abundance so just pool
  #   comm <- sweep(comm, 1, rowSums(comm, na.rm = TRUE), "/")  # relative abun
  # }
  
  dat     <- dat_prep_phylo(comm, tree)
  pabun   <- dat$pcomm
  plength <- dat$pLength
  
  N         <- ncol(pabun)
  PD        <- numeric(N)
  names(PD) <- row.names(comm)
  qPD       <- PD
  if (q == 1) {
    for (i in 1:N) {
      TT     <- sum(pabun[, i] * plength)
      I      <- which(pabun[, i] > 0)
      PD[i]  <- exp(-sum(plength[I] * (pabun[, i][I]/TT) * log(pabun[, i][I]/TT, base)))
      qPD[i] <- PD[i]/TT
    } # eo for i
  } else {
    for (i in 1:N) {
      TT     <- sum(pabun[, i] * plength)
      I      <- which(pabun[, i] > 0)
      PD[i]  <- sum(plength[I] * (pabun[, i][I]/TT)^q)^(1/(1 - q))
      qPD[i] <- PD[i]/TT
    } # eo for i
  } # eo else
  qPD
} # eo relab_hill_phylo

#' FD_MLE
#' R code for obtaining functional diversity (FD) based on Chao et al. (2018) 
#' Please cite Chao et al. (2018) An attribute-diversity approach to functional diversity,
#' functional beta diversity, and related (dis)similarity measures. 
#' Functional Diversity of a single site for specified values of tau and q
#' FD_MLE (data, dij, tau, q) is a function of obtaining FD index of order q.
#' @param data a vector of species sample frequencies.
#' @param dij a matrix of species-pairwise distances.
#' @param tau a numeric for a specified level of threshold distinctiveness.
#' @param q a numeric for a specified diversity order q. 
#'
#' @return a numeric value of FD.
#' @export
FD_MLE   <- function(data, dij, tau, q){
  dij                             <- as.matrix(dij)
  dij[which(dij>tau,arr.ind = T)] <- tau
  
  a     <- as.vector((1 - dij/tau) %*% data )  
  data  <- data[a!=0]
  a     <- a[a!=0]
  v     <- data/a
  nplus <- sum(data)
  if(q==1){
    FD  <- exp(sum(-v*a/nplus*log(a/nplus)))
    qFD <- (FD/nplus)^(1/2)
  }else{
    FD  <- (sum(v*((a/nplus)^q)))^(1 / (1-q))
    qFD <- (FD/nplus)^(1/2)
  } # eo else
} # eo FD_MLE

#' find_bestq
#' This function computes the hill numbers for TD/PD/FD with q_nb different q between q_min and
#' q_max it selects the q for wich the r² of lm(esth_score~hill_number) is maximized and finally
#' compute the hill numbers for this q
#' @param div  the diversity worked with : c(TD,PD,FD)
##              if div = TD, hillfun = hill_taxa
##              if div = PD, hillfun = relab_hill_phylo
##              if div = FD, hillfun = FD_MLE
#' @param data matrix of relative abundance. Quadrat_code must be rownames and Species_name must be
#'              colnames for div = PD, colnames = id_name of the species, for div = TD or FD 
#'              colnames(data) = id_code of the species
#' @param tree  phylotree generated by ape::read_tree. Default is NULL. Only needed if div = PD
#' @param dij distance matrix generated by ade4::dist.kab (Pavoine et al. 2009). Default is NULL. 
##              Only needed if div = FD
#' @param tau threshold for dij (Chao et al. 2019). Default is NULL, recommended value is
#'              mean(dij). Only needed if div = FD.
#' @param esth a two columns matrix of the aesthetic scores. rownames(esth) must be the same than
#'               rownames(data). First column is quadrat_code and second one is esth_score.
#' @param q_min min value for q
#' @param q_max max value for q
#' @param q_nb number of values to test for q in range q_min;q_max
#' @param ncores number of cores to use
#' @param log_trans logical. Default is FALSE. if TRUE the tested relation is 
#'                  lm(log(esth)~log(hill_index)) 
#'                  esth must be already log transformed when entered in the function if 
#'                  log_trans == TRUE
#'
#' @return hill_number_q -> vector of the hill number with q maximizing the R² for all assemblages
#'         bestq         -> the q maximizing the R² of lm(esth_score~hill_number)
#'         bestr2        -> max value of R²
#'         all_r2_q      -> all q value tested and the associated R²
#' @export

find_bestq <- function(div, data, tree = NULL, dij = NULL, tau = NULL, esth, q_min, q_max, q_nb,
                       ncores, log_trans = FALSE){
  # div = "PD"; data = relab; tree = tree; esth = esth; q_min = -1; q_max = 1; q_nb = 10;
  # ncores = 4; log_trans = log_trans
  colnames(esth) <- c("quadrat_code", "esth_score")
  Q              <- seq(q_min, q_max, length.out = q_nb)
  R2             <- do.call(rbind, parallel::mclapply(X = Q, FUN = function(q){
    if (div == "TD"){
      Hill_number <- hill_taxa(data, q, MARGIN = 1)
    } # eo if td
    if (div == "PD"){
      Hill_number <- relab_hill_phylo(data, tree, q)
    } # eo if pd
    if (div == "FD"){
      Hill_number <- apply(relab, 1, function(assemblage){ 
        FD_MLE(data = assemblage, dij = dis_traits_cora, tau = tau, q = q)
      })
    } # eo if fd
    
    Hill_number <- Hill_number[order(names(Hill_number))]
    
    if (log_trans == TRUE){ Hill_number <- log(Hill_number)}
    esth        <- esth[order(esth$quadrat_code),]
    r2          <- summary(lm(esth$esth_score ~ Hill_number))$r.squared
    cbind(q, r2)
  }, mc.cores = ncores)) # eo R2
  
  R2             <- as.data.frame(R2)
  bestq          <- R2[which(R2$r2 == max(R2$r2)),]
  if (div == "TD"){
    Hill_number_q  <- hill_taxa(data, bestq$q)
  } #eo if TD
  if (div == "PD"){
    Hill_number_q  <- relab_hill_phylo(data, tree, bestq$q)
  } #eo if PD
  if (div == "FD"){
    Hill_number_q  <- apply(relab, 1, function(assemblage){
      FD_MLE(data = assemblage, dij = dis_traits_cora, tau = tau, q = bestq$q )})
  } #eo if FD
  
  if(log_trans == TRUE){ Hill_number_q <- log(Hill_number_q)}
  
  list(hill_number_q = Hill_number_q, bestq = bestq$q, bestr2 = bestq$r2, all_r2_q = R2)
} # eo find_bestq
