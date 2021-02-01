###################################################################################################
#'  Appendix S5: Hill numbers.
#'
#' This script produces the Langlois et al.'s 2021 paper Appendix S5:
#' 
#' Figure S1, i.e. a panel of three scatterplots representing the variation of the R2 of the linear
#' relation between the aesthetic values and, respectively qTD (left), qFD (middle), qPD (right), 
#' according to the value of the Hill q coefficient.
#' 
#' Figure S2, i.e. a panel of two scatterplots representing the density of 
#' the standardized qFD (left) and the standardized qPD (right).
#'
#' @author Juliette Langlois, \email{juliette.a.langlois@@gmail.com},
#'         Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#'         François Guilhaumon, \email{francois.guilhaumon@@ird.fr}
#'
#' @date 2021/01/12
##################################################################################################

# Figure S1 ------------------------------------------------------------------------------------------------------

# Load data ----
data_all <- read.csv(hh("data", "quadrat_table.csv"))
phylo    <- read.csv(hh("data", "phylo_table.csv"))
load(file = hh("data", "trait_table.RData"))

esth_all <- data_all[,c("quadrat_code", "esth_score")]

relab           <- data_all
rownames(relab) <- relab$quadrat_code
relab           <- relab[,grep(pattern = "sp_", x = colnames(relab))]

# remove assemblages for which no species is identified
S        <- apply(relab, 1, sum)
if(length(which(S == 0)) != 0){ relab <- relab[- which(rownames(relab) %in% names(which(S==0))),]}

# Same quadrats
ifelse(length(setdiff(as.character(esth_all$quadrat_code), rownames(relab))) != 0,
       esth <- esth_all[-which(esth_all$quadrat_code %in% setdiff(esth_all$quadrat_code, rownames(relab))),],
       esth <- esth_all) # eo ifelse
if(length(setdiff(rownames(relab), esth_all$quadrat_code)) != 0){
  relab <- relab[-which(rownames(relab) %in% setdiff(rownames(relab), as.character(esth_all$quadrat_code))),]
} # eo if

esth           <- esth[order(esth$quadrat_code),]
rownames(esth) <- esth$quadrat_code
relab          <- relab[order(rownames(relab)),]

log_trans <- FALSE
nproc     <- 20

rm(data_all, esth_all, S)
# ----

# Panel a ----
# Compute the taxonomic Hill numbers
start_time <- Sys.time()
res        <- find_bestq(div = "TD", data = relab, esth = esth, q_min = -1, q_max = 1, q_nb = 1000, ncores = nproc, log_trans = log_trans)
end_time   <- Sys.time()
end_time - start_time

# Extract the best q, the best Rsquared and TD_q for each assemblage

bestq_TD    <- res$bestq
Rsquared_TD <- res$bestr2
q_R2_TD     <- res$all_r2_q
qTD         <- res$hill_number_q

# Plot of the evolution of R2 with q

qtd_plot <- ggplot2::ggplot(q_R2_TD, ggplot2::aes(x=q, y=r2)) + 
  ggplot2::geom_point(size = 1, color =  viridis::viridis(n = 1)) +
  ggplot2::ylab("R-squared of lm(log(E)~TDq)") + 
  ggplot2::theme_light() + 
  ggplot2::theme(text = ggplot2::element_text(size = 12),
        axis.title = ggplot2::element_text(size = 12),
        axis.text = ggplot2::element_text(size = 12))

qTD              <- as.data.frame(qTD)
qTD$quadrat_code <- rownames(qTD)
qTD              <- qTD[,c("quadrat_code", "qTD")]

rm(bestq_TD, Rsquared_TD, q_R2_TD, res, end_time, start_time)
# ----

# Panel b ----

# Built the distance matrix
# normalize 
trait_norm <- data.frame(id_name = trait_all[,1], rbind(apply(trait_all[,-c(1:2)], 2, function(col){
  col <- as.numeric(as.character(col))
  normalize(col)
})))

# create objects for dist.ktab function
single <- c("colonial", "gregarious", "unit_height","base_cover", "base_type", "consistency", "condition_of_food",
            "size_of_food", "engineering","coralligenous_builder", "thermal_preference",
            "sedimentation", "organic_pollution", "salinity", "hydrodynamism", "light", "nb_interactions", "nb_substrate", "nb_def_strat")

for (i in 1:length(single)){
  objname <- single[i]
  j       <- which(colnames(trait_all) == single[i])
  assign(objname, as.data.frame(trait_all[,j]))
} # eo for i

row.names(colonial)              <- row.names(trait_all)
row.names(gregarious)            <- row.names(trait_all)
row.names(unit_height)           <- row.names(trait_all)
row.names(base_cover)            <- row.names(trait_all)
row.names(base_type)             <- row.names(trait_all)
row.names(consistency)           <- row.names(trait_all)
row.names(condition_of_food)     <- row.names(trait_all)
row.names(size_of_food)          <- row.names(trait_all)
row.names(engineering)           <- row.names(trait_all)
row.names(coralligenous_builder) <- row.names(trait_all)
row.names(thermal_preference)    <- row.names(trait_all)
row.names(sedimentation)         <- row.names(trait_all)
row.names(organic_pollution)     <- row.names(trait_all)
row.names(salinity)              <- row.names(trait_all)
row.names(hydrodynamism)         <- row.names(trait_all)
row.names(light)                 <- row.names(trait_all)
row.names(nb_interactions)       <- row.names(trait_all)
row.names(nb_substrate)          <- row.names(nb_substrate)
row.names(nb_def_strat)          <- row.names(nb_def_strat)

# Compute the distance matrix
dis_traits_cora <- as.matrix(ade4::dist.ktab(x = ade4::ktab.list.df(list(
  condition_of_food,
  gregarious, base_type, consistency, size_of_food, engineering, thermal_preference, sedimentation, organic_pollution, hydrodynamism, light,
  unit_height,base_cover, nb_interactions, nb_def_strat, nb_substrate,
  coralligenous_builder, colonial, salinity)),
  type = c("N",                                     # N for nominal
           "O","O","O","O","O","O","O","O","O","O", # O for ordered
           "Q", "Q", "Q", "Q", "Q",                 # Q for quantitative
           "D", "D", "D"),                          # D for dichotomous
  scan = FALSE))

# Save distance matrix
save(dis_traits_cora, file = hh("output", "05_funct_dist_matrix.RData"))

rm(base_cover, base_type, colonial, condition_of_food, consistency, coralligenous_builder, engineering, gregarious,
   hydrodynamism, light, nb_def_strat, nb_interactions, nb_substrate, organic_pollution, salinity, sedimentation,
   size_of_food, thermal_preference, unit_height,
   i,j,objname, single,
   trait_all, trait_norm)

# Compute tau parameter
# according to Chao et al 2019 its better to take tau = mean when comparing numerous assemblages
tau <- mean(dis_traits_cora[which(upper.tri(dis_traits_cora) == TRUE)])

# Compute the functional Hill numbers for 1000 q between -1 and 1
start_time <- Sys.time()
res        <- find_bestq(div = "FD", data = relab, dij = dis_traits_cora, tau = tau, esth = esth, 
                         q_min = -1, q_max = 1, q_nb = 1000, ncores = nproc, log_trans = log_trans)
end_time   <- Sys.time()
end_time - start_time

# Extract the best q, the best Rsquared and FD_q for each assemblage
bestq_FD    <- res$bestq
Rsquared_FD <- res$bestr2
q_R2_FD     <- res$all_r2_q
qFD         <- as.data.frame(res$hill_number_q)
colnames(qFD) <- "qFD"
qFD$quadrat_code <- rownames(qFD)


# plot of the evolution of R2 with q
qfd_plot <- ggplot2::ggplot(q_R2_FD, ggplot2::aes(x = q, y = r2)) +
  ggplot2::geom_point(size = 1, color = viridis::viridis(n = 1)) +
  ggplot2::ylab("R-squared of lm(log(E)~qFD)") + 
  ggplot2::theme_light() + 
  ggplot2::theme(text = ggplot2::element_text(size = 12),
        axis.title = ggplot2::element_text(size = 12),
        axis.text = ggplot2::element_text(size = 12))

rm(tau, start_time, res, end_time, Rsquared_FD, q_R2_FD, dis_traits_cora)

# ----

# Panel c ----
# Species must be identified with their names not code in the table of relative abundance
data <- relab
for (i in 1:ncol(data)){colnames(data)[i] <- as.character(phylo[which(phylo$ID_code == colnames(data)[i]),"species"])}
colnames(data) <- gsub(pattern = "-", replacement = "_", x = colnames(data)) #just in case

# Create the tree 
tree_chain        <- create_tree_chain(phylo)
tree              <- ape::read.tree(text = paste0(tree_chain,";"), edge.width = 2)
# Save tree for future use
save(tree, file = hh("output", "05_phylo_tree.RData"))

# Compute the phylogenetic Hill numbers for 1000 q between -1 and 1
start_time <- Sys.time()
res        <- find_bestq(div = "PD", data = data, tree = tree, esth = esth, q_min = -1, q_max = 1, q_nb = 1000, 
                         ncores = nproc, log_trans = log_trans)
end_time   <- Sys.time()
end_time - start_time

# Extract the best q, the best Rsquared and PD_q for each assemblage
bestq_PD    <- res$bestq
Rsquared_PD <- res$bestr2
q_R2_PD     <- res$all_r2_q
qPD         <- as.data.frame(res$hill_number_q)
colnames(qPD) <- "qPD"
qPD$quadrat_code <- rownames(qPD)

# Plot of the evolution of R2 with q
qpd_plot <- ggplot2::ggplot(q_R2_PD, ggplot2::aes(x=q, y=r2)) + 
  ggplot2::geom_point(size = 1, color =  viridis::viridis(n = 1)) +
  ggplot2::ylab("R-squared of lm(log(E)~PDq)") + 
  ggplot2::theme_light() + 
  ggplot2::theme(text = ggplot2::element_text(size = 12),
        axis.title = ggplot2::element_text(size = 12),
        axis.text = ggplot2::element_text(size = 12))

rm(phylo, i, tree_chain, tree, start_time, res, end_time, Rsquared_PD, q_R2_PD, log_trans, data)
# ----

# Combine the three panels ----

graphs <- ggpubr::ggarrange(qtd_plot, qfd_plot, qpd_plot, ncol = 3, nrow = 1, widths = c(1,1,1), align = "v")
ggplot2::ggsave(filename = hh("output", "05_AppendixS5_FigureS1.pdf"), plot = graphs, width = 18, height = 8, units = "cm", dpi = 320, family = "sans")

# ----

# Save Hill number values for future use ----

hillnb <- merge(qTD, qFD, by = "quadrat_code")
hillnb <- merge(hillnb, qPD, by = "quadrat_code")

save(hillnb, file = hh("output", "05_hillnb.RData"))
rm(esth, graphs, qFD, qPD, qTD, qfd_plot, qpd_plot, qtd_plot)
# ----

# Figure S2 ------------------------------------------------------------------------------------------------------

# Load data ----

data_all <- read.csv(hh("data", "quadrat_table.csv"))
phylo    <- read.csv(hh("data", "phylo_table.csv"))
abund    <- read.csv(file = hh("data","abundance_table.csv"))

load(file = hh("data", "trait_table.RData"))

load(file = hh("output","05_funct_dist_matrix.RData"))
load(file = hh("output", "05_phylo_tree.RData"))
load(file = hh("output", "05_hillnb.RData"))

# set quadrat codes as rownames
relab           <- data_all
rownames(relab) <- relab$quadrat_code
relab           <- relab[,grep(pattern = "sp_", x = colnames(relab))]
rownames(abund) <- abund$quadrat_code
abund           <- abund[,-which(colnames(abund)=="quadrat_code")]

# remove assemblages for which no species is identified
S        <- apply(relab, 1, sum)
if(length(which(S == 0)) != 0){ relab <- relab[- which(rownames(relab) %in% names(which(S==0))),]}
S        <- apply(abund, 1, sum)
if(length(which(S == 0)) != 0){ abund <- abund[- which(rownames(abund) %in% names(which(S==0))),]}

log_trans <- FALSE

rm(S, data_all)

# ----

# tau parameter according to Chao et al 2019 ----
tau <- mean(dis_traits_cora[which(upper.tri(dis_traits_cora) == TRUE)])
# ----

# for PD need to use the id_name of the species, not the id_code ----
abund_names <- abund
for (i in 1:ncol(abund_names)){
  colnames(abund_names)[i] <- as.character(phylo[which(as.character(phylo$ID_code) == colnames(abund_names)[i]),"species"])}
colnames(abund_names) <- gsub(pattern = "-", replacement = "_", x = colnames(abund_names))
rm(i)
# ----

# Compute SES ----
# FD
qFD <- hillnb[,c("quadrat_code", "qFD")]

start_time <- Sys.time()
qFD_all    <- standefsize(div = "FD", data = abund, dij = dis_traits_cora, tau = tau, q = bestq_FD, 
                          hilldiv = qFD, log_trans = log_trans, nb = 1000, mcores = nproc) 

end_time   <- Sys.time()
end_time - start_time

colnames(qFD_all)    <- c("qFD", "mean_nm", "sd_nm", "SES_qFD")
qFD_all$quadrat_code <- rownames(qFD_all)
qFD_ses              <- qFD_all[,c("quadrat_code", "SES_qFD")]

# PD
qPD <- hillnb[,c("quadrat_code", "qPD")]

start_time <- Sys.time()
qPD_all    <- standefsize(div = "PD", data = abund_names, tree = tree, q = bestq_PD, 
                          hilldiv = qPD, log_trans = log_trans, nb = 1000, mcores = nproc) 
end_time   <- Sys.time()
end_time - start_time

colnames(qPD_all)    <- c("qPD", "mean_nm", "sd_nm", "SES_qPD")
qPD_all$quadrat_code <- rownames(qPD_all)
qPD_ses              <- qPD_all[,c("quadrat_code", "SES_qPD")]

# ----

# Plot distrib ----
# colors
col   <- viridis::viridis(n = 3, alpha = 0.6)
# mean
mufd  <- mean(qFD_all$SES_qFD)
mupd  <- mean(qPD_all$SES_qPD)
# skewness
skewFD <- PerformanceAnalytics::skewness(qFD_all$SES_qFD)
skewPD <- PerformanceAnalytics::skewness(qPD_all$SES_qPD)

par(mar = c(0,0,0,0))

densesfd <- ggplot2::ggplot(qFD_all, ggplot2::aes(x = SES_qFD)) + 
  ggplot2::geom_density(alpha = 0.5, col = col[2], fill = col[2]) + 
  ggplot2::theme_light() + 
  ggplot2::labs(x = bquote(~qFD[SES]), y = "Density") + 
  ggplot2::theme(text = ggplot2::element_text(size = 12),
        axis.title = ggplot2::element_text(size = 12),
        axis.text = ggplot2::element_text(size = 8), 
        panel.grid = ggplot2::element_blank(),
        legend.position = "none") + 
  ggplot2::geom_vline(xintercept = mufd, col = "red",
             linetype = "dotdash")

densespd <- ggplot2::ggplot(qPD_all, ggplot2::aes(x = SES_qPD)) + 
  ggplot2::geom_density(alpha = 0.5, col = col[1], fill = col[1]) + 
  ggplot2::theme_light() + 
  ggplot2::labs(x = bquote(~qPD[SES]), y = "Density") + 
  ggplot2::theme(text = ggplot2::element_text(size = 12),
        axis.title = ggplot2::element_text(size = 12),
        axis.text = ggplot2::element_text(size = 8), 
        panel.grid = ggplot2::element_blank(),
        legend.position = "none") + 
  ggplot2::geom_vline(xintercept = mupd, col = "red",
             linetype = "dotdash")

graphs <- ggpubr::ggarrange(densesfd, densespd, ncol = 2, nrow = 1, widths = c(1,1), align = "v")
ggplot2::ggsave(filename = hh("output", "05_AppendixS5_FigureS2.pdf"), plot = graphs, width = 18, height = 8, units = "cm", dpi = 320, family = "sans")

# ----

# Save SES hill numbers for future use ----

hillnb <- merge(hillnb, qFD_ses, by = "quadrat_code")
hillnb <- merge(hillnb, qPD_ses, by = "quadrat_code")
save(hillnb, file = hh("output", "05_hillnb_ses.RData"))

file.remove(hh("output", "05_hillnb.RData"), hh("output", "05_funct_dist_matrix.RData"), hh("output", "05_phylo_tree.RData"))

rm(list=ls(all=TRUE))

# ----

