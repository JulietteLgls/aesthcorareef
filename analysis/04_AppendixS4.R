###################################################################################################
#' Appendix S4: PCA of the anthropogenic pressures.
#'
#' This script produces the Langlois et al.'s 2021 paper Appendix S4 Figure S1,
#' i.e. a one panel plot representation of the contribution of the 13 anthropogenic pressures
#' on the plane of the first and second axes of the PCA.
#'
#' @author Juliette Langlois, \email{juliette.a.langlois@@gmail.com},
#'         Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#'         François Guilhaumon, \email{francois.guilhaumon@@ird.fr}
#'
#' @date 2021/01/12
##################################################################################################

# Prepare data ----

table_all <- read.csv(hh("data", "quadrat_table.csv"))

pressures           <- table_all
rownames(pressures) <- pressures[, "quadrat_code"]
pressures           <- pressures[,-1]
pressures           <- pressures[,grep(pattern = "pressure_", x = colnames(pressures))]

# ----

# PCA of all pressures ---- 

res.pca <- prcomp (pressures, scale = FALSE)

# eigen values
eig.val <- factoextra::get_eigenvalue(res.pca)
factoextra::fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

# coordinates
var <- factoextra::get_pca_var(res.pca)
coord_var   <- var$coord
contrib_var <- var$contrib
# ----

# Plot ----
var_plot <- factoextra::fviz_pca_var(res.pca, col.var = "contrib", geom = c("arrow", "text"),
                                     labelsize = 5, gradient.cols = viridis::viridis(15, end = 0.8, direction = -1),
                                     repel = TRUE) +
  ggplot2::theme(text = ggplot2::element_text(size = 15), axis.title = ggplot2::element_text(size = 15),
        axis.text = ggplot2::element_text(size = 15))

var_plot <- ggpubr::ggpar(var_plot, title = "", legend.title = "Contribution", legend = c(0.8, 0.305),
                          ggtheme = ggplot2::theme_light()) + 
  ggpubr::font("legend.title", size = 18) + 
  ggpubr::font("legend.text", size = 13)

# Save plot
ggplot2::ggsave(filename = hh("output", "04_AppendixS4_FigureS1.pdf"), plot = var_plot, device = "pdf",
       width = 18, height = 18, units = "cm", family = "sans")

# ----

# Save results of PCA for future use ----

# extract the coordinates of the individuals for the two first axis
ind          <- factoextra::get_pca_ind(res.pca)
coord_ind    <- ind$coord

acp_2ax      <- as.data.frame(ind$coord[,1:2])
quadrat_code <- rownames(acp_2ax)
acp_2ax      <- cbind.data.frame(quadrat_code = quadrat_code, threat_1 = acp_2ax[, 1], threat_2 = acp_2ax[, 2])

# save file
colnames(acp_2ax) <- c("quadrat_code", "Exploitation", "Anthropization")
write.csv(acp_2ax, hh("output","04_results_PCApressures.csv"), row.names = FALSE)

rm(list=ls(all=TRUE))

# ----
