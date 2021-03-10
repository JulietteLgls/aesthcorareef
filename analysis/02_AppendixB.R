###################################################################################################
#' Appendix B: Functional traits.
#'
#' This script produces the Langlois et al.'s 2021 paper Appendix B Figure B.1,
#' i.e. a two panels plot representing the contribution of the 32 considered traits to the two
#' first axes of the PCA and the coordinates of the species in the plane formed by the two first
#' axes of the PCA.
#' 
#' Table S1: see data/trait_table.RData
#' 
#' @author Juliette Langlois, \email{juliette.a.langlois@@gmail.com},
#'         Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#'         François Guilhaumon, \email{francois.guilhaumon@@ird.fr}
#'
#' @date 2021/01/12
###################################################################################################

# Load data ----

load(hh("data", "trait_table.RData"))

# ----

# Prepare data ----

data           <- trait_all
data           <- trait_all[,-which(colnames(data) %in% c("X", "id_name"))]

# Change factors into numerics
data$colonial              <- as.numeric(levels(data$colonial))[data$colonial]
data$gregarious            <- as.numeric(levels(data$gregarious))[data$gregarious]
data$consistency           <- as.numeric(levels(data$consistency))[data$consistency]
data$condition_of_food     <- as.numeric(levels(data$condition_of_food))[data$condition_of_food]
data$size_of_food          <- as.numeric(levels(data$size_of_food))[data$size_of_food]
data$engineering           <- as.numeric(levels(data$engineering))[data$engineering]
data$coralligenous_builder <- as.numeric(levels(data$coralligenous_builder))[
                              data$coralligenous_builder]
data$thermal_preference    <- as.numeric(levels(data$thermal_preference))[data$thermal_preference]
data$sedimentation         <- as.numeric(levels(data$sedimentation))[data$sedimentation]
data$organic_pollution     <- as.numeric(levels(data$organic_pollution))[data$organic_pollution]
data$salinity              <- as.numeric(levels(data$salinity))[data$salinity]
data$hydrodynamism         <- as.numeric(levels(data$hydrodynamism))[data$hydrodynamism]
data$light                 <- as.numeric(levels(data$light))[data$light]
data$base_type             <- as.numeric(levels(data$base_type))[data$base_type]
# normalize
data_norm <- rbind(apply(data, 2, normalize))

# ----

# PCA ----

res.pca  <- prcomp (data_norm, scale = FALSE)

# eigen values
eig.val <- factoextra::get_eigenvalue(res.pca)
# coordinates
var          <- factoextra::get_pca_var(res.pca)
contrib_axes <- var$coord[,1:4]
contrib_axes <- cbind(rownames(contrib_axes), contrib_axes)

# ----

# Plots ----

# Coordinates of individuals
ind_plot <- factoextra::fviz_pca_ind(res.pca, repel = FALSE) +
  ggplot2::theme_light() +
  ggplot2::coord_fixed(ratio = 1)+
  ggplot2::theme(text = ggplot2::element_text(size = 15),
        axis.title = ggplot2::element_text(size = 15),
        axis.text = ggplot2::element_text(size = 12))

# Variable contribution
var_plot <- factoextra::fviz_pca_var(res.pca, col.var = "contrib", geom = c("arrow", "text"),
                                     labelsize = 5, 
                                     gradient.cols = viridis::viridis(
                                       35, end = 0.8, direction = 1),
                                     repel = TRUE) +
  ggplot2::coord_fixed(ratio = 1) +
  ggplot2::theme(text = ggplot2::element_text(size = 15),
        axis.title = ggplot2::element_text(size = 15),
        axis.text = ggplot2::element_text(size = 12))

# Extract the legend from varplot
var_plot <- ggpubr::ggpar(var_plot, legend.title = "Contribution", legend = "right",
                          ggtheme = ggplot2::theme_light(), font.family = "sans") +
  ggpubr::font("legend.title", size = 12) + 
  ggpubr::font("legend.text", size = 10)

# extract legend
leg <- cowplot::get_legend(var_plot)
var_plot <- var_plot + ggplot2::theme(legend.position = "none", 
                                      text = ggplot2::element_text(size = 15),
                                      axis.title = ggplot2::element_text(size = 15),
                                      axis.text = ggplot2::element_text(size = 12))
# Save plots
pp    <- ggpubr::ggarrange(var_plot, ind_plot, ncol = 2, heights = c(1,1))
ppleg <- ggpubr::ggarrange(leg, pp, ncol = 2, widths = c(.3,1)) 
ggplot2::ggsave(hh("output", "02_AppendixB_FigureB1.pdf"), plot = ppleg, width = 40, height = 20,
                units = "cm", dpi = 320, family = "sans")

# ----

