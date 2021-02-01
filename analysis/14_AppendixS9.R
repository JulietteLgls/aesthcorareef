###################################################################################################
#' Appendix S9: Relationship between the aesthetic values and the depth of all stations and between 
#' the aesthetic values and the longitude of the stations at the east of Marseille.
#' 
#' This script produces the Langlois et al.'s 2021 paper Appendix S9 Figure S1, i.e.
#' a two panels scatterplot of the relationship between the aesthetic values and the depth of all 
#' stations (left) and the relationship between the aesthetic values and the longitude of the 
#' stations at the east of Marseille (right).
#' 
#' @author Juliette Langlois, \email{juliette.a.langlois@@gmail.com},
#'         Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#'         François Guilhaumon, \email{francois.guilhaumon@@ird.fr}
#'
#' @date 2021/01/12
##################################################################################################

# Load data ----

load(hh("output", "12_table_station.RData"))

col <- viridis::viridis(4)

# ----

# esth ~depth on stations ----
poly_depth <- ggplot2::ggplot(table_station, ggplot2::aes(x = depth, y = esth_score)) +
  ggplot2::geom_point(size = 3, shape = 20, col = col[2]) + 
  ggplot2::geom_smooth(method = lm, data = table_station, formula = y ~ poly(x = x, degree = 2), col = col[4], se = FALSE) +
  ggplot2::theme_light() + 
  ggplot2::theme(text = ggplot2::element_text(size = 12),
        axis.title = ggplot2::element_text(size = 12),
        axis.text = ggplot2::element_text(size = 8), 
        panel.grid = ggplot2::element_blank(),
        legend.position = "none") +
  ggplot2::xlab("Depth (m)") +
  ggplot2::ylab("Aesthetic value of the stations")

mod <- lm(formula = esth_score ~ poly(x = depth, degree = 2), data = table_station)

# ----

# Longitude of Marseille = 5.4 ----

table_cont <- table_station[which(table_station$region != "Corse"),]
marseast   <- table_cont[which(table_cont$longitude >= 5.4),]
longmars   <-
  ggplot2::ggplot(table_cont, ggplot2::aes(x = longitude, y = esth_score)) +
  ggplot2::geom_point(size = 3, shape = 20, col = col[2]) + 
  ggplot2::geom_smooth(method = lm, se = FALSE, col = col[4], data = marseast) +
  ggplot2::geom_vline(xintercept = 5.4, col = "red", lty = "dashed") +
  ggplot2::annotate(geom = "text", label = "Marseille", x = 4.9, y = 7.45, col = "red") +
  ggplot2::theme_light() + 
  ggplot2::theme(text = ggplot2::element_text(size = 12),
        axis.title = ggplot2::element_text(size = 12),
        axis.text = ggplot2::element_text(size = 8), 
        panel.grid = ggplot2::element_blank(),
        legend.position = "none") +
  ggplot2::xlab("Longitude") +
  ggplot2::ylab("Aesthetic value of the stations")

modmars <- lm(marseast$esth_score ~ marseast$longitude)

# Save ----

plots <- ggpubr::ggarrange(poly_depth, longmars, ncol = 2)
ggplot2::ggsave(plots, filename = hh("output", "14_AppendixS9_FigureS1.pdf"), width = 20, height = 10, units = "cm")

rm(list=ls(all=TRUE))

# ----
