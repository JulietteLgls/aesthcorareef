###################################################################################################
#' Appendix H: Correlation between the aesthetic values of the quadrats and the averaged 
#' station value.
#' 
#' This script produces the Langlois et al.'s 2021 paper Appendix H Figure H.1, i.e. a four panels
#' scatterplot of the relation between the values of the aesthetic value (up left corner), 
#' qTD (up right corner), qFDSES (bottom left corner) and qPDSES (bottom right corner) for the 
#' quadrats and the stations.
#' 
#' @author Juliette Langlois, \email{juliette.a.langlois@@gmail.com},
#'         Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#'         François Guilhaumon, \email{francois.guilhaumon@@ird.fr}
#'
#' @date 2021/01/12
###################################################################################################

# Load data ----

load(file = file.path("output", "10_quadrat_pressure_hillses_ranks.RData"))
load(file = file.path("output", "12_table_station.RData"))

# ----

# Combine station and quadrat values at quadrat level ----
quadrats           <- table[, c("quadrat_code", "station", "esth_score", "qTD", "SES_qPD",
                                "SES_qFD")]
colnames(quadrats) <- c("quadrat_code", "station", "esth_score_quadrat", 
                        "qTD_quadrat", "SES_qPD_quadrat", "SES_qFD_quadrat")

stations           <- table_station[, c("station", "esth_score", "qTD", "SES_qPD", "SES_qFD")]
colnames(stations) <- c("station", "esth_score_station",
                        "qTD_station", "SES_qPD_station", "SES_qFD_station")

qsta_table <- merge(quadrats, stations, by = "station")

# ----

# Plots ----

col <- viridis::viridis(4, begin = 0.9, end = 0.1)

# Aesthetic
esth_qs <- ggplot2::ggplot(qsta_table, ggplot2::aes(x = esth_score_quadrat,
                                                    y = esth_score_station)) +
  ggplot2::geom_point(size = 0.8, shape = 20, col = col[1]) + 
  ggplot2::theme_light() + 
  ggplot2::theme(text = ggplot2::element_text(size = 12),
        axis.title = ggplot2::element_text(size = 12),
        axis.text = ggplot2::element_text(size = 8), 
        panel.grid = ggplot2::element_blank(),
        legend.position = "none") +
  ggplot2::scale_x_continuous(breaks = c(7.1, 7.2, 7.3, 7.4)) +
  ggplot2::scale_y_continuous(breaks = c(7.1, 7.2, 7.3, 7.4)) +
  ggplot2::xlab("Aesthetic value of the quadrats") +
  ggplot2::ylab("Aesthetic value of the stations")

modlog <- lm(qsta_table$esth_score_station ~ qsta_table$esth_score_quadrat)

# qTD
qtd_qs <- ggplot2::ggplot(qsta_table, ggplot2::aes(x = qTD_quadrat, y = qTD_station)) +
  ggplot2::geom_point(size = 0.8, shape = 20, col = col[2]) + 
  ggplot2::theme_light() + 
  ggplot2::theme(text = ggplot2::element_text(size = 12),
        axis.title = ggplot2::element_text(size = 12),
        axis.text = ggplot2::element_text(size = 8), 
        panel.grid = ggplot2::element_blank(),
        legend.position = "none") +
  ggplot2::scale_x_continuous(breaks = c(10, 20, 30, 40)) +
  ggplot2::scale_y_continuous(breaks = c(10, 20, 30, 40)) +
  ggplot2::xlab("qTD of the quadrats") +
  ggplot2::ylab("qTD of the stations")

modqtd <- lm(qsta_table$qTD_station ~ qsta_table$qTD_quadrat)

# qFD_SES
qfd_qs <- ggplot2::ggplot(qsta_table, ggplot2::aes(x = SES_qFD_quadrat, y = SES_qFD_station)) +
  ggplot2::geom_point(size = 0.8, shape = 20, col = col[3]) + 
  ggplot2::theme_light() + 
  ggplot2::theme(text = ggplot2::element_text(size = 12),
        axis.title = ggplot2::element_text(size = 12),
        axis.text = ggplot2::element_text(size = 8), 
        panel.grid = ggplot2::element_blank(),
        legend.position = "none") +
  ggplot2::labs(x = bquote(~qFD[SES]~'of the quadrats'), y = bquote(~qFD[SES]~'of the stations'))

modqfd <- lm(qsta_table$SES_qFD_station ~ qsta_table$SES_qFD_quadrat)

# qPD_SES
qpd_qs <- ggplot2::ggplot(qsta_table, ggplot2::aes(x = SES_qPD_quadrat, y = SES_qPD_station)) +
  ggplot2::geom_point(size = 0.8, shape = 20, col = col[4]) + 
  ggplot2::theme_light() + 
  ggplot2::theme(text = ggplot2::element_text(size = 12),
        axis.title = ggplot2::element_text(size = 12),
        axis.text = ggplot2::element_text(size = 8), 
        panel.grid = ggplot2::element_blank(),
        legend.position = "none") +
  ggplot2::labs(x = bquote(~qPD[SES]~'of the quadrats'), y = bquote(~qPD[SES]~'of the stations'))

modqpd <- lm(qsta_table$SES_qPD_station ~ qsta_table$SES_qPD_quadrat)
# ----

# Save ----

plots <- ggpubr::ggarrange(esth_qs, qtd_qs, qfd_qs, qpd_qs, ncol = 2, nrow = 2)
ggplot2::ggsave(plots, filename = hh("output", "13_AppendixH_FigureH1.pdf"),  width = 20,
                height = 15, units = "cm")

rm(list=ls(all=TRUE))

# ----
