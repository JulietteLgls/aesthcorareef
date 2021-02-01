###################################################################################################
#' Appendix S7: Relationships between the taxonomic, functional and phylogenetic ranks of the 
#' quadrats.
#' 
#' This script produces the Langlois et al.'s 2021 paper Appendix S7, i.e. a three panels 
#' scatterplot representing the relationship between  qPDSES rank and TD rank (right), qFDSES rank 
#' and TD rank (middle) and qPDSES rank and qFDSES rank (right).
#' 
#' @author Juliette Langlois, \email{juliette.a.langlois@@gmail.com},
#'         Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#'         François Guilhaumon, \email{francois.guilhaumon@@ird.fr}
#'
#' @date 2021/01/12
##################################################################################################

# Load data ----

load(hh("output", "08_quadrat_pressure_hillses.RData"))

#color palette
pal <- "viridis" 

# ----

# Ecological value index ----
# First rank the quadrat by their TD/PD/FD value
qTD           <- cbind.data.frame(quadrat_code = table$quadrat_code, qTD = table$qTD)
rownames(qTD) <- NULL
qTD           <- qTD[order(qTD$qTD, decreasing = T),]
qTD$rank      <- seq(1, nrow(qTD),1)
qTD           <- qTD[order(qTD$quadrat_code),]

SES_qPD     <- cbind.data.frame(quadrat_code = table$quadrat_code, SES_qPD = table$SES_qPD)
SES_qPD      <- SES_qPD[order(SES_qPD$SES_qPD, decreasing = T),]
SES_qPD$rank <- seq(1, nrow(SES_qPD),1)
SES_qPD      <- SES_qPD[order(SES_qPD$quadrat_code),]

SES_qFD      <- cbind.data.frame(quadrat_code = table$quadrat_code, SES_qFD = table$SES_qFD)
SES_qFD      <- SES_qFD[order(SES_qFD$SES_qFD, decreasing = T),]
SES_qFD$rank <- seq(1, nrow(SES_qFD),1)
SES_qFD      <- SES_qFD[order(SES_qFD$quadrat_code),]

# bind the 3 ranks, sum them and compute the ecological rank
ecovalue           <- cbind.data.frame(quadrat_code = qTD$quadrat_code, TD_rank = qTD$rank, PD_rank = SES_qPD$rank, FD_rank = SES_qFD$rank)
ecovalue$sum       <- apply(ecovalue[,which(colnames(ecovalue) %in% c("TD_rank", "PD_rank", "FD_rank"))], 1, sum)
ecovalue           <- ecovalue[order(ecovalue$sum),]
ecovalue$eco_rank  <- seq(1, nrow(ecovalue),1)
ecovalue           <- ecovalue[order(ecovalue$quadrat_code),]
ecovalue           <- ecovalue[, - which(colnames(ecovalue) == "sum")]

# plot TD PD FD ranks
par(mar = c(1,1,0,1))
col <- viridis::viridis(3, begin = 0.9, end = 0.1)

tdpd <- ggplot2::ggplot(ecovalue, ggplot2::aes(x = TD_rank, y = PD_rank)) +
  ggplot2::geom_point(size = 0.8, shape = 20, col = col[1]) + 
  ggplot2::theme_light() + 
  ggplot2::scale_x_continuous(breaks = c(1000, 3000, 5000, 7000),
                     labels = c("1000", "3000", "5000", "7000")) + 
  ggplot2::scale_y_continuous(breaks = c(1000, 3000, 5000, 7000),
                     labels = c("1000", "3000", "5000", "7000")) +
  ggplot2::labs(x = bquote(~qPD[SES]~'rank'), y = "TD rank")

modtdpd <- lm(PD_rank ~ TD_rank, data = ecovalue)

tdfd <- ggplot2::ggplot(ecovalue, ggplot2::aes(x = TD_rank, y = FD_rank)) +
  ggplot2::geom_point(size = 0.8, shape = 20, col = col[2]) + 
  ggplot2::theme_light() + 
  ggplot2::scale_x_continuous(breaks = c(1000, 3000, 5000, 7000),
                     labels = c("1000", "3000", "5000", "7000")) + 
  ggplot2::scale_y_continuous(breaks = c(1000, 3000, 5000, 7000),
                     labels = c("1000", "3000", "5000", "7000")) +
  ggplot2::labs(x = bquote(~qFD[SES]~'rank'), y = "TD rank")

modtdfd <- lm(FD_rank ~ TD_rank, data = ecovalue)

pdfd <- ggplot2::ggplot(ecovalue, ggplot2::aes(x = PD_rank, y = FD_rank)) +
  ggplot2::geom_point(size = 0.8, shape = 20, col = col[3]) + 
  ggplot2::theme_light() + 
  ggplot2::scale_x_continuous(breaks = c(1000, 3000, 5000, 7000),
                     labels = c("1000", "3000", "5000", "7000")) + 
  ggplot2::scale_y_continuous(breaks = c(1000, 3000, 5000, 7000),
                     labels = c("1000", "3000", "5000", "7000")) +
  ggplot2::labs(x = bquote(~qPD[SES]~'rank'), y = bquote(~qFD[SES]~'rank'))

modpdfd <- lm(FD_rank ~ PD_rank, data = ecovalue)

threed <- ggpubr::ggarrange(tdpd, tdfd, pdfd, ncol = 3, nrow = 1)

ggplot2::ggsave(filename = hh("output", "10_AppendixS7_FigureS1.pdf"), plot = threed, 
       width = 18, height = 9, units = "cm", dpi = 320, family = "sans")
# ----

# Aesthetic rank ----
esth           <- table[,c("quadrat_code", "esth_score")]
esth           <- esth[order(esth$esth_score, decreasing = T),]
esth$esth_rank <- seq(1, nrow(esth),1)
esth           <- esth[order(esth$quadrat_code),]

# ----

# Save ----

esth_eco_rank <- merge(x = ecovalue, y = esth, by = "quadrat_code")
table <- merge(table, esth_eco_rank[,-which(colnames(esth_eco_rank) == "esth_score")], by = "quadrat_code")

save(table, file = hh("output", "10_quadrat_pressure_hillses_ranks.RData"))

rm(table, ecovalue, esth, esth_eco_rank, modpdfd, modtdpd, modtdfd, pdfd, tdfd, tdpd, qTD, SES_qFD, SES_qPD, threed, 
   col, pal)

# ----
