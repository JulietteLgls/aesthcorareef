###################################################################################################
#' Appendix S1: Temporality of biodiversity data.
#'
#'This script produces the Langlois et al.'s 2021 paper Appendix S1, i.e.
#' a grid indicating the year each station has been sampled.
#'
#' @author Juliette Langlois, \email{juliette.a.langlois@@gmail.com},
#'         Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#'         François Guilhaumon, \email{francois.guilhaumon@@ird.fr}
#'
#' @date 2021/01/12
##################################################################################################

# Load data ----
table <- read.csv(hh("data", "quadrat_table.csv"))

# select info
data <-  unique(table[, c("site", "station", "year")])

# count the number of station per site per year
nb_sta_pery <- table(data$site, data$year)

# set color bar
cols        <- gray.colors(n = max(nb_sta_pery)+1, start = 1, end = 0, gamma = 2.2, rev = FALSE)

# ----

# Divide the data set in three : corsica, cont1 cont2 ----

# corsica
data_corse          <- table[which(table$region %in% "Corse"),]
data_corse          <- data_corse[order(data_corse$latitude, decreasing = TRUE),]
data_corse$site     <- factor(data_corse$site, levels = unique(data_corse$site[order(data_corse$latitude)]))

data_corse          <- unique(data_corse[, c("site", "station", "year", "latitude")])
data_corse$site     <- factor(data_corse$site, levels = unique(data_corse$site[order(data_corse$latitude)]))
data_corse          <- data_corse[,-which(colnames(data_corse) == "latitude")]
nbsta_corse         <- table(data_corse$site, data_corse$year)

# Rows of 0 for the year in which corsica hasn't been sampled
c_2010 <-  rep(0, nrow(nbsta_corse))
c_2012 <-  rep(0, nrow(nbsta_corse))
c_2015 <-  rep(0, nrow(nbsta_corse))
c_2016 <-  rep(0, nrow(nbsta_corse))

# Bind all the years
nbsta_corse           <- cbind(nbsta_corse, c_2010, c_2012, c_2015, c_2016)
nbsta_corse           <- nbsta_corse[, c("c_2010", "2011", "c_2012", "2013", "2014", "c_2015", "c_2016", "2017")]
colnames(nbsta_corse) <- c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")
nbsta_corse           <- as.table(nbsta_corse)
rm(c_2010, c_2012, c_2015, c_2016, data_corse)

# mainland
data_cont           <- table[- which(table$region %in% c("Corse")),]
data_cont           <- data_cont[order(data_cont$longitude),]
rownames(data_cont) <- NULL
data_cont           <- unique(data_cont[, c("site", "station", "year", "longitude")])
data_cont$site      <- factor(data_cont$site, levels = unique(data_cont$site[order(data_cont$longitude, decreasing = TRUE)]))
data_cont           <- data_cont[,-which(colnames(data_cont) == "longitude")]
nbsta_cont          <- table(data_cont$site, data_cont$year)

# Rows of 0 for the year in which mainland hasn't been sampled
c_2011 <-  rep(0, nrow(nbsta_cont))
c_2014 <-  rep(0, nrow(nbsta_cont))

# Bind all the years
nbsta_cont           <- cbind(nbsta_cont, c_2011, c_2014)
nbsta_cont           <- nbsta_cont[, c("2010", "c_2011", "2012", "2013", "c_2014", "2015", "2016", "2017")]
colnames(nbsta_cont) <- c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")
nbsta_cont           <- as.table(nbsta_cont)

nbsta_cont1     <- nbsta_cont[1:(nrow(nbsta_cont)%/%2),]
nbsta_cont2     <- nbsta_cont[((nrow(nbsta_cont)%/%2)+1):nrow(nbsta_cont),]
rm(c_2011, c_2014, data_cont)

# ----

# Plot ----

# Corsica
df_corse      <- as.data.frame(nbsta_corse)
df_corse$Freq <- as.factor(df_corse$Freq)

corse <- ggplot2::ggplot(df_corse, ggplot2::aes(Var2, Var1, fill = Freq)) + 
  ggplot2::geom_tile(color = "gray") +
  ggplot2::geom_vline(xintercept = 3.5, color = "red", linetype = "dashed") +
  ggplot2::coord_fixed(ratio = 1/2) +
  ggplot2::scale_x_discrete(position = "top") +
  ggplot2::scale_fill_manual(values = cols) + 
  ggplot2::theme_light() + 
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(angle = 45, vjust = 0, hjust = 0, size = 7),
        axis.text.y = ggplot2::element_text(size = 5, family = "sans"),
        legend.title = ggplot2::element_text(size = 18, family = "sans"),
        legend.text = ggplot2::element_text(size = 16, family = "sans"), 
        legend.position = "none") + 
  ggplot2::labs(fill = "Number of station")

# Cont 1
df_cont1      <- as.data.frame(nbsta_cont1)
df_cont1$Freq <- as.factor(df_cont1$Freq)
levels(df_cont1$Freq) <- c(levels(df_cont1$Freq), "4", "5")

cont1 <- ggplot2::ggplot(df_cont1, ggplot2::aes(Var2, Var1, fill = Freq)) + 
  ggplot2::geom_tile(color = "gray") +
  ggplot2::geom_vline(xintercept = 3.5, color = "red", linetype = "dashed") +
  ggplot2::coord_fixed(ratio = 1/2) +
  ggplot2::scale_x_discrete(position = "top") +
  ggplot2::scale_fill_manual(values = cols, drop = FALSE) + 
  ggplot2::theme_light() + 
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(angle = 45, vjust = 0, hjust = 0, size = 7),
        axis.text.y = ggplot2::element_text(size = 5, family = "sans"),
        legend.title = ggplot2::element_text(size = 18, family = "sans"),
        legend.text = ggplot2::element_text(size = 16, family = "sans"),
        legend.position = "none") +
  ggplot2::labs(fill = "Number of station")

# Cont 2
df_cont2      <- as.data.frame(nbsta_cont2)
df_cont2$Freq <- as.factor(df_cont2$Freq)
levels(df_cont2$Freq) <- c(levels(df_cont2$Freq), "5")

cont2 <- ggplot2::ggplot(df_cont2, ggplot2::aes(Var2, Var1, fill = Freq)) + 
  ggplot2::geom_tile(color = "gray") +
  ggplot2::geom_vline(xintercept = 3.5, color = "red", linetype = "dashed") +
  ggplot2::coord_fixed(ratio = 1/2) +
  ggplot2::scale_x_discrete(position = "top") +
  ggplot2::scale_fill_manual(values = cols, drop = FALSE) + 
  ggplot2::theme_light() + 
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(angle = 45, vjust = 0, hjust = 0, size = 7),
        axis.text.y = ggplot2::element_text(size = 5, family = "sans"),
        legend.title = ggplot2::element_text(size = 8, family = "sans"),
        legend.text = ggplot2::element_text(size = 8, family = "sans")) +
  ggplot2::labs(fill = "Number of\nstations")

# Arrange the three plots
leg   <- cowplot::get_legend(cont2)
cont2 <- cont2 + ggplot2::theme(legend.position = "none")
plots <- egg::ggarrange(cont2, cont1, corse, newpage = TRUE, nrow = 1, ncol = 3)    

# Save final plot
par(mar = c(0,0,0,0))
plots_leg <- ggpubr::ggarrange(plots, leg, ncol = 2, widths = c(7,1))
ggplot2::ggsave(filename = hh("output", "01_AppendixS1_FigureS1.pdf"), plot = plots_leg, device = "pdf", family = "sans")

# ----

rm(data, cont1, cont2, corse, df_cont1, df_cont2, df_corse, leg, plots, plots_leg, table, cols, nb_sta_pery, nbsta_cont,
   nbsta_cont1, nbsta_cont2, nbsta_corse)
graphics.off()
