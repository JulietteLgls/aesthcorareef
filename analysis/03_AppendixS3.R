###################################################################################################
#' Appendix S3: Cladistic information.
#'
#' This script produces the Langlois et al.'s 2021 paper Appendix S3 Figure S1,
#' i.e. a phylogenetic tree of the 177 taxa identified at the species or genus level.
#' 
#' Table S1: see data/phylo_table.csv
#'
#' @author Juliette Langlois, \email{juliette.a.langlois@@gmail.com},
#'         Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#'         François Guilhaumon, \email{francois.guilhaumon@@ird.fr}
#'
#' @date 2021/01/12
##################################################################################################

# Load data ----

phylo <- read.csv(hh("data", "phylo_table.csv"))

# ----

# Prepare plot ----
# colors according to the phyllum
ifelse(length(which(is.na(phylo$species))) != 0, phylo_slct <-  phylo[-which(is.na(phylo$species)),], phylo_slct <- phylo)

tax     <- ape::as.phylo(~Kingdom/Phylum/Class/Order/Family/Genus/species, data = phylo_slct)
phlm    <- merge(data.frame(species = tax$tip.label), phylo_slct[c("species", "Phylum")], by = "species", sort = F)
palette <- c(RColorBrewer::brewer.pal(n = 10, name = "Paired"),"darkgoldenrod1","brown")
cols    <- ifelse(phlm$Phylum == unique(phlm$Phylum)[1], palette[1],
               ifelse(phlm$Phylum == unique(phlm$Phylum)[2], palette[2],
                ifelse(phlm$Phylum == unique(phlm$Phylum)[3], palette[3],
                 ifelse(phlm$Phylum == unique(phlm$Phylum)[4], palette[4],
                  ifelse(phlm$Phylum == unique(phlm$Phylum)[5], palette[5],
                   ifelse(phlm$Phylum == unique(phlm$Phylum)[6], palette[6],
                    ifelse(phlm$Phylum == unique(phlm$Phylum)[7], palette[7],
                     ifelse(phlm$Phylum == unique(phlm$Phylum)[8], palette[8],
                      ifelse(phlm$Phylum == unique(phlm$Phylum)[9], palette[9],
                       ifelse(phlm$Phylum == unique(phlm$Phylum)[10], palette[10],
                        ifelse(phlm$Phylum == unique(phlm$Phylum)[11], palette[11],
                         ifelse(phlm$Phylum == unique(phlm$Phylum)[12], palette[12],""))))))))))))
# ----

# Plot ----

pdf(hh("output", "03_AppendixS3_FigureS1.pdf"))
par(mar=c(0, 0, 0, 0), mfrow=c(1,1))
ape::plot.phylo(tax, type = "fan",
     # col = "lightgray",
     edge.color = "lightgray",
     tip.col = cols,
     label.offset = 0.015,
     edge.width = 0.5,
     cex = 0.35,
     font = 1,
     align.tip.label = TRUE)
legend(-1.26, -0.8, legend = unique(phlm$Phylum),
       col = palette, lty = 1, cex = 0.4)
dev.off()

# ----
