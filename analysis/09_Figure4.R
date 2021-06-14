###################################################################################################
#' Figure 4: Vizualisation of the SEM.
#' 
#' This script produces the Langlois et al.'s 2021 paper Figure 4, i.e. a Chord chart 
#' summarizing the interactions between the aesthetic value, qTD, qFDSES, qPDSES, the percentage 
#' of sediment cover, depth and the two first axes of the PCA on the 13 anthropic threats.
#'
#' @author Juliette Langlois, \email{juliette.a.langlois@@gmail.com},
#'         Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#'         François Guilhaumon, \email{francois.guilhaumon@@ird.fr}
#'
#' @date 2021/01/12
###################################################################################################

# Load data ----

SEM_table <- read.csv(hh("output", "08_Table2.csv"))
# remove the explained % of dependant variable and the line of depth
edges           <- SEM_table[1:(nrow(SEM_table)-1), 
                             c("Explaining.Variable", "Dependant.Variable", "Coefficient")]
colnames(edges) <- c("from", "to", "weight")

# ----

# Chord plot ----

# customize the links to add a dotted border to the negative ones
sub  <- as.data.frame(edges[which(edges$weight<0), 1:2])
sub$lty_df     <- rep(3, nrow(sub)) # type of the line
sub$lwd_df     <- rep(1, nrow(sub)) # width of the line
sub$border_df  <- rep("black", nrow(sub)) # color of the line

# Correct names
wrong <- c("esth_score", "depth", "qTD", "SES_qPD", "SES_qFD", 
           "Exploitation", "Sediment")
right <- c("Aesthetic Value", "Depth", "qTD", "qPD_SES", "qFD_SES",
           "Exploitation", "Sediment")
for (i in 1: length(wrong)) {
  edges$from <- gsub(wrong[i], right[i], edges$from)
  edges$to <- gsub(wrong[i], right[i], edges$to)
  sub$from <- gsub(wrong[i], right[i], sub$from)
  sub$to <- gsub(wrong[i], right[i], sub$to)
}

edges$from    <- as.factor(edges$from)
edges$to      <- as.factor(edges$to)
n             <- length(union(edges$from, edges$to))
edges         <- edges[order(edges$weight),] # order the links according to their weight
pal           <- viridis::viridis(n = n, option = "viridis")
order_sectors <- c("Depth","Sediment", "Exploitation", "qFD_SES", "qPD_SES",
                   "qTD", "Aesthetic Value")
names(pal)    <- order_sectors
graphics.off()

pdf(hh("output","09_Figure4.pdf"), 
    family = "sans", width = 7, height = 7)
circlize::circos.par(gap.after = c(rep(3,n)))
circlize::chordDiagram(edges, 
                       grid.col              = pal,
                       order                 = c("Depth","Sediment",
                                                 "Exploitation", "qFD_SES",
                                                 "qPD_SES", "qTD",
                                                 "Aesthetic Value"),
                       annotationTrack       =  c("name", "grid"),
                       annotationTrackHeight = c(0.03, 0.1),
                       # make the links finish by an arrow
                       directional    = 1,
                       direction.type = "arrows",
                       link.arr.type  =  "big.arrow",
                       # dotted border
                       link.lty    = sub[,c("from", "to", "lty_df")], 
                       link.lwd    = sub[,c("from", "to", "lwd_df")],
                       link.border = sub[,c("from", "to", "border_df")],
                       scale       = FALSE
)
circlize::circos.clear()
dev.off()

rm(wrong, right, n, pal, edges, SEM_table, sub, i, order_sectors)
# -----
