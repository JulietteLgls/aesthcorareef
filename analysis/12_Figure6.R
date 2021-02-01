###################################################################################################
#' Figure 6: Map of the aesthetic value and ecological value of the stations of coralligenous 
#' reefs of the French Mediterranean coastline.
#' 
#' This script produces the Langlois et al.'s 2021 paper Figure 6, i.e. a map of the 
#' aesthetic value and ecological value of the stations of coralligenous reefs of the French
#' Mediterranean coastline and a scatterplot representing the relation between the aesthetic 
#' rank and the ecological rank of the stations and their distribution on the edges of the figure.
#' 
#' @author Juliette Langlois, \email{juliette.a.langlois@@gmail.com},
#'         Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#'         François Guilhaumon, \email{francois.guilhaumon@@ird.fr}
#'
#' @date 2021/01/12
##################################################################################################

# Load data ----

load(hh("output", "10_quadrat_pressure_hillses_ranks.RData"))

pal <- "viridis" 

# ----

# Agregate at the station level ----

# Create a table of which the unity is the station and not the quadrat code
table_station <- unique(cbind.data.frame(station   = table$station, 
                                         site      = table$site, 
                                         depth     = table$depth, 
                                         longitude = table$longitude, 
                                         latitude  = table$latitude,
                                         region    = table$region))
table_station           <- table_station[order(table_station$station),]
rownames(table_station) <- NULL

# to characters and numerics
table_station$station   <- as.character(table_station$station)
table_station$site      <- as.character(table_station$site)
table_station$depth     <- as.numeric(as.character(table_station$depth))
table_station$latitude  <- as.numeric(as.character(table_station$latitude))
table_station$longitude <- as.numeric(as.character(table_station$longitude))
table_station$region    <- as.character(table_station$region)

# unique station
dupl <- as.character(table_station$station[which(duplicated(table_station$station) & duplicated(table_station$depth))])
dupl <- unique(dupl)

for(i in 1:length(dupl)){
  n             <- length(which(table_station$station == dupl[i]))
  table_station <- table_station[-which(table_station$station == dupl[i])[1:n-1],]
} # eo for i

# Select the columns to mean
col_mean <- colnames(table)[-which(colnames(table) %in% c("quadrat_code", "site", "station", "longitude", "latitude", "depth", "region", "year"))]

# Function to aggregate
#' sta_agr
#' Compute the mean of a given vector by station
#' @param stations list of the stations
#' @param var_col column to work on
#' @param colname name of the column to work on
#'
#' @return the mean value of the vector for each station
#' @export
sta_agr <- function(stations, var_col, colname){
  res            <- do.call(rbind.data.frame, lapply(unique(stations), function(sta){
    c( as.character(sta), mean(var_col[ which (stations == as.character(sta))], na.rm = TRUE) )}))
  colnames(res)  <- c("station", colname)
  res            <- res[order(res$station),]
  res
}

# Single column "station
sta_mean           <- cbind.data.frame(lapply(col_mean, function(var){
  sta_agr(table$station, table[,which(colnames(table) == var)], var)
}))
rownames(sta_mean) <- sta_mean$station
sta_mean           <- sta_mean[,-which(colnames(sta_mean) == "station")]
sta_mean$station   <- rownames(sta_mean)
rownames(sta_mean) <- NULL

# Final table at the station level
table_station <- merge(table_station, sta_mean, by = "station")
rm(sta_mean, col_mean, dupl, i, n, sta_agr)

# Get characters and numbers instead of levels
table_station$esth_score       <- as.numeric(levels(table_station$esth_score))[table_station$esth_score]
table_station$qTD              <- as.numeric(levels(table_station$qTD))[table_station$qTD]
table_station$SES_qPD          <- as.numeric(levels(table_station$SES_qPD))[table_station$SES_qPD]
table_station$SES_qFD          <- as.numeric(levels(table_station$SES_qFD))[table_station$SES_qFD]
table_station$Exploitation     <- as.numeric(levels(table_station$Exploitation))[table_station$Exploitation]
table_station$Anthropization   <- as.numeric(levels(table_station$Anthropization))[table_station$Anthropization]
table_station$substrate_recouv <- as.numeric(levels(table_station$substrate_recouv))[table_station$substrate_recouv]

# Save for future use
save(table_station, file = hh("output", "12_table_station.RData"))

# Here the aesthetic score is needed non transformed
table_station$esth_score <- exp(table_station$esth_score)

# ----

# Select columns to work with ----
data_station <- data.frame(site       = table_station$site, 
                           station    = table_station$station, 
                           longitude  = table_station$longitude, 
                           latitude   = table_station$latitude, 
                           profondeur = table_station$depth, 
                           region     = table_station$region,
                           eco_rank   = table_station$eco_rank,
                           esth_rank  = table_station$esth_rank,
                           esth_score = table_station$esth_score)
data_station <- data_station[order(data_station$site),]
rownames(data_station) <- NULL
rm(table_station)
# ----

# Colours according to esth_score ----
# Classes at equal intervals
step <- (max(data_station$esth_score) - min(data_station$esth_score))/5
c1   <- round(min(data_station$esth_score) + step,2)
c2   <- round(min(data_station$esth_score) + 2*step,2)
c3   <- round(min(data_station$esth_score) + 3*step,2)
c4   <- round(min(data_station$esth_score) + 4*step,2)

data_station$esth_class         <- cut(data_station$esth_score, breaks = c(min(data_station$esth_score)-1, c1, c2, c3, c4, max(data_station$esth_score)+1))
levels(data_station$esth_class) <- c(paste0("≤",c1), 
                                     paste0("(",c1,";",c2,"]"), 
                                     paste0("(",c2,";",c3,"]"), 
                                     paste0("(",c3,";",c4,"]"), 
                                     paste0(">",c4))

# Attribute a color to each class of esthe score.
col <- cbind.data.frame(class = levels(data_station$esth_class), col = viridis::viridis(n = 5, direction = 1, option = pal))
for (i in 1: nrow(data_station)){
  for(j in 1 : nrow(col)){
    if(as.character(data_station$esth_class[i]) == as.character(col$class[j])){data_station$col[i] <- levels(col$col)[col$col][j]} # eo if
  } # eo for j
} # eo for i

rm(i,j, c2, c3, col, c1, c4, step)

# ----

# Size according to Ecological rank ----

data_station$eco_rank <- as.numeric(levels(data_station$eco_rank))[data_station$eco_rank]
# Classes at equal intervals
step <- (max(data_station$eco_rank) - min(data_station$eco_rank))/5
s1   <- round(min(data_station$eco_rank) + step, 0)
s2   <- round(min(data_station$eco_rank) + 2*step, 0)
s3   <- round(min(data_station$eco_rank) + 3*step, 0)
s4   <- round(min(data_station$eco_rank) + 4*step, 0)

data_station$eco_class          <- cut(data_station$eco_rank, breaks = c(min(data_station$eco_rank)-1, s1, s2, s3, s4, max(data_station$eco_rank)+1))
levels(data_station$eco_class ) <- c(paste0("≤",s1), paste0("(",s1,";",s2,"]"), paste0("(",s2,";",s3,"]"), paste0("(",s3,";",s4,"]"), paste0(">",s4))
# Attribute a size to each class of eco rank. The lower the eco rank, the higher the eco value and the bigger the point
data_station$size[order(data_station$eco_class)] <- c(5,4,3,2,1)[as.numeric(data_station$eco_class[order(data_station$eco_class)])]

rm(step, s1, s2, s3, s4)

# ----

# Plot distrib ----
model                  <- lm(formula = as.numeric(as.character(esth_rank)) ~ as.numeric(as.character(eco_rank)), data = data_station)
data_station$esth_rank <- as.numeric(as.character(data_station$esth_rank))

p <- ggplot2::ggplot(data_station, ggplot2::aes(x = eco_rank, y = esth_rank, size = size)) +
  ggplot2::geom_point(ggplot2::aes(colour = esth_class)) +
  ggplot2::theme_bw() +
  viridis::scale_color_viridis(discrete = TRUE) +
  ggplot2::theme(legend.position = "none") +
  ggplot2::labs(x = "Ecological rank", y = "Aesthetic rank") +
  ggplot2::scale_x_reverse(position = "bottom") +
  ggplot2::scale_y_reverse(position = "left") +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 60, family = "sans"), 
        axis.title.y = ggplot2::element_text(size = 60, family = "sans"), 
        axis.text.x = ggplot2::element_text(size = 35, family = "sans"), 
        axis.text.y = ggplot2::element_text(size = 35, family = "sans"), 
        panel.grid = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = "transparent"),
        plot.background = ggplot2::element_rect(fill = "transparent", color = NA)
  )
# marginal density
q <- ggExtra::ggMarginal(p, type = "density", margins = "both", color = "lightgray", fill = "gray85")

ggplot2::ggsave(q, filename = hh("output", "12_esth_eco_density.png"), bg = "transparent", width = 13, height = 13)

# ----

# Separate mainland data and corse data ----

data_corse          <- data_station[which(data_station$region %in% "Corse"),]
data_corse          <- data_corse[order(data_corse$latitude, decreasing = TRUE),]
data_corse$site     <- factor(data_corse$site, levels = unique(data_corse$site[order(data_corse$latitude)]))

data_cont           <- data_station[- which(data_station$region %in% c("Corse")),]
data_cont           <- data_cont[order(data_cont$longitude),]
rownames(data_cont) <- NULL
data_cont$site      <- factor(data_cont$site, levels = unique(data_cont$site[order(data_cont$longitude, decreasing = FALSE)]))

# ----

# Get map background ----

load(hh("data", "mainland_background.RData"))
load(hh("data", "corsica_background.RData"))

# ----

# MAINLAND

# Preparation mainland ----
# create vectors
xlims       <- c(2.65, 7.8)
ylims_lat   <- c(42.41, 44)
ylims_depth <- c(85, 15)
## vertical lines
x_cont  <- data_cont$longitude
y0_cont <- data_cont$latitude
y1_cont <- rep(0, length(y0_cont))

# prepare legend
# size according to eco value
leg_size <- unique(cbind.data.frame(size = data_station$eco_class, cex = data_station$size))
leg_size <- leg_size[order(leg_size$cex),]
# col according to esthval
leg_col  <- unique(cbind.data.frame(esth_val = data_station$esth_class, col = data_station$col))
leg_col  <- leg_col[order(leg_col$esth_val, decreasing = FALSE),]

# ----  

# Mainland plot ----

pdf(hh("output", "12_mainland_map.pdf"))
nf <- layout( matrix(c(1,2), ncol=1))
par(mar = c(0,4,2,0.5))
# TOP
# create empty plot
plot(1, type = "n", xaxt = "n", yaxt = "n", xlim = xlims, ylim = ylims_lat, xlab = "", ylab = "", bty = "n")
# add raster background
plot(cont_bound_rast, add = TRUE, col = "gray85", legend = FALSE) 
# longitudes
axis(side = 3, las = 1, at = c(2, 3, 4, 5, 6, 7), cex.lab = 1, lwd = -1, lwd.ticks = 2, col.ticks = "gray", labels = c("2°E", "3°E", "4°E", "5°E", "6°E", "7°E"))
# latitudes
axis(side = 2, las = 1, at = c(42, 42.5, 43, 43.5, 44, 44.5), cex.lab = 1, lwd = -1, lwd.ticks = 2, col.ticks = "gray", labels = c("42°N", "42.5°N", "43°N", "43.5°N", "44°N", "44.5°N" ))
# plot sites
points(data_cont$longitude, data_cont$latitude, pch = 21, col = scales::alpha(colour = "black", alpha = 0.6), bg = scales::alpha(colour = "gray", alpha = 0.6), cex = 1)
# plot Marseille
points(x = 5.4, y = 43.25, pch = 21, col = "red", bg = "red", cex = 1.5)
text(x = 5.75, y = 43.35, labels = "Marseille", col = "red", family = "sans")
# lines to connect to stations
segments ( x0 = x_cont, y0 = y0_cont, x1 = x_cont, y1 = y1_cont, col = "lightgray", lty = 3)
# scale
maps::map.scale(x = 3, y = 43.9, relwidth = 0.15, metric = TRUE, ratio = FALSE, cex = 0.5)

# BOTTOM
par(mar = c(0.5,4,0,0.5))
# create empty plot
plot(1, type = "n", bty = 'n', xaxt = "n", yaxt = 'n', ylab = "Depth(m)", xlab = "", xlim = xlims, ylim = ylims_depth)
# add depth axis
axis(side = 2, las = 1, at = c(20, 30, 40, 50, 60, 70, 80), cex.lab = 1, lwd = -1, lwd.ticks = 2, col.ticks = "gray", labels = c("20", "30", "40", "50", "60", "70", "80"))
# horizontal depth lines
abline(h = c(80, 70, 60, 50, 40, 30, 20), lty = 3, col  = "lightgray")
# vertical lines to join the ones of the upper panel
abline(v = x_cont, col = "lightgray", lty = 3)
# station points
points(x = data_cont$longitude, y = data_cont$profondeur, col = scales::alpha(colour = data_cont$col, alpha = 0.7), pch = 20, cex = data_cont$size*0.7)
dev.off()

# ----

# CORSICA

# Prepare plot ----
# centroide of island
# get long lat from raster
data_matrix <- rasterToPoints(corsica_bound_rast)
# get  coordinates of centroidecentroide
centroide <- geosphere::centroid(data_matrix[,1:2])
# get polar coordinates of centroide
# polar_centroide <- useful::cart2pol(as.numeric(centroide$longitude), as.numeric(centroide$latitude))
polar_centroide <- useful::cart2pol(centroide[1], centroide[2])

# coordinates of the sites
sites <- unique(cbind.data.frame(site = data_corse$site, longitude = data_corse$longitude, latitude  = data_corse$latitude))
x0    <- data_corse$longitude
y0    <- data_corse$latitude

# polar coordinate of the sites
polar_sites   <- cbind.data.frame(site = data_corse$site, useful::cart2pol((data_corse$longitude - as.numeric(centroide[,"lon"])), 
                                                                   (data_corse$latitude - as.numeric(centroide[,"lat"]))))
polar_sites$x <- as.numeric(centroide[,"lon"]) + polar_sites$x
polar_sites$y <- as.numeric(centroide[,"lat"]) + polar_sites$y

# radius of the circle
X     <- cbind(as.vector(rbind(data_corse$longitude, centroide[,"lon"])), 
               as.vector(rbind(data_corse$latitude, centroide[,"lat"]))) # distance matrix between the centroide and each site
n     <- nrow(X)
r     <- max(as.matrix(dist(X))[n,])

# circle function
plotCircle <- function(x, y, r, ...) {angles <- seq(0, 2 * pi, length.out = 360)
lines(r * cos(angles) + x, r * sin(angles) + y, ...) }

# Depths
depths <- c(40, 50, 60, 70, 80, 90)
radius <- r + (depths - 30) / 100
# pt 40r : the intersection of the segemnt coming from the real site and the first circle
pt_40r   <- cbind.data.frame(site = polar_sites$site, theta = polar_sites$theta,r = rep(radius[1], nrow(polar_sites)))
pt_40r$x <- as.numeric(centroide[,"lon"]) + useful::pol2cart( pt_40r$r, pt_40r$theta)$x
pt_40r$y <- as.numeric(centroide[,"lat"]) + useful::pol2cart( pt_40r$r, pt_40r$theta)$y
# pt 40v : same but with the segment comes from the virtual site. Prevent overlapping of points and labels
pt_40v <- cbind.data.frame(site = polar_sites$site, theta = pt_40r$theta, r = pt_40r$r)
change <- c("St-Florent", "Canari", "Centuri", "Scandola-1","Sanguinaires", "BonifacioS1", "Lavezzi" , "Tarco", "Tarco2")
adjust <- c(-0.05, -0.02 , 0.02, 0.02, -0.02, -0.05, 0.05, 0, 0.05)
for (i in 1:length(change)){ pt_40v$theta[which(pt_40v$site == change[i])] <- pt_40v$theta[which(pt_40v$site == change[i])] + adjust[i] }
pt_40v$x <- as.numeric(centroide[,"lon"]) + useful::pol2cart( pt_40v$r, pt_40v$theta)$x
pt_40v$y <- as.numeric(centroide[,"lat"]) + useful::pol2cart( pt_40v$r, pt_40v$theta)$y
# pt 90 : projection of pt_40v on the last circle
pt_90   <- cbind.data.frame(site = polar_sites$site, theta = pt_40v$theta, r = rep(radius[length(radius)], nrow(polar_sites)))
pt_90$x <- as.numeric(centroide[,"lon"]) + useful::pol2cart( pt_90$r, pt_90$theta)$x
pt_90$y <- as.numeric(centroide[,"lat"]) + useful::pol2cart( pt_90$r, pt_90$theta)$y

# coordinates of the stations
polar_corse           <- cbind.data.frame(site = data_corse$site, station = data_corse$station, depth = data_corse$profondeur, esth = data_corse$esth_score, 
                                          ecoval = data_corse$eco_rank, col = data_corse$col, cex = data_corse$size)
polar_corse$theta_sta <- pt_90$theta
polar_corse$r_sta     <- r + ((polar_corse$depth - 30) / 100)
polar_corse$x_sta     <- as.numeric(centroide[,"lon"]) + useful::pol2cart(polar_corse$r_sta, polar_corse$theta_sta)$x
polar_corse$y_sta     <- as.numeric(centroide[,"lat"]) + useful::pol2cart(polar_corse$r_sta, polar_corse$theta_sta)$y

# coordinates for the scale
x0     <- as.numeric(centroide[,"lon"]) + useful::pol2cart(r = radius[1], theta = 180 )$x 
y0     <- as.numeric(centroide[,"lat"])
x1     <- as.numeric(centroide[,"lon"]) + useful::pol2cart(r = radius[length(radius)], theta = 180 )$x
y1     <- y0
x2     <- as.numeric(centroide[,"lon"]) + useful::pol2cart(r = radius[3], theta = 180 )$x
y2     <- y0

# coordinates of the labels of the axis
x_axis <- as.numeric(centroide[,"lon"]) + useful::pol2cart(radius, 180)$x
y_axis <- y0

# unique corailleurs
cora_long <- 8.764440
cora_lat  <- 41.85930 

# ----

# Corsica plot ----

x   <- centroide[, "lon"] # center x
y   <- centroide[, "lat"] # center y
n   <- 1000 # number of pts
pts <- seq(0, 2 * pi, length.out = n)
r   <- radius[length(radius)] # radius
xy  <- cbind(x + r * sin(pts), y + r * cos(pts))
sl  <- sp::SpatialPolygons(list(Polygons(list(Polygon(xy)), "line")))

xlims <- c(7,10.6)
ylims <- c(40.5, 44)

pdf(hh("output", "12_corsica_map.pdf"))
par(mar = c(0,3,2,3))
# empty plot
plot(1, type = "n", xaxt = "n", yaxt = "n", xlim = xlims, ylim = ylims, xlab = "", ylab = "", bty = 'n')
# background from raster
plot(corsica_bound_rast, col = "gray85", legend = FALSE, add = TRUE) 
# sites
points(unique(data_corse$longitude)[-which(unique(data_corse$longitude) == cora_long )],
       unique(data_corse$latitude)[-which(unique(data_corse$latitude) == cora_lat )],
       pch = 21, col = scales::alpha("black",0.6), bg = scales::alpha("gray70", 0.6) , cex = 1)
# segments from the site to pt_40r
segments(x0 = data_corse$longitude, y0 = data_corse$latitude, x1 = pt_40r$x, y1 = pt_40r$y, lty = "dotted", col = "lightgray", lwd = 1.2, xpd = TRUE)
# segments from pt_40v to pt_90 
segments(x0 = pt_40v$x, y0 = pt_40v$y, x1 = pt_90$x, y1 = pt_90$y, lty = "dotted", col = "lightgray", lwd = 1.2)
for (i in 1: length(radius)){plotCircle(as.numeric(centroide[,"lon"]), as.numeric(centroide[, "lat"]), radius[i], lty = 3, col = "lightgray", lwd = 1.2)} # eo for i
# white polygon to hide east side circles
polygon(x = c(9.7, 10.8, 10.8, 9.7), y = c(42.9, 44.5, 40.3, 41.5), col = "white", border = "white")
# longitude axis
axis(side = 3, las = 1, cex = 1, lwd = -1, lwd.ticks = 2, col.ticks = "gray", at = c(8, 9, 10), labels = c("8°E", "9°E", "10°E"))
# latitude axis
axis(side = 4, las = 1, pos = 10.2, cex = 01, lwd = -1, lwd.ticks = 2, col.ticks = "gray", at = c( 41, 41.5, 42, 42.5, 43, 43.5), labels = c("41°N", "41.5°N", "42°N", "42.5°N", "43°N","43.5°N"))
# stations
points(x = polar_corse$x_sta, y = polar_corse$y_sta, pch = 20, col = scales::alpha(polar_corse$col, 0.7), cex = polar_corse$cex*0.7, xpd = TRUE)
# depth axis
arrows(x0 = 8.09, y0 = 42, x1 = 7.58, y1 = 42, col = "black", length = 0.08)
# depth labels
text(x = c(8.1, 7.9, 7.7), y = 42, labels = as.character(depths)[c(1,3,5)], pos = 1, cex = 0.6, col = "black")
# title of depth axis
text(x = 7.85, y = 42, labels = "Depth (m)", pos = 3, cex = 0.7, col = "black", xpd = TRUE)
# scale
maps::map.scale(x = 7.8, y = 40.6, relwidth = 0.15, metric = TRUE, ratio = FALSE, cex = 0.5, xpd = TRUE)
dev.off()

# ----

# Legend ----

xs <- c(2.5, 3.5, 4.5, 5.5, 6.5)
ys <- rep(2, 5)

png(hh("output", "12_legend.png"))
par(mar = c(0,0,0,0))
# purple 
plot(x = xs, y = ys, xlim = c(0,10), ylim = c(0,10), axes = FALSE, xlab = "", ylab = "", pch = 19, cex = leg_size$cex + 3,
     col = unique(data_cont$col[order(data_cont$esth_score)])[1])
# deep blue
points(x = xs, y = ys+1.1, pch = 19, cex = leg_size$cex + 3, col = unique(data_cont$col[order(data_cont$esth_score)])[2])
# turquoise
points(x = xs, y = ys+2.2, pch = 19, cex = leg_size$cex +3, col = unique(data_cont$col[order(data_cont$esth_score)])[3])
# green
points(x = xs, y = ys+3.3, pch = 19, cex = leg_size$cex+3,col = unique(data_cont$col[order(data_cont$esth_score)])[4])
# yellow
points(x = xs, y = ys+4.4, pch = 19, cex = leg_size$cex+3,col = unique(data_cont$col[order(data_cont$esth_score)])[5])
# arrows
arrows(x0 = xs[1]-0.25,         y0 = ys[1]+5.3, x1 = xs[length(xs)]+0.45, y1 = ys[1]+5.3, code = 2, length = 0.1, lwd = 2)
arrows(x0 = xs[length(xs)]+0.7, y0 = ys[1]-0.7, x1 = xs[length(xs)]+0.7,  y1 = ys[1]+5,   code = 2, length = 0.1, lwd = 2)
# labels
graphics::text (x = 4.5,                y = ys[1]+5.8, labels = "Ecological value", cex = 2.5)
graphics::text (x = xs[length(xs)]+1.2, y = 4,         labels = "Aesthetic value",  cex = 2.5, srt = 270)

dev.off()

# ----

# Gather ----

# Maps
cont       <- magick::image_read_pdf(hh("output", "12_mainland_map.pdf"))
cors       <- magick::image_read_pdf(hh("output", "12_corsica_map.pdf"))
dens       <- magick::image_read(hh("output", "12_esth_eco_density.png"))
leg_transp <- magick::image_read(hh("output", "12_legend.png"))

# Combine the two maps
both <- magick::image_append(c(cont,cors), stack = FALSE)
# Add the legend
compo_leg <- magick::image_composite(both, magick::image_scale(leg_transp, "1100x1100"), offset = "+1750+1100")
# Add the density plot
full_compo <- magick::image_composite(compo_leg, magick::image_scale(dens, "800x800"), offset = "+1950+170")

# Save the full composition
pdf(hh("output","12_Figure6.pdf"), width = 11, height = 8, family = "sans")
par(mar = c(0,0,0,0))
plot(full_compo)
dev.off()

rm(list=ls(all=TRUE)) 
file.remove(hh("output", "12_corsica_map.pdf"), hh("output", "12_esth_eco_density.png"), hh("output", "12_legend.png"), hh("output", "12_mainland_map.pdf"))
# ----
