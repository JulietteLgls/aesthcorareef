###################################################################################################
#' Figure 5: Relationship between the aesthetic and the ecological values of the quadrats.
#' 
#' This script produces the Langlois et al.'s 2021 paper Figure 5, i.e. a scatterplot 
#' representing the relationship between the aesthetic and the ecological values of the quadrats.
#' 
#' @author Juliette Langlois, \email{juliette.a.langlois@@gmail.com},
#'         Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#'         François Guilhaumon, \email{francois.guilhaumon@@ird.fr}
#'
#' @date 2021/01/12
###################################################################################################

# Load data ----

load(hh("output", "10_quadrat_pressure_hillses_ranks.RData"))
pal <- "viridis" 

# ----

# Select four examples of each extreme situation ----
# Arbitrary choice of four example for each extreme situation

# aes_rich    <- df[which(df$quadrat_code %in% c("aes_rich1", "aes_rich2", "aes_rich3", 
# "aes_rich4")),]
# aes_p       <- df[which(df$quadrat_code %in% c("aes_poor1", "aes_poor2" ,"aes_poor3", 
# "aes_poor4" )),]
# naes_p      <- df[which(df$quadrat_code %in% c("naes_poor1", "naes_poor2", "naes_poor3", 
# "naes_poor4")),]
# naes_rich   <- df[which(df$quadrat_code %in% c("naes_rich1", "naes_rich1", "naes_rich1", 
# "naes_rich1")),]
#
# corner_pict <- cbind.data.frame(aes_rich = aes_rich$quadrat_code, naes_p = naes_p$quadrat_code,
#                                 naes_rich = naes_rich$quadrat_code, aes_p = aes_p$quadrat_code)
#
# write.csv(x = corner_pict, file = hh("output","corner_picts.csv"))
# corner_pict <- read.csv( paste0(path_res,"corner_picts.csv"))

# path_all_picts <- hh("path", "to", "pictures") # path to your pictures
#
# dir.create(hh("output", "corner_picts"))}
# dir.create(hh("output", "corner_picts", "aes_rich"))}
# dir.create(hh("output", "corner_picts", "naes_p"))}
# dir.create(hh("output", "corner_picts", "naes_rich"))}
# dir.create(hh("output", "corner_picts", "aes_p"))}

# # ----

# Call the 16 pictures, rescale, add a border, the name and a letter ----

#' format_pict
#' load a png picture, rescale it, add a symbol in the upper left corner and save in as pdf
#' @param path_ori where to find the picture
#' @param name_ori its name 
#' @param path_final where to save it once processed
#' @param name_final how to name it
#' @param point_type which type of point, can be diamond, square, circle or triangle
#' @param fill_col filling color of the symbol
#' @param border_col color of the border of the symbol
#'
#' @return the picture with the symbol
#' @export
format_pict <- function(path_ori, name_ori, path_final, name_final, point_type, fill_col, 
                        border_col){

  if (point_type == "square")  {pch <- 22 ; size <-  7}
  if (point_type == "circle")  {pch <- 21 ; size <- 7}
  if (point_type == "triangle"){pch <- 24 ; size <-  7}
  if (point_type == "diamond") {pch <- 23 ; size <- 7}

  img <- imager::load.image(paste0(path_ori, name_ori)) # load the initial image
  # add a red symbol in the upper left corner and save
  pdf(paste0(path_final, name_final))
  plot(img, axes = FALSE)
  points(x = 0.1*imager::width(img), y = 0.1*imager::height(img), pch = pch, col = border_col,
         bg = fill_col, cex = size )
  dev.off()
  # load the new jpg image and add a white border
  img <- magick::image_read_pdf(paste0(path_final, name_final))
  img <- magick::image_scale(image = img, geometry = '300x300')
  return(img)
}

fill_col   <-  "white"
border_col <-  "black"

# aes_p1 <- format_pict(path_ori   = path_all_picts, 
#                       path_final = path_aesp, 
#                       name_ori   = paste0(corner_pict$aes_p[1], ".png"),
#                       name_final = "aes_p1.pdf",
#                       point_type = "square", 
#                       fill_col   = fill_col, 
#                       border_col = border_col)
# aes_p2 <- format_pict(path_ori   = path_all_picts,
#                       path_final = path_aesp, 
#                       name_ori   = paste0(corner_pict$aes_p[2], ".png"),
#                       name_final = "aes_p2.pdf", 
#                       point_type = "square", 
#                       fill_col   = fill_col, 
#                       border_col = border_col)
# aes_p3 <- format_pict(path_ori   = path_all_picts,
#                       path_final = path_aesp, 
#                       name_ori   = paste0(corner_pict$aes_p[3], ".png"), 
#                       name_final = "aes_p3.pdf",
#                       point_type = "square", 
#                       fill_col   = fill_col, 
#                       border_col = border_col)
# aes_p4 <- format_pict(path_ori   = path_all_picts,
#                       path_final = path_aesp,
#                       name_ori   = paste0(corner_pict$aes_p[4], ".png"),
#                       name_final = "aes_p4.pdf",
#                       point_type = "square",
#                       fill_col   = fill_col,
#                       border_col = border_col)
# 
# naes_p1 <- format_pict(path_ori   = path_all_picts,
#                        path_final = path_naesp,
#                        name_ori   = paste0(corner_pict$naes_p[1], ".png"),
#                        name_final = "naes_p1.pdf",
#                        point_type = "circle", 
#                        fill_col   = fill_col,
#                        border_col = border_col)
# naes_p2 <- format_pict(path_ori   = path_all_picts,
#                        path_final = path_naesp,
#                        name_ori   = paste0(corner_pict$naes_p[2], ".png"),
#                        name_final = "naes_p2.pdf",
#                        point_type = "circle", 
#                        fill_col   = fill_col,
#                        border_col = border_col)
# naes_p3 <- format_pict(path_ori   = path_all_picts, 
#                        path_final = path_naesp, 
#                        name_ori   = paste0(corner_pict$naes_p[3], ".png"),
#                        name_final = "naes_p3.pdf", 
#                        point_type = "circle",
#                        fill_col   = fill_col,
#                        border_col = border_col)
# naes_p4 <- format_pict(path_ori   = path_all_picts,
#                        path_final = path_naesp, 
#                        name_ori   = paste0(corner_pict$naes_p[4], ".png"),
#                        name_final = "naes_p4.pdf",
#                        point_type = "circle", 
#                        fill_col   = fill_col, 
#                        border_col = border_col)
# 
# naes_rich1 <- format_pict(path_ori   = path_all_picts,
#                           path_final = path_naesrich,
#                           name_ori   = paste0(corner_pict$naes_rich[1], ".png"), 
#                           name_final = "naes_rich1.pdf",
#                           point_type = "triangle",
#                           fill_col   = fill_col, 
#                           border_col = border_col)
# naes_rich2 <- format_pict(path_ori   = path_all_picts,
#                           path_final = path_naesrich,
#                           name_ori   = paste0(corner_pict$naes_rich[2], ".png"), 
#                           name_final = "naes_rich2.pdf",
#                           point_type = "triangle",
#                           fill_col   = fill_col, 
#                           border_col = border_col)
# naes_rich3 <- format_pict(path_ori   = path_all_picts,
#                           path_final = path_naesrich,
#                           name_ori   = paste0(corner_pict$naes_rich[3], ".png"),
#                           name_final = "naes_rich3.pdf", 
#                           point_type = "triangle",
#                           fill_col   = fill_col,
#                           border_col = border_col)
# naes_rich4 <- format_pict(path_ori   = path_all_picts,
#                           path_final = path_naesrich,
#                           name_ori   = paste0(corner_pict$naes_rich[4], ".png"), 
#                           name_final = "naes_rich4.pdf",
#                           point_type = "triangle",
#                           fill_col   = fill_col,
#                           border_col = border_col)
# 
# aes_rich1  <- format_pict(path_ori   = path_all_picts,
#                           path_final = path_aesrich, 
#                           name_ori   = paste0(corner_pict$aes_rich[1], ".png"),
#                           name_final = "aes_rich1.pdf", 
#                           point_type = "diamond", 
#                           fill_col   = fill_col,
#                           border_col = border_col)
# aes_rich2  <- format_pict(path_ori   = path_all_picts, 
#                           path_final = path_aesrich,
#                           name_ori   = paste0(corner_pict$aes_rich[2], ".png"),
#                           name_final = "aes_rich2.pdf",
#                           point_type = "diamond", 
#                           fill_col   = fill_col,
#                           border_col = border_col)
# aes_rich3  <- format_pict(path_ori   = path_all_picts,
#                           path_final = path_aesrich,
#                           name_ori   = paste0(corner_pict$aes_rich[3], ".png"), 
#                           name_final = "zaes_rich3.pdf",
#                           point_type = "diamond",
#                           fill_col   = fill_col,
#                           border_col = border_col)
# aes_rich4  <- format_pict(path_ori   = path_all_picts,
#                           path_final = path_aesrich,
#                           name_ori   = paste0(corner_pict$aes_rich[4], ".png"),
#                           name_final = "aes_rich4.pdf", 
#                           point_type = "diamond", 
#                           fill_col   = fill_col,
#                           border_col = border_col)

# here for the example, load a fake picture
fake_pict <- format_pict(path_ori   = hh("data/"), 
                         path_final = hh("output/"), 
                         name_ori   = paste0("fake_picture", ".png"), 
                         name_final = "11_fake_picture.pdf", 
                         point_type = "diamond", 
                         fill_col   = fill_col,
                         border_col = border_col)

# ----

# Create center plot ----
xlim        <- range(table$eco_rank)
ylim        <- range(table$esth_rank)
# center_plot <- magick::image_graph(width = 600, height = 600, res = 100)
center_plot <- ggplot2::ggplot(table, ggplot2::aes(x = `eco_rank`, y = `esth_rank`) ) +
  ggplot2::stat_density_2d(ggplot2::aes(fill = ..density..), geom = "raster", contour = FALSE) +
  viridis::scale_fill_viridis(option = pal, direction=1) +
  ggplot2::scale_x_reverse(position = "top", expand = c(0,0)) +
  ggplot2::scale_y_reverse(position = "right", expand = c(0,0)) +
  ggplot2::geom_point(size = 0.8, shape = 20, col = "gray") +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(hjust = 0),
          axis.text.y = ggplot2::element_text(angle = 0),
          axis.ticks = ggplot2::element_blank(),
          axis.text.x.top = ggplot2::element_blank(),
          axis.text.y.right = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank(),
          panel.border = ggplot2::element_blank(),
          plot.margin = ggplot2::unit(c(0.5,0.5,0.5,0.5), "cm"),
          legend.position ='none')
# +
# ggplot2::geom_point(data = df[which(df$eco_rank %in% aes_p$ecological_rank),],
#                     ggplot2::aes(x = df$eco_rank[which(df$eco_rank %in% aes_p$eco_rank)], 
#                                  y = df$esth_rank[which(df$eco_rank %in% aes_p$eco_rank)]), 
#                     shape = 22, bg = fill_col, col = border_col, size = 4) +
# ggplot2::geom_point(data = df[which(df$eco_rank %in% naes_p$eco_rank),],
#                     ggplot2::aes(x = (df$eco_rank[which(df$eco_rank %in% naes_p$eco_rank)] - 50),
#                  y = (df$esth_rank[which(df$eco_rank %in% naes_p$eco_rank)] - 50)),
#              shape = 21, bg = fill_col, col = border_col, size = 4) +
# ggplot2::geom_point(data = df[which(df$eco_rank %in% naes_rich$eco_rank),],
#                     ggplot2::aes(x = (df$eco_rank[which(df$eco_rank %in% naes_rich$eco_rank)] + 50),
#                  y = (df$esth_rank[which(df$eco_rank %in% naes_rich$eco_rank)] - 50)),
#              shape = 24, bg = fill_col, col = border_col, size = 4) +
# ggplot2::geom_point(data = df[which(df$eco_rank %in% aes_rich$eco_rank),],
#                     ggplot2::aes(x = (df$eco_rank[which(df$eco_rank %in% aes_rich$eco_rank)] + 50),
#                  y = (df$esth_rank[which(df$eco_rank %in% aes_rich$eco_rank)] + 50)),
#              shape = 23, bg = fill_col, col = border_col, size = 4)
# ggplot2::ggsave(plot = center_plot, filename = hh("output", "11_center_plot.pdf"), width = 20, 
#                 height = 20, units = "cm")
center_plot <- magick::image_read_pdf(hh("output", "11_center_plot.pdf"))
center_plot <- magick::image_scale(image = center_plot, geometry = '600x600')
# plot(center_plot)
# ----

# Create the frame plot ----
# top        <- magick::image_append(c(aes_p4, aes_rich1, aes_rich2), stack = FALSE)
# left       <- magick::image_append(c(aes_p3, aes_p2, aes_p1, naes_p4), stack = TRUE)
# plot_tl    <- magick::image_append(c(left, top), stack = FALSE)
# right      <- magick::image_append(c(aes_rich3, aes_rich4, naes_rich1, naes_rich2), stack = TRUE)
# plot_tlr   <- magick::image_append(c(plot_tl, right), stack = FALSE)
# bottom     <- magick::image_append(c(naes_p3, naes_p2, naes_p1, naes_rich4, naes_rich3))
# frame_plot <- magick::image_append(c(plot_tlr, bottom), stack = TRUE)

# fake frame
top        <- magick::image_append(c(fake_pict, fake_pict, fake_pict), stack = FALSE)
left       <- magick::image_append(c(fake_pict, fake_pict, fake_pict, fake_pict), stack = TRUE)
plot_tl    <- magick::image_append(c(left, top), stack = FALSE)
right      <- magick::image_append(c(fake_pict, fake_pict, fake_pict, fake_pict), stack = TRUE)
plot_tlr   <- magick::image_append(c(plot_tl, right), stack = FALSE)
bottom     <- magick::image_append(c(fake_pict, fake_pict, fake_pict, fake_pict, fake_pict))
frame_plot <- magick::image_append(c(plot_tlr, bottom), stack = TRUE)

rm(top,left, plot_tl, right, plot_tlr, bottom)
# ----

# Create and Save final plot ----
# final_plot <- image_composite(frame_plot, image_scale(center_plot, "950x950"), offset = "+302+258")

# here a fake composite image is created to reproduce the same proprtions
final_plot <- magick::image_composite(frame_plot, magick::image_scale(center_plot, "950x950"), 
                                      offset = "+300+254")
#
pdf(file = hh("output", "11_Figure5.pdf"), width = 11, height = 11, family = "sans")
par(mar = c(0,0,0,0))
plot(final_plot)
# eco arrow
shape::Arrows(x0 = 335, y0 = 315, x1 = 1210, y1 = 315, arr.length = 0.15, arr.width = 0.15,
              arr.type = "triangle")
# esth arrow
shape::Arrows(x0 = 320, y0 = 325, x1 = 320, y1 = 1198, arr.length = 0.15, arr.width = 0.15, 
              arr.type = "triangle")
# axis title
text(x = 755, y = 265, labels = "Ecological value", pos = 3, cex = 1.8, family = "sans")
text(x = 310, y = 745, labels = "Aesthetic value", pos = 3, cex = 1.8, srt = 90, family  = "sans")
# ranks esth
text(x = 305, y = 377, labels = "Rank 7692", cex = 1, family  = "sans", srt = 90)
text(x = 305, y = 1170, labels = "Rank 1", cex = 1, family  = "sans", srt = 90)
# ranks eco
text(x = 388, y = 300, labels = "Rank 7692", cex = 1, family  = "sans")
text(x = 1184, y = 300, labels = "Rank 1", cex = 1, family  = "sans")
dev.off()

rm(table, border_col, center_plot, fake_pict, fill_col, final_plot, frame_plot, pal, xlim, ylim,
   format_pict)
file.remove(hh("output", "11_center_plot.pdf"), hh("output", "11_fake_picture.pdf"))

# ----