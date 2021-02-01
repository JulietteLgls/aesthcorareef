###################################################################################################
#' Figure 3: Histogram of the significant effects of 68 species on the aesthetic score.
#'
#' This script produces the Langlois et al.'s 2021 paper Figure 3, 
#' i.e. a Histogram of the strength (and direction) of the significant (p < 0.05) effects of 68
#' species on the aesthetic values of the photographic quadrats.
#'
#' @author Juliette Langlois, \email{juliette.a.langlois@@gmail.com},
#'         Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#'         François Guilhaumon, \email{francois.guilhaumon@@ird.fr}
#'
#' @date 2021/01/12
##################################################################################################

# Load data ----

effect      <- read.csv(hh("output", "06_AppendixS6_TableS1.csv"))
phylo_table <- read.csv(hh("data", "phylo_table.csv"))

# ----

# Prepare Plot ---- 

effect       <- effect[order(effect$Code),]
effect_clado <- phylo_table[phylo_table$ID_code %in% effect$Code,]

# actualize the levels
effect_clado$ID_code <- factor(effect_clado$ID_code)
effect_clado$id_name <- factor(effect_clado$id_name)
effect_clado$species <- factor(effect_clado$species)  
effect_clado$Genus   <- factor(effect_clado$Genus)
effect_clado$Family  <- factor(effect_clado$Family)
effect_clado$Order   <- factor(effect_clado$Order)
effect_clado$Class   <- factor(effect_clado$Class)
effect_clado$Phylum  <- factor(effect_clado$Phylum)
effect_clado$Kingdom <- factor(effect_clado$Kingdom)

effect           <- as.data.frame(effect)
effect           <- effect[,-which(colnames(effect) == "Name")]
colnames(effect) <- c("ID_code", "effect", "std_error")
effect_clado     <- merge(effect, effect_clado, by = "ID_code")
class            <- effect_clado[,c("ID_code","id_name", "Family", "Order","Class", "Phylum", "effect")]

group1     <- c("family_54", "family_77", "family_36")
group2     <- c("order_7", "order_42")                
group3     <- c("order_40", "order_21")               

for(i in 1:nrow(class)){
  if(class$Family[i] == "family_6") {class$group[i] <- "group4"}      
  if(class$Family[i] %in% group1)   {class$group[i] <- "group1"}       
  if(class$Order[i]  == "order_9")  {class$group[i] <- "group5"}        
  if(class$Phylum[i] == "phylum_1") {class$group[i] <- "group6"}   
  if(class$Phylum[i] == "phylum_7") {class$group[i] <- "group7"}   
  if(class$Phylum[i] == "phylum_3") {class$group[i] <- "group8"}  
  if(class$Phylum[i] == "phylum_9") {class$group[i] <- "group8"}     
  if(class$Phylum[i] == "phylum_6") {class$group[i] <- "group9"}        
  if(class$Phylum[i] == "phylum_10"){class$group[i] <- "group10"} 
  if(class$Phylum[i] == "phylum_2") {class$group[i] <- "group11"}     
  if(class$Order[i]  == "order_13") {class$group[i] <- "group12"}      
  if(class$Order[i]  %in% group2)   {class$group[i] <- "group2"}
  if(class$Order[i]  %in% group3)   {class$group[i] <- "group3"}    
  if(class$ID_code[i] == "sp_137")  {class$group[i] <- "group3"} 
} # eo for

class$id_name <- with(class, reorder(id_name, effect))
class$ID_code <- with(class, reorder(ID_code, effect))

# reorder species inside the groups
# order the groups according to the value othe higher effect
class  <-  as.data.frame(class)
groups <- as.vector(unique(class$group))

# We want to know if the higher effect in absolute value is negative or positive 
# and we want to get it in order to use it to order the groups
maxes  <- unlist(lapply(X = groups, FUN = function(g){
  # g <- groups[7]
  maxabs <- max(abs(class$effect[which(class$group == g)]))
  ifelse(class$effect[which(abs(class$effect) == maxabs)] >0, # condition
         r <- maxabs, # if yes
         r <- -maxabs) # if no
  r}))
maxgrp <- data.frame(groups, maxes)
maxgrp <- maxgrp[order(maxgrp[,2], decreasing = TRUE),]
class$group <-  factor(class$group, levels = maxgrp[,1])

# order table by effect of group and effect in group
class_bygrp         <-  dplyr::group_by(.data = class, group) 
class_bygrp         <- dplyr::arrange(.data = class_bygrp, desc(effect), .by_group = TRUE) 
class_bygrp$ID_code <-  factor(class_bygrp$ID_code, levels = class_bygrp$ID_code)

# Same color bar for all
class_bygrp$colclas <- cut(x = class_bygrp$effect, breaks = 10, labels = FALSE)
cols                <- viridis::viridis(n = 10, direction = 1)

for(i in 1:length(cols)){
  for(j in 1:nrow(class_bygrp)){
    if(class_bygrp$colclas[j] == i){
      class_bygrp$colclas[j] <- cols[i]
    } # eo if
  } # eo for j
} # eo for i

# ----

# Plot ----

par(mar = c(0,0,0,0), bg = NA)
sp_barplot <-
  ggplot2::ggplot(data = class_bygrp, ggplot2::aes(x = ID_code, y = effect )) +
  ggplot2::geom_bar(fill =  class_bygrp$colclas, stat = "identity") +
  ggplot2::facet_grid(~group, scales = "free_x", space = "free_x") + # create facets by groups + same size for all bars
  ggplot2::theme_light() +
  ggplot2::theme(panel.grid = ggplot2::element_blank(),
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 panel.border = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_text(angle = 90),
                 axis.ticks.x = ggplot2::element_blank(),
                 panel.spacing.x = ggplot2::unit(0, "null"), # remove white space between facets
                 strip.text.x = ggplot2::element_text(angle = 0, size = 10),
                 strip.background.x = ggplot2::element_blank()) + 
  ggplot2::ylim(range(class$effect)) +
  ggplot2::labs(y = "", x = "") + 
  ggplot2::geom_vline(xintercept = 0.39, linetype = "dotted", 
                      color = "tomato", size = 0.8) +
  ggplot2:: geom_hline(yintercept = 0, linetype = "solid",
                       color = "red", size = 0.7)
# save
ggplot2::ggsave(plot = sp_barplot, filename = hh("output", "07_Figure3.pdf"), height = 8, width = 20, units = "cm", limitsize = FALSE)

# ----
