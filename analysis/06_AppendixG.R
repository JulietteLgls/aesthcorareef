###################################################################################################
#'  Appendix G: Effect of the relative abundance of the 68 species on the aesthetic score.
#'
#' This script produces the Langlois et al.'s 2021 paper Appendix G Table G1, 
#' i.e. a table of the significant effects of the relative abundance of the 68 species on the 
#' aesthetic values of the quadrats where they are present. 
#'
#' @author Juliette Langlois, \email{juliette.a.langlois@@gmail.com},
#'         Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#'         François Guilhaumon, \email{francois.guilhaumon@@ird.fr}
#'
#' @date 2021/01/12
###################################################################################################

# Load tables ----

quadrat_table <- read.csv(hh("data", "quadrat_table.csv"))
phylo_table   <- read.csv(hh("data", "phylo_table.csv"))

# ----

# Select columns ----

data <- quadrat_table[, c(which(colnames(quadrat_table) %in% c("quadrat_code", "esth_score")), 
                          grep(pattern = "sp_", colnames(quadrat_table)))]

rm(quadrat_table)

# ----

# Select species identified on at least five quadrats ----

data_rownames           <- data
rownames(data_rownames) <- data_rownames$quadrat_code
data_rownames           <- data_rownames[,-which(colnames(data_rownames) == "quadrat_code")]

nb_qd     <- colSums(data_rownames !=0)
slct_5qd  <- nb_qd[nb_qd >= 5]
data_slct <- data[,c(1, which(colnames(data) %in% names(slct_5qd)))]

data <- data_slct

# check that they are not quadrats with no species identified on it
nbsp_qd <- rowSums(data !=0)
if(length(which(nbsp_qd == 0))==0){cat("\nNo empty quadrats ==> OK!\n")}

rm(nbsp_qd, nb_qd, slct_5qd)

# ----

# Order species by their contribution ----

# to do so we need a numeric table
data_num <- data_rownames[,which(colnames(data_rownames) %in% colnames(data))]
sp_names <- colnames(data_num)[-which(colnames(data_num) == "esth_score")]

# give species list all_id and data table dat_cor; 
# the variabe we want to explain is varcor and we chose the metric we want to use to order the 
# species ("var","pervar","R2","pearson.p). Here we want to use PERVAR the percentage of varcor
# individualy explained by all the other variables
pervar    <- look_cor(all_id = sp_names, dat_cor = data_num, respvar = "esth_score", 
                      ord = "PERVAR")
SPE_order <- pervar$var

# ----

# Keep only the species of which effect is significant ----

final_sp <- reduce_mod(sp_list = SPE_order, respvar = "esth_score", dat_cor = data_num, thr = 0.05)

# ----

# Model with selected species ----

last_mod <- lm(as.formula(paste("esth_score", "~", paste(final_sp, collapse = "+"))), data_num)
sum_last <- summary(last_mod)

# ----

# Extract the coefficients ----

eff_type      <- "Estimate"
effect        <- sum_last$coefficients[,eff_type]
names(effect) <- rownames(sum_last$coefficients)
effect        <- effect[-1]

# add the name of the species
codes  <- rownames(sum_last$coefficients)[-1]
names  <- phylo_table[which(phylo_table$ID_code %in% codes),c("ID_code","species")]

res         <- as.data.frame(sum_last$coefficients[-1,])
res$ID_code <- rownames(res)
final       <- merge(x = names, y = res, by = "ID_code")

app5_tablseS1           <- final[,c("ID_code", "species", "Estimate", "Std. Error")]
colnames(app5_tablseS1) <- c("Code", "Name", "Effect", "Standard_Error")
# ----

# Save for future use ----

write.csv(x = app5_tablseS1, file = hh("output", "06_AppendixG_TableG1.csv"), row.names = FALSE)

rm(data, data_num, last_mod, names, pervar, res, sum_last, codes, eff_type, effect, final_sp, 
   SPE_order, app5_tablseS1, data_rownames, data_slct, final, phylo_table, sp_names)

# ----

