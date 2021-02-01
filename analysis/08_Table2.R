###################################################################################################
#' Table 2: Coefficients and percentage of explained variance in the SEM model.
#'
#' This script produces the Langlois et al.'s 2021 paper Table 2, 
#' i.e. a table registering the percentage of explained variance for each variable included in 
#' the SEM (Structural Equation Model) and detail of the coefficients corresponding to each 
#' explaining variables.
#'
#' @author Juliette Langlois, \email{juliette.a.langlois@@gmail.com},
#'         Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#'         François Guilhaumon, \email{francois.guilhaumon@@ird.fr}
#'
#' @date 2021/01/12
##################################################################################################

# Load data ----

quadrat_table <- read.csv(hh("data", "quadrat_table.csv"))
pressures     <- read.csv(hh("output", "04_results_PCApressures.csv"))
load(hh("output", "05_hillnb_ses.RData"))

table <- merge(quadrat_table, pressures, by = "quadrat_code")
table <- merge(table, hillnb, by = "quadrat_code")

pal       <- "viridis"

# Save full table for future use
save(table, file = hh("output", "08_quadrat_pressure_hillses.RData"))

rm(quadrat_table, pressures, hillnb)

# ----

# Simplify table ----

# remove threats only keep the last axes of PCA, remove non SES PD and FD, remove unused context
keep  <- c("esth_score", "quadrat_code", "station", "site", "depth", "qTD", "SES_qPD", "SES_qFD", "Exploitation", "Anthropization" , "substrate_recouv")
table <- as.data.frame(table[,which(colnames(table) %in% keep)])

# Get the columns in the right order
table           <- table[, keep]
colnames(table) <- c("esth_score", "quadrat_code", "station", "site", "depth", "qTD", "SES_qPD", "SES_qFD", "Exploitation", "Anthropization","Sediment")

# Get characters and numbers instead of levels
if(is.character(table$quadrat_code) == FALSE){table$quadrat_code <- as.character(levels(table$quadrat_code))[table$quadrat_code]}
if(is.character(table$station) == FALSE){table$station           <- as.character(levels(table$station))[table$station]}
if(is.character(table$site) == FALSE){table$site                 <- as.character(levels(table$site))[table$site]}

rm(keep)

# ----

# Function to compute the models according to the graph ----  

#' test_indep
#' This function computes a lme model with the given variables and the metrics to evaluated if the
#'  hypothesis can be accepted or if it must be rejected
#' @param graph a character string begining by graph LR. Each link between two variables in the
#'  hypothetic model must be specied as explaining-->dependant with ";" as separator
#' @param df data frame containing the values of the variable included in the model
#' @param random_var list of the variables considered as random effect
#' @param random_str character string of type "~1|random_var1|random_var2"
#' @param ncores number of cores to use
#'
#' @return C        = metric to compare to the chi-square table
#'         DF       = 2 * number of degree of freedom of the model
#'         p-values = p-values of the independance claims in the tested hypothetic model. if the
#'         pvalue of Var1-Var2 is lower than 0.05 it means that the independance is unlikely to be.
#'         No info is given on the direction of the relation. To know which direction is the good 
#'         one, or its logical given the data or check the two directions. The one to keep is the
#'         one that gives the lowest C
#' @export
test_indep <- function(graph, df, random_var, random_str, ncores ) {
  
  # graph      = graph
  # df         = df
  # random_var = random_var
  # random_str = random_str
  # ncores     = 4
  
  # # fixed variables
  fixed    <- colnames(df)[- which(colnames(df) %in% random_var)]
  # code du graph
  code <-  gsub( "graph LR; ","" , graph)
  code <- strsplit(code, ";")[[1]]
  # Independent variables
  indep <- paste0(parallel::mclapply(1: length(fixed), function(i){ paste0(lapply(1 :length(fixed), function(j){
    if(j>i){ if(( length(grep(pattern = paste0(fixed[i],"-->",fixed[j]), code)) ==0 && 
                  length(grep(pattern = paste0(fixed[j],"-->",fixed[i]), code)) ==0) == TRUE ){
      paste0(fixed[i],"-",fixed[j]) }}
  }), collapse = ",")}, mc.cores = ncores), collapse = ",")
  indep <- unique(strsplit(indep, ",")[[1]])  
  indep <- indep[-which(indep == "NULL")]    
  
  # Control variable == those who are before the arrow going to one of the indep var
  indep_control        <- rbind.data.frame(lapply(indep, function(indep_couple){
    var                  <- strsplit(indep_couple,"-")[[1]]
    control              <- lapply(fixed, function(control_var){ if(length(grep(pattern = paste0(control_var,"-->",var[1]),code)) !=0|
                                                                    length(grep(pattern = paste0(control_var,"-->",var[2]), code)) !=0)
      control_var})
    control <- paste0(control[-which(control == "NULL")], collapse = ",")
  }))
  names(indep_control) <- indep
  
  # Compute models, extract proba and compute c = -2*sum(ln(pi))
  Y         <- lapply(1:ncol(indep_control), function(i){ strsplit(names(indep_control[i]), "-")[[1]][1] })
  X         <- lapply(1:ncol(indep_control), function(i){ strsplit(names(indep_control[i]), "-")[[1]][2] })
  
  for(i in 1:length(Y)){
    if (Y[[i]] == "depth"){Y[[i]] <- strsplit(names(indep_control[i]), "-")[[1]][2]}
    if (strsplit(names(indep_control[i]), "-")[[1]][1] == "depth"){X[[i]] <- strsplit(names(indep_control[i]), "-")[[1]][1] }
  }
  
  Xs        <- lapply(1:ncol(indep_control), function(i){ paste0(gsub(",","+", indep_control[,i]),"+",X[[i]] )})
  mod_chars <- lapply(1:length(Y), function(i){ paste0("nlme::lme(",Y[[i]],"~",Xs[[i]],", data = df, random = ", 
                                                       random_str,", na.action = na.omit, 
                                                       control = nlme::lmeControl(returnObject = TRUE))" )})
  mods      <- parallel::mclapply(mod_chars, function(m){ eval(parse(text = m))}, mc.cores = ncores)
  sums      <- lapply(mods, summary)  
  p         <- lapply(1:length(sums), function(i){ sums[[i]]$tTable[X[[i]],"p-value"]})
  names(p)  <- indep
  c         <- as.vector(lapply(p, function(pi){ log(pi) }))
  c         <- unlist(c, use.names = FALSE)
  c         <- (-2)*sum(c)
  k         <- length(p)
  DF        <- 2*k
  
  p         <- unlist(p, use.names = FALSE)
  p         <- cbind.data.frame(independent_variables = indep, proba = p)
  
  return_list <- list( c, DF, p)
  names(return_list) <- c("C", "DF", "p-values")
  return(return_list)
} 

# ----

# Create the table with relations ----

# must be a squared matrix
rel <- cbind(c(0,0,0,0,0,0,0,0), # impact of aesthetic on other variables
             c(0,0,0,0,0,1,1,1), # impact of depth
             c(1,0,0,1,1,0,0,0), # impact of qTD
             c(1,0,0,0,1,0,0,0), # impact of SES_qPD
             c(1,0,0,0,0,0,0,0), # impact of SES_qFD
             c(0,0,0,0,0,0,1,1), # impact of Exploitation (1st axis PCA)
             c(0,0,1,0,0,0,0,0), # impact of Anthropization (2nd axis PCA)
             c(1,0,1,1,1,0,0,0)) # impact of sediment coverage

colnames(rel) <- c("esth_score", "depth", "qTD", "SES_qPD", "SES_qFD", "Exploitation","Anthropization", "Sediment")
rownames(rel) <- colnames(rel)
# ----

# Create character string and mermaid graph ----  

decl  <- paste0(lapply(1:ncol(rel), function(i){
  y   <- rownames(rel)[i] 
  x   <- as.vector(names(which(rel[i,] ==1)))
  paste0(lapply(x, function(var){ paste0(var,"-->",y) }), collapse = "; ")
}), collapse = "; ")
graph <- paste0("graph LR; ",decl)

# DiagrammeR::mermaid(graph)
# graphics.off()

rm(decl)

# ----  

# Apply the function ----

# select the variables in table that are in the matrix of relationships
df         <- as.data.frame(table)
df         <- table[,which(colnames(table) %in% c("quadrat_code", "station", "site", colnames(rel)))]
# normalize numeric values
rownames(df)           <- df$quadrat_code
data_loc               <- df[, which(colnames(df) %in% c("quadrat_code", "station", "site"))]
df                     <- df[, -which(colnames(df) %in% c("quadrat_code", "station", "site"))]
data_norm              <- as.data.frame(rbind(apply(df, 2, normalize)))
data_norm$quadrat_code <- rownames(data_norm)
df                     <- merge(x = data_norm, y = data_loc, by = "quadrat_code")

# Set the random variables for pseudo replication
random_var <- c("site", "station", "quadrat_code")
random_str <- "~1|site/station/quadrat_code"

# apply the function
stat_model <- test_indep(graph, df, random_var, random_str, ncores = 4)  
chi2       <- cbind.data.frame(C = stat_model$C, DF = stat_model$DF)
p_values   <- as.matrix(stat_model$`p-values`)

rm(data_loc, data_norm, chi2, p_values, stat_model, table, graph, random_str, random_var)

# ----   

# Quantify interactions ---- 
# Write all the relations existing in the model ie the arrows
model <- lapply(1 : nrow(rel), function(i){
  if(length(colnames(rel)[which(rel[i,] ==1)] !=0)){ 
    temp <- paste0(colnames(rel)[which(rel[i,] ==1)], collapse = "+")
    paste0(gsub(" ", "", rownames(rel)[i]),"~",temp)
  } })
model    <- model[-which(model == "NULL")] 
model    <- paste0(model, collapse = ";") # here we have the list of all the lm composing the model
fit      <- lavaan::cfa(model, data = df) # cfa for Confirmatory Factor Analysis : fit the cfa to df
sum      <- lavaan::parameterEstimates(fit, rsquare = TRUE,
                                       se = TRUE, zstat = TRUE,
                                       pvalue = TRUE) 
# get the rsquared of all the relations between all the variables in the model
# Explaination of the return of lavaan::parameterEstimates
# lhs = the estimated parameter ; 
# op = the operator ("~" for regression ; "=~" can be read as "is measured by" for latent variables ; 
#      "~~" for variances and covariances ; "~1" for itercepts); 
# rhs = the parameter contributing to the estimated one; est = rsquared in this case;
# se = standard error ; z = z-values (default) ; p-value = pvalue corresponding to the z-statistic; 
# ci lower and ci upper = confidence interval bounds

rsquared           <- sum[which(sum$op == "r2"), c("lhs","est")] # % of explained variance of each variable
colnames(rsquared) <- c("variable", "rsquared")
sum                <- sum[-which(sum$op %in% c("~~", "r2")),] # keep only the regressions
from               <- as.character(sum$rhs)
to                 <- as.character(sum$lhs)
weight             <- sum$est
edges              <- cbind.data.frame(from, to, weight) 
# Here is the amout of variance explained individually by each variable and the sign of the contribution

# Combine the two tables into one
SEM_table             <- edges
SEM_table$percent_var <- NA
for(i in 1: nrow(rsquared)){
  SEM_table$percent_var[which(SEM_table$to == rsquared$variable[i])] <- rsquared$rsquared[i]
}
SEM_table <- SEM_table[,c("to", "from", "weight", "percent_var")]

# which explained variables have no dependancy?
SEM_table <- as.matrix(SEM_table)
indep_var <- setdiff(colnames(rel), SEM_table[, "to"]) # only depth is independant
add       <- c("depth", NA, NA, NA)
SEM_table <- rbind(SEM_table, add)

# rename columns and variables
colnames(SEM_table) <- c("Dependant Variable", "Explaining Variable", "Coefficient", "% of explained variance")
wrong               <- c("esth_score", "depth", "qTD", "SES_qPD", "SES_qFD", "Exploitation", "Anthropization" ,"Sediment")
right               <- c("Aesthetic Value", "Depth", "qTD", "qPD_SES", "qFD_SES", "Exploitation", "Anthropization" ,"Sediment")
for (i in 1: length(wrong)) {
  SEM_table[, "Explaining Variable"] <- gsub(wrong[i], right[i], SEM_table[, "Explaining Variable"])
  SEM_table[, "Dependant Variable"]  <- gsub(wrong[i], right[i], SEM_table[, "Dependant Variable"])
}

# Save
write.csv(x = SEM_table, file = hh("output", "08_Table2.csv"), row.names = FALSE)

rm(df, edges, fit, rel, rsquared, SEM_table, sum, add, from, i, indep_var, model, pal, right, to, wrong, 
   test_indep, weight)

#----
