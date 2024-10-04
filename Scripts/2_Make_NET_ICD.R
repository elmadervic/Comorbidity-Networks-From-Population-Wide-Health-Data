#_____________________________________________________________#
# Make Gexf files for ICD10 Codes
# 
# @Elma Hot Dervic
# September 2024
#_____________________________________________________________#


library(igraph)
library(dplyr)
library(rgexf)
library(stringr)


# Get all ICD10 codes A00-N99 - 1080 codes
# nodes in the network
Diagnoses_All <- read.csv("ICD10_Diagnoses_All.csv")
str(Diagnoses_All)
num_diag <- 1080


all_years <- seq(2003, 2014, by = 2 )
all_ages <- seq(1, 16, by = 2)



# we have predefined colors for ICD chapters
# rbg colors
r <- c(26,88,150,180,255,88,26,26,242,26,242,242,180,26)
g <- c(242,242,29,26,202,26,242,149,118,57,26,26,242,242)
b <- c(57,26,26,242,1,242,149,242,26,242,26,211,26,242)



# predefined position of nodes
nameFile <- "R-files/NodesPosition1080.csv"
position <- read.csv(nameFile,  stringsAsFactors = F)


# in the data names of codes are in german
# here we load english names
nodes_name <- readRDS("ICD_eng.RDS")
nodes_name <- nodes_name[,c(2,4,5)]
nodes_name <- merge(Diagnoses_All, nodes_name, by.x = "icd_code", by.y = "Code", all.x = T)




# Sanitize node names
nodes_name$ShortDescription <- gsub("&", "&amp;", nodes_name$ShortDescription)
nodes_name$ShortDescription <- gsub("<", "&lt;", nodes_name$ShortDescription)
nodes_name$ShortDescription <- gsub(">", "&gt;", nodes_name$ShortDescription)
nodes_name$ShortDescription <- gsub("\"", "&quot;", nodes_name$ShortDescription)
nodes_name$ShortDescription <- gsub("'", "&apos;", nodes_name$ShortDescription)


# Sanitize node names
nodes_name$LongDescription <- gsub("&", "&amp;", nodes_name$LongDescription)
nodes_name$LongDescription <- gsub("<", "&lt;", nodes_name$LongDescription)
nodes_name$LongDescription <- gsub(">", "&gt;", nodes_name$LongDescription)
nodes_name$LongDescription <- gsub("\"", "&quot;", nodes_name$LongDescription)
nodes_name$LongDescription <- gsub("'", "&apos;", nodes_name$LongDescription)



# *************** YEARS ***********************************************


# ************* MALE ******************


for(year in c(1:6)) {
  
  year_matched <- all_years[year]
  
  
  # get matrix of Odds Ratios
  # NameCSV <- paste0("Data/3.AdjacencyMatrices/Adj_Matrix_Male_ICD_year_", year_matched,"-", (year_matched+1), ".csv")
  # OR <- matrix(scan(NameCSV), ncol = num_diag, byrow = TRUE)
  
  
  OR <- readRDS( paste0("Data/3.AdjacencyMatrices/Adj_Matrix_Male_ICD_year_", year_matched,"-", (year_matched+1), ".rds"))
  
  # make it data frame
  edges_m <- as.matrix(which(OR>1.5, arr.ind = T))
  edges <- NULL
  edges$Source <- edges_m[,1]
  edges$Target <- edges_m[,2]
  edges$weight <- OR[OR>1.5]
  str(edges)
  
  edges <- as.data.frame(edges)

  
  #_______SIZE_______
  
  # read estimates for size of all nodes for this sex and this year
  nodes_new <- readRDS(paste0("R-files/NodesSize/Nodes_Size_male", year_matched, ".rds"))
  nodes_new <- merge(nodes_new, Diagnoses_All, by ="icd_code", all.x = T)
  str(nodes_new)
  # make size comparable for Females & Males in the same group   
  nodes_second_group <-  readRDS(paste0("R-files/NodesSize/Nodes_Size_female", year_matched, ".rds"))
  nodes_new$size <- nodes_new$size/(max(c(nodes_new$size, nodes_second_group$size))-min(c(nodes_new$size, nodes_second_group$size)))*100
  # add to make smallest nodes visible
  nodes_new$size <-  nodes_new$size + 5
  
  
  #_______COLOR_______
  
  # make colors based on ICD Chapters - first letter of ICD code
  nodes_new$icd_class  <- str_trim(substring(nodes_new$icd_code,1,1))
  # # make class of node based on the fist letter of name
  a <- toupper(letters[1:14])
  nodes_new$icd_class <- match(nodes_new$icd_class, a)
  # date frame with column r g b = color for each node
  node_colors <- NULL
  node_colors <- data.frame(r = r[nodes_new$icd_class], g = g[nodes_new$icd_class], b = b[nodes_new$icd_class])
  # alpha is transparency - set to 1 
  node_colors$alpha <- rep(1, length(node_colors$r))
  
  
  #_______POSITION_______
  # add predefined positions
  nodes_new <- merge(nodes_new, position, by ="icd_code")
  nodes_new$z <- rep(0, num_diag)
  nodes_new$X <- NULL
  nodes_positions <- nodes_new[, c("x", "y", "z")]
  
  # save nodes id and label in this data frame
  nodes <- data.frame(id = nodes_new$diagnose_id, label = as.character(nodes_new$icd_code))
  
  
  
  
  # Make GRAPH
  
  g1 <- write.gexf(nodes, edges[,c(1,2)], 
                   nodesVizAtt = list(color = node_colors, position = nodes_positions, size = nodes_new$size), 
                   edgesWeight = edges[,3], 
                   nodesAtt = nodes_name )
  
  
  
  f <- file(paste("Data/4.Graphs-gexffiles/Graph_Male_ICD_Year_", year_matched, ".gexf", sep =  ""))
  writeLines(g1$graph, con = f)
  close(f)
  
  
  
}



# ************* FEMALE ******************


for(year in c(1:6)) {
  year_matched <- all_years[year]
  
  # get matrix of Odds Ratios
  # NameCSV <- paste0("Data/3.AdjacencyMatrices/Adj_Matrix_Female_ICD_year_", year_matched,"-", (year_matched+1), ".csv")
  # OR <- matrix(scan(NameCSV), ncol = num_diag, byrow = TRUE)
  
  
  OR <- readRDS( paste0("Data/3.AdjacencyMatrices/Adj_Matrix_Female_ICD_year_", year_matched,"-", (year_matched+1), ".rds"))
  
  # make it data frame
  edges_m <- as.matrix(which(OR>1.5, arr.ind = T))
  edges <- NULL
  edges$Source <- edges_m[,1]
  edges$Target <- edges_m[,2]
  edges$weight <- OR[OR>1.5]
  str(edges)
  edges <- as.data.frame(edges)

  
  
  #_______SIZE_______
  
  # read data for all nodes for this sex and this year
  nodes_new <- readRDS(paste0("R-files/NodesSize/Nodes_Size_female", year_matched, ".rds"))
  nodes_new <- merge(nodes_new, Diagnoses_All, by ="icd_code", all.x = T)
  str(nodes_new)
  # make size comparable for Females & Males in the same group   
  nodes_second_group <-  readRDS(paste0("R-files/NodesSize/Nodes_Size_male", year_matched, ".rds"))
  nodes_new$size <- nodes_new$size/(max(c(nodes_new$size, nodes_second_group$size))-min(c(nodes_new$size, nodes_second_group$size)))*100
  # add to make smallest nodes visible
  nodes_new$size <-  nodes_new$size + 5
  
  
  #_______COLOR_______
  nodes_new$icd_class  <- str_trim(substring(nodes_new$icd_code,1,1))
  # # make class of node based on the fist letter of name
  a <- toupper(letters[1:14])
  nodes_new$icd_class <- match(nodes_new$icd_class, a)
  # date frame with column r g b = color for each node
  node_colors <- NULL
  node_colors <- data.frame(r = r[nodes_new$icd_class], g = g[nodes_new$icd_class], b = b[nodes_new$icd_class])
  # alha is transparency - set to 1 
  node_colors$alpha <- rep(1, length(node_colors$r))
  
  
  
  
  #_______POSITION_______
  nodes_new <- merge(nodes_new, position, by ="icd_code")
  nodes_new$z <- rep(0, num_diag)
  nodes_new$X <- NULL
  # save nodes id and label in this data frame
  nodes <- data.frame(id = nodes_new$diagnose_id, label = as.character(nodes_new$icd_code))
  
  
  
  nodes_positions <- nodes_new[, c("x", "y", "z")]
  
   g1 <- write.gexf(nodes, edges[,c(1,2)], 
                   nodesVizAtt = list(color = node_colors, position = nodes_positions, size = nodes_new$size), 
                   edgesWeight = edges[,3], 
                   nodesAtt = nodes_name )
  
  
  f <- file(paste("Data/4.Graphs-gexffiles/Graph_Female_ICD_Year_", year_matched, ".gexf", sep =  ""))
  writeLines(g1$graph, con = f)
  close(f)
  
  
  
}




# *************** AGES ***********************************************


# ************* MALE ******************

for(age in c(1:8)) {
  
  age_matched <- all_ages[age]
  
  
  # get matrix of Odds Ratios
  # NameCSV <- paste0("Data/3.AdjacencyMatrices/Adj_Matrix_Male_ICD_age_", age, ".csv")
  # OR <- matrix(scan(NameCSV), ncol = num_diag, byrow = TRUE)
  
  
  OR <- readRDS( paste0("Data/3.AdjacencyMatrices/Adj_Matrix_Male_ICD_age_", age, ".rds"))
  
  
  # make it data frame
  edges_m <- as.matrix(which(OR>1.5, arr.ind = T))
  edges <- NULL
  edges$Source <- edges_m[,1]
  edges$Target <- edges_m[,2]
  edges$weight <- OR[OR>1.5]
  str(edges)
  
  edges <- as.data.frame(edges)
  max(edges$weight)
  
  
  #_______SIZE_______
  
  # read data for all nodes for this sex and this age groups
  nodes_new <- readRDS(paste0("R-files/NodesSize/Nodes_Size_male", age_matched, ".rds"))
  nodes_new <- merge(nodes_new, Diagnoses_All, by ="icd_code", all.x = T)
  str(nodes_new)
  # make size comparable for Females & Males in the same group   
  nodes_second_group <-  readRDS(paste0("R-files/NodesSize/Nodes_Size_female", age_matched, ".rds"))
  nodes_new$size <- nodes_new$size/(max(c(nodes_new$size, nodes_second_group$size))-min(c(nodes_new$size, nodes_second_group$size)))*100
  # add to make smallest nodes visible
  nodes_new$size <-  nodes_new$size + 5
  
  
  #_______COLOR_______
  nodes_new$icd_class  <- str_trim(substring(nodes_new$icd_code,1,1))
  # # make class of node based on the fist letter of name
  a <- toupper(letters[1:14])
  nodes_new$icd_class <- match(nodes_new$icd_class, a)
  # date frame with column r g b = color for each node
  node_colors <- NULL
  node_colors <- data.frame(r = r[nodes_new$icd_class], g = g[nodes_new$icd_class], b = b[nodes_new$icd_class])
  # alha is transparency - set to 1 
  node_colors$alpha <- rep(1, length(node_colors$r))
  
  
  
  #_______POSITION_______
  nodes_new <- merge(nodes_new, position, by ="icd_code")
  nodes_new$z <- rep(0, num_diag)
  nodes_new$X <- NULL
  # save nodes id and label in this data frame
  nodes <- data.frame(id = nodes_new$diagnose_id, label = as.character(nodes_new$icd_code))
  
  
  
  nodes_positions <- nodes_new[, c("x", "y", "z")]
  
  g1 <- write.gexf(nodes, edges[,c(1,2)], 
                   nodesVizAtt = list(color = node_colors, position = nodes_positions, size = nodes_new$size), 
                   edgesWeight = edges[,3], 
                   nodesAtt = nodes_name )
  
  f <- file(paste("Data/4.Graphs-gexffiles/Graph_Male_ICD_Age_", age, ".gexf", sep =  ""))
  writeLines(g1$graph, con = f)
  close(f)
  
  
  
}



# ************* FEMALE ******************


for(age in c(1:8)) {
  age_matched <- all_ages[age]
  
  
  # get matrix of Odds Ratios
  # NameCSV <- paste0("Data/3.AdjacencyMatrices/Adj_Matrix_Female_ICD_age_", age, ".csv")
  # OR <- matrix(scan(NameCSV), ncol = num_diag, byrow = TRUE)
  
  
  OR <- readRDS( paste0("Data/3.AdjacencyMatrices/Adj_Matrix_Female_ICD_age_", age, ".rds"))
  
  # make it data frame
  edges_m <- as.matrix(which(OR>1.5, arr.ind = T))
  edges <- NULL
  edges$Source <- edges_m[,1]
  edges$Target <- edges_m[,2]
  edges$weight <- OR[OR>1.5]
  str(edges)
  
  edges <- as.data.frame(edges)
  max(edges$weight)
  
  #_______SIZE_______
  
  # read data for all nodes for this sex and this age groups
  nodes_new <- readRDS(paste0("R-files/NodesSize/Nodes_Size_female", age_matched, ".rds"))
  nodes_new <- merge(nodes_new, Diagnoses_All, by ="icd_code", all.x = T)
  str(nodes_new)
  # make size comparable for Females & Males in the same group
  nodes_second_group <-  readRDS(paste0("R-files/NodesSize/Nodes_Size_male", age_matched, ".rds"))
  nodes_new$size <- nodes_new$size/(max(c(nodes_new$size, nodes_second_group$size))-min(c(nodes_new$size, nodes_second_group$size)))*100
  # add to make smallest nodes visible
   nodes_new$size <-  nodes_new$size + 5
  
  
   #_______COLOR_______
  nodes_new$icd_class  <- str_trim(substring(nodes_new$icd_code,1,1))
  # # make class of node based on the fist letter of name
  a <- toupper(letters[1:14])
  nodes_new$icd_class <- match(nodes_new$icd_class, a)
  # date frame with column r g b = color for each node
  node_colors <- NULL
  node_colors <- data.frame(r = r[nodes_new$icd_class], g = g[nodes_new$icd_class], b = b[nodes_new$icd_class])
  # alha is transparency - set to 1 
  node_colors$alpha <- rep(1, length(node_colors$r))
  
  
 
  #_______POSITION_______
  nodes_new <- merge(nodes_new, position, by ="icd_code")
  nodes_new$z <- rep(0, num_diag)
  nodes_new$X <- NULL
  
  nodes_new <-   nodes_new[order(  nodes_new$diagnose_id),]
  # save nodes id and label in this data frame
  nodes <- data.frame(diagnose_id = nodes_new$diagnose_id, label = as.character(nodes_new$icd_code))
  
  

  nodes_positions <- nodes_new[, c("x", "y", "z")]
  g1 <- write.gexf(nodes, edges[,c(1,2)], 
                   nodesVizAtt = list(color = node_colors, position = nodes_positions, size = nodes_new$size), 
                   edgesWeight = edges[,3], 
                   nodesAtt = nodes_name )
  
  f <- file(paste("Data/4.Graphs-gexffiles/Graph_Female_ICD_Age_", age, ".gexf", sep =  ""))
  writeLines(g1$graph, con = f)
  close(f)
  
  
  
}


