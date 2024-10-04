#_____________________________________________________________#
# Make Gexf files for Chronic Conditions
# 
# @Elma Hot Dervic
# September 2024
#_____________________________________________________________#



library(igraph)
library(dplyr)
library(rgexf)
library(stringr)


# Get all Chronic Conditions - 46 
# nodes in the network
Diagnoses_All <- read.csv("Chronic_All.csv")
str(Diagnoses_All)
num_diag <- 46


all_years <- seq(2003, 2014, by = 2 )
all_ages <- seq(1, 16, by = 2)

# predefined position of nodes
position <- readRDS("R-files/Chronic_position.rds")
position$id <- c(1:nrow(position))

# we have predefined colors for ICD chapters
# rbg colors
r <- c(26,88,150,180,255,88,26,26,242,26,242,242,180,26)
g <- c(242,242,29,26,202,26,242,149,118,57,26,26,242,242)
b <- c(57,26,26,242,1,242,149,242,26,242,26,211,26,242)


# *************** YEARS ***********************************************


# ************* MALE ******************


for(year in c(1:6)) {
  year_matched <- all_years[year]
  
  
  # NameCSV <- paste0("Data/3.AdjacencyMatrices/Adj_Matrix_Male_Chronic_year_", year_matched,"-", (year_matched+1), ".csv")
  # OR <- matrix(scan(NameCSV), ncol = num_diag, byrow = TRUE)
  
  
  OR <- readRDS( paste0("Data/3.AdjacencyMatrices/Adj_Matrix_Male_Chronic_year_", year_matched,"-", (year_matched+1), ".rds"))

  edges_m <- as.matrix(which(OR>1.5, arr.ind = T))
  edges <- NULL
  edges$Source <- Diagnoses_All$id[edges_m[,1]]
  edges$Target <- Diagnoses_All$id[edges_m[,2]]
  edges$weight <- OR[OR>1.5]
  str(edges)
  
  edges <- as.data.frame(edges)
  max(edges$weight)
  
  #_______SIZE_______
  # read data for all nodes for this sex and this year
  nodes_new <- readRDS(paste0("R-files/NodesSize/Chronic/Nodes_Size_male", year_matched, ".rds"))
  nodes_new <- merge(nodes_new,   Diagnoses_All, by = "id", all.x = T)
  str(nodes_new)
  # make size comparable for Females & Males in the year
  nodes_second_group <-  readRDS(paste0("R-files/NodesSize/Chronic/Nodes_Size_female", year_matched, ".rds"))
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
  nodes_new <- merge(nodes_new, position, by = "id")
  nodes_new$z <- rep(0, num_diag)
  nodes_new$X <- NULL
  nodes_positions <- nodes_new[, c("x", "y", "z")]
  
  
  # save nodes id and label in this data frame
  nodes <- data.frame(id = nodes_new$id, label = as.character(nodes_new$label))
  
  
 

  
  g1 <- write.gexf(nodes, edges[,c(1,2)], 
                   nodesVizAtt = list(color = node_colors, position = nodes_positions, size = nodes_new$size), 
                   edgesWeight = edges[,3],
                   nodesAtt = nodes_new[,c("id", "icd_code")] )
  
  f <- file(paste("Data/4.Graphs-gexffiles/Graph_Male_Chronic_Year_", year_matched, ".gexf", sep =  ""))
  writeLines(g1$graph, con = f)
  close(f)
  
  

}



# ************* FEMALE ******************


for(year in c(1:6)) {
  year_matched <- all_years[year]
  
  
  # NameCSV <- paste0("Data/3.AdjacencyMatrices/Adj_Matrix_Female_Chronic_year_", year_matched,"-", (year_matched+1), ".csv")
  # OR <- matrix(scan(NameCSV), ncol = num_diag, byrow = TRUE)
  
  
  OR <- readRDS( paste0("Data/3.AdjacencyMatrices/Adj_Matrix_Female_Chronic_year_", year_matched,"-", (year_matched+1), ".rds"))
  
  edges_m <- as.matrix(which(OR>1.5, arr.ind = T))
  edges <- NULL
  edges$Source <- Diagnoses_All$id[edges_m[,1]]
  edges$Target <- Diagnoses_All$id[edges_m[,2]]
  edges$weight <- OR[OR>1.5]
  str(edges)
  
  edges <- as.data.frame(edges)
  max(edges$weight)
  
  #_______SIZE_______
  # read data for all nodes for this sex and year
  nodes_new <- readRDS(paste0("R-files/NodesSize/Chronic/Nodes_Size_female", year_matched, ".rds"))
  nodes_new <- merge(nodes_new,   Diagnoses_All, by = "id", all.x = T)
  str(nodes_new)
  # make size comparable for Females & Males in the year
  
  nodes_second_group <-  readRDS(paste0("R-files/NodesSize/Chronic/Nodes_Size_male", year_matched, ".rds"))
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
  nodes_new <- merge(nodes_new, position, by = "id")
  nodes_new$z <- rep(0, num_diag)
  nodes_new$X <- NULL
  nodes_positions <- nodes_new[, c("x", "y", "z")]
  
  
  # save nodes id and label in this data frame
  nodes <- data.frame(id = nodes_new$id, label = as.character(nodes_new$label))
  
  
  

  
  g1 <- write.gexf(nodes, edges[,c(1,2)], 
                   nodesVizAtt = list(color = node_colors, position = nodes_positions, size = nodes_new$size), 
                   edgesWeight = edges[,3],
                   nodesAtt = nodes_new[,c("id", "icd_code")] )
  
  f <- file(paste("Data/4.Graphs-gexffiles/Graph_Female_Chronic_Year_", year_matched, ".gexf", sep =  ""))
  writeLines(g1$graph, con = f)
  close(f)
  
  
  
}




# *************** AGES ***********************************************

# SKIP 1st age group, chronic conditions - kids! 
# ************* MALE ******************

for(age in c(2:8)) {
  age_matched <- all_ages[age]
  
  
  # NameCSV <- paste0("Data/3.AdjacencyMatrices/Adj_Matrix_Male_Chronic_age_", age, ".csv")
  # OR <- matrix(scan(NameCSV), ncol = num_diag, byrow = TRUE)
  
  
  OR <- readRDS( paste0("Data/3.AdjacencyMatrices/Adj_Matrix_Male_Chronic_age_", age, ".rds"))
  
  edges_m <- as.matrix(which(OR>1.5, arr.ind = T))
  edges <- NULL
  edges$Source <- Diagnoses_All$id[edges_m[,1]]
  edges$Target <- Diagnoses_All$id[edges_m[,2]]
  edges$weight <- OR[OR>1.5]
  str(edges)
  
  edges <- as.data.frame(edges)
  max(edges$weight)
  
  #_______SIZE_______
  # read data for all nodes for this sex and this age groups
  nodes_new <- readRDS(paste0("R-files/NodesSize/Chronic/Nodes_Size_male", age_matched, ".rds"))
  nodes_new <- merge(nodes_new,   Diagnoses_All, by = "id", all.x = T)
  str(nodes_new)
  # make size comparable for Females & Males in the same group   
  nodes_second_group <-  readRDS(paste0("R-files/NodesSize/Chronic/Nodes_Size_female", age_matched, ".rds"))
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
  nodes_new <- merge(nodes_new, position, by = "id")
  nodes_new$z <- rep(0, num_diag)
  nodes_new$X <- NULL
  nodes_positions <- nodes_new[, c("x", "y", "z")]
  
  # save nodes id and label in this data frame
  nodes <- data.frame(id = nodes_new$id, label = as.character(nodes_new$label))
  
  

  
  g1 <- write.gexf(nodes, edges[,c(1,2)], 
                   nodesVizAtt = list(color = node_colors, position = nodes_positions, size = nodes_new$size), 
                   edgesWeight = edges[,3],
                   nodesAtt = nodes_new[,c("id", "icd_code")] )
  
  f <- file(paste("Data/4.Graphs-gexffiles/Graph_Male_Chronic_Age_", age, ".gexf", sep =  ""))
  writeLines(g1$graph, con = f)
  close(f)
  
  
  
}



# ************* FEMALE ******************


for(age in c(2:8)) {
  age_matched <- all_ages[age]
  
  # NameCSV <- paste0("Data/3.AdjacencyMatrices/Adj_Matrix_Female_Chronic_age_", age, ".csv")
  # OR <- matrix(scan(NameCSV), ncol = num_diag, byrow = TRUE)
  
  
  OR <- readRDS( paste0("Data/3.AdjacencyMatrices/Adj_Matrix_Female_Chronic_age_", age, ".rds"))
  
  edges_m <- as.matrix(which(OR>1.5, arr.ind = T))
  edges <- NULL
  edges$Source <- Diagnoses_All$id[edges_m[,1]]
  edges$Target <- Diagnoses_All$id[edges_m[,2]]
  edges$weight <- OR[OR>1.5]
  str(edges)
  
  edges <- as.data.frame(edges)
  max(edges$weight)
  
  #_______SIZE_______
  # read data for all nodes for this sex and this age groups
  nodes_new <- readRDS(paste0("R-files/NodesSize/Chronic/Nodes_Size_female", age_matched, ".rds"))
  nodes_new <- merge(nodes_new,   Diagnoses_All, by = "id", all.x = T)
  str(nodes_new)
  # make size comparable for Females & Males in the same group   
  nodes_second_group <-  readRDS(paste0("R-files/NodesSize/Chronic/Nodes_Size_male", age_matched, ".rds"))
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
  nodes_new <- merge(nodes_new, position, by = "id")
  nodes_new$z <- rep(0, num_diag)
  nodes_new$X <- NULL
  nodes_positions <- nodes_new[, c("x", "y", "z")]
  
  
  # save nodes id and label in this data frame
  nodes <- data.frame(id = nodes_new$id, label = as.character(nodes_new$label))

  
  
  
  g1 <- write.gexf(nodes, edges[,c(1,2)], 
                   nodesVizAtt = list(color = node_colors, position = nodes_positions, size = nodes_new$size), 
                   edgesWeight = edges[,3],
                   nodesAtt = nodes_new[,c("id", "icd_code")] )
  
  f <- file(paste("Data/4.Graphs-gexffiles/Graph_Female_Chronic_Age_", age, ".gexf", sep =  ""))
  writeLines(g1$graph, con = f)
  close(f)
  
  
  
}

  
