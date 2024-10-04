#_____________________________________________________________#
#  Make Adjancy Matrix for Chronic
# 
# @Elma Hot Dervic
# September 2024
#
# INPUT:
# "Data/2.ContingencyTables/Chronic_ContingencyTables_Male_Final.rds"
# "Data/2.ContingencyTables/Chronic_ContingencyTables_Female_Final.rds"
#
# OUTPUTS:
# for Female & Males
# for time frames 2 years
# for all age groups
# paste0("Data/3.AdjacencyMatrices/Adj_Matrix_Male_Chronic_year_", year_matched,"-", (year_matched+1), ".csv")
# paste0("Data/3.AdjacencyMatrices/Adj_Matrix_Male_Chronic_year_", year_matched,"-", (year_matched+1), ".rds")
#_____________________________________________________________#

library(igraph)
library(dplyr)
library(rgexf)
library(stringr)




num_diag <- 46
all_years <- seq(2003, 2014, by = 2 )
all_ages <- seq(1, 16, by = 2)


diagnose <- read.csv("Chronic_All.csv")

# *************** YEARS ***********************************************


# ************* MALE ******************

CTables <- readRDS("Data/2.ContingencyTables/Chronic_ContingencyTables_Male_Final.rds")
str(CTables)

for(year in c(1:6)) {

  year_matched <- all_years[year]
  print(year_matched)
  P.value <- matrix(0, num_diag, num_diag)
  OR <- matrix(0, num_diag, num_diag)
  cases <- matrix(0, num_diag, num_diag)
  CI_1 <- matrix(0, num_diag, num_diag)
  CI_2 <- matrix(0, num_diag, num_diag)

for(i in 1:num_diag){
  for(k in 1:num_diag){
    data <- CTables [i,k,year,,]
    data <- data[data[,1]>5,]
    if(length(data)>4){
      if(nrow(data)>2){
        cases[i,k] <- sum(data[,1])
        test <- NULL
        test <- mantelhaen.test(array(as.vector(t(data)), dim = c(2, 2, nrow(data))), correct = TRUE) 
        P.value[i,k] <-  test$p.value
        OR[i,k] <- test$estimate
        CI_1[i,k] <- test$conf.int[1]
        CI_2[i,k] <- test$conf.int[2]
      }}
  }}

OR[is.na(OR)] <- 0
OR[is.nan(OR)] <- 0
OR[is.infinite(OR)] <- 0
OR[CI_1<=1] <- 0
OR[which(P.value>=0.05)] <- 0
diag(OR) <- 0
OR[which(cases < 100 )] <- 0
OR[OR<1.5] <- 0

write.table(OR, paste0("Data/3.AdjacencyMatrices/Adj_Matrix_Male_Chronic_year_", year_matched,"-", (year_matched+1), ".csv"), row.names = F, col.names = F)
saveRDS(OR, paste0("Data/3.AdjacencyMatrices/Adj_Matrix_Male_Chronic_year_", year_matched,"-", (year_matched+1), ".rds"))

}



# ************* FEMALE ******************

CTables <- readRDS("Data/2.ContingencyTables/Chronic_ContingencyTables_Female_Final.rds")
str(CTables)

for(year in c(1:6)) {
  
  year_matched <- all_years[year]
  print(year_matched)
  P.value <- matrix(0, num_diag, num_diag)
  OR <- matrix(0, num_diag, num_diag)
  cases <- matrix(0, num_diag, num_diag)
  CI_1 <- matrix(0, num_diag, num_diag)
  CI_2 <- matrix(0, num_diag, num_diag)
  
  for(i in 1:num_diag){
    for(k in 1:num_diag){
      data <- CTables [i,k,year,,]
      data <- data[data[,1]>5,]
      if(length(data)>4){
        if(nrow(data)>2){
          cases[i,k] <- sum(data[,1])
          test <- NULL
          test <- mantelhaen.test(array(as.vector(t(data)), dim = c(2, 2, nrow(data))), correct = TRUE) 
          P.value[i,k] <-  test$p.value
          OR[i,k] <- test$estimate
          CI_1[i,k] <- test$conf.int[1]
          CI_2[i,k] <- test$conf.int[2]
        }}
    }}
  
  OR[is.na(OR)] <- 0
  OR[is.nan(OR)] <- 0
  OR[is.infinite(OR)] <- 0
  OR[CI_1<=1] <- 0
  OR[which(P.value>=0.05)] <- 0
  diag(OR) <- 0
  OR[which(cases < 100 )] <- 0
  OR[OR<1.5] <- 0
  
  write.table(OR, paste0("Data/3.AdjacencyMatrices/Adj_Matrix_Female_Chronic_year_", year_matched,"-", (year_matched+1), ".csv"), row.names = F, col.names = F)
  saveRDS(OR, paste0("Data/3.AdjacencyMatrices/Adj_Matrix_Female_Chronic_year_", year_matched,"-", (year_matched+1), ".rds"))
  
}


# *************** AGE GROUPS ***********************************************


# ************* MALE ******************

CTables <- readRDS("Data/2.ContingencyTables/Chronic_ContingencyTables_Male_Final.rds")
str(CTables)

for(age in c(1:8)) {
  

  print(age)
  
  P.value <- matrix(0, num_diag, num_diag)
  OR <- matrix(0, num_diag, num_diag)
  cases <- matrix(0, num_diag, num_diag)
  CI_1 <- matrix(0, num_diag, num_diag)
  CI_2 <- matrix(0, num_diag, num_diag)
  
  for(i in 1:num_diag){
    for(k in 1:num_diag){
      data <- CTables [i,k,,age,]
      data <- data[data[,1]>5,]
      if(length(data)>4){
        if(nrow(data)>2){
          cases[i,k] <- sum(data[,1])
          test <- NULL
          test <- mantelhaen.test(array(as.vector(t(data)), dim = c(2, 2, nrow(data))), correct = TRUE) 
          P.value[i,k] <-  test$p.value
          OR[i,k] <- test$estimate
          CI_1[i,k] <- test$conf.int[1]
          CI_2[i,k] <- test$conf.int[2]
        }}
    }}
  
  OR[is.na(OR)] <- 0
  OR[is.nan(OR)] <- 0
  OR[is.infinite(OR)] <- 0
  OR[CI_1<=1] <- 0
  OR[which(P.value>=0.05)] <- 0
  diag(OR) <- 0
  OR[which(cases < 100 )] <- 0
  OR[OR<1.5] <- 0
  
  write.table(OR, paste0("Data/3.AdjacencyMatrices/Adj_Matrix_Male_Chronic_age_", age, ".csv"), row.names = F, col.names = F)
  saveRDS(OR, paste0("Data/3.AdjacencyMatrices/Adj_Matrix_Male_Chronic_age_", age, ".rds"))
  
}



# ************* FEMALE ******************

CTables <- readRDS("Data/2.ContingencyTables/Chronic_ContingencyTables_Female_Final.rds")
str(CTables)

for(age in c(1:8)) {
  
  print(age)
  
  P.value <- matrix(0, num_diag, num_diag)
  OR <- matrix(0, num_diag, num_diag)
  cases <- matrix(0, num_diag, num_diag)
  CI_1 <- matrix(0, num_diag, num_diag)
  CI_2 <- matrix(0, num_diag, num_diag)
  
  for(i in 1:num_diag){
    for(k in 1:num_diag){
      data <- CTables [i,k,,age,]
      data <- data[data[,1]>5,]
      if(length(data)>4){
        if(nrow(data)>2){
          cases[i,k] <- sum(data[,1])
          test <- NULL
          test <- mantelhaen.test(array(as.vector(t(data)), dim = c(2, 2, nrow(data))), correct = TRUE) 
          P.value[i,k] <-  test$p.value
          OR[i,k] <- test$estimate
          CI_1[i,k] <- test$conf.int[1]
          CI_2[i,k] <- test$conf.int[2]
        }}
    }}
  
  OR[is.na(OR)] <- 0
  OR[is.nan(OR)] <- 0
  OR[is.infinite(OR)] <- 0
  OR[CI_1<=1] <- 0
  OR[which(P.value>=0.05)] <- 0
  diag(OR) <- 0
  OR[which(cases < 100 )] <- 0
  OR[OR<1.5] <- 0
  
  write.table(OR, paste0("Data/3.AdjacencyMatrices/Adj_Matrix_Female_Chronic_age_", age, ".csv"), row.names = F, col.names = F)
  saveRDS(OR, paste0("Data/3.AdjacencyMatrices/Adj_Matrix_Female_Chronic_age_", age, ".rds"))
  
}


