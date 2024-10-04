#_____________________________________________________________#
# Estimate some of network properties 
# 
# @Elma Hot Dervic
# September 2024
#_____________________________________________________________#



library(ggalluvial)
library(ggplot2)
library(cowplot)
library(ggpubr)
library(igraph)
library(dplyr)
library(stringr)
library(RColorBrewer)



# *************** Ages ***********************************************


# ************* MALE ******************


number_of_rows <- 16
properties <- data.frame(Age = rep(c(1:8),2),
                         sex = rep(NA, number_of_rows),
                         nodes = rep(NA, number_of_rows), 
                         degree = rep(NA, number_of_rows), 
                         avg_path_length = rep(NA, number_of_rows),
                         betweenness = rep(NA, number_of_rows),
                         centrality = rep(NA, number_of_rows),
                         closeness = rep(NA, number_of_rows),
                         density = rep(NA, number_of_rows),
                         modularity = rep(NA, number_of_rows)
)




# ************* MALE ******************



ii <- 1
for(age in 1:8){

  
  OR <- readRDS( paste0("Data/3.AdjacencyMatrices/Adj_Matrix_Male_ICD_age_", age, ".rds"))
  
  # Normalize the link weights
  for (i in 1:nrow( OR)) {
    # Calculate the sum of weights for the current node
    sum_weights <- sum( OR[i, ])
    
    # Avoid division by zero
    if (sum_weights > 0) {
      # Normalize the weights
       OR[i, ] <-  OR[i, ] / sum_weights
    }
  }
  
  
  graph <- graph_from_adjacency_matrix(OR, mode = "max",  weighted = T)
  properties$nodes[ii] <-  sum(colSums(OR)!=0 | rowSums(OR)!=0)
  x <- rowSums(OR!=0) # degree(graph)
  properties$degree[ii] <- mean(x[x!=0]) # mean(degree(graph))
  
  

  properties$avg_path_length[ii]  <- average.path.length(graph, directed = F)

  properties$betweenness[ii]  <- mean(betweenness(graph))
 
  
  x <- closeness(graph, mode = "total")
  x <- x[!is.nan(x)]
  properties$closeness[ii]  <- mean(x, nan.rm = T)

 
  properties$density[ii]  <-  edge_density(graph)
  
  
  properties$sex[ii]  <-  "Male"
  

  w <- cluster_walktrap(graph)
  properties$modularity[ii] <- modularity(w)
  
  
  ii <- ii + 1
  
}




str( properties)


#  ******************** FEMALE **************************



ii <- 9
for(age in 1:8){
  
  
  OR <- readRDS( paste0("Data/3.AdjacencyMatrices/Adj_Matrix_Female_ICD_age_", age, ".rds"))

  
  # Normalize the link weights
  for (i in 1:nrow( OR)) {
    # Calculate the sum of weights for the current node
    sum_weights <- sum( OR[i, ])
    
    # Avoid division by zero
    if (sum_weights > 0) {
      # Normalize the weights
       OR[i, ] <-  OR[i, ] / sum_weights
    }
  }
  
  
  graph <- graph_from_adjacency_matrix(OR, mode = "max",  weighted = T)
  properties$nodes[ii] <- sum(colSums(OR)!=0 | rowSums(OR)!=0)
  
  x <- rowSums(OR!=0) # degree(graph)
  properties$degree[ii] <-  mean(x[x!=0])
 
  
  properties$avg_path_length[ii]  <- average.path.length(graph, directed = F)

  properties$betweenness[ii]  <- mean(betweenness(graph))
  
  x <- closeness(graph, mode = "total")
  x <- x[!is.nan(x)]
  properties$closeness[ii]  <- mean(x, nan.rm = T)
 
 
  properties$density[ii]  <-  edge_density(graph)
  
  w <- cluster_walktrap(graph)
  properties$modularity[ii] <- modularity(w)
  
  
  properties$sex[ii]  <-  "Female"
  
  ii <- ii + 1

}




custom.col <- brewer.pal(n=12,"Paired")

#  "#A6CEE3" "#1F78B4" "#B2DF8A" "#33A02C" "#FB9A99" "#E31A1C"
# "#FDBF6F" "#FF7F00" "#CAB2D6" "#6A3D9A" "#FFFF99" "#B15928"


custom_labels <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79")


plot0 <- ggplot(properties, aes(x= Age, y = nodes, linetype = sex)) + 
  geom_line( alpha=0.7, colour = "#A6CEE3", size = 1.5) + 
  ylab("Number of \n connected nodes") + theme_bw(base_size=9*96/72) + xlab("") +
  theme(
    panel.background = element_rect(fill = NA),
    panel.grid = element_line(colour = "lightgrey", size=0.2),
    axis.line = element_line(size = 0.5, colour = "darkgrey"),
    panel.ontop = FALSE,
    panel.border = element_blank(),
    legend.title = element_blank(),
    strip.text.y = element_text(size = 12),
    axis.text = element_text( size = 12 ),
    axis.title = element_text(size = 12),
    legend.position = "none") +
  scale_x_continuous(breaks = 1:8, labels = custom_labels)  # Apply custom labels
    plot0
    
    
plot1 <- ggplot(properties, aes(x= Age, y = degree, linetype = sex)) + 
  geom_line( alpha=0.7, colour = "#1F78B4", size = 1.5)+ xlab("")  + scale_color_manual(values = custom.col[2]) + 
  ylab("Degree") + theme_bw(base_size=9*96/72) + xlab("") +
  theme(
    panel.background = element_rect(fill = NA),
    panel.grid = element_line(colour = "lightgrey", size=0.2),
    axis.line = element_line(size = 0.5, colour = "darkgrey"),
    panel.ontop = FALSE,
    panel.border = element_blank(),
    legend.title = element_blank(),
    strip.text.y = element_text(size = 12),
    axis.text.y = element_text( size = 12 ),
    axis.text.x = element_blank(),
    axis.title = element_text(size = 12),
    legend.position = "none")
plot1 



plot2 <- ggplot(properties, aes(x = Age, y = avg_path_length, linetype = sex)) + 
  geom_line( alpha=0.7, color = custom.col[4], size = 1.5)  + xlab("")  +
  ylab("Average \n path length") + theme_bw(base_size=9*96/72) + xlab("") +
  theme(
    panel.background = element_rect(fill = NA),
    panel.grid = element_line(colour = "lightgrey", size=0.2),
    axis.line = element_line(size = 0.5, colour = "darkgrey"),
    panel.ontop = FALSE,
    panel.border = element_blank(),
    legend.title = element_blank(),
    strip.text = element_text(size = 12),
    strip.text.y = element_text(size = 12),
    strip.text.x = element_text(size = 8),
    axis.text.y = element_text( size = 12 ),
    axis.text.x = element_blank(),
    axis.title = element_text(size = 12),
    legend.position = "none"
  ) 
plot2 



plot3 <- ggplot(properties, aes(x = Age, y = betweenness , linetype = sex)) + 
  geom_line(size = 1.5, color = custom.col[6], alpha=0.7)+ xlab("")  +
  ylab("Betweenness") + theme_bw(base_size=9*96/72) + xlab("") +
  theme(
    panel.background = element_rect(fill = NA),
    panel.grid = element_line(colour = "lightgrey", size=0.2),
    axis.line = element_line(size = 0.5, colour = "darkgrey"),
    panel.ontop = FALSE,
    panel.border = element_blank(),
    legend.title = element_blank(),
    strip.text = element_text(size = 12),
    strip.text.y = element_text(size = 12),
    strip.text.x = element_text(size = 8),
    axis.text.y = element_text( size = 12 ),
    axis.text.x = element_blank(),
    axis.title = element_text(size = 12),
    legend.position = "none"
  ) 

plot3

plot4 <- ggplot(properties, aes(x = Age, y = density, linetype = sex)) + 
  geom_line( size = 1.5, color = custom.col[7], alpha=0.7)+ xlab("")  +
  ylab("Density") + theme_bw(base_size=9*96/72) +
  theme(
    panel.background = element_rect(fill = NA),
    panel.grid = element_line(colour = "lightgrey", size=0.2),
    axis.line = element_line(size = 0.5, colour = "darkgrey"),
    panel.ontop = FALSE,
    panel.border = element_blank(),
    legend.title = element_blank(),
    strip.text = element_text(size = 12),
    strip.text.y = element_text(size = 12),
    strip.text.x = element_text(size = 8),
    axis.text.y = element_text( size = 12 ),
    axis.text.x = element_blank(),
    axis.title = element_text(size = 12),
    legend.position = "none"
  ) 

plot4



plot5 <- ggplot(properties, aes(x = Age, y = closeness,  linetype = sex)) + 
  geom_line(size = 1.5, colour = custom.col[8],  alpha=0.7)+ xlab("") + 
  ylab("Closeness") + theme_bw(base_size=9*96/72) + xlab("") +
  theme(
    panel.background = element_rect(fill = NA),
    panel.grid = element_line(colour = "lightgrey", size=0.2),
    axis.line = element_line(size = 0.5, colour = "darkgrey"),
    panel.ontop = FALSE,
    panel.border = element_blank(),
    legend.title = element_blank(),
    strip.text = element_text(size = 12),
    strip.text.y = element_text(size = 12),
    strip.text.x = element_text(size = 8),
    axis.text.y = element_text( size = 12 ),
    axis.text.x = element_text( size = 12, angle = 20,  hjust = 1),
    axis.title = element_text(size = 12),
    legend.position = "none"
  ) +   scale_x_continuous(breaks = 1:8, labels = custom_labels)  # Apply custom labels

plot5




plot6 <- ggplot(properties, aes(x = Age, y = modularity,  linetype = sex)) + 
  geom_line(size = 1.5, colour = custom.col[12],  alpha=0.7)+ xlab("") + 
  ylab("Modularity") + theme_bw(base_size=9*96/72) + xlab("Age Groups") +
  theme(
    panel.background = element_rect(fill = NA),
    panel.grid = element_line(colour = "lightgrey", size=0.2),
    axis.line = element_line(size = 0.5, colour = "darkgrey"),
    panel.ontop = FALSE,
    panel.border = element_blank(),
    legend.title = element_blank(),
    strip.text = element_text(size = 12),
    strip.text.y = element_text(size = 12),
    strip.text.x = element_text(size = 8),
    axis.text = element_text( size = 12 ),
    axis.title = element_text(size = 12),
    legend.position = "none",
    axis.text.x = element_text(angle=20, hjust = 1)
  )  + scale_x_continuous(breaks = 1:8, labels = custom_labels)  # Apply custom labels

plot6








p <- ggplot(properties, aes(x = Age, y = betweenness,  linetype = sex)) + 
  geom_line(size = 1.5, colour = "black",  alpha=0.7)+ xlab("") + 
  ylab("") + theme_bw(base_size=9*96/72) + xlab("Age Groups") +
  theme(
    panel.background = element_rect(fill = NA),
    panel.grid = element_line(colour = "lightgrey", size=0.2),
    axis.line = element_line(size = 0.5, colour = "darkgrey"),
    panel.ontop = FALSE,
    panel.border = element_blank(),
    legend.title = element_blank(),
    strip.text = element_text(size = 12),
    strip.text.y = element_text(size = 12),
    strip.text.x = element_text(size = 8),
    axis.text = element_text( size = 12 ),
    axis.title = element_text(size = 12),
    legend.position = "bottom",
    axis.text.x = element_text(angle=20, hjust = 1)
  )  + scale_x_continuous(breaks = 1:8, labels = custom_labels)  # Apply custom labels

p

# Extract the legend from one plot
legend <- get_legend(p + theme(legend.position = "bottom"))


figure <- ggarrange( plot0,
                     ggarrange(plot1, plot2, labels = c("B", "C"), ncol = 2, heights = c(1, 1)), 
                     ggarrange( plot3, plot4, labels = c("D", "E") , ncol = 2, heights = c(1, 1)),
                     ggarrange( plot5, plot6, labels = c("F", "G") , ncol = 2, heights = c(1, 1)), 
                     ncol = 1, labels = c("A", "", "", ""), heights = c(2,1, 1.1)) 



figure <- ggarrange( figure,  legend,
                     ncol = 1, heights = c(1, 0.05)) 

figure

name <- paste0("Plots/Network_properties.pdf")
ggsave(figure,  file = name,  width = 16*1.25, height = 16*1.25, units = "cm", dpi=300)
name <- paste0("Plots/Network_properties.png")
ggsave(figure,  file = name,  width = 16*1.25, height = 16*1.25, units = "cm", dpi=300, bg = "white")







