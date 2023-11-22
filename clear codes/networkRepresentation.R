#install.packages("igraph")
library(igraph)

#setwd("D:\\PRACA\\R\\how simple are tropical food webs")
setwd("C:\\Users\\Piotr Szefer\\Desktop\\Work\\extrapolation\\datasets")
guilds <- read.table("guilds.csv", header = TRUE, sep = "\t")
names <- as.character(guilds[,1])
names[5] <- "CHRY133a"
rownames(guilds) <- names
#Chunk of code for individual based rarefaction

miners <- t(guilds[guilds$Guild == unique(guilds$Guild)[3], 2:(dim(guilds)[2] - 6)])
mobile.chewers <- t(guilds[guilds$Guild == unique(guilds$Guild)[4], 2:(dim(guilds)[2] - 6)])
semi <- t(guilds[guilds$Guild == unique(guilds$Guild)[5], 2:(dim(guilds)[2] - 6)])

## Change all teh entries into absence presence.
miners[is.na(miners)] <- 0
miners[miners>0] <- 1
colnames(miners) <- rep("M", dim(miners)[2])
rownames(miners) <- rep("P", dim(miners)[1])

mobile.chewers[is.na(mobile.chewers)] <- 0
mobile.chewers[mobile.chewers>0] <- 1
colnames(mobile.chewers) <- rep("M", dim(mobile.chewers)[2])
rownames(mobile.chewers) <- rep("P", dim(mobile.chewers)[1])

semi[is.na(semi)] <- 0
semi[semi>0] <- 1
colnames(semi) <- rep("M", dim(semi)[2])
rownames(semi) <- rep("P", dim(semi)[1])

make_graph <- function(mat){
  # This function makes a square adjacency matrix, with weighted interactions (?)
  # form an abundance matrix
  mat1 <- matrix(0, nrow = dim(mat)[2], ncol = dim(mat)[2])
  colnames(mat1) <- rep("M", dim(mat)[2])
  rownames(mat1) <- rep("M", dim(mat)[2])
  mat2 <- t(mat)
  mat3 <- mat
  mat4 <- matrix(0, nrow = dim(mat)[1], ncol = dim(mat)[1])
  colnames(mat4) <- rep("P", dim(mat)[1])
  rownames(mat4) <- rep("P", dim(mat)[1])
  
  minUp <- cbind(mat1, mat2)
  minDown <- cbind(mat3, mat4) 
  minAdj <- rbind(minUp, minDown)
  return(minAdj)
}

minAdj <- make_graph(miners)
mobAdj <- make_graph(mobile.chewers)
semAdj <- make_graph(semi)

minGraph <- graph_from_adjacency_matrix(minAdj)
mobGraph <- graph_from_adjacency_matrix(mobAdj)
semGraph <- graph_from_adjacency_matrix(semAdj)


transitivity(minGraph)

names(V(minGraph))

# Miners graph
colours <- rep(c("red", rgb(0,150,0, 155, maxColorValue = 255)), c(dim(miners)[2], dim(miners)[1]))
length(colours)
plot(minGraph, edge.arrow.size=0, vertex.size=5, vertex.color = colours,
     vertex.label = NA)

# Mobile chewers graph
colours <- rep(c("red", rgb(0,150,0, 155, maxColorValue = 255)), c(dim(mobile.chewers)[2],
                                                                   dim(mobile.chewers)[1]))
length(colours)
plot(mobGraph, edge.arrow.size=0, vertex.size=5, vertex.color = colours,vertex.label = NA)

l <- layout_in_circle(mobGraph)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

grays <- rgb(150,150,150, 100, maxColorValue = 255)

plot(mobGraph,edge.arrow.size=0, vertex.size=5, vertex.label=NA, 
     edge.color=grays,
     vertex.color = colours, edge.width = 0.1)
transitivity(mobGraph)

# Semiconcealed graph

colours <- rep(c("red", rgb(0,150,0, 155, maxColorValue = 255)), c(dim(semi)[2], 
                                                                   dim(semi)[1]))
length(colours)
plot(semGraph, edge.arrow.size=0, vertex.size=5, vertex.color = colours,
     vertex.label = NA)
transitivity(semGraph)
cohesion(semGraph)

## What if I center the matrix entries
minAdj <- (minAdj - mean(minAdj))/(max(minAdj) - min(minAdj))

## Eigenvalues of the adjacency matrices (0,1)
minEig <- eigen(minAdj)
mobEig <- eigen(mobAdj)
semEig <- eigen(semAdj)

plot(Im(minEig$values)~Re(minEig$values))
plot(Im(semiEig$values)~Re(semiEig$values))
plot(Im(semiEig$values)~Re(semiEig$values))
