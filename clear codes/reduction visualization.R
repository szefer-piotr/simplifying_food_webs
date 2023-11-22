# Animation of graph deacreasing in ordered fasion

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
# miners[miners>0] <- 1
colnames(miners) <- rep("M", dim(miners)[2])
rownames(miners) <- rep("P", dim(miners)[1])

# mobile.chewers[is.na(mobile.chewers)] <- 0
# mobile.chewers[mobile.chewers>0] <- 1
# colnames(mobile.chewers) <- rep("M", dim(mobile.chewers)[2])
# rownames(mobile.chewers) <- rep("P", dim(mobile.chewers)[1])
# 
# semi[is.na(semi)] <- 0
# semi[semi>0] <- 1
# colnames(semi) <- rep("M", dim(semi)[2])
# rownames(semi) <- rep("P", dim(semi)[1])

make_graph <- function(mat){
  # This function makes a square adjacency matrix, with weighted interactions (?)
  # form an abundance matrix
  
  mat1 <- matrix(0, nrow = dim(mat)[2], ncol = dim(mat)[2])
  
  #colnames(mat1) <- rep("M", dim(mat)[2])
  #rownames(mat1) <- rep("M", dim(mat)[2])
  
  # Different names
  colnames(mat1) <- paste("U", 1:dim(mat)[2], sep="")
  rownames(mat1) <- paste("U", 1:dim(mat)[2], sep="")
  
  
  mat2 <- t(mat)
  mat3 <- mat
  mat4 <- matrix(0, nrow = dim(mat)[1], ncol = dim(mat)[1])
  
  #colnames(mat4) <- rep("P", dim(mat)[1])
  #rownames(mat4) <- rep("P", dim(mat)[1])
  
  colnames(mat2) <- paste("L", 1:dim(mat)[1], sep="")
  rownames(mat3) <- paste("L", 1:dim(mat)[1], sep="")
  
  minUp <- cbind(mat1, mat2)
  minDown <- cbind(mat3, mat4) 
  minAdj <- rbind(minUp, minDown)
  return(minAdj)
}

plotGraph <- function(matrix){
  matAdj <- make_graph(matrix)
  matGraph <- graph_from_adjacency_matrix(matAdj, weighted = TRUE)
  colours <- rep(c("red", rgb(0,150,0, 155, maxColorValue = 255)), 
                 c(dim(matrix)[2], dim(matrix)[1]))
  matGraph <- delete.vertices(matGraph, which((degree(matGraph, mode="all")/2)==0))
  deg <- degree(matGraph, mode="all")/2
  #V(matGraph)$size <- deg
  
  plot(matGraph, edge.arrow.size=0, vertex.size = 5, vertex.color = colours,
       vertex.label = V(minGraph)$names, rescale = T,vertex.label.cex = 0.6)
}

plotGraph2 <- function(graph, lo, colours){
  # Figure out how to get colours
    plot(graph, edge.arrow.size=0, vertex.size = 5, vertex.color = colours,
       vertex.label = V(minGraph)$names, rescale = T,vertex.label.cex = 0.6,
       layout = lo)
}


##          ###
##          ###
##          ###
#####   ###########
####     #########
###       #######
##         #####
#           ###
             #

## New procedure
# 1. I will reduce the adjacency matrix and preserve the vertex coordinates
drop <- 2 # How many edges should be left at the end


matrix <- miners
minAdj <- make_graph(matrix) 
minGraph <- graph_from_adjacency_matrix(minAdj, weighted = TRUE)
coord <- layout_with_fr(minGraph)
colours <- rep(c("red", rgb(0,150,0, 155, maxColorValue = 255)), 
               c(dim(matrix)[2], dim(matrix)[1]))
upper <- dim(matrix)[2]
lower <- dim(matrix)[1]

# 2. The minimal value of upper/lower species row/column sum 
# is going to be set up to zero
setwd("C:/Users/Piotr Szefer/Desktop/Work/extrapolation/supplementary/reduction_visualization/parasitoidsL")

upperBool <- FALSE


if (upperBool){
  
  steps <- upper - drop
  png(file="example%02d.png", width=800, height=800)
  
  for (step in 1:steps){
    
    # Reduce the adjacency matrix
    cs <- colSums(minAdj[,1:upper])
    
    # Ordered removal
    edges <- which(cs == min(cs[cs>0]))
    chosenOne <- as.numeric(sample(c(edges,edges), 1))
    minAdj[,chosenOne] <- 0
    minAdj[chosenOne,] <- 0
    
    # Make a graph
    minGraph <- graph_from_adjacency_matrix(minAdj, weighted = TRUE)
    
    # Delete the nonconnected vertices
    deg <- degree(minGraph, mode="all")
    nonConVert <- which(deg==0)
    minGraph <- delete.vertices(minGraph, nonConVert)
    
    # And delete the same vertices from the coodinate list
    lo <- coord[-nonConVert,]
    color <- colours[-nonConVert]
    # Plot the graph with th enew layout
    plotGraph2(minGraph, lo, color)
    text(0.9,1, paste("Prop of upper", round(1-step/upper, digits = 3)))
  }
  dev.off()
} else {
  
  png(file="example%02d.png", width=800, height=800)
  steps <- lower - drop
  
  for (step in 1:steps){
    
    # Reduce the adjacency matrix
    cs <- colSums(minAdj[,(upper+1):(upper+lower)])
    edges <- which(cs == min(cs[cs>0]))
    chosenOne <- as.numeric(sample(c(edges,edges), 1)) + upper
    minAdj[,chosenOne] <- 0
    minAdj[chosenOne,] <- 0
    
    # Make a graph
    minGraph <- graph_from_adjacency_matrix(minAdj, weighted = TRUE)
    
    # Delete the nonconnected vertices
    deg <- degree(minGraph, mode="all")
    nonConVert <- which(deg==0)
    minGraph <- delete.vertices(minGraph, nonConVert)
    
    # And delete the same vertices from the coodinate list
    lo <- coord[-nonConVert,]
    color <- colours[-nonConVert]
    # Plot the graph with th enew layout
    plotGraph2(minGraph, lo, color)
    text(0.9,1, paste("Prop of upper", round(1-step/upper, digits = 3)))
  }
  dev.off()
}

# Creating a gif
system("magick -delay 50 *.png par_red.gif")
file.remove(list.files(pattern=".png"))




### Using the above code
## Simplify the parasitoids matrix
setwd("C:\\Users\\Piotr Szefer\\Desktop\\Work\\extrapolation\\datasets")
equal <- read.csv("ufwEqual.csv", header = TRUE, sep = "\t")
equal[is.na(equal)] <- 0
equal <- equal[, - c(1,2)]
set.seed(2)

order.and.clear <- function(dataset){
  #This function order the dataset based on rows and columns
  #and removes zero rows and columns. Dataset should not contain any NAs!
  dataset <- dataset[rowSums(dataset)>0,]
  dataset <- dataset[, colSums(dataset)>0]
  dataset <- dataset[order(rowSums(dataset), decreasing = TRUE), ]
  dataset <- dataset[,order(colSums(dataset), decreasing = TRUE)]
  return(dataset)
}

herb.all <- equal[,-2]
herb.eq <- as.matrix(rowSums(equal[3:dim(equal)[2]]) + equal[,1])
herb.eq <- as.matrix(herb.eq[rowSums(herb.eq)>0,])
par.eq <- as.matrix(colSums(equal[3:dim(equal)[2]]))
par.eq <- as.matrix(par.eq[rowSums(par.eq)>0,])


sum(rowSums(equal>0))
dim(equal)
ordCl <- order.and.clear(equal[3:dim(equal)[2]])
length(rowSums(ordCl))

matrix <- as.matrix(equal)
minAdj <- make_graph(matrix) 
minGraph <- graph_from_adjacency_matrix(minAdj, weighted = TRUE)
coord <- layout_with_fr(minGraph)
colours <- rep(c("red", rgb(0,150,0, 155, maxColorValue = 255)), 
               c(dim(matrix)[2], dim(matrix)[1]))

# 2. The minimal value of upper/lower species row/column sum 
# is going to be set up to zero

upperBool <- TRUE

setwd("C:/Users/Piotr Szefer/Desktop/Work/extrapolation/supplementary/reduction_visualization/parasitoids")

if (upperBool){
  steps <- upper - drop
  png(file="example%02d.png", width=800, height=800)
  initAbund <- sum(minAdj)/2
  for (step in 1:steps){
    
    # Reduce the adjacency matrix
    cs <- colSums(minAdj[,1:upper])
    edges <- which(cs == min(cs[cs>0]))
    print(paste ("Lowest abundance:", min(cs[cs>0]), sep=" "))
    print(edges)
    chosenOne <- as.numeric(sample(c(edges,edges), 1))
    minAdj[,chosenOne] <- 0
    minAdj[chosenOne,] <- 0
    # Make a graph
    minGraph <- graph_from_adjacency_matrix(minAdj, weighted = TRUE)
    
    # Delete the nonconnected vertices
    deg <- degree(minGraph, mode="all")
    nonConVert <- which(deg==0)
    minGraph <- delete.vertices(minGraph, nonConVert)
    
    # And delete the same vertices from the coodinate list
    lo <- coord[-nonConVert,]
    color <- colours[-nonConVert]
    # Plot the graph with th enew layout
    plotGraph2(minGraph, lo, color)
    text(0.9,1, paste("Prop of up. ab.", round((sum(minAdj)/2)/initAbund, digits = 3)))
    text(0.9,0.8, "Upper level removed")
  }
  dev.off()
} else {
  png(file="example%02d.png", width=800, height=800)
  steps <- lower - drop
  for (step in 1:steps){
    cs <- colSums(minAdj[,(upper+1):(upper+lower)])
    edges <- which(cs == min(cs[cs>0]))
    print(min(cs[cs>0]))
    print(edges)
    chosenOne <- as.numeric(sample(c(edges,edges), 1)) + upper
    minAdj[,chosenOne] <- 0
  }
}


