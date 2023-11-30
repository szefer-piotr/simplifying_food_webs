setwd("C://R//data//50perc//tri trophic png//abundance and interaction matrix from equal sampling")
abundance.matrix <- read.csv("abundance matrix tri trophic.csv", header = TRUE, sep = "\t")
names(abundance.matrix)
n.plants <- 37


get.parasites <- function(names, dataset, n.plants){
  #Returns number of parasites involved in interactions with caterpillars, which names are given as a character vector
  sub <- dataset[(n.plants+1):dim(dataset)[1], names]
  rownames(sub) <- dataset[(n.plants+1):dim(dataset)[1], 1]
  parasites.id <- unique(row.names(sub[rowSums(sub)>0,]))
  number <- length(parasites.id)
  ret.list <- list(number = number, id = parasites.id)
  return(ret.list)
}


#Creating a matrix witch names of herbivores that are most abundant
abundance <- colSums(abundance.matrix[,-1])
abundance <- sort(abundance, decreasing = TRUE)
#Total abundance of herbivores
tot.abu <- sum(abundance)
half.abu <- round(tot.abu*0.5)
#How many herbivores are 50pc of abundance?
half.n.herbivores <- sum((cumsum(abundance)>half.abu)==FALSE)+1

parasites <- get.parasites(names(abundance[1:half.n.herbivores]), abundance.matrix, n.plants = n.plants)
parasites$number
