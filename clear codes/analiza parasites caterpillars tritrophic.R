#Loading the data
setwd("C://R//data//50perc//testset")
setwd("C://R//how simple are tropical food webs//50perc//testset")
abundance.matrix <- read.csv("tri_trophic_matrix.csv", header = TRUE, sep = "\t")

#Number of interactions in the web
n.interactions <- sum(abundance.matrix[,-1]>0)
half.interactions <- round(n.interactions * 0.50)

#Number of plants in the dataset (number of top rows with plants)
n.plants <- 37

#last row of the dataset
endrow <- dim(abundance.matrix)[1]

#Number of parasite species
n.parasites <- length(abundance.matrix[(n.plants+1):endrow, 1])

# Computing number of arthropods needed to preserve 50% of all interactions.
# First the degree distribution for caterpillars
caterpillars.degree <- colSums(abundance.matrix[,-1]>0)
caterpillars.degree <- as.matrix(caterpillars.degree)
half.species <- sum((cumsum(sort(c(as.matrix(caterpillars.degree)),decreasing=TRUE))>half.interactions)==FALSE)+1

# Ordering in decreasing order
ord <- order(as.data.frame(caterpillars.degree), decreasing = TRUE)
caterpillars.degree <- caterpillars.degree[ord,]

#Return the names of half species
names <- names(caterpillars.degree)[1:half.species]

#Function get.parasites should give the number of parasite species involved in interacitons with given set of caterpillars

get.parasites <- function(names, dataset, n.plants){
  #Returns number of parasites involved in interactions with caterpillars, which names are given as a character vector
  sub <- dataset[(n.plants+1):dim(dataset)[1], names]
  rownames(sub) <- dataset[(n.plants+1):dim(dataset)[1], 1]
  parasites.id <- unique(row.names(sub[rowSums(sub)>0,]))
  number <- length(parasites.id)
  ret.list <- list(number = number, id = parasites.id)
  return(ret.list)
}

parasites.involved <- get.parasites(names, abundance.matrix, n.plants = 37)
parasites.involved

