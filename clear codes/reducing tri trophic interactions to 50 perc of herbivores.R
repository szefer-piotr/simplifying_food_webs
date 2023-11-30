setwd("C://R//data//50perc//tri trophic png//abundance and interaction matrix from equal sampling")
abundance.matrix <- read.csv("abundance matrix tri trophic.csv", header = TRUE, sep = "\t")
names(abundance.matrix)
n.plants <- 37
abundance <- colSums(abundance.matrix[,-1])
abundance <- sort(abundance, decreasing = TRUE)
half.abundance <- sum(abundance)*0.5
#Number of all herbivores
n.herbivores <- sum(colSums(abundance.matrix[,-1])>0)
#Number of herbivore species making up to 50% of abundance
n.arthropod.species <- sum((cumsum(abundance)>half.abundance)==FALSE)+1 #number of herbivores making up to 50% of abundance
n.herbivores
n.arthropod.species

#INTERACTIONS
#All interactions
all.interactions <- sum(colSums(abundance.matrix[,-1]>0))
half.interactions <- round(all.interactions*0.5)
all.interactions
#Number of interactions making up to 50% of abundance
n.interactions <- sum((cumsum(sort(c(as.matrix(abundance.matrix[,-1])),decreasing=TRUE))>(half.abundance))==FALSE)+1
n.interactions 







# CHECK ALL AGAIN IT sHOULD BE NUMBER OF INTERACTION THAT MAKE 50% OF ALL INTERACTIONS

setwd("C://R//data//50perc//tri trophic png//abundance and interaction matrix from equal sampling")
interaction.matrix <- read.csv("interaction matrix tri trophic.csv", header = TRUE, sep = "\t")
abundance.matrix <- read.csv("abundance matrix tri trophic.csv", header = TRUE, sep = "\t")
dim(interaction.matrix)
dim(abundance.matrix)
#names(interaction.matrix)
n.plants <- 37
interactions <- colSums(interaction.matrix[,-1])
dim(interaction.matrix)
interactions <- sort(interactions, decreasing = TRUE) #sorted array of speciec with summed interactions(up and down the web)
half.interactions <- round(sum(interactions)*0.5) #half of the interactions 
n.arthropod.species.int <- sum((cumsum(interactions)>half.interactions)==FALSE)+1 #number of most connected herbivores

names.of.most.connected.herbivores <- names(interactions[1:n.arthropod.species.int])

# Counting how many interactions are with plants and how many are with parasites
subset.matrix.interactions <- abundance.matrix[, names.of.most.connected.herbivores]
connections.with.plants <- rowSums(subset.matrix[1:n.plants, ])
connections.with.parasites <- rowSums(subset.matrix)








#Using the names of most connected species i subset the abundance matrix to visualise results
subset.matrix.abundance <- abundance.matrix[, names.of.most.connected.herbivores]
rownames(subset.matrix.abundance) <- abundance.matrix[,1]
plants <- subset.matrix.abundance[1:n.plants, ]
parasites <- subset.matrix.abundance[(n.plants+1):dim(subset.matrix.abundance)[1], ]
s(subset.matrix.abundance)>0, ]
#plants
#plotting subset
dim(plants)
dim(parasites)
library(bipartite)
plotweb(parasites)
plotweb(parasites.full)
plotweb2(plants, parasites)

#And for full web

plants.full <- abundance.matrix[1:n.plants, -1]
rownames(plants.full) <- abundance.matrix[1:n.plants,1]
plants.full <- as.data.frame(t(plants.full))
plants.full[1:5,1:5]
class(plants.full)

parasites.full <- abundance.matrix[(n.plants+1):dim(abundance.matrix)[1], -1]
rownames(parasites.full) <- abundance.matrix[(n.plants+1):dim(abundance.matrix)[1], 1]
parasites.full[1:5,1:5]

plotweb2(parasites.full, plants.full, empty = TRUE)
?plotweb2