#Function calculating 50 perc of herbivores and 50perc of parasites involved

get.parasites <- function(names, dataset, n.plants){
  #Returns number of parasites involved in interactions with caterpillars, which names are given as a character vector
  #print(n.plants +1)
  #print(dim(dataset)[1])
  sub <- as.data.frame(dataset[(n.plants+1):dim(dataset)[1], names])
  #print(dim(sub))
  rownames(sub) <- dataset[(n.plants+1):dim(dataset)[1], 1]
  parasites.id <- row.names(sub[rowSums(sub)>0,])
  #print(parasites.id)
  number <- length(parasites.id)
  ret.list <- list(number = number, id = parasites.id)
  return(ret.list)
}
#for number of interactions/ not taking interaction strength into account
#top.arthropods.50.per <- function(dataset, n.plants){
 # n.interactions <- sum(dataset[,-1]>0) #computing nuber of interactions
  #half.interactions <- round(n.interactions * 0.50) #half of the interactions
  #endrow <- dim(dataset)[1]
  #caterpillars.degree <- colSums(dataset[,-1]>0)
  #caterpillars.degree <- as.matrix(caterpillars.degree)#ten i poprzedni wiersz mozna polaczyc
  #half.species <- sum((cumsum(sort(c(as.matrix(caterpillars.degree)),decreasing=TRUE))>half.interactions)==FALSE)+1
  #ord <- order(as.data.frame(caterpillars.degree), decreasing = TRUE) # Ordering in decreasing order
  #caterpillars.degree <- caterpillars.degree[ord,]
  #names <- names(caterpillars.degree)[1:half.species] #Return the names of half species
  #parasites.involved <- get.parasites(names, abundance.matrix, n.plants = 37)
  #results <- list(Half.herbivores = half.species, Half.parasites = parasites.involved$number)
  #return(results)
#}

#For abundance
top.arthropods.50.abu <- function(dataset, n.plants){
  n.abundance <- sum(dataset[1:n.plants,-1]) #computing total abundance of herbivore (based on interactions with plants)
  half.abundance <- round(n.abundance * 0.50) #half of the interactions
  endrow <- dim(dataset)[1]
  caterpillars.abundance <- colSums(dataset[1:n.plants,-1])
  caterpillars.species <- sum(colSums(dataset[1:n.plants, -1])>0)
  caterpillars.abundance <- as.matrix(caterpillars.abundance)#ten i poprzedni wiersz mozna polaczyc
  half.species <- sum((cumsum(sort(c(as.matrix(caterpillars.abundance)),decreasing=TRUE))>half.abundance)==FALSE)+1
  ord <- order(as.data.frame(caterpillars.abundance), decreasing = TRUE) # Ordering in decreasing order
  caterpillars.abundance <- caterpillars.abundance[ord,]
  names <- names(caterpillars.abundance)[1:half.species] #Return the names of half species
  #print(names)
  herbs <- colSums(dataset[1:n.plants,-1])
  herbs <- herbs[herbs>0]
  herbs <- names(herbs)
  print(length(herbs))
  parasites.species <- get.parasites(herbs, dataset, n.plants)$number
  #print(parasites.species)
  #parasites.species <- sum(rowSums(dataset[(n.plants+1):dim(dataset)[1], -1])>0)
  parasites.involved <- get.parasites(names, dataset, n.plants)
  results <- list(Half.herbivores = half.species, Half.parasites = parasites.involved$number, 
                  Herbivores = caterpillars.species, Parasites = parasites.species)
  return(results)
}

#test.matrix <- abundance.matrix[25:dim(abundance.matrix)[1], ]
#test.matrix[,1]

#columnSums <- colSums(test.matrix[,-1])
#columnSums <- columnSums[columnSums > 0]
#length(names(columnSums))
#top.arthropods.50.abu(test.matrix, 13)
#length
#length(names(columnSums>0))
#sum(columnSums>0)

#Loading the data
#setwd("C://R//data//50perc//testset")
#abundance.matrix <- read.csv("tri_trophic_matrix.csv", header = TRUE, sep = "\t")
#top.arthropods.50.per(abundance.matrix, 37)

setwd("C://R//data//50perc//tri trophic png//abundance and interaction matrix from equal sampling")
abundance.matrix <- read.csv("abundance matrix tri trophic.csv", header = TRUE, sep = "\t")
names(abundance.matrix)

setwd("C://R//data//50perc//tri trophic png//abundance and interaction matrix from 1ha")
abundance.matrix <- read.csv("tri trophic food webs from 1ha.csv", header = TRUE, sep = "\t")
names(abundance.matrix)
#Rarefaction loop


n<-100                                                                                 #number of replications - can be changed
n.plants <- 37

grand.out<-matrix(nrow=n.plants,ncol=4,data=0) #create output matrix ready to receive inputs
dim(grand.out)
for (i in 1:n){
  out<-matrix(nrow=n.plants,ncol=4,data=0)
  re <- top.arthropods.50.abu(abundance.matrix, n.plants)
  out[1,1] <- re$Half.herbivores
  out[1,2] <- re$Half.parasites
  out[1,3] <- re$Herbivores
  out[1,4] <- re$Parasites
  collapsed.temp <- abundance.matrix
  tree.counter <- n.plants
  #print("i is")
  #print(i)
  for (j in 2:n.plants){ #loop for removal of randomly selected tree species
    #print("J is")
    #print(j)
    collapsed.temp[-sample(tree.counter,1),]->collapsed.temp
    top <- top.arthropods.50.abu(collapsed.temp, tree.counter)
    out[j,1] <- top$Half.herbivores
    out[j,2] <- top$Half.parasites
    out[j,3] <- top$Herbivores
    out[j,4] <- top$Parasites
    tree.counter <- tree.counter - 1
  }
  out+grand.out->grand.out
}
grand.out/n->grand.out

equal <- grand.out
ha1 <- grand.out

windows(400,300)
par(mfrow = c(2,2))
plot(rev(1:37), equal[,3], ylim = c(0,300),
     ylab = "Number of arthropod species",
     xlab = "Tree species",
     pch = 2, col = "blue")
par(new = TRUE)
plot(rev(1:37), equal[,4], ylim=c(0, 300), axes = FALSE, col = "red", pch = 16,
     ylab = "",
     xlab = "")
plot(rev(1:37), equal[,1], ylim = c(0, 65),
     ylab = "Number of herbivores making up to 50 % of abundance and connected parasites",
     xlab = "Tree species",
     pch = 2, col = "blue")
par(new=TRUE)
plot(rev(1:37), equal[,2], ylim=c(0, 65), axes = FALSE, col = "red", pch = 16,
     ylab = "",
     xlab = "")
legend("topleft", legend = c("Herbivores", "Parasites connected with given herbivore species"),
       pch = c(2,16), col = c("blue", "red"))


plot(rev(1:37), ha1[,3], ylim = c(0,155),
     ylab = "Number of arthropod species",
     xlab = "Tree species",
     pch = 2, col = "blue")
par(new = TRUE)
plot(rev(1:37), ha1[,4], ylim=c(0, 155), axes = FALSE, col = "red", pch = 16,
     ylab = "",
     xlab = "")
plot(rev(1:37), ha1[,1], ylim = c(0, 30),
     ylab = "Number of herbivores making up to 50 % of abundance and connected parasites",
     xlab = "Tree species",
     pch = 2, col = "blue")
par(new=TRUE)
plot(rev(1:37), ha1[,2], ylim=c(0, 30), axes = FALSE, col = "red", pch = 16,
     ylab = "",
     xlab = "")
#legend("topleft", legend = c("Herbivores making up to 50% of abundance", "Parasites conected with reduced number of herbivores"),
       #pch = c(2,16), col = c("blue", "red"))
