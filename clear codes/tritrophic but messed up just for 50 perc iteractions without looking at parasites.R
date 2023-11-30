setwd("C://R//data//50perc//tri trophic png//abundance and interaction matrix from equal sampling")
abundance.matrix <- read.csv("abundance matrix tri trophic.csv", header = TRUE, sep = "\t")

the50perc.tritrophic <- function(dataset){
  abundance <- colSums(dataset)
  abundance <- sort(abundance, decreasing = TRUE)
  half.abundance <- sum(abundance)*0.5
  n.herbivores <- sum(colSums(dataset)>0) #All herbivore species
  n.arthropod.species <- sum((cumsum(abundance)>half.abundance)==FALSE)+1 #number of herbivores making up to 50% of abundance
  all.interactions <- sum(colSums(dataset>0))
  n.interactions <- sum((cumsum(sort(c(as.matrix(dataset)),decreasing=TRUE))>(half.abundance))==FALSE)+1
  res <- c(n.arthropod.species, n.herbivores,n.interactions,all.interactions)
  return(res)
}
n.plants <- 37

the50perc.tritrophic(abundance.matrix[,-1]) #function use matrix with one additional column with plant names
#abundance.matrix[,-1]->collapsed.temp #lets delete this one column
#row <- sample((n.plants),1) #sampling one row from range 1:number of plants
#collapsed.temp[-row, ]->collapsed.temp #reducing dataset (now number of plants is -1)
#collapsed.temp <- collapsed.temp[, colSums(collapsed.temp[1:36, ])>0] #removing herbivores which have no connections with plants
#the50perc.tritrophic(collapsed.temp)

n<-100 #number of replications - can be changed

grand.out<-matrix(nrow=n.plants,ncol=4,data=0) #create output matrix ready to receive inputs
parasites <- matrix(nrow = n.plants, ncol = 1, data=0)
for (i in 1:n){ #loop for replications
  out<-matrix(nrow=n.plants,ncol=4,data=0) #create an output matrix
  par.no <- matrix(nrow = n.plants, ncol = 1, data=0)
  the50perc.tritrophic(abundance.matrix[,-1])->out[1,] #first one without first collumn
  abundance.matrix[,-1]->collapsed.temp  #create temporary dataset that can be manipulated
  for (j in 2:n.plants){ #loop for removal of randomly selected tree species
    row = sample((n.plants - j +1),1)
    collapsed.temp[-row, ]->collapsed.temp #remove a randomly selected tree species
    #subset for non-zero herbivores in the plant herb part
    collapsed.temp <- collapsed.temp[, colSums(collapsed.temp[1:(n.plants-j+1), ])>0]
    print(dim(collapsed.temp[1:(n.plants-j+1), ]))
    the50perc.tritrophic(collapsed.temp)->out[j,] #re calculate arthropod numbers and add them to output
    par.no[j,1] <- how.many.parasite.connections(collapsed.temp, connections = out[j, 3], plants.no = (n.plants - j +1))
  }
  out+grand.out->grand.out
  parasites <- parasites + par.no#at the end of each of the n loops, add the most recent output to the running total and...
}
grand.out/n->grand.out
parasites/n->parasites

par(new=TRUE)
plot(parasites~rev(1:37), xlab = "", 
     ylab = "", axes = FALSE,
     ylim = c(0,31), col = "red")

plot(grand.out[,3]~rev(1:37), xlab = "Number of tree species", 
     ylab = "Number of interactions with making up to 50% of interactions",
     ylim = c(0,31))

windows(400,300)
par(mfrow=c(2,2)) 
plot(grand.out[,2]~rev(1:37),ylab="Total herbivore species",
     xlab="Number of tree species")
plot(grand.out[,1]~rev(1:37),ylab="Herbivore species making up 50% abundance",
     xlab="Number of tree species",ylim=c(0,18))
plot(grand.out[,4]~rev(1:37),ylab="Total interactions",
     xlab="Number of tree species")
plot(grand.out[,3]~rev(1:37),ylab="Interactions making up 50% of interaction abundance",
     xlab="Number of tree species",ylim=c(0,31))


#ANALYSIS FOR 1HA PLOTS (using trees becase there is no data about tree id)

setwd("C://R//data//50perc//tri trophic png//abundance and interaction matrix from 1ha")
abundance.matrix <- read.csv("tri trophic food webs from 1ha.csv", header = TRUE, sep = "\t")
n.plants <- 37
the50perc.tritrophic(abundance.matrix[,-1])
n<-100 #number of replications - can be changed
grand.out<-matrix(nrow=n.plants,ncol=4,data=0) #create output matrix ready to receive inputs
for (i in 1:n){ #loop for replications
  out<-matrix(nrow=n.plants,ncol=4,data=0) #create an output matrix
  the50perc.tritrophic(abundance.matrix[,-1])->out[1,] #first one without first collumn
  abundance.matrix[,-1]->collapsed.temp  #create temporary dataset that can be manipulated
  for (j in 2:n.plants){ #loop for removal of randomly selected tree species
    row = sample((n.plants - j +1),1)
    collapsed.temp[-row, ]->collapsed.temp #remove a randomly selected tree species
    #subset for non-zero herbivores in the plant herb part
    collapsed.temp <- collapsed.temp[, colSums(collapsed.temp[1:(n.plants-j+1), ])>0]
    print(dim(collapsed.temp[1:(n.plants-j+1), ]))
    the50perc.tritrophic(collapsed.temp)->out[j,] #re calculate arthropod numbers and add them to output
  }
  out+grand.out->grand.out #at the end of each of the n loops, add the most recent output to the running total and...
}
grand.out/n->grand.out
windows(400,300)
par(mfrow=c(2,2)) 
plot(grand.out[,2]~rev(1:37),ylab="Total herbivore species",
     xlab="Number of tree species")
plot(grand.out[,1]~rev(1:37),ylab="Herbivore species making up 50% abundance",
     xlab="Number of tree species",ylim=c(0,12))
plot(grand.out[,4]~rev(1:37),ylab="Total interactions",
     xlab="Number of tree species")
plot(grand.out[,3]~rev(1:37),ylab="Interactions making up 50% of interaction abundance",
     xlab="Number of tree species",ylim=c(0,18))










#Looking for associated parasites
#Count parasite interactions
n.plants
abundance.matrix[(n.plants+1):dim(abundance.matrix)[1], 1]



############################################################################################################


how.many.parasite.connections <- function(dataset, connections, plants.no){
  count <- 0
  for (i in 1:connections){
    value <- sort(as.matrix(dataset), decreasing = TRUE)[i]
    coordinates <- which(dataset == value, arr.ind = TRUE)
    #print(coordinates)
    if (coordinates[1] > plants.no) {
      count <- count + 1
      #print(plants.no)
      #print(count)
    } 
  }
  return(count)
}

how.many.parasite.connections(abundance.matrix[-1], 31, plants.no = 37)


#########################################################################################


subset.matrix <- abundance.matrix[6:25, 6:25]
i = 1
value <- sort(as.matrix(subset.matrix), decreasing = TRUE)[i]
coordinates <- which(subset.matrix == value, arr.ind = TRUE)
subset.matrix[coordinates[1], coordinates[2]]