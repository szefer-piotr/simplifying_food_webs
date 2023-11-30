

### Do
### Spra
### Wdzenia


#ABUNDANCE FOR SECONDARY FOREST
setwd("C:/R/how simple are tropical food webs/50perc/secondary")
read.table("single trees secondary forest.csv",header=T, sep = "\t")->species
species <- species[,-dim(species)[2]]
species$subplot <- substr(species[,1], 1, 4)
mean(table(species$subplot)) # Its 48.72 tree individuals per plot
#Summing arthropod species per one subplot (one subplot per row)
subplots <- matrix(0, nrow = 25, ncol = (dim(species)[2] - 2)) #Creating an emtpty matrix for every subplot
colnames(subplots) <- names(species[, 2:(dim(species)[2]-1)]) # adding colum names
rownames(subplots) <- unique(species$subplot) # Adding row names
#Looping through all subplot names and summing in column all 533 species
for (i in 1:25){
  name <- unique(species$subplot)[i]
  temp <- species[species$subplot == name, ][2:(dim(species)[2]-1)]
  subplots[i,] <- colSums(temp)
}
sum(colSums(subplots)) == sum(colSums(species[,2:(dim(species)[2]-1)])) # Checking if any individual was not ommited
#Rarefaction trough subplots, but subplots obtained by colsum can be used only for abundance analysis.
#Function for 50pc abundance and interactions
top.50pc.function.insects<-function(species){
  sum(colSums(species)>0)->total.sp.rich                                                        #total number of species of insect in the dataset
  sum(colSums(species))*0.50->abundance                                                         #half the total abundance - need to know the minimum number of species to accumulate to get this
  sum((cumsum(sort(colSums(species),decreasing=TRUE))>abundance)==FALSE)+1->n.arthropod.species #this is the minimum number - note that the sum of individuals from these species will always be equal to or larger than "abundance", since unless the final species added brings the totals equal there will be an overshoot
  sum(c(as.matrix(species)))*0.50->int.abundance
  sum(species>0)->total.interactions
  sum((cumsum(sort(c(as.matrix(species)),decreasing=TRUE))>int.abundance)==FALSE)+1->n.interactions
  c(ifelse(total.sp.rich==0,0,n.arthropod.species),total.sp.rich,ifelse(total.interactions==0,0,n.interactions),total.interactions) #when the total species richness is zero, so is the 50% (code above does odd things when there are no species left)
}
n<-100                                                                                 #number of replications - can be changed
grand.out<-matrix(nrow=dim(subplots)[1],ncol=4,data=0) #create output matrix ready to receive inputs
subplots <- as.data.frame(subplots)
for (i in 1:n){                                                                        #loop for replications
  out<-matrix(nrow=dim(subplots)[1],ncol=4,data=0) #output for each replication
  top.50pc.function.insects(subplots)->out[1,] #calculate arthropod species numbers for whole dataset
  subplots->subplots.temp #create temporary dataset that can be manipulated
  for (j in 2:dim(subplots.temp)[1]){ #loop for removal of randomly selected tree species
    subplots.temp[-sample(dim(subplots.temp)[1],1),]->subplots.temp
    print(j)
    print(dim(subplots.temp)) #remove a randomly selected tree species
    top.50pc.function.insects(subplots.temp)->out[j,]                             #re calculate arthropod numbers and add them to output
  }
  out+grand.out->grand.out                                                           #at the end of each of the n loops, add the most recent output to the running total and...
}
grand.out/n->grand.out
abu.sec <- grand.out
abu.sec <- abu.sec[, 1:2]
abu.sec

#ABUNDANCE FOR SECONDARY FOREST

#Reading data
setwd("C:\\R\\how simple are tropical food webs\\")
read.table("Primary forest miner species.txt",header=T)->species
#Creating additional column named SUBPLOT wit 20x20m sublots codes
species$subplot <- substr(species[,1], 1, 4)
dim(table(species$subplot)) # it should have 25 elements
#Calculating average number of trees per subplot
mean(table(species$subplot)) # Its 55.12 tree individuals per plot
#Summing arthropod species per one subplot (one subplot per row)
subplots <- matrix(0, nrow = 25, ncol = (dim(species)[2] - 2)) #Creating an emtpty matrix for every subplot
colnames(subplots) <- names(species[, 2:(dim(species)[2]-1)]) # adding colum names
rownames(subplots) <- unique(species$subplot) # Adding row names
#Looping through all subplot names and summing in column all 533 species
for (i in 1:25){
  name <- unique(species$subplot)[i]
  temp <- species[species$subplot == name, ][2:(dim(species)[2]-1)]
  subplots[i,] <- colSums(temp)
}
sum(colSums(subplots)) == sum(colSums(species[,2:(dim(species)[2]-1)])) # Checking if any individual was not ommited
#Rarefaction trough subplots, but subplots obtained by colsum can be used only for abundance analysis.
#Function for 50pc abundance and interactions
top.50pc.function.insects<-function(species){
  sum(colSums(species)>0)->total.sp.rich                                                        #total number of species of insect in the dataset
  sum(colSums(species))*0.50->abundance                                                         #half the total abundance - need to know the minimum number of species to accumulate to get this
  sum((cumsum(sort(colSums(species),decreasing=TRUE))>abundance)==FALSE)+1->n.arthropod.species #this is the minimum number - note that the sum of individuals from these species will always be equal to or larger than "abundance", since unless the final species added brings the totals equal there will be an overshoot
  sum(c(as.matrix(species)))*0.50->int.abundance
  sum(species>0)->total.interactions
  sum((cumsum(sort(c(as.matrix(species)),decreasing=TRUE))>int.abundance)==FALSE)+1->n.interactions
  c(ifelse(total.sp.rich==0,0,n.arthropod.species),total.sp.rich,ifelse(total.interactions==0,0,n.interactions),total.interactions) #when the total species richness is zero, so is the 50% (code above does odd things when there are no species left)
}
top.50pc.function.insects(subplots)
top.50pc.function.insects(species[, 2:(dim(species)[2]-1)])
#Raraefying for abundance
n<-100                                                                                 #number of replications - can be changed
grand.out<-matrix(nrow=dim(subplots)[1],ncol=4,data=0) #create output matrix ready to receive inputs
subplots <- as.data.frame(subplots)
for (i in 1:n){                                                                        #loop for replications
  out<-matrix(nrow=dim(subplots)[1],ncol=4,data=0) #output for each replication
  top.50pc.function.insects(subplots)->out[1,] #calculate arthropod species numbers for whole dataset
  subplots->subplots.temp #create temporary dataset that can be manipulated
  for (j in 2:dim(subplots.temp)[1]){ #loop for removal of randomly selected tree species
    subplots.temp[-sample(dim(subplots.temp)[1],1),]->subplots.temp
    print(j)
    print(dim(subplots.temp)) #remove a randomly selected tree species
    top.50pc.function.insects(subplots.temp)->out[j,]                             #re calculate arthropod numbers and add them to output
  }
  out+grand.out->grand.out                                                           #at the end of each of the n loops, add the most recent output to the running total and...
}
grand.out/n->grand.out
abu.prim <- grand.out[,1:2]
abu.prim

#INTERACTION IN PRIMARY FOREST

setwd("C:/R/how simple are tropical food webs")
read.table("Primary forest miner species.txt",header=T)->species
read.table("Primary forest miner env.txt",header=T)->env

species$TreeID <- env$Species
species$subplot <- substr(species[,1], 1, 4)
species$subplot
names(species)

#Unique tree arthropod interactions for given dataset (number of plots need to be keep somewhere else)

simplify <- function(species){
  rowsum(species[,2:(dim(species)[2]-2)],species$TreeID)->data
  return(data)
}

inter <- simplify(species)

top.50pc.function.insects<-function(species){
  sum(colSums(species)>0)->total.sp.rich                                                        #total number of species of insect in the dataset
  sum(colSums(species))*0.50->abundance                                                         #half the total abundance - need to know the minimum number of species to accumulate to get this
  sum((cumsum(sort(colSums(species),decreasing=TRUE))>abundance)==FALSE)+1->n.arthropod.species #this is the minimum number - note that the sum of individuals from these species will always be equal to or larger than "abundance", since unless the final species added brings the totals equal there will be an overshoot
  
  ##part for looking at interactions
  
  sum(c(as.matrix(species)))*0.50->int.abundance
  sum(species>0)->total.interactions
  sum((cumsum(sort(c(as.matrix(species)),decreasing=TRUE))>int.abundance)==FALSE)+1->n.interactions
  c(ifelse(total.sp.rich==0,0,n.arthropod.species),total.sp.rich,ifelse(total.interactions==0,0,n.interactions),total.interactions) #when the total species richness is zero, so is the 50% (code above does odd things when there are no species left)
}

top.50pc.function.insects(inter)

# Now with modified for loop i will drop one random plot from dataset, simplify it and calculate 50pc of interactions

n<-100
inter.out<-matrix(nrow=25,ncol=4,data=0)
for (i in 1:n){
  out<-matrix(nrow=25,ncol=4,data=0) #matrix for results 25 subplots
  top.50pc.function.insects(inter)->out[1,] #first row of results is max from simpl data
  species -> species.temp #create a copy of full data
  for (j in 2:25){
    sample.sub <- sample(unique(species.temp$subplot), 1)
    species.temp <- species.temp[species.temp$subplot != sample.sub, ]
    sim.sub <- simplify(species.temp)
    top.50pc.function.insects(sim.sub)->out[j,]
  }
  out+inter.out->inter.out
}
inter.out/n->inter.out 
par(mfrow = c(1,2))
plot(inter.out[,4]~rev(1:25),ylab="Interactions",xlab="Number of 0.04ha plots")
plot(inter.out[,3]~rev(1:25),ylab="Interactions making up 50% of interaction abundance",xlab="Number of 0.04ha plots",ylim=c(0,16))

int.prim <- inter.out[,3:4]

#INTERACTIONS SECONDARY

setwd("C:/R/how simple are tropical food webs/50perc/secondary")
read.table("single trees secondary forest.csv",header=T, sep = "\t")->species

species$subplot <- substr(species[,1], 1, 4)
species$subplot
names(species)
#Unique tree arthropod interactions for given dataset (number of plots need to be keep somewhere else)
simplify <- function(species){
  rowsum(species[,2:(dim(species)[2]-2)],species$TreeID)->data
  return(data)
}
inter <- simplify(species)
top.50pc.function.insects<-function(species){
  sum(colSums(species)>0)->total.sp.rich                                                        #total number of species of insect in the dataset
  sum(colSums(species))*0.50->abundance                                                         #half the total abundance - need to know the minimum number of species to accumulate to get this
  sum((cumsum(sort(colSums(species),decreasing=TRUE))>abundance)==FALSE)+1->n.arthropod.species #this is the minimum number - note that the sum of individuals from these species will always b
  sum(c(as.matrix(species)))*0.50->int.abundance
  sum(species>0)->total.interactions
  sum((cumsum(sort(c(as.matrix(species)),decreasing=TRUE))>int.abundance)==FALSE)+1->n.interactions
  c(ifelse(total.sp.rich==0,0,n.arthropod.species),total.sp.rich,ifelse(total.interactions==0,0,n.interactions),total.interactions) #when the total species richness is zero, so is the 50% (code above does odd things when there are no species left)
}
top.50pc.function.insects(inter)
n<-100
inter.out<-matrix(nrow=25,ncol=4,data=0)
for (i in 1:n){
  out<-matrix(nrow=25,ncol=4,data=0) #matrix for results 25 subplots
  top.50pc.function.insects(inter)->out[1,] #first row of results is max from simpl data
  species -> species.temp #create a copy of full data
  for (j in 2:25){
    sample.sub <- sample(unique(species.temp$subplot), 1)
    species.temp <- species.temp[species.temp$subplot != sample.sub, ]
    sim.sub <- simplify(species.temp)
    top.50pc.function.insects(sim.sub)->out[j,]
  }
  out+inter.out->inter.out
}
inter.out/n->inter.out 
int.sec <- inter.out[,3:4]

#PLOTing

#int.prim
#int.sec
#abu.prim
#abu.sec
windows(400,300)
par(mfrow=c(2,2))

#plot for abundance and then interactions

plot(abu.prim[,2]~rev(1:25),ylab="Total athropod species",
     xlab="Number of 0.04ha plots",  ylim = c(0, 300),
     type = "l", lwd = 2)
par(new = TRUE)
plot(abu.sec[,2]~rev(1:25),ylab="",xlab="", 
     ylim = c(0, 300), type = "l", lwd = 2, lty = 2, col = "red")

plot(abu.prim[,1]~rev(1:25),
     ylab="Arthropod species making up 50% abundance",
     xlab="Number of 0.04ha plots", ylim = c(0,15),
     type = "l", lwd = 2)
par(new = TRUE)
plot(abu.sec[,1]~rev(1:25),ylab="",xlab="", 
     col="red", type = "l", lwd = 2, lty = 2, ylim = c(0,15))

#Interactions

plot(int.prim[,2]~rev(1:25),ylab="Total interactions",
     xlab="Number of 0.04ha plots",  ylim = c(0, 500),
     type = "l", lwd = 2)
par(new = TRUE)
plot(int.sec[,2]~rev(1:25),ylab="",xlab="", ylim = c(0, 500), 
     type = "l", lty = 2, lwd = 2, col = "red")

plot(int.prim[,1]~rev(1:25),
     ylab="Interactions making up 50% of interactions",
     xlab="Number of 0.04ha plots", ylim = c(0,16),
     type = "l", lwd = 2)
par(new = TRUE)
plot(int.sec[,1]~rev(1:25),ylab="",xlab="", 
     col="red", type = "l", lwd = 2, lty = 2, ylim = c(0,16))

#PERCENTAGE OF DROP

par(mfrow = c(1,1))

maximum <- c(int.prim[1,2],int.sec[1,2],abu.prim[1,2],abu.sec[1,2])
perc <- c(int.prim[1,1],int.sec[1,1],abu.prim[1,1],abu.sec[1,1])

names(maximum) <- c("IP", "IS", "SP", "SS" )
names(perc) <- c("IP", "IS", "SP", "SS" )

xcord <- barplot(maximum, ylab = "Number of species/interactions")
barplot(perc, add = TRUE, col = "black")

vals <- c(int.prim[1,1]/int.prim[1,2], int.sec[1,1]/int.sec[1,2],abu.prim[1,1]/abu.prim[1,2],abu.sec[1,1]/abu.sec[1,2])
vals <- round(vals*100, 2)
text(xcord, (perc + 25), vals)
