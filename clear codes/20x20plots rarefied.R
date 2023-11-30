#Reading data
setwd("C://R//how simple are tropical food webs//analizy//How simple are tropical food webs email from tom fayle and vojta novotny")
read.table("Primary forest miner species.txt",header=T)->species
setwd("C://R//how simple are tropical food webs//50perc//secondary")
read.table("single trees secondary forest.csv",header=T, sep = "\t")->species
species <- as.data.frame(species)
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
  
  ##part for looking at interactions
  
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

abu.prim

#Column 3 and 4 are not accurate beacause in the dataset interactions are clumped to obtain 
#abundance, the dataset for interactions is created below.

top.50pc.function.insects(subplots)
top.50pc.function.insects(species[,2:534])
subplots.out <- grand.out

subplots.out

par(mfrow = c(1,2))

plot(subplots.out[,2]~rev(1:25),ylab="Total athropod species",xlab="Number of 0.04ha plots")

plot(subplots.out[,1]~rev(1:25),ylab="Arthropod species making up 50% abundance",xlab="Number of 0.04ha plots")

text(15,6, c("Mean nmber of trees per plot = 48.72"))



