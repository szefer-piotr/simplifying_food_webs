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

int.prima <- inter.out[,3:4]







###SOME CODE
##################################################################################
##################################################################################
# Counting the interactions per plot per insect species.

#Summing arthropod interaction number per species per one subplot (one subplot per row)

sub.inter <- matrix(0, nrow = 25, ncol = (dim(species)[2] - 2)) #Creating an emtpty matrix for every subplot
colnames(sub.inter) <- names(species[, 2:(dim(species)[2]-1)]) # adding colum names
rownames(sub.inter) <- unique(species$subplot) # Adding row names

#Looping through all subplot names and summing in column all 533 species

for (i in 1:25){
  name <- unique(species$subplot)[i]
  temp <- species[species$subplot == name, ][2:(dim(species)[2]-1)]
  sub.inter[i,] <- colSums(temp>0)
  print(sub.inter[1:5, 1:5])
}

#Modified functio for 50 pc interaction and sum of iteraction from dataset

top.50pc.function.insects.MOD<-function(species){
  "For datasets with counted number of interactions per plot"
  sum(colSums(species))->total.sp.interactions #total number of species of insect in the dataset
  sum(colSums(species))*0.50->half.inter #half the total interactions - need to know the minimum number of plots to accumulate to get this
  sum((cumsum(sort(colSums(species),decreasing=TRUE))>half.inter)==FALSE)+1->n.arthropod.species 
  return(n.arthropod.species)
}

top.50pc.function.insects.MOD(sub.inter)
test <- matrix(c(3,0,1,0,7,5,14,0,1,0,1,0,5,15,2,1,1,15,25,4), nrow = 4, ncol = 5)
m <- matrix(0, nrow = 2, ncol = 1)
m[1] <- top.50pc.function.insects.MOD(test)
m

#Rarefaction

sub.inter <- as.data.frame(sub.inter)
sub.inter[1:5, 1:5]

n<-100                                                                                 #number of replications - can be changed

sub.inter.out<-matrix(nrow=dim(sub.inter)[1],ncol=1,data=0) #create output matrix ready to receive inputs
dim(sub.inter.out)

for (i in 1:n){                                                                        #loop for replications
  out<-matrix(nrow=dim(sub.inter)[1],ncol=1,data=0)                                  #output for each replication
  top.50pc.function.insects.MOD(sub.inter)->out[1,]                                      #calculate arthropod species numbers for whole dataset
  sub.inter->sub.inter.temp                                                          #create temporary dataset that can be manipulated
  for (j in 2:dim(sub.inter.temp)[1]){                                               #loop for removal of randomly selected tree species
    sub.inter.temp[-sample(dim(sub.inter.temp)[1],1),]->sub.inter.temp
    print(j)
    print(dim(sub.inter.temp))#remove a randomly selected tree species
    top.50pc.function.insects(sub.inter.temp)->out[j]                             #re calculate arthropod numbers and add them to output
  }
  out+sub.inter.out->sub.inter.out                                                           #at the end of each of the n loops, add the most recent output to the running total and...
}
sub.inter.out/n->sub.inter.out
sub.inter.out

##################################################################################
