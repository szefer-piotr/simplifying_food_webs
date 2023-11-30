###################### FUNCTIONS FOR LOOKING AT THE NUMBER OF INSECT SPECIES REPRESENTED IN THE 50% OF INDIVIDUAL INSECTS MAKING UP THE MOST ABUNDANT SPECIES

#load data

setwd("C:/R/data")

read.table("Primary forest miner species.txt",header=T)->species

read.table("Primary forest miner env.txt",header=T)->env

##HOW MANY INSECT SPECIES MAKE UP THE 50% MOST ABUNDANT INSECT SPECIES, AND HOW DOES THIS CHANGE WITH PLANT SPECIES RICHNESS?

##looking at 50% abundance for insects

top.50pc.function.insects<-function(species){
	sum(colSums(species[,-1])>0)->total.sp.rich                                                        #total number of species of insect in the dataset
	sum(colSums(species[,-1]))*0.50->abundance                                                         #half the total abundance - need to know the minimum number of species to accumulate to get this
	sum((cumsum(sort(colSums(species[,-1]),decreasing=TRUE))>abundance)==FALSE)+1->n.arthropod.species #this is the minimum number - note that the sum of individuals from these species will always be equal to or larger than "abundance", since unless the final species added brings the totals equal there will be an overshoot
	
	##part for looking at interactions
	
	sum(c(as.matrix(species)))*0.50->int.abundance
	sum(species>0)->total.interactions
	sum((cumsum(sort(c(as.matrix(species)),decreasing=TRUE))>int.abundance)==FALSE)+1->n.interactions
	c(ifelse(total.sp.rich==0,0,n.arthropod.species),total.sp.rich,ifelse(total.interactions==0,0,n.interactions),total.interactions) #when the total species richness is zero, so is the 50% (code above does odd things when there are no species left)
	}

rowsum(species[,-1],env$Species)->collapsed #makes a new matrix with one tree species per row

top.50pc.function.insects(collapsed) #test - returns four values - first the 50% species number, the total number of species 100%, the 50% interaction number, and the total number of interactions 100%)

####### use the single function above to look accumulation of total arthropod species and %50 arthropod species with changing number of tree species

####loop for replicating rarefactions (n times) since removal of tree species is random, hence need for smoothing between runs

n<-100                                                                                 #number of replications - can be changed

grand.out<-matrix(nrow=dim(collapsed)[1],ncol=4,data=0) #create output matrix ready to receive inputs

for (i in 1:n){                                                                        #loop for replications
	out<-matrix(nrow=dim(collapsed)[1],ncol=4,data=0)                                  #output for each replication
	top.50pc.function.insects(collapsed)->out[1,]                                      #calculate arthropod species numbers for whole dataset
	collapsed->collapsed.temp                                                          #create temporary dataset that can be manipulated
	for (j in 2:dim(collapsed.temp)[1]){                                               #loop for removal of randomly selected tree species
		collapsed.temp[-sample(dim(collapsed.temp)[1],1),]->collapsed.temp
    print(j)
    print(dim(collapsed.temp))#remove a randomly selected tree species
		top.50pc.function.insects(collapsed.temp)->out[j,]                             #re calculate arthropod numbers and add them to output
		}
	out+grand.out->grand.out                                                           #at the end of each of the n loops, add the most recent output to the running total and...
	}
grand.out/n->grand.out                                                                 #divide by n to get a mean

par(mfrow=c(2,2))                                                                      #plot means (code above could be modified to give confidence intervals if needed)

plot(grand.out[,2]~rev(1:223),ylab="Total athropod species",xlab="Number of tree species")

plot(grand.out[,1]~rev(1:223),ylab="Arthropod species making up 50% abundance",xlab="Number of tree species",ylim=c(0,14))

plot(grand.out[,4]~rev(1:223),ylab="Total interactions",xlab="Number of tree species")

plot(grand.out[,3]~rev(1:223),ylab="Interactions making up 50% of interaction abundance",xlab="Number of tree species",ylim=c(0,16))

primary.out <- grand.out




##FOCUS INSTEAD ON TREES - HOW MANY INSECT SPECIES IN THE TOP 50% OF TREE SPECIES? MAYBE THIS IS NOT SO INTERESTING....

##function for taking a dataset comprising "species" and "env" and calculating 1. the total number of miner species present, 
##and 2. the total number of miner species present in the most abundant tree species making up the top 50% of total tree abundance

top.50pc.function.plants<-function(species,env){
	sum(colSums(species[,-1])>0)->total.sp.rich                                           #total number of species of insect in the dataset
	length(env$Species)*0.50->abundance                                                   #half of the total tree abundance
	sum((cumsum(summary(env$Species))>abundance)==FALSE)+1->n.species.included
	length(levels(env$Species))->tot.tree.richness
	summary(env$Species)[1:n.species.included]->top.50pc
	names(top.50pc)->to.include
	species[env$Species%in%to.include,]->top.50pc.insects
	sum(colSums(top.50pc.insects[,-1])>0)->top.50pc.sp.rich
	c(total.sp.rich,top.50pc.sp.rich,tot.tree.richness,n.species.included)->out
	names(out)<-c("Total arth richness","Top 50% arth richness","Total tree richness","Top 50% tree richness")
	out
	}

top.50pc.function.plants(species,env)                                                    #note that the input is of the original form "species" not the "collapsed" input


