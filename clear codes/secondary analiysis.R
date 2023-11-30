setwd("C:/R/data/50perc/secondary")

read.table("WSspecies.txt",header=T)->species
species.ins <- species[, 3:(dim(species)[2]-14)]

dim(species.ins)
dim(species)

species.ins[1:5,1:5]

top.50pc.function.insects(species.ins)
sum(species.ins>0)

#Rarefaction loop

n<-100                                                                                 #number of replications - can be changed

collapsed <- species.ins
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

plot(grand.out[,2]~rev(1:91),ylab="Total athropod species",xlab="Number of tree species")

plot(grand.out[,1]~rev(1:91),ylab="Arthropod species making up 50% abundance",xlab="Number of tree species",ylim=c(0,14))

plot(grand.out[,4]~rev(1:91),ylab="Total interactions",xlab="Number of tree species")

plot(grand.out[,3]~rev(1:91),ylab="Interactions making up 50% of interaction abundance",xlab="Number of tree species",ylim=c(0,16))

dim(grand.out)

secondary.out <- grand.out