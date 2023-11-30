#Plotting and computing function

rarefy <- function(guilds50perc, n = 100){
  grand.out<-matrix(nrow=dim(guilds50perc)[1],ncol=4,data=0) #create output matrix ready to receive inputs
  for (i in 1:n){ #loop for replications
    out<-matrix(nrow=dim(guilds50perc)[1],ncol=4,data=0) #output for each replication
    top.50pc.function.insects(guilds50perc)->out[1,] #calculate arthropod species numbers for whole dataset
    guilds50perc->guilds50perc.temp #create temporary dataset that can be manipulated&
    print(i/100)
    for (j in 2:dim(guilds50perc.temp)[1]){ #loop for removal of randomly selected tree species
      guilds50perc.temp[-sample(dim(guilds50perc.temp)[1],1),]->guilds50perc.temp #remove a randomly selected tree species
      top.50pc.function.insects(guilds50perc.temp)->out[j,] #re calculate arthropod numbers and add them to output
    }
    out+grand.out->grand.out #at the end of each of the n loops, add the most recent output to the running total and...
  }
    
  grand.out/n->grand.out  #divide by n to get a mean
  
  par(mfrow=c(2,2)) #plot means (code above could be modified to give confidence intervals if needed)
  plot(grand.out[,2]~rev(1:dim(guilds50perc)[1]),ylab="Total athropod species",xlab="Number of tree species")
  plot(grand.out[,1]~rev(1:dim(guilds50perc)[1]),ylab="Arthropod species making up 50% abundance",xlab="Number of tree species",)
  plot(grand.out[,4]~rev(1:dim(guilds50perc)[1]),ylab="Total interactions",xlab="Number of tree species")
  plot(grand.out[,3]~rev(1:dim(guilds50perc)[1]),ylab="Interactions making up 50% of interaction abundance",xlab="Number of tree species")
  return(grand.out)
}



