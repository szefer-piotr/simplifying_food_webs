setwd("C:\\R\\data\\50perc")
Data <- read.table("BER.csv", header = TRUE, sep = ",", dec = ".")

summary(Data)

class(Data[3,3])

Data1 <- Data[,1]
Data1
len <- length(Data1)
Empty_row <- matrix(0, nrow = len)

Data2 <- data.frame(lapply(Data[,2:dim(Data)[2]], as.numeric), stringsAsFactors=FALSE)
Data3 <- data.frame(lapply(Data, as.numeric), stringsAsFactors=FALSE)

summary(Data2)

Data <- cbind(Empty_row, Data2)
Data

apply(Data[3,], 2, class)

row.names(Data2) <- Data1

New.data$SpNames <- Data1
New.data

Data <- t(Data2)
Data
top.50pc.function.insects(Data3)
sum(colSums(collapsed)>0)->total.sp.rich
total.sp.rich






n<-100                                                                                 #number of replications - can be changed

grand.out<-matrix(nrow=dim(Data3)[1],ncol=4,data=0) #create output matrix ready to receive inputs

for (i in 1:n){                                                                        #loop for replications
  out<-matrix(nrow=dim(Data3)[1],ncol=4,data=0)                                  #output for each replication
  top.50pc.function.insects(Data3)->out[1,]                                      #calculate arthropod species numbers for whole dataset
  Data3->Data3.temp                                                          #create temporary dataset that can be manipulated
  for (j in 2:dim(Data3.temp)[1]){                                               #loop for removal of randomly selected tree species
    Data3.temp[-sample(dim(Data3.temp)[1],1),]->Data3.temp             #remove a randomly selected tree species
    top.50pc.function.insects(Data3.temp)->out[j,]
    out  #re calculate arthropod numbers and add them to output
  }
  out+grand.out->grand.out                                                           #at the end of each of the n loops, add the most recent output to the running total and...
}
grand.out/n->grand.out 
grand.out

plot(grand.out[,2]~rev(1:8),ylab="Total athropod species",xlab="Number of sites")

plot(grand.out[,1]~rev(1:8),ylab="Arthropod species making up 50% abundance",xlab="Number of sites",ylim=c(0,3))

plot(grand.out[,4]~rev(1:8),ylab="Total interactions",xlab="Number of sites")

plot(grand.out[,3]~rev(1:8),ylab="Interactions making up 50% of interaction abundance",xlab="Number of sites",ylim=c(0,7))
