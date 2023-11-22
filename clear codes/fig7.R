# 8 sites 4 species

###################### FUNCTIONS FOR LOOKING AT THE NUMBER OF INSECT SPECIES REPRESENTED IN THE 50% OF INDIVIDUAL INSECTS MAKING UP THE MOST ABUNDANT SPECIES

#load data

setwd("C:/R/how simple are tropical food webs/50perc/")

read.table("BER.csv",header=T, sep = ",") -> ber
read.table("COP.csv",header=T, sep = ",") -> cop
read.table("MAA.csv",header=T, sep = ",") -> maa
read.table("SYW.csv",header=T, sep = ",") -> syw
#sum(colSums(species[,-1])>0)
#read.table("Primary forest miner env.txt",header=T)->env

##HOW MANY INSECT SPECIES MAKE UP THE 50% MOST ABUNDANT INSECT SPECIES, AND HOW DOES THIS CHANGE WITH PLANT SPECIES RICHNESS?

##looking at 50% abundance for insects

collapsed.ber <- ber[, 2:dim(ber)[2]]
collapsed.cop <- cop[, 2:dim(cop)[2]]
collapsed.maa <- maa[, 2:dim(maa)[2]]
collapsed.syw <- syw[, 2:dim(syw)[2]]

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

#rowsum(species[,-1],env$Species)->collapsed #makes a new matrix with one tree species per row

top.50pc.function.insects(collapsed.ber) #test - returns four values - first the 50% species number, the total number of species 100%, the 50% interaction number, and the total number of interactions 100%)

####### use the single function above to look accumulation of total arthropod species and %50 arthropod species with changing number of tree species

####loop for replicating rarefactions (n times) since removal of tree species is random, hence need for smoothing between runs

n<-100                                                                                 #number of replications - can be changed

#ber.mat <- matrix(nrow=dim(collapsed.ber)[1],ncol=4,data=0) #create output matrix ready to receive inputs
#cop.mat <- matrix(nrow=dim(collapsed.cop)[1],ncol=4,data=0)
#maa.mat <- matrix(nrow=dim(collapsed.maa)[1],ncol=4,data=0)
#syw.mat <- matrix(nrow=dim(collapsed.syw)[1],ncol=4,data=0)

loop <- function(collapsed) {
  grand.out <- ber.mat <- matrix(nrow=dim(collapsed)[1],ncol=4,data=0)
  for (i in 1:n){                                                                        #loop for replications
    out<-matrix(nrow=dim(collapsed)[1],ncol=4,data=0)                                  #output for each replication
    top.50pc.function.insects(collapsed)->out[1,]                                      #calculate arthropod species numbers for whole dataset
    collapsed->collapsed.temp                                                          #create temporary dataset that can be manipulated
    for (j in 2:dim(collapsed.temp)[1]){                                               #loop for removal of randomly selected tree species
      collapsed.temp[-sample(dim(collapsed.temp)[1],1),]->collapsed.temp
      top.50pc.function.insects(collapsed.temp)->out[j,]                             #re calculate arthropod numbers and add them to output
    }
    out+grand.out->grand.out                                                           #at the end of each of the n loops, add the most recent output to the running total and...
  }
  return(grand.out/n->grand.out)
}

ber.out <- loop(collapsed.ber)
cop.out <- loop(collapsed.cop)
maa.out <- loop(collapsed.maa)
syw.out <- loop(collapsed.syw)

# col 2 is total arthropods species,
# col 1 is 50 of species
ber.out
cop.out
maa.out
syw.out

# NEW PLOT

red <- rgb(150,  0,  0,200,maxColorValue = 255) 
grn <- rgb(0  ,150,  0,200,maxColorValue = 255)
blu <- rgb(0  ,  0,150,200,maxColorValue = 255)
bck <- rgb(0  ,  0,  0,200,maxColorValue = 255)
colours <- c(red,grn,blu,bck)
datasets <- list(ber.out,
                 cop.out,
                 maa.out,
                 syw.out)

setwd("C:/R/how simple are tropical food webs")
pdf("fig7.pdf", height = 5, width=10)
ys <- c(datasets[[1]][,2],
          datasets[[2]][,2],
          datasets[[3]][,2],
          datasets[[4]][,2])
ylim <- c(min(ys), max(ys))
xlim <- c(1,8)
names <- c("BER","COP","MAA", "SYW")

par(mfrow=c(1,2))
plot(0, 
     xlim=xlim, ylim=ylim,
     xlab="Number of sites", ylab="Total arthropod species",
     cex.axis=0.9,cex.lab=0.9,cex.main=0.7,
     mgp=c(2,1,0))

for (i in 1:4){
  par(new=TRUE)
  plot(datasets[[i]][,2]~rev(1:8),cex=1.5,pch=16,
       col=colours[i],xlim=xlim,ylim=ylim,
       xlab="", ylab="", axes=F,type = "b")
  #text(2,datasets[[i]][6,2] ,names[i])
}


legend("topleft", col=colours, c("BER","COP","MAA", "SYW"), 
       pch=16, pt.cex = 1.5, cex=0.8)

ys <- c(datasets[[1]][,1],
        datasets[[2]][,1],
        datasets[[3]][,1],
        datasets[[4]][,1])
ylim <- c(min(ys), max(ys))

plot(0,xlim=xlim, ylim=ylim,
     xlab="Number of sites", ylab="Arthropod species making up 50% of abundance",
     cex.axis=0.9,cex.lab=0.9,cex.main=0.7,
     mgp=c(2,1,0))

for (i in 1:4){
  par(new=TRUE)
  plot(datasets[[i]][,1]~rev(1:8),cex=1.5,pch=16,
       col=colours[i],xlim=xlim,ylim=ylim,
       xlab="", ylab="", axes=F,type = "b")
}
dev.off()



# PLOTPLOTPLOT

windows(width=10, height=8)
par(mfrow=c(2,2))
plot(ber.out[,2]~rev(1:8),ylab="Total athropod species",
     xlab="Number of sites", col = "black", type = "l", lty = 1, lwd = 1,
     ylim = c(0,100))
par(new=TRUE)
plot(cop.out[,2]~rev(1:8),ylab="",xlab="", axes = FALSE,
     type = "l", lty = 2, lwd = 1,
     ylim = c(0,100))
par(new=TRUE)
plot(maa.out[,2]~rev(1:8),ylab="",xlab="", axes = FALSE,
     type = "l", lty = 1, lwd = 2,
     ylim = c(0,100))
par(new=TRUE)
plot(syw.out[,2]~rev(1:8),ylab="",xlab="", axes = FALSE,
     type = "l", lty = 2, lwd = 2,
     ylim = c(0,100))
#NEXT
plot(ber.out[,1]~rev(1:8),ylab="Arthropod species making up 50% abundance",
     xlab="Number of sites", type = "l", lty = 1, lwd = 1,
     ylim = c(0,6))
par(new=TRUE)
plot(cop.out[,1]~rev(1:8),ylab="",xlab="", axes = FALSE,
     type = "l", lty = 2, lwd = 1,
     ylim = c(0,6))
par(new=TRUE)
plot(maa.out[,1]~rev(1:8),ylab="",xlab="", axes = FALSE,
     type = "l", lty = 1, lwd = 2,
     ylim = c(0,6))
par(new=TRUE)
plot(syw.out[,1]~rev(1:8),ylab="",xlab="", axes = FALSE,
     type = "l", lty = 2, lwd = 2,
     ylim = c(0,6))
#NEXT
plot(ber.out[,4]~rev(1:8),ylab="Total interactions",xlab="Number of sites", 
     type = "l", lty = 1, lwd = 1, ylim = c(0,210))
par(new=TRUE)
plot(cop.out[,4]~rev(1:8),ylab="",xlab="", axes = FALSE,
     type = "l", lty = 2, lwd = 1, ylim = c(0,210))
par(new=TRUE)
plot(maa.out[,4]~rev(1:8),ylab="",xlab="", axes = FALSE,
     type = "l", lty = 1, lwd = 2, ylim = c(0,210))
par(new=TRUE)
plot(syw.out[,4]~rev(1:8),ylab="",xlab="", axes = FALSE,
     type = "l", lty = 2, lwd = 2, ylim = c(0,210))
#NEXT
plot(ber.out[,3]~rev(1:8),ylab="Interactions making up 50% of interaction abundance",
     xlab="Number of sites", type = "l", lty = 1, lwd = 1, ylim = c(0,16))
par(new=TRUE)
plot(cop.out[,3]~rev(1:8),ylab="",xlab="", axes = FALSE, 
     type = "l", lty = 2, lwd = 1, ylim = c(0,16))
par(new=TRUE)
plot(maa.out[,3]~rev(1:8),ylab="",xlab="", axes = FALSE, 
     type = "l", lty = 1, lwd = 2, ylim = c(0,16))
par(new=TRUE)
plot(syw.out[,3]~rev(1:8),ylab="",xlab="", axes = FALSE,
     type = "l", lty = 2, lwd = 2, ylim = c(0,16))

legend("topleft", legend = c("FBE","FCO", "FMA","FSY?"), lty = c(1,2,1,2),
       lwd = c(1,1,2,2))

