#IBiSCA analysis witch accumulation curve

accumulation <- function(test.matrix, xlab = "Number of Samples", ...){
  "This function plots an accumulation curve"
  Species <- vector(mode = "integer", length = dim(test.matrix)[2])
  for (n in 1:dim(test.matrix)[2]){
    Species[n] <- sum(rowSums(as.matrix(test.matrix[, 1:n]), na.rm = TRUE)>0)
  }
  plot(Species, xlab = xlab, ...)
  return(Species)
}

#accumulation(test.matrix) #should be 3,4,5,5,6,6
#plot(curve)

ar <- function(dataset){
  column.names <- names(dataset) #Creates a vector with column names
  site.names <- substr(column.names, 7, 10) #Subtracted site names
  site.rep <- as.numeric(substr(column.names, 12, 13)) #Subtracted time replications
  name.strings <- unique(site.names)[5:14]
  result <- matrix(0, nrow = 1, ncol = 2)
  for (i in 1:10){
    site.data <- dataset[, site.names == name.strings[i]]
    site.data.results <- accumulation(site.data)
    replicates <- site.rep[site.names == name.strings[i]]
    site.result <- cbind(site.data.results, replicates)
    result <- rbind(result, site.result)
  }
  return(result)
}


#### CODE BELOW PRODUCES ACCUMULATION CURVES FOR FOUR FAMILIES

setwd("C:\\R\\how simple are tropical food webs\\50perc\\IBISCA accumulation")

flatidae <- read.csv("flatidae.csv", header = TRUE, sep = "\t")
flatidae.out <- ar(flatidae)[-1,]
flatidae.out <- as.data.frame(flatidae.out)
flatidae.mean <- tapply(flatidae.out[,1], flatidae.out[,2], mean)

arctiidae <- read.csv("arctiidae.csv", header = TRUE, sep = "\t")
arctiidae.out <- ar(arctiidae)[-1,]
arctiidae.out <- as.data.frame(arctiidae.out)
arctiidae.mean <- tapply(arctiidae.out[,1], arctiidae.out[,2], mean)

geometridae <- read.csv("geometridae.csv", header = TRUE, sep = "\t")
geometridae.out <- ar(geometridae)[-1,]
geometridae.out <- as.data.frame(geometridae.out)
geometridae.mean <- tapply(geometridae.out[,1], geometridae.out[,2], mean)

pyraloidea <- read.csv("pyraloidea.csv", header = TRUE, sep = "\t")
pyraloidea.out <- ar(pyraloidea)[-1,]
pyraloidea.out <- as.data.frame(pyraloidea.out)
pyraloidea.mean <- tapply(pyraloidea.out[,1], pyraloidea.out[,2], mean)

windows(400,300)
plot(as.data.frame(arctiidae.mean), 
     xlab = "", ylab = "", 
     axes = FALSE, col = "red", pch = 2, ylim = c(0,150),
     type = "l", lwd = 1, lty = 1)
par(new = TRUE)
plot(as.data.frame(geometridae.mean), 
     xlab = "", ylab = "", 
     axes = FALSE, col = "blue", pch = 3, ylim = c(0,150),
     type = "l", lwd = 1, lty = 2)
par(new = TRUE)
plot(as.data.frame(pyraloidea.mean), 
     xlab = "", ylab = "", 
     axes = FALSE, col = "brown", pch = 4, ylim = c(0,150),
     type = "l", lwd = 2, lty = 1)
legend("topleft", c("Arctiidae", "Geometridae", "Pyraloidea", "Flatidae"), 
       lty = c(1,2,1,2), lwd = c(1,1,2,2), 
       col = c("red", "blue", "brown","black"))
par(new = TRUE)
plot(as.data.frame(flatidae.mean), 
     xlab = "Time replications", ylab = "Species", 
     axes = TRUE, col = "black", pch = 1, ylim = c(0,150),
     type = "l", lwd = 2, lty = 2)


###code below calculates accumulation curves without looking at a sites'

#Uploading the data

setwd('C:\\R\\how simple are tropical food webs\\50perc\\IBISCA accumulation')
flatidae <- read.csv("flatidae.csv", header = TRUE, sep = "\t")
flatidae.out <- accumulation(flatidae[,5:dim(flatidae)[2]], 
                             ylim = c(0, 350),
                             xlim = c(0, 210),
                             col = "red",
                             axes = FALSE,
                             ylab = "",
                             xlab = "")
flatidae.out

par(new=TRUE)

arctidae <- read.csv("arctiidae.csv", header = TRUE, sep = "\t")
arctidae.out <- accumulation(arctidae[,5:dim(arctidae)[2]], 
                             ylim = c(0, 350),
                             xlim = c(0, 210),
                             axes = FALSE,
                             ylab = "",
                             xlab = "")
arctidae.out

par(new=TRUE)

geometridae <- read.csv("geometridae.csv", header = TRUE, sep = "\t")
geometridae.out <- accumulation(geometridae[,5:dim(geometridae)[2]], 
                             ylim = c(0, 350),
                             xlim = c(0, 210),
                             col = "blue",
                             axes = FALSE,
                             ylab = "",
                             xlab = "")
length(geometridae.out)

par(new=TRUE)

pyraloidea <- read.csv("pyraloidea.csv", header = TRUE, sep = "\t")
pyraloidea.out <- accumulation(pyraloidea[,5:dim(pyraloidea)[2]], 
                                ylim = c(0, 350),
                                xlim = c(0, 210),
                                col = "grey30")
pyraloidea.out

geometridae[is.na(geometridae)] <- 0
pyraloidea[is.na(pyraloidea)] <- 0
arctidae[is.na(arctidae)] <- 0
flatidae[is.na(flatidae)] <- 0

geo <- geometridae[,-c(1:4)]
pyr <- pyraloidea[,-c(1:4)]
arc <- arctidae[,-c(1:4)]
fla <- flatidae[,-c(1:4)]

setwd("C:\\R\\how simple are tropical food webs")
source("funct_simplifyV2.R")
fla.res <- simplify(fla, edge="col",n=100,reduction.level=0.95)

collapse <- function(dataset){
  sn <- as.numeric(substr(colnames(dataset), 12, 13))
  colnames(dataset) <- sn
  reps <- unique(colnames(dataset))
  new.matrix <- matrix(0, nrow = dim(dataset)[1], ncol=length(reps))
  for (i in 1:length(reps)){
    new.matrix[,i] <- rowSums(dataset[,colnames(dataset)==reps[i]])
  }
  return(new.matrix)
}

geo.coll <- collapse(geo)
pyr.coll <- collapse(pyr)
arc.coll <- collapse(arc)
fla.coll <- collapse(fla)

geo.sim <- simplify(geo.coll, edge="col",n=100,reduction.level=0.95)
pyr.sim <- simplify(pyr.coll, edge="col",n=100,reduction.level=0.92)
arc.sim <- simplify(arc.coll, edge="col",n=100,reduction.level=0.95)
fla.sim <- simplify(fla.coll, edge="col",n=100,reduction.level=0.95)

datasets <- list(geo.sim,pyr.sim,arc.sim,fla.sim)

red<-rgb(150,  0,  0,200,maxColorValue=255)
grn<-rgb(0  ,150,  0,200,maxColorValue=255)
blu<-rgb(0  ,0  ,150,200,maxColorValue=255)
bck<-rgb(0  ,0  ,  0,200,maxColorValue=255)

colours <- c(red,grn,blu,bck)
type = "b"
lwd=2
lty <- c(1,1,1,1)
noise <- c(-0.1,0,0.1,0.2)
# FULL SPECIES PLOT

pdf("fig8.pdf", width=10,height=5)
par(mfrow=c(1,2))
ys<-c(datasets[[1]][,4],
      datasets[[2]][,4],
      datasets[[3]][,4],
      datasets[[4]][,4])
ylim<-c(min(ys),max(ys))
xlim<-c(2,20)

plot(0,xlim=xlim,ylim=ylim,cex.lab=0.75,cex.axis=0.75,mgp = c(1.5, 0.5, 0),
     xlab="Time replication",ylab="Species")
text(3,(5/4.8)*310, "A")
for(i in 1:4){
  par(new=TRUE)
  plot(datasets[[i]][,4]~rev(0:20),
       xlim=xlim, ylim=ylim, xlab="", ylab="",
       axes=F, col = colours[i], pch=16, cex=1.5,
       type=type,lwd=lwd,lty=lty[i])
}

#plot(geo.sim[,16]~rev(1:21))

# 50% SPECIES PLOT
ys<-c(datasets[[1]][,16],
      datasets[[2]][,16],
      datasets[[3]][,16],
      datasets[[4]][,16])
ylim<-c(min(ys),max(ys))
xlim<-c(2,20)

plot(0,xlim=xlim,ylim=ylim,cex.lab=0.75,cex.axis=0.75,mgp = c(1.5, 0.5, 0),
     xlab="Time replication",ylab="Species making up 50% of arthropods abundance")
text(3,4.8, "B")
for(i in 1:4){
  par(new=TRUE)
  plot(datasets[[i]][,16]+noise[i]~rev(0:20),
       xlim=xlim, ylim=ylim, xlab="", ylab="",
       axes=F, col = colours[i], pch=16,cex=1.5,
       type=type, lwd=lwd,lty=lty[i])
}
legend("bottomright", c("Geometridae", "Pyralidae", "Arctiidae", "Flatridae"),
       pch=16, col = colours, pt.cex = 1.5, bty = "n")
dev.off()
