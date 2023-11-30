setwd("C:\\R\\how simple are tropical food webs\\tritrophic data")

order.and.clear <- function(dataset){
  #This function order the dataset based on rows and columns
  #and removes zero rows and columns. Dataset should not contain any NAs!
  dataset <- dataset[rowSums(dataset)>0,]
  dataset <- dataset[, colSums(dataset)>0]
  dataset <- dataset[order(rowSums(dataset), decreasing = TRUE), ]
  dataset <- dataset[,order(colSums(dataset), decreasing = TRUE)]
  return(dataset)
}

equal <- read.csv("ufwEqual.csv", header = TRUE, sep = "\t")
setwd("C:\\R\\how simple are tropical food webs")
equal <- as.matrix(equal[, c(-1,-2)])
equal[is.na(equal)] <- 0

#getting rid of the empty columns and rows from equal dataset
dim(equal)
equal <- equal[rowSums(equal)>0, ]
equal <- equal[, colSums(equal)>0]

library(bipartite)

# Colour the parasites that account for 50%
red <- "grey80"
red <- rgb(150,0,0,150,maxColorValue=255) 
blu <- rgb(0,0,150,255,maxColorValue=255)
rownames(equal) <- seq(1:dim(equal)[1])
colnames(equal) <- seq(1:dim(equal)[2])

###### Colours for parasites
equal.ord <- order.and.clear(equal) #order and remove empty rows to easily find the 50%
tot.abu <- sum(equal) # total abundance
subset <- equal.ord[ ,cumsum(colSums(equal.ord))<(tot.abu/2)] #subset of data whith only 50
subset <- subset[rowSums(subset)>0,] # only connected rows left
upper.level.to.colour1 <- as.numeric(colnames(subset)) # which upper sp to colour
lower.level.to.colour1 <- as.numeric(rownames(subset)) # which lower lev to colour

row.indices <- as.vector(which(subset>0, arr.ind = TRUE)[,1])
col.indices <- as.vector(which(subset>0, arr.ind = TRUE)[,2])
row.for.int <- as.numeric(rownames(subset)[row.indices])
col.for.int <- as.numeric(colnames(subset)[col.indices])
interaction.col1 <- ((row.for.int - 1)*dim(equal)[2])+col.for.int

############################################################################


# Colours for arhropods 
subset1 <- equal.ord[cumsum(rowSums(equal.ord))<(tot.abu/2), ]
subset1 <- subset1[,colSums(subset1)>0]
upper.level.to.colour2 <- as.numeric(colnames(subset1))
lower.level.to.colour2 <- as.numeric(rownames(subset1))

row.indices <- as.vector(which(subset1>0, arr.ind = TRUE)[,1])
col.indices <- as.vector(which(subset1>0, arr.ind = TRUE)[,2])
row.for.int <- as.numeric(rownames(subset1)[row.indices])
col.for.int <- as.numeric(colnames(subset1)[col.indices])
interaction.col2 <- ((row.for.int - 1)*dim(equal)[2])+col.for.int

#########################################
####### Colours for interactions ########

interactions <- sort(equal.ord[equal.ord>0], decreasing = T)
inter.50pc <- interactions[cumsum(interactions)<(tot.abu/2)]

intR <- vector('numeric', length=length(inter.50pc))
intC <- vector('numeric', length=length(inter.50pc))

for (int in 1:length(inter.50pc)){
  print(int)
  indices <- which(equal==inter.50pc[int], arr.ind=T)
  print(indices)
  intR[int] <- indices[1,1]
  intC[int] <- indices[1,2]
}

intR
intC

intC[9] <- 119
intR[9] <- 28
intC[13]<- 77
intR[13]<- 66
intC[15]<- 91
intR[15]<- 61


subset.int <- equal[intR, intC]
upper.level.to.colour3 <- as.numeric(colnames(subset.int))
lower.level.to.colour3 <- as.numeric(rownames(subset.int))

row.indices <- as.vector(which(subset.int>0, arr.ind = TRUE)[,1])
col.indices <- as.vector(which(subset.int>0, arr.ind = TRUE)[,2])
row.for.int <- as.numeric(rownames(subset.int)[row.indices])
col.for.int <- as.numeric(colnames(subset.int)[col.indices])
interaction.col3 <- ((row.for.int - 1)*dim(equal)[2])+col.for.int

####### END OF INTERACTIONS ######


###### setting up colours
up.colour1 <- rep(red, dim(equal)[2])
up.colour1[upper.level.to.colour1] <- blu
lo.colour1 <- rep(red, dim(equal)[1])
lo.colour1[lower.level.to.colour1] <- blu
int.col1 <- rep(NULL, dim(equal)[1]*dim(equal)[2]) #substitute NULL with red
int.col1[interaction.col1] <- blu


up.colour2 <- rep(red, dim(equal)[2])
up.colour2[upper.level.to.colour2] <- blu
lo.colour2 <- rep(red, dim(equal)[1])
lo.colour2[lower.level.to.colour2] <- blu
int.col2 <- rep(NULL, dim(equal)[1]*dim(equal)[2]) # substitiute NUll with red
int.col2[interaction.col2] <- blu


up.colour3 <- rep(red, dim(equal)[2])
up.colour3[upper.level.to.colour3] <- blu
lo.colour3 <- rep(red, dim(equal)[1])
lo.colour3[lower.level.to.colour3] <- blu
int.col3 <- rep(NULL, dim(equal)[1]*dim(equal)[2])
int.col3[interaction.col3] <- blu


ls <- 0.005
hs <- (0.0015*ls)/0.0023

windows(4000,2000)
pdf("fig2.pdf", height=7, width=7)
par(mfrow=c(2,1), mar=c(2,2,2,2))
plotweb(equal, method="cca",
        y.width.low=0.1,y.width.high=0.1,
        high.lablength=0,low.lablength=0,
        
        arrow = "down", empty=F,
        
        low.spacing = ls, high.spacing = hs,
        
        col.high = up.colour1,
        bor.col.high = up.colour1,
        
        col.low = lo.colour1,
        bor.col.low = lo.colour1,
        
        col.interaction=int.col1,
        bor.col.interaction=int.col1,
        
        plot.axes = F
          )
text(0.1,1.7,"A", cex = 1.5)

plotweb(equal, method="cca",
        y.width.low=0.1,y.width.high=0.1,
        high.lablength=0,low.lablength=0,
        
        arrow = "up", empty=F,
        
        low.spacing = ls, high.spacing = hs,
        
        col.high = up.colour2,
        bor.col.high = up.colour2,
        
        col.low = lo.colour2,
        bor.col.low = lo.colour2,
        
        col.interaction=int.col2,
        bor.col.interaction=int.col2
)
text(0.1,1.7,"B", cex = 1.5)
dev.off()

pdf("figS1.pdf", height=7, width=7)
par(mfrow=c(2,1), mar=c(2,2,2,2))

# Interactions
plotweb(equal, method="normal",
        y.width.low=0.1,y.width.high=0.1,
        high.lablength=0,low.lablength=0,
        
        arrow = "down", 
        empty=T,
        
        low.spacing = ls, high.spacing = hs,
        
        col.high = up.colour3,
        bor.col.high = up.colour3,
        
        col.low = lo.colour3,
        bor.col.low = lo.colour3,
        
        col.interaction=int.col3 ,
        bor.col.interaction=int.col3 
)
dev.off()
