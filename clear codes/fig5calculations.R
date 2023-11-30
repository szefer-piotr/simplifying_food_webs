# Calculations 
setwd("C:\\R\\how simple are tropical food webs\\datasets")
source("funct_simplifyV3.R")
setwd("C:\\R\\how simple are tropical food webs")

setwd("C:\\R\\how simple are tropical food webs")
guilds <- read.table("guilds.csv", header = TRUE, sep = "\t")
guilds[is.na(guilds)] <- 0
guilds.val <- guilds[,2:39] #Plants are in columns and arthropods are in rows
setwd("C:\\R\\how simple are tropical food webs\\tritrophic data")
equal <- read.csv("ufwEqual.csv", header = TRUE, sep = "\t")
setwd("C:\\R\\how simple are tropical food webs")
equal <- as.matrix(equal[, c(-1,-2)])
equal[is.na(equal)] <- 0
#removing zeros from parasite data
parasites <- equal[-which(rowSums(equal)==0),]
miners <- guilds.val[guilds$Guild == unique(guilds$Guild)[3],]
mobile <- guilds.val[guilds$Guild == unique(guilds$Guild)[4],]
semi   <- guilds.val[guilds$Guild == unique(guilds$Guild)[5],]


#Transposing matrices
miners <- t(miners)
mobile <- t(mobile)
semi   <- t(semi)

# Reducing and randomising data

dim(semi)

#semi.row <- simplify(semi,  edge="row",n=100,reduction.level=0.95)
#write.table(semi.row, "semi.row.txt")
#mobi.row <- simplify(mobile,    edge="row",n=100,reduction.level=0.95)
#write.table(mobi.row, "mobi.row.txt")
#mine.row <- simplify(miners,    edge="row",n=100,reduction.level=0.85)
#write.table(mine.row, "mine.row.txt")
#para.row <- simplify(parasites, edge="row",n=100,reduction.level=0.97)
#write.table(para.row, "para.row.txt")

#semi.col <- simplify(semi,  edge="col",n=100,reduction.level=0.98)
#write.table(semi.col, "semi.col.txt")
#mobi.col <- simplify(mobile,    edge="col",n=100,reduction.level=0.99)
#write.table(mobi.col, "mobi.col.txt")
#mine.col <- simplify(miners,    edge="col",n=100,reduction.level=0.95)
#write.table(mine.col, "mine.col.txt")
#para.col <- simplify(parasites, edge="col",n=100,reduction.level=0.96)
#write.table(para.col, "para.col.txt")

# INteractions (Not enough)
#semi.int <- simplify(semi,  edge="int",n=100,reduction.level=0.97)
#write.table(semi.int, "semi.int.txt")
#mobi.int <- simplify(mobile,    edge="int",n=100,reduction.level=0.99)
#write.table(mobi.int, "mobi.int.txt")
#mine.int <- simplify(miners,    edge="int",n=100,reduction.level=0.97)
#write.table(mine.int, "mine.int.txt")
#para.int <- simplify(parasites, edge="int",n=100,reduction.level=0.97)
#write.table(para.int, "para.int.txt")

# Individuals

#semi.int <- simplify(semi,  edge="ind",n=100,reduction.level=0.97, by=100)
#write.table(semi.int[semi.int[,3]>0, ], "semi.ind.txt")
#mobi.ind <- simplify(mobile,    edge="ind",n=100,reduction.level=0.97, by=100)
#write.table(mobi.ind[mobi.ind[,3]>0, ], "mobi.ind.txt")
#mine.ind <- simplify(miners,    edge="ind",n=100,reduction.level=0.97, by=53)
#write.table(mine.ind[mine.ind[,2]>0, ], "mine.ind.txt")
#para.ind <- simplify(parasites, edge="ind",n=100,reduction.level=0.97, by=10)
#write.table(para.ind[para.ind[,3]>0, ], "para.ind.txt")

setwd("C:\\R\\how simple are tropical food webs\\simplification results")
# Reading the resuts
semi.row <- read.table("semi.row.txt")
mobi.row <- read.table("mobi.row.txt")
mine.row <- read.table("mine.row.txt")
para.row <- read.table("para.row.txt")

semi.col <- read.table("semi.col.txt")
mobi.col <- read.table("mobi.col.txt")
mine.col <- read.table("mine.col.txt")
para.col <- read.table("para.col.txt")

semi.int <- read.table("semi.int.txt")
mobi.int <- read.table("mobi.int.txt")
mine.int <- read.table("mine.int.txt")
para.int <- read.table("para.int.txt")

semi.ind <- read.table("semi.ind.txt")
mobi.ind <- read.table("mobi.ind.txt")
mine.ind <- read.table("mine.ind.txt")
para.ind <- read.table("para.ind.txt")

# Plots

red <- rgb(150,0,0,75,maxColorValue=255) 
grn <- rgb(0,150,0,75,maxColorValue=255)

res.list <- list(mine.col, mobi.col, semi.col,
                 mine.row, mobi.row, semi.row,
                 mine.int, mobi.int, semi.int,
                 mine.ind, mobi.ind, semi.ind)

colours <- c(rgb(150,0,0,75,maxColorValue=255),rgb(0,150,0,75,maxColorValue=255))

cols.to.plot <- c(4,6,8,10,12,14,16,18,20)
labels <- names(para.int)

# Plot COLS
windows(1000,1000)

pdf("fig5D.pdf")
par(mfrow=c(3,3))

for (num in cols.to.plot){ # specify columns with H2 Vuln Gen
  vals <- c(res.list[[1]][,num],res.list[[1]][,num+1],
            res.list[[2]][,num],res.list[[2]][,num+1],
            res.list[[3]][,num],res.list[[3]][,num+1])
  xlim <- c(0  ,1)
  ylim <- c(min(vals[vals>0], na.rm=T),max(vals[vals>0], na.rm=T))
  
  plot(NULL, xlim=xlim, ylim=ylim, ylab=labels[num], xlab="Prop. of arthropods",
       main = "Arthropod species removal")
  
  for(j in 1:3){
    
    par(new=TRUE)
    plot(res.list[[j]][,2]/res.list[[j]][1,2], res.list[[j]][,num],
         col=colours[2], pch=16, ylim=ylim,
         xlim = xlim, axes=FALSE, xlab="",ylab="")
    par(new=TRUE)
    plot(res.list[[j]][,3]/res.list[[j]][1,3], res.list[[j]][,num + 1],
         col=colours[1], pch=16, ylim=ylim,
         xlim = xlim, axes=FALSE, xlab="",ylab="")
    abline(v=0.5,col=colours[1],lty=2)
    par(new=TRUE)
  }
  par(new=FALSE)
}
dev.off()


# PLOT FOR ROWS
pdf("fig5E.pdf")
par(mfrow=c(3,3))
for (num in cols.to.plot){ # specify columns with H2 Vuln Gen
  vals <- c(res.list[[4]][,num],res.list[[4]][,num+1],
            res.list[[5]][,num],res.list[[5]][,num+1],
            res.list[[6]][,num],res.list[[6]][,num+1])
  xlim <- c(0  ,1)
  ylim <- c(min(vals[vals>0], na.rm=T),max(vals[vals>0], na.rm=T))
  
  plot(NULL, xlim=xlim, ylim=ylim, ylab=labels[num], xlab="Prop. of arthropods",
       main = "Plant species removal")
  
  for(j in 4:6){
    
    par(new=TRUE)
    plot(res.list[[j]][,2]/res.list[[j]][1,2], res.list[[j]][,num],
         col=colours[2], pch=16, ylim=ylim,
         xlim = xlim, axes=FALSE, xlab="",ylab="")
    par(new=TRUE)
    plot(res.list[[j]][,3]/res.list[[j]][1,3], res.list[[j]][,num + 1],
         col=colours[1], pch=16, ylim=ylim,
         xlim = xlim, axes=FALSE, xlab="",ylab="")
    abline(v=0.5,col=colours[1],lty=2)
    par(new=TRUE)
  }
  par(new=FALSE)
}
dev.off()

############### <_----------------------------
# PLOT FOR INTERACTIONS
pdf("fig5F.pdf")
par(mfrow=c(3,3))
for (num in cols.to.plot){ # specify columns with H2 Vuln Gen
  vals <- c(res.list[[7]][,num],res.list[[7]][,num+1],
            res.list[[8]][,num],res.list[[8]][,num+1],
            res.list[[9]][,num],res.list[[9]][,num+1])
  xlim <- c(0  ,1)
  ylim <- c(min(vals[vals>0], na.rm=T),max(vals[vals>0], na.rm=T))
  
  plot(NULL, xlim=xlim, ylim=ylim, ylab=labels[num], xlab="Prop. of arthropods",
       main = "Interactions removal")
  
  for(j in 7:9){
    
    par(new=TRUE)
    plot(res.list[[j]][,2]/res.list[[j]][1,2], res.list[[j]][,num],
         col=colours[2], pch=16, ylim=ylim,
         xlim = xlim, axes=FALSE, xlab="",ylab="")
    par(new=TRUE)
    plot(res.list[[j]][,3]/res.list[[j]][1,3], res.list[[j]][,num + 1],
         col=colours[1], pch=16, ylim=ylim,
         xlim = xlim, axes=FALSE, xlab="",ylab="")
    abline(v=0.5,col=colours[1],lty=2)
    par(new=TRUE)
  }
  par(new=FALSE)
}
dev.off()

########################
# PLOT FOR INDIVIDUALS #
########################

pdf("fig5G.pdf")
par(mfrow=c(3,3))
for (num in cols.to.plot){ # specify columns with H2 Vuln Gen
  vals <- c(res.list[[10]][,num],res.list[[10]][,num+1],
            res.list[[11]][,num],res.list[[11]][,num+1],
            res.list[[12]][,num],res.list[[12]][,num+1])
  xlim <- c(0  ,1)
  ylim <- c(min(vals[vals>0], na.rm=T),max(vals[vals>0], na.rm=T))
  
  plot(NULL, xlim=xlim, ylim=ylim, ylab=labels[num], xlab="Prop. of arthropods",
       main = "Individuals removal")
  
  for(j in 10:12){
    
    par(new=TRUE)
    plot(res.list[[j]][,2]/res.list[[j]][1,2], res.list[[j]][,num],
         col=colours[2], pch=16, ylim=ylim,
         xlim = xlim, axes=FALSE, xlab="",ylab="")
    par(new=TRUE)
    plot(res.list[[j]][,3]/res.list[[j]][1,3], res.list[[j]][,num + 1],
         col=colours[1], pch=16, ylim=ylim,
         xlim = xlim, axes=FALSE, xlab="",ylab="")
    abline(v=0.5,col=colours[1],lty=2)
    par(new=TRUE)
  }
  par(new=FALSE)
}
dev.off()


##############################
##### LOOPED FULL PLOTS ######
##############################


#### PARASITES DATASET

windows(1600,1600)
pdf("fig5A.pdf")
par(mfrow=c(3,3),mar=c(3,3,3,3), cex.axis=0.6,cex.lab=0.6,cex.main=0.7,cex=1,
    mai=c(0.5,0.5,0.5,0.5), tck=-0.025, mgp=c(1,0.2,0), type="o")

cols.to.plot <- c(4,6,8,10,12,14,16,18,20)
labels <- names(para.int)

# INTERACTION LOOP
for(col in cols.to.plot){
  vector <- c(para.int[,col],para.int[,col+1])
  ylim <- c(min(vector[vector>0]),max(vector[vector>0]))
  
  plot(para.int[,2]/para.int[1,2],para.int[,col],  ylim=ylim,
       axes=F,xlab="",ylab="",
       col=grn, pch=16)
  par(new=TRUE)
  
  plot(para.int[,3]/para.int[1,3],para.int[,col+1],ylim=ylim,
       axes=T,xlab="Prop. of parasites ind.",ylab=labels[col], main= "Interaction removal",
       col=red, pch=16)
  abline(v=0.5, lty=2, col=red)
  #text(0.9, 0.99, "A")
}
dev.off()

# ROWS LOOP
windows(1600,1600)
pdf("fig5B.pdf")
par(mfrow=c(3,3),mar=c(3,3,3,3), cex.axis=0.6,cex.lab=0.6,cex.main=0.7,cex=1,
    mai=c(0.5,0.5,0.5,0.5), tck=-0.025, mgp=c(1,0.2,0), type="o")
cols.to.plot <- c(4,6,8,10,12,14,16,18,20)
labels <- names(para.row)
for(col in cols.to.plot){
  vector <- c(para.row[,col],para.row[,col+1])
  ylim <- c(min(vector[vector>0],na.rm=T),max(vector[vector>0],na.rm=T))
  
  plot(para.row[,2]/para.row[1,2],para.row[,col],  ylim=ylim,
       axes=F,xlab="",ylab="",
       col=grn, pch=16)
  par(new=TRUE)
  
  plot(para.row[,3]/para.row[1,3],para.row[,col+1],ylim=ylim,
       axes=T,xlab="Prop. of parasites ind.",ylab=labels[col], main= "Rows removal",
       col=red, pch=16)
  abline(v=0.5, lty=2, col=red)
  #text(0.9, 0.99, "A")
}
dev.off()
# COLUMNS LOOP
windows(1600,1600)
pdf("fig5C.pdf")
par(mfrow=c(3,3),mar=c(3,3,3,3), cex.axis=0.6,cex.lab=0.6,cex.main=0.7,cex=1,
    mai=c(0.5,0.5,0.5,0.5), tck=-0.025, mgp=c(1,0.2,0), type="o")
cols.to.plot <- c(4,6,8,10,12,14,16,18,20)
labels <- names(para.row)
for(col in cols.to.plot){
  vector <- c(para.col[,col],para.col[,col+1])
  ylim <- c(min(vector[vector>0],na.rm=T),max(vector[vector>0],na.rm=T))
  
  plot(para.col[,2]/para.col[1,2],para.col[,col],  ylim=ylim,
       axes=F,xlab="",ylab="",
       col=grn, pch=16)
  par(new=TRUE)
  
  plot(para.col[,3]/para.col[1,3],para.col[,col+1],ylim=ylim,
       axes=T,xlab="Prop. of parasites ind.",ylab=labels[col], main= "Columns removal",
       col=red, pch=16)
  abline(v=0.5, lty=2, col=red)
  #text(0.9, 0.99, "A")
}
dev.off()

#### Parasites individuals ####

# INDIVIDUALS LOOP
windows(1600,1600)
pdf("fig5H.pdf")
par(mfrow=c(3,3),mar=c(3,3,3,3), cex.axis=0.6,cex.lab=0.6,cex.main=0.7,cex=1,
    mai=c(0.5,0.5,0.5,0.5), tck=-0.025, mgp=c(1,0.2,0), type="o")
cols.to.plot <- c(4,6,8,10,12,14,16,18,20)
labels <- names(para.ind)
for(col in cols.to.plot){
  vector <- c(para.ind[,col],para.ind[,col+1])
  ylim <- c(min(vector[vector>0],na.rm=T),max(vector[vector>0],na.rm=T))
  
  plot(para.ind[,2]/para.ind[1,2],para.ind[,col],  ylim=ylim,
       axes=F,xlab="",ylab="",
       col=grn, pch=16)
  par(new=TRUE)
  
  plot(para.ind[,3]/para.ind[1,3],para.ind[,col+1],ylim=ylim,
       axes=T,xlab="Prop. of parasites ind.",ylab=labels[col], main= "Individuals removal",
       col=red, pch=16)
  abline(v=0.5, lty=2, col=red)
  #text(0.9, 0.99, "A")
}
dev.off()
################

# PLOT WITH SELECTED VALUES

red <- rgb(150,0,0,75,maxColorValue=255) 
grn <- rgb(0,150,0,75,maxColorValue=255)

guilds.data <- list(mine.col, mobi.col, semi.col,
                 mine.row, mobi.row, semi.row,
                 mine.int, mobi.int, semi.int)
parasites <- list(para.col,para.row,para.int)

colours <- c(rgb(150,0,0,75,maxColorValue=255),rgb(0,150,0,75,maxColorValue=255))

labels <- c("H2", "Generality", "Vulnerability")
titles <- rep(c("Columns","Rows","Interactions"))
guild.name <- c("MIN","MOB","SEM")
cols <- c(10,12,14) #Ordered
data.index <- c(1,4,7)
pch=16
letters<-c("A","B","C","D","E","F","G","H","I")


# FOR FINAL PLOTS FOR VOJTA
#GUILDS
setwd("C:/R/how simple are tropical food webs/test plots")
pdf("fig5guilds.pdf")
par(mfrow=c(3,3),mar=c(3,3,3,3), cex.axis=0.6,cex.lab=0.6,cex.main=0.7,cex=1,
    mai=c(0.5,0.5,0.5,0.5), tck=-0.025, mgp=c(1,0.2,0))#, type="o")

for(i in 1:3){
  di <- data.index[i]
  for(j in 1:3){
    ys <- c(guilds.data[[di]][,cols[j]],
            guilds.data[[di+1]][,cols[j]],
            guilds.data[[di+2]][,cols[j]],
            guilds.data[[di]][,cols[j]+1],
            guilds.data[[di+1]][,cols[j]+1],
            guilds.data[[di+2]][,cols[j]+1])
    xlim <- c(0,1)
    ylim <- c(min(ys[ys>0],na.rm=T),max(ys[ys>0],na.rm=T))
    # Empty plot
    plot(NULL,xlim=xlim,ylim=ylim,
         xlab="Proportion of arthropod individuals",
         ylab=labels[j],main=letters[((i-1)*3)+j])
    #Ordered
    par(new=TRUE)
    plot(guilds.data[[di]][,2]/guilds.data[[di]][1,2],guilds.data[[di]][,cols[j]],
         axes = FALSE, xlab="",ylab="",col=colours[2],pch=pch,
         xlim=xlim,ylim=ylim)
    text(1,guilds.data[[di]][1,cols[j]]-0.05, guild.name[1])
    par(new=TRUE)
    plot(guilds.data[[di+1]][,2]/guilds.data[[di+1]][1,2],guilds.data[[di+1]][,cols[j]],
         axes = FALSE, xlab="",ylab="",col=colours[2],pch=pch,
         xlim=xlim,ylim=ylim)
    text(1,guilds.data[[di+1]][1,cols[j]]-0.05, guild.name[2])
    par(new=TRUE)
    plot(guilds.data[[di+2]][,2]/guilds.data[[di+2]][1,2],guilds.data[[di+2]][,cols[j]],
         axes = FALSE, xlab="",ylab="",col=colours[2],pch=pch,
         xlim=xlim,ylim=ylim)
    text(1,guilds.data[[di+2]][1,cols[j]]-0.05, guild.name[3])
    #Random
    par(new=TRUE)
    plot(guilds.data[[di]][,3]/guilds.data[[di]][1,3],guilds.data[[di]][,cols[j]+1],
         axes = FALSE, xlab="",ylab="",col=colours[1],pch=pch,
         xlim=xlim,ylim=ylim)
    par(new=TRUE)
    plot(guilds.data[[di+1]][,3]/guilds.data[[di+1]][1,3],guilds.data[[di+1]][,cols[j]+1],
         axes = FALSE, xlab="",ylab="",col=colours[1],pch=pch,
         xlim=xlim,ylim=ylim)
    par(new=TRUE)
    plot(guilds.data[[di+2]][,3]/guilds.data[[di+2]][1,3],guilds.data[[di+2]][,cols[j]+1],
         axes = FALSE, xlab="",ylab="",col=colours[1],pch=pch,
         xlim=xlim,ylim=ylim)
  }
}
dev.off()

### AND PARASITES
##### REPAETED SOME PREPS

labels <- c("H2", "Generality", "Vulnerability")
titles <- rep(c("Columns","Rows","Interactions"))
cols <- c(10,12,14) #Ordered
data.index <- c(1,4,7)
pch=16

letters<-c("A","B","C","D","E","F","G","H","I")
# And parasites now.
pdf("fig5par.pdf")
par(mfrow=c(3,3),mar=c(3,3,3,3), cex.axis=0.6,cex.lab=0.6,cex.main=0.7,cex=1,
    mai=c(0.5,0.5,0.5,0.5), tck=-0.025, mgp=c(1,0.2,0), type="o")
for(j in 1:3){
  for(i in 1:3){
    ys <- c(parasites[[j]][,cols[i]],
            parasites[[j]][,cols[i]+1])
    xlim <- c(0,1)
    ylim <- c(min(ys[ys>0],na.rm=T),max(ys[ys>0],na.rm=T))
    plot(NULL,xlim=xlim,ylim=ylim,
         xlab="Proportion of parasite individuals",
         ylab=labels[i],main=letters[((j-1)*3)+i])
    #ORdered
    par(new=TRUE)
    plot(parasites[[j]][,2]/parasites[[j]][1,2],parasites[[j]][,cols[i]],
         axes = FALSE, xlab="",ylab="",col=colours[2],pch=pch,
         xlim=xlim,ylim=ylim)
    #Random
    par(new=TRUE)
    plot(parasites[[j]][,3]/parasites[[j]][1,3],parasites[[j]][,cols[i]+1],
         axes = FALSE, xlab="",ylab="",col=colours[1],pch=pch,
         xlim=xlim,ylim=ylim)
  }
}
dev.off()