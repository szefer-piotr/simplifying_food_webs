# Szefer et al. Dominant species of insects in tropical rainforest food webs

# Datasets
source("C:/Users/szefe/Desktop/Work/extrapolation/clear codes/data_processing_biomass.R")

# Function for the simplifications
source("C:/Users/szefe/Desktop/Work/extrapolation/clear codes/reduceBiomass.R")

# Reduce biomass randomly and get the reduction sequences based on biomass.
s020p <- reduceBiomass(yPlantBioP, dec.point.acc = 1, randomization = 20)

#Morris, R. J., Lewis, O. T. and Godfray, H. C. J. (2004) Experimental evidence for apparent competition in a tropical forest food web. Nature 428, 310–313
meansS020p <- apply(s020p[[2]], 1, mean)
sd020p <- apply(s020p[[2]], 1, sd)

matplot(s020p[[2]], type = "p", pch = 19)

# Get the reduction results for the network descriptors
SpNo <- reduceFW(yIntMatP, sequences = s020p)
Vuln <- reduceFW(yIntMatP, sequences = s020p, index = "vulnerability")

biomass <- s020p[[2]]

Ind <- makeIndices(20)

insectRich <- SpNo[,Ind]
insectVuln <- Vuln[,Ind]
Generality <- Vuln[,Ind+1]

matplot(biomass, insectRich, type = "l")
abline(v = sum(yPlantBioP)/2)
matplot(biomass, insectVuln, type = "l", main="Average number of Herbivores per plant")
matplot(biomass, Generality, type = "l", main="Average number of Plants per Herbivore")

# PLots with the highlited network containig only 50 percent of the biomass.
# How can i incorporate the abundance/biomass of the resources in the dynamics of
# the food web.

# Which species yield 50% of the biomass? How they are connected compared with the rest?
cumsum(yPlantBio)
sum(yPlantBio)/2

# Primary plants degree
degreePlantsP <- rowSums(yIntMatP>0)
degreePlantsS <- rowSums(yIntMatS>0)

plantNamesPdeg <- names(degreePlantsP)
plantNamesPabu <- names(yPlantBioP)

plantNamesSdeg <- names(degreePlantsS)
plantNamesSabu <- names(yPlantBioS)


alphaOrderP <- order(plantNamesPabu)
alphaOrderS <- order(plantNamesSabu)

plot(degreePlantsS~log(yPlantBioS[alphaOrderS]))
plot(degreePlantsP~log(yPlantBioP[alphaOrderP]))

# Visualization of the 50% most abundant plants and their interactions
library(igraph)

# Primary graph
subMat1 <- yIntMatP
subMat2 <- matrix(0, nrow = dim(yIntMatP)[1], ncol=dim(yIntMatP)[1])
colnames(subMat2) <- rownames(subMat1)
subMat3 <- matrix(0, nrow = dim(yIntMatP)[2], ncol=dim(yIntMatP)[2])
rownames(subMat3) <- colnames(subMat1)
subMat4 <- t(yIntMatP)

upper <- cbind(subMat1, subMat2)
lower <- cbind(subMat3, subMat4)

yIntGraph <- rbind(upper, lower)
dim(yIntGraph)

image(yIntGraph)

yIntGraph <- graph_from_adjacency_matrix(yIntGraph, weighted = T)

plot(yIntGraph, edge.arrow.size=0, vertex.size=5, vertex.color = "gray80", vertex.label = NA)
plot(yIntGraph, edge.arrow.size=0, vertex.size=5)

pdf("trialplot.pdf", width = 20, height = 20)
plot(yIntGraph, edge.arrow.size=0, vertex.size= log(c(yPlantBioP+1, rep(exp(1), dim(yIntMatP)[2]))))
dev.off()
#getwd()

# Bipartite trial

# Primary colour interactions and 
#pdf("bipartiteBiomassPrimary.pdf", width = 20, height = 8)

rownames(yIntMatP) # Are alphabetical
rownames(yPlantBioP) # Sorted according to biomass
yPlant50 <- yPlantBioP[cumsum(yPlantBioP) < sum(yPlantBioP)/2]

# Indices to colour
indToCol <- which(rownames(yIntMatP)%in%names(yPlant50))

lowerAbuCol <- rep("gray", dim(yIntMatP)[1])
lowerAbuCol[indToCol] <- "red"

intInd <- which(yIntMatP > 0, arr.ind = T)

# If the colours in bipartite plot go from up to down 
interactionPositions <- which(yIntMatP>0, arr.ind = T)
interactionPositions<- interactionPositions[order(interactionPositions[,1]),]

yIntMatPind <- yIntMatP

for (i in 1:dim(interactionPositions)[1]){
  row <- interactionPositions[i,1]
  col <- interactionPositions[i,2]
  yIntMatPind[row,col] <- i
}

yIntMatPind <- yIntMatPind[indToCol,]
yIntMatPind <- yIntMatPind[yIntMatPind>0]

interactionColours <- rep(NULL, length(interactionPositions))
interactionColours[yIntMatPind] <- "red"

upperAbuCol

bipartite::plotweb(yIntMatP, low.abun = yPlantBioP, low.abun.col = lowerAbuCol, 
                   arrow="down", text.rot=90) #, 
                   #col.interaction = interactionColours,bor.col.interaction = interactionColours)


#dev.off()

#pdf("bipartiteBiomassSecondary.pdf", width = 20, height = 8)
bipartite::plotweb(yIntMatS, low.abun = yPlantBioS, arrow="down", text.rot=90)
#dev.off()

