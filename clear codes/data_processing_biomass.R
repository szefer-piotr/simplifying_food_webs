# Data processing file for the exploration of the effect 
# of reduction of the plants biomass on arthropod communities

## 0. lad some helpfull stuff
source("C:/Users/szefe/Desktop/Work/garden experiment/supplementary/code/contingencyTable.R")

## 1. Read raw data.
setwd("C:/Users/szefe/Desktop/Work/extrapolation/datasets")
yawanInt  <- read.csv("yawan_interactions.csv")
yawanCom <- read.csv("yawan_plant_community.csv")

wanangInt <- read.csv("wanang_interactions.csv")
wanangCom <- read.csv("wanang_plant_community.csv")

# Separate into primary and secondary forests
# 1. Yawan
yawanComP <- yawanCom[yawanCom$TYPE == "P",]
yawanComS <- yawanCom[yawanCom$TYPE == "S",]
yawanIntP <- yawanInt[substr(yawanInt$sample_code_tree,2,2) == "P",]
yawanIntS <- yawanInt[substr(yawanInt$sample_code_tree,2,2) == "S",]
# 2. Wanang
wanangComP <- wanangCom[wanangCom$Locality_Code == "WP",]
wanangComS <- wanangCom[wanangCom$Locality_Code == "WS",]
wanangIntP <- wanangInt[substr(wanangInt$sample_code,2,2) == "P",]
wanangIntS <- wanangInt[substr(wanangInt$sample_code,2,2) == "S",]

## 2. Clear the data and preapare subsets. I need the plant biomass and interaction matrix.
### 2a.
# sp_code is herbivory names, herbivores are the upper level
wIntMatP <- contingencyTable2(wanangIntP, "code", "sp_code", "qty")
wIntMatS <- contingencyTable2(wanangIntS, "code", "sp_code", "qty")
# windows(400,600)
# par(mfrow = c(2,1))
# bipartite::plotweb(wIntMatP)
# bipartite::plotweb(wIntMatS)
# Plot
# pdf("wanang_network.pdf", width = 20, height = 8)
# bipartite::plotweb(wIntMat)
# dev.off()
# Create the proper code_names for the community dataset, so that they match the ionteractions dataset
# DBH is a measure of the biomass.
# Leaf biomass...?
wComDat <- wanangCom[,c("Genus_ID", "Species_AUTHOR","DBH")]
wComDat$PlantName <- paste(substr(wComDat[,1], 1,4),
                           substr(wComDat[,2], 1,4),
                           sep = "")
head(wComDat)
# I try to match the species names based on the collection numbers
ComLevels <- as.character(unique(wComDat$PlantName)) #265 levels
IntLevels <- as.character(unique(wanangInt$code))    #324 levels
# Plants in interaction network present in the community data.
IntLevels %in% ComLevels
# why things are missing???
# See which species are not there, are these non tree species
IntLevels[!(IntLevels %in% ComLevels)]


### 2b. And how about Yawan?
yIntMatP <- contingencyTable2(yawanIntP, "TreesGeorge", "field_code", "qty")
yIntMatS <- contingencyTable2(yawanIntS, "TreesGeorge", "field_code", "qty")
# windows(400,600)
# par(mfrow = c(2,1))
# bipartite::plotweb(yIntMatP)
# bipartite::plotweb(yIntMatS)
# Plot
# pdf("yawan_network.pdf", width = 50, height = 20)
# bipartite::plotweb(yIntMat)
# dev.off()

# Measures of biomass:
# DBH, BA, MatureLeafTotalWeight,
bm <- c("MatureLeafTotalWeight", "YoungLeafTotalWeight",
        "WoodSampleDryWeight", "WoodSampleVolume", 
        "DBH", "TrunkHeight", "BA")

yComDatP <- yawanComP[,c("Genus", "species", bm)]
yComDatP[is.na(yComDatP)] <- 0

yComDatS <- yawanComS[,c("Genus", "species", bm)]
yComDatS[is.na(yComDatS)] <- 0


# To get full biomass measures for trees i need leafes and trunk weight. 
# This is biomass for mature and young leafs. NA are 0's
yComDatP$EstBiomass <- yComDatP$MatureLeafTotalWeight + yComDatP$YoungLeafTotalWeight
yComDatS$EstBiomass <- yComDatS$MatureLeafTotalWeight + yComDatS$YoungLeafTotalWeight

# Wood weight can be estimated from the wood density (weight/volume) and trunk height.
# Or simply DBH can be used. But I would need to find how to calculate the biomass from it.
#woodWeight

# One more column with names that match the interaction dataset.
yComDatP$PlantName <- paste(yComDatP[,1], yComDatP[,2],
                           sep = "_")
yComDatS$PlantName <- paste(yComDatS[,1], yComDatS[,2],
                            sep = "_")

# Check if the interaction network is a ssubset of community dataset.
yComLevelsP <- as.character(unique(yComDatP$PlantName)) #180 sp
yComLevelsS <- as.character(unique(yComDatS$PlantName)) #180 sp

yIntLevelsP <- as.character(unique(yawanIntP$TreesGeorge)) #117 sp
yIntLevelsS <- as.character(unique(yawanIntS$TreesGeorge)) #117 sp

# Plants in interaction network present in the community data.
yIntLevelsP %in% yComLevelsP
yIntLevelsS %in% yComLevelsS
# all are there

# Biomass data
yBiomassDatP <- yComDatP[yComDatP$PlantName %in% yIntLevelsP, c("PlantName", "EstBiomass")]
yBiomassDatS <- yComDatS[yComDatS$PlantName %in% yIntLevelsS, c("PlantName", "EstBiomass")]


# Now I can get the cumulative biomass for all the plants
yAbundanceP <- tapply(yBiomassDatP$EstBiomass, yBiomassDatP$PlantName, sum, na.rm=TRUE)
yPlantBioP <- sort(yAbundanceP, decreasing=T)

yAbundanceS <- tapply(yBiomassDatS$EstBiomass, yBiomassDatS$PlantName, sum, na.rm=TRUE)
yPlantBioS <- sort(yAbundanceS, decreasing=T)

# Files to be removed so that only the proper files are loaded into memory with a description
# files <- ls()
# save <- c("yPlantBio", "yIntMat", "wPlantBio", "wIntMat")
rm(list = ls()[!(ls() %in% c("yPlantBioP", "yPlantBioS", "yIntMatP", "yIntMatS",
                             "wPlantBioP", "wIntMatP","wPlantBioS", "wIntMatS"))])

print("Loaded files for Wanang and Yawan:")
print("Interaction matrices: wIntMatP, yIntMatP,wIntMatS, yIntMatS")
print("Community biomass data frames: wPlantBioP, yPlantBioP, wPlantBioS, yPlantBioS")
