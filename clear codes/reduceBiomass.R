reduceBiomass <- function(COM, type, dec.point.acc = 1, 
                          proportional = FALSE, randomization = 10, treshold = 2){
  # Community dataset with the same names as the interaction network
  # Acuuracy dec.point.accu rounds the community biomass values to that number.
  # At each iteration network decriptors are calculated using ... function from 
  # the bipartite package.
  # Returns the order of species removal that can be later feed to the reduceFW function
  
  # 1. Process input
  unit <- 10^-dec.point.acc
  if (length(dim(COM)) == 1){
    print("Community data set as a vector. Converting to data frame.")
    COM <- data.frame(names(COM), COM)
  }
  # Round the biomass values to a given accuracy
  COM[,2] <- round(COM[,2], dec.point.acc)
  print(paste("At the accuracy level", unit,
              "zero biomass values in:", as.character(COM[COM[,2]==0,1])))
  COM <- COM[!COM[,2]==0, ]
  COM[,1] <- as.character(COM[,1])
  
  # 2. Reduction algorithm
  # Randomly pick the species for the removal (all have equal prob to be picked for
  # the removal of one unit biomass)
  COM[,2] <- round(COM[,2],1)
  sequences <- matrix("", nrow = (dim(COM[,2])[1]-treshold), ncol = randomization)
  biomasses <- matrix(0, nrow = (dim(COM[,2])[1]-treshold), ncol = randomization)
  
  # Draw species to reduce
  for (n in 1:randomization){
    dummyCOM <- COM
    sequence <- vector("character",0)
    biomass <- vector("numeric", 0)
    print(paste("Iteration no.", n))
    while (dim(dummyCOM)[1] > treshold){
      specToReduce <- as.character(dummyCOM[sample(dim(dummyCOM)[1],1),1])
      # Reduce biomass data and check if the plant species biomass is equal to zero
      rowNo <- which(dummyCOM[,1] %in% specToReduce)
      dummyCOM[rowNo,2] <- dummyCOM[rowNo,2] - unit
      if (dummyCOM[rowNo,2]<=0){
        #print(paste(as.character(dummyCOM[rowNo,1]), "was removed."))
        sequence <- c(sequence,as.character(dummyCOM[rowNo,1]))
        biomass <- c(biomass,sum(dummyCOM[,2]))
        dummyCOM <- dummyCOM[-rowNo, ]
      }
    }
    sequences[,n] <- sequence
    biomasses[,n] <- biomass
  }
  return(list(sequences,biomasses))
}

reduceFW <- function(INT, sequences, index = "number of species"){
  
  require(bipartite)
  
  rows <- dim(sequences[[1]])[1] # No of rows in sequences
  cols <- dim(sequences[[1]])[2] # No of columns in sequences
  indices <- length(networklevel(INT, index = index)) # Number of outputs
  
  # Matrix of the simplification results
  indRedMat <- matrix(nrow = rows, ncol = cols*indices)
  # For each randomization create dummy set and simplfy
  for (seq in 1:cols){
    print(paste("Randomization no.", seq))
    # Reduce the network species by species
    dummyINT <- INT
    indRun <- matrix(nrow = rows, ncol = indices)
    count <- 1
    for (spec in sequences[[1]][,seq]){
      specToRemove <- which(rownames(dummyINT) == spec)
      # Remove the species from the list
      dummyINT <- dummyINT[-specToRemove, ]
      # Remove empty columns
      dummyINT <- dummyINT[, colSums(dummyINT) != 0]
      # Calculate statistics
      indRun[count, ] <- networklevel(dummyINT, index = index)
      count <- count+1
    }
    col <- 2*seq-1
    indRedMat[, col:(col+1)] <- indRun
  }
  return(indRedMat)
}

## Indices for the function networklevel:
# ‘connectance’,
# ‘web asymmetry’,
# ‘links per species’,
# ‘number of compartments’,
# ‘compartment diversity’,
# ‘cluster coefficient’, which will compute both the network-wide cluster coefficient as well as those for each level,
# ‘nestedness’,
# ‘weighted nestedness’,
# ‘weighted NODF’,
# ‘ISA’ (or alternatively ‘interaction strength asymmetry’ or
#  ‘dependence asymmetry’),
# ‘SA’ (or alternatively ‘specialisation asymmetry’),
# ‘linkage density’,
# ‘weighted connectance’,
# ‘Fisher alpha’,
# ‘interaction evenness’,
# ‘Alatalo interaction evenness’,
# ‘Shannon diversity’,
# ‘H2’;
# ‘number of species’ in the respective trophic level,
# ‘mean number of links’,
# ‘mean number of shared partners’,
# ‘weighted cluster coefficient’,
# ‘degree distribution’,
# ‘togetherness’,
# ‘C score’,
# ‘V ratio’,
# ‘discrepancy’,
# ‘extinction slope’,
# ‘robustness’,
# ‘niche overlap’,
# ‘generality’,
# ‘vulnerability’,72 networklevel
# ‘fc’ (or alternatively ‘functional complementarity’).

makeIndices <- function(size){
  indices <- c()
  for (i in 1:size){
    indices <- c(indices, (2*i - 1))
  }
  return(indices)
}
