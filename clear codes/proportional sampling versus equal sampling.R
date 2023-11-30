# Equal sampling versus proportional sampli
# Load data and calculate the abundance per 1g if leaves then reduce based on least abundant
# arthropod species 

setwd("C:\\R\\how simple are tropical food webs")
wpspecies <- read.table("WPspecies.txt", header = TRUE, sep = "\t")
wsspecies <- read.table("WSspecies.txt", header = TRUE, sep = "\t")
wp.raw <- wpspecies[,2:511]
ws.raw <- wsspecies[,2:511]
lw.primary <- wpspecies$MatureLeafTotalWeight + wpspecies$YoungLeafTotalWeight
lw.secondary <- wsspecies$MatureLeafTotalWeight + wsspecies$YoungLeafTotalWeight

# Raw values divided by pland mass (gives [ind/unit of weight])
wp.prop <- wp.raw / matrix(lw.primary, ncol = dim(wp.raw)[2], nrow = dim(wp.raw)[1])
ws.prop <- ws.raw / matrix(lw.secondary, ncol = dim(ws.raw)[2], nrow = dim(ws.raw)[1])

###Removal of the zero entries
wp.prop <- wp.prop[rowSums(wp.prop, na.rm = TRUE)>0, ]
ws.prop <- ws.prop[rowSums(ws.prop, na.rm = TRUE)>0, ]
wp.prop <- wp.prop[, colSums(wp.prop, na.rm = TRUE)>0]
ws.prop <- ws.prop[, colSums(ws.prop, na.rm = TRUE)>0]

sec.abu.sum <- sum(ws.prop)
prim.abu.sum <-sum(wp.prop)

plot(sort(colSums(ws.prop)))

#Ordering the matrix by columns...
ws.prop.org <- ws.prop[, order(colSums(ws.prop), decreasing = TRUE)]
wp.prop.org <- wp.prop[, order(colSums(wp.prop), decreasing = TRUE)]

# ...and by rows.
ws.prop.org <- ws.prop.org[order(rowSums(ws.prop.org), decreasing = TRUE), ]
wp.prop.org <- wp.prop.org[order(rowSums(wp.prop.org), decreasing = TRUE), ]

plot(colSums(wp.prop.org))
plot(rowSums(wp.prop.org))

# Reducing function

calculate.proportion <- function(dataset, full){
  proportion <- sum(dataset)/full
  return(proportion)
}

### Budowanie funkcji

# Mamy uporzadkowane macierze od najbardziej zageszczonej do najrzadziej zag herbivores



secondary.result <- reduce(ws.prop.org, 2)
plot(rev(secondary.result[,2]), secondary.result[,1], 
     ylab = "Proportion of total arthropod abundance", 
     xlab = "Number of arthropod species removed")
secondary.result.plants <- reduce(ws.prop.org, 1)
plot(rev(secondary.result.plants[,2]), secondary.result.plants[,1], 
     ylab = "Proportion of total arthropod abundance", 
     xlab = "Number of plant species removed")

primary.result <- reduce(wp.prop.org, 2)
primary.result.plants <- reduce(wp.prop.org, 1)

###
# Equal sampling



##### Proportional sampling
# Proportion of speces removed for secondary fores

windows(400,400)
par(mfrow = c(2,2))
secondary.result.prop <- secondary.result[,2]/secondary.result[1,2]
secondary.result.plants.prop <- secondary.result.plants[,2]/secondary.result.plants[1,2]
plot(secondary.result[,1]~rev(secondary.result.prop), 
     #ylab = "Proportion of total arthropod abundance", 
     #xlab = "Proportion of plant species removed", 
     ylab = "",
     xlab = "",
     axes = FALSE)
par(new = TRUE)
plot(secondary.result.plants[,1]~rev(secondary.result.plants.prop), 
     ylab = "Proportion of total arthropod abundance", 
     xlab = "Proportion of species removed", col = "red")
legend(0, 0.6, pch = c(1,1),
       col = c("black", "red"), 
       c("Arthropod species removed", "Plant species removed"))
text(0.2, 0.8, c("Secondary forest"))

#Primary removal and plots
primary.result.prop <- primary.result[,2]/primary.result[1,2]
primary.result.plants.prop <- primary.result.plants[,2]/primary.result.plants[1,2]
plot(primary.result[,1]~rev(primary.result.prop), 
     #ylab = "Proportion of total arthropod abundance", 
     #xlab = "Proportion of plant species removed", 
     ylab = "",
     xlab = "",
     axes = FALSE)
par(new = TRUE)
plot(primary.result.plants[,1]~rev(primary.result.plants.prop), 
     ylab = "Proportion of total arthropod abundance", 
     xlab = "Proportion of species removed", col = "red")
legend(0, 0.6, pch = c(1,1),
       col = c("black", "red"), 
       c("Arthropod species removed", "Plant species removed"))
text(0.2, 0.8, c("Primary forest"))

############           ############
############ RAW DATA  ############
############           ############

# Remove emty rows etc
ws.raw <- ws.raw[,colSums(ws.raw)>0]
ws.raw <- ws.raw[rowSums(ws.raw)>0, ]
ws.raw <- ws.raw[,order(colSums(ws.raw), decreasing = TRUE)]
ws.raw <- ws.raw[order(rowSums(ws.raw), decreasing = TRUE),]

wp.raw <- wp.raw[,colSums(wp.raw)>0]
wp.raw <- wp.raw[rowSums(wp.raw)>0, ]
wp.raw <- wp.raw[,order(colSums(wp.raw), decreasing = TRUE)]
wp.raw <- wp.raw[order(rowSums(wp.raw), decreasing = TRUE),]

### <> PRIMARY REDUCED ARTHROPODS AND PLANTS

primary.reduced.arthropods <- reduce(pir,2)
primary.reduced.plants <- reduce(pir, 1)

prim.red.prop.art <- reduce(pip,2)
prim.red.prop.pla <- reduce(pip,1)

#primary.reduced.arthropods[,2] <- primary.reduced.arthropods[,2]/primary.reduced.arthropods[1,2]
#primary.reduced.plants[,2] <- primary.reduced.plants[,2]/primary.reduced.plants[1,2]


### Secondary forest reduction of species

#SECONDARY REDUSCED ARTHROPODS AND PLANTS

secondary.reduced.arthropods <- reduce(sir,2)
secondary.reduced.plants <- reduce(sir,1)

sec.red.prop.art <- reduce(sip,2)
sec.red.prop.pla <- reduce(sip,1)

  
###

  
secondary.reduced.arthropods[,2] <- secondary.reduced.arthropods[,2]/secondary.reduced.arthropods[1,2]
secondary.reduced.plants[,2] <- secondary.reduced.plants[,2]/secondary.reduced.plants[1,2]

###  #     ##  #####
#  # #    #  #   #
###  #    #  #   #
#    ####  ##    #

#Order before plotting

pip <- pip[order(rowSums(pip),decreasing = TRUE),
           order(colSums(pip),decreasing = TRUE)]
#plot(rowSums(pip))
#plot(colSums(pip))

pir <- pir[order(rowSums(pir),decreasing = TRUE),
           order(colSums(pir),decreasing = TRUE)]
sip <- sip[order(rowSums(sip),decreasing = TRUE),
           order(colSums(sip),decreasing = TRUE)]
sir <- sir[order(rowSums(sir),decreasing = TRUE),
           order(colSums(sir),decreasing = TRUE)]

#Dominance structure of a community
plot(colSums(sir)/sum(colSums(sir)), ylim = c(0,0.2), xlim = c(0,20))
par(new = TRUE)
plot(colSums(sip)/sum(colSums(sip)), col = "red", ylim = c(0, 0.2), xlim = c(0,20))


primary.reduced.arthropods <- reduce(pir,2)
primary.reduced.plants <- reduce(pir,1)
secondary.reduced.arthropods <- reduce(sir,2)
secondary.reduced.plants <- reduce(sir,1)
prim.red.prop.art <- reduce(pip,2)
prim.red.prop.pla <- reduce(pip,1)
sec.red.prop.art <- reduce(sip,2)
sec.red.prop.pla <- reduce(sip,1)

###RAW
windows(300,400)
par(mfrow = c(2,1))

# Plot of primary raw...should be with proportional
plot(primary.reduced.arthropods[,1]~(primary.reduced.arthropods[,2]),
     axes = FALSE, ylab = "", xlab = "", ylim = c(0,1), type = "l", lty = 1)
par(new = TRUE)
plot(primary.reduced.plants[,1]~(primary.reduced.plants[,2]),
     ylab = "", axes = FALSE, 
     xlab = "", ylim = c(0,1), xlim = c(0,1), 
     type = "l", lty = 1, col = "red")
par(new = TRUE)
plot(prim.red.prop.art[,1]~(prim.red.prop.art[,2]),
     axes = FALSE, ylab = "", xlab = "", ylim = c(0,1), type = "l", lty = 1, 
     lwd = 2)
par(new = TRUE)
plot(prim.red.prop.pla[,1]~(prim.red.prop.pla[,2]),
     ylab = "Proportion of total arthropod abundance", 
     xlab = "Proportion of species", ylim = c(0,1), xlim = c(0,1), 
     type = "l", lty = 1, lwd = 2, col = "red")

legend(0.4,0.6, c("Arthropods equal", "Plants equal ", "Arthropods proportional",
                "Plants proportional"),lty = c(1,1,1,1), lwd = c(1,1,2,2),
       col = c("black", "red", "black", "red"))
text(0.4, 0.8, "Primary forest")

# For secondary forest

plot(secondary.reduced.arthropods[,1]~(secondary.reduced.arthropods[,2]),
     axes = FALSE, ylab = "", xlab = "", ylim = c(0,1), type = "l", lty = 1)
par(new = TRUE)
plot(secondary.reduced.plants[,1]~(secondary.reduced.plants[,2]),
     ylab = "", axes = FALSE,
     xlab = "", ylim = c(0,1), xlim = c(0,1),
     type = "l", lty = 1, col = "red")
# PROPORTIONAL####<<<<<<<<<<<<<<<####<#<#<#<#<##<#<
par(new = TRUE)
plot(sec.red.prop.art[,1]~(sec.red.prop.art[,2]),
     axes = FALSE, ylab = "", xlab = "", ylim = c(0,1), type = "l", lty = 1,
     lwd = 2)
par(new = TRUE)
plot(sec.red.prop.pla[,1]~(sec.red.prop.pla[,2]),
     ylab = "Proportion of total arthropod abundance", 
     xlab = "Proportion of species", ylim = c(0,1), xlim = c(0,1),
     type = "l", lty = 1, lwd = 2, col = "red")
text(0.4, 0.8, "Secondary forest")

#######<#<#<<#<#<#<#<#<#<#<#<#<<#<#<#<


###### AGAIN EVERYTHING CLEAN AND ON THE ONE GRAPH

setwd("C:\\R\\how simple are tropical food webs")
wpspecies <- read.table("WPspecies.txt", header = TRUE, sep = "\t")
wsspecies <- read.table("WSspecies.txt", header = TRUE, sep = "\t")
wp.raw <- wpspecies[,2:511]
ws.raw <- wsspecies[,2:511]
lw.primary <- wpspecies$MatureLeafTotalWeight + wpspecies$YoungLeafTotalWeight
lw.secondary <- wsspecies$MatureLeafTotalWeight + wsspecies$YoungLeafTotalWeight

################################################ GOOOOOOOOODOODOODODOD
#Try to create new data withouth NA in them
#Clear data with interactions all code needed to create the dataset with prop and raw interactions
names(wpspecies)
wp.spec.weight <- wpspecies[,c(2:511, 522, 524)]
sum(is.na(wp.spec.weight))
wp.spec.weight <- wp.spec.weight[rowSums(wp.spec.weight)>0,]
sum(is.na(wp.spec.weight))

#p(rimary)i(interactions)r(raw)

pir.env <- wp.spec.weight[rowSums(wp.spec.weight[,1:510])>0, c(511,512)] #extracting weight rows
sum(is.na(wp.spec.weight))
weight.primary <- pir.env[,1] + pir.env[,2] #adding them for total weight
sum(is.na(weight.primary))
pir <- wp.spec.weight[,1:510] #only caterpillar plant data
sum(rowSums(pir)==0) #number of empty rows
pir <- pir[rowSums(pir)>0, ] #deletion of empty rows
pir <- pir[,colSums(pir)>0] #deletion of empty columns
dim(pir)
pmat <- matrix(weight.primary, ncol = dim(pir)[2], nrow = dim(pir)[1])
pip <- pir/pmat 
dim(pip)
sum(is.na(pip))
sum(colSums(pip)==0) # pip is dataset with proportions pir dataset with raw data

#### AND FOR SECONDARY

names(wsspecies)
dim(wsspecies)
ws.spec.weight <- wsspecies[,c(2:511, 522, 524)]
sum(is.na(ws.spec.weight))
ws.spec.weight <- ws.spec.weight[rowSums(ws.spec.weight)>0,] #Removal of zero rows from abundance and weight data
sum(is.na(ws.spec.weight))
#extracting weight rows where caterpilar abundance on trees is nonzero
sir.env <- ws.spec.weight[rowSums(ws.spec.weight[,1:510])>0, c(511,512)] 
dim(sir.env)
sum(is.na(ws.spec.weight))

weight.secondary <- sir.env[,1] + sir.env[,2] #adding them for total weight
sum(is.na(weight.secondary))

sir <- ws.spec.weight[,1:510] #only caterpillar plant data
dim(sir)
sum(is.na(sir))
sum(rowSums(sir)==0)
sum(colSums(sir)==0)#number of empty rows
sir <- sir[rowSums(sir)>0, ] #deletion of empty rows
sir <- sir[,colSums(sir)>0] #deletion of empty columns
#delete the wrong enterence in the 26th row
sum(sir[26,])
sum(weight.secondary[26])
sum(weight.secondary)
weight.secondary <- weight.secondary[-26]
length(weight.secondary)
dim(sir)
sum(sir)
sir <- sir[-26, ]
dim(sir) #Till this moment everything is in order
# Creating a matrix with weights of leaves
smat <- matrix(weight.secondary, ncol = dim(sir)[2], nrow = dim(sir)[1])
sip <- sir/smat 
dim(sip)
sum(is.na(sip))
sum(colSums(pip)==0)

######## ENDDNENDNENENE


# Raw values divided by pland mass (gives [ind/unit of weight])
#wp.prop <- wp.raw / matrix(lw.primary, ncol = dim(wp.raw)[2], nrow = dim(wp.raw)[1])
#ws.prop <- ws.raw / matrix(lw.secondary, ncol = dim(ws.raw)[2], nrow = dim(ws.raw)[1])
#wp.prop[is.na(wp.prop)] <- 0
#ws.prop[is.na(ws.prop)] <- 0

###Removal of the zero entries
#wp.prop <- wp.prop[rowSums(wp.prop, na.rm = TRUE)>0, ]
#ws.prop <- ws.prop[rowSums(ws.prop, na.rm = TRUE)>0, ]
#wp.prop <- wp.prop[, colSums(wp.prop, na.rm = TRUE)>0]
#ws.prop <- ws.prop[, colSums(ws.prop, na.rm = TRUE)>0]
#wp.raw <- wp.raw[,colSums(wp.raw, na.rm = TRUE)>0]
#wp.raw <- wp.raw[rowSums(wp.raw, na.rm = TRUE)>0, ]
#ws.raw <- ws.raw[,order(colSums(ws.raw, na.rm = TRUE), decreasing = TRUE)]
#ws.raw <- ws.raw[order(rowSums(ws.raw, na.rm = TRUE), decreasing = TRUE),]

# Ordering the matrix
# Primary raw
#wp.raw <- wp.raw[order(rowSums(wp.raw), decreasing = TRUE),] #Order plants (rows)
#wp.raw <- wp.raw[, order(colSums(wp.raw), decreasing = TRUE)] # Order columns (species)
# Primary prop
#wp.prop <- wp.prop[order(rowSums(wp.prop), decreasing = TRUE),] #Order plants (rows)
#wp.prop <- wp.prop[, order(colSums(wp.prop), decreasing = TRUE)] # Order columns (species)

reduce <- function(dataset, margin){
  # This function takes data matrix ordered by rows and by columns and reuces
  # it by removing least abundant components from given margin and calculates
  # proportion of abundance left.
  ws.temp <- dataset
  abu <- sum(dataset)
  prop <- matrix(0, nrow = dim(dataset)[margin], ncol = 2)
  prop [1, ] <- c(1, dim(dataset)[margin])
  for (i in 1:(dim(dataset)[margin]-1)){
    #print(i)
    #print(dim(ws.temp)[2] - i)
    if (margin == 2) {
      ws.temp <- ws.temp[, -dim(ws.temp)[margin]]
      res <- sum(ws.temp)/abu
      prop[i + 1, ] <- c(res, dim(as.matrix(ws.temp))[margin])
    }
    else{
      ws.temp <- ws.temp[-dim(ws.temp)[margin], ]
      res <- sum(ws.temp)/abu
      prop[i + 1, ] <- c(res, dim(ws.temp)[margin])
    }
    
  }
  prop[,2] <- prop[,2]/prop[1,2]
  return(prop)
}

# Analysis and plotting primary raw
#Primary raw arthropods
windows(400,300)
prim.raw.arthropods <- reduce(wp.raw, 2) # computation
prim.raw.arthropods[,2] <- prim.raw.arthropods[,2]/prim.raw.arthropods[1,2]
plot(prim.raw.arthropods[,1]~prim.raw.arthropods[,2],
     xlab = "", 
     ylab = "",
     axes = FALSE, xlim = c(0,1), ylim = c(0,1),
     type = "l", lty = 1, col = "black")
#Primary raw plants
prim.raw.plants <- reduce(wp.raw, 1)
prim.raw.plants[,2] <- prim.raw.plants[,2]/prim.raw.plants[1,2]
par(new = TRUE)
plot(prim.raw.plants[,1]~prim.raw.plants[,2],
     xlab = "", 
     ylab = "",
     axes = FALSE, xlim = c(0,1), ylim = c(0,1),
     type = "l", lty = 2, col = "black")


# Analysis and plotting primary proportional
prim.prop.arthropods <- reduce(wp.prop,2)
prim.prop.arthropods[,2] <- prim.prop.arthropods[,2]/prim.prop.arthropods[1,2]
par(new = TRUE)
plot(prim.prop.arthropods[,1]~prim.prop.arthropods[,2],
     xlab = "", 
     ylab = "",
     axes = FALSE, xlim = c(0,1), ylim = c(0,1),
     type = "l", lty = 1, col = "red")

prim.prop.plants <- reduce(wp.prop,1)
prim.prop.plants[,2] <- prim.prop.plants[,2]/prim.prop.plants[1,2]
par(new = TRUE)
plot(prim.prop.plants[,1]~prim.prop.plants[,2],
     xlab = "Proportion of species", 
     ylab = "Proportion of arthropod abundance",
     xlim = c(0,1), ylim = c(0,1),
     type = "l", lty = 2, col = "red")
legend(0.4,0.6, c("Primary Arthropods Raw", 
                  "Primary Plants Raw",
                  "Primary Arthropods Proportional", 
                  "Primary Plants Proportional"),
       lty = c(1,2,1,2), col = c("black", "black", "red", "red"))

##### SECONDARY SHOULD BE DONE IN THE SAME MANNER
##### BUT NOW I WILL MOVE TO INTERACTIONS IN PRIMARY FOREST

reduce.interactions <- function(dataset){
  wp.temp <- dataset
  no.of.interaction <- sum(dataset>0)
  print(no.of.interaction)
  result <- matrix(0, nrow = no.of.interaction, ncol = 2)
  result[1,] <- c(1,1)
  for (i in 1:(no.of.interaction-1)){
    print(no.of.interaction - i)
    coord <- which(wp.temp == min(wp.temp[wp.temp>0]), arr.ind = TRUE)
    print(wp.temp[coord[1,1], coord[1,2]])
    wp.temp[coord[1,1], coord[1,2]] <- 0
    #Count the interactions
    arthropods.abundance <- sum(wp.temp)
    arthropods.abundance.prop <- arthropods.abundance/sum(dataset)
    result[i + 1, ] <- c(arthropods.abundance.prop, (no.of.interaction - i)/no.of.interaction)
  }
  return(result)
}

prim.raw.int <- reduce.interactions(pir)
windows(400,300)
plot(prim.raw.int[,1]~prim.raw.int[,2], xlim = c(0,1), ylim = c(0,1),
     ylab = "", xlab = "", type = "l", lty = 1, col = "black", axes = FALSE)

prim.prop.int <- reduce.interactions(pip)
par(new = TRUE)
plot(prim.prop.int[,1]~prim.prop.int[,2],xlim = c(0,1), ylim = c(0,1),
     ylab = "", xlab = "", 
     type = "l", lty = 2, col = "black", axes = FALSE)

sec.raw.int <- reduce.interactions(sir)
sec.prop.int <- reduce.interactions(sip)

par(new = TRUE)
plot(sec.raw.int[,1]~sec.raw.int[,2], xlim = c(0,1), ylim = c(0,1),
     ylab = "", xlab = "", type = "l", lty = 1, col = "red", axes = FALSE)
par(new = TRUE)
plot(sec.prop.int[,1]~sec.prop.int[,2],xlim = c(0,1), ylim = c(0,1),
     ylab = "Abundance of arthropods", xlab = "Proportion of interactions", 
     type = "l", lty = 2, col = "red", axes = TRUE)
legend(0.3,0.4, c("Primary equal sampling", "Primary proportional sampling",
                  "Secondary equal sampling", "Secondary proportional sampling"),
       lty = c(1,2,1,2), col = c("black", "black", "red", "red"))

