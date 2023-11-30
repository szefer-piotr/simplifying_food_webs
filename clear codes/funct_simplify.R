library(bipartite)

calcChar <- function(dataset){
  # Helper function that calculates focal parameters
  GenVul=networklevel(dataset, index="vulnerability")
  results <- c("Abu"=sum(dataset),
               "Row"=sum(rowSums(dataset)>0),
               "Col"=sum(colSums(dataset)>0),
               "Int"=sum(dataset>0),
               "H2" =networklevel(dataset, index="H2"),
               "Gen"=GenVul[1],
               "Vul"=GenVul[2]
  )
  return(results)
}

simplify <- function(dataset, edge = "ind", n = 10, reduction.level=0.5){
  # Function takes interaction matrix, randomise it and reduce it
  if       (edge == "ind")   {res.rows <- sum(dataset)
  }else if (edge == "row") {res.rows <- dim(dataset)[1]
  }else if (edge == "col") {res.rows <- dim(dataset)[2]
  }else if (edge == "int") {res.rows <- sum(dataset>0)
  }else    {print("Specify the edge: 'row, 'col', 'ind', 'int'")}
  results <- matrix(0, nrow=res.rows+1, ncol=15)
  results[,1] <- 0:res.rows
  colnames(results) <- c("Obj", "AbuOrd", "AbuRnd", "RowSpO", "RowSpR",
                         "ColSpO", "ColSpR","IntOrd","IntRnd", "H2Ord",
                         "H2Rnd", "GenOrd","GenRnd","VulOrd","VulRnd")
  ord.cols <- c(2,4,6,8,10,12,14)
  rnd.cols <- c(3,5,7,9,11,13,15)
  
  # calculations for the progress bar
  ####################################
  maxi <- round(res.rows*reduction.level,0)
  maxj <- n
  print(paste("Number of randomisation=",n,". Maxj=",maxj))            #DEBUG
  print(paste("Number of interaction=",res.rows,". Maxi=", maxi))      #DEBUG
  pbOrd <- txtProgressBar(min = 0, max = maxi, style = 3)
  pb <- txtProgressBar(min = 0, max = maxi*maxj, style = 3)
  ####################################
  
  if      (edge=="ind"){
    
    # Individuals module
    
    # Ordered individuals
    print("Reduction")
    
    temp.dat<-dataset # temporal dataset
    results[1,ord.cols]<-calcChar(temp.dat)
    
    for (i in 1:maxi){
      
      ####################################
      # Reduction process progress bar
      setTxtProgressBar(pbOrd, i)
      ####################################
      
      lowest.int <- which(temp.dat==min(temp.dat[temp.dat>0]),arr.ind=T)
      row <- lowest.int[1,1]
      col <- lowest.int[1,2]
      temp.dat[row,col] <- temp.dat[row,col]-1
      results[i+1, ord.cols] <- calcChar(temp.dat)
    }
    close(pb)
    
    # Randomised individuals
    print("Randomisation")
    
    for(j in 1:n){
      temp.dat<-dataset
      randomisation.results <- matrix(0, ncol = 7, nrow = res.rows+1)
      for (i in 1:maxi){
        
        ####################################
        # Randomisation process progress bar
        progres <- ((j-1)*maxi)+i
        setTxtProgressBar(pb, progres)
        ####################################
        
        nonzero.int <- which(temp.dat>0, arr.ind=T)
        rnd.int <- sample(dim(nonzero.int)[1], 1)
        row <- nonzero.int[rnd.int,1]
        col <- nonzero.int[rnd.int,2]
        temp.dat[row, col] <- temp.dat[row, col] - 1
        randomisation.results[i+1,] <- calcChar(temp.dat)
      }
      results[,rnd.cols] <- results[,rnd.cols]+randomisation.results
    }
    results[,rnd.cols]  <- results[,rnd.cols]/n
    results[1,rnd.cols] <- calcChar(temp.dat)
    close(pb)
    
  }else if(edge == "int"){
    
    # Interactions module
    
    # Ordered interactions
    print("Reduction")
    
    temp.dat<-dataset # temporal dataset
    results[1,ord.cols]<-calcChar(temp.dat)
    for (i in 1:maxi){
      
      ####################################
      # Reduction process progress bar
      setTxtProgressBar(pbOrd, i)
      ####################################
      
      lowest.int <- which(temp.dat==min(temp.dat[temp.dat>0]),arr.ind=T)
      row <- lowest.int[1,1]
      col <- lowest.int[1,2]
      temp.dat[row,col] <- 0
      results[i+1, ord.cols] <- calcChar(temp.dat)
    }
    
    # Ramdomised interactions
    print("Randomisation")
  
    for (j in 1:maxj){
      temp.dat<-dataset
      randomisation.results <- matrix(0, ncol = 7, nrow = res.rows+1)
      for (i in 1:maxi){
        
        ####################################
        # Randomisation process progress bar
        progres <- ((j-1)*maxi)+i
        setTxtProgressBar(pb, progres)
        ####################################
        
        nonzero.int <- which(temp.dat>0, arr.ind=T)
        rnd.int <- sample(dim(nonzero.int)[1], 1)
        row <- nonzero.int[rnd.int,1]
        col <- nonzero.int[rnd.int,2]
        temp.dat[row, col] <- 0
        randomisation.results[i+1,] <- calcChar(temp.dat)
      }
      results[,rnd.cols] <- results[,rnd.cols]+randomisation.results
    }
    results[,rnd.cols]  <- results[,rnd.cols]/n
    results[1,rnd.cols] <- calcChar(dataset)
    close(pb)
  }else if (edge == "row"){
    
    # Rows module
    
    # Ordered rows
    print("Reduction")
    
    temp.dat<-dataset # temporal dataset
    results[1,ord.cols]<-calcChar(temp.dat)
    for (i in 1:maxi){
      
      ####################################
      # Reduction process progress bar
      setTxtProgressBar(pbOrd, i)
      ####################################
      
      row.sums <- rowSums(temp.dat)
      row.to.delete <- which(row.sums==min(row.sums[row.sums>0]))
      temp.dat <- temp.dat[-row.to.delete[1],]
      results[i+1, ord.cols] <- calcChar(temp.dat)
    }
    
    # Ramdomised rows
    print("Randomisation")
    
    for (j in 1:maxj){
      temp.dat<-dataset
      randomisation.results <- matrix(0, ncol = 7, nrow = res.rows+1)
      for (i in 1:maxi){
        
        ####################################
        # Randomisation process progress bar
        progres <- ((j-1)*maxi)+i
        setTxtProgressBar(pb, progres)
        ####################################
        
        row.sums <- rowSums(temp.dat)
        rows.to.sample <- which(row.sums>0)
        rnd.row <- sample(rows.to.sample, 1)
        temp.dat <- temp.dat[-rnd.row,]
        randomisation.results[i+1,] <- calcChar(temp.dat)
      }
      results[,rnd.cols] <- results[,rnd.cols]+randomisation.results
    }
    results[,rnd.cols]  <- results[,rnd.cols]/n
    results[1,rnd.cols] <- calcChar(dataset)
    close(pb)
    
  }else if (edge == "col"){
    
    # Columns module
    
    # Ordered columns
    print("Reduction")
    
    temp.dat<-dataset # temporal dataset
    results[1,ord.cols]<-calcChar(temp.dat)
    for (i in 1:maxi){
      
      ####################################
      # Ordering process progress bar
      setTxtProgressBar(pbOrd, i)
      ####################################
      
      col.sums <- colSums(temp.dat)
      columns.to.delete <- which(col.sums==min(col.sums[col.sums>0]))
      temp.dat <- temp.dat[,-columns.to.delete[1]]
      results[i+1, ord.cols] <- calcChar(temp.dat)
    }
    
    # Ramdomised columns
    print("Randomisation")
    
    for (j in 1:maxj){
      temp.dat<-dataset
      randomisation.results <- matrix(0, ncol = 7, nrow = res.rows+1)
      for (i in 1:maxi){
        
        ####################################
        # Randomisation process progress bar
        progres <- ((j-1)*maxi)+i
        setTxtProgressBar(pb, progres)
        ####################################
        
        col.sums <- colSums(temp.dat)
        columns.to.sample <- which(col.sums>0)
        rnd.column <- sample(columns.to.sample, 1)
        temp.dat <- temp.dat[,-rnd.column]
        randomisation.results[i+1,] <- calcChar(temp.dat)
      }
      results[,rnd.cols] <- results[,rnd.cols]+randomisation.results
    }
    results[,rnd.cols]  <- results[,rnd.cols]/n
    results[1,rnd.cols] <- calcChar(dataset)
    close(pb)
  }
  return(results)
}

############### END OF THE FUNCTION ##########################


# Test
A <- matrix(round(rexp(12*7, rate=25)*100,0),nrow=7,ncol=12)

RES <- simplify(A, edge="ind", n = 10, reduction.level = 0.995)
RES <- simplify(A, edge="int", n = 10, reduction.level = 0.99)
RES <- simplify(A, edge="row", n = 100, reduction.level = 0.7)
RES <- simplify(A, edge="col", n = 100, reduction.level = 0.7)

dim(semi)
semi <- t(semi)

ind.semi <- simplify(semi, edge = "row", n=20, reduction.level = 0.9)
