RUNTIME_OPTION <- 1;
TRAIT_THRESHOLD <- 10;

TRYfn <- './DATA/TryAccSpecies.txt'
LOCALfn <- './DATA/K67_CleanSpecies_Subset.csv'
METAfn <- './DATA/K67_TRYMETA_tde202036145356.txt'
TRAITfn <- './DATA/8839.txt'

qryIDs = c(14,15,56,48,1096,185,3115);

if(RUNTIME_OPTION == 1){ 
    # Load in the TRY species list and the local species list
    TRYspecies <- read.csv(TRYfn, header = TRUE, sep = "\t")
    # Load in the Local species list 
    LocalSpecies <-  read.csv(LOCALfn,header = TRUE,sep = ",")
    
    #Find the common species
    comSet <- merge(x=TRYspecies,y=LocalSpecies,by.x = "AccSpeciesName",by.y = "spp")
    n1 <- comSet$AccSpeciesID;
    
    # Print output to requestTRYMeta
    sink('requestTRYMeta.txt')
    cat("Navigate to the following webpage: https://www.try-db.org/de/InfoBySpecies2.php.\n\n")
    cat("Copy and paste the following species IDs into TRY.\n\n")
    cat(n1,sep=", ")
    sink()
}

if(RUNTIME_OPTION == 2){
    # Load in the TRY species list and the local species list
    TRYspecies <- read.csv(TRYfn, header = TRUE, sep = "\t")
    # Load in the Local species list 
    LocalSpecies <-  read.csv(LOCALfn,header = TRUE,sep = ",")
    
    #Find the common species
    comSet <- merge(x=TRYspecies,y=LocalSpecies,by.x = "AccSpeciesName",by.y = "spp")
    n1 <- comSet$AccSpeciesID;
    
    # Read in the downloaded TRY Meta data (currently 1 file support)
    TRYMeta <- read.csv(METAfn,sep="\t",skip = 3)
    
    # Generate meta statistics
    metaStats <- data.frame("Trait"=TRYMeta$Trait,"TraitID"=TRYMeta$TraitID)
    metaStats$traitSum <- rowSums((TRYMeta[,3:ncol(TRYMeta)]>0),na.rm=TRUE)
    metaStats$msmtSum <- rowSums(TRYMeta[,3:ncol(TRYMeta)],na.rm = TRUE)
    
    # Sort metadata by greatest number of traits and observations
    sortMetaTbl <- metaStats[order(-metaStats$traitSum,-metaStats$msmtSum),]
    
    # Write metatable to CSV
    write.csv(sortMetaTbl,file="TraitMetaData.csv",row.names = FALSE)
    
    ix <- which(metaStats$traitSum > TRAIT_THRESHOLD)
    n2 <- metaStats$TraitID[ix]
    
    #Write the data request file
    sink("reqestTRYData.txt")
    cat("Navigate to the TRY Data Request area (https://www.try-db.org/TryWeb/Prop1.php), login, and accept the terms. \n")
    cat("Copy and paste the following trait IDs into TRY.\n\n")
    cat(n1,sep=", ")
    cat("\n\n Next, copy and paste the following species IDs into TRY.\n\n")
    cat(n2,sep = ", ")
    sink()
}

if(RUNTIME_OPTION == 3){
    
    #Read in TRY data
    df <- read.csv(TRAITfn, header = TRUE, sep = '\t')
    
    i <- 1
    qryData <- list()
    traitStats <- data.frame("TraitID"=qryIDs,"min"= NA,"max"=NA,"mean"=NA,"median"=NA,"numSp"=NA,"numMsmt"=NA)
    for (val in qryIDs){
        ix <- which(df$TraitID == val)
        
        qryData[[i]] <- data.frame("StdValue"=df$StdValue[ix],"UnitName"=df$UnitName[ix],"AccSpeciesID"=df$AccSpeciesID[ix],"AccSpeciesName"=df$AccSpeciesName[ix])
        traitStats$min[i] <- min(qryData[[i]]$StdValue,na.rm = TRUE)
        traitStats$max[i] <- max(qryData[[i]]$StdValue,na.rm = TRUE)
        traitStats$mean[i] <- mean(qryData[[i]]$StdValue,na.rm = TRUE)
        traitStats$median[i] <- median(qryData[[i]]$StdValue,na.rm = TRUE)
        traitStats$numSp[i] <- length(unique(qryData[[i]]$AccSpeciesName))
        traitStats$numMsmt[i] <- length(qryData[[i]]$StdValue)
        
        i <- i+1
    }
    
    
    
}