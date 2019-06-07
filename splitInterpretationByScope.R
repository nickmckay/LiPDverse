

splitInterpretationByScope <- function(TS){

#split TS by interpretation
library(stringr)


sTS <- TS

for(i in 1:length(TS)){
  #how many interpretations?
  mts <- TS[[i]]
  tsNames <- names(mts)
  iNames <- tsNames[which(grepl("interpretation[0-9]",tsNames))]
  
  if(length(iNames) > 0){#there are some
  snames <- str_split(iNames,"_")
  upref <- unique(sapply(snames,"[[",1))
  ro <- str_split(upref,"interpretation")
  maxIntNum <- max(as.numeric(sapply(ro,"[[",2)))
  lenIntNum <- length(upref)
  
  
  
  if(maxIntNum==lenIntNum){
    nInt <- lenIntNum
  }else{
    stop("theres a discrepancy about the number of interpretations")
  }
  
  scopeType <- as.data.frame(matrix(NA,nrow = nInt,ncol = 2))
  for(n in 1:nInt){
  thisScope <- mts[[str_c("interpretation",as.character(n),"_scope")]]
  if(is.null(thisScope)){
    #normally
    #stop("All interpretations must have a scope")
    thisScope <- "climate"
  }
  
  scopeType[n,1] <- thisScope
  
  
    if(n==1){
      scopeType[n,2] <- 1
    }else{#count how many others match this scope
      prev <- sum(thisScope==scopeType[,1],na.rm = T)
      scopeType[n,2] <- prev
    }
    
  
    #loop through all the variables and assign
  sch <- str_c("interpretation",as.character(n))
  ti <-  tsNames[grepl(sch,tsNames)]
  #remove scope from this
 # ti <- ti[-which(ti==str_c(sch,"_scope"))]
  
  
  #add them in
  for(v in ti){
    thisVar <- str_remove(v,sch)
    mts[[str_c(scopeType[n,1],"Interpretation",as.character(scopeType[n,2]),thisVar)]] <- mts[[v]]
    mts[[v]] <- NULL
  }
  
    }#end loop through interpretations
  
  }#end Has interpretations
  sTS[[i]] <- mts
}#end loop through TS

return(sTS)
}


#combineInterpretationByScope
combineInterpretationByScope <- function(sTS){
  
  cTS <- sTS
  scopes = c('climate','isotope','ecology','chronology');
  
  for(i in 1:length(sTS)){
    mts <- sTS[[i]]
    
    fnames <- names(mts)
    #remove any preexisting fields
    interpNames <- str_which(fnames,"interpretation[0-9]")
    if(length(interpNames>0)){
      print("Removing 'interpretationX_*' fields, which we are about to create...")
      mts[interpNames] <- NULL
    }
    fnames <- names(mts)
    ti <- 0
    for(s in scopes){
      sts <- str_c(s,'Interpretation')
      
      #how many interpertaions with this scope
      
      nInterp <- length(unique(na.omit(str_extract(fnames,str_c(sts,"[0-9]")))))
      if(nInterp>0){
        for(ni in 1:nInterp){
          ti <- ti+1 #new interp number
          
          inames <- fnames[str_which(fnames,str_c(sts,as.character(ni),"_"))]
          for(ini in inames){
            varName <- str_remove(ini,str_c(sts,as.character(ni)))
            newName <- str_c("interpretation",as.character(ti),varName)
            mts[[newName]] <- mts[[ini]]
            mts[[ini]] <- NULL
            
          }
          mts[[str_c("interpretation",as.character(ti),"_scope")]] <- s
          
        }
      }
      
    }
    cTS[[i]] <- mts
  }
  
  return(cTS)
}  
