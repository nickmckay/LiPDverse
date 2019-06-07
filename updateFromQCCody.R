#update from QC sheet
library(googledrive)
library(tidyverse)
library(here)
library(lipdR)
library(geoChronR)
source('~/GitHub/lipdverse/splitInterpretationByScope.R')
D <- readLipd("/Users/npm4/Dropbox/HoloceneLiPDLibrary/")
TS <- extractTs(D)
sTS <- splitInterpretationByScope(TS)

#setup reporting
report <- c()
reportY <- c()
noMatch <- c()

#download qc sheet
setwd(here())
x <- drive_get(as_id("19HA3E4dJCU3ZrPlgibsRs5xIyPt41ISbfhxfdb4ucj4"))
qc <- drive_download(x,path = here("updated.csv"),type = "csv",overwrite = T)
qcs <- read_csv(here("updated.csv"))

#download name conversion
convo <- as_id("1T5RrAtrk3RiWIUSyO0XTAa756k6ljiYjYpvP67Ngl_w") %>% 
  drive_get() %>% 
  drive_download(path = here("convo.csv"),overwrite = T) %>% 
  select(local_path) %>%
  as.character() %>% 
  read_csv()

#convert names
qcNames <- names(qcs)
tsNames <- c()
for(i in 1:length(qcNames)){
  ind <- which(convo$qcSheetName %in% qcNames[i]) 
  if(length(ind)==0){
    print(qcNames[i])
  }else if(length(ind)==1){
    tsNames[i] <- convo$tsName[ind]
  }else{
    stop("multiple convo matches")
  }
}

#see if any qcsheet TSids are missing from the database
TSid <- pullTsVariable(sTS,"paleoData_TSid")
dsn <- pullTsVariable(sTS,"dataSetName")
missingTSid <- c()
for(i in 3:nrow(qcs)){
  if(!any(qcs$TSid[i] == TSid,na.rm = T) & !is.na(qcs$TSid[i])){
    missingTSid <- c(missingTSid,qcs$TSid[i])
  }
}

#loop through TS and test
newTS <- sTS

allNames <- unique(unlist(sapply(sTS,names)))
missingNames <- allNames[!allNames %in% convo$tsName]
missingNamesTS <- missingNames[order(missingNames)]

allNamesConvo <- convo$tsName
missingNamesConvo <- convo$tsName[!convo$tsName %in% allNames]

bnames <- c("geo","pub","funding","dataPub")
dsn <- sapply(sTS,"[[","dataSetName")
for(i in 1:length(sTS)){
  thisTSid <- sTS[[i]]$paleoData_TSid
  #find which QC row
  qci <- which(qcs$TSid %in% thisTSid)
  #if doesn't exist, report out:
  if(length(qci)==0){
    if(any(grepl(sTS[[i]]$paleoData_variableName,c("year","depth","age")))){
    report = rbind(report,str_c("TSid: ",thisTSid ," for variableName: ",sTS[[i]]$paleoData_variableName ," in dataset:",sTS[[i]]$dataSetName, " doesn't exist in QC sheet") )
    }else{
      reportY = rbind(reportY,str_c("TSid: ",thisTSid ," for variableName: ",sTS[[i]]$paleoData_variableName ," in dataset:",sTS[[i]]$dataSetName, " doesn't exist in QC sheet") )
    }
  }else if(length(qci)>1){#then too many matches
    stop()
    report = rbind(report,str_c("Too many matches for TSid: ",thisTSid ," for variableName: ",sTS[[i]]$paleoData_variableName ," in dataset:",sTS[[i]]$dataSetName) )
  }else{#loop through variables and force an update
    thisTSnames <- unique(c(names(sTS[[i]]),allNamesConvo)) #find names for this ts, combine with convo names for updates
    for(j in 1:length(thisTSnames)){
      rn <- which(tsNames %in% thisTSnames[j])
      # #ignore TS names without matches

      #find matches
      if(length(rn) == 1){
        #check type
        varType <- convo$type[which(convo$tsName %in% thisTSnames[j])]
        if(varType == "character"){
          varFun <- as.character
        }else if(varType == "numeric"){
          varFun <- as.numeric
        }else if(varType == "boolean"){
          varFun <- as.logical
        }else{
          stop("variable type not recognized")
        }
        #apply to all timeseries from this dataset?
        sname <- str_split(thisTSnames[j],"_")
        #start with false
        apply2all <- FALSE
        if(length(sname[[1]])==1){#then its base
          apply2all <- TRUE
        }else{
          base <- str_remove_all(sname[[1]][1],"[0-9]")
          apply2all <- any(grepl(base,bnames))
        }
        
        #fill it in.
        if(apply2all){#then fill it in for all in dataset
          #print(sname)
          dsni <- which(grepl(sTS[[i]]$dataSetName,dsn))
          for(k in 1:length(dsni)){
          newTS[[dsni[k]]][thisTSnames[j]] <- varFun(qcs[qci,rn])
          }
        }else{#then just for this one timeseries
          newTS[[i]][thisTSnames[j]] <- varFun(qcs[qci,rn])
        }
        
      }
      if(length(rn) > 1){
        stop("there shouldn't be multiple matches in ts names")
      }
    }
  }#end loop through variables and force an update
}

newCts <- combineInterpretationByScope(newTS)
Dnew <- collapseTs(newCts)

