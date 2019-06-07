#update from QC sheet
library(googledrive)
library(tidyverse)
library(here)
library(lipdR)
library(geoChronR)
source('~/GitHub/lipdverse/splitInterpretationByScope.R')

#D <- readLipd("/Users/npm4/Dropbox/LiPD/Holocene/")
D <- readLipd()
TS <- extractTs(D)
sTS <- splitInterpretationByScope(TS)
#save(list = c("D","TS","sTS"),file = here("html",project,str_c(project,".RData")))
#save(list = c("D","TS","sTS"),file = "/Users/npm4/GitHub/lipdverse/html/iso2kv0_10_0/iso2kv0_10_0.RData")
#setup reporting
report <- c()
noMatch <- c()
# 
#download qc sheet template
setwd(here())
x <- drive_get(as_id(qcSheetId))
qc <- drive_download(x,path = here("template.csv"),type = "csv",overwrite = T)
qcs <- read_csv(here("template.csv"))

#download name conversion
convo <- as_id("1T5RrAtrk3RiWIUSyO0XTAa756k6ljiYjYpvP67Ngl_w") %>% 
  drive_get() %>% 
  drive_download(path = here("convo.csv"),overwrite = T) %>% 
  select(local_path) %>%
  as.character() %>% 
  read_csv()

#filter ts
#only grab isotope data
varNames <- pullTsVariable(sTS,"paleoData_variableName")
uvn <- unique(varNames)

to.omit <- c("depth","age","year")#,"std","uncertainty")
toi <- c()
for(to in to.omit){
toi <- append(toi,which(grepl(to,uvn,ignore.case = T)))
}
toi <- unique(toi)

to.omit.specific <- c("yr")
for(to in to.omit){
  toi <- append(toi,which(to == uvn))
}

rem.var <- uvn[toi]

good.in <- which(!varNames %in% rem.var)

fsTS <- sTS[good.in]

#global holocene temp
#fsTS <- filterTs(TS,"paleoData_useHoloceneTemp == TRUE")
outRows <- length(fsTS)+1

allAge <- pullTsVariable(fsTS,"age")
allYear <- pullTsVariable(fsTS,"year")

toRep <- which(sapply(allAge,length)==0 & sapply(allYear,length)>0)

for(t in toRep){
  allAge[[t]] <- convertAD2BP(allYear[[t]])
}

minAge <- sapply(allAge,min,na.rm=TRUE)
maxAge <- sapply(allAge,max,na.rm=TRUE)

fsTS <- pushTsVariable(fsTS,"minYear",minAge)
fsTS <- pushTsVariable(fsTS,"maxYear",maxAge)

#vectors to create
toPull <- names(qcs)
#omit any?
#toPull <- toPull[!grepl("dataPub",toPull)]# & !grepl("Has paleo depth data",toPull) & !grepl("Has chronology data",toPull)]

allNames <- unique(unlist(sapply(fsTS,names)))

out <- as.data.frame(matrix(NA,nrow = outRows,ncol = length(toPull)))
names(out) <- toPull
out[1,] <- qcs[1,]
for(i in 1:length(toPull)){
  #see if the spreadsheet is empty (WHY DID I DO THIS?)
  #if(all(is.na(qcs[[toPull[i]]][-1]))){#then it's probably all empty
  #  print(str_c("putting an empty column for ",toPull[i]))
  if(FALSE){
  }else{
  n2p <- convo$tsName[toPull[i]==convo$qcSheetName]
  if(any(n2p==allNames)){
    vec <- pullTsVariable(fsTS,n2p)
  }else{
    print(str_c("Does not exist in TS. Putting an empty column for ",toPull[i]))
    vec <- rep(NA,outRows-1)
  }
  out[2:outRows,i] <- vec
  }  
}


out[is.na(out)] <- ""
out[out=="NA"] <- ""

write_csv(out,path = "qc.csv")














# 
# 
# 
# #convert names
# qcNames <- names(qcs)
# tsNames <- c()
# for(i in 1:length(qcNames)){
#   ind <- which(convo$qcSheetName %in% qcNames[i]) 
#   if(length(ind)==0){
#     print(qcNames[i])
#   }else if(length(ind)==1){
#     tsNames[i] <- convo$tsName[ind]
#   }else{
#     stop("multiple convo matches")
#   }
# }
# 
# #loop through TS and test
# newTS <- sTS
# 
# allNames <- unique(unlist(sapply(sTS,names)))
# missingNames <- allNames[!allNames %in% convo$tsName]
# missingNamesTS <- missingNames[order(missingNames)]
# 
# 
# missingNamesConvo <- convo$tsName[!convo$tsName %in% allNames]
# 
# bnames <- c("geo","pub","funding","dataPub")
# dsn <- sapply(sTS,"[[","dataSetName")
# for(i in 1:length(sTS)){
#   thisTSid <- sTS[[i]]$paleoData_TSid
#   #find which QC row
#   qci <- which(qcs$TSid %in% thisTSid)
#   #if doesn't exist, report out:
#   if(length(qci)==0){
#     report = rbind(report,str_c("TSid: ",thisTSid ," for variableName: ",sTS[[i]]$paleoData_variableName ," in dataset:",sTS[[i]]$dataSetName, " doesn't exist in QC sheet") )
#   }else if(length(qci)>1){#then too many matches
#     report = rbind(report,str_c("Too many matches for TSid: ",thisTSid ," for variableName: ",sTS[[i]]$paleoData_variableName ," in dataset:",sTS[[i]]$dataSetName) )
#   }else{#loop through variables and force an update
#     thisTSnames <- unique(c(names(sTS[[i]]),missingNamesConvo)) #find names for this ts, combine with convo names for updates
#     for(j in 1:length(thisTSnames)){
#       rn <- which(tsNames %in% thisTSnames[j])
#       # #ignore TS names without matches
#       
#       #find matches
#       if(length(rn) == 1){
#         #check type
#         varType <- convo$type[which(convo$tsName %in% thisTSnames[j])]
#         if(varType == "character"){
#           varFun <- as.character
#         }else if(varType == "numeric"){
#           varFun <- as.numeric
#         }else if(varType == "boolean"){
#           varFun <- as.logical
#         }else{
#           stop("variable type not recognized")
#         }
#         #apply to all timeseries from this dataset?
#         sname <- str_split(thisTSnames[j],"_")
#         #start with false
#         apply2all <- FALSE
#         if(length(sname[[1]])==1){#then its base
#           apply2all <- TRUE
#         }else{
#           base <- str_remove_all(sname[[1]][1],"[0-9]")
#           apply2all <- any(grepl(base,bnames))
#         }
#         
#         #fill it in.
#         if(apply2all){#then fill it in for all in dataset
#           #print(sname)
#           dsni <- which(grepl(sTS[[i]]$dataSetName,dsn))
#           for(k in 1:length(dsni)){
#             newTS[[dsni[k]]][thisTSnames[j]] <- varFun(qcs[qci,rn])
#           }
#         }else{#then just for this one timeseries
#           newTS[[i]][thisTSnames[j]] <- varFun(qcs[qci,rn])
#         }
#         
#       }
#       if(length(rn) > 1){
#         stop("there shouldn't be multiple matches in ts names")
#       }
#     }
#   }#end loop through variables and force an update
# }
# 
# newCts <- combineInterpretationByScope(newTS)
# Dnew <- collapseTs(newCts)

