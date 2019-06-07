#Dashboard builder functions...

createTSid <- function(){
  return(paste(c("R",sample(c(letters,LETTERS,seq(0,9)),size = 10,replace=TRUE)),collapse = ""))
}

addTSidToLipd <- function(L){
  mts <- lipdR::extractTs(L)
  for(i in 1:length(mts)){
    if(length(mts[[i]]$paleoData_TSid)==0){
      mts[[i]]$paleoData_TSid <- createTSid()
    }
  }
  return(collapseTs(mts))
}


plotCol <- function(thisTS,ind,timeCol = NA){
  if(thisTS[[1]]$mode == "chron"){
    mode <- "chron"
  }else{
    mode <- "paleo"
  }
  
  
  hasYear <- FALSE
  hasAge <- FALSE
  hasSeq <- FALSE
  if("year" %in% names(thisTS[[ind]])){
    hasYear <- TRUE
    year <- thisTS[[ind]]$year
  }
  if("age" %in% names(thisTS[[ind]])){
    hasAge <- TRUE
    age <- thisTS[[ind]]$age
  }
  if(hasYear & !hasAge){#create an ageColumn
    age <- geoChronR::convertAD2BP(year)
    hasAge <- TRUE
  }
  if(!hasYear & hasAge){#create an ageColumn
    year <- geoChronR::convertBP2AD(age)
    hasYear <- TRUE
  }
  if((!hasYear & !hasAge) | all(is.na(year))){#has no years or ages
    year <- seq_along(thisTS[[ind]][[stringr::str_c(mode,"Data_values")]])
    hasSeq <- TRUE
  }
  
  if(is.na(timeCol)){#then decide which to use heuristically
    
    if(hasSeq){#then no ages, just a sequence
      timeCol <- "index"
    }else{
      if(min(year,na.rm = T)<0){# if the series starts before 1AD, plot as BP
        timeCol <- "age"
        if(all(is.na(age))){#if all NAs, use a sequence
          year <- seq_along(thisTS[[ind]][[stringr::str_c(mode,"Data_values")]])
          timeCol <- "index"
        }
      }else{
        timeCol <- "year"
        if(all(is.na(year))){#if all NAs, use a sequence
          year <- seq_along(thisTS[[ind]][[stringr::str_c(mode,"Data_values")]])
          timeCol <- "index"
        }
      }
    }
  }
  
  
  #specify units
  if(timeCol == "age"){
    timeUnits <- "Year BP (1950)"
  }else if(timeCol == "year") {
    timeUnits <- "Year AD"
  }else if(timeCol == "index") {
    timeUnits <- "(no age or year column, or theyre all NA!)"
  }
  
  #create year/values dataframe
  if(timeCol == "age"){
    df <- data.frame(time = -age, values = thisTS[[ind]][[stringr::str_c(mode,"Data_values")]]) %>% 
      arrange(desc(-time))
    
  }else if(timeCol == "year") {
    df <- data.frame(time = year, values = thisTS[[ind]][[stringr::str_c(mode,"Data_values")]])%>% 
      arrange(desc(-time))
  }else if(timeCol == "index") {
    df <- data.frame(time = year, values = thisTS[[ind]][[stringr::str_c(mode,"Data_values")]])%>% 
      arrange(desc(-time))
  }
  
  
  if(thisTS[[ind]][[stringr::str_c(mode,"Number")]] == 1  & thisTS[[ind]]$tableNumber == 1){
    names(df)[2] <-str_c(thisTS[[ind]][[stringr::str_c(mode,"Data_variableName")]]," (",thisTS[[ind]][[stringr::str_c(mode,"Data_units")]],")")
  }else{
    names(df)[2] <-str_c(thisTS[[ind]][[stringr::str_c(mode,"Data_variableName")]]," (",thisTS[[ind]][[stringr::str_c(mode,"Data_units")]],") [",as.character(thisTS[[ind]][[stringr::str_c(mode,"Number")]]),"-",as.character(thisTS[[ind]]$tableNumber),"]")
  }
  
  # #bin into annual chunks
  # bo <- geoChronR::bin(time = thisTS[[ind]]$year, values = thisTS[[ind]]$paleoData_values,binvec = seq(floor(min(thisTS[[ind]]$year)),ceiling(max(thisTS[[ind]]$year))))
  # if(all(is.na(bo$y))){
  #   return("Non-numeric column")
  # }
  # #interpolate NAs
  # if(any(is.nan(bo$y))){
  #   bo$y[which(is.nan(bo$y))] <- approx(x = bo$x,y = bo$y,xout = bo$x[which(is.nan(bo$y))])$y
  # }
  # 
  # #bo <- dplyr::filter(bo,x > 0)
  # #cts <- ts(data = bo$y,start = as.Date(getDate(min(bo$x))),deltat = 365.24)
  # cts <- ts(data = bo$y,start = as.Date(getDate(min(bo$x))),deltat = 365.24)
  # #cts <- xts(data = bo$y,order.by = ym,frequency = 1,)
  # 
  dy.plot <- dygraph(df, main = thisTS[[ind]][[stringr::str_c(mode,"Data_variableName")]]) %>% 
    dyAxis("x", drawGrid = FALSE, label = timeUnits) %>%
    dyAxis("y", label = names(df)[2]) %>%
    dyOptions(includeZero = FALSE, 
              axisLineColor = "navy", 
              gridLineColor = "lightblue",
              connectSeparatedPoints = TRUE,
    ) %>% 
    dyRangeSelector(height = 50)
  
  return(dy.plot)
}

writeCollapsibleChunks <- function(thisRmd,thisTS = thisTS,name = "pub",vars = c("author","citeKey","journal","volume","pages","pubYear","title","DOI"),tsi =1,indent = 0,forceName = NULL,dontClose = FALSE,open = FALSE){
  
  if(is.null(forceName)){
    forceName = name
  }
  
  if(open){
    thisRmd <- str_c(thisRmd,str_c('<details open style="margin-left: ',as.character(indent),'px">') ,sep = "\n") %>% 
      str_c(str_c("<summary>",forceName,"</summary>"),sep = "\n")
  }else{
    thisRmd <- str_c(thisRmd,str_c('<details style="margin-left: ',as.character(indent),'px">') ,sep = "\n") %>% 
      str_c(str_c("<summary>",forceName,"</summary>"),sep = "\n")
  }
  
  if(tolower(name) == "root"){
    these.vars = vars
  }else{
    these.vars = paste0(str_c(name,"_"),vars)
  }
  
  
  for(j in 1:length(these.vars)){
    if(!is.null(thisTS[[tsi]][[these.vars[j]]])){#skip it if it's not there
      #add in the metadata
      thisRmd <- str_c(thisRmd,str_c('<p style="margin-left: ',as.character(indent),'px"><strong>',vars[j],": </strong>",thisTS[[tsi]][these.vars[j]]),sep = "\n")
    }
  }
  
  #close out the details sections.
  if(!dontClose){
    thisRmd <- str_c(thisRmd,"</details>",sep = "\n") 
  }
  return(thisRmd)
}

createDashboardRmd <- function(thisTS,i,project,chronTS = NA){
  
  
  #load in the starter text
  thisRmd <- read_file("start.Rmd")
  
  #replace the title
  thisRmd <- str_replace(thisRmd,pattern = "LiPD-Dashboards",replacement = as.character(map.meta$dataSetName[i]))
  
  #set index number and close the first code chunk
  thisRmd <- str_c(thisRmd,str_c("i = ",as.character(i)),sep = "\n") %>% 
    #str_c('thisTS <- filterTs(TS, str_c("dataSetName == ",udsn[i]))',sep = "\n") %>% 
    str_c('thisTS <- TS[which(udsn[i] == dsn)]',sep = "\n") %>% 
    #str_c('thisTS <- thisTS[which(!sapply(thisTS,function(x){all(is.na(x$paleoData_values))}))]',sep = "\n") %>% 
    str_c("```",sep = "\n") 
  
  
  #write title. 
  thisRmd <- str_c(thisRmd,str_c("#",as.character(map.meta$dataSetName[i])),sep = "\n")  %>% 
    str_c("\n")
  
  
  csvName <- str_replace_all(str_c(as.character(map.meta$dataSetName[i]),".csv"),"'","_")
  csvNameChron <- str_replace_all(str_c(as.character(map.meta$dataSetName[i]),"-chron.csv"),"'","_")
  
  
  #write metadata sidebar
  thisRmd <- str_c(thisRmd,"Metadata {.sidebar}",sep = "\n") %>% 
    str_c("-------------------------------------",sep = "\n") %>% 
    str_c(str_c("[Download LiPD file](",as.character(map.meta$dataSetName[i]),".lpd)"),sep = "\n") %>% 
    str_c("\n") %>% 
    str_c("            \n") %>%
    str_c(str_c("[Edit LiPD file](http://lipd.net/playground?source=http://lipdverse.org/",project,"/",as.character(map.meta$dataSetName[i]),".lpd)"),sep = "\n") %>% 
    str_c("\n") %>% 
    str_c("            \n") %>%
    str_c(str_c("[Download paleoData only (csv)](",csvName,")"),sep = "\n") %>% 
    str_c("\n") %>% 
    str_c("            \n")
  
  
  if(any(!is.na(chronTS))){
    thisRmd <- thisRmd %>%
      str_c(str_c("[Download chronData only (csv)](",csvNameChron,")"),sep = "\n") %>% 
      str_c("\n") %>% 
      str_c("            \n")
  }
  
  
    thisRmd <- thisRmd %>%
    str_c("[Report an issue (include dataset name)](https://github.com/nickmckay/LiPDverse/issues)",sep = "\n") %>% 
    str_c("\n")
  
  
  
  
  #get all the variable names
  allNames <- names(thisTS[[1]])
  
  
  ###########Write root metadata
  thisRmd <- writeCollapsibleChunks(thisRmd,thisTS,name = "root",vars = c("archiveType", "originalDataURL","lipdVersion","dataContributor"),open = TRUE)
  ###########End root metadata
  
  ############WRITE PUB###############
  #large pub section
  pubNames <- allNames[grep("pub*", allNames)]
  if(length(pubNames)>1){
    npub <- max(as.numeric(str_extract_all(pubNames,pattern = "[0-9]")))
  }else{
    npub <- 0
  }
  
  if(npub>0){
    bigName <-  "pub"
    vars <-  c("author","citeKey","journal","volume","pages","pubYear","title","DOI")
    
    thisRmd <- str_c(thisRmd,"<details>",sep = "\n") %>% 
      str_c(str_c("<summary>",bigName,"</summary>"),sep = "\n")
    
    
    #write each publication.
    for(p in 1:npub){
      thisRmd <- writeCollapsibleChunks(thisRmd,thisTS,name = str_c(bigName,as.character(p)),vars = vars,indent = 10)
    }
    
    #close pub setion
    thisRmd <- str_c(thisRmd,"</details>",sep = "\n") 
  }
  ############END PUB###############
  
  ##################################Write Funding metadata.................
  pubNames <- allNames[grep("funding*", allNames)]
  if(length(pubNames)>1){
    npub <- max(as.numeric(str_extract_all(pubNames,pattern = "[0-9]")))
  }else{
    npub <- 0
  }
  
  if(npub>0){
    bigName <-  "funding"
    vars <-  c("agency","grant","country")
    
    
    thisRmd <- str_c(thisRmd,"<details>",sep = "\n") %>% 
      str_c(str_c("<summary>",bigName,"</summary>"),sep = "\n")
    
    
    #write each publication.
    for(p in 1:npub){
      thisRmd <- writeCollapsibleChunks(thisRmd,thisTS,name = str_c(bigName,as.character(p)),vars = vars,indent = 10)
    }
    
    #close pub setion
    thisRmd <- str_c(thisRmd,"</details>",sep = "\n") 
  }
  ############END FUNDING###############
  
  ###########Write geo metadata
  thisRmd <- writeCollapsibleChunks(thisRmd,thisTS,name = "geo",vars = c("latitude", "longitude","elevation","region","siteName","pages2kRegion","country","state"),open = TRUE)
  ###########End root metadata
  
  #PaleoData preamble
  thisRmd <- str_c(thisRmd,"<details open>",sep = "\n") %>% 
    str_c(str_c("<summary>PaleoData columns</summary>"),sep = "\n")
  
  ##################################Write PaleoData Column metadata.................
  ##loop through columns and plot them.
  hasInterpretation <- sapply(thisTS,function(x){!is.null(x$interpretation1_variable)})
  paleoNum <-  pullTsVariable(thisTS,"paleoNumber")
  tableNum <-  pullTsVariable(thisTS,"tableNumber")
  
  
  
  thisVarNames <- sapply(thisTS,"[[","paleoData_variableName")
  xcol <- which(thisVarNames %in% c("year","depth","age"))
  
  
  plotOrder <-  seq(1, length(thisTS)) #generate a sequence to start with
  isxcol <- plotOrder %in% xcol
  plotOrder <- plotOrder[order(paleoNum,tableNum,-isxcol,-hasInterpretation)]#sort by table Number
  
  for(cc in plotOrder){#for each column..
    #regular metadata
    
    if(max(paleoNum) == 1  & max(tableNum) == 1){
      tvname <- str_c(thisTS[[cc]]$paleoData_variableName," (",thisTS[[cc]]$paleoData_units,")")
    }else{
      tvname <- str_c(thisTS[[cc]]$paleoData_variableName," (",thisTS[[cc]]$paleoData_units,") [",as.character(thisTS[[cc]]$paleoNumber),"-",as.character(thisTS[[cc]]$tableNumber),"]")
    }
    
    thisRmd <- writeCollapsibleChunks(thisRmd,thisTS,name = "paleoData",forceName = tvname,vars = c("TSid", "variableName","units","description","useInGlobalTemperatureAnalysis"),tsi = cc,dontClose = TRUE,indent = 10)
    
    
    
    ###INTERPRETATIONS
    allNames=names(thisTS[[cc]])
    pubNames <- allNames[grep("interpretation*", allNames)]
    
    if(length(pubNames)>1){
      npub <- max(as.numeric(str_extract_all(pubNames,pattern = "[0-9]")))
    }else{
      npub <- 0
    }
    
    if(npub>0){
      bigName <-  "interpretation"
      
      thisRmd <- str_c(thisRmd,"<details>",sep = "\n") %>% 
        str_c(str_c("<summary>",bigName,"</summary>"),sep = "\n")
      
      
      #write each interpretation
      for(p in 1:npub){
        vars <- str_extract(pubNames,str_c("(?<=interpretation",as.numeric(p),"_).*$")) #get all.
        thisRmd <- writeCollapsibleChunks(thisRmd,thisTS,name = str_c(bigName,as.character(p)),vars = vars,tsi = cc,forceName = as.character(p),indent = 20)
      }
      
      #close paleo setion
      thisRmd <- str_c(thisRmd,"</details>",sep = "\n") 
    }
    thisRmd <- str_c(thisRmd,"</details>",sep = "\n") 
    ##END INTERPRETATIONS
  }
  
  
  
  
  ############END PaleoData Column###############
  
  
  if(!any(is.na(chronTS))){
    
    #ChronData preamble
    thisRmd <- str_c(thisRmd,"<details>",sep = "\n") %>% 
      str_c(str_c("<summary>ChronData columns</summary>"),sep = "\n")
##################################Write ChronData Column metadata.................
    ##loop through columns and plot them.
    chronNum <-  pullTsVariable(chronTS,"chronNumber")
    tableNum <-  pullTsVariable(chronTS,"tableNumber")
    
    
    
    thisVarNames <- sapply(chronTS,"[[","chronData_variableName")
    xcol <- which(thisVarNames %in% c("year","depth","age"))
    
    
    plotOrderChron <-  seq(1, length(chronTS)) #generate a sequence to start with
    isxcol <- plotOrderChron %in% xcol
    plotOrderChron <- plotOrderChron[order(chronNum,tableNum,-isxcol)]#sort by table Number
    
    for(cc in plotOrderChron){#for each column..
      #regular metadata
      
      if(max(chronNum) == 1  & max(tableNum) == 1){
        tvname <- str_c(chronTS[[cc]]$chronData_variableName," (",chronTS[[cc]]$chronData_units,")")
      }else{
        tvname <- str_c(chronTS[[cc]]$chronData_variableName," (",chronTS[[cc]]$chronData_units,") [",as.character(chronTS[[cc]]$chronNumber),"-",as.character(chronTS[[cc]]$tableNumber),"]")
      }
      
      thisRmd <- writeCollapsibleChunks(thisRmd,thisTS = chronTS,name = "chronData",forceName = tvname,vars = c("TSid", "variableName","units","description"),tsi = cc,dontClose = TRUE,indent = 10)
      #close each column setion
      thisRmd <- str_c(thisRmd,"</details>",sep = "\n") 
    }
    
    #######END CHRONDATA!!!!######################
  }
  
  ######END METADATA!!!!############################
  
  
  #load in mapchunk
  mapChunk <- read_file("mapChunk.Rmd")
  
  thisRmd <- str_c(thisRmd,mapChunk,sep = "\n") %>% 
    str_c("\n")
  
  ##create csv output for download
  lengths <- sapply(thisTS,function(x){length(x$paleoData_values)})
  outdf <- data.frame(matrix(NA, nrow = max(lengths), ncol = length(lengths)))
  
  for(cc in 1:length(plotOrder)){#for each column..
    outdf[1:lengths[plotOrder[cc]],cc] <- thisTS[[plotOrder[cc]]]$paleoData_values
    if(max(paleoNum) == 1  & max(tableNum) == 1){
      names(outdf)[cc] <-str_c(thisTS[[plotOrder[cc]]]$paleoData_variableName," (",thisTS[[plotOrder[cc]]]$paleoData_units,")")
    }else{
      names(outdf)[cc] <-str_c(thisTS[[plotOrder[cc]]]$paleoData_variableName," (",thisTS[[plotOrder[cc]]]$paleoData_units,") [",as.character(thisTS[[plotOrder[cc]]]$paleoNumber),"-",as.character(thisTS[[plotOrder[cc]]]$tableNumber),"]")
    }
    print(names(outdf)[cc])
  }
  readr::write_csv(outdf,here("html",project,csvName))
  
  
  
  
  if(!any(is.na(chronTS))){
    ##create chron csv output for download
    lengths <- sapply(chronTS,function(x){length(x$chronData_values)})
    outdfChron <- data.frame(matrix(NA, nrow = max(lengths), ncol = length(lengths)))
    
    for(cc in 1:length(plotOrderChron)){#for each column..
      outdfChron[1:lengths[plotOrderChron[cc]],cc] <- chronTS[[plotOrderChron[cc]]]$chronData_values
      if(max(chronNum) == 1  & max(tableNum) == 1){
        names(outdfChron)[cc] <-str_c(chronTS[[plotOrderChron[cc]]]$chronData_variableName," (",chronTS[[plotOrderChron[cc]]]$chronData_units,")")
      }else{
        names(outdfChron)[cc] <-str_c(chronTS[[plotOrderChron[cc]]]$chronData_variableName," (",chronTS[[plotOrderChron[cc]]]$chronData_units,") [",as.character(chronTS[[plotOrderChron[cc]]]$chronNumber),"-",as.character(chronTS[[plotOrderChron[cc]]]$tableNumber),"]")
      }
      print(names(outdfChron)[cc])
    }
    readr::write_csv(outdfChron,here("html",project,csvNameChron))
    
  }
  
  
  ##Add in timeseries plots
  thisRmd <- str_c(thisRmd,"Row {.tabset .tabset-fade}",sep = "\n") %>% 
    str_c("-----------------------------------------------------------------------",sep = "\n") %>% 
    str_c("\n")
  
  thisVarNames <- sapply(thisTS,"[[","paleoData_variableName")
  
  #graph order
  xcol <- which(thisVarNames %in% c("year","depth","age"))
  
  graphOrder = plotOrder[-which(plotOrder %in% xcol)]
  graphNames = names(outdf)[-which(plotOrder %in% xcol)]
  for(cc in 1:length(graphOrder)){#for each column..
    #setup a new column in the markdown
    thisRmd <- str_c(thisRmd,str_c("### ",graphNames[cc]),sep = "\n") %>% 
      str_c("```{r}",sep = "\n") %>% 
      str_c(str_c("plotCol(thisTS,ind = ",as.character(graphOrder[cc]),")"),sep = "\n") %>% 
      str_c("```",sep = "\n")  %>% 
      str_c("\n")
  }
  
  
  if(!any(is.na(chronTS))){
    ##Add in chronData plots
    thisRmd <- str_c(thisRmd,"Row {.tabset .tabset-fade}",sep = "\n") %>% 
      str_c("-----------------------------------------------------------------------",sep = "\n") %>% 
      str_c("\n")
    
    #graph order
    thisVarNames <- sapply(chronTS,"[[","chronData_variableName")
    
    graphOrder = plotOrderChron
    graphNames = names(outdfChron)
    for(cc in 1:length(graphOrder)){#for each column..
      #setup a new column in the markdown
      thisRmd <- str_c(thisRmd,str_c("### ",graphNames[cc]),sep = "\n") %>% 
        str_c("```{r}",sep = "\n") %>% 
        str_c(str_c("plotCol(chronTS,ind = ",as.character(graphOrder[cc]),")"),sep = "\n") %>% 
        str_c("```",sep = "\n")  %>% 
        str_c("\n")
    }
  }
  
  
  #write out the Rmd
  write_file(thisRmd,path = here("html",project,str_replace_all(str_c(as.character(map.meta$dataSetName[i]),".Rmd"),"'","_")))
  
}

createProjectRmd <- function(project){
  
  
  #load in the starter text
  thisRmd <- read_file(here("start.Rmd"))
  
  #replace the title
  thisRmd <- str_replace(thisRmd,pattern = "LiPD-Dashboards",replacement = project)
  
  #set index number and close the first code chunk
  thisRmd <- str_c(thisRmd,str_c("i = ",as.character(1)),sep = "\n") %>% 
    str_c("```",sep = "\n") 
  
  
  #write title. 
  thisRmd <- str_c(thisRmd,str_c("#",project),sep = "\n")  %>% 
    str_c("\n")
  
  #write metadata sidebar
  thisRmd <- str_c(thisRmd,"Metadata {.sidebar}",sep = "\n") %>% 
    str_c("-------------------------------------",sep = "\n") %>% 
    str_c(str_c("[Download all LiPD files for ", project,"](",project,".zip)"),sep = "\n") %>% 
    str_c("\n") %>% 
    str_c("            \n") %>%
    str_c(str_c("[Download R serialization of all LiPD files for ", project,"](",project,".RData)"),sep = "\n") %>% 
    str_c("\n") %>% 
    str_c("            \n") %>%
    str_c(str_c("[Download matlab serialization of all LiPD files for ", project,"](",project,".mat)"),sep = "\n") %>% 
    str_c("\n") %>% 
    str_c("            \n") %>%
    str_c("[Report an issue (include project name)](https://github.com/nickmckay/LiPDverse/issues)",sep = "\n") %>% 
    str_c("\n")
  
  
  #load in mapchunk
  mapChunk <- read_file("mapChunk.Rmd")
  
  mapChunk <- str_replace(mapChunk,pattern = "buff <- 15",replacement = "buff <- 60")
  
  thisRmd <- str_c(thisRmd,mapChunk,sep = "\n") %>% 
    str_c("\n")
  
  
  #write out the Rmd
  write_file(thisRmd,path = here("html",project,"index.Rmd"))
  
}

