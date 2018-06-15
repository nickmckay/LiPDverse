#Dashboard builder functions...


plotCol <- function(thisTS,ind,timeCol = NA){
  
  hasYear <- FALSE
  hasAge <- FALSE
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
  
  
  if(is.na(timeCol)){#then decide which to use heuristically
    
    if(min(year,na.rm = T)<0){# if the series starts before 1AD, plot as BP
      timeCol <- "age"
    }else{
      timeCol <- "year"
    }
  }
    #specify units
  if(timeCol == "age"){
    timeUnits <- "Year BP (1950)"
  }else if(timeCol == "year") {
    timeUnits <- "Year AD"
  }
  
  #create year/values dataframe
  if(timeCol == "age"){
    df <- data.frame(time = -age, values = thisTS[[ind]]$paleoData_values) %>% 
      arrange(desc(-time))
    
  }else if(timeCol == "year") {
    df <- data.frame(time = year, values = thisTS[[ind]]$paleoData_values)%>% 
      arrange(desc(-time))
  }
  
  names(df)[2] <-str_c(thisTS[[ind]]$paleoData_variableName," (",thisTS[[ind]]$paleoData_units,")")
  
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
  dy.plot <- dygraph(df, main = thisTS[[ind]]$paleoData_variableName) %>% 
    dyAxis("x", drawGrid = FALSE, label = timeUnits) %>%
    dyAxis("y", label = str_c(thisTS[[ind]]$paleoData_variableName," (",thisTS[[ind]]$paleoData_units,")")) %>%
    dyOptions(includeZero = FALSE, 
              axisLineColor = "navy", 
              gridLineColor = "lightblue",
              connectSeparatedPoints = TRUE,
              ) %>% 
    dyRangeSelector(height = 50)
  
  return(dy.plot)
}

writeCollapsibleChunks <- function(thisRmd,name = "pub",vars = c("author","citeKey","journal","volume","pages","pubYear","title","DOI"),tsi =1,indent = 0,forceName = NULL,dontClose = FALSE,open = FALSE){
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

createDashboardRmd <- function(thisTS,i){


#load in the starter text
thisRmd <- read_file("start.Rmd")

#replace the title
thisRmd <- str_replace(thisRmd,pattern = "LiPD-Dashboards",replacement = as.character(map.meta$dataSetName[i]))

#set index number and close the first code chunk
thisRmd <- str_c(thisRmd,str_c("i = ",as.character(i)),sep = "\n") %>% 
  #str_c('thisTS <- filterTs(TS, str_c("dataSetName == ",udsn[i]))',sep = "\n") %>% 
  str_c('thisTS <- TS[which(udsn[i] == dsn)]',sep = "\n") %>% 
  str_c("```",sep = "\n") 


#write title. 
thisRmd <- str_c(thisRmd,str_c("#",as.character(map.meta$dataSetName[i])),sep = "\n")  %>% 
  str_c("\n")
  
  #write metadata sidebar
  thisRmd <- str_c(thisRmd,"Metadata {.sidebar}",sep = "\n") %>% 
    str_c("-------------------------------------",sep = "\n") %>% 
    str_c("\n")
  
  
  
  
  #get all the variable names
  allNames <- names(thisTS[[1]])
  
  
  ###########Write root metadata
  thisRmd <- writeCollapsibleChunks(thisRmd,name = "root",vars = c("archiveType", "originalDataURL","lipdVersion","dataContributor"),open = TRUE)
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
      thisRmd <- writeCollapsibleChunks(thisRmd,name = str_c(bigName,as.character(p)),vars = vars,indent = 10)
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
      thisRmd <- writeCollapsibleChunks(thisRmd,name = str_c(bigName,as.character(p)),vars = vars,indent = 10)
    }
    
    #close pub setion
    thisRmd <- str_c(thisRmd,"</details>",sep = "\n") 
  }
  ############END FUNDING###############
  
  ###########Write geo metadata
  thisRmd <- writeCollapsibleChunks(thisRmd,name = "geo",vars = c("latitude", "longitude","elevation","region","siteName","pages2kRegion","country","state"),open = TRUE)
  ###########End root metadata
  
  #PaleoData preamble
  thisRmd <- str_c(thisRmd,"<details open>",sep = "\n") %>% 
    str_c(str_c("<summary>PaleoData columns</summary>"),sep = "\n")
  
  ##################################Write PaleoData Column metadata.................
  ##loop through columns and plot them.
  hasInterpretation <- sapply(thisTS,function(x){!is.null(x$interpretation1_variable)})
  
  
  plotOrder <-  seq(1, length(thisTS))
  plotOrder <- plotOrder[order(-hasInterpretation)]
  
  for(cc in plotOrder){#for each column..
    #regular metadata
    thisRmd <- writeCollapsibleChunks(thisRmd,name = "paleoData",forceName = thisTS[[cc]]$paleoData_variableName,vars = c("TSid", "variableName","units","description","useInGlobalTemperatureAnalysis"),tsi = cc,dontClose = TRUE,indent = 10)
    
    
    
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
      
      
      #write each publication.
      for(p in 1:npub){
        vars <- str_extract(pubNames,str_c("(?<=interpretation",as.numeric(p),"_).*$")) #get all.
        thisRmd <- writeCollapsibleChunks(thisRmd,name = str_c(bigName,as.character(p)),vars = vars,tsi = cc,forceName = as.character(p),indent = 20)
      }
      
      #close pub setion
      thisRmd <- str_c(thisRmd,"</details>",sep = "\n") 
    }
    thisRmd <- str_c(thisRmd,"</details>",sep = "\n") 
    ##END INTERPRETATIONS
  }
  
  ############END PaleoData Column###############
  #Add chronData in the future?
  
  ######END METADATA!!!!############################
  
  
  #load in mapchunk
  mapChunk <- read_file("mapChunk.Rmd")
  
  thisRmd <- str_c(thisRmd,mapChunk,sep = "\n") %>% 
    str_c("\n")
  
  
  
  ##Add in timeseries plots
  thisRmd <- str_c(thisRmd,"Row {.tabset .tabset-fade}",sep = "\n") %>% 
    str_c("-----------------------------------------------------------------------",sep = "\n") %>% 
    str_c("\n")
  
  
  
  for(cc in plotOrder){#for each column..
    #setup a new column in the markdown
    thisRmd <- str_c(thisRmd,str_c("### ",thisTS[[cc]]$paleoData_variableName),sep = "\n") %>% 
      str_c("```{r}",sep = "\n") %>% 
      str_c(str_c("plotCol(thisTS,ind = ",as.character(cc),")"),sep = "\n") %>% 
      str_c("```",sep = "\n")  %>% 
      str_c("\n")
  }
  
  
  #write out the Rmd
  write_file(thisRmd,path = here("html",str_replace_all(str_c(as.character(map.meta$dataSetName[i]),".Rmd"),"'","_")))
  
    }


