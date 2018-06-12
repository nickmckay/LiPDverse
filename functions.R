#Dashboard builder functions...

plotCol <- function(thisTS,ind){
  
  #bin into annual chunks
  bo <- bin(time = thisTS[[ind]]$year, values = thisTS[[ind]]$paleoData_values,binvec = seq(floor(min(thisTS[[ind]]$year)),ceiling(max(thisTS[[ind]]$year))))
  
  #interpolate NAs
  if(any(is.nan(bo$y))){
    bo$y[which(is.nan(bo$y))] <- approx(x = bo$x,y = bo$y,xout = bo$x[which(is.nan(bo$y))])$y
  }
  
  cts <- ts(data = bo$y,start = floor(min(bo$x)),end = ceiling(max(bo$x)))
  
  dy.plot <- dygraph(cts, main = thisTS[[ind]]$paleoData_variableName) %>% 
    dyAxis("x", drawGrid = FALSE, label = "Year AD") %>%
    dyAxis("y", label = str_c(thisTS[[ind]]$paleoData_variableName," (",thisTS[[ind]]$paleoData_units,")")) %>%
    dyOptions(includeZero = TRUE, 
              axisLineColor = "navy", 
              gridLineColor = "lightblue") %>% 
    dyRangeSelector()
  
  return(dy.plot)
}


i <- 2

#load in the starter text
thisRmd <- read_file("start.Rmd")

#replace the title
thisRmd <- str_replace(thisRmd,pattern = "LiPD-Dashboards",replacement = as.character(map.meta$dataSetName[i]))

#set index number and close the first code chunk
thisRmd <- str_c(thisRmd,str_c("i = ",as.character(i)),sep = "\n") %>% 
  str_c("```",sep = "\n") 


#write title. 
thisRmd <- str_c(thisRmd,str_c("#",as.character(map.meta$dataSetName[i])),sep = "\n")  %>% 
  str_c("\n")

#write metadata sidebar
thisRmd <- str_c(thisRmd,"Metadata {.sidebar}",sep = "\n") %>% 
  str_c("-------------------------------------",sep = "\n") %>% 
  str_c("\n")


writeCollapsibleChunks <- function(thisRmd,name = "pub",vars = c("author","citeKey","journal","volume","pages","pubYear","title","DOI"),tsi =1){
  
  thisRmd <- str_c(thisRmd,"<details>",sep = "\n") %>% 
    str_c(str_c("<summary>",name,"</summary>"),sep = "\n")
  
  if(tolower(name) == "root"){
    these.vars = vars
  }else{
    these.vars = paste0(str_c(name,"_"),vars)
  }
  
  
  for(j in 1:length(these.vars)){
    if(!is.null(thisTS[[tsi]][[these.vars[j]]])){#skip it if it's not there
    #add in the metadata
    thisRmd <- str_c(thisRmd,str_c("<p><strong>",vars[j],": </strong>",thisTS[[tsi]][these.vars[j]]),sep = "\n")
    }
  }
  
  #close out the details sections.
  thisRmd <- str_c(thisRmd,"</details>",sep = "\n") 
  return(thisRmd)
}

#Write root metadata
thisRmd <- writeCollapsibleChunks(thisRmd,name = "root",vars = c("archiveType", "originalDataURL","lipdVersion","dataContributor"))

#Write Pub metadata
#large pub section
bigName <-  "pub"


thisRmd <- str_c(thisRmd,"<details>",sep = "\n") %>% 
  str_c(str_c("<summary>",bigName,"</summary>"),sep = "\n")
allNames <- names(thisTS[[1]])
pubNames <- allNames[grep("pub*", allNames)]
if(length(pubNames)>1){
  npub <- max(as.numeric(str_extract_all(pubNames,pattern = "[0-9]")))
}else{
  npub <- 0
}

#write each publication.
for(p in 1:npub){
thisRmd <- writeCollapsibleChunks(thisRmd,name = str_c("pub",as.character(p)),vars = c("author","citeKey","journal","volume","pages","pubYear","title","DOI"))
}

#close pub setion
thisRmd <- str_c(thisRmd,"</details>",sep = "\n") 


#write out the Rmd
write_file(thisRmd,path = str_c(as.character(map.meta$dataSetName[i]),".Rmd") )
