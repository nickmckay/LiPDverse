#create all the dashboards
library(flexdashboard)
library(maptools)
library(tidyverse)
library(purrr)
library(leaflet)
library(plotly)
library(lipdR)
library(dygraphs)
library(geoChronR)
library(here)
source(here("functions.R"))

#prepare LiPD data..
D <- readLipd("~/Dropbox/LiPD/PAGES2k/Temp_v2_1_0/")
TS <- extractTs(D)
save(list = c("D","TS"),file = "temp.Rdata")
setwd(here())
#remove columns we don't want to plot
varNames <- sapply(TS, "[[","paleoData_variableName")

good <- which(!(varNames %in% c("year","depth","age")))
TS <- TS[good]


#All datasets
dsn <- sapply(TS,"[[","dataSetName")
ui <- which(!duplicated(dsn))
udsn <- dsn[ui]
lat <- sapply(TS,"[[","geo_latitude")[ui]
lon <- sapply(TS,"[[","geo_longitude")[ui]
elev <- sapply(TS,"[[","geo_elevation")[ui]
archiveType <- sapply(TS,"[[","archiveType")[ui]
link <- paste0(udsn,".html") %>% 
  str_replace_all("'","_")


#Organize metadata for map
map.meta <- data.frame(dataSetName = udsn, #datasetname
                       lat = lat,#lat
                       lon = lon,#lon
                       elev = elev,#elevation
                       archiveType = factor(archiveType),#archiveType
                       link = link)#Link
failed = c()
for(i in 1:nrow(map.meta)){
  print(i)
  fname <- str_replace_all(udsn[i],"'","_")
  if(!file.exists(here("html",str_c(fname,".html")))){
    
    #thisTS <- filterTs(TS, str_c("dataSetName == ",udsn[i]))
    thisTS <- TS[which(udsn[i] == dsn)]
    test = try(createDashboardRmd(thisTS = thisTS,i = i))
    Sys.sleep(1)
    test = try(rmarkdown::render(here("html",str_c(fname,".Rmd"))))
    if(grepl(class(test),"try-error")){
      failed = c(failed, udsn[i])
    }
  }
}