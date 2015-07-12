require('plyr')
require("RCurl")
require("jsonlite")

dir.create("data")

countryCodes<-c("BEL","GBR")

#Split COFOG code in 3 parts since the OECD server cannot deal with all codes at once
COFOG1<-"010+T+GOVEXP+0101+0201+020+0301+030+0401+040+0501+050+0601+060+0701+070+0801+080+0901+090+1001+100+0102+0202+0302+0402"
COFOG2<-"0502+0602+0702+0802+0902+1002+0103+0203+0303+0403+0503+0603+0703+0803+0903+1003+0104+0204+0304+0404+0504+0604+0704+0804+0904+1004+0105"
COFOG3<-"0205+0305+0405+0505+0605+0705+0805+0905+1005+0106+0306+0406+0506+0606+0706+0806+0906+1006+0107+0108+0407+0907+1007+0408+0908+1008+0409+1009"

COFOGs<-c(COFOG1,COFOG2,COFOG3)

getAmountsForKey<-function(keyValues) {
  laply(keyValues[[1]]$observations,function(x) x[[1]][1])*1000
}

createCSV<-function() {

for (countryCode in countryCodes) {
  
  tryCatch({
    message(paste0("Downloading data from ODCE for country code ",countryCode))
    
    govExpenditures<-NULL
    
    for (COFOG in COFOGs) {
      data<-fromJSON(getURL(paste0("http://stats.oecd.org/SDMX-JSON//data/SNA_TABLE11/",countryCode,".TLYCG.",COFOG,".GS13.C/all?startTime=2008&endTime=2014")))
      
      amounts<-data$dataSets[1,2]
      
      codeCOFOG<-data$structure$dimensions$series$values[[3]]
      country<-data$structure$dimensions$series$values[[1]]
      years<-data$structure$dimensions$observation$value[[1]]
      currency<-data$structure$attribute$series$values[[2]]$id
      
      #The dimensions are referenced in the format x:x:x:x:x, the third entries gives the index
      # of the COFOG code. E.g., x:x:3:x:x refers to the COFOG code whose index is 23 in the 
      #codeCOFOG array. The following extracts the COFOG code id from teh x:x:x:x:x key
      idCodeCOFOG<-as.numeric(laply(sapply(colnames(amounts),strsplit,":"),function(x) x[3]))+1
      
      govExpenditure<-NULL
      for (i in 1:length(amounts)) govExpenditure<-rbind(govExpenditure,c(codeCOFOG[idCodeCOFOG[i],],currency,getAmountsForKey(amounts[i])))
      
      colnames(govExpenditure)<-c("cofog-code","name","currency",years$name)
      
      govExpenditures<-rbind(govExpenditures,govExpenditure)
      
    }
    
    govExpenditures<-govExpenditures[order(unlist(govExpenditures[,1])),]
    govExpenditures<-sapply(as.data.frame(govExpenditures),unlist)
    #Remove total
    govExpenditures<-govExpenditures[-which(govExpenditures[,1]=="T"),]
    #Divide COFOG codes into levels 1 and 2
    cofoglevel1 <-as.numeric(sapply(govExpenditures[,1],substr,1,2))
    cofoglevel2 <-as.numeric(sapply(govExpenditures[,1],substring,3))
    
    govExpenditures<-cbind("cofog-level-1"=cofoglevel1,"cofog-level-2"=cofoglevel2,govExpenditures)
    govExpenditures<-govExpenditures[,-3]
    
    write.csv(file=paste0("data/government-expenditure-cofog-",gsub(" ","-",tolower(country$name)),".csv"),govExpenditures,row.names=F,quote=T)
    message(paste0("Succesfully created data/government-expenditure-cofog-",gsub(" ","-",tolower(country$name)),".csv"))
    
  }, warning = function(cond) {
    message("Warning: Data not properly processed")
    message(cond)
  }, error = function(cond) {
    message("Error: Data not properly processed")
    message(cond)
  })
  
}

}


createDP<-function() {
    
    require(dpmr)
    meta<-list(name = "government-expenditure-cofog",
         title = 'Government expenditure, COFOG nomenclature',
         description = 'Government expenditure, COFOG nomenclature.',
         maintainer = list(),
         contributors = list(),
         version = "1",
         last_updated = as.Date(Sys.time()),
         homepage = '',
         keywords = list(),
         publisher = list(),
         url = '',
         base = '',
         image = '',
         licenses = data.frame(id= "odc-pddl",
                              url = 'http://opendatacommons.org/licenses/pddl/',
                              name = "Public Domain Dedication and License"),
         dataDependencies = '',
         sources = data.frame(name = 'OECD',
                              web = 'http://stats.oecd.org'))
    
    datapackage_init_files("data","government-expenditure-cofog",meta=meta,overwrite=T)
    
    
    
}

