ntp_traffic_go<-function(){
    repeat{
        print(paste(format(Sys.time(),"%H:%M"),"start"))
        ntp_traffic_dl()
        print(paste(format(Sys.time(),"%H:%M"),"end"))
        Sys.sleep(290)
    }
}

ntp_traffic_dl <- function () {
    library(bitops)
    library(RCurl)
    library(XML)
    library(httr)
    
    options(digits=22)
    options(stringsAsFactors=FALSE)
    setInternet2(TRUE)
    
    csv.data.url<-getURL("http://data.ntpc.gov.tw/od/data/api/93DC5275-ABE0-4AF0-ABEF-1A7A54876174?$format=csv",encoding = "utf-8")
    csv.meta.url<-getURL("http://data.ntpc.gov.tw/od/data/api/D4ACABED-5960-4A2B-9AF1-C62D7AF37622?$format=csv",encoding = "utf-8")
    
    csv.data <- strsplit(csv.data.url,"\n")
    csv.df <- data.frame(vdid=character(),datacollectime=character(),status=character(),vsrid=character(),
                         vsrdir=character(),speed=character(),laneoccupy=character(),carid=character(),volume=character())
    
    for (i in 1:length(csv.data[[1]])) {
      csv.df <- rbind(csv.df,gsub("\"","",strsplit(csv.data[[1]][i],",")[[1]]))
    }
    colnames(csv.df)<-csv.df[1,]
    csv.df<-csv.df[-1,]
    
    fname <- gsub(" ","_",gsub("/|:","",csv.df$datacollecttime[1]))
    
    write.csv(csv.df,paste0("./ntp_traffic_data/",fname,".csv"),row.names=FALSE)
    
    print(fname)
}