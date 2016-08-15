traffic_data_compile<-function(){
  library(reshape2)
  
  data.root<-"./ntp_traffic_data/"
  output.path<-"./ntp_traffic_output/"
  file.list<-dir(data.root)[grepl("(0|5).csv$",dir(data.root))]
  
  print("loading data...")
    
  pb <- txtProgressBar(max = length(file.list), style = 3)
  
  for(i in 1:length(file.list)){
    data.df<-read.csv(paste0(data.root,file.list[i]),header=TRUE,na.strings="-99")
    #data.df <-data.df[-1,]
    
    #data.df$datacollecttime<-as.Date(data.df$datacollecttime,tz="Asia/Taipei")
    data.df$datacollecttime<-strptime(data.df$datacollecttime,format="%Y/%m/%d %H:%M:%S",tz="Asia/Taipei")
    data.df$time<-format(data.df$datacollecttime,"%H_%M")
    data.df$weekday<-format(data.df$datacollecttime,"%u")
    data.df$holiday<-0;data.df$holiday[data.df$weekday>5]<-1
    #data.df$holiday[data.df$date==2016-05-02]<-1
    data.df$count<-1

    ##if (substr(data.df$time[1],5,5) != 4|9)
    min.num <- as.integer(substr(data.df$time[1],4,5))
    min.check <- (min.num+1) %% 5
    if (min.check != 0) {
      new.min <- as.character(min.num+1)
      new.min <- paste0(rep(0,2-nchar(new.min)),new.min)
      data.df$time <- paste0(substr(data.df$time[1],1,3),new.min)
    }
    
    data.df<-data.df[c("vdid","holiday","time","vsrdir","carid","speed","volume","count")]
    
    if(i==1) merge.df<-data.df else merge.df<-rbind(merge.df,data.df)
    
    setTxtProgressBar(pb, i)
  }

  close(pb)
  
  print("casting...")
  #melt.df<-melt(merge.df,id.vars = c("vdid","holiday","time","vsrdir","carid"),measure.vars = c("speed","volume","count"))
  melt.df<-melt(merge.df,id.vars = c("vdid","holiday","time","carid"),measure.vars = c("speed","volume","count"))
  #cast.df<-dcast(melt.df, vdid+holiday+time+vsrdir+carid ~ variable, mean)
  cast.df<-dcast(melt.df, vdid+holiday+time+carid ~ variable, mean)
  #count.cast<-dcast(melt.df, vdid+holiday+time+vsrdir+carid ~ variable, sum)[c(1:5,8)]
  count.cast<-dcast(melt.df, vdid+holiday+time+carid ~ variable, sum)[c(1:4,7)]
  
  total.cast<-dcast(melt.df, vdid+time+carid~variable,mean)
  total.cast$holiday=3
  total.cast<-data.frame(total.cast[c(1)],total.cast$holiday,total.cast[c(2:6)])
  colnames(total.cast)<-colnames(cast.df)
  total.count<-dcast(melt.df, vdid+time+carid ~ variable, sum)[c(1:3,6)]
  
  #cast.df<-rbind(cast.df,total.cast)
  
  #output.df<-data.frame(cast.df[c(1:7)],count.cast[c(6)])
  #output.df<-data.frame(cast.df[c(1:7)],count.cast[c(5)])
  output.df1<-data.frame(cast.df[c(1:7)],count.cast[c(5)])
  output.df2<-data.frame(total.cast[c(1:7)],total.count[c(4)])
  
  output.df<-rbind(output.df1,output.df2)
  
  print("exporting...")
  output.name<-format(Sys.time(),"%Y%m%d%H%M")
  write.csv(merge.df,paste0(output.path,"data_compile",output.name,".csv"))
  write.csv(output.df,paste0(output.path,"output_",output.name,".csv"))
  
  print("done!")
}

traffic_graph<-function(vdid,carid){
  library(ggplot2)
  options(stringsAsFactors = FALSE)
  options(digits=3)
 
  if(is.null(vdid)) stop("please input vdid")
  
  output.path<-"./ntp_traffic_output/"
  output.name<-max(dir(output.path)[grepl("output",dir(output.path))])
  output.time<-format(Sys.time(),"%Y%m%d%H%M")
  
  output.df<-read.csv(paste0(output.path,output.name))
  parse.df<-output.df[output.df$vdid==vdid,]
  parse.df<-parse.df[parse.df$carid==carid,]
  if(nrow(parse.df)==0) stop("carid not found")
  parse.df<-parse.df[!(parse.df$holiday==3),]
  parse.df<-parse.df[!(substr(parse.df$time,5,5)==3),]
  parse.df<-parse.df[!(substr(parse.df$time,5,5)==8),]
  parse.df$time<-ordered(as.factor(parse.df$time))
  parse.df$time_o<-as.numeric(parse.df$time)
  
  #time.level<-levels(parse.df$time)
  
  #for (i in 1:length(time.level)) {
    #time.df<-parse.df[parse.df$time==time.level[i],]
    #if (!is.null(time.df)) {
      #new.df<-data.frame(99999,
                         #vdid,
                         #3,
                         #time.level[i],
                         #carid,
                         #round(mean(as.numeric(time.df$speed)),digits=3),
                         #round(sum(as.numeric(time.df$volume)),digits=3),
                         #round(sum(as.numeric(time.df$count)),digits=3),
                         #round(sum(as.numeric(time.df$count.1)),digits=3),
                         #time.df$time_o[1])
      #colnames(new.df)<-colnames(parse.df)
      #parse.df<-rbind(parse.df,new.df)
    #}
  #}
  
  parse.df$holiday<-as.factor(parse.df$holiday)
  
  bks<-c(seq(1,288,12),288)
  lbs<-seq(0,24)

  lowcount.workdays<-unique(parse.df$time_o[parse.df$count<0 & parse.df$holiday =="0"])
  lowcount.holidays<-unique(parse.df$time_o[parse.df$count<0 & parse.df$holiday =="1"])
  
  scale.x.setup<-scale_x_continuous(breaks=bks,labels=lbs)
  legend.setup<-theme(legend.position = c(.88, .85),legend.title=element_blank(),legend.background=element_rect(color="#dddddd"))
  
  parameter.df<-data.frame(item=c("speed","volume"),ymin=c(0,0),ymax=c(60,15),stringsAsFactors = FALSE)
  
  for(i in 1:2){
    graph.df<-data.frame(x=parse.df$time_o,y=parse.df[,parameter.df[i,1]],holiday=parse.df$holiday)
  
    lab.setup<-labs(list(x = "Time", y = parameter.df[i,1]))
      
    g<-ggplot(graph.df,aes(x,y,color=factor(holiday,labels=c("Workdays","Holidays"))))#,"Total"))))
    if(length(lowcount.holidays)!=0) g<-g+annotate("rect", fill="#009e73",xmin = lowcount.holidays-.2, xmax = lowcount.holidays+.2, ymin = parameter.df[i,2], ymax = parameter.df[i,3],alpha = .1)
    if(length(lowcount.workdays)!=0) g<-g+annotate("rect", fill="red",xmin = lowcount.workdays-.2, xmax = lowcount.workdays+.2, ymin = parameter.df[i,2], ymax = parameter.df[i,3],alpha = .1)
    g<-g+geom_point(size=.8,alpha=.2)
    g<-g+geom_line(stat="smooth",method="loess",se=FALSE,span=0.1,alpha=.75,size=1)
    g<-g+scale.x.setup+lims(y=c(parameter.df[i,2],parameter.df[i,3]))
    g<-g+legend.setup+lab.setup
    
    ggsave(g,filename=paste0(output.path,parameter.df[i,1],"_",vdid,"_",carid,"_",output.time,".png"),width=8.8,height=3.6)
  }
}