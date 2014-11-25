
distribution <- function(rid=NULL,q=NULL,choices=NULL,selected=NULL,demo=NULL,mult=F,dir=NULL, dirv=NULL){
  
  if(is.null(demo)){
    demo=0
  }
  #rid and q are essential, we cannot do anything without it
  if(is.null(rid)|is.null(q)){
    er <- list(type=1,message="You have not provided a rid and q parameter")
    return(er)
  }else{
    if(mult){
      par<-"m"
    }else{
      par<-"s"
    }
    #parse a filename from rid and question number
    if(is.null(dir)|is.null(dirv)){
      fname <- paste("/home/opencpu/data",rid,"_",q,"_",par,".csv",sep="")
      fname_v <- paste("/home/opencpu/data",rid,"_",q,"_",par,"variance.csv",sep="")
    }else{
      fname <-dir
      fname_v <- dirv
    }
  }
  #if no selection is provided, we read the current file without writing in it
  if(is.null(selected)){
    #check if data.frame has been stored before
    if(file.exists(fname)){
      d<-read.table(fname,header=T,sep=";",stringsAsFactors=F)
    }else{
      er <- list(type=2,message="You are trying to read from a file that does not exist")
      return(er)
    }
    #selection is provided
  }else{
    if(!mult & length(selected)>1){
      er<-list(type=3,message="You claim this is single choice, yet selected has more items")
      return(er)
    }
    #check if data.frame has been stored before
    if(file.exists(fname)){
      d<-read.table(fname,header=T,sep=";",stringsAsFactors=F)
    }else{
      d<-data.frame()
    }
    #is selected a multiple choice/single choice
    if(mult){
      #define column names
      choice_length <- c(1:length(choices))
      cnames <- c(sapply(choice_length,function(x) paste(q,"_",x,sep="")),"demo")
      row <- c(sapply(choices,function(x) ifelse(x %in% selected,x,0)),demo)
      #rbind new entry with the old
      d<-rbind(d,row)
      colnames(d)<-cnames
    }else{
      #the same as above but without matching
      cnames <- c(paste(q,sep=""),"demo")
      row <- c(selected,demo)
      #rbind new entry with the old
      d<-rbind(d,row)
      names(d)<-cnames
    }
  }
  #output variables multiple/single
  if(mult){
    dist<-sapply(choices,function(x) length(d[d==x]))
    if(is.null(demo)){
      demodist<-0
    }else{
      demodist<-0
    }
    d[,choice_length]<-apply(d[,choice_length],2,as.numeric)
    mn <- apply(d[,choice_length],2,mean)
    sd <- apply(d[,choice_length],2,sd)
    v <- apply(d[,choice_length],2,var)
    
  }else{
    dist<-as.data.frame(table(label=d[,q]),responseName="freq",stringsAsFactors=F)
    dist$pct <- round(100*(dist$freq/nrow(d)),2)
    if(is.null(demo)){
      demodist<-0
    }else{
      demodist<-as.data.frame(table(label=d[,q],demo=d[,"demo"]),responseName="freq",stringsAsFactors=F)
      demodist$pct <- round(100*(demodist$freq/nrow(d)),2)
    }
    mn <- mean(as.numeric(d[,q]))
    sd <- sd(as.numeric(d[,q]))
    v <- var(as.numeric(d[,q]))
  }
  
  #resave file
  write.table(d,file=fname,sep=";",row.names=F)
  write.table(v,file=fname_v,sep=";",row.names=F,append=T)
  n<-nrow(d)
  #return list
  r<- list(data=d,dist=dist,mean=mn,sd=sd,var=v,n=n,type=0,demodist=demodist,message="OK")
  return(r)
  
}

