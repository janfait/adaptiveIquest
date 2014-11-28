###################
# DISTRIBUTION
#
#  @q - [povinna, text] promenna otazky, jako napriklad "q1", "s10" apod.
#  @rid - [povinna, integer] cislo vyzkumu
#  @selected - [volitelna, integer/string] kterou hodnotu respondent vybral napr. 1,2,666 , v pripade multiple choice pak c(1,2,3)
#  @choices - [volitelan,integer] jake hodnoty ma na vyber
#  @mult - [volitelna,logical] je otazka multiple choice? napr. TRUE FALSE
#  @dir - [povinna,text] nazev souboru v adresari /home/opencpu/data/#####.csv kam se uklada rozdeleni
#  @dirv - [povinna,text] nazev souboru v adresari /home/opencpu/data/#####_v.csv  kam se uklada rozptyl
#
#  Ex : distribution(rid=10,q="q1",mult=T,selected=c("1","2"), choices=c(1,2,3,4,5,6,7),dir="C:/Users/fait/Desktop/test.csv",dirv="C:/Users/fait/Desktop/testv.csv")
##################

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
    #parse text
    if(is.character(choices)){
      choices <- eval(parse(text=choices))
    }
    if(is.character(selected)){
      selected <- eval(parse(text=selected)) 
    }
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
    dist<-unname(unlist(d))
    dist<- as.data.frame(table(label=dist),responseName="freq",stringsAsFacors=F)
    dist$pct <- round(100*(dist$freq/nrow(d)),2)
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
  
  if(file.exists(fname_v)){
    write.table(t(v),file=fname_v,sep=";",row.names=F,append=T,col.names=F)
  }else{
    write.table(t(v),file=fname_v,sep=";",row.names=F,col.names=T)
  }
  
  #resave file
  write.table(d,file=fname,sep=";",row.names=F)
  
  n<-nrow(d)
  #return list
  r<- list(data=d,dist=dist,mean=mn,sd=sd,var=v,n=n,type=0,demodist=demodist,message="OK")
  return(r)
  
}

###################
# DELETE
#
#  @dir - [povinna, text] adresa souboru, ktery chcete vymazat nejcasteji "/home/opencpu/data/xxxx.csv"
#
#  #############################

delete <- function(dir=NULL){

  if(is.null(dir)){
    er <- list(type=1,message="You have not provided a filename")
    return(er)
  }else{
    if(file.exists(dir)){
      res<-try(file.remove(dir),silent=T)
      if(inherits(res,"try-error")){
        res<-list(type=3,"Failed to remove the file. Dunno what is wrong.")
      }else{
        res<-list(type=0,message="File removed")
      }
      return(res)
    }else{
      er <- list(type=2,message="The file you are trying to delete do not exist")
      return(er)
    }
  }
}
