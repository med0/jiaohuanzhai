
#---------funs start---
library(RCurl)
library(XML)

# 1.
.getPost <- function(x){
  #post <- scan(x,what="character",quiet=TRUE,sep="\n")
  y=strsplit(x,"&")[[1]]
  lst=gsub("(^.*?)(=)(.*$)","\\3",y)
  lstnames=gsub("(^.*?)(=)(.*$)","\\1",y)
  names(lst)=lstnames
  lst=as.list(lst)
  return(lst)
}

# 1. end


#2. start
.posturl=function(){
  theurl=paste0(
    "http://www.szse.cn/szseWeb/FrontController.szse?randnum=",
    format(runif(1),digits=16))
  return(theurl)
}
#2. end 

#3. start
.tryWeb=function(FUN,theurl,curl=myHandle,failsleep=0.45,oksleep=0.15,...){
  success <- FALSE
  failnum = 0 # create the time according the FAIL TIME 
  while (!success){
    cat("downloading...\n")
    success=tryCatch({
      temp=FUN(theurl,curl=curl,...)
      cat("download ok.","\n\n\n")
      Sys.sleep(oksleep)
      TRUE
    } ,error = function(e) {
      failnum <<-failnum+1
      cat(e$message,"\n")
      cat("error! try again after",failsleep+2*failnum,"seconds...","\n")
      Sys.sleep(failsleep+2*failnum)
      FALSE
    },warning  = function(e) {
      failnum <<- failnum+1
      cat(e$message,"\n")
      cat("warining! try again after",failsleep+2*failnum,"seconds...","\n")
      Sys.sleep(failsleep+2*failnum)
      FALSE})  
  }
  return(temp)
}
#3. end

# start 
.getExclusive= function(conn,failsleep=2){
  success= FALSE 
  failnum=0 # create the time according the FAIL TIME 
  while (!success){
    success=tryCatch({
      temp=dbGetQuery(conn,"begin exclusive transaction;")
      cat("exclusive transaction ok","\n\n")
      TRUE  
    },error = function(e) {
      cat(e$message,"\n")
      #cat("error! try again ...\n")
      failnum <<- failnum+1
      #cat(failsleep+2*failnum,"\n")
      cat("error! try again after",failsleep+2*failnum,"seconds...","\n")
      Sys.sleep(failsleep+2*failnum)
      FALSE
    },warning  = function(e) {
      cat(e$message,"\n")
      #cat("warining! try again ...\n")
      failnum <<- failnum+1
      #cat(failsleep+2*failnum,"\n")
      cat("warining! try again after",failsleep+2*failnum,"seconds...","\n")
      Sys.sleep(failsleep+2*failnum)
      FALSE
    }
    )
    
  }
  return(success)
}
# end 

#4. start
# the fun do :
# wrap the fun: .getWeb according to sz stock exchange website 
# and reencode the web ,then parse the web using lib XML 
.szseWeb=function(postlst){
  temp=.tryWeb(postForm,theurl=.posturl(),.params=postlst,
               curl=myHandle,style="POST",.encoding="utf-8",
               .contentEncodeFun = c)
  temp=iconv(temp,"GBK","utf-8")
  # these above  encoding parament succeed ,but do not know why :(
  doc <- htmlParse(temp,asText=TRUE,useInternalNodes=TRUE,encoding="UTF-8") 
  return(doc)
}
#4. end



#get total pages POST INFORMATION by setting startDATE and endDATE
#wrap the fun .szseWeb 
.pageinfo=function(startdate,enddate){
  #startdate=as.Date("2015-1-1")
  #enddate=as.Date("2015-1-31")
  thepostlst=list(
    ACTIONID="7",
    AJAX="AJAX-TRUE",
    CATALOGID="1842_xxpl",
    TABKEY="tab1",
    txtDMorJC="",
    txtStart=as.character(startdate),
    txtEnd=as.character(enddate),
    REPORT_ACTION="search"
  )
  
  cat("try to get page information from",
      as.character(startdate),"to",as.character(enddate),"\n")
  doc=.szseWeb(postlst=thepostlst)
  myPath="//table/tr/td/input[@class='cls-navigate-go']"
  pageinfo=xpathApply(doc,path=myPath,fun=xmlGetAttr,"onclick")[[1]][1]
  # another method
  #els = getNodeSet(doc,myXPath)
  #pageinfo= sapply(els,xmlGetAttr,"onclick")
  if (is.null(pageinfo)){
    pageinfo=thepostlst
    cat("there are",1,"page","\n")
  } else {
    pageinfo=gsub("(^.+szse\\?)(ACTIONID.+?)('.+$)","\\2",pageinfo)
    pageinfo=.getPost(pageinfo)
    cat("there are",pageinfo$tab1PAGECOUNT,"pages","\n")
  }
  
  return(pageinfo)
}

#-----end 

#---start 
#the fun download all POSTLISTs according to the parament pageinfo which is created by fun .pageinfo
.getAllpostlst=function(pageinfo){
  f1=function(i){
    cat("try to download the page",i,"\n")
    pageinfo$tab1PAGENUM=as.character(i)
    doc = .szseWeb(pageinfo)
    # 
    myPath="//table[@id='REPORTID_tab1' and @class='cls-data-table']/tr/td/a"
    allpostlst=xpathSApply(doc,path=myPath,fun=xmlGetAttr,"onclick")
    allpostlst=gsub("(^.+szse\\?)(.+?)('.*$)","\\2",allpostlst)
    return(allpostlst)
  }
  # deal with ONLY 1 pages 
  n=ifelse (is.null(pageinfo$tab1PAGECOUNT),1,as.integer(pageinfo$tab1PAGECOUNT))
  allpostlst=lapply((1:n) ,FUN=f1)
  allpostlst=unlist(allpostlst)
  return(allpostlst)  
}


#--get the data I want exactly
.getLonghu=function(postchar){
  postlst=.getPost(postchar)
  cat("try to download LongHuBang:","\n")
  doc=.szseWeb(postlst)
  
  
  # get the buying top 1th-5th & selling top 1th-5th  table
  myXPath="//table[@id='REPORTID_tab2']"
  els = getNodeSet(doc,myXPath)
  buy5sell5=readHTMLTable(els[[1]],
                          colClasses=c(rep("character",2),rep("FormattedNumber",2)) ,
                          stringsAsFactors=FALSE)
  
  buysellidx=gsub("(^买|卖)([0-9]$)","\\1",buy5sell5[,1])
  orderidx=as.numeric(gsub("(^买|卖)([0-9]$)","\\2",buy5sell5[,1]) )
  buy5sell5= data.frame(buysellidx,orderidx,
                        yingyebu=gsub("\\s","",buy5sell5[,2]),
                        buy=buy5sell5[,3],
                        sell=buy5sell5[,4],stringsAsFactors=FALSE)
  
  # stock buy & sell information
  myXPath="//table[@class='cls-data-table-detail']"
  els = getNodeSet(doc,myXPath)
  stockinfo=readHTMLTable(els[[1]],header=FALSE,stringsAsFactors=FALSE)
  useinfo=list(
    gonggaodate=stockinfo[1,2],
    yichangqijian=stockinfo[1,4],
    stockname=gsub("(^.+?)\\s+\\S*[0-9].*$","\\1",stockinfo[2,1]),
    stockcode=gsub("^.+?([0-9]{2,9}).*$","\\1",stockinfo[2,1]),
    chengjiaoliang=as.numeric(gsub("(^[0-9]{1,100})\\s+.*$","\\1",stockinfo[2,3]) ),
    chengjiaojine=as.numeric(gsub("(^[0-9]{1,100})\\s+.*$","\\1",stockinfo[2,5]) ),
    piluyuanyin=stockinfo[3,1]
  )
  useinfo=as.data.frame(lapply(useinfo,rep,nrow(buy5sell5)),stringsAsFactors=FALSE)
  buy5sell5=cbind(buy5sell5,useinfo)
  return(buy5sell5) 
}
# end 

# start 
# a useful FUN get from internet,a fun creatt anoher fun 
f <- function(lst)
  function(nm) unlist(lapply(lst, "[[", nm), use.names=FALSE)
# end


#start 
szLonghu=function(startdate,enddate) {
  pageinfo=.pageinfo(startdate,enddate)
  postlsts=.getAllpostlst(pageinfo)
  x=lapply(postlsts, .getLonghu)
  df0 <- as.data.frame(Map(f(x), names(x[[1]])),
                       stringsAsFactors=FALSE)
  #df1 <- do.call(rbind, x)
  #identical(df0, df1)
  df0$jiaoyisuo="sz"
  return(df0)
}


#end
#---------funs end-----


#------set http header ,then CURL handle  start 
httpheader<- c(
  "User-Agent"="Mozilla/5.0 (Macintosh; Intel Mac OS X 10.11; rv:48.0) Gecko/20100101 Firefox/48.0",
  "Accept"="application/json, text/javascript, */*; q=0.01",
  "Accept-Language"="zh-CN,zh;q=0.8,en-US;q=0.5,en;q=0.3",
  "Accept-Encoding"="gzip, deflate",
  "Content-Type"="application/x-www-form-urlencoded; charset=UTF-8",
  "X-Requested-With"="XMLHttpRequest",
  "Connection"="keep-alive"
)
myHandle<- getCurlHandle(httpheader =httpheader,
                         verbose=TRUE,
                         connecttimeout=15,
                         cookiefile="",
                         timeout=10)  
#------set http header ,then CURL handle  end 

#---set the SQLite Database name start
longhubangsqlitename="/Users/miaobingxiang/R/longhubang.sqlite"
#---set the SQLite Database name end
# 
.writelonghubangTable=function(dbname,tablename,value){
  con <- dbConnect(RSQLite::SQLite(), dbname)   #create connect
  exc_success=.getExclusive(con)   # create exclusive transmation
  success=FALSE
  while (!success){
    success=tryCatch({
      write_sucess=dbWriteTable(con,tablename,value,append=TRUE)
      a=dbGetQuery(con,"commit")
      #dis_success=dbDisconnect(con)
      TRUE
    },error = function(e) {
      cat(e$message,"\n")
      a=dbGetQuery(con,"rollback")
      #dis_success=dbDisconnect(con)
      FALSE
    },warning = function(e) {
      cat(e$message,"\n")
      a=dbGetQuery(con,"rollback")
      #dis_success=dbDisconnect(con)
      FALSE
    })
  }  
  dis_success=dbDisconnect(con)
  success  
}

# end


# 0 get the last date 
con <- dbConnect(RSQLite::SQLite(), longhubangsqlitename)#create connect
dateidx = dbGetQuery(con,"select gonggaodate from longhubang;")
dbDisconnect(con)
startdateidx = as.character(max(as.Date(dateidx$gonggaodate))+1)
enddateidx=as.character(Sys.Date())

startdateidx
enddateidx
# 1---------shenzhen longhubang start
sz_temp=szLonghu(startdateidx,enddateidx)
.writelonghubangTable(longhubangsqlitename,"longhubang",sz_temp)




# 1---------shenzhen longhubang end

#column="szse",
postlst=list(
  category="",
  column="",
  columnTitle="历史公告查询",
  limit="",
  pageNum=1,
  pageSize=30,
  plate="",
  seDate="请选择日期",
  searchkey="可交换;",
  showTitle="-1/searchkey/可交换",
  sortName="",	
  sortType="",
  stock="",
  tabName="fulltext",
  trade=""
)


(temp=.tryWeb(postForm,theurl="http://www.cninfo.com.cn/cninfo-new/announcement/query",
             .params=postlst,
             curl=myHandle,style="POST",.encoding="UTF-8"))

#write.table(temp,"d:\\c2.html",row.names = "",col.names = "",quote=FALSE)

cat(temp)

#temp=iconv(temp,"GBK","UTF-8")
#temp=gsub("charset\\s*=\\s*gb2312","charset=UTF-8",temp)
#attr(temp,"Content-Type")[2]='UTF-8'
# these above  encoding parament succeed ,but do not know why :(
doc <- htmlParse(temp,asText=TRUE,useInternalNodes=TRUE,encoding="UTF-8") 

