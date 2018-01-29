lapply(wiot.files, function(x){
  y<-open.rdata(x)
  valuematrixonly(y, mode="valueadded")
})

# Create valueadded matrix
valuematrixonly<-function(df, mode="flows", sector="FRA20"){
  
  # Modes: "flows", "techcoef", "valueadded"
  df<-as.data.frame(df)
  
  if(mode=="flows"){
    df.cin<-ci.matrix(df)
  }
  else if(mode=="techcoef"){
    df.cin<-cit.matrix(df)
  }
  else if(mode=="valueadded"){
    df.cin<-valueadded.matrix(df)
  }
  else {print("Enter a valid mode: flows, techcoef, valueadded")}
  
  save(df.cin, file=paste0("../outputs/matrix/",mode,"/",df[2,5],"_",mode,".RData"))
}

wiot.vmatrix<-list.files(path="../outputs/matrix/valueadded", full.names=TRUE)
wiot.vmatrix

lapply(wiot.vmatrix, function(x){
  y<-open.rdata(x)
  #z<-substr(deparse(substitute(x)),30,33)
  #print(z)
  colrowfr(y)
})

#df<-open.rdata(wiot.vmatrix[1])

colrowfr<-function(dfx,year){
  df<-as.data.frame(dfx)
  df.colnames<-colnames(df)
  df1<-data.frame(col=df[,"FRA20"])
  #which( colnames(df)=="FRA20" )
  df2<-as.data.frame(df[860,])
  df2<-t(df2)
  df2<-as.data.frame(df2)
  colnames(df2)<-"row"
  df2$col<-df1$col
  df2$ind<-rownames(df2)
  topcol<-df2[order(df2$col, decreasing = TRUE), c("ind","col")]
  toprow<-df2[order(df2$row, decreasing = TRUE), c("ind","row")]
  df3<-list(topcol,toprow)
  
  save(df3, file=paste0("../outputs/matrix/FRA20/",year,".RData"))
}

y<-open.rdata(wiot.vmatrix[1])
colrowfr(y,"2000")


y<-open.rdata(wiot.vmatrix[6])
colrowfr(y,"2005")

y<-open.rdata(wiot.vmatrix[11])
colrowfr(y,"2010")


y<-open.rdata(wiot.vmatrix[15])
colrowfr(y,"2014")
