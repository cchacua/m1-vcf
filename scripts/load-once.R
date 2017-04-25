# LS data
wiot.files<-list.files(path="../data/wiot", full.names=TRUE)
wiot.files

# Data list (Run Once)
wiot<-lapply(wiot.files,open.rdata)
save(wiot, file="../outputs/wiot.RData")
wiot.ci.matrix<-lapply(wiot,ci.matrix)
save(wiot.ci.matrix, file="../outputs/wiot.ci.matrix.compress.RData")
rm(wiot.ci.matrix)
wiot.cit.matrix<-lapply(wiot,cit.matrix)
save(wiot.cit.matrix, file="../outputs/wiot.cit.matrix.RData")
rm(wiot.cit.matrix)


# Create networks
lapply(wiot.files, function(x){
  y<-open.rdata(x)
  networks(y, mode="flows")
})

#lapply(wiot, networks, mode="flows")
lapply(wiot, networks, mode="techcoef")