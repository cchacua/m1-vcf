# Institution: Université de Bordeaux
# Subject: Mémoire M1- Intelligence économique
# Author: Christian Mauricio CHACUA DELGADO

memory.size(max = TRUE)
options(digits=4)
options(stringsAsFactors=FALSE)


# Install packages
#source("./scripts/packages-install.R")
# Load packages
source("./scripts/packages-load.R")
# Create output files from the original data (Run once)
# source("./scripts/load-once.R")
wiot.cit.matrix<-open.rdata("../outputs/wiot.cit.matrix.RData")

ci.2000<-as.data.frame(wiot[1])
ci.2000<-ci.matrix(ci.2000)

wiot.df<-as.data.frame(wiot[1])
wiot.df.cin<-cit.matrix(wiot.df)
cit.names<-colnames(wiot.df.cin)
cit.names<-remRight(cit.names,2)
rownames(wiot.df.cin)<-cit.names
colnames(wiot.df.cin)<-cit.names
wiot.df.cin$id<-rownames(wiot.df.cin)
cit.mlist<-melt(wiot.df.cin, id = "id")
#cit.mlist$value<-ifelse(cit.mlist$value<=0.000001, NA, cit.mlist$value*100)
cit.mlist$value<-ifelse(cit.mlist$value==0, NA, cit.mlist$value*100)
cit.mlist<-na.omit(cit.mlist)
cit.nodes<-cbind(cit.names, wiot.df[1:2464,1:5])
cit.net<-network(cit.mlist[1:100,], vertex.attr=cit.nodes, matrix.type="edgelist", loops=T, multiple=F, ignore.eval = T)
plot(cit.net)
summary(cit.mlist[,3])



cit.2000<-sapply(cit.2000, zerosmall)
cit.2000<-as.data.frame(cit.2000)
summary(cit.2000[,2])
cit.2000<-as.matrix(cit.2000)
cit.2000.net<-as.network(cit.2000)
tnet<-networkDynamic(network.list=list(cit.2000.net,cit.2000.net))

# Network with only CI data  


  
# Network with in terms of technical coefficients

  
  