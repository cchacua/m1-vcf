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


# LS data
  wiot.files<-list.files(path="../data/wiot", full.names=TRUE)
  wiot.files
# Data list
  wiot<-lapply(wiot.files,open.rdata)

# Network with only CI data  
  wiot.df<-as.data.frame(wiot[1])
  wiot.colnames<-as.data.frame(colnames(wiot.df))
  # CI data goes until line 2464= 44 countries * 56 sectors
  wiot.df<-wiot.df[1:2464,6:2470]
  
  
# Network with in terms of technical coefficients
  wiot.df<-as.data.frame(wiot[1])
  # CI data goes until line 2464= 44 countries * 56 sectors
  # Line 2472 (GO) has the Output at basic prices
  wiot.df<-wiot.df[1:2464,6:2470]
  


wiot.2000.adj<- wiot.2000
wiot.2000.net <- network(wiot.2000.adj, matrix.type="adjacency", directed=TRUE,
                loops=F, multiple=F, ignore.eval = F)



