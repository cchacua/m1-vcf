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

  wiot.ci.matrix<-lapply(wiot,ci.matrix)
  save(wiot.ci.matrix, file="../outputs/wiot.ci.matrix.RData", compress="bzip2")
  rm(wiot.ci.matrix)
  wiot.cit.matrix<-lapply(wiot,cit.matrix)
  save(wiot.cit.matrix, file="../outputs/wiot.cit.matrix.RData")
  rm(wiot.cit.matrix)
  
  
# Network with only CI data  


  
# Network with in terms of technical coefficients

  
  