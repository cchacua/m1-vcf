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

cit.2000<-as.data.frame(wiot[1])
cit.2000<-cit.matrix(cit.2000)
cit.2000<-sapply(cit.2000, zerosmall)
cit.2000<-as.data.frame(cit.2000)
summary(cit.2000[,2])



# Network with only CI data  


  
# Network with in terms of technical coefficients

  
  