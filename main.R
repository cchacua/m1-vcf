# Institution: Université de Bordeaux
# Subject: Mémoire M1- Intelligence économique
# Author: Christian Mauricio CHACUA DELGADO

memory.size(max = TRUE)
options(digits=4)
options(stringsAsFactors=FALSE)


wiot.files<-list.files(path="../data/wiot", full.names=TRUE)
wiot.files
wiot.2000<-load(wiot.files[1])



# Install packages
source("./scripts/packages-install.R")
# Load packages
source("./scripts/packages-load.R")


