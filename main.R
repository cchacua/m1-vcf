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
  wiot.df.ci<-wiot.df[1:2464,6:2469]
  # Columns 6:2469 have the CI data
  # Columns 57-61 for each country goes from colums 2470:2689
    # CONS_h,	CONS_np,	CONS_g,	GFCF,	INVEN,	EXP	GO
    # Final consumption expenditure by households	Final consumption expenditure by non-profit organisations serving households (NPISH)	Final consumption expenditure by government	Gross fixed capital formation	Changes in inventories and valuables	Exports	Total output
  # Colum 2690 has the total production, which is the same value as in line 2472 (GO)
    # View(wiot.df[1,2690])
    # View(wiot.df[2472,6])
  wiot.net.ci<- network(wiot.df.ci, matrix.type="adjacency", directed=TRUE, loops=TRUE, multiple=F, ignore.eval = F)
  #?network
  
  
# Network with in terms of technical coefficients
  wiot.df<-as.data.frame(wiot[1])
  # CI data goes until line 2464= 44 countries * 56 sectors
  # Line 2472 (GO) has the Output at basic prices
  wiot.df.ci<-wiot.df[c(1:2464, 2472),6:2469]
  wiot.df.cin<-sapply(wiot.df.ci, dividelast)
  wiot.df.cin<-as.data.frame(wiot.df.cin)
