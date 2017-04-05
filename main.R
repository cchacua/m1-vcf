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
source("./scripts/functions.R")


# Load data
wiot<-open.rdata("../outputs/wiot.RData")

#####
# Networks of flows
#####
  n.flows.files<-list.files(path="../outputs/networks/flows", full.names=TRUE)
  
  lapply(n.flows.files,function(x){
    y<-open.rdata(x)
    networks.degree(y, .1)
  })
  
  lapply(n.flows.files,function(x){
    y<-open.rdata(x)
    networks.strenght(y, 50)
  })
  
  
  # y<-open.rdata(n.flows.files[1])
  # networks.strenght(y, 50)
  
  #Special cases
  #2014 CHN27 ROW27 Construction, ROW24 Electricity, gas, steam and air conditioning supply  CHN15 Manufacture of basic metals
  y.df<-wiot[15]
  y.df<-as.data.frame(y.df)
  View(y.df[,c("IndustryCode", "IndustryDescription", "Country", "RNr", "CHN27", "ROW27", "ROW24")])
  y<-open.rdata(n.flows.files[15])
  y.s<-strength(y$Network, mode = "in")
  y.s<-as.data.frame(y.s)
  
  #2000 USA51 Public administration and defence; compulsory social 
  # USA27 Construction  USA44 Real estate activities  USA5 Manufacture of food products, beverages
  y.df<-wiot[1]
  y.df<-as.data.frame(y.df)
  View(y.df[,c("IndustryCode", "IndustryDescription", "Country", "RNr", "USA51", "ROW27", "ROW24")])
  y<-open.rdata(n.flows.files[1])
  y.s<-strength(y$Network, mode = "in")
  y.s<-as.data.frame(y.s)

    
#####
# Networks of technical coefficients
#####
  n.techcoef.files<-list.files(path="../outputs/networks/techcoef", full.names=TRUE)
  lapply(n.techcoef.files,function(x){
    y<-open.rdata(x)
    networks.degree(y, .1)
  })
  
  lapply(n.techcoef.files,function(x){
    y<-open.rdata(x)
    networks.strenght(y, binwidth=.1, thousands = FALSE)
  })
  
  
  # Special Cases
  # Negative value added: 2013: Luxembourg 35 Postal and courier activities and 32 Water transport
  # TOP CI: BRA10 DNK10 Manufacture of coke and refined petroleum products MLT4 Mining and quarrying
  y<-open.rdata(n.techcoef.files[14]) 
  y.s<-strength(y$Network, mode = "in")
  y.s<-as.data.frame(y.s)
  y.df<-wiot[14]
  y.df<-as.data.frame(y.df)
  View(y.df[,c("IndustryCode", "IndustryDescription", "Country", "RNr", "LUX32", "LUX35")])

  #networks.strenght(y,  binwidth=.1, thousands = FALSE)
  
  # 2012 MLT4 Mining and quarrying
  y.df<-wiot[13]
  y.df<-as.data.frame(y.df)
  View(y.df[,c("IndustryCode", "IndustryDescription", "Country", "RNr", "MLT4")])
  y<-open.rdata(n.techcoef.files[13])
  y.s<-strength(y$Network, mode = "in")
  y.s<-as.data.frame(y.s)
  #networks.strenght(y,  binwidth=.1, thousands = FALSE)
  
  
  # 2011 MLT4 Mining and quarrying, 33 Air transport
  y.df<-wiot[12]
  y.df<-as.data.frame(y.df)
  View(y.df[,c("IndustryCode", "IndustryDescription", "Country", "RNr", "MLT4", "LVA33")])
  y<-open.rdata(n.techcoef.files[12])
  y.s<-strength(y$Network, mode = "in")
  y.s<-as.data.frame(y.s)
  #networks.strenght(y,  binwidth=.1, thousands = FALSE)
  
# Leontief Matrix
  
  L<-leontief.matrix(wiot[12])
  