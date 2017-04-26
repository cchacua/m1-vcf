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
  #wiot<-open.rdata("../outputs/wiot.RData")

# Connected components
  #comp.distribution(wiot[1])

#####
# Network of flows
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

  # CLustering
  clustering.flow<-lapply(n.flows.files,function(x){
    y<-open.rdata(x)
    year<-y$Year
    net<-y$Network
    net<-as.undirected(net, mode="collapse")
    localw<-transitivity(net,  vids ="FRA20" ,type="weighted")
    averagecl<-transitivity(net,type="average")
    global<-transitivity(net, type="global")
    print(year)
    w<-c(Year=year,Local=localw, Average=averagecl, Global=global)
  })
  clustering.flow.2<-as.data.frame(clustering.flow)
  clustering.flow.2<-t(clustering.flow.2)
  clustering.flow.2<-as.data.frame(clustering.flow.2)
  #rownames(clustering.flow.2)<-clustering.flow.2$Year
  clustering.flow.2<- melt(clustering.flow.2, id="Year")
  write.csv(clustering.flow.2, "../outputs/clustering.flow.csv")
  
  clustering.flow.graph<-ggplot(data=clustering.flow.2, aes(x=Year, y=value, group=variable, color=variable)) + geom_line() + geom_point()
  clustering.flow.graph
  
  
#####
# Network of technical coefficients
#####
  n.techcoef.files<-list.files(path="../outputs/networks/techcoef", full.names=TRUE)
  
  # Special Cases
  # Negative value added: 
  
  lapply(n.techcoef.files, manip.negav)
  
  # In brief, sectors that were deleted are:
  #2014
  #"LUX32", "MLT4"
  #2013
  #"LUX32", "LUX35", 
  #2012
  #"MLT4"
  #2011
  #"MLT4", "LVA33"
  #2000
  # "MLT4"
  #c("LUX32", "LUX35", "MLT4", "LVA33")
  
  # Degree distribution
  lapply(n.techcoef.files,function(x){
    y<-open.rdata(x)
    networks.degree(y, .1)
  })
  
  # Strength distribution
  lapply(n.techcoef.files,function(x){
    y<-open.rdata(x)
    networks.strenght(y, binwidth=.1, thousands = FALSE)
  })
  
  # Network size
  sizes<-lapply(n.techcoef.files,function(x){
    y<-open.rdata(x)
    year<-y$Year
    net<-y$Network
    nodes<-gorder(net)
    edges<-gsize(net)
    w<-c(Year=year,Nodes=nodes,Edges=edges)
  })
  sizes<-t(as.data.frame(sizes))
  sizes<-as.data.frame(sizes)
  rownames(sizes)<-sizes$Year
  
  nodesizes.plot<-ggplot(sizes, aes(Year, Nodes, colour="#7CAE00")) + geom_line(size=1)+ geom_point(size=2)+xlab("Year") + ylab("Number of nodes")+ theme(legend.position="none")
  ggsave(paste0("../outputs/","node_sizes", ".png", sep=""), plot = nodesizes.plot, device = "png",
         scale = 1, width = 16, height = 5, units = "cm",
         dpi = 300, limitsize = TRUE) 
  
  edgesizes.plot<-ggplot(sizes, aes(Year, Edges, colour="#7CAE00")) + geom_line(size=1)+ geom_point(size=2)+xlab("Year") + ylab("Number of edges")+ theme(legend.position="none")
  ggsave(paste0("../outputs/","edge_sizes", ".png", sep=""), plot = edgesizes.plot, device = "png",
         scale = 1, width = 16, height = 5, units = "cm",
         dpi = 300, limitsize = TRUE) 
  
  # See if AUS46 belong to the connected component in 2000
    # net.2000.ci<-open.rdata(n.techcoef.files[1])
    # net.2000.ci<-net.2000.ci$Network
    # subcomponent(net.2000.ci,"AUS49", mode ="all")

#####
# Networks of value added
#####    

  n.valueadded.files<-list.files(path="../outputs/networks/valueadded", full.names=TRUE)
  
  # Strength distribution
  lapply(n.valueadded.files,function(x){
    y<-open.rdata(x)
    networks.strenght(y, binwidth=.1, thousands = FALSE)
  })
  
  # See if AUS46 belong to the connected component in 2000
    # net.2000.va<-open.rdata(n.valueadded.files[1])
    # net.2000.va<-net.2000.va$Network
    # subcomponent(net.2000.va,"AUS47", mode ="all")
  