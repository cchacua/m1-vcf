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
  

  # Communities
  lapply(n.flows.files,function(x, mode="flows"){
    y<-open.rdata(x)
    year<-y$Year
    print(year)
    y<-y$Network
    
    # y<-simplify(y, remove.multiple = FALSE, remove.loops = TRUE)
    # communityinfo<-cluster_infomap(y)
    # save(communityinfo, file=paste0("../outputs/communities/cluster_infomap/", year,mode, ".RData"))
    # ?membership
    
    y<-as.undirected(y, mode="collapse")
    community1<-cluster_fast_greedy(y)
    pdf(paste0("../outputs/communities/cluster_fast_greedy/", year,mode, ".pdf"), width=14, height=100)
    # Do some plotting
    plot_dendrogram(community1, cex = 0.2)
    # Close the PDF file's associated graphics device (necessary to finalize the output)
    dev.off()
    save(community1, file=paste0("../outputs/communities/cluster_fast_greedy/", year,mode, ".RData"))
    
    # community2<-multilevel.community(y)
    # save(community2, file=paste0("../outputs/communities/multilevel.community/", year,mode, ".RData"))
    # 
    })

  lapply(n.flows.files,function(x){
    y<-open.rdata(x)
    networks.degree(y, .1)
  })
  
  lapply(n.flows.files,function(x){
    y<-open.rdata(x)
    networks.strenght(y,  binwidth=2, thousands="2", xlims=c(30,20,20), ylims=c(150,150,150))
  })
  
  df.strenght<-function(datalist, thousands="1", mode="total"){
    # Modes: grid and ind
    # Types: d, cd, s, cs
    year<-datalist$Year
    print(year)
    cit.net<-datalist$Network
    if(mode=="total"){d.total<-strength.as.df(cit.net, "total", thousands)}
    else if (mode=="in"){d.in<-strength.as.df(cit.net, "in", thousands)}
    else if(mode=="out"){d.out<-strength.as.df(cit.net, "out", thousands)}
    else{print("No mode")}
  }
  total<-lapply(n.flows.files,function(x){
    y<-open.rdata(x)
    df.strenght(y, thousands="2")
  })
  View(total[1])
  
  
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
    local<-transitivity(net,  vids ="FRA20" ,type="local")
    averagecl<-transitivity(net,type="average")
    global<-transitivity(net, type="global")
    
    localw<-transitivity(net,  vids ="FRA20" ,type="weighted")
    
    localwall<-transitivity(net,type="weighted")
    localwall<-as.data.frame(localwall)
    localwall$values<-ifelse(localwall$localwall==Inf, NA, localwall$localwall)
    localwall$values<-ifelse(localwall$localwall>=1, 1, localwall$localwall)
    localwall<-mean(localwall$values, na.rm =TRUE)
    
    print(year)
    w<-c(Year=year,Local=local, Average=averagecl, Global=global, Local_W=localw,Average_W=localwall )
  })
  clustering.flow.2<-as.data.frame(clustering.flow)
  clustering.flow.2<-t(clustering.flow.2)
  clustering.flow.2<-as.data.frame(clustering.flow.2)
  #rownames(clustering.flow.2)<-clustering.flow.2$Year
  clustering.flow.2<- melt(clustering.flow.2, id="Year")
  write.csv(clustering.flow.2, "../outputs/clustering.flow.csv")
  
  clustering.flow.2<-read.csv("../outputs/clustering/clustering.flow.csv")
  clustering.flow.graph<-ggplot(data=clustering.flow.2, aes(x=Year, y=value, group=variable, color=variable)) + geom_line() + geom_point()+ylab("Clustering coefficient")+ guides(fill=guide_legend(title=NULL))+scale_x_continuous(minor_breaks = seq(2000 , 2014, 1), breaks = seq(2000 , 2014, 5))
  
  ggsave(paste0("../outputs/","clustering.flow", ".png", sep=""), plot = clustering.flow.graph, device = "png",
         scale = 1, width = 10, height = 5, units = "cm",
         dpi = 300, limitsize = TRUE) 
  
  # Communities
  lapply(n.flows.files[1],function(x){
    y<-open.rdata(n.flows.files[1])
    year<-y$Year
    print(year)
    net<-y$Network
    community<-cluster_edge_betweenness(net)
    save(x, file=paste0("../outputs/communities/newman/flows_",year,".RData"))
  })
  
  com.flows.files<-list.files(path="../outputs/communities/cluster_fast_greedy/flows", full.names=TRUE)  
  lapply(com.flows.files,function(x){
    print(x)
    name<-substr(x, nchar(x)-15+1, nchar(x)-6)
    print(name)
    y<-open.rdata(x)
    w<-membership(y)
    w<-as.list(w)
    w<-as.data.frame(w)
    w<-t(w)
    w<-as.data.frame(w)
    w$id<-rownames(w)
    w$Country<-substr(w$id, 1, 3)
    w$Sector<-substr(w$id, 4, nchar(w$id))
    w$Sector<-ifelse(nchar(w$Sector)==1, paste0("0",w$Sector,sep=""),w$Sector)
    w$V1<-ifelse(nchar(w$V1)==1, paste0("0",w$V1,sep=""),w$V1)
    w$V1<-as.factor(w$V1)
    w<-w[order(w$Country, decreasing = TRUE),]
    w$Community<-w$V1
    plot<-ggplot(w, aes(Sector, Country)) + geom_tile(aes(fill = Community), colour = "white") + scale_fill_manual(values= rainbow(length(unique(w$V1))))
    ggsave(paste0("../outputs/communities/cluster_fast_greedy/",name, ".png", sep=""), plot = plot, device = "png",
           scale = 1.9, width = 16, height = 8, units = "cm",
           dpi = 300, limitsize = TRUE) 
  })
  
  graph.assortativity("../outputs/networks/flows","flows")
  
#####
# Network of technical coefficients
#####
  n.techcoef.files<-list.files(path="../outputs/networks/techcoef", full.names=TRUE)
  
  # Communities
    lapply(n.techcoef.files,function(x, mode="techcoef"){
    y<-open.rdata(x)
    year<-y$Year
    print(year)
    y<-y$Network
    
    # y<-simplify(y, remove.multiple = FALSE, remove.loops = TRUE)
    # communityinfo<-cluster_infomap(y)
    # save(communityinfo, file=paste0("../outputs/communities/cluster_infomap/", year,mode, ".RData"))
    # ?membership
    
    y<-as.undirected(y, mode="collapse")
    community1<-cluster_fast_greedy(y)
    pdf(paste0("../outputs/communities/cluster_fast_greedy/", year,mode, ".pdf"), width=14, height=100)
    # Do some plotting
    plot_dendrogram(community1, cex = 0.2)
    # Close the PDF file's associated graphics device (necessary to finalize the output)
    dev.off()
    save(community1, file=paste0("../outputs/communities/cluster_fast_greedy/", year,mode, ".RData"))
     
    # community2<-multilevel.community(y)
    # save(community2, file=paste0("../outputs/communities/multilevel.community/", year,mode, ".RData"))
    # 
  })
  
  com.techcoef.files<-list.files(path="../outputs/communities/cluster_fast_greedy/techcoef", full.names=TRUE)  
  lapply(com.techcoef.files,function(x){
    print(x)
    name<-substr(x, nchar(x)-18+1, nchar(x)-5)
    print(name)
    y<-open.rdata(x)
    w<-membership(y)
    w<-as.list(w)
    w<-as.data.frame(w)
    w<-t(w)
    w<-as.data.frame(w)
    w$id<-rownames(w)
    w$Country<-substr(w$id, 1, 3)
    w$Sector<-substr(w$id, 4, nchar(w$id))
    w$Sector<-ifelse(nchar(w$Sector)==1, paste0("0",w$Sector,sep=""),w$Sector)
    w$V1<-ifelse(nchar(w$V1)==1, paste0("0",w$V1,sep=""),w$V1)
    w$V1<-as.factor(w$V1)
    w<-w[order(w$Country, decreasing = TRUE),]
    w$Community<-w$V1
    plot<-ggplot(w, aes(Sector, Country)) + geom_tile(aes(fill = Community), colour = "white") + scale_fill_manual(values= rainbow(length(unique(w$V1))))
    ggsave(paste0("../outputs/communities/cluster_fast_greedy/",name, ".png", sep=""), plot = plot, device = "png",
           scale = 1.9, width = 16, height = 8, units = "cm",
           dpi = 300, limitsize = TRUE) 
  })
  
  
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
    networks.strenght(y,  binwidth=.1, thousands="0", xlims=c(15,1,12), ylims=c(600,600,600))
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
  
  nodesizes.plot<-ggplot(sizes, aes(Year, Nodes, colour="#7CAE00")) + geom_line(size=1)+ geom_point(size=2)+xlab("Year") + ylab("Number of nodes")+ theme(legend.position="none")+scale_x_continuous(minor_breaks = seq(2000 , 2014, 1), breaks = seq(2000 , 2014, 5))
  ggsave(paste0("../outputs/","node_sizes", ".png", sep=""), plot = nodesizes.plot, device = "png",
         scale = 1, width = 8, height = 5, units = "cm",
         dpi = 300, limitsize = TRUE) 
  
  edgesizes.plot<-ggplot(sizes, aes(Year, Edges, colour="#7CAE00")) + geom_line(size=1)+ geom_point(size=2)+xlab("Year") + ylab("Number of edges")+ theme(legend.position="none")+scale_x_continuous(minor_breaks = seq(2000 , 2014, 1), breaks = seq(2000 , 2014, 5))
  ggsave(paste0("../outputs/","edge_sizes", ".png", sep=""), plot = edgesizes.plot, device = "png",
         scale = 1, width = 8, height = 5, units = "cm",
         dpi = 300, limitsize = TRUE) 

  plot.edgenodes<-grid.arrange(nodesizes.plot, edgesizes.plot, ncol=2)
  ggsave(paste0("../outputs/","edgenode_sizes", ".png", sep=""), plot = plot.edgenodes, device = "png",
         scale = 1, width = 16, height = 5, units = "cm",
         dpi = 300, limitsize = TRUE) 
  
  # See if AUS46 belong to the connected component in 2000
    # net.2000.ci<-open.rdata(n.techcoef.files[1])
    # net.2000.ci<-net.2000.ci$Network
    # subcomponent(net.2000.ci,"AUS49", mode ="all")
  graph.clustering(n.techcoef.files)
  clustering.graph<-ggplot(data=read.csv("../outputs/clustering/No/tecchnical coefficients.csv"), aes(x=Year, y=value, group=variable, color=variable)) + geom_line() + geom_point()+ylab("Clustering coefficient")+ guides(fill=guide_legend(title=NULL))+scale_x_continuous(minor_breaks = seq(2000 , 2014, 1), breaks = seq(2000 , 2014, 5))
  ggsave(paste0("../outputs/clustering/clustering.","tecchnical coefficients", ".png", sep=""), plot = clustering.graph, device = "png",
         scale = 1, width = 10, height = 5, units = "cm",
         dpi = 300, limitsize = TRUE) 
  
  net.techcoef.files<-files<-list.files(path="../outputs/networks/techcoef", full.names=TRUE)  
  assortativity<-lapply(net.techcoef.files,function(x){
    y<-open.rdata(x)
    w<-assortativity_degree(y$Network, directed = TRUE)
    c(w,y$Year)
  })
  assortativity<-as.data.frame(assortativity)
  assortativity<-t(assortativity)
  assortativity<-as.data.frame(assortativity)
  colnames(assortativity)<-c("Value", "Year")
  rownames(assortativity)<-assortativity$Year
  assortativity.plot<-ggplot(assortativity, aes(Year, Value, colour="#7CAE00")) + geom_line(size=1)+ geom_point(size=2)+xlab("Year") + ylab("Assortativity degree")+ theme(legend.position="none")+scale_x_continuous(minor_breaks = seq(2000 , 2014, 1), breaks = seq(2000 , 2014, 5))
  ggsave(paste0("../outputs/","assortativity.techcoef", ".png", sep=""), plot = assortativity.plot, device = "png",
         scale = 1, width = 8, height = 5, units = "cm",
         dpi = 300, limitsize = TRUE) 
  
  graph.assortativity("../outputs/networks/techcoef","techcoeff")
#####
# Networks of value added
#####    

  n.valueadded.files<-list.files(path="../outputs/networks/valueadded", full.names=TRUE)
  
  lapply(n.valueadded.files,function(x, mode="valueadded"){
    y<-open.rdata(x)
    year<-y$Year
    print(year)
    y<-y$Network
    
    # y<-simplify(y, remove.multiple = FALSE, remove.loops = TRUE)
    # communityinfo<-cluster_infomap(y)
    # save(communityinfo, file=paste0("../outputs/communities/cluster_infomap/", year,mode, ".RData"))
    # ?membership
    
    y<-as.undirected(y, mode="collapse")
    y.weight<-E(y)$weight
    y.weight<-ifelse(y.weight<0,0, y.weight)
    community1<-cluster_fast_greedy(y,  weights = y.weight)
    pdf(paste0("../outputs/communities/cluster_fast_greedy/", year,mode, ".pdf"), width=14, height=100)
    # Do some plotting
    plot_dendrogram(community1, cex = 0.2)
    # Close the PDF file's associated graphics device (necessary to finalize the output)
    dev.off()
    save(community1, file=paste0("../outputs/communities/cluster_fast_greedy/", year,mode, ".RData"))
    
    # community2<-multilevel.community(y)
    # save(community2, file=paste0("../outputs/communities/multilevel.community/", year,mode, ".RData"))
    # 
  })
  
  # Strength distribution
  lapply(n.valueadded.files,function(x){
    y<-open.rdata(x)
    networks.strenght(y, binwidth=.1, thousands = FALSE)
  })
  
  lapply(n.valueadded.files,function(x){
    y<-open.rdata(x)
    networks.strenght(y,  binwidth=.1, thousands="0", xlims=c(40,1.2,40), ylims=c(300,1500,300))
  })
  
  # See if AUS46 belong to the connected component in 2000
    # net.2000.va<-open.rdata(n.valueadded.files[1])
    # net.2000.va<-net.2000.va$Network
    # subcomponent(net.2000.va,"AUS47", mode ="all")
  graph.clustering(n.valueadded.files)
  clustering.graph<-ggplot(data=read.csv("../outputs/clustering/No/value added.csv"), aes(x=Year, y=value, group=variable, color=variable)) + geom_line() + geom_point()+ylab("Clustering coefficient")+ guides(fill=guide_legend(title=NULL))+scale_x_continuous(minor_breaks = seq(2000 , 2014, 1), breaks = seq(2000 , 2014, 5))
  ggsave(paste0("../outputs/clustering/clustering.","value added", ".png", sep=""), plot = clustering.graph, device = "png",
         scale = 1, width = 10, height = 5, units = "cm",
         dpi = 300, limitsize = TRUE) 
  
  
  com.valueadded.files<-list.files(path="../outputs/communities/cluster_fast_greedy/valueadded", full.names=TRUE)  
  lapply( com.valueadded.files,function(x){
    print(x)
    name<-substr(x, nchar(x)-20+1, nchar(x)-6)
    print(name)
    y<-open.rdata(x)
    w<-membership(y)
    w<-as.list(w)
    w<-as.data.frame(w)
    w<-t(w)
    w<-as.data.frame(w)
    w$id<-rownames(w)
    w$Country<-substr(w$id, 1, 3)
    w$Sector<-substr(w$id, 4, nchar(w$id))
    w$Sector<-ifelse(nchar(w$Sector)==1, paste0("0",w$Sector,sep=""),w$Sector)
    w$V1<-ifelse(nchar(w$V1)==1, paste0("0",w$V1,sep=""),w$V1)
    w$V1<-as.factor(w$V1)
    w<-w[order(w$Country, decreasing = TRUE),]
    w$Community<-w$V1
    plot<-ggplot(w, aes(Sector, Country)) + geom_tile(aes(fill = Community), colour = "white") + scale_fill_manual(values= rainbow(length(unique(w$V1))))
    ggsave(paste0("../outputs/communities/cluster_fast_greedy/",name, ".png", sep=""), plot = plot, device = "png",
           scale = 1.9, width = 16, height = 8, units = "cm",
           dpi = 300, limitsize = TRUE) 
  })
  
  graph.assortativity("../outputs/networks/valueadded","valueadded")
#####
# Descriptive statistics
#####
  wiot.files<-list.files(path="../data/wiot", full.names=TRUE)
  
  descriptives<-lapply(wiot.files[c(1,11,15)],function(x){
    y<-open.rdata(x)
    basics.fr20(y)
  })
  descriptives<-as.data.frame(descriptives)
  descriptives<-descriptives[,c(-3,-5)]
  xtable(descriptives)
  
  rankingci.df<-lapply(wiot.files[c(1,11,15)],function(x){
    y<-open.rdata(x)
    rankingci(y)
  })
  rankingci.df<-as.data.frame(rankingci.df)
  rownames(rankingci.df)<-1:nrow(rankingci.df)
  rankingci.xtable<-xtable(rankingci.df)
  print(rankingci.xtable, include.rownames = FALSE, booktabs = TRUE)
  
  
  
  rankingint.df<-lapply(wiot.files[c(1,11,15)],function(x){
    y<-open.rdata(x)
    rankingint(y)
  })
  rankingint.df<-as.data.frame(rankingint.df)
  #rownames(rankingint.df)<-1:nrow(rankingint.df)
  rankingint.xtable<-xtable(rankingint.df)
  print(rankingint.xtable, include.rownames = FALSE, booktabs = TRUE)
  
  