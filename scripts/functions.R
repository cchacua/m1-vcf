# Custom Functions

# Substract right
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# Remove last n characters from a string    
remRight <- function(x, n){
  substr(x, 1, nchar(x)-n)
}

# Open Rdata as dataframe
open.rdata<-function(x){local(get(load(x)))}

# Extract production of each year for the transportation sector
prod.fr20<-function(df){
  wiot.df<-as.data.frame(df)
  value<-wiot.df[2472, c("FRA20", "Year")]
}

# Capitalize first letter
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

# Divide a column by its last value  
dividelast<-function(column){
  #column<-wiot.df.ci[,1]
  vector<-column[1:length(column)-1]
  scalar<-column[length(column)]
  ifelse(scalar==0, .<-vector/1, .<-vector/scalar)
  .<-as.data.frame(.)
}

# Zero to values less than 0.000001
zerosmall<-function(column){
  ifelse(column<0.0000001, .<-0, .<-column)
  .<-as.data.frame(.)
  .<-.*100
}

# Divide a column by its last value and put zero to values less than 0.000001
dividelastsmall<-function(column){
  #column<-wiot.df.ci[,1]
  vector<-column[1:length(column)-1]
  scalar<-column[length(column)]
  ifelse(scalar==0, .<-vector/1, .<-vector/scalar)
  ifelse(.<0.0000001, .<-0, .<-.)
  .<-as.data.frame(.)
  .<-.*100
}
# CI matrix only
ci.matrix<-function(df){
  wiot.df<-as.data.frame(df)
  #wiot.colnames<-as.data.frame(colnames(wiot.df))
  # CI data goes until line 2464= 44 countries * 56 sectors
  wiot.df.ci<-wiot.df[1:2464,6:2469]
  # Columns 6:2469 have the CI data
  # Columns 57-61 for each country goes from colums 2470:2689
  # CONS_h,	CONS_np,	CONS_g,	GFCF,	INVEN,	EXP	GO
  # Final consumption expenditure by households	Final consumption expenditure by non-profit organisations serving households (NPISH)	Final consumption expenditure by government	Gross fixed capital formation	Changes in inventories and valuables	Exports	Total output
  # Colum 2690 has the total production, which is the same value as in line 2472 (GO)
  # View(wiot.df[1,2690])
  # View(wiot.df[2472,6])
}

# CI matrix of technical coefficients
cit.matrix<-function(df){
  wiot.df<-as.data.frame(df)
  # CI data goes until line 2464= 44 countries * 56 sectors
  # Line 2472 (GO) has the Output at basic prices
  wiot.df.ci<-wiot.df[c(1:2464, 2472),6:2469]
  wiot.df.cin<-sapply(wiot.df.ci, dividelast)
  wiot.df.cin<-as.data.frame(wiot.df.cin)
  
  cit.names<-colnames(wiot.df.cin)
  cit.names<-unique(cit.names)
  cit.names<-remRight(cit.names,2)
  #rownames(df.cin)<-cit.names
  colnames(wiot.df.cin)<-cit.names
  wiot.df.cin
}

# Leontieff matrix
# http://qua.st/decompr/leontief-interpretation/
leontief.matrix<-function(df){
  df<-as.data.frame(df)
  # CI data goes until line 2464= 44 countries * 56 sectors
  # Line 2472 (GO) has the Output at basic prices
  
  #Technical coefficients matrix
  ci<-df[c(1:2464, 2472),6:2469]
  cin<-sapply(ci, dividelast)
  cin<-as.data.frame(cin)
  cit.names<-colnames(cin)
  cit.names<-unique(cit.names)
  cit.names<-remRight(cit.names,2)
  #rownames(df.cin)<-cit.names
  colnames(cin)<-cit.names
  
  # I-A
  ia<-diag(nrow(cin))-cin
  L<-solve(ia)
}

# Value-added contribution matrix
valueadded.matrix<-function(df){
  df<-as.data.frame(df)
  # CI data goes until line 2464= 44 countries * 56 sectors
  # Line 2472 (GO) has the Output at basic prices
  
  #Technical coefficients matrix
  cin<-df[c(1:2464, 2472),6:2469]
  cin<-sapply(cin, dividelast)
  cin<-as.data.frame(cin)
  cit.names<-colnames(cin)
  cit.names<-unique(cit.names)
  cit.names<-remRight(cit.names,2)
  #rownames(df.cin)<-cit.names
  colnames(cin)<-cit.names
  # I-A
  cin<-diag(nrow(cin))-cin
  cin<-solve(cin)
  
  va.df<-df[c(2470, 2472),6:2469]
  va.df<-as.data.frame(t(va.df))
  va.df$valuead<-ifelse(va.df[,2]==0, va.df[,1]/1, va.df[,1]/va.df[,2])
  #va.df$valuead<-va.df[,1]/va.df[,2]
  va.matrix<-diag(va.df$valuead)
  
  valcontrib<-va.matrix %*% cin
  valcontrib<-as.data.frame(valcontrib)
  colnames(valcontrib)<-cit.names
  valcontrib
}

# Create networks
networks<-function(df, mode="flows", sector="FRA20", subcomponent=TRUE){
  
  # Modes: "flows", "techcoef", "valueadded"
  df<-as.data.frame(df)
  
  if(mode=="flows"){
    df.cin<-ci.matrix(df)
  }
  else if(mode=="techcoef"){
    df.cin<-cit.matrix(df)
  }
  else if(mode=="valueadded"){
    df.cin<-valueadded.matrix(df)
  }
  else {print("Enter a valid mode: flows, techcoef, valueadded")}
  
  # Matrix and graph
  cit.imatrix<-as.matrix(df.cin)
  cit.net<-graph_from_adjacency_matrix(cit.imatrix, mode="directed",  weighted = TRUE, diag = TRUE)
  cit.net<-delete_vertices(cit.net, c("LUX32", "LUX35", "MLT4", "LVA33"))
  
  # Extract only the subcomponent where "sector" is located
  if(subcomponent==TRUE){      
    cit.net.trans.list<-subcomponent(cit.net,   sector, mode ="all")
    cit.net.trans<-induced_subgraph(cit.net, cit.net.trans.list)
    # # To verify there is only a connected component
    # cit.net.trans.clus<-components(cit.net.trans, mode ="weak")
    # cit.net.trans.clus.mem<-as.data.frame(cit.net.trans.clus[1])
    # summary(cit.net.trans.clus.mem[,1])
    # To get the number of nodes
    print(paste("Year = ", df[2,5]))
    print(paste("Number of nodes = ", gorder(cit.net.trans)))
    print(paste("Number of Edges = ",gsize(cit.net.trans)))
    x<-list(Year=df[2,5], Network=cit.net.trans)
    save(x, file=paste0("../outputs/networks/",mode,"/",df[2,5],"_",mode,".RData"))
    
  }
  else{
    print(paste("Year = ", df[2,5]))
    print(paste("Number of nodes = ", gorder(cit.net)))
    print(paste("Number of Edges = ",gsize(cit.net)))
    x<-list(Year=df[2,5], Network=cit.net)
    save(x, file=paste0("../outputs/networks/",mode,"/",df[2,5],"_",mode,".RData"))
  }
 
}

# Graph component distribution
comp.distribution<-function(df, mode="flows", binwidth=20){
  
  # Modes: "flows", "techcoef", "value"
  df<-as.data.frame(df)
  
  if(mode=="flows"){
    df.cin<-ci.matrix(df)
  }
  else if(mode=="techcoef"){
    df.cin<-cit.matrix(df)
  }
  else {print("Enter a valid mode: flows, techcoef, value")}
  
  # Matrix and graph
  cit.imatrix<-as.matrix(df.cin)
  cit.net<-graph_from_adjacency_matrix(cit.imatrix, mode="directed",  weighted = TRUE, diag = TRUE)
  
  cit.net.clus<-components(cit.net, mode ="weak")
  cit.net.clus.mem<-as.data.frame(cit.net.clus[1])
  cit.net.clus.mem$country<-rownames(cit.net.clus.mem)
  cit.net.clus.mem$membership<-ifelse(cit.net.clus.mem$membership==1, NA, cit.net.clus.mem$membership)
  #colnames(cit.net.clus.mem)<-c("Size", "Number")
  dist<-ggplot(cit.net.clus.mem, aes(x=membership)) + geom_histogram(fill="#00B0F6", binwidth=binwidth) + xlab("")+ ylab(NULL)+ggtitle("Total")
  dist
}

# Degree as df
  degree.as.df<-function(network, mode, thousands=TRUE){
    d<-degree(network, mode = mode)
    d<-as.data.frame(d)
    d$type<-firstup(mode)
    colnames(d)<-c("Degree","Type" )
    if(thousands==TRUE){
      d$Degree<-d$Degree/1000
      d
    }
    else{d}
  }

  
# Create networks and degree distribution graphs
networks.degree<-function(datalist, binwidth=20, mode="ind"){
  # Modes: grid and ind
  # Types: d, cd, s, cs
  year<-datalist$Year
  cit.net<-datalist$Network
    d.total<-degree.as.df(cit.net, "total")
    d.in<-degree.as.df(cit.net, "in")
    d.out<-degree.as.df(cit.net, "out")
  
  if(mode=="grid"){  
    degree<-rbind(d.out, d.in, d.total)
    #degree$Logdegree<-log(degree$Degree)
    plot<-ggplot(degree, aes(x=Degree)) + geom_histogram(fill="#00B0F6", binwidth=binwidth) + xlab("Degree")+
      ylab("Frequency")+facet_grid( . ~ Type, scales="free", shrink=FALSE)+ggtitle(year)
    ggsave(paste0("../outputs/degree/", year, "_degree.png", sep=""), plot = plot, device = "png",
           scale = 1, width = 16, height = 5, units = "cm",
           dpi = 300, limitsize = TRUE)
    }
  else if(mode=="ind"){
    p.total<-ggplot(d.total, aes(x=Degree)) + geom_histogram(fill="#00B0F6", binwidth=binwidth) + xlab("")+ ylab(NULL)+ggtitle("Total")+ geom_point(aes(x=d.total["FRA20","Degree"], y=2, size = 1, colour = "#FEFEFE", shape =8))+ scale_shape_identity()+ theme(legend.position="none")
    p.in<-ggplot(d.in, aes(x=Degree)) + geom_histogram(fill="#00B0F6", binwidth=binwidth) + xlab("")+ ylab(paste0("Frequency - ", year))+ggtitle("In")+ geom_point(aes(x=d.in["FRA20","Degree"], y=2, size = 1, colour = "#FEFEFE", shape =8))+ scale_shape_identity()+ theme(legend.position="none")
    p.out<-ggplot(d.out, aes(x=Degree)) + geom_histogram(fill="#00B0F6", binwidth=binwidth) + xlab("Degree")+ ylab(NULL)+ggtitle("Out")+ geom_point(aes(x=d.out["FRA20","Degree"], y=2, size = 1, colour = "#FEFEFE", shape =8))+ scale_shape_identity()+ theme(legend.position="none")
    
    plot<-grid.arrange(p.in, p.out, p.total, ncol=3)
    
    ggsave(paste0("../outputs/degree/", year, "_degree.png", sep=""), plot = plot, device = "png",
           scale = 1, width = 16, height = 5, units = "cm",
           dpi = 300, limitsize = TRUE)
  }
  else{print("Select a correct mode: grid or ind")}
}

# Strength as df
strength.as.df<-function(network, mode, thousands=TRUE){
  d<-strength(network, mode = mode)
  d<-as.data.frame(d)
  d$type<-firstup(mode)
  colnames(d)<-c("Strength","Type" )
  if(thousands==TRUE){
    d$Strength<-d$Strength/1000
    d
  }
  else{d}
}

# Strenght distribution
networks.strenght<-function(datalist, binwidth=20, mode="ind", thousands=TRUE){
  # Modes: grid and ind
  # Types: d, cd, s, cs
  year<-datalist$Year
  cit.net<-datalist$Network
  d.total<-strength.as.df(cit.net, "total", thousands)
  d.in<-strength.as.df(cit.net, "in", thousands)
  d.out<-strength.as.df(cit.net, "out", thousands)
  
  if(mode=="grid"){  
    degree<-rbind(d.out, d.in, d.total)
    #degree$Logdegree<-log(degree$Strength)
    plot<-ggplot(degree, aes(x=Strength)) + geom_histogram(fill="#00BA38", binwidth=binwidth) + xlab("Strength")+
      ylab("Frequency")+facet_grid( . ~ Type, scales="free", shrink=FALSE)+ggtitle(year)
    ggsave(paste0("../outputs/strength/", year, "_strength.png", sep=""), plot = plot, device = "png",
           scale = 1, width = 16, height = 5, units = "cm",
           dpi = 300, limitsize = TRUE)
  }
  else if(mode=="ind"){
    p.total<-ggplot(d.total, aes(x=Strength)) + geom_histogram(fill="#00BA38", binwidth=binwidth) + xlab("")+ ylab(NULL)+ggtitle("Total")+ geom_point(aes(x=d.total["FRA20","Strength"], y=2, size = 1, colour = "#FEFEFE", shape =8))+ scale_shape_identity()+ theme(legend.position="none")
    p.in<-ggplot(d.in, aes(x=Strength)) + geom_histogram(fill="#00BA38", binwidth=binwidth) + xlab("")+ ylab(paste0("Frequency - ", year))+ggtitle("In")+ geom_point(aes(x=d.in["FRA20","Strength"], y=2, size = 1, colour = "#FEFEFE", shape =8))+ scale_shape_identity()+ theme(legend.position="none")
    p.out<-ggplot(d.out, aes(x=Strength)) + geom_histogram(fill="#00BA38", binwidth=binwidth) + xlab("Strength")+ ylab(NULL)+ggtitle("Out")+ geom_point(aes(x=d.out["FRA20","Strength"], y=2, size = 1, colour = "#FEFEFE", shape =8))+ scale_shape_identity()+ theme(legend.position="none")
    
    plot<-grid.arrange(p.in, p.out, p.total, ncol=3)
    
    ggsave(paste0("../outputs/strength/", year, "_Strength.png", sep=""), plot = plot, device = "png",
           scale = 1, width = 16, height = 5, units = "cm",
           dpi = 300, limitsize = TRUE)
  }
  else{print("Select a correct mode: grid or ind")}
}

# Sectors with negative added-values
  manip.negav<-function(numberlist=n.techcoef.files[15]){
    print(numberlist)  
    y<-open.rdata(numberlist) 
    y.s<-strength(y$Network, mode = "in")
    y.s<-as.data.frame(y.s)
    y.s$country<-rownames(y.s)
    print(y.s[y.s$y.s>=1,])
  }
  
# Graph a measure for all years, taking a network file
  graphmeasure.years<-function(list, measure="transitivity.w"){
    sizes<-lapply(list,function(x){
      y<-open.rdata(x)
      year<-y$Year
      net<-y$Network
      
      if(measure=="transitivity.w"){
        Measure.out<-transitivity(net, type="weighted")
        legend<-"Average weighted clustering coefficient"}
      else if(measure=="transitivity.g"){
        Measure.out<-transitivity(net, type="global")
        legend<-"Global clustering coefficient"}
      w<-c(Year=year,Measure=Measure.out, Legend=legend)
    })
    sizes<-t(as.data.frame(sizes))
    sizes<-as.data.frame(sizes)
    rownames(sizes)<-sizes$Year
    Legend.t<-sizes$Legend[1]
    measure.plot<-ggplot(sizes, aes(Year, Measure, colour="#7CAE00")) + geom_line(size=1)+ geom_point(size=2)+xlab("Year") + ylab(Legend.t)+ theme(legend.position="none")
    ggsave(paste0("../outputs/",measure, ".png", sep=""), plot = measure.plot, device = "png",
           scale = 1, width = 16, height = 5, units = "cm",
           dpi = 300, limitsize = TRUE) 
    
  }

  graph.clustering<-function(list, name="network"){
    clustering<-lapply(list,function(x){
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
    clustering<-as.data.frame(clustering)
    clustering<-t(clustering)
    clustering<-as.data.frame(clustering)
    #rownames(clustering)<-clustering$Year
    clustering<- melt(clustering, id="Year")
    write.csv(clustering, paste0("../outputs/clustering_",name,".csv", sep=""))
    
    clustering.graph<-ggplot(data=clustering, aes(x=Year, y=value, group=variable, color=variable)) + geom_line() + geom_point()+ylab("Clustering coefficient")+ guides(fill=guide_legend(title=NULL))
    
    ggsave(paste0("../outputs/",name, ".png", sep=""), plot = clustering.graph, device = "png",
           scale = 1, width = 10, height = 5, units = "cm",
           dpi = 300, limitsize = TRUE) 
  }    
  