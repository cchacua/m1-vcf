
wiot.2000 <- open.rdata(wiot.files[1])
wiot.2001 <- local(get(load(wiot.files[2])))
wiot.2002 <- local(get(load(wiot.files[3])))
wiot.2003 <- local(get(load(wiot.files[4])))
wiot.2004 <- local(get(load(wiot.files[5])))
wiot.2005 <- local(get(load(wiot.files[6])))
wiot.2006 <- local(get(load(wiot.files[7])))
wiot.2007 <- local(get(load(wiot.files[8])))
wiot.2008 <- local(get(load(wiot.files[9])))
wiot.2009 <- local(get(load(wiot.files[10])))
wiot.2010 <- local(get(load(wiot.files[11])))
wiot.2011 <- local(get(load(wiot.files[12])))
wiot.2012 <- local(get(load(wiot.files[13])))
wiot.2013 <- local(get(load(wiot.files[14])))
wiot.2014 <- local(get(load(wiot.files[15])))


summary(cit.mlist[,3])

?clique.census


cit.2000<-sapply(cit.2000, zerosmall)
cit.2000<-as.data.frame(cit.2000)
summary(cit.2000[,2])
cit.2000<-as.matrix(cit.2000)
cit.2000.net<-as.network(cit.2000)
tnet<-networkDynamic(network.list=list(cit.2000.net,cit.2000.net))


# Example
g<-rgraph(20,tprob=0.06) #Generate a sparse random graph
#Find weak components
cd<-component.dist(g,connected="weak")
cd$membership #Who's in what component?
cd$csize #What are the component sizes?
#Plot the size distribution
plot(1:length(cd$cdist),cd$cdist/sum(cd$cdist),ylim=c(0,1),type="h")
lgc<-component.largest(g,connected="weak") #Get largest component
gplot(g,vertex.col=2+lgc) #Plot g, with component membership
#Plot largest component itself
gplot(component.largest(g,connected="weak",result="graph"))
#Find strong components
cd<-component.dist(g,connected="strong")
cd$membership #Who's in what component?
cd$csize #What are the component sizes?
#Plot the size distribution
plot(1:length(cd$cdist),cd$cdist/sum(cd$cdist),ylim=c(0,1),type="h")
lgc<-component.largest(g,connected="strong") #Get largest component
gplot(g,vertex.col=2+lgc) #Plot g, with component membership
#Plot largest component itself
gplot(component.largest(g,connected="strong",result="graph"))

# With Network package

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
# cit.net<-network(cit.mlist[137984:2000,], vertex.attr=cit.nodes, matrix.type="edgelist", loops=T, multiple=F, ignore.eval = T)
# comp<-component.dist(cit.net,connected="weak")
# comp.df<-as.data.frame(comp$membership)
# summary(comp.df[,1])
#plot(cit.net, vertex.col=2+comp)



net<-wiot.net15.f

d.total<-degree(net, mode = "total")
d.out<-degree(net, mode = "out")
d.in<-degree(net, mode = "in")
d.total<-as.data.frame(d.total)
d.total$type<-"Total"
colnames(d.total)<-c("Degree","Type" )
d.out<-as.data.frame(d.out)
d.out$type<-"Out"
colnames(d.out)<-c("Degree","Type" )
d.in<-as.data.frame(d.in)
d.in$type<-"In"
colnames(d.in)<-c("Degree","Type" )
degree<-rbind(d.out, d.in, d.total)
#degree$Logdegree<-log(degree$Degree)
plot<-ggplot(degree, aes(x=Degree)) + geom_histogram(fill="#F8766D") + xlab("Degree")+
  ylab("Frequency")+facet_grid( . ~ Type, scales="free_x")+ggtitle("2014")

ggsave("myplot.png", plot = plot, device = "png",
       scale = 1, width = 16, height = 5, units = "cm",
       dpi = 300, limitsize = TRUE)




ggsave(paste0("../output/", "filename", ".png", sep=""), width = 16, height = 5, units = "cm", scale=1)


deg.dist.total<-degree_distribution(net, cumulative=T, mode="total")
plot(x=0:max(d.total$Degree), y=1-deg.dist.total, pch=19, cex=1.2, col="orange",
     xlab="Degree", ylab="Cumulative Frequency")



##########################################################################################

# Extract column of FRA20
fr20.col<-wiot.df.cin[,"FRA20"]
fr20.col<-as.data.frame(fr20.col)
rownames(fr20.col)<-cit.names
fr20.col[,1]<-ifelse(fr20.col[,1]==0, NA, fr20.col[,1])
fr20.col<-na.omit(fr20.col)
# The French transportation sector had relationships with 2.192 of 2.464 industries in the whole dataset



wiot.df<-as.data.frame(wiot[15])

wiot.net15.f<-networks(wiot.df,mode="flows")
#wiot.net15.tc<-networks(wiot.df,mode="techcoef")

networks.degree(wiot.net15.f, .1)



# Get neighbor edges
#?incident
fr20.neighbors.e<-incident(cit.net.trans, "FRA20", mode="in")
# Get neighbor nodes
fr20.neighbors.v<-neighbors(cit.net.trans, "FRA20", mode="in")




trans.clus <- cluster_walktrap(cit.net.trans)
membership(trans.clus)
sizes(trans.clus)
?walktrap.community






#cit.net.layout<-layout_with_lgl(cit.net, root= "FRA20")
?layout_with_lgl


plot(cit.net, edge.arrow.size=.4, edge.curved=.1)


# http://datastorm-open.github.io/visNetwork/igraph.html
visIgraph(cit.net)
cit.net.viz <- toVisNetworkData(cit.net)
visNetwork(nodes = cit.net.viz$nodes, edges = cit.net.viz$edges, height = "500px")



cit.net.clus<-components(cit.net, mode ="weak")
#clusters(cit.net)
cit.net.clus.mem<-as.data.frame(cit.net.clus[1])
summary(cit.net.clus.mem[,1])

components(graph, mode = c("weak", "strong"))
cit.net.dec<-decompose.graph(cit.net, min.vertices=2)
sapply(cit.net.dec, diameter)





# Extract production of the transportation sector
production.fr20<-sapply(wiot, prod.fr20)
production.fr20<-as.data.frame(t(production.fr20))


# List of countries WIOT
length(unique(wiot.df$Country))


##########################################################################################
# Special cases

#2014 LUX32 MLT4   
manip.negav(n.techcoef.files[15])

#2013: Luxembourg 35 Postal and courier activities and 32 Water transport
# TOP CI: BRA10 DNK10 Manufacture of coke and refined petroleum products MLT4 Mining and quarrying
y<-open.rdata(n.techcoef.files[14]) 
y.s<-strength(y$Network, mode = "in")
y.s<-as.data.frame(y.s)
y.df<-wiot[14]
y.df<-as.data.frame(y.df)
View(y.df[,c("IndustryCode", "IndustryDescription", "Country", "RNr", "LUX32", "LUX35")])
y.sum<-sum(y.df[1:2464,"LUX32"])
y.sum/y.df[2472,"LUX32"]
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

#2010
y<-open.rdata(n.techcoef.files[10])
y.s<-strength(y$Network, mode = "in")
y.s<-as.data.frame(y.s)
