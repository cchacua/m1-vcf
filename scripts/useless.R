
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