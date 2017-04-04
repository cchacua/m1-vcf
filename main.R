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


wiot.df<-as.data.frame(wiot[15])

wiot.net15.f<-networks(wiot.df,mode="flows")
wiot.net15.tc<-networks(wiot.df,mode="techcoef")

networks.degree(wiot.net15.f, 10)



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




# Network with only CI data  


  
# Network with in terms of technical coefficients

# Extract production of the transportation sector
production.fr20<-sapply(wiot, prod.fr20)
production.fr20<-as.data.frame(t(production.fr20))


# List of countries WIOT
length(unique(wiot.df$Country))
