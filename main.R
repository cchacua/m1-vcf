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
wiot<-open.rdata("../outputs/wiot.RData")

# igraph
wiot.df<-as.data.frame(wiot[15])
wiot.df.cin<-cit.matrix(wiot.df)
cit.names<-colnames(wiot.df.cin)
cit.names<-unique(cit.names)
cit.names<-remRight(cit.names,2)
#rownames(wiot.df.cin)<-cit.names
colnames(wiot.df.cin)<-cit.names

cit.imatrix<-as.matrix(wiot.df.cin)
cit.net<-graph_from_adjacency_matrix(cit.imatrix, mode="directed",  weighted = TRUE, diag = TRUE)
# To get the number of nodes and edges
gorder(cit.net)
gsize(cit.net)

#Only the transportation sector network
cit.net.trans.list<-subcomponent(cit.net, "FRA20", mode ="all")
cit.net.trans<-induced_subgraph(cit.net, cit.net.trans.list)
# To get the number of nodes
print("Number of nodes")
print(gorder(cit.net.trans))
print("Number of Edges")
print(gsize(cit.net.trans))
  # # To verify there is only a connected component
  # cit.net.trans.clus<-components(cit.net.trans, mode ="weak")
  # cit.net.trans.clus.mem<-as.data.frame(cit.net.trans.clus[1])
  # summary(cit.net.trans.clus.mem[,1])
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
