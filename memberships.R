
comm.list<-as.data.frame(x=list(community1$names, community1$membership), col.names = c("country", "community"))
comm.list.comnumber<-comm.list[comm.list$country=="FRA20",2]
comm.list.set1<-comm.list[comm.list$community==comm.list.comnumber,]
comm.list.setm1<-comm.list[comm.list$community==comm.list.comnumber-1,]
comm.list.setp1<-comm.list[comm.list$community==comm.list.comnumber+1,]
