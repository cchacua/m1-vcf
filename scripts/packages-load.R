library('ndtv')
library('network')

# Custom Functions

open.rdata<-function(x){local(get(load(x)))}

dividelast<-function(column){
  #column<-wiot.df.ci[,1]
  vector<-column[1:length(column)-1]
  scalar<-column[length(column)]
  output<-vector/scalar
  output<-as.data.frame(output)
}