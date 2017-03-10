library('ndtv')
library('network')
library('reshape2')

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
    }
    
