wincor <-
  function(x,y=NULL,tr=.2){
    #   Compute the Winsorized correlation between x and y.
    #
    #   tr is the amount of Winsorization
    #   This function also returns the Winsorized covariance
    #
    #    Pairwise deletion of missing values is performed.
    #
    if(is.null(y[1])){
      y=x[,2]
      x=x[,1]
    }
    sig<-NA
    if(length(x)!=length(y))stop("Lengths of vectors are not equal")
    m1=cbind(x,y)
    #m1<-elimna(m1)
    nval=nrow(m1)
    x<-m1[,1]
    y<-m1[,2]
    g<-floor(tr*length(x))
    xvec<-winval(x,tr)
    yvec<-winval(y,tr)
    wcor<-cor(xvec,yvec)
    wcov<-var(xvec,yvec)
    if(sum(x==y)!=length(x)){
      test<-wcor*sqrt((length(x)-2)/(1.-wcor^2))
      sig<-2*(1-pt(abs(test),length(x)-2*g-2))
    }
    list(cor=wcor,cov=wcov,p.value=sig,n=nval)
  }

winval<-function(x,tr=.2){
  #
  #  Winsorize the data in the vector x.
  #  tr is the amount of Winsorization which defaults to .2.
  #
  #  This function is used by several other functions that come with this book.
  #
  y<-sort(x)
  n<-length(x)
  ibot<-floor(tr*n)+1
  itop<-length(x)-ibot+1
  xbot<-y[ibot]
  xtop<-y[itop]
  winval<-ifelse(x<=xbot,xbot,x)
  winval<-ifelse(winval>=xtop,xtop,winval)
  winval
}