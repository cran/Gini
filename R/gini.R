# x: population of each region
# y: resource of each region
gini=function(x,y){
  index=order(y/x)
  x=x[index]
  y=y[index]
  X=cumsum(x)/sum(x)
  Y=cumsum(y)/sum(y)
  G=1-sum((X-c(0,X[1:(length(X)-1)]))*(Y+c(0,Y[1:(length(Y)-1)])))
  return(G)
}
