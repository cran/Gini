# x: population of each region
# y: resource of each region
gini.1997=function(x,y){
  index=order(y/x)
  x=x[index]
  y=y[index]
  X=cumsum(x)/sum(x)
  Y=cumsum(y)/sum(y)
  G=sum(X[1:(length(X)-1)]*Y[2:length(Y)]-X[2:length(X)]*Y[1:(length(Y)-1)])
  return(G)
}
