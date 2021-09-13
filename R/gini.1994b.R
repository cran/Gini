# x: population of each region
# y: resource of each region
gini.1994b=function(x,y){
  index=order(y/x)
  x=x[index]
  y=y[index]
  Y=cumsum(y)/sum(y)
  h=x/sum(x)
  S1=0.5*sum((c(0,Y[1:(length(Y)-1)])+Y)*h)
  G=(0.5-S1)/0.5
  return(G)
}
