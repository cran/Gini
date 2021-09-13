# x: population of each region
# y: resource of each region
gini.2002=function(x,y){
  index=order(y/x)
  x=x[index]
  y=y[index]
  W=y/sum(y)
  P=x/sum(x)
  Q=cumsum(y)/sum(y)
  G=1-sum(P*(2*Q-W))
  return(G)
}
