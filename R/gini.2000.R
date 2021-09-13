# x: population of each region
# y: resource of each region
gini.2000=function(x,y){
  index=order(y/x)
  w=x/sum(x)
  y=y/sum(y)
  w=w[index]
  y=y[index]
  v=cumsum(y)
  G=sum(w*y)+2*sum(w*(1-v))-1
  return(G)
}
