# x: population of each region
# y: resource of each region
gini.1994=function(x,y){
  index=order(y/x)
  A=x[index]
  B=y[index]
  E=cumsum(B)
  S=sum(0.5*A*B+A*c(0,E[1:(length(E)-1)]))/(sum(A)*sum(B))
  G=2*(0.5-S)
  return(G)
}
