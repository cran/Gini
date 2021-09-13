# x: population of each region
# y: resource of each region
# group: grouping 'y' according to the 'x', usually 5 to 10
gini.2007=function(x,y,group=5){
  if(group>length(x)){
    message("'group' must be samller than the length of 'x'!")
  }else{
    index=order(x)
    x=x[index]
    y=y[index]
    X=cumsum(x/sum(x))
    W=NULL
    for(i in 1:(group-1)){
      W[i]=sum(y[1:(which(X>i/group)[1])])/sum(y)
    }
    G=1-(2*sum(W)+1)/group
    return(G)
  }
}
