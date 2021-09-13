\name{gini.1994}
\alias{gini.1994}
\title{Gini Coefficient}
\description{
To calculate the Gini coefficient according to the following literature:
Hansheng Ding, Shanlian Hu. A study on the equity of distribution of health resources in China(in Chinese). Zhongguo Wiesheng Shiye Guanli, 1994, (2): 105-107.
}
\usage{
gini.1994(x,y)
}
\arguments{
  \item{x}{vector, population of each region}
  \item{y}{vector, health resource of each region}
}
\value{
  \item{G}{Gini coefficient. Critria: <0.3=best fairness, 0.3-0.4=relative fairness, >0.4=unfairness, >0.6=highly unfair}
}
\note{
Please feel free to contact us, if you have any advice and find any bug!

Update:

Version 0.1.0: The first version.
}
\seealso{
  \code{\link{gini}}
  \code{\link{gini.1997}}
  \code{\link{gini.2000}}
  \code{\link{gini.2002}}
  \code{\link{gini.2007}}
  \code{\link{gini.1994b}}
}
\examples{
x=c(382.8,522.7,192.4,227.4,490.2,108.0,222.5,220.5,231.2,375.3,323.9,79.9,305.7,98.7,46.1,35.3)
y=c(2778,3333,1673,1708,2118,1077,1850,1557,2010,2587,2482,616,2010,936,633,582)
gini.1994(x,y)#0.1216807
}
