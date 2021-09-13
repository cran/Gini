\name{gini.1997}
\alias{gini.1997}
\title{Gini Coefficient}
\description{
To calculate the Gini coefficient according to the following literature:
Weiguo Shi. A simple method of calculating the Gini coefficient(in Chinese). Jiangsu Tongji, 1997, (2): 16-18.
}
\usage{
gini.1997(x,y)
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
  \code{\link{gini.1994}}
  \code{\link{gini}}
  \code{\link{gini.2000}}
  \code{\link{gini.2002}}
  \code{\link{gini.2007}}
  \code{\link{gini.1994b}}
}
\examples{
x=c(382.8,522.7,192.4,227.4,490.2,108.0,222.5,220.5,231.2,375.3,323.9,79.9,305.7,98.7,46.1,35.3)
y=c(2778,3333,1673,1708,2118,1077,1850,1557,2010,2587,2482,616,2010,936,633,582)
gini.1997(x,y)#0.1216807
}
