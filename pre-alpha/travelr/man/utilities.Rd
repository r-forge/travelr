\name{travelr.utilities}
\alias{rmse}
\title{
Root Mean Square Error
}
\description{
Compute Root Mean Square Error between two vectors
}
\usage{
rmse(v1, v2)
}
\arguments{
  \item{v1,v2}{
  Vectors to be compared
}
}
\details{
Computes Root Mean Square Error between two vectors using the formula commonly applied in travel demand modeling.
}
\value{
Returns a numeric scalar value.
}
\author{
Jeremy Raw
}

\examples{
v1<-rnorm(100,mean=100,sd=2)
v2<-rnorm(100,mean=100,sd=2)
rmse(v1,v2)
}
