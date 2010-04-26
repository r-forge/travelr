\name{tntp}
\alias{tntp}
\alias{tntp.network}
\alias{tntp.od}
\title{
Load TNTP-format networks
}
\description{
Load publicly available sample networks in TNTP format
}
\usage{
tntp.network(linkfile)
tntp.od(odfile)
}
\arguments{
  \item{linkfile}{
  File containing network links
}
  \item{odfile}{
  File containing origin/destination flow information
}
}
\details{
Use these functions to import sample networks in a standard format.

Such networks can be found here: \url{http://www.bgu.ac.il/~bargera/tntp}

No function is provided for reading node information that is sometimes supplied with these networks, since those
files are generally amenable to input through one of the standard \code{\link{read.table}} variants.
}
\value{
\code{tntp.network}returns a \code{\link{highway.net}} structure suitable for use with travelr functions.

\code{tntp.od} returns a matrix of origin/destination flows
}
\author{
Jeremy Raw
}
