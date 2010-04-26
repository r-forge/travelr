\name{sample.networks}
\alias{travelr.data}
\alias{sample.networks}
\alias{Richmond.VA.net}
\alias{Richmond.VA.od}
\alias{SiouxFalls.net}
\alias{SiouxFalls.od}

\title{
Sample networks for highway assignment testing.
}
\description{
Data files to support testing highway assignments.
}
\usage{
data("Richmond.VA")
data("SiouxFalls")
}
\details{
Two pre-built sample networks are available with \pkg{travelr}: the smaller \dQuote{Sioux Falls} network and the much
larger \dQuote{Richmond, VA} network which illustrates how to use static turn penalties and alternate like attribute names.

These sample networks load two elements through the \code{data} interface: a built highway network (including turn
penalties where available), and a demand matrix for all vehicles.
\describe{
\item{\var{network}.od}{demand matrix}
\item{\var{network}.net}{network}
}
The Sioux Falls network comes from public sources (specifically, Hillel Bar-Gera's site containing sample
networks for testing assignment algorithms at \url{http://www.bgu.ac.il/~bargera/tntp}).  The Sioux Falls
network was adjusted slightly so that zone paths are more likely to have unique travel times, in order to
facilitate testing against other traffic assignment implementations.

Other networks from \url{http://www.bgu.ac.il/~bargera/tntp} can be loaded into travelr format using
function \code{\link{tntp.network}}. Trip tables can be loaded from the same source using \code{\link{tntp.od}}.
}
\seealso{
	\code{\link{tntp.network}}, \code{\link{tntp.od}}
}
