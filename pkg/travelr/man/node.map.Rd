%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Copyright (C) 2010 by Jeremy Raw
%   This program is free software; you can redistribute it and/or modify
%   it under the terms of the GNU General Public License as published by
%   the Free Software Foundation; either version 2 of the License, or
%   (at your option) any later version.
% 
%   This program is distributed in the hope that it will be useful,
%   but WITHOUT ANY WARRANTY; without even the implied warranty of
%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%   GNU General Public License for more details.
% 
%   A copy of the GNU General Public License is available at
%   http://www.r-project.org/Licenses/
%   and included in the R distribution (in directory ‘share/licenses’).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{map.highway.nodes}
\alias{map.highway.nodes}
\alias{summary.highway.node.map}
\alias{print.highway.node.map}

\title{
Build network node sequence
}
\description{
Map network nodes to a continuous sequence
}
\usage{
map.highway.nodes(nodes, numZones )
\method{summary}{highway.node.map}(object,...)
\method{print}{highway.node.map}(x,...)
}
\arguments{
  \item{nodes}{
    An optional node specification, typically a \code{data.frame}, that can contain useful information such as geographical coordinates
}
  \item{numZones}{
    The number of zones (origins and destinations) in the network.  The zones must have sequential numbers from \code{1}
	to \code{numZones}
}
  \item{object}{
	The \code{highway.node.map} object to be summarized
}
  \item{x}{
	The \code{highway.node.map} object to be printed
}
  \item{\dots}{
	Additional arguments passed to generic methods
}
}
\details{
\code{map.highway.nodes} builds a \code{highway.node.map} structure that can be used to convert a sequence of
nodes with gaps to a compact sequence of integers with no missing values.  The structure has functions to support
mapping and unmapping nodes, as illustrated in the examples.

\code{map.highway.nodes} needs to know the number of zones (origins or destinations) in the system, since
zones are never renumbered:  they have to be continuous from \code{1} to however many zones there are, and
the input network will need to be edited if that is not the case.

Travel models are often coded to leave gaps in node number sequences in order to facilitate editing.  Such gaps
cause performance problems when they are left in place, and \code{map.highway.nodes} is a simple tool for converting
an integer sequence with gaps into a continuous sequence, and recovering the original sequence later from the
continuous sequence.

\strong{Note:}The standard R factor utility can also be used to construct a suitable node mapping, either alone or in
conjunction with \code{map.highway.nodes}.  The sequence with gaps can be converted to a factor, and the resulting
factor codes will serve as a suitable sequence.  Either approach can be used successfully, provided one carefully
handles zone numbering.  In particular, if the maximum zone number in the original network is, say, \code{250}, but
there is a range of numbers from \code{176} to \code{200} that are not used, then the correct value for the network
\code{numZones} parameter after applying the \R function \code{factor} is 225.  \code{map.highway.nodes} will catch this
situation and force the user to address it explicitly, saving much grief later on.

}
\value{
Returns a \code{highway.node.map} object, with two functions, \code{map} and \code{unmap} that can be used to
map and unmap nodes
}
\author{
Jeremy Raw
}
\seealso{
	\code{\link{as.highway.net}}
}
\examples{
numZones <- 5
bad.nodes <- c(1:numZones,round(runif(15,min=6,max=100)))
nodeMap <- map.highway.nodes(bad.nodes,numZones)
good.nodes <- nodeMap$map(bad.nodes)
bad.nodes.2 <- nodeMap$unmap(good.nodes)
results <- data.frame(Original=bad.nodes,Mapped=good.nodes,Unmapped=bad.nodes.2)
all( results$Original==results$Unmapped )
}
