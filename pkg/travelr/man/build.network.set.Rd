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
%   and included in the R distribution (in directory 'share/licenses').
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{build.network.set}
\alias{build.network.set}
\alias{.build.network.set}
\alias{network.set}
\title{
Construct internal structures for shortest path evaluation
}
\description{
Build suitable internal structures for computing shortest paths on a highway network.
}
\usage{
.build.network.set(network, link.subset=NULL, pen.subset=NULL)
}
\arguments{
  \item{network}{
	A highway network
}
  \item{link.subset}{
	A vector index indicating which network links are active for this class (default: \emph{all} links)
}
  \item{pen.subset}{
	A vector index indicating which penalty values are active for this class (default: \emph{no} penalties})
}

\details{
This function is usually only called from within \code{\link{new.assignment.set}}.  It sets up
an internal representation of a highway network from which a path tree can be rapidly built.  It
is provided publicly, since it may be useful for building a function to rapidly load and format
an external network in some new file format.

The \code{link.subset} and \code{pen.subset} are standard indexes suitable for selecting a subset
from a vector whose length is given by \code{network$numLinks}, either logical or integer.  In
particular, setting \code{link.subset} (or \code{pen.subset}) to \code{TRUE} will select all links
(or penalties).

If \code{link.subset} is not provided, \emph{all} links will be selected.

If \code{pen.subset} is not provided, \emph{no} penalties will be selected.
}
\value{
Returns a version of the network suitable for use in building shortest paths efficiently.
}
\author{
Jeremy Raw
}
\seealso{
	\code{\link{shortest.paths}}
}
