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

\name{Path functions}
\alias{build.paths}
\alias{load.paths}
\alias{build.and.load.paths}
\alias{intercept.paths}

\title{
Functions for managing shortest paths
}
\description{
Build shortest paths from an assignment set and load demands on the paths
}
\usage{
build.paths(aset,costs)
build.and.load.paths(aset,costs)
load.paths(aset,paths)
}
\arguments{
  \item{aset}{
	An \code{\link{assignment.set}}
}
  \item{costs}{
	A vector or matrix of costs for the network.  Unused links may be zero or NA.
}
  \item{paths}{
	A \code{path.set} build by \code{\link{build.paths}}
}
}
\details{

These functions are used to support link-based highway assignment algorithms, as well as operations on path sets
resulting from highway assignment.  \code{build.paths} makes a path set given a set of costs. The first application of
\code{build.paths} is usually performed using zero volumes, corresponding to a free-flow condition.  See \code{\link{free.flow}}.

With the resulting paths, the \code{load.paths} function will place the demand matrix onto the network links according
to the shortest paths.  This corresponds to an all-or-nothing assignment.  It differs from the \dQuote{\acronym{AON}}
highway assignment method in that it does not diagnostic checking and has a simpler set of parameters.

\code{build.and.load.paths} performs a build.paths operation followed directly by the equivalent of load.paths.

The result of both \code{load.paths} and \code{build.and.load.paths} is a \code{data.frame} with one numeric column of
link volumes per assignment class.  The \code{names} of the resulting \code{data.frame} of volumes correspond to the
names of the assignment classes.

In general, particularly when testing new code, it is preferable to build all-or-nothing volumes by using
\code{highway.assign( aset, method="AON" )} rather than calling \code{build.and.load.paths} directly, since more
error-checking will occur.

The costs for path building are a link vector or a list or data.frame with one column per assignment class, and
one row for each link in the network.  See \code{\link{assignment.set}} for details.  Ordinarily the costs are
constructed by passing the link volumes to the cost.function stored within the assignment set.
}
\value{
\code{build.paths} constructs a path set, using a vector of costs that should have been built by the
\code{cost.function} associated with the \code{assignment.set}.
\code{load.paths} constructs a \code{data.frame} with one numeric column of link volumes per assignment class
\code{build.and.load.paths} returns a list consisting of the loaded volumes (the same as \code{load.paths}) and the
corresponding path set as would have been returned from \code{build.paths}
}
\author{
Jeremy Raw
}
\seealso{
	\code{\link{highway.assign}} for general equilibrium assignment methods for highway networks.

	\code{\link{assignment.set}} for information about how to assemble assignment sets.

	\code{\link{free.flow}} for free-flow volumes.
}
