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

\name{cost.functions}
\alias{cost function}
\alias{cost.function}
\alias{cost.functions}
\alias{routing cost}

\title{
Construct cost functions
}
\description{
Cost functions (Volume/Delay) are used to establish equlibribum conditions for highway assignment.
}
\details{
Cost functions are called from within the assignment algorithms (\code{\link{highway.assign}} like this:

\preformatted{cost.function(volume,aset)}

The parameters have the following definitions:
\describe{
\item{volume}{volumes for which costs are to be computed; see below}
\item{aset}{An \code{\link{assignment.set}} describing the classes in the assignment}
}

The function returns a \code{data.frame} with one column per assignment class, each of which contains the
numeric routing cost for the network links for vehicles in that class.  (The \dfn{routing cost} is the network
link value that will be minimized when building shortest paths for assigning vehicles of the class).

The assignment set is passed to the function so that different assignment classes in the set may have their costs
evaluated separately.  That can be useful if, for example, certain assignment classes have routing costs that are
evaluated differently than other classes.  Passenger vehicles, for example, may be considerably more sensitive to the
presence of other trucks.

If the assignment set is defined with a \code{\link{cost.volume.type}} of \dQuote{vector}, the \code{volume} passed to
the cost function will be a vector corresponding to the sum of all the class volumes on each link.  Otherwise, if the
\code{\link{cost.volume.type}} is \dQuote{matrix}, a \code{data.frame} will be passed with the link volume for each class
in its own column.

Helper functions are available to construct cost (and objective) functions in the classic form known as
\acronym{\link{BPR}} (Bureau of Public Roads).

}
\value{
\code{cost.function} returns a numeric vector or data.frame of the same shape as \code{volume}
}
\author{
Jeremy Raw
}
\seealso{
	\code{\link{assignment.set}} for a description of how the cost function is used.
	\code{\link{bpr.functions}} for BPR helper functions
	\url{http://emme2.spiess.ch/conic/conic.html} for a technical discussion of some alternative cost functions
}
