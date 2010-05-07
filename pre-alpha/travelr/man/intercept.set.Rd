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

\name{intercept.set}
\alias{intercept.set}
\alias{new.intercept.set}
\alias{select link}
\title{
Manage intercept lists for select link analysis on highway assignments
}
\description{
Keep track of select link and select zone information for highway assignments.
}
\usage{
new.intercept.set(links, filter.od = NULL)
}
\arguments{
  \item{links}{A logical vector with one entry per link in the network; links marked as \code{TRUE} are \dQuote{selected}}
  \item{filter.od}{A function that takes a demand matrix and returns the same matrix, except with zeroes in cells corresponding to origin/destination pairs that are \emph{NOT} of interest }
}
\details{
This function returns an \dfn{intercept set}, a list with two elements corrsponding to its parameters.  If the \code{filter.od} function is not supplied (the default), a
stand-in function is created that simply returns its parameter.  Otherwise, filter.od is defined as:

\preformatted{
filter.od(od)
}
and it returns its (numeric matrix) parameter \code{od}, but puts zeroes in any cell corresponding to an origin/destination pair that is not of interest, even if its path crosses one of the selected links.

Internally, this structure is used by \code{\link{build.intercepts}}.

Most users will set the highway assignment \code{control} parameter \code{intercept} to an intercept set to perform
select link analysis.  See \code{\link{highway.assign}} for details.
}
\value{
Returns a list suitable for use with the \code{\link{build.intercepts}} function.
}
\author{
Jeremy Raw
}
\seealso{
	\code{\link{build.intercepts}}, and \code{\link{highway.assign}} for obtaining the results of a select link analysis
}
\examples{
\dontrun{
# Select links 17 and 24
select.links <- ( network$Links$.LinkID %in% c(17,24) )

# but only if the trips originate in zones 1:3, and end up in zones 12:24
zone.pairs <- data.matrix(expand.grid(c(1:3,12:24)))
select.matrix <- matrix(0,nrow=network$numZones,ncol=network$numZones)
select.matrix[zone.pairs] <- 1
select.od <- function(od) { od * select.matrix }

# build an intercept set using these specifications
iset <- new.intercept.set( select.links, select.od )

# use in the highway assignment algorithm
assignment.results <- highway.assign( aset, method="Frank.Wolfe", control=list(intercept=iset) )

# or use directly with build.intercept.function (for example, if creating a new assignment algorithm)

#    First, create a service function (a do-nothing placeholder if iset is not set)
build.intercepts <- build.intercept.function(iset,aset)

#    Then find some paths (free flow in this case)
paths <- build.paths(aset,aset$ff.flow)

#    Then apply the function
intercept <- build.intercepts(paths)

# the intercept result contains a list of matrices (element "od") with the selected demand for each class, and
# a data.frame (element "volumes) of selected volumes.
}
}
