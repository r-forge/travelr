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

\name{build.intercept.function}
\alias{build.intercepts}
\alias{build.intercept.function}
\alias{intercept list}
\title{
Perform 'Select Link' Analysis for Highway Network
}
\description{
Define an intercept function for select link analysis.
}
\usage{
build.intercept.function(iset,aset)
}
\arguments{
  \item{iset}{
  Returned from the \code{\link{intercept.set}} function, a list defining links and origin/destination pairs of interest
}
  \item{aset}{
  An \code{\link{assignment.set}} describing the demand and other characteristics used to build the path set
}
}
\details{
Model developers will find the function \code{\link{new.intercept.set}} more useful.

This helper function builds a function that is used internally by \code{\link{highway.assign}} to compute intercepts for
a certain path structure and demand matrix.  Typically, this function will only be of interest to developers of new highway
assignment algorithms.

}
\value{
Returns a function that builds an \dfn{intercept list} with two components: \code{od}, a list containing the selected
origin/destination matrix for each assignment class, and \code{volumes} which is a \code{data.frame} of volumes
for each assignment class (comparable to the results from \code{\link{load.paths}}).
}
\author{
Jeremy Raw
}
\seealso{
	\code{\link{new.intercept.set}}, \code{\link{build.paths}}
}
