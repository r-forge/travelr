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

\name{free.flow}
\alias{free.flow}

\title{
Free-flow Volumes
}
\description{
Build free-flow volumes from an assignment set
}
\usage{
free.flow(aset)
}
\arguments{
  \item{aset}{
	An \code{\link{assignment.set}}
}
}
\details{

\code{free.flow} produces a suitably structured set of numeric zero values for use in building costs;
the result is a set of link costs by assignment class at free flow conditions.

If an assignment set is constructed using \code{\link{new.assignment.set}}, the free-flow volumes will have been
computed by this function and will already be stored in the \code{ff.vol} element of the \code{\link{assignment.set}}.

}
\value{
\code{free.flow} applies the assignment set cost function to a vector or matrix of zeroes and returns the resulting link costs
}
\author{
Jeremy Raw
}
\seealso{
	\code{\link{assignment.set}}
}
