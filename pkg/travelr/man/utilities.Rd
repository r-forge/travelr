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

\name{travelr.utilities}
\alias{rmse}
\alias{hwy.gamma.function}
\alias{gamma.function}
\title{
Travel Modeling Utility Functions
}
\description{
Functions for Travel Model Development including Root Mean Square Error and a \dQuote{gamma} function for trip distribution models.
}
\usage{
rmse(v1, v2)
hwy.gamma.function(x,b,c)
}
\arguments{
  \item{v1,v2}{
  Vectors to be compared
}
  \item{x}{
  Impedance values to which gamma function will be applied
}
  \item{b,c}{
  Gamma function parameters
}
}
\details{
\code{rmse} computes Root Mean Square Error between two vectors using the formula commonly applied in travel demand modeling.
\code{hwy.gamma.function} computes friction factors from a vector or matrix of impedance values using the gamma function formula from NCHRP 365
and a scaling factor of \code{1}.
}
\value{
\code{rmse} returns a numeric scalar value.
\code{hwy.gamma.function} returns a numeric object the same shape as \code{x}
}
\references{
Martin, W.A., and McGuckin, N.A., 1998, \emph{NCHRP Report 365: Travel Estimation Techniques for Urban Planning},
National Cooperative Highway Research Program / Transportation Research Board
}
\seealso{
	\code{example(\link{skim.paths})} for use of the gamma function.
}
\author{
Jeremy Raw
}

\examples{
v1<-rnorm(100,mean=100,sd=2)
v2<-rnorm(100,mean=100,sd=2)
rmse(v1,v2)

# see example(skim.paths) for use of gamma function
}
