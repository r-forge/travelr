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

\name{package-travelr}
\alias{package-travelr}
\alias{travelr}

\title{
TravelR: Tools for writing travel demand models
}
\description{
TravelR provides some common, specialized tools for travel demand modeling in R.
}
\details{

The \pkg{travelr} package provides tools for travel demand modeling that are not otherwise available in \R.

In particular, the package directly supports the following operations:
\itemize{
\item{Iterative proportional fitting (dQuote{Fratar} expansion) of matrices to new marginal totals (gravity models are a sub-species of this fitting): \code{\link{ipf}}}
\item{Re-aggregating matrices to a different set of dimensions using an equivalence table: \code{\link{reaggregate.matrix}}}
\item{Numeric integration by adaptive quadrature for use in assignment algorithms: \code{\link{cost.integrator}}}
\item{Loading a variety of highway network file formats into a standard internal format: \code{\link{highway.net}}}
\item{Equilibrium highway assignment using a variety of methods: \code{\link{highway.assign}}}
}

Other basic operations for travel demand modeling, such as trip generation models using either rate- or regression-based
methods, cross-classification models, gravity models, destination choice models, and mode choice models using a variety
of logit formulations can be straightforwardly constructed (and often estimated) using a combination of \R packages and
\pkg{travelr} tools.  Examples and vignettes will gradually be added to \pkg{travelr} to illustrate these operations.

Additional tools may appear later, most notably tools for transit path-building and assignment.

\pkg{travelr} is intended both for producing \\dQuote{working models} and for researching model algorithms, particularly
algorithms for highway assignment.
}
\references{
Martin, W.A., and McGuckin, N.A., 1998, \emph{NCHRP Report 365: Travel Estimation Techniques for Urban Planning},
National Cooperative Highway Research Program / Transportation Research Board
Ortuzar, J. D., and Willumsen, L.G., 2001, \emph{Modelling Transport, Third Edition}, John Wiley & Sons
}
\examples{
\dontrun{
# A tiny, complete travel model using travelr functions
# You may run this example with example(skim.paths)
data(SiouxFalls)

# Trip Generation
productions<-rowSums(SiouxFalls.od)
attractions<-colSums(SiouxFalls.od)

# Highway Skims
cost.function<-with(SiouxFalls.net$Links,function(...)FFTime)
aclass <- make.assignment.class(SiouxFalls.net,"All",SiouxFalls.od)
aset <- new.assignment.set(SiouxFalls.net,list(All=aclass),cost.volume.type="vector",cost.function=cost.function)
paths <- build.paths(aset,aset$ff.cost)
travel.times <- skim.paths(paths,aset$ff.cost)[["All"]] # only one purpose: "All trips"

# Trip Distribution (Gravity Model with gamma function)
base.distribution <- hwy.gamma.function(travel.times,-0.02,-0.123) # HBW coefficients from NCHRP 365
trip.table <- ipf(base.distribution,list(rows=productions, cols=attractions),method="absolute")
aset <- hwy.update.demand(aset,"All",trip.table)

# Trip Assignment
assignment.results <- highway.assign(aset,method="Frank.Wolfe")
loaded.links <- assignment.results$volumes
}
}
