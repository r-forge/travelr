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

\name{assignment.class}
\alias{assignment.class}
\alias{make.assignment.class}
\alias{add.assignment.class}
\alias{add.assignment.class.list}

\title{
Building highway assignment classes
}
\description{
These functions facilitate constructing network variants and demand sets for use in multi-class highway assignment.
}
\usage{
make.assignment.class(network, name, demand, link.subset=TRUE, penalty.subset=NULL, cost.function=NULL)
add.assignment.class(classes,aclass)
\method{add.assignment.class}{list}(classes,aclass)
}
\arguments{
  \item{network}{
	The base highway network on which this assignment set will be loaded
}
  \item{name}{
	A name (character string) for the assignment class
}
  \item{demand}{
    A demand matrix for the named assignment class
}
  \item{link.subset}{
	An index vector that can be used to select links in the base network that will be available for this assignment
	class
}
  \item{penalty.subset}{
	An index vector that selects which penalties (from the network's turn penalty table) will be applied to this
	assignment set
}
  \item{cost.function}{
	A \code{\link{cost.function}} for the class. Not required or recommend; see details
}
  \item{classes}{
	A list, either empty or containing previously added assignment classes
}
  \item{aclass}{
  A highway.assignment.class constructed by \code{make.assignment.class}
}
}
\details{
These functions support multi-class \link{highway assignment}.

Use \code{make.assignment.class} to construct a new assignment class.  Use \code{add.assignment.class} to add another
assignment class to a list of assignment classes.  A list of assignment classes is required for the \code{classes} parameter
in \code{\link{new.assignment.set}}.

It is common to have a non-null \code{link.subset}, with the most common situation being to distinguish networks in
which all links are available from one that only contains links available to a certain vehicle class (e.g. trucks or
high-occupancy vehicles).  It is similarly useful to have a non-null \code{penalty.subset}, either for penalties that
are only applicable during certain periods of the day, or that are only applicable to certain vehicle classes.  Only the
links and penalties in the indicated subset will be examined when building paths.  Care should be taken that the
terminal nodes (zones) all remain connnected when the particular subset is selected.  The \pkg{igraph} package can be
useful for rapidly testing connectivity.  The shortest path functions will warn if the resulting network subset is not
connected.

The \code{cost.function} parameter is not used directly, but is provided as a convenience since it is sometimes
desirable to decompose the \code{cost.function} for the \code{\link{assignment.set}} into smaller functions specifically
for the individual classes.  These can be called by the user's assignment set cost function.  Getting fancy with cost
functions is not advised since it can wreak havoc with the convergence of equilibrium assignment algorithms, and the
simplest possible cost function model should be sought (which usually means providing a single function through
\code{\link{new.assignment.set}}).

\code{add.assignment.class} is generic, and can add an assignment class to an \code{\link{assignment.set}} or to an
existing list of assignment classes previously created with \code{add.assignment.class}.  It performs a variety of
tests to ensure that the proposed assignment class has everythign it needs to operate successfully.
}
\value{
\code{make.assignment.class} returns a list of elements with class \code{highway.assignment.class} suitable for
building a \code{\link{assignment.set}}.  The specific list elements are:
\describe{
	\item{name}{A unique name for the class, constructed according to the rules of legal names for \R symbols; see \code{\link{make.names}} for help in creating a good name}
	\item{demand}{A demand matrix with demand for this class (e.g. trucks, or high-occupancy vehicles)}
	\item{network.set}{The network topology, including turn penalties and open links, for this assignment class}
	\item{cost.function}{A class-specific cost function; only called if specifically coded in the assignment set cost function}
}

\code{add.assignment.class} returns its first argument, to which \code{aclass} has been added.
}
\author{
Jeremy Raw
}
\seealso{
	\code{\link{assignment.set}},\code{\link{highway.assign}},
	\code{\link{add.assignment.class.highway.assignment.set}}
}
