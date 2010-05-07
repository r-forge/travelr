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

\name{reaggregate.matrix}
\alias{reaggregate}
\alias{reaggregate.matrix}
\title{
Aggregate or disaggregate at matrix
}
\description{
Change the size of a matrix by adding or subdividing its cells.
}
\usage{
reaggregate.matrix(mtx, eq, nrow=NULL, ncol=NULL)
}
\arguments{
  \item{mtx}{
  A matrix that will be refactored
}
  \item{eq}{
  A \code{data.frame} which is a correspondence table describing how to compute output cells from input cells
}
  \item{nrow}{
  An explict number of rows in the result matrix; the default is to use the maximum value of \code{eq$o}
}
  \item{ncol}{
  An explict number of columns in the result matrix; the default is to use the maximum value of \code{eq$p}
}
}
\details{

\code{reaggregate.matrix} will compute a new matrix with revised dimensions from an input matrix, \code{mtx}, based on
the correspondences described in the equivalence table, \code{eq}.

The equivalence table must be a \code{data.frame} or \code{matrix} containing the following columns:
\describe{
	\item{i}{The row index of an input matrix cell}
	\item{j}{The column index of an input matrix cell}
	\item{o}{The row index of an output matrix cell}
	\item{p}{The column index of an output matrix cell}
	\item{fact}{The fraction of cell \code{mtx[i, j]} to be placed in cell \code{mtx.out[o, p]} }
}

Each row in the equivalence table describes how to move some or all of the value in the \code{[i,j]}
cell in the input matrix to the corresponding $\code{[o,p]} cll in the output matrix.  The value in
the input cell is multiplied by \code{eq$fact} and all the values contributing to the value of cell
$\code{[o,p]} in the output are added together to form the final result.

If a particular \code{i,j} pair is not represented in the equivalence table then that pair will not contribute to the
output matrix.  Indices that are beyond the dimensions of the input matrix may introduce \code{NA}'s into an output
cell, even if other correspondences introduce known values.

With suitable correspondence factors, matrices may be expanded or reduced in size.
}

\value{
A \code{matrix} with dimensions \code{[nrow,ncol]} computed by applying correspondence factors.
}

\author{ Jeremy Raw }

\examples{
   # The basic mechanics of aggregating a matrix...
   m <- matrix(1:100,nrow=10,ncol=10)
   eq <- expand.grid(i=1:10,j=1:10)
   eq$fact <- 1
   eq$o <- ceiling(eq$i/2)
   eq$p <- ceiling(eq$j/2)
   om <- reaggregate.matrix(m,eq)

   # Disaggregating is trickier since factors are necessary
   req <- eq[,c("i","j","fact","o","p")]
   names(req) <- c("o","p","fact","i","j")
   req$fact <- 0.25
   rom <- reaggregate.matrix(om,req)

   # Building an equivalence table using a correspondence table
   # Used in travel modeling to convert zones to districts
   ct <- data.frame( from=c( 1, 2, 3, 4, 5, 6, 7, 8, 9,10),
                       to=c( 2, 2, 1, 3, 3, 3, 4, 4, 5, 5) )
   c.eq <- expand.grid(i=1:10,j=1:10)
   c.eq$o <- ct$to[match(c.eq$i,ct$from)]
   c.eq$p <- ct$to[match(c.eq$j,ct$from)]
   c.eq$fact <- 1
   c.om <- reaggregate.matrix(m,c.eq)
}
