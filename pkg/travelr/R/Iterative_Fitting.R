####################################################################################################
#   travelr\r\Iterative_Fitting.R by Jeremy Raw  Copyright (C) 2010
#
#   Portions Copyright (C) 2002  Oregon Department of Transportation
#	   and released under the GNU public license
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
# 
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
# 
#   A copy of the GNU General Public License is available at
#   http://www.r-project.org/Licenses/
#   and included in the R distribution (in directory ‘share/licenses’).
####################################################################################################

# Root Mean Square Error - Everybody's Favorite
rmse<-function(v1,v2) {
	e<-v1-v2;
	sqrt(sum(e^2)/(length(e)))
}

# Gamma Function - For building gravity models with ipf

hwy.gamma.function<-function(x,b,c)	ifelse(x==0,0,(x^b)*exp(x*c))

# A function to perform iterative proportional fitting on an array with
# 2 (or more?) dimensions.  The code has been tested with two dimensions
# but in principle may have as many as you like.

ipf <- function(mtx,factors,method=NULL,max.rmse=1e-7,max.iter=50) {
	# mtx is an object with dimensions
	# Determine the dimensions of mtx, and return an error
	# Note that default method (if method is not supplied) is "absolute"
	# Other options are "percent" or "fraction", in which case new factors
	# are computed using the marginal totals of mtx
	da<-dim(mtx)
	la<-length(da)
	dn<-dimnames(mtx)
	if ( is.null(da) )
		stop(sprintf("Parameter 'mtx' must have at least two dimensions"))
	if ( is.list(factors) ) {
		lf<-length(factors)
		if ( ! all(sapply(factors,is.numeric)) )
			stop("Growth factors must be numeric vectors")
		if ( lf<la ) {
			warning(sprintf(gettext("Not enough growth factor vectors for %d dimensions: recycling"),la))
			length.diff <- la-lf
			copy.index <- 1
			new.index <- lf+1
			while ( length.diff>0 ) {
				factors[[new.index]]<-factors[[copy.index]]
				length.diff <- length.diff-1
				new.index <- new.index+1
				copy.index <- copy.index+1
			}
		}
		lf <- length(factors)
	} else if ( is.vector(factors) && is.numeric(factors) ) {
		factors<-list(rows=factors,cols=factors)
		lf<-2
	} else {
		stop("Growth factors must be a single numeric vector or a list or data.frame of numeric vectors\n")
	}
	if (lf!=la) {
		stop('Must have as many growth factors as matrix dimensions')
	}
	warn.zeroes<-0
	dim.seq <- seq_along(da)
	for ( i in dim.seq ) {
		zero.factors<-which(factors[[i]]==0)
		if ( any(zero.factors) ) {
			factors[[i]][zero.factors]<-0.001
			warn.zeroes<-warn.zeroes+1
		}
		if (da[i]!=length(factors[[i]])) {
			warning(
				sprintf(
					gettext("Growth factors in dimension %d have wrong length (%d vs %d): elements will be recycled.\n"),
					i,length(mtx[[i]]),length(factors[[i]])
					)
				)
		}
# 		if ( any(factors[[i]]<0) ) {
# 			warning(paste(factors[[i]],collapse=", "),"\n","All factors must be >= 0")
# 			factors[[i]][factors[[i]]<0]<-0
# 			cat(factors[[i]])
# 		}
	}
	if ( warn.zeroes>0 ) warning( warn.zeroes," zeroes in growth factors; adjusted to 0.001")
	if ( !is.null(method) ) {
		# For "growth factor" methods, the factors are specified relative to the existing margins
		if ( method=="percent" ) {
			factors<-lapply(factors,function(v) v/100)
			method="fraction"
		}
		if ( method=="fraction" ) {
			for ( i in seq_along(factors) ) factors[[i]]<-factors[[i]]*apply(mtx,i,sum)
			method="absolute"
		}
	} else {
		method="absolute"  # the core method factors the array so the margins match the factors
	}
# 	if ( !isTRUE(all.equal(sum(factors[[1]]),sum(factors[[2]]))) ) {
# 		warning("Factors do not yield the same totals\nColumns will be adjusted to row totals")
# 		factors[[2]] <- factors[[2]]*(sum(factors[[1]])/sum(factors[[2]]))
# 	}

	# Finally, we'll actually do the fitting...
	iter<-0
	new.rmse<-rmse(unlist(factors),c(rowSums(mtx),colSums(mtx)))
	mtx.sums<-vector("list",0)
	while( new.rmse>max.rmse && iter<max.iter ) {
 		for ( i in dim.seq ) {
			mtx.sums[[i]]<-apply(mtx,i,sum)
 			mtx<-sweep(mtx,i,factors[[i]]/mtx.sums[[i]],"*")
 		}
		new.rmse<-rmse(unlist(factors),unlist(mtx.sums))
		iter<-iter+1
#		cat("Iteration",iter,"RMSE:",new.rmse,"\n")
	}
	dimnames(mtx)<-dn
	attr(mtx,"Converged")<-iter<max.iter
	attr(mtx,"RMSE")<-new.rmse
	attr(mtx,"Iteration")<-iter
	class(mtx)<-c("iterative.fit",class(mtx)) # keep the original class identity (as matrix or whatever)
	return(mtx)
}

print.iterative.fit <- function(x,...) {
	print(addmargins(x),...)
	cat("RMSE:",attr(x,"RMSE"),"\n")
	if(attr(x,"Converged"))
		cat("Converged in",attr(x,"Iteration"),"Iterations\n")
	else
		cat("All",attr(x,"Iteration"),"Iterations Completed\n")
	invisible(TRUE)
}

###########################################################################
# In keeping with the terms of the GNU Public License, source code for the
# original ODOT code on which this version of ipf was based is here:
#
# # iterative proportional fitting function
#    ipf <- function(rowcontrol, colcontrol, seed, maxiter=50, closure=0.01){
#    # input data checks: sum of marginal totals equal and no zeros in marginal totals
#    #if(sum(rowcontrol) != sum(colcontrol)) stop("sum of rowcontrol must equal sum of colcontrol")
#    if(any(rowcontrol==0)){
#       numzero <- sum(rowcontrol==0)
#       rowcontrol[rowcontrol==0] <- 0.001
#       warning(paste(numzero, "zeros in rowcontrol argument replaced with 0.001", sep=" "))
#       }
#    if(any(colcontrol==0)){
#       numzero <- sum(colcontrol==0)
#       colcontrol[colcontrol==0] <- 0.001
#       warning(paste(numzero, "zeros in colcontrol argument replaced with 0.001", sep=" "))
#       }
#    # set initial values
#    result <- seed
#    rowcheck <- 1
#    colcheck <- 1
#    iter <- 0
#    # successively proportion rows and columns until closure or iteration criteria are met
#    while((rowcheck > closure) & (colcheck > closure) & (iter < maxiter))
#       {
# 	 rowtotal <- rowSums(result)
# 	 rowfactor <- rowcontrol/rowtotal
# 	 result <- sweep(result, 1, rowfactor, "*")
# 	 coltotal <- colSums(result)
# 	 colfactor <- colcontrol/coltotal
# 	 result <- sweep(result, 2, colfactor, "*")
#      rowcheck <- sum(abs(1-rowfactor))
# 	 colcheck <- sum(abs(1-colfactor))
# 	 iter <- iter + 1
#       }
#    result
#    }

reaggregate.matrix <- function( mtx, eq, nrow=NULL, ncol=NULL ) {
	# eq is a data.frame (or data matrix) that has columns i,j,o,p,fact
	# to build such an equivalence table, the function expand.grid is useful
	# example:
	if ( is.null(nrow) ) nrow <- max(eq$o)
	else if ( nrow < max(eq$o) ) eq <- eq[-which(eq$o > nrow),]
	# else just use nrow as it is
	if ( is.null(ncol) ) ncol<-max(eq$p)
	else if ( ncol < max(eq$p) ) eq <- eq[-which(eq$p > ncol),]
	# else just use ncol as it is

	mtx.out <- matrix(0,nrow=nrow,ncol=ncol)
	oval <- aggregate( data.frame(value=(mtx[data.matrix(eq[,c("i","j")])] * eq$fact)),
	                   by=list(o=eq$o,p=eq$p),
					   sum)
	mtx.out[data.matrix(oval[,c("o","p")])] <- oval$value
	mtx.out
}
