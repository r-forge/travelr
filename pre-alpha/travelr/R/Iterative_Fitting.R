####################################################################################################
#   travelr\r\Iterative_Fitting.R by Jeremy Raw  Copyright (C) 2010
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

hwy.gamma.function<-function(x,b,c)	(x^b)*exp(x*c)

# A function to perform iterative proportional fitting on an array with
# 2 (or more?) dimensions -- how would that work for more than 2?

ipf <- function(mtx,factors,method=NULL,max.rmse=1e-7,max.iter=50) {
	# a is an object with dimensions
	# Determine the dimensions of a, and return an error
	# Note that default method (if method is not supplied) is "absolute"
	# Other options are "percent" or "fraction"
	da<-dim(mtx)
	la<-length(da)
	dn<-dimnames(mtx)
	if ( is.null(da) || la!=2 )
		stop(sprintf(gettext("Parameter 'mtx' must have two dimensions, not %d"),length(la))) # for now...
	if ( is.list(factors) ) {
		lf<-length(factors)
		if ( ! all(sapply(factors,is.numeric)) )
			stop("Growth factors must be numeric vectors")
		if ( lf==1 ) {
			factors[[2]]<-factors[[1]]
			lf=2
		}
	} else if ( is.vector(factors) && is.numeric(factors) ) {
		factors<-list(rows=factors,cols=factors)
		lf<-2
	} else {
		stop("Growth factors must be a single numeric vector or a list or data.frame of numeric vectors\n")
	}
	if (lf!=la) {
		stop('Must have as many growth factors as matrix dimensions')
	}
	for ( i in seq_along(da) ) {
		if (da[i]!=length(factors[[i]])) {
			warning(
				sprintf(
					gettext("Growth factors in dimension %d have wrong length (%d vs %d): elements will be recycled.\n"),
					i,length(mtx[[i]]),length(factors[[i]])
					)
				)
		}
		if ( any(factors[[i]]<0) ) {
			stop("All factors must be >= 0")
		}
	}
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
	if ( !isTRUE(all.equal(sum(factors[[1]]),sum(factors[[2]]))) ) {
		warning("Factors do not yield the same totals\nColumns will be adjusted to row totals")
		factors[[2]] <- factors[[2]]*(sum(factors[[1]])/sum(factors[[2]]))
	}

	# Finally, we'll actually do the fitting...
	iter<-0
	it.seq<-seq_along(factors)
	mtx<-t(mtx)
	new.rmse<-rmse(unlist(factors),c(colSums(mtx),rowSums(mtx))) # note that a is the transpose of the original matrix
	while( new.rmse>max.rmse && iter<max.iter ) {
		for (i in it.seq) mtx<-apply(mtx,1,function(v,rs)v*rs,rs=factors[[i]]/colSums(mtx))
		new.rmse<-rmse(unlist(factors),c(colSums(mtx),rowSums(mtx)))
		iter<-iter+1
#		cat("Iteration",iter,"RMSE:",new.rmse,"\n")
	}
	final<-t(mtx)
	dimnames(final)<-dn
	attr(final,"Converged")<-iter<max.iter
	attr(final,"RMSE")<-new.rmse
	attr(final,"Iteration")<-iter
	class(final)<-c("iterative.fit",class(final)) # keep the original class identity (as matrix or whatever)
	return(final)
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
