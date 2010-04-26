/*
 *  travelr/src/path_templates_R.h by Jeremy Raw  Copyright (C) 2010
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  A copy of the GNU General Public License is available at
 *  http://www.r-project.org/Licenses/
 *  and included in the R distribution (in directory ‘share/licenses’).
*/

#ifndef _BUILD_PATH_R_H_
#error "Must include template headers from build_path_R.h"
#endif

#ifdef DEBUG_TRAVELR
#include <time.h>
#include <Rinternals.h>
#endif

// Supporting operations for PathWalker data types

template <class DataType> void SkimVector<DataType>::setZero() {
	for ( int i=0; i<getLength(); ++i ) Set(i,0);  // DataType needs implicit conversion from 0
}

template <class DataType> int SkimVector<DataType>::isAllZeroes() const {
	int i=0;
	for ( ; i<getLength(); ++i )
		if (Get(i)!=0) break;  // DataType needs implicit != comparison to 0
	return (i==getLength());
};

template <class DataType> int SkimVector<DataType>::whichNonZero() const {
	int i=0;
	for ( ; i<getLength(); ++i )
		if (Get(i)!=0) break;  // DataType needs implicit != comparison to 0
	return (i>=getLength()?-1:i);
};

// PathWalker class definition

template <class LinkType, class DemandType> void PathWalker<LinkType,DemandType>::Load() {
	// Load values onto the paths
#ifdef DEBUG_TRAVELR_LOAD
	clock_t clock0,clock1;
	Rprintf("Starting path load for %d origins (Start = %ld)\n",SPF.getNumOrigins(),clock0=clock());
#endif
	if ( ! linkVector.isAllZeroes() )
		error("load vector (size %d) is not all zeroes: %d->%f",
			  linkVector.getLength(),linkVector.whichNonZero(),linkVector.Get(linkVector.whichNonZero()));
	for ( int origin=0; origin<SPF.getNumOrigins(); ++origin ) {
		ShortestPathTree spt = SPF.getSPT(origin);
		for ( int dest=0; dest<SPF.getNumOrigins(); ++dest ) {
			if (origin!=dest) {
				DemandType dvalue = getDemand(origin,dest);
#ifdef DEBUG_TRAVELR_LOAD
				Rprintf("Loading %d -> %d as %f:",origin,dest,dvalue);
#endif
				for ( int node=dest; node!=origin; node = spt.Parent(node) ) {
					if (!spt.isValid(node)) {
						error("Invalid link for node %d: %d\n",node,spt.Link(node));
					}
					addLinkValue(spt.Link(node), dvalue);
#ifdef DEBUG_TRAVELR_LOAD
					Rprintf(" %d (%.0f)",spt.Link(node),getLinkValue(spt.Link(node)));
#endif
				}
#ifdef DEBUG_TRAVELR_LOAD
				Rprintf("\n");
#endif
			}
		}
	}
#ifdef DEBUG_TRAVELR_LOAD
	Rprintf("Done with path Load (End = %ld)\n",clock1=clock());
	Rprintf("About %.4f seconds (%ld clocks per second)\n",(double(clock1)-double(clock0))/double(CLOCKS_PER_SEC),CLOCKS_PER_SEC);
#endif
}

template <class LoadType, class DemandType> void PathWalker<LoadType,DemandType>::Skim(const DemandType & empty_value) {
	// Skim values from the paths
#ifdef DEBUG_TRAVELR_SKIM
	clock_t clock0,clock1;
	Rprintf("Starting path Skim for %d origins (Start = %ld)\n",SPF.getNumOrigins(),clock0=clock());
#endif
	for ( int origin=0; origin<SPF.getNumOrigins(); ++origin ) {
		ShortestPathTree spt = SPF.getSPT(origin);
		for ( int dest=0; dest<SPF.getNumOrigins(); ++dest ) {
			if (origin!=dest) {
				for ( int node=dest; node!=origin; node = spt.Parent(node) ) {
					if (!spt.isValid(node)) {
						error("Invalid link for node %d: %d\n",node,spt.Link(node));
					}
					addDemand(origin, dest, getLinkValue(spt.Link(node)) );
				}
			} else {
				setDemand(origin, dest, empty_value);
			}
		}
	}
#ifdef DEBUG_TRAVELR_SKIM
	Rprintf("Done with path Skim (End = %ld)\n",clock1=clock());
	Rprintf("About %.4f seconds (%ld clocks per second)\n",(double(clock1)-double(clock0))/double(CLOCKS_PER_SEC),CLOCKS_PER_SEC);
#endif
}

template <class LoadType, class DemandType> int PathWalker<LoadType,DemandType>::Walk( int origin, int dest, int * buffer, int bufSize ) {
	// Return 0-based link indices along the path of interest
	int next = 0;
	if (!buffer) error("No buffer supplied for Path walk\n");
	if (origin<0||origin>=SPF.getNumOrigins()) error("Requested origin (%d) that is not in zone range(0:%d)\n",origin,SPF.getNumOrigins()-1);
	ShortestPathTree spt = SPF.getSPT(origin);
	for (int node=dest; node!=origin; node = spt.Parent(node)) {
		if (!spt.isValid(node)) {
			error("Invalid link for node %d: %d\n",node,spt.Link(node));
		}
		if (next<bufSize)
			buffer[next++] = spt.Link(node);
		else next++;
	}
	return next;
}

template <class LoadType, class DemandType> void PathWalker<LoadType,DemandType>::Intercept() {
	// Mark OD pairs whose paths contain links of interest
	// Differs from skim in that we don't look at all links, but short-circuit after the first hit
	// Perhaps an option to count the number of intercepted links on the path?
#ifdef DEBUG_TRAVELR_ICPT
	clock_t clock0,clock1;
	Rprintf("Starting path Intercept for %d origins (Start = %ld)\n",SPF.getNumOrigins(),clock0=clock());
#endif
	for ( int origin=0; origin<SPF.getNumOrigins(); ++origin ) {
		ShortestPathTree spt = SPF.getSPT(origin);
		for ( int dest=0; dest<SPF.getNumOrigins(); ++dest ) {
			if (origin!=dest) {
				for ( int node=dest; node!=origin; node = spt.Parent(node) ) {
					if (!spt.isValid(node)) {
						error("Invalid link for node %d: %d\n",node,spt.Link(node));
					}
					if ( getLinkValue(spt.Link(node))!=0 ) {
#ifdef DEBUG_TRAVELR_ICPT
						Rprintf("Intercepted %d : %d (%d)\n",origin,dest,getLinkValue(spt.Link(node)));
#endif
						setDemand(origin, dest, 1); // mark the OD pair
						break;                      // and continue to the next OD pair
					}
				}
			}
		}
	}
#ifdef DEBUG_TRAVELR_ICPT
	Rprintf("Done with path Intercept (End = %ld)\n",clock1=clock());
	Rprintf("About %.4f seconds (%ld clocks per second)\n",(double(clock1)-double(clock0))/double(CLOCKS_PER_SEC),CLOCKS_PER_SEC);
#endif
}
