/*
 *  travelr/src/short_path_R.cc by Jeremy Raw  Copyright (C) 2010
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

// R extension to build shortest paths from a network structure and cost vector, and to perform operations on the
// resulting path tree (load, skim, walk, intercept)

// The error-checking that happens here is primarily for debugging purposes.  These functions are intended to be called
// from R wrapper functions that do complete error-checking (see path_functions.R in the travelr package source).

// A note on coding standards:
// These functions are a C-level facade for R to use in calling the C++ code that does the heavy lifting
// All SEXP structures (R parameters) have names starting in lower-case r, and with the remainder beginning with an
// upper case letter.

#include <R.h>
#include <Rinternals.h>
#include "build_path_R.h"

// Helper class to manage network attributes

class NetworkParameters {
	private:
		int numNodes;     				
		int numLinks;						
		int numZones;						
		int firstThruNode;

		SEXP rNodesName;
		SEXP rLinksName;
		SEXP rZonesName;
		SEXP rThruName;

		SEXP rName; SEXP rValue;

		int getAttribute(SEXP rNetwork, SEXP rAttrName);
		void setAttribute(SEXP rNetwork, SEXP rAttrName, int value);
						  
	public:
		NetworkParameters( SEXP networkWithAttributes );
		~NetworkParameters();

		int getNodes() const { return numNodes; }
		int getLinks() const { return numLinks; }
		int getZones() const { return numZones; }
		int getFirstThruNode() const { return firstThruNode; }

		void setNetworkParameters( SEXP networkForAttributes );
};

int NetworkParameters::getAttribute( SEXP rNetwork, SEXP rAttrName ) {
	SET_STRING_ELT(rName,0,rAttrName);
	rValue = getAttrib(rNetwork,rName);
	int value = -1;
	if ( isInteger(rValue) ) value = *INTEGER(rValue);
	return value;
}

void NetworkParameters::setAttribute( SEXP rNetwork, SEXP rAttrName, int value ) {
	SET_STRING_ELT(rName,0,rAttrName);
	INTEGER(rValue)[0] = value;
	setAttrib(rNetwork,rName,duplicate(rValue));
}

NetworkParameters::NetworkParameters( SEXP rNetwork ) :
	numNodes(-1), numLinks(-1), numZones(-1), firstThruNode(-1),
	rNodesName(mkChar("numNodes")), rLinksName(mkChar("numLinks")),
	rZonesName(mkChar("numZones")), rThruName(mkChar("firstThruNode")),
	rName(PROTECT(allocVector(STRSXP,1))),rValue(PROTECT(allocVector(INTSXP,1)))
{
	if ( !isNull(rNetwork) ) {
		numNodes	  = getAttribute( rNetwork,rNodesName );
		numLinks	  = getAttribute( rNetwork,rLinksName );
		numZones	  = getAttribute( rNetwork,rZonesName );
		firstThruNode = getAttribute( rNetwork,rThruName );
	}
#ifdef DEBUG_TRAVELR
	Rprintf("Loaded Network Parameters:\n");
	Rprintf("Nodes: %d, Links: %d, Zones: %d, FirstThruNode %d\n",numNodes,numLinks,numZones,firstThruNode);
#endif
}

NetworkParameters::~NetworkParameters() {
	UNPROTECT(2);
}

void NetworkParameters::setNetworkParameters( SEXP rNetwork ) {
#if 0
	Rprintf("Setting Network Attributes\n");
	Rprintf("Nodes: %d, Links: %d, Zones: %d, FirstThruNode %d\n",numNodes,numLinks,numZones,firstThruNode);
#endif
	if ( ! isNull(rNetwork) ) {
		setAttribute( rNetwork, rNodesName, numNodes );
		setAttribute( rNetwork, rLinksName, numLinks );
		setAttribute( rNetwork, rZonesName, numZones );
		setAttribute( rNetwork, rThruName,  firstThruNode );
	}
}

extern "C"
   SEXP shortest_paths(SEXP rEdges, SEXP rOffsets, SEXP rTurns, SEXP rCosts, SEXP rPenalties)
{
	// Build a shortest path forest from nodes 1-z
	// Notes on R weirdnesses:
	//    1. the rows are the data fields, and the columns are the records

	// Load network parameters

	NetworkParameters np(rEdges);

	int	 numLinks	  = int(ncols(rEdges));
	int	 numNodes	  = int(ncols(rOffsets));	 // Used to compute size of shortest path tree (SPT)
	int	 numCosts	  = int(length(rCosts));	 // Must match numLinks
	int  numZones     = np.getZones();
	int  firstThruNode= np.getFirstThruNode();

	// Basic sanity checks

	if ( numNodes != np.getNodes() )
		error("Network node dimension (%d) does not match node number attribute(%d)\n",numNodes,np.getNodes());
	if ( numLinks != np.getLinks() )
		error("Network link dimension (%d) does not match link number attribute(%d)\n",numLinks,np.getLinks());
	if ( numCosts != np.getLinks() )
		error("Cost vector length (%d) must match network link dimension (%d)\n",numCosts,np.getLinks());
	if ( numZones > numNodes )
		error("Number of Nodes (%d) must be at least the number of Zones (%d)\n",
			  np.getNodes(),np.getZones());
	if ( firstThruNode >= numNodes )
		error("First through node (%d) must be less than number of nodes (%d)\n",
			  firstThruNode,numNodes);

	EdgeList	*edges	   = (EdgeList*)(INTEGER(rEdges));
	OffsetList	*offsets   = (OffsetList*)(INTEGER(rOffsets));
	double		*costs	   = (!isNull(rCosts)) ? REAL(rCosts) : 0;
	TurnList	*turns	   = (!isNull(rTurns)) ? (TurnList*)(INTEGER(rTurns)) : 0;
	double		*penalties = (!isNull(rPenalties)) ? REAL(rPenalties) : 0;

#ifdef DEBUG_TRAVELR_PATH
	Rprintf("START shortest_paths DEBUG RECORD\n");
	int numPenalties = penalties ? length(rPenalties) : 0;
	int numTurns = turns ? ncols(rTurns) : 0;

	Rprintf("Number of Zones = %d\n",numZones);
	Rprintf("First Through Node = %d\n",firstThruNode);
	Rprintf("Costs: %d, Links: %d\n",numCosts,numLinks);
	Rprintf("Penalties: %d\n",numPenalties);

	Rprintf("Edges\n");
	for ( int r=0; r<numLinks; ++r ) {
		EdgeList * el = edges+r;
		Rprintf("Edge %d: %d->link(%d,%d)\n",r,el->Link,el->A,el->B);
	}
	Rprintf("Offsets\n");
	for ( int r=0; r<numNodes; ++r ) {
		Rprintf("Offset %d:",r); (offsets+r)->Dump();
	}
	int max_c = numCosts;
	if (max_c>10) max_c=10;
	Rprintf("First %d Costs:\n",max_c);
	for ( int r=0; r<max_c; ++r)
		Rprintf("Cost %d: %f\n",r,costs[r]);
	if (penalties && turns) {
		int max_t = numTurns;
		if (max_t>10) max_t=10;
		for ( int r=0; r<max_t; ++r ) {
			Rprintf("Turn List %d:",r); (turns+r)->Dump(penalties);
		}
	} else {
		Rprintf("No penalty table\n");
	}
	Rprintf("Allocating SPF as space for %d integers (2 x %d x %d)\n",2*numNodes*numZones,numNodes,numZones);
	Rprintf("END shortest_paths DEBUG RECORD\n");
#endif

	// Construct network object for path builder
	Network netwk( edges, offsets, turns, penalties, numLinks, numNodes, numZones, firstThruNode );

	// Allocate the shortest path forest (SPF - "a set of trees")
	// Use -1 value as shorthand for UNALLOCATED 
	SEXP rSPF;
	PROTECT(rSPF=alloc3DArray(INTSXP,2,np.getNodes(),np.getZones()));
	int * spf = INTEGER(rSPF);
	for ( int r=0; r<(2*numNodes*numZones); ++r)
		spf[r]=-1;
	np.setNetworkParameters( rSPF );

	// Allocate the array for accumulating path distances
	SEXP rCostTree;
	PROTECT(rCostTree=allocMatrix(REALSXP,numNodes,numZones));

	// Construct the path builder itself, using allocated R objects to hold computed results.

	int result = PathBuilder( &netwk, spf, REAL(rCostTree) ).Paths(costs);
	if (result) error("shortest_paths failed with result %d\n",result);

	UNPROTECT(2);
	return(rSPF);
}

extern "C"
   SEXP load_paths( SEXP rSPF, SEXP rDemand )
{
	NetworkParameters np(rSPF);

	if ( !isInteger(rSPF) ) error("load_paths: path tree must have integer storage mode");
	if ( !isReal(rDemand) ) error("load_paths: demand matrix must be real-valued");

	ShortestPathForest SPF(np.getNodes(),np.getZones(),np.getLinks(),INTEGER(rSPF));
	SkimMatrix<double> dm(REAL(rDemand),np.getZones());

	SEXP rLoad;
	PROTECT(rLoad=allocVector(REALSXP,np.getLinks()));
	SkimVector<double> lv(REAL(rLoad),np.getLinks());
	lv.setZero();

	PathWalker<double,double>( SPF, lv, dm ).Load();   // copies from demand matrix to load vector

	UNPROTECT(1);
	return(rLoad);
}

extern "C"
   SEXP build_and_load_paths( SEXP rEdges, SEXP rOffsets, SEXP rTurns, SEXP rCosts, SEXP rPenalties, SEXP rDemand ) {
	// Streamlined function to build paths and load them, designed for speed
	// If this files with the network, call the separate path and load functions for greater error-checking

	NetworkParameters np(rEdges);

	EdgeList	*edges	   = (EdgeList*)(INTEGER(rEdges));
	OffsetList	*offsets   = (OffsetList*)(INTEGER(rOffsets));
	double		*costs	   = (!isNull(rCosts)) ? REAL(rCosts) : 0;
	TurnList	*turns	   = (!isNull(rTurns)) ? (TurnList*)(INTEGER(rTurns)) : 0;
	double		*penalties = (!isNull(rPenalties)) ? REAL(rPenalties) : 0;

	// Construct network object for path builder
	Network netwk( edges, offsets, turns, penalties,
				   np.getLinks(), np.getNodes(), np.getZones(), np.getFirstThruNode() );

	// Allocate the shortest path forest (SPF - "a set of trees")
	// Use -1 value as shorthand for UNALLOCATED 
	SEXP rSPF;
	PROTECT(rSPF=alloc3DArray(INTSXP,2,np.getNodes(),np.getZones()));
	int * spf = INTEGER(rSPF);
	for ( int r=0; r<(2*np.getNodes()*np.getZones()); ++r)
		spf[r]=-1;
	np.setNetworkParameters( rSPF );

	// Allocate the array for accumulating path distances
	SEXP rCostTree;
	PROTECT(rCostTree=allocMatrix(REALSXP,np.getNodes(),np.getZones()));

	// Construct the path builder itself, using allocated R objects to hold computed results.

	int result = PathBuilder( &netwk, spf, REAL(rCostTree) ).Paths(costs);
	if (result) error("build_paths_and_load failed with result %d\n",result);

	// Now build the variables required for the load

	ShortestPathForest SPF(np.getNodes(),np.getZones(),np.getLinks(), spf);
#ifdef DEBUG_TRAVELR_LOAD
	if ( isNull(rDemand) ) Rprintf( "rDemand is NULL!\n");
#endif
	SkimMatrix<double> dm(REAL(rDemand),np.getZones());

	SEXP rLoad;
	PROTECT(rLoad=allocVector(REALSXP,np.getLinks()));
	SkimVector<double> lv(REAL(rLoad),np.getLinks());
	lv.setZero();

	PathWalker<double,double>( SPF, lv, dm ).Load();   // copies from demand matrix to load vector

	// return list containing paths and loads
	SEXP rResult;
	PROTECT(rResult=allocVector(VECSXP,2));
	SET_VECTOR_ELT(rResult,0,rLoad);
	SET_VECTOR_ELT(rResult,1,rSPF);
	SEXP rNames;
	PROTECT(rNames=allocVector(STRSXP,2));
	SET_STRING_ELT(rNames,0,mkChar("volumes"));
	SET_STRING_ELT(rNames,1,mkChar("paths"));
	setAttrib(rResult, R_NamesSymbol, rNames );

	UNPROTECT(5);
	return(rResult);
}

extern "C"
   SEXP skim_paths( SEXP rSPF, SEXP rCostMatrix, SEXP rEmptyVal )
{
	// Get number zones, number of nodes and number of links from tree attributes.

	NetworkParameters np(rSPF);
	double empty_val = REAL(rEmptyVal)[0];

	SEXP rSkimMatrix;
	PROTECT(rSkimMatrix=allocMatrix(REALSXP,np.getZones(),np.getZones()));
	SkimMatrix<double> dm(REAL(rSkimMatrix),np.getZones());
	dm.setAll(empty_val);

	ShortestPathForest SPF(np.getNodes(),np.getZones(),np.getLinks(),INTEGER(rSPF));
	SkimVector<double> cv(REAL(rCostMatrix), np.getLinks());

	PathWalker<double,double> ( SPF, cv, dm ).Skim(empty_val);      // accumulate cost vector into matrix cells along paths

	UNPROTECT(1);
	return(rSkimMatrix);
}

extern "C"
   SEXP walk_paths( SEXP rSPF, SEXP rOrigins, SEXP rDestinations )
{
	// This function walks all possible combinations of origins and destinations
	// WARNING: origin and destination are ZERO-BASED lists of nodes (used directly as indices into the path tree)

	NetworkParameters np(rSPF);

	SEXP rPathList;
	PROTECT(rPathList=allocMatrix(VECSXP,length(rOrigins),length(rDestinations)));

	ShortestPathForest SPF(np.getNodes(),np.getZones(),np.getLinks(),INTEGER(rSPF));
	PathWalker<int,int> pw( SPF );

	SEXP rPath;
	int maxPath, currPath, *pathlist=0;
	int *op=INTEGER(rOrigins);
	int *dp=INTEGER(rDestinations);
	int pair=0;
	currPath = maxPath = 100;
	for ( int o=0; o<length(rOrigins); ++o ) {
		for ( int d=0; d<length(rDestinations); ++d ) {
			int org=op[o];
			int dst=dp[d];
			if (org!=dst) {
				while(1) {
					pathlist = new int[maxPath];
					currPath =pw.Walk(org, dst, pathlist, maxPath);
					if (currPath>maxPath) {
						delete [] pathlist;
						maxPath = currPath;
					} else
						break;
				}
			} else {
				currPath=0;
			}
			PROTECT( rPath = allocVector(INTSXP,currPath) );
			int * pp = (int *)INTEGER(rPath);
			if ( currPath>0 ) {
				int lastP = currPath-1;
#ifdef DEBUG_TRAVELR_WALK
				Rprintf("Path (0-based) from %d -> %d: ",org,dst);
#endif
				for ( int p=lastP; p>=0; --p ) {
					pp[p] = pathlist[lastP-p];
				}
#ifdef DEBUG_TRAVELR_WALK
				for ( int p=0; p<=lastP; ++p ) {
					Rprintf(" %d",pp[p]);
				}
				Rprintf("\n");
#endif
				delete [] pathlist;
			}
#ifdef DEBUG_TRAVELR_WALK
			else {
				Rprintf("No path from %d -> %d\n",org,dst);
			}
#endif
			SET_VECTOR_ELT(rPathList,pair++,rPath);
			UNPROTECT(1);
		}
	}

	UNPROTECT(1);
	return(rPathList);
}

extern "C"
   SEXP walk_pairs( SEXP rSPF, SEXP rOrigin, SEXP rDest )
{
	// This function walks matched pairs of origin and dest
	// Get number zones, number of nodes and number of links from tree attributes.
	// WARNING: origin and destination are ZERO-BASED lists of nodes (used directly as indices into the path tree)

	NetworkParameters np(rSPF);

#ifdef DEBUG_TRAVELR_WALK
	Rprintf("Walking Explicit O/D Pairs\n");
#endif

	if ( length(rOrigin)!=length(rDest) ) error("For walk_pairs, origin and dest must have the same length");
			// should have already checked in R code

	SEXP rPathList;
	PROTECT(rPathList=allocVector(VECSXP,length(rOrigin)));

	SEXP pathnames;
	PROTECT(pathnames=allocVector(VECSXP,length(rOrigin)));

	ShortestPathForest SPF(np.getNodes(),np.getZones(),np.getLinks(),INTEGER(rSPF));
	PathWalker<int,int> pw( SPF );

	// walk through all combinations of origin and destination elements
	// TODO: enlarge to pass a matrix, or list, or data.frame of pairs.

	SEXP rPath;
	int maxPath, currPath, *pathlist=0;
	int *op=INTEGER(rOrigin), *dp=INTEGER(rDest);
	currPath = maxPath = 100;
	for ( int pair=0; pair<length(rOrigin); ++pair ) {
		int org=op[pair];
		int dst=dp[pair];
		if (org!=dst) {
			while(1) {
				pathlist = new int[maxPath];
				currPath =pw.Walk(org, dst, pathlist, maxPath);
				if (currPath>maxPath) {
					delete [] pathlist;
					maxPath = currPath;
				} else
					break;
			}
		} else {
			currPath=0;
		}
		PROTECT( rPath = allocVector(INTSXP,currPath) );

		int * pp = (int *)INTEGER(rPath);
		if ( currPath>0 ) {
			int lastP = currPath-1;
#ifdef DEBUG_TRAVELR_WALK
			Rprintf("Path (0-based) from %d -> %d: ",org,dst);
#endif
			for ( int p=lastP; p>=0; --p ) {
				pp[p] = pathlist[lastP-p];
			}
#ifdef DEBUG_TRAVELR_WALK
			for ( int p=0; p<=lastP; ++p ) {
				Rprintf(" %d",pp[p]);
			}
			Rprintf("\n");
#endif
			delete [] pathlist;
		}
#ifdef DEBUG_TRAVELR_WALK
		else {
			Rprintf("No path from %d -> %d\n",org,dst);
		}
#endif
		SET_VECTOR_ELT(rPathList,pair,rPath);

		char name_buf[20];
		sprintf(name_buf,"O%d.D%d",org+1,dst+1);  // 1-based names
		SET_VECTOR_ELT(pathnames, pair, mkChar(name_buf));

		UNPROTECT(1);
	}
	setAttrib(rPathList, R_NamesSymbol, pathnames );
	UNPROTECT(2);

	return(rPathList);
}

extern "C"
   SEXP intercept_paths( SEXP rSPF, SEXP links )
{
	// Get number zones, number of nodes and number of links from tree attributes.
	// TODO: thorough error-checking on the parameter types, consistent bounds, etc.

	// links should be a link vector (with numLinks elements), and be an integer (really BOOLEAN) array in which
	// TRUE/non-zero elements are checked for path interception and the corresponding OD pair is marked

	NetworkParameters np(rSPF);

#ifdef DEBUG_TRAVELR_ICPT
	Rprintf("Intercepting Links\n");
	Rprintf("Zones: %d, Nodes: %d, Links: %d\n",np.getZones(),np.getNodes(),np.getLinks());
	Rprintf("Selected Links:");
	for ( int icp=0; icp<np.getLinks(); ++icp )
		Rprintf(icp>0?",%d":"%d",INTEGER(links)[icp]);
	Rprintf("\n");
#endif

	ShortestPathForest SPF(np.getNodes(),np.getZones(),np.getLinks(),INTEGER(rSPF));

	SEXP intercept;
	PROTECT(intercept=allocMatrix(INTSXP,np.getZones(),np.getZones()));
	SkimMatrix<int> im( INTEGER(intercept), np.getZones() );
	im.setAll(0);

	SkimVector<int> ln( INTEGER(links), np.getLinks() );

	PathWalker<int,int> ( SPF, ln, im ).Intercept(); // places results in demand matrix

	UNPROTECT(1);
	return(intercept);
}
