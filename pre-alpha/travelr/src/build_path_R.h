/*
 *  travelr/src/build_path_R.h by Jeremy Raw  Copyright (C) 2010
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
#define _BUILD_PATH_R_H_

// define DEBUG_TRAVELR to turn on debugging
// turn off noxious outputs by commenting out the sub-definitions

#define DEBUG_TRAVELR
#ifdef DEBUG_TRAVELR
#undef  DEBUG_TRAVELR_PATH
#define DEBUG_TRAVELR_LOAD
#define DEBUG_TRAVELR_SKIM
#define DEBUG_TRAVELR_WALK
#define DEBUG_TRAVELR_ICPT
#endif

struct EdgeList {
	int A;
	int B;
	int Link;
};

class OffsetList {
	private:
		int A;						// Not used directly: debug: should match index used to retrieve record
		int start;
		int end;
		int turnstart;
		int turnend;
	public:
		int Start() const     { return start; }
		int End() const       { return end; }
		int TurnStart() const { return turnstart; }
		int TurnEnd() const   { return turnend; }
		void Dump() const;
};// array of offsets for links leaving node

struct TurnList {
	private:
		int B;							// 
		int parent, child;				// 
		int p;							// Index to penalty list for details
	public:
		int    Matches( int A, int C ) const { return A==parent && C==child; }
		double Penalty( double * penalties ) const { return penalties[p]; }
		void   Dump(double * penalties) const;
};

class Network {
	private:
		EdgeList      *links;				// array of link_ct links (edge list)
		OffsetList    *stars;
		TurnList      *turns;
		double        *penalties;
		int			   link_ct;				// number of links (link elements)
		int			   node_ct;				// number of nodes (star elements)
		int			   zone_ct;				// number of zones (terminal nodes for path search)
		int			   thru_node;

	public:
		Network(EdgeList *links, OffsetList * stars, TurnList * turns, double * penalties,
				int linkCount, int nodeCount, int zoneCount, int thruNode);
		~Network();

		EdgeList     *getLinkList() const { return links; }

		int  numLinks() const      { return link_ct; }
		int  numNodes() const      { return node_ct; }
		int  numZones() const      { return zone_ct; }
		int  firstThruNode() const { return thru_node; }
		int  conformOrigins(int origins);

		int  getFirstLinkFrom(int A)  		{ return stars[A].Start(); }
		int  isLinkLeaving(int link, int A) { return link<stars[A].End(); }

		int  getNode(int link) const { return links[link].B; }

		double    turnPenalty(int A, int B, int C);
};

class PathElement;						// Defined in elements.h
class CostElement;						// Defined in elements.h

class ShortestPathTree {				// Wrapper for PathElement *, with error checking
	private:
		int numNodes;
		int numLinks;
		PathElement *SPT;
	public:
		ShortestPathTree( int numNodes, int numLinks, void * pathBuffer ) :
			numNodes(numNodes), numLinks(numLinks), SPT((PathElement*)pathBuffer) {}
		int isValid( int node ) const;
		int Parent( int node ) const;
		int Link( int node ) const;
};

class ShortestPathForest {
	private:
		int          numNodes;
		int          numOrigins;
		int			 numLinks;
		PathElement *SPF;				// Shortest-Path Forest (array zones rows, node_ct columns)
		CostElement *costs;				// Cost portion of the forest
	public:
		// TODO: need to add checking for origin vs. numZones
		ShortestPathForest( Network * net, int * pathBuffer, double * costBuffer ) :
			numNodes(net->numNodes()), numOrigins(net->numZones()), numLinks(net->numLinks()),
			SPF((PathElement*)pathBuffer), costs((CostElement*)costBuffer) {}
		ShortestPathForest( int numNodes, int numOrigins, int numLinks, int * pathBuffer )
				: numNodes(numNodes), numOrigins(numOrigins), numLinks(numLinks), SPF((PathElement*)pathBuffer), costs(0) {}
		ShortestPathTree getSPT(int origin) const;
		PathElement *getSPTBuffer(int origin) const;
		CostElement *getCosts(int origin) const;
		int getNumNodes() const { return numNodes; }
		int getNumOrigins() const { return numOrigins; }
		int isNodeValid( int node );
};

class PathBuilder {
	private:
		Network            * net;
		ShortestPathForest   SPF;
		double             * costs;
		int			         num_origins;

		class   Heap;
		int     onePath( int origin, Heap & queue );

		double  getMaxPath();
		double  getCost(int link) const;
		double  getCost(int A, int B, int link) const;

	public:
		PathBuilder( Network * net, int * SPF_buffer, double * cost_buffer );
		~PathBuilder();

		double  setCosts( double * c ) { costs=c; return(getMaxPath()); }
		int     Paths( double * costs=0, int origins=0 );

		int     numOrigins() const { return num_origins; }

		int  numLinks() const      { return net->numLinks(); }
		int  numNodes() const      { return net->numNodes(); }
		int  numZones() const      { return net->numZones(); }
		int  firstThruNode() const { return net->firstThruNode(); }
		int  conformOrigins(int origins) { return net->conformOrigins(origins); }

		int  getFirstLinkFrom(int A)	    { return net->getFirstLinkFrom(A); }
		int  isLinkLeaving(int link, int A) { return net->isLinkLeaving(link,A); }

		int  getNode(int link) const { return net->getNode(link); }
};

enum PointerVariant { intType, doubleType };

class BaseSkimVector {
	private:
		void * vector;
		int    vecLength;
	protected:
		void * Vector() const { return vector; }
	public:
		BaseSkimVector( void *vp, int len ) : vector(vp), vecLength(len) {}
		int getLength() const { return vecLength; }
		int checkElement(int element) { return element>=0 && element<vecLength; }
};
		

template <class DataType> class SkimVector : public BaseSkimVector {
	public:
		SkimVector( DataType * dp, int dim ) : BaseSkimVector(dp,dim) {}
		SkimVector( ) : BaseSkimVector(0,0) {}
		int isAllZeroes() const;
		int whichNonZero() const;
		void setZero();

		DataType & Get(int element) const { return ((DataType *)(Vector()))[element]; }
		DataType   Set(int element, DataType value) { return ((DataType *)(Vector()))[element] = value; }
};

class DemandIndex {
	private:
		int numOrigins;
	public:
		DemandIndex( int numOrigins ) : numOrigins(numOrigins) {}
		int getOrigins() const { return numOrigins; }
		int getBufferSize() const { return numOrigins*numOrigins; }
		int getIndex(int i, int j) { return((j)*numOrigins+(i)); }
		int operator()( int i, int j ) { return getIndex(i,j); }
};

class BaseSkimMatrix {
	private:
		void        * matrix;
		DemandIndex   index;
	protected:
		void * Matrix() { return matrix; }
		int getBufferSize() const { return index.getBufferSize(); }
	public:
		BaseSkimMatrix( void * mtx, int numOrigins ) : matrix(mtx), index(numOrigins) {}
		int getIndex(int i, int j) { return index(i,j); }
		int checkIndex( int i, int j ) { return i>=0 && i<index.getOrigins() && j>=0 && j<index.getOrigins(); }
};

template <class DataType> class SkimMatrix : public BaseSkimMatrix {
	public:
		SkimMatrix( DataType * dp, int numOrigins ) : BaseSkimMatrix(dp,numOrigins) {}
		SkimMatrix( ) : BaseSkimMatrix(0,0) {}

		void setAll( DataType value ) {
			for ( int dij=0; dij<getBufferSize(); ++dij ) Set(dij,value); // TODO: allow initialization to alternate value
		}

		DataType & Get( int dij ) { return ((DataType *)(Matrix()))[dij]; }
		DataType & Get( int i, int j ) { return Get( getIndex(i,j) ); }

		DataType   Set( int dij, DataType value ) { return ((DataType *)(Matrix()))[dij]=value; }
		DataType   Set( int i, int j, DataType value ) { return Set( getIndex(i,j), value ); }
};

template <class LinkType, class DemandType> class PathWalker {
	private:
		ShortestPathForest     SPF;
		SkimVector<LinkType>   linkVector;
		SkimMatrix<DemandType> demand;

	protected:
		LinkType   & getLinkValue( int link ) { return linkVector.Get(link); }
		DemandType & getDemand(int dij) { return demand.Get(dij); }
		DemandType & getDemand(int i, int j) { return demand.Get(i,j); }

		LinkType   setLinkValue(int link, LinkType value) { return linkVector.Set(link, value); }
		DemandType setDemand(int dij, DemandType value) { return demand.Set(dij,value); }
		DemandType setDemand(int i, int j, DemandType value) { return demand.Set(i,j,value); }

		void addLinkValue(int link, LinkType value) { linkVector.Get(link)+=value; }
		void addDemand(int dij, const double & v) { demand.Get(dij) += v; }
		void addDemand(int i, int j, const double & v) { demand.Get(i,j) += v; }

	public:
		PathWalker( ShortestPathForest & SPF ) : SPF( SPF ), linkVector(), demand() {}
		PathWalker( ShortestPathForest & SPF, SkimVector<LinkType> & linkVector, SkimMatrix<DemandType> & demand ) :
			SPF( SPF ), linkVector(linkVector), demand(demand) {}

		void Load();							   // sum i-j demand onto all link values on i-j path
		void Skim( const DemandType & empty_val ); // sum all link values on i-j path into i-j demand
		void Intercept();						   // mark i-j pairs where any link vector element on i-j path is non-zero
		int  Walk( int origin, int dest, int * buffer, int bufSize );
};

#include "path_templates_R.h"

#endif  // _SHORT_PATH_R_H_
