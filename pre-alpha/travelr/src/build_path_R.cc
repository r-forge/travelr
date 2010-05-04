/*
 *  travelr/src/build_path_R.cc by Jeremy Raw  Copyright (C) 2010
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

// This file contains most of the core functionality of the travelr path tree operations
// Some of the top level functions for building, loading, skimming, walking and intercepting networks are maintained as
// C++ templates in the file "path_templates_R.h", which is included via "build_path_R.h".  The decision to use
// templates was made in order to generalize code for handling vectors and matrices of various types

#include "build_path_R.h"
#include "elements_R.h"

#ifdef DEBUG_TRAVELR
#include <Rinternals.h>
#include <time.h>

// Debugging Helpers

void OffsetList::Dump() const {
	Rprintf(" %d->links(%d,%d), turns(%d,%d)\n",A,start,end,turnstart,turnend);
}

void TurnList::Dump(double * penalties) const {
	Rprintf(" %d: from(%d,%d)->to(%d,%d): %f\n",B,parent,B,B,child,penalties[p]);
}
#endif

// Inline helper functions for ShortestPathTree

int ShortestPathTree::isValid( int node ) const
{
	return(!(node>=numNodes||node<0||SPT[node].Link()>=numLinks||SPT[node].Link()<0));
}
int ShortestPathTree::Parent( int node ) const { return SPT[node].Parent(); }
int ShortestPathTree::Link( int node ) const { return SPT[node].Link(); }

// Inline helper functions for ShortestPathForest

ShortestPathTree ShortestPathForest::getSPT(int origin) const {
	return ShortestPathTree(numNodes,numLinks,SPF+(origin*numNodes));
}
PathElement * ShortestPathForest::getSPTBuffer(int origin) const { return SPF+(origin*numNodes); }
CostElement * ShortestPathForest::getCosts(int origin) const { return costs ? costs+(origin*numNodes) : 0; }

// Network Class

Network::Network(EdgeList * links, OffsetList * stars, TurnList * turns, double * penalties,
				 int linkCount, int nodeCount, int zoneCount, int firstThruNode )
: links(links), stars(stars), turns(turns), penalties(penalties),
  link_ct(linkCount), node_ct(nodeCount), zone_ct(zoneCount), thru_node(firstThruNode)
{
	if ( !links || !stars || !link_ct || !node_ct )
		return;  // TODO: error checking
	if ( thru_node > (zone_ct+1) ) thru_node = zone_ct+1;
		// Through nodes must be non-terminal
}

Network::~Network() {
	// No internally managed memory
}

int Network::conformOrigins(int num_origins) {
	if ( num_origins==0 || num_origins>zone_ct ) {
		if (zone_ct)
			num_origins = zone_ct;
		else {
			return 1;
		}
	}
	return num_origins;
}

inline double Network::turnPenalty( int A, int B, int C ) {
	int turn = stars[B].TurnStart();
	if ( turn && turns ) {
		for ( int tp=turn; tp<=stars[B].TurnEnd(); ++tp ) {
			if (turns[tp].Matches(A,C)) {
				double penalty = penalties ? turns[tp].Penalty(penalties) : -1.0;
#ifdef DEBUG_TRAVELR_PATH
				Rprintf("Found penalty %d->%d-%d: %f\n",A,B,C);
#endif
			return penalty;
			}
		}
	}
	return 0.0;
}

// Heap-based priority queue class

// The Candidate structure stores priority queue elements so we can identify the shortest available
// path from the set of links that have been explored.

class Candidate {
	private:
		int B;							// next node
		double cost;						// total weight from origin to B
	public:
		Candidate() : B(NO_PATH_ELEMENT), cost(0.0) {}
		double Cost() const { return cost; }
		int Node() const { return B; }
		double Set( int newB, double newCost ) { B=newB, cost=newCost; return cost; }
};

class PathBuilder::Heap {
	private:
		int capacity;
		int size;
		Candidate **heap;
		Candidate  *candidates;
		static Candidate Null_Element;
	public:
		Heap(int capacity);
//		~Heap();
		void Clear() { size=0; }
		int isEmpty() const { return size==0; }
		int getSize() const { return size; }
		int getCapacity() const { return capacity; }
		Candidate * Pool() { return candidates; }
		int Push( Candidate * candidate );
		Candidate * Pop();
};

Candidate PathBuilder::Heap::Null_Element;  // Has lowest possible cost (0.0)

PathBuilder::Heap::Heap( int capacity ) : capacity(capacity), size(0), heap(0), candidates(0) {
//  Changed to use safer memory allocation (so R error trapping can occur safely)
	candidates = (Candidate *) R_alloc(capacity, sizeof(Candidate));
	heap = (Candidate **) R_alloc(capacity+1, sizeof(Candidate*));
// 	candidates = new Candidate[ capacity ];
// 	heap = new Candidate * [ capacity + 1 ];
	heap[ 0 ] = &Null_Element;  // Makes the math much easier!
}

// PathBuilder::Heap::~Heap() {
//  Changed for use with R_alloc -- memory will be freed when R .Call returns
// 	delete [] candidates;
// 	delete [] heap;
// }

int PathBuilder::Heap::Push(Candidate * candidate) {
	int i = ++size;  // Set available position to new end of heap
	if (i==capacity) return -capacity;				 // Check for overflow
	while (heap[i/2]->Cost() > candidate->Cost()) {  // if parent of available position is worse than candidate
		heap[i] = heap[i/2];						 // move parent down into available position
		i/=2;										 // change available position up to former parent position
	}												 // leave loop when parent is less costly than candidate (null element always is)
	heap[i] = candidate;							 // install candidate in available position
	return 0;
}

Candidate * PathBuilder::Heap::Pop() {

	if (isEmpty())
		return heap[0];

	Candidate *min  = heap[1];						 // Extract best available node from head of queue
	Candidate *last = heap[size--];					 // Reduce heap size by orphaning the last element

	int i = 1;										 // Set available position to front of heap (former 'min')
	for ( int child=i*2;							 // Find first child
		  child<=size;								 // Check if it's in the heap (less or equal to size)
		  i=child, child=i*2) {						 // Still looking? Set available position to best child's former
													 // position and find first child of that new available position
		if (child!=size &&							 // Is child less than size?
			  heap[child]->Cost() > heap[child+1]->Cost() ) // And is first child worse than second child?
			child++;								 // If so, set child to second (better) child
		if ( last->Cost() > heap[child]->Cost() )	 // Is orphaned last element worse than current (best) child?
			heap[i] = heap[child];					 // If so, promote best child to available position
		else										 // If not (orphan is better than current best child)
			break;									 // orphan is better than best child, so it deserves the available position
	}
	heap[i] = last;									 // Place orphan in available position
	return min;
}

// PathBuilder class definition

PathBuilder::PathBuilder( Network * net, int * spf, double * costp ) :
	net(net), SPF(net, spf, costp), costs(0), num_origins(net->numZones())
{
}

PathBuilder::~PathBuilder() {
}

double PathBuilder::getMaxPath() {
	double maxpath=0.0;
	int i = numLinks()-1;
	maxpath = 0.0;
	if (costs) {
		do {
			maxpath += costs[i];
		} while (--i>0);
	} else {
		maxpath = double(numLinks()); // default cost is 1.0
	}
	return maxpath;
}

inline double PathBuilder::getCost(int A, int B, int link) const {
	double linkcost = getCost(link);
	double penalty = net->turnPenalty(A,B,getNode(link));
	return penalty<0.0 ? penalty : linkcost+penalty;
}

inline double PathBuilder::getCost(int link) const {
	return costs ? costs[link] : 1.0;
}

int PathBuilder::onePath( int origin, Heap & queue ) {

	// initialize structures for this path
	int A = origin;
	int zonesRemaining = numZones() - 1; // Already found the origin
	PathElement * SPT = SPF.getSPTBuffer(origin);
	CostElement * SPC = SPF.getCosts(origin);
	if (!SPC)
		return zonesRemaining;

	queue.Clear();
	Candidate * curr = queue.Pool();

	// Root the shortest path tree, and initialize the priority queue
	A = origin;
	SPT[A].SetParent( A, -1 );
	SPC[A].SetCost(0.0);
	SPT[A].Allocate();
#ifdef DEBUG_TRAVELR_PATH
	Rprintf("Allocating Origin %d\n",A);
#endif

	// Find links departing the origin
	for ( int link = getFirstLinkFrom(A); isLinkLeaving(link,A); ++link ) {
		int B = getNode(link);
		SPT[B].SetParent( A, link );
		SPC[B].SetCost( curr->Set(B,getCost(link)) );
#ifdef DEBUG_TRAVELR_PATH
		Rprintf("Candidate node/link: %d->%d/%d\n",A,B,link);
#endif
		int test=queue.Push( curr++ ); if (test<0) return(test);
	}

	// Look for remaining destinations
	// Note that for highway assignment applications, we stop short of a spanning tree
	// -- we only span the "zones" (terminal nodes)
	while ( zonesRemaining>0 ) {
		if ( !SPT[A].isAllocated() ) {			// Screwed up algorithm: should throw something!
#ifdef DEBUG_TRAVELR_PATH
			Rprintf("Parent node is not allocated!\n");
#endif
			break;
		}
		if ( A>=firstThruNode() ) {  // typically zones are not through nodes, but they could be
			// find links leaving A, but only if A is a through node
#ifdef DEBUG_TRAVELR_PATH
			Rprintf("Leaving %d: ",A);
#endif
			for ( int link = getFirstLinkFrom(A); isLinkLeaving(link,A); ++link ) {
				int B = getNode(link);
				if ( !SPT[B].isAllocated() ) {
					// Still seeking a path to B
					// Include turn penalties through A to B
					double linkcost = getCost(SPT[A].Parent(),A,link);
					if (linkcost>=0.0) {
						double newcost = linkcost + SPC[A].Cost();
						if ( newcost < SPC[B].Cost() ) {
							// This path is better than any we have currently found
							SPT[B].SetParent( A, link );
							SPC[B].SetCost( curr->Set(B,newcost) );
							int test=queue.Push( curr++ ); if (test<0) return(test);
#ifdef DEBUG_TRAVELR_PATH
							Rprintf("%dC(%d/%.2f) ",B,link,linkcost);
#endif
						}
#ifdef DEBUG_TRAVELR_PATH
						else {
							Rprintf("%d(%d/%.2f) ",B,link,linkcost);
						}
#endif
					}
// #ifdef DEBUG_TRAVELR_PATH
//  					else {
// 						Rprintf("Turn prohibition: %d->%d->%d\n",SPT[A].Parent(),A,B);
// 					}
// #endif
				}
#ifdef DEBUG_TRAVELR_PATH
				Rprintf("%dA ",B);
#endif
			}
#ifdef DEBUG_TRAVELR_PATH
			Rprintf("\n");
#endif
		}
		A = queue.Pop()->Node();				// Find next A (pivot) candidate
		while ( (A != NO_PATH_ELEMENT ) &&		// ensure we haven't hit the end of the queue
				(SPT[A].isAllocated()) ) {      // and that a better path to A has not already been found
#ifdef DEBUG_TRAVELR_PATH
			if (SPT[A].isAllocated()) Rprintf("Already allocated %d\n",A);
#endif
			A = queue.Pop()->Node();			// Since that A failed, try again
		}
		if ( A==NO_PATH_ELEMENT )				// got to bottom of the queue with no unallocated element
		{
#ifdef DEBUG_TRAVELR_PATH
			Rprintf("Out of candidates!  Network is probably unconnected.\n");
#endif
			break;								// so we're done with some zones inaccessible
		}
		SPT[A].Allocate();						// Mark the new A node as allocated
		if ( A<numZones() ) {					// if we found a zone, count down toward completion
			--zonesRemaining;
#ifdef DEBUG_TRAVELR_PATH
			Rprintf("Allocating %d->%d (Link %d) %d Remaining\n",SPT[A].Parent(),A,SPT[A].Link(),zonesRemaining);
#endif
		}
	}
	return zonesRemaining;	// 0 if we found them all, if positive then how many we didn't find
	                        // if negative, then some setup or runtime error
}

int PathBuilder::Paths( double * costs, int origins_sought ) {

	int result = 0;

	num_origins = conformOrigins(origins_sought);
	if ( num_origins==0 )
		return -1;
	double maxpath = setCosts(costs);
	Heap queue(numLinks()/2);

#ifdef DEBUG_TRAVELR_PATH
	clock_t clock0,clock1;
	Rprintf("Working through %d origins (Start = %ld)\n",num_origins,clock0=clock());
#endif
	for ( int z = 0; z<num_origins; ++z ) {
#ifdef DEBUG_TRAVELR_PATH
		Rprintf("Origin: %d\n",z);
#endif
		CostElement * SPC = SPF.getCosts(z);
		for (int p=0; p<numNodes(); ++p) {
			SPC[p].SetCost(maxpath);
		}
		if ( (result=onePath ( z, queue ))!=0 ) {
#ifdef DEBUG_TRAVELR_PATH
			if ( result>0 )
				Rprintf("Origin: %d - failed to find %d destination%s\n",z,result,(result>1?"s":""));
			else
				Rprintf("Path building internal failure: %d - check heap size and capacity (%d out of %d)\n",result,queue.getSize(), queue.getCapacity());
#endif
			break;
		}
	}
#ifdef DEBUG_TRAVELR_PATH
 	Rprintf("Done with %d origins (End = %ld)\n",num_origins,clock1=clock());
 	Rprintf("About %.4f seconds (%ld clocks per second)\n",(double(clock1)-double(clock0))/double(CLOCKS_PER_SEC),CLOCKS_PER_SEC);
#endif
	return result;
}
