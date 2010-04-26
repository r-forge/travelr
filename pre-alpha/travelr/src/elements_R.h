/*
 *  travelr/src/elements_R.cc by Jeremy Raw  Copyright (C) 2010
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

#ifndef _ELEMENTS_R_H_
#define _ELEMENTS_R_H_

// PathElement

// The path element holds data for a node label, indicating the parent node (with a flag to say if
// the connection to the parent has been allocated (or is simply pending -- we cache the best
// parent discovered so far for this node, and simply mark it allocated when it shows up as the
// best overall.

class PathElement {
	private:
		int parent;				// left bit is 0 if allocated, otherwise 1; rest is parent node index
		int link;					// link associated with move from parent to this element
	public:
		void Allocate();
		int  isAllocated() const;
		void SetParent(int newParent, int newLink);
		int  Parent() const;
		int  Link() const;
};

#define PATH_ELEMENT_UNALLOCATED (1<<((sizeof(int)*8)-1))
#define PATH_ELEMENT_ALLOCATED   (~(PATH_ELEMENT_UNALLOCATED))
#define NO_PATH_ELEMENT          (int(PATH_ELEMENT_UNALLOCATED|PATH_ELEMENT_ALLOCATED))
#define IS_UNALLOCATED(x)        (((x)&PATH_ELEMENT_UNALLOCATED)>0)
#define MARK_UNALLOCATED(x)      ((x)|PATH_ELEMENT_UNALLOCATED)
#define IS_ALLOCATED(x)          (((x)&PATH_ELEMENT_UNALLOCATED)==0)
#define MARK_ALLOCATED(x)        ((x)&PATH_ELEMENT_ALLOCATED)

inline void PathElement::Allocate() { parent=MARK_ALLOCATED(parent); }
inline int PathElement::isAllocated() const { return IS_ALLOCATED(parent);	}
inline void PathElement::SetParent(int newParent, int newLink) { parent=MARK_UNALLOCATED(newParent), link=newLink; }
inline int PathElement::Parent() const { return parent; }
inline int PathElement::Link() const { return link; }

// CostElement
// Provides functions for managing link costs during path building

class CostElement {
	private:
		double   cost;					// distance from origin to this node
	public:
		void SetCost(double newCost=0.0);
		double Cost() const;
};

inline void CostElement::SetCost(double newCost) { cost=newCost; }
inline double CostElement::Cost() const { return cost; }

#endif
