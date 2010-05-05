####################################################################################################
#   travelr\r\Cost_Integrator.R by Jeremy Raw  Copyright (C) 2010
# 
#   The code for the two functions "cost.integrator" and "inner.integrator" was
#   adapted from Wikipedia.  The text and code at Wikipedia are copyrighted and
#   distributed under the Creative Commons Share-Alike License 3.0, which is
#   available at http://creativecommons.org/licenses/by-sa/3.0/
#
#   The Wikipedia code is additionally available (as the base code had not been
#   modified at Wikipedia since before June, 2009) under the terms of the GNU
#   Free Documentation License, available at
#   http://en.wikipedia.org/wiki/Wikipedia:GFDL
#
#   Redistributing this code under the GNU General Public License, version 2.0
#   or later is likely to be unproblematic (YMMV, IANAL).  Should additional
#   permissions or prohibitions be placed on the code under one of the above
#   licenses, the genesis of the code implies that the above licenses will take
#   precedence.  It is recommended that this code be maintained in a separate
#   source code file, with this license text and copyright information intact.
#
####################################################################################################

# The following comment block reproduces code from Wikipedia, accessed May 4, 2010
# http://en.wikipedia.org/wiki/Adaptive_Simpson%27s_method
#
# From Wikipedia:
# --------------------------------------------------------------------------------
# Here is an implementation of the adaptive Simpson's method in C99 that avoids
# redundant evaluations of f and quadrature computations, but also suffers from
# memory runaway.  Suggested improvement would be to use a for loop in
# adaptiveSimpsonsAux:
#
# #include <math.h>  // include file for fabs and sin
# #include <stdio.h> // include file for printf
#  
# //
# // Recursive auxiliary function for adaptiveSimpsons() function below
# //                                                                                                 
# double adaptiveSimpsonsAux(double (*f)(double), double a, double b, double epsilon,                 
#                          double S, double fa, double fb, double fc, int bottom) {                 
#   double c = (a + b)/2, h = b - a;                                                                  
#   double d = (a + c)/2, e = (c + b)/2;                                                              
#   double fd = f(d), fe = f(e);                                                                      
#   double Sleft = (h/12)*(fa + 4*fd + fc);                                                           
#   double Sright = (h/12)*(fc + 4*fe + fb);                                                          
#   double S2 = Sleft + Sright;                                                                       
#   if (bottom <= 0 || fabs(S2 - S) <= 15*epsilon)                                                    
#     return S2 + (S2 - S)/15;                                                                        
#   return adaptiveSimpsonsAux(f, a, c, epsilon/2, Sleft,  fa, fc, fd, bottom-1) +                    
#          adaptiveSimpsonsAux(f, c, b, epsilon/2, Sright, fc, fb, fe, bottom-1);                     
# }         
#  
# //
# // Adaptive Simpson's Rule
# //
# double adaptiveSimpsons(double (*f)(double),   // ptr to function
#                            double a, double b,  // interval [a,b]
#                            double epsilon,  // error tolerance
#                            int maxRecursionDepth) {   // recursion cap        
#   double c = (a + b)/2, h = b - a;                                                                  
#   double fa = f(a), fb = f(b), fc = f(c);                                                           
#   double S = (h/6)*(fa + 4*fc + fb);                                                                
#   return adaptiveSimpsonsAux(f, a, b, epsilon, S, fa, fb, fc, maxRecursionDepth);                   
# }                                                                                                   
#  
#  
# int main(){
#  double I = adaptiveSimpsons(sin, 0, 1, 0.000000001, 10); // compute integral of sin(x)
#                                                           // from 0 to 1 and store it in 
#                                                           // the new variable I
#  printf("I = %lf\n",I); // print the result
#  return 0;
# }
# -----------------------------------------------------------------------------------
# End of Wikipedia section
#
# The "runaway memory" problem could potentially be an issue in the R code, but
# the Wikipedia-suggested solution of unrolling the recursion into a loop is
# overkill if the cost function is well-behaved.  Time and experience will tell
# if we need to revisit this.

# Note: cost.integrator can handle the special requirement that the volume and
# cost parameters be vectors (and that the integral of the whole be the sum of
# the integrals of each element from "free flow" volume (0) to the corresponding
# element in "congested" volume).  This function should also handle everything
# happily even if ff.vol and cong.vol are data.frames or matrices.

# The standard R 'integrate' function can't do any of that...

cost.integrator <- function( cost.function, ff.vol, ff.cost, cong.vol, cong.cost, tol=1e-8, max.depth=14 ) {
	# Integrate the cost function using adaptive quadrature with inside-interval tolerance testing
	ivl      <- (cong.vol-ff.vol)/2
	mid.vol  <- ff.vol+ivl
	mid.cost <- cost.function(mid.vol)
	result   <- (ivl/3)*(ff.cost+4*mid.cost+cong.cost)
	inner.integrator( cost.function, ff.vol, ff.cost, mid.vol, mid.cost, cong.vol, cong.cost, result, max.depth, tol )
}

inner.integrator <- function( cost.function, ff.vol,ff.cost,mid.vol,mid.cost,cong.vol,cong.cost,result,depth, tol) {
	ivl         <- (cong.vol-ff.vol)/4
	left.vol    <- ff.vol + ivl
	left.cost   <- cost.function(left.vol)
	right.vol   <- cong.vol - ivl
	right.cost  <- cost.function(right.vol)
	h.6         <- ivl/3
	result.left <- sum(h.6*(ff.cost+4*left.cost+mid.cost))
	result.right<- sum(h.6*(mid.cost+4*right.cost+cong.cost))
	result.2    <- result.left + result.right
	if ( depth<=0 || (abs(result.2-result)<=(tol*15)) ) { # if splitting doesn't improve result, return best guess
		result  <- result.2 + (result.2-result)/15
	} else {  # otherwise, process each half recursively seeking a better fit
		result <- Recall(cost.function,ff.vol,ff.cost,left.vol,left.cost,mid.vol,mid.cost,result.left,depth-1,tol/2)+
				  Recall(cost.function,mid.vol,mid.cost,right.vol,right.cost,cong.vol,cong.cost,result.right,depth-1,tol/2)
	}
	result
}
