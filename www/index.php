<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="/"><img src="<?php echo $themeroot; ?>/images/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- own website starts here, the following may be changed as you like -->

<!-- ORIGINAL R-Forge Project Header: deemed "clunky"
<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>
-->
<h2>Welcome to the TravelR project (Travel Demand Modeling in R)!</h2>
<p>The TravelR project hosts tools for building travel demand models in R, including highway assignment with
   practical features such as multi-class equilibrium assignment, network turn penalties, select link
   analysis, and network skims.</p>

<h3>Project Status</h3>
<p> The TravelR project's first package, not suprisingly named "<strong>travelr</strong>", is in "pre-alpha" state (the code is not quite
   complete, what is there sort of works, but what works and what doesn't is changing daily and sometimes hourly).
   A very preliminary version of the package is expected to be surreptitiously released some time in May 2010, and
   announced to a close circle of co-conspiratoRs.  Subscribe to the travelr-announce list, or email the lead
   developer  through the <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/">project page</a>
   if you would like to join the inner circle.</p>

<h3>History, Motivation and Philosophy</h3>
<p> Some travel model developers and users have been using R very happily for years, but the ability to do a complete
   model in R has eluded us since there hasn't been a good way to do
   <a href="http://en.wikipedia.org/wiki/Traffic_assignment">highway assignment</a>.  The TravelR project was initiated
   to bridge that gap.  The mastermind behind TravelR is Jeremy Raw, a professional travel demand modeler (and erstwhile
   professional software developer).

   Please be aware that TravelR is a "hobby" project for which no one is currently getting paid.  Work on the project
   happens in Jeremy's (rather limited) spare time.  Please attend to the
   <a href="http://en.wikipedia.org/wiki/Yamas">Yamas</a> and
   <a href="http://en.wikipedia.org/wiki/Niyamas">Niyamas</a>
   as you contemplate this body of work.
</p>

<h3>Audience</h3>
<p> The intended audience for TravelR is two-fold:</p>
   <ul>
      <li>
	Researchers exploring travel demand modeling algorithms and methods who would like to use the
	very fast, flexible, powerful and extensible R open-source environment to speed their work
	and to share their results.
      </li>
      <li>
	Model developers who would like to build innovative and useful models in R for practical application.
      </li>
   </ul>

<h3>Design Goals</h3>
<p>
   To support these two audiences, there are two design goals:
   <dl>
      <dt>Truly Open Source</dt>
      <dd>
	 Source code is not open if it is incomprehensible.  We want the <strong>travelr</strong> code to be clear,
	 well-structured and well-documented; to use standard R approaches to data manipulation whenever possible;
	 to be fast enough so small test problems can be solved almost instantaneously; and ultimately to be
	 irresistible to researchers because you won't have to write any of the hard stuff all over again.<br/>(On the
	 other hand, we would also like to be able to get into the Scorpion pose.)
      </dd>
      <dt>Industrial-strength features</dt>
      <dd>
	 It is one thing to explore algorithms on simplified networks.  It is something else again to perform real-world
	 analyses.  Because the primary users of R for travel demand modeling are what the industry calls "practitioners"
	 (as opposed to "academics" or "researchers"), we find ourselves wanting to solve real problems with flexible,
	 powerful tools.  Certain indispensable features don't have open-source implementations at all, let alone ones
	 that might actually be practical. TravelR aims to rectify that shortcoming.
      </dd>
   </dl>
</p>
<h3>Features</h3>
<p>To meet the "industrial-strength" goal, the initial set of features include the following:</p>
<dl>
  <dt>Iterative Proportional Fitting</dt>
  <dd>
     a.k.a. 'Fratar' expansion, which grows a seed matrix toward new marginal totals, expressed either as
     absolute targets, or as fractions or percents of the current marginal totals.  Travel demand modelers love this
     function and are lost without it
  </dd>
  <dt>Highway Networks</dt>
  <dd>Allows flexible coding of highway networks so you mostly won't have to rename your link or node
     attributes, and interoperates with some other cool R packages (e.g.
     <a href="http://igraph.sourceforge.net/">igraph</a>).  As long as you can provide the network as tables that R
     can import (CSV, DBF, ODBC) and you understand which fields hold labels for the "from" and "to" nodes, you can
     make a highway network that TravelR can use
  <dt>Equilibrium Highway Assignment</dt>
  <dd>The <ital>raison d'&ecirc;tre</ital> for the package.  Supports a handful of (initially link-based) methods:
       <ul>
	  <li>All-or-Nothing</li>
	  <li>Multiple Successive Averages</li>
	  <li>Frank-Wolfe</li>
	  <li>ParTan (Parallel-Tangent)</li>
       </ul>
       The assignment driver function is extensible, so new algorithms can be added at the user level <strong>without</strong> rebuilding the package
  </dd>
  <dt>Production Support</dt>
  <dd>
     The highway assignment should be able to scale up to problems of real-world size and complexity.  Features that
     initially support that goal include:
     <dl>
	<dt>"Fast enough"</dt>
	<dd>We intend to optimize performance enough that we can model "life-size" networks in R, in our lifetime</dd>
	<dt>Multi-Class Assignment</dt>
	<dd>Supported from the ground up, allowing class-by-class network subsets, demand matrices, and cost
	   functions, in a way that makes it relatively painless to try out multi-class assignment with new
	   assignment algorithms
	</dd>
	<dt>Turn Penalties</dt>
	<dd>
	   Useful for simplifying network topology as well as applying static or dynamic path costs
	   at network junctions; turn prohibitions and penalties are implemented in path-building at the C++ level
	   using numeric cost vectors, so the evaluation of paths with penalties remains fast even if the turn
	   penalties are recomputed at each equilibrium iteration
	</dd>
	<dt>Select Link Analysis</dt>
	<dd>Extracts demand matrices and link flows from paths that intercept certain links, and is integrated into the
	   assignment algorithms so as to extract correctly weighted equilibrium flows
	</dd>
	<dt>Network Skims</dt>
	<dd>Another mainstay of travel demand modeling, "skimming" a network generates a numeric origin/destination
	   matrix by applying a function to a vector of values associated with the links along a set of network
	   paths (e.g. adding up the link lengths to get an origin-destination total path distance).  The goal is to
	   have skimming operate like R's series of "apply" functions (paths + link value vector + function ->
	   matrix, where "function" is passed a vector subset of values for each origin-destination path in turn)
     </dl>
  </dd>
</dl>
<h3>Unimplemented Features and Other Limitations</h3>
<p>Certain desirable features are still missing.</p>
<p>No path-based highway assignment.  All the implemented algorithms are link-based, which is easier and "cheaper" in
   terms of time and memory.  Someone who really comprehends the implementation of path-based assignment is invited to
   contribute data structures and algorithmic ideas.</p>
<p>No dynamic assignment.  Like path-based assignment, it's on the list but it would be helpful to get input on how to
   do it efficiently.</p>
<p>No function to load and skim Transit Networks.  The requirements for transit networks are a bit more cumbersome than
   highway networks.  We plan to get there, but a lot of that depends on time and interest (As noted above, TravelR is
   currently a "hobby" project, in that no one is explicitly getting paid to work on it and time is not always
   available).  It may also depend on getting someone involved in the project who has had a bit more practical
   experience coding and analyzing transit networks.</p>
<p>The <strong>travelr</strong> package currently does everything in memory. You need to fit your model into that
   limit, which is not as bad as it sounds:  we're currently testing four-class assignment on a network with 15,000
   links and 980 zones and coming in around 300 megabytes for a full equilibrum assignment run (that's probably a
   "mid-size" network by professional standards).</p>
<h3>Relationship to Commercial Software</h3>
<p> TravelR is not intended to be a replacement for commercial travel demand modeling software packages for most model
   developers and users.</p>
<p>To use this R package successfully, you need to know what you're doing (both in R and in
   travel demand modeling).  There is very little "canned" stuff in it, though we are putting together some detailed
   working models as examples.  You need to be able to write network cost functions (and know in detail what a network
   cost function is supposed to do), and you need to have external tools for coding your highway network.  There are no
   explicit data management or user interface tools in the project, although you can cobble together a lot from
   <a href="http://cran.r-project.org">other R packages</a>.</p>
<p>That said, it is our hope that many travel modelers will find TravelR useful and accessible enough so it is there
   for you when you need to try something new and different.</p> 
<h3>Implementation</h3>
<p> The path management code is written in C++ and the rest of the package is in R. The code is open source,
   copyrighted, and licensed under the GNU public license, version 2 or later.</p>
<h3>Further details</h3>
<p> The R-Forge <strong>project summary page</strong> is located <a href="http://<?php echo $domain; ?>/projects/<?php
   echo $group_name; ?>/"><strong>here</strong></a>. Look there for news, and eventually, the package itself.</p>

</body>
</html>
