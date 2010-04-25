This branch of the repository works something like 'pkg' except that it is not
automatically built.  You can checkout or export the package sources and use
the batch files to build, locally install, and test the package.  I'll keep the
latest "working" built library for binary installation in the pre-alpha root.

To build or install from source, you'll need the Rtools (see "Writing R extensions"
for detailed information on what's in that package, where to get it, and how to
install it).

Development (as of 4/25/2010) has been done with R 2.10.1 (even though 2.11.0 has
just been released) and a consistent Rtools package, and the package should
build with those tools.

Feel free to check in changes (the SVN "blame" function is handy, and I reserve
the right to undo things that are not in the spirit of R or TravelR -- if I do
so, I'll explain in detail).

Enjoy!
Jeremy  (jeremy dot raw at earthlink dot net)
