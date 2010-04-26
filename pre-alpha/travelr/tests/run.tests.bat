@echo OFF
rem Short script to run tests from within uninstalled source tree
rem Goal is to streamline the "build" and "check" functionality
rem during development
rem echo Removing data files
rem del /f /q data\*
echo Installing package 'travelr'
set OLDDIR=%CD%
cd ..\..
R CMD INSTALL travelr --no-data
cd %OLDDIR%
echo Running tests
@echo ON
if not exist data\SiouxFalls.rda       Rscript Test_01_Highway_Network.R
if not exist data\SiouxFallsAset.Rdata Rscript Test_02_Assignment_Classes.R
if exist data\SiouxFalls.rda if exist data\SiouxFallsAset.Rdata Rscript Test_03_Highway_Assignment.R
Rscript Test_04_ipf.R
