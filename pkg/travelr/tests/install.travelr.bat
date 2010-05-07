@echo OFF
echo Installing package 'travelr'
set OLDDIR=%CD%
cd ..\..
R CMD INSTALL travelr --no-data
cd %OLDDIR%
