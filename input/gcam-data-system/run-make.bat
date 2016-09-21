@echo off
REM Note the path below must be modified to match locations on your machine
REM The first path referencing GnuWin32 should be changed to the correct needed-exes\bin directory
REM The second path for R should be double checked to make sure that it is pointing to the correct version
SET PATH=C:\Program Files\Java\jdk1.7.0_79\bin;%PATH%;E:\GCAM\needed-exes\bin;C:\Program Files\R\R-3.1.1\bin\x64

REM By default we will run "make all" however by changing CMD=all to CMD=clean it will clean
REM the data system instead which i useful to do for instance when you update the data system
IF [%1]==[] ( SET CMD=xml ) ELSE ( set CMD=%1 )

make %CMD%

pause