@ECHO off

setlocal
set PATH=C:\Program Files\R\R-3.0.1\bin


Rscript.exe G:\MaybergShare\DBS_GPS_Pedometer\src\main.R

echo --------------------------------------------------------
echo R Program has Finished
echo --------------------------------------------------------
pause
exit