@REM Copyright (C) 2022 Sensei Bio 
@REM    Developer: Feng Feng
@REM 	version 1.0.0
@REM This script calling another one in the Scripts folder of miniconda to open the console and 
@REM 	and set up conda environment
@REM 	see the bat file for details.
@REM  Report bugs to ffeng@senseibio.com

CD C:\Users\FFeng

%windir%\System32\cmd.exe "/K" C:\Users\FFeng\AppData\Local\r-miniconda\Scripts\activate_diannShiny.bat R4seurat
