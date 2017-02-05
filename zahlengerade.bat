@echo OFF

title Zahlengerade

@echo Zahlengerade: create simple number lines from yaml files

if not defined 1 (
set /p inputfile= "Enter the path to the input file: "
) else (
inputfile=%1
)

if ["%inputfile%"]==[""] (
@echo No input file.
pause
exit
)

@echo Generating svg file from input file %inputfile%...

start zahlengerade.exe -w 1000 -o output.svg %inputfile%

@echo Done.

pause