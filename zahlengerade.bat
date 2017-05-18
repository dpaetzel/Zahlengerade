@echo OFF

title Zahlengerade

@echo Zahlengerade: create simple number lines from yaml files

if [%1]==[] (
set /p inputfile= "Enter the path to the input file: "
) else (
set inputfile=%1
)

if ["%inputfile%"]==[""] (
@echo No input file.
pause
exit
)

@echo Generating svg file from input file %inputfile%...

zahlengerade.exe -w 1000 -o output.svg %inputfile%

@echo Done.

pause