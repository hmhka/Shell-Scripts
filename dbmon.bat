@echo off
@if not "%OS%"=="Windows_NT" goto :END
 
rem ****************************************************************
rem dbmon - Gather promon statistics
rem Created: March 19, 2010 by George Potemkin
rem Last Modified: 
rem ****************************************************************

 
rem ****************************************************************
rem Variable definitions
rem ****************************************************************

set DLC=C:\dlc10.2B
set InputFile=D:\Support\spar.nnov.ru\dbmon.input.txt

set DbPath=%1

rem ****************************************************************
rem Sanity checks
rem ****************************************************************

if exist %DLC%\bin\_mprshut.exe goto :DbPathCheck
echo DLC=%DLC% >%ErrorLog%
echo ERROR: DLC environment variable not set correctly.
goto :EXIT

:DbPathCheck
if exist %DbPath%.db goto :InputFileCheck
echo ERROR: Database %DbPath%.db does not exist.
goto :EXIT

:InputFileCheck
if exist %InputFile% goto :MainBlock
echo DbPath=%DbPath%
echo ERROR: Input file %InputFile% not found.
goto :EXIT


:MainBlock

rem Set DbName (without path)
for /F %%i in (' echo %DbPath%') do set DbName=%%~nxi

rem set LastName=%DbPath%
rem :DbName
rem set DbName=%LastName%
rem for /F "delims=\ tokens=1*" %%i in ('echo %DbName%') do set LastName=%%j
rem if NOT %LastName%Empty==Empty goto :DbName
rem goto :FileName

call :CurrDateTimeId

set OutputFile=%DbName%.promon.%ID%.log
"%DLC%\bin\_mprshut" -0 -NL %DbPath% < %InputFile% > %OutputFile%

goto :EXIT

rem ****************************************************************
rem  DbName - set DbName variable 
rem ****************************************************************
:DbName
set DbName=
echo DbPath=%DbPath%
for /F "delims=\" %%i in ('echo %DbPath%') do echo %%i

rem DbName==%%i

goto :END


rem ****************************************************************
rem  CurrDateTimeId - set ID variable as YYYYMMDDHHMMSS
rem ****************************************************************
:CurrDateTimeId

set ID=
call :CurrDateTime

rem CurrDate:
rem for /F "tokens=2" %%i in ('date /T') do set CurrDate=%%i

rem Year:
for /F "delims=./ tokens=3" %%i in ('echo %CurrDate%') do set ID=%%i

rem Month:
for /F "delims=./ tokens=1" %%i in ('echo %CurrDate%') do set ID=%ID%%%i

rem Day:
for /F "delims=./ tokens=2" %%i in ('echo %CurrDate%') do set ID=%ID%%%i


rem CurrTime:
rem for /F "delims=#" %%i in ('time /T') do set CurrTime=%%i

rem Hours:
set HH=
for /F "delims=:ap tokens=1" %%i in ('echo %CurrTime%') do set HH=%%i

rem Minutes:
set MM=
for /F "delims=:ap tokens=2" %%i in ('echo %CurrTime%') do set MM=%%i

rem Seconds:
set SS=
for /F "delims=:ap tokens=3" %%i in ('echo %CurrTime%') do set SS=%%i

rem -- Ante or Post Meridiem --
set TT=
for /F "delims=:0123456789m" %%i in ('echo %CurrTime%') do set TT="%%i"

if %TT%Empty==Empty goto :SetID

if %TT%m==pm goto :PostMeridiem

rem Ante Meridiem or Military Time
rem ------------------------------
if %HH%==0  set HH=00
if %HH%==1  set HH=01
if %HH%==2  set HH=02
if %HH%==3  set HH=03
if %HH%==4  set HH=04
if %HH%==5  set HH=05
if %HH%==6  set HH=06
if %HH%==7  set HH=07
if %HH%==8  set HH=08
if %HH%==9  set HH=09

goto :SetID

rem Post Meridiem
rem -------------
:PostMeridiem

if %HH%==1  set HH=13
if %HH%==2  set HH=14
if %HH%==3  set HH=15
if %HH%==4  set HH=16
if %HH%==5  set HH=17
if %HH%==6  set HH=18
if %HH%==7  set HH=19
if %HH%==8  set HH=20
if %HH%==9  set HH=21
if %HH%==10 set HH=22
if %HH%==11 set HH=23

:SetID
set ID=%ID%_%HH%%MM%%SS%
set HH=
set MM=
set SS=
set TT=

goto :END


rem ****************************************************************
rem  CurrDateTime - Set CurrDateTime variable with current date and time.
rem ****************************************************************
:CurrDateTime

rem Current Date:
for /F "tokens=2" %%i in ('date /T') do set CurrDate=%%i

rem Current Time:
rem set TempFile=%TEMP%\currtime.tmp
rem echo.|time >%TempFile%
rem for /F "tokens=5" %%i in ('type %TempFile%') do set CurrTime=%%i
rem del %TempFile%
rem set TempFile=

for /F "delims=., tokens=1" %%i in ('echo %TIME%') do set CurrTime=%%i
for /F "tokens=1" %%i in ('echo %CurrTime%') do set CurrTime=%%i

goto :END


rem ****************************************************************
rem  Error - exit on error
rem ****************************************************************
:EXIT

echo .
echo .
echo .
echo .
echo .
echo .
echo .
echo .
echo .
echo See results in %OutputFile%

rem ****************************************************************
rem Just cleanup
rem ****************************************************************
set ID=
set CurrDate=
set CurrTime=
set DbPath=
set OutputFile=

goto :END

rem ****************************************************************
rem  END - end of call.
rem ****************************************************************
:END

