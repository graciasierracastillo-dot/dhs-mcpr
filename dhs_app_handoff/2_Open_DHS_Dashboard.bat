@echo off
setlocal
set "ROOT=%~dp0"
set "R_EXE="

for /f "delims=" %%I in ('where R.exe 2^>nul') do if not defined R_EXE set "R_EXE=%%I"

if not defined R_EXE (
  for /f "delims=" %%I in ('dir /b /ad "%ProgramFiles%\R\R-*" 2^>nul ^| sort /r') do (
    if not defined R_EXE if exist "%ProgramFiles%\R\%%I\bin\R.exe" set "R_EXE=%ProgramFiles%\R\%%I\bin\R.exe"
  )
)

if not defined R_EXE if defined ProgramFiles(x86) (
  for /f "delims=" %%I in ('dir /b /ad "%ProgramFiles(x86)%\R\R-*" 2^>nul ^| sort /r') do (
    if not defined R_EXE if exist "%ProgramFiles(x86)%\R\%%I\bin\R.exe" set "R_EXE=%ProgramFiles(x86)%\R\%%I\bin\R.exe"
  )
)

if not defined R_EXE (
  echo Could not find R.exe.
  echo Please install R for Windows first, then run this file again.
  pause
  exit /b 1
)

cd /d "%ROOT%"
"%R_EXE%" --vanilla -f "%ROOT%launch_app_clean.R"

if errorlevel 1 (
  echo.
  echo App launch failed. If packages are missing, run 1_Install_DHS_Dashboard_Packages.bat first.
  pause
  exit /b 1
)
