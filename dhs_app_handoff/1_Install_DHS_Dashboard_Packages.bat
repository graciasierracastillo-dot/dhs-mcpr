@echo off
setlocal
set "ROOT=%~dp0"
set "RSCRIPT_EXE="

for /f "delims=" %%I in ('where Rscript.exe 2^>nul') do if not defined RSCRIPT_EXE set "RSCRIPT_EXE=%%I"

if not defined RSCRIPT_EXE (
  for /f "delims=" %%I in ('dir /b /ad "%ProgramFiles%\R\R-*" 2^>nul ^| sort /r') do (
    if not defined RSCRIPT_EXE if exist "%ProgramFiles%\R\%%I\bin\Rscript.exe" set "RSCRIPT_EXE=%ProgramFiles%\R\%%I\bin\Rscript.exe"
  )
)

if not defined RSCRIPT_EXE if defined ProgramFiles(x86) (
  for /f "delims=" %%I in ('dir /b /ad "%ProgramFiles(x86)%\R\R-*" 2^>nul ^| sort /r') do (
    if not defined RSCRIPT_EXE if exist "%ProgramFiles(x86)%\R\%%I\bin\Rscript.exe" set "RSCRIPT_EXE=%ProgramFiles(x86)%\R\%%I\bin\Rscript.exe"
  )
)

if not defined RSCRIPT_EXE (
  echo Could not find Rscript.exe.
  echo Please install R for Windows first, then run this file again.
  pause
  exit /b 1
)

cd /d "%ROOT%"
"%RSCRIPT_EXE%" --vanilla "%ROOT%install_app_packages.R"

if errorlevel 1 (
  echo.
  echo Package install failed.
  pause
  exit /b 1
)

echo.
echo Package install finished.
pause
