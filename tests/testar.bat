@echo off
rem Compila e executa os testes DUnitX do DownloadThree. Uso: tests\testar.bat [Win32|Win64] (padrao Win32).
rem O codigo de saida e 0 quando todos os testes passam.
setlocal
set "PLATAFORMA=%~1"
if "%PLATAFORMA%"=="" set "PLATAFORMA=Win32"
call "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat" || exit /b 1
cd /d "%~dp0"
msbuild DownloadThreeTestes.dproj /t:Build /p:Config=Debug /p:Platform=%PLATAFORMA% /v:minimal /nologo || exit /b 1
"%PLATAFORMA%\Debug\DownloadThreeTestes.exe" --exit:Continue --consolemode:Quiet
