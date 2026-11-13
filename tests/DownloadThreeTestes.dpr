program DownloadThreeTestes;

// Testes automatizados do DownloadThree (DUnitX), com um servidor HTTP local (sem depender da internet).
// Linha de comando: DownloadThreeTestes.exe [--exit:Pause] [--xml:resultado.xml]
// Pela linha de comando: tests\testar.bat [Win32|Win64]

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}
{$STRONGLINKTYPES ON}
uses
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ELSE}
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  {$ENDIF}
  DUnitX.TestFramework,
  Testes.ServidorLocal in 'Testes.ServidorLocal.pas',
  Testes.Downloads in 'Testes.Downloads.pas';

{$IFNDEF TESTINSIGHT}
var
  LRunner: ITestRunner;
  LResultados: IRunResults;
{$ENDIF}
begin
{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
{$ELSE}
  try
    TDUnitX.CheckCommandLine;
    LRunner := TDUnitX.CreateRunner;
    LRunner.UseRTTI := True;
    LRunner.FailsOnNoAsserts := True;
    if TDUnitX.Options.ConsoleMode <> TDunitXConsoleMode.Off then
      LRunner.AddLogger(TDUnitXConsoleLogger.Create(TDUnitX.Options.ConsoleMode = TDunitXConsoleMode.Quiet));
    if TDUnitX.Options.XMLOutputFile <> '' then
      LRunner.AddLogger(TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile));

    LResultados := LRunner.Execute;
    if not LResultados.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Pressione <Enter> para sair.');
      System.Readln;
    end;
  except
    on E: Exception do
    begin
      System.Writeln(E.ClassName, ': ', E.Message);
      System.ExitCode := EXIT_ERRORS;
    end;
  end;
{$ENDIF}
end.
