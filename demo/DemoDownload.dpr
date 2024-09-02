program DemoDownload;

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Vcl.Forms,
  uTest in 'uTest.pas' {FrmTest},
  Downloads.Interfaces in '..\src\Downloads.Interfaces.pas',
  Downloads.HTTPClient in '..\src\Downloads.HTTPClient.pas',
  Downloads.Indy in '..\src\Downloads.Indy.pas',
  Downloads.WinInet in '..\src\Downloads.WinInet.pas',
  Downloads.Three in '..\src\Downloads.Three.pas',
  Dialogs.FormWait in 'Dialogs.FormWait.pas' {FormFundo},
  Downloads.Language in '..\src\Downloads.Language.pas';

{$R *.res}

begin
  StartLeakChecking();
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmTest, FrmTest);
  Application.Run;
end.
