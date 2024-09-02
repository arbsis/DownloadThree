unit uTest;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  IdComponent, IdBaseComponent, IdTCPConnection, IdTCPClient, IdHTTP, Vcl.ExtCtrls,
  System.ImageList, Vcl.ImgList,

  Dialogs.FormWait,
  Downloads.Interfaces,
  Downloads.HTTPClient,
  Downloads.Indy,
  Downloads.WinInet,
  Downloads.Three;

type
  TFrmTest = class(TForm)
    Label1: TLabel;
    edtUrl: TEdit;
    Label2: TLabel;
    edtFileName: TEdit;
    ProgressBar1: TProgressBar;
    ProgressBar2: TProgressBar;
    ProgressBar3: TProgressBar;
    ProgressBar4: TProgressBar;
    btnSimpleDownloadFileInterface: TButton;
    btnDownloadWinInet: TButton;
    Button1: TButton;
    btnVerificarMD5ImagemLogoInterface: TButton;
    btnDownloadThreadInterface: TButton;
    btnThreeDownload: TButton;
    btnThreeSimple: TButton;
    btnThreeMD5: TButton;
    btnDownloadHTTPClient: TButton;
    btnDownloadIndy: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnSimpleDownloadFileInterfaceClick(Sender: TObject);
    procedure btnDownloadWinInetClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnVerificarMD5ImagemLogoInterfaceClick(Sender: TObject);
    procedure btnDownloadThreadInterfaceClick(Sender: TObject);
    procedure btnThreeDownloadClick(Sender: TObject);
    procedure btnThreeMD5Click(Sender: TObject);
    procedure btnThreeSimpleClick(Sender: TObject);
    procedure btnDownloadHTTPClientClick(Sender: TObject);
    procedure btnDownloadIndyClick(Sender: TObject);

  private
    { Private declarations }
    FDownT: IDownloadThread;
    ProgressT: array [0..3] of Integer;
    TamanhoArquivoThread: Int64;

    procedure onEndDownload(AValue: Variant);
    procedure OnErrorDownload(AValue: Variant);
    procedure onUpdateDownload(AProgress, ASize: Integer);
    procedure onStartDownload(AValue: Variant);
    procedure ReceiveThreadDataEvent(const Sender: TObject; ThreadNo, ASpeed: Integer; AContentLength: Int64; AReadCount: Int64; var Abort: Boolean);

  public
    { Public declarations }
  end;

var
  FrmTest: TFrmTest;
  Fundo: TFormWait;

implementation

{$R *.dfm}

uses
  System.DateUtils,
  System.IOUtils,
  ShellAPI, Downloads.Language;

{ TForm1 }

procedure TFrmTest.btnDownloadWinInetClick(Sender: TObject);
begin
  Fundo.Exibir(Self, True, 'Downloading file by WinInet...', True);
  TDownloadWinInet
    .New
      .OnEnd(onEndDownload)
      .OnUpdate(onUpdateDownload)
      .OnError(OnErrorDownload)
    .Download(edtUrl.Text, edtFileName.Text);
end;

procedure TFrmTest.btnDownloadThreadInterfaceClick(Sender: TObject);
begin
  ProgressT[0] := 0;
  ProgressT[1] := 0;
  ProgressT[2] := 0;
  ProgressT[3] := 0;

  Fundo.Exibir(Self, True, 'Downloading file...', True);
  FDownT :=
    TDownloadThreadHTTPClient
      .New
      .OnEnd(onEndDownload)
      .OnError(OnErrorDownload)
      .OnStart(onStartDownload)
      .OnThreadData(ReceiveThreadDataEvent)
      .Download(edtUrl.Text, edtFileName.Text);
end;

procedure TFrmTest.btnSimpleDownloadFileInterfaceClick(Sender: TObject);
begin
  Fundo.Exibir(Self, True);
  TThread.CreateAnonymousThread(
  procedure
  begin
    var LResult: Boolean;
    var LMsg: TStringList := TStringList.Create;

    Fundo.Atualizar('TDownloadToolsHTTPClient');
    LResult :=
      TDownloadToolsHTTPClient.New.SimpleDownloadFile(edtUrl.Text, edtFileName.Text);
    if LResult then
      LMsg.Add('HTTPClient Success!')
    else
      LMsg.Add('HTTPClient Error!');

    Fundo.Atualizar('TDownloadToolsIndy');
    LResult :=
      TDownloadToolsIndy.New.SimpleDownloadFile(edtUrl.Text, edtFileName.Text);
    if LResult then
      LMsg.Add('Indy Success!')
    else
      LMsg.Add('Indy Error!');

    Fundo.Atualizar('TDownloadToolsWinInet');
    LResult :=
      TDownloadToolsWinInet.New.SimpleDownloadFile(edtUrl.Text, edtFileName.Text);
    if LResult then
      LMsg.Add('WinInet Success!')
    else
      LMsg.Add('WinInet Error!');

    TThread.Synchronize(TThread.Current,
    procedure
    begin
      Fundo.Fechar;
      ShowMessage(LMsg.Text);
    end);
  end).Start;
end;

procedure TFrmTest.btnVerificarMD5ImagemLogoInterfaceClick(Sender: TObject);
begin
  var LResult: Boolean;

  LResult := TDownloadToolsWinInet.New.CheckMD5File(
    edtUrl.Text,
    edtFileName.Text + '1',
    '');
  if LResult then
    ShowMessage('WinInet Success!')
  else
    ShowMessage('WinInet Error!');

  LResult := TDownloadToolsIndy.New.CheckMD5File(
    edtUrl.Text,
    edtFileName.Text + '2',
    '');
  if LResult then
    ShowMessage('Indy Success!')
  else
    ShowMessage('Indy Error!');

  LResult := TDownloadToolsHTTPClient.New.CheckMD5File(
    edtUrl.Text,
    edtFileName.Text + '3',
    '');
  if LResult then
    ShowMessage('HTTPClient Success!')
  else
    ShowMessage('HTTPClient Error!');
end;

procedure TFrmTest.Button1Click(Sender: TObject);
begin
  ShellExecute(handle, 'explore', PChar(TDirectory.GetCurrentDirectory), '', '', SW_SHOWMAXIMIZED);
end;

procedure TFrmTest.btnDownloadHTTPClientClick(Sender: TObject);
begin
  Fundo.Exibir(Self, True, 'Downloading file by HTTPClient...', True);
  TDownloadHTTPClient
    .New
      .OnEnd(onEndDownload)
      .OnUpdate(onUpdateDownload)
      .OnError(OnErrorDownload)
    .Download(edtUrl.Text, edtFileName.Text);
end;

procedure TFrmTest.btnDownloadIndyClick(Sender: TObject);
begin
  Fundo.Exibir(Self, True, 'Downloading file by Indy...', True);
  TDownloadIndy
    .New
      .OnEnd(onEndDownload)
      .OnUpdate(onUpdateDownload)
      .OnError(OnErrorDownload)
    .Download(edtUrl.Text, edtFileName.Text);
end;

procedure TFrmTest.btnThreeDownloadClick(Sender: TObject);
begin
  Fundo.Exibir(Self, True, 'Downloading file...', True);
  var a := TDownloadThree
    .New
      .OnEnd(onEndDownload)
      .OnUpdate(onUpdateDownload)
      .OnError(OnErrorDownload)
    .Download(edtUrl.Text, edtFileName.Text);

  ShowMessage('Finish!'+ sLineBreak +'TypeDownload: ' + a.TypeDownload);
end;

procedure TFrmTest.btnThreeMD5Click(Sender: TObject);
begin
  var LResult := TDownloadThreeTools.New.CheckMD5File(
    edtUrl.Text,
    edtFileName.Text,
    '');

  if LResult then
    ShowMessage('Success!')
  else
    ShowMessage('Error!')
end;

procedure TFrmTest.btnThreeSimpleClick(Sender: TObject);
begin
  Fundo.Exibir(Self, True);
  var LResult := TDownloadThreeTools.New.SimpleDownloadFile(edtUrl.Text, edtFileName.Text);
  Fundo.Fechar;
  if LResult then
    ShowMessage('Success!')
  else
    ShowMessage('Error!');
end;

procedure TFrmTest.FormCreate(Sender: TObject);
begin
  edtUrl.Text := 'http://212.183.159.230/5MB.zip';
  edtFileName.Text := '5MB.zip';
  Fundo := TFormWait.CreateNew(Self, True);

  {If use Downloads.Language.LangLanguage for translate messages}
  Downloads.Language.DownloadLanguage := TDownloadLanguage.ptBR;
end;

procedure TFrmTest.onEndDownload(AValue: Variant);
begin
  TThread.Synchronize(nil,
  procedure
  begin
    Fundo.Fechar;
    ShowMessage('Finish: ' + AValue);
  end);
end;

procedure TFrmTest.OnErrorDownload(AValue: Variant);
begin
  Fundo.Fechar;
  ShowMessage('Erro ao baixar o arquivo.' + #13#10 + AValue);
end;

procedure TFrmTest.onStartDownload(AValue: Variant);
begin
  TamanhoArquivoThread := AValue;
end;

procedure TFrmTest.onUpdateDownload(AProgress, ASize: Integer);
begin
  Fundo.Atualizar(AProgress, ASize);
end;

procedure TFrmTest.ReceiveThreadDataEvent(const Sender: TObject; ThreadNo, ASpeed: Integer; AContentLength, AReadCount: Int64; var Abort: Boolean);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      case ThreadNo of
        0:
          begin
            ProgressBar1.Max := AContentLength;
            ProgressBar1.Position := AReadCount;
          end;
        1:
          begin
            ProgressBar2.Max := AContentLength;
            ProgressBar2.Position := AReadCount;
          end;
        2:
          begin
            ProgressBar3.Max := AContentLength;
            ProgressBar3.Position := AReadCount;
          end;
        3:
          begin
            ProgressBar4.Max := AContentLength;
            ProgressBar4.Position := AReadCount;
          end;
      end;

      ProgressT[ThreadNo] := AReadCount;
      Fundo.Atualizar(ProgressT[0] + ProgressT[1] + ProgressT[2] + ProgressT[3], TamanhoArquivoThread);
    end);
end;

end.
