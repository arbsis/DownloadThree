unit Downloads.WinInet;

interface

uses
  Winapi.Windows,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.ImageList,
  Winapi.WinInet,
  Downloads.Interfaces;

type

  TDownloadWinInet = class(TInterfacedObject, IDownload)
  private
    FFileName: String;
    FOnEnd: TIntegrationMessage;
    FOnUpdate: TDownloadOnUpdate;
    FOnError: TIntegrationMessage;

    procedure DoEnd;
    procedure DoUpdate(AContentLength: Int64; AReadCount: Int64);
  public
    function OnEnd(AValue: TIntegrationMessage): IDownload;
    function OnUpdate(AValue: TDownloadOnUpdate): IDownload;
    function OnError(AValue: TIntegrationMessage): IDownload;
    function TypeDownload: String;
    function Download(AUrl, AFile: String): IDownload;
    class function New: IDownload;
  end;

  TDownloadToolsWinInet = class(TInterfacedObject, IDownloadTools)
  public
    function SimpleDownloadFile(AUrl, AFile: String): Boolean;
    function CheckMD5File(AUrl, AFile, AMD5: String): Boolean;
    class function New: IDownloadTools;
  end;

  // returns the expected download size.  Returns -1 if one not provided
  function GetContentLength(URLHandle: HINTERNET): Int64;
  function GetContentStatusCode(URLHandle: HINTERNET): Integer;

  // True quando o WinInet está sem o TLS 1.2 (Opções da Internet > Avançadas > "Usar TLS 1.2").
  // O Windows 7 vem com ele desligado; do Windows 8 em diante, ligado. Sem ele, os downloads em https falham.
  function TLS12Desligado: Boolean;

implementation

uses
  System.Hash, System.Win.Registry, Downloads.Language;

function GetContentLength(URLHandle: HINTERNET): Int64;
var
  SBuffer: Array[1..20] of char;
  SBufferSize: DWORD;
  srv: DWORD;
begin
  srv := 0;
  // Tamanho em bytes (com 20, cabiam só 9 dígitos: arquivos acima de ~1 GB ficavam sem tamanho e o Download recusava)
  SBufferSize := SizeOf(SBuffer);
  if HttpQueryInfo(URLHandle, HTTP_QUERY_CONTENT_LENGTH, @SBuffer, SBufferSize, srv) then
  begin
    var LTexto: String;
    SetString(LTexto, PChar(@SBuffer[1]), SBufferSize div SizeOf(Char));
    Result := StrToInt64Def(LTexto, -1);
  end
  else
    Result := -1;
end;

function GetContentStatusCode(URLHandle: HINTERNET): Integer;
var
  SBuffer: Array[1..20] of char;
  SBufferSize: DWORD;
  srv: DWORD;
begin
  srv := 0;
  SBufferSize := SizeOf(SBuffer); // em bytes
  if HttpQueryInfo(URLHandle, HTTP_QUERY_STATUS_CODE, @SBuffer, SBufferSize, srv) then
  begin
    var LTexto: String;
    SetString(LTexto, PChar(@SBuffer[1]), SBufferSize div SizeOf(Char));
    Result := StrToIntDef(LTexto, -1);
  end
  else
    Result := -1;
end;

function TLS12Desligado: Boolean;
const
  CHAVE = 'Software\Microsoft\Windows\CurrentVersion\Internet Settings';
  POLITICA = 'Software\Policies\Microsoft\Windows\CurrentVersion\Internet Settings';
  SECURE_PROTOCOLS = 'SecureProtocols';
  TLS_1_2 = $800;
var
  LProtocolos: Integer;

  function Ler(ARaiz: HKEY; const ACaminho: String): Boolean;
  var
    LRegistro: TRegistry;
  begin
    Result := False;
    LRegistro := TRegistry.Create(KEY_READ);
    try
      LRegistro.RootKey := ARaiz;
      if LRegistro.OpenKeyReadOnly(ACaminho) and LRegistro.ValueExists(SECURE_PROTOCOLS) then
      begin
        LProtocolos := LRegistro.ReadInteger(SECURE_PROTOCOLS);
        Result := True;
      end;
    finally
      LRegistro.Free;
    end;
  end;

begin
  try
    // A política (GPO) vale mais que a escolha do usuário, e a do usuário mais que a da máquina.
    if Ler(HKEY_LOCAL_MACHINE, POLITICA) or Ler(HKEY_CURRENT_USER, POLITICA) or
       Ler(HKEY_CURRENT_USER, CHAVE) or Ler(HKEY_LOCAL_MACHINE, CHAVE) then
      Result := (LProtocolos and TLS_1_2) = 0
    else
      Result := not TOSVersion.Check(6, 2); // sem valor gravado: o padrão do Windows
  except
    Result := False;
  end;
end;

{ TDownloadToolsWinInet }

class function TDownloadToolsWinInet.New: IDownloadTools;
begin
  Result := Self.Create;
end;

function TDownloadToolsWinInet.SimpleDownloadFile(AUrl, AFile: String): Boolean;
const
  BufferSize = 1024;
var
  hSession, hURL: HInternet;
  Buffer: array[1..BufferSize] of Byte;
  BufferLen: DWORD;
  LArquivo: TFileStream;
  LCriou: Boolean;
begin
  // Nunca levanta exceção: em qualquer falha devolve False, e o TDownloadThreeTools tenta o próximo método.
  // Antes, sem conexão (Windows 7 sem TLS 1.2), o BlockWrite gravava um tamanho sem valor: "I/O error 1784".
  Result := False;
  LCriou := False;
  try
    hSession := InternetOpen('', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
    if hSession = nil then
      Exit;
    try
      hURL := InternetOpenURL(hSession, PChar(AUrl), nil, 0, 0, 0);
      if hURL = nil then
        Exit;
      try
        if GetContentStatusCode(hURL) >= 400 then
          Exit;

        LArquivo := TFileStream.Create(AFile, fmCreate);
        LCriou := True;
        try
          repeat
            // Só grava o que foi lido; a leitura que falha interrompe o download.
            if not InternetReadFile(hURL, @Buffer, SizeOf(Buffer), BufferLen) then
              Abort; // cai no except abaixo, que apaga o arquivo
            if BufferLen > 0 then
              LArquivo.WriteBuffer(Buffer, BufferLen);
          until BufferLen = 0;
        finally
          LArquivo.Free;
        end;
        Result := True;
      finally
        InternetCloseHandle(hURL);
      end;
    finally
      InternetCloseHandle(hSession);
    end;
  except
    Result := False;
  end;

  // Arquivo pela metade não fica no disco.
  if LCriou and (not Result) then
    DeleteFile(AFile);
end;

function TDownloadToolsWinInet.CheckMD5File(AUrl, AFile, AMD5: String): Boolean;
begin
  Result := True;
  If ((not FileExists(AFile)) or (THashMD5.GetHashStringFromFile(AFile) <> AMD5)) Then
  begin
    If FileExists(AFile) Then
      DeleteFile(AFile);
    Result := SimpleDownloadFile(AUrl, AFile);
  end;
end;

{ TDownloadWinInet }
procedure TDownloadWinInet.DoEnd;
begin
  try
    try
      if Assigned(FOnEnd) then
        FOnEnd(FFileName);
    except on E: Exception do
      if Assigned(FOnError) then
        FOnError(E.Message);
    end;
  finally
    FFileName := EmptyStr;
  end;
end;

procedure TDownloadWinInet.DoUpdate(AContentLength, AReadCount: Int64);
begin
  try
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(FOnUpdate) then
          FOnUpdate(AReadCount, AContentLength);
      end);
  except on E: Exception do
    if Assigned(FOnError) then
      FOnError(E.Message);
  end;
end;

function TDownloadWinInet.Download(AUrl, AFile: String): IDownload;
const
  BufferSize = 1024;
var
  hSession, hURL: HInternet;
  Buffer: array[1..BufferSize] of Byte;
  BufferLen: DWORD;
  LArquivo: TFileStream;
  LCriou, LBaixou: Boolean;
begin
  Result := Self;
  FFileName := AFile;
  LCriou := False;
  LBaixou := False;
  try
    hSession := InternetOpen('', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
    if hSession = nil then
      raise Exception.CreateFmt(LangNoConnection[Integer(DownloadLanguage)], [GetLastError]);
    try
      // Sem conexão: no Windows 7 sem TLS 1.2 o https não abre (ver TLS12Desligado).
      hURL := InternetOpenURL(hSession, PChar(AUrl), nil, 0, 0, 0);
      if hURL = nil then
        raise Exception.CreateFmt(LangNoConnection[Integer(DownloadLanguage)], [GetLastError]);
      try
        if GetContentStatusCode(hURL) >= 400 then
          raise Exception.Create(LangNotFound[Integer(DownloadLanguage)]);

        var LContentLength := GetContentLength(hURL);
        var LBufferTotal: Int64 := 0;
        if LContentLength < 0 then
          raise Exception.Create(LangNotFound[Integer(DownloadLanguage)]);

        LArquivo := TFileStream.Create(AFile, fmCreate);
        LCriou := True;
        try
          repeat
            // Só grava o que foi lido; a leitura que falha interrompe o download.
            if not InternetReadFile(hURL, @Buffer, SizeOf(Buffer), BufferLen) then
              raise Exception.CreateFmt(LangReadError[Integer(DownloadLanguage)], [GetLastError]);
            if BufferLen > 0 then
            begin
              LArquivo.WriteBuffer(Buffer, BufferLen);
              LBufferTotal := LBufferTotal + BufferLen;
              DoUpdate(LContentLength, LBufferTotal);
            end;
          until BufferLen = 0;
        finally
          LArquivo.Free;
        end;
        LBaixou := True;
      finally
        InternetCloseHandle(hURL);
      end;
    finally
      InternetCloseHandle(hSession);
    end;
  except on E:Exception do
    begin
      FFileName := EmptyStr;
      // Arquivo pela metade não fica no disco.
      if LCriou then
        DeleteFile(AFile);
      if Assigned(FOnError) then
        FOnError(E.Message);
    end;
  end;

  // O OnEnd só vem quando o download terminou; antes ele era chamado até depois de uma falha.
  if LBaixou then
    DoEnd;
end;

class function TDownloadWinInet.New: IDownload;
begin
  Result := Self.Create;
end;

function TDownloadWinInet.OnEnd(AValue: TIntegrationMessage): IDownload;
begin
  Result := Self;
  FOnEnd := AValue;
end;

function TDownloadWinInet.OnError(AValue: TIntegrationMessage): IDownload;
begin
  Result := Self;
  FOnError := AValue;
end;

function TDownloadWinInet.OnUpdate(AValue: TDownloadOnUpdate): IDownload;
begin
  Result := Self;
  FOnUpdate := AValue;
end;

function TDownloadWinInet.TypeDownload: String;
begin
  Result := 'WinInet';
end;

end.

