unit Downloads.WinInet;

interface

uses
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

implementation

uses
  System.Hash, Downloads.Language;

function GetContentLength(URLHandle: HINTERNET): Int64;
var
  SBuffer: Array[1..20] of char;
  SBufferSize: DWORD;
  srv: DWORD;
begin
  srv := 0;
  SBufferSize := 20;
  if HttpQueryInfo(URLHandle, HTTP_QUERY_CONTENT_LENGTH, @SBuffer, SBufferSize, srv) then
    Result := StrToInt64(String(SBuffer))
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
  SBufferSize := 20;
  if HttpQueryInfo(URLHandle, HTTP_QUERY_STATUS_CODE, @SBuffer, SBufferSize, srv) then
    Result := StrToInt(String(SBuffer))
  else
    Result := -1;
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
  F: File;
begin
  Result := False;
  hSession := InternetOpen('', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);

  // Establish the secure connection
  InternetConnect(
    hSession,
    PChar(AUrl),
    INTERNET_DEFAULT_HTTPS_PORT,
    '',
    '',
    INTERNET_SERVICE_HTTP,
    0,
    0
  );

  try
    hURL := InternetOpenURL(hSession, PChar(AUrl), nil, 0, 0, 0);
    if GetContentStatusCode(hURL) >= 400 then
      Exit;
    try
      AssignFile(f, AFile);
      Rewrite(f,1);
      try
        repeat
          InternetReadFile(hURL, @Buffer, SizeOf(Buffer), BufferLen);
          BlockWrite(f, Buffer, BufferLen);
        until BufferLen = 0;
      finally
        CloseFile(f);
        Result := True;
      end;
    finally
      InternetCloseHandle(hURL);
    end
  finally
    InternetCloseHandle(hSession);
  end;
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
  F: File;
begin
  FFileName := AFile;
  try
    hSession := InternetOpen('', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);

    // Establish the secure connection
    InternetConnect(
      hSession,
      PChar(AUrl),
      INTERNET_DEFAULT_HTTPS_PORT,
      '',
      '',
      INTERNET_SERVICE_HTTP,
      0,
      0
    );

    try
      hURL := InternetOpenURL(hSession, PChar(AUrl), nil, 0, 0, 0);
      if GetContentStatusCode(hURL) >= 400 then
        raise Exception.Create(LangNotFound[Integer(DownloadLanguage)]);

      var LContentLength := GetContentLength(hURL);
      var LBufferTotal: Int64 := 0;
      if LContentLength < 0 then
        raise Exception.Create(LangNotFound[Integer(DownloadLanguage)]);
      try
        AssignFile(f, AFile);
        Rewrite(f,1);
        try
          repeat
            InternetReadFile(hURL, @Buffer, SizeOf(Buffer), BufferLen);
            BlockWrite(f, Buffer, BufferLen);
            LBufferTotal := LBufferTotal + BufferLen;
            DoUpdate(LContentLength, LBufferTotal);
          until BufferLen = 0;
        finally
          CloseFile(f);
          DoEnd;
        end;
      finally
        InternetCloseHandle(hURL);
      end
    finally
      InternetCloseHandle(hSession);
    end;
  except on E:Exception do
    begin
      FFileName := EmptyStr;
      if Assigned(FOnError) then
        FOnError(E.Message);
    end;
  end;
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

