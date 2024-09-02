unit Downloads.Indy;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.ImageList,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP, IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL,
  Downloads.Interfaces;

type

  TDownloadIndy = class(TInterfacedObject, IDownload)
  private
    FIdSSL: TIdSSLIOHandlerSocketOpenSSL;
    FHttp: TIdHTTP;
    FDownloadStream: TMemoryStream;
    FFileName: String;
    FWorkCountMax: Int64;

    FOnEnd: TIntegrationMessage;
    FOnUpdate: TDownloadOnUpdate;
    FOnError: TIntegrationMessage;

    constructor Create;
    procedure DoEnd(ASender: TObject; AWorkMode: TWorkMode);
    procedure DoUpdate(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
    procedure DoBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
  public
    destructor Destroy; override;
    function OnEnd(AValue: TIntegrationMessage): IDownload;
    function OnUpdate(AValue: TDownloadOnUpdate): IDownload;
    function OnError(AValue: TIntegrationMessage): IDownload;
    function TypeDownload: String;
    function Download(AUrl, AFile: String): IDownload;
    class function New: IDownload;
  end;

  TDownloadToolsIndy = class(TInterfacedObject, IDownloadTools)
  public
    function SimpleDownloadFile(AUrl, AFile: String): Boolean;
    function CheckMD5File(AUrl, AFile, AMD5: String): Boolean;
    class function New: IDownloadTools;
  end;

implementation

uses
  System.Hash, Downloads.Language;

{ TDownloadToolsIndy }

class function TDownloadToolsIndy.New: IDownloadTools;
begin
  Result := Self.Create;
end;

function TDownloadToolsIndy.SimpleDownloadFile(AUrl, AFile: String): Boolean;
begin
  Result := False;
  var LIdSSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  var LHttp := TIdHTTP.Create(nil);
  var LFile := TMemoryStream.Create;
  try
    LIdSSL.SSLOptions.Method := sslvTLSv1_2;
    LHttp.IOHandler := LIdSSL;
    try
      LHttp.Get(AUrl, LFile);
      LFile.SaveToFile(AFile);
      Result := True;
    except on E:Exception do
      begin
        if LHttp.GetHashCode >= 400 then
          Exit;
      end;
    end;
  finally
    LIdSSL.Free;
    LHttp.Free;
    LFile.Free;
  end;
end;

function TDownloadToolsIndy.CheckMD5File(AUrl, AFile, AMD5: String): Boolean;
begin
  Result := True;
  If ((not FileExists(AFile)) or (THashMD5.GetHashStringFromFile(AFile) <> AMD5)) Then
  begin
    If FileExists(AFile) Then
      DeleteFile(AFile);
    Result := SimpleDownloadFile(AUrl, AFile);
  end;
end;

{ TDownloadIndy }

constructor TDownloadIndy.Create;
begin
  FIdSSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  FIdSSL.SSLOptions.Method := sslvTLSv1_2;
  FHttp := TIdHTTP.Create(nil);
  FHttp.IOHandler := FIdSSL;
  FHttp.OnWorkBegin := DoBegin;
  FHttp.OnWork := DoUpdate;
//  FHttp.OnWorkEnd := DoEnd;
end;

destructor TDownloadIndy.Destroy;
begin
  FIdSSL.Free;
  FHttp.Free;
  inherited;
end;

procedure TDownloadIndy.DoBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
begin
  FWorkCountMax := AWorkCountMax;
end;

procedure TDownloadIndy.DoEnd(ASender: TObject; AWorkMode: TWorkMode);
begin
  try
    try
      TThread.Synchronize(nil,
        procedure
        begin
          if Assigned(FOnEnd) then
            FOnEnd(FFileName);
        end);
    except on E: Exception do
      begin
        if Assigned(FOnError) then
          FOnError(E.Message);
      end;
    end;
  finally
    FFileName := EmptyStr;
    FreeandNil(FDownloadStream);
  end;
end;

function TDownloadIndy.Download(AUrl, AFile: String): IDownload;
begin
  FFileName := AFile;
  FDownloadStream := TMemoryStream.Create;
  try
    FHttp.Get(AUrl, FDownloadStream);
    FDownloadStream.SaveToFile(FFileName);
    DoEnd(FHttp, TWorkMode.wmRead);
  except on E: Exception do
    begin
      FreeAndNil(FDownloadStream);
      FFileName := EmptyStr;
      if Assigned(FOnError) then
        if FHttp.GetHashCode >= 400 then
          FOnError(LangNotFound[Integer(DownloadLanguage)])
        else
          FOnError(E.Message);
    end;
  end;
end;

class function TDownloadIndy.New: IDownload;
begin
  Result := Self.Create;
end;

function TDownloadIndy.OnEnd(AValue: TIntegrationMessage): IDownload;
begin
  Result := Self;
  FOnEnd := AValue;
end;

function TDownloadIndy.OnError(AValue: TIntegrationMessage): IDownload;
begin
  Result := Self;
  FOnError := AValue;
end;

function TDownloadIndy.OnUpdate(AValue: TDownloadOnUpdate): IDownload;
begin
  Result := Self;
  FOnUpdate := AValue;
end;

function TDownloadIndy.TypeDownload: String;
begin
  Result := 'Indy';
end;

procedure TDownloadIndy.DoUpdate(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
begin
  try
    TThread.Queue(nil,
      procedure
      begin
        if Assigned(FOnUpdate) then
          FOnUpdate(AWorkCount, FWorkCountMax);
      end);
  except on E: Exception do
    if Assigned(FOnError) then
      FOnError(E.Message);
  end;
end;

end.
