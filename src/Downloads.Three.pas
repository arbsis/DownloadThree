unit Downloads.Three;

interface

uses
  System.Classes,
  System.Threading,
  Downloads.Interfaces,
  Downloads.WinInet,
  Downloads.Indy,
  Downloads.HTTPClient;

type
 {$SCOPEDENUMS ON}
  TTypeDownload = (WinInet, Indy, HTTPClient, None);

  TDownloadThree = class(TInterfacedObject, IDownload)
  private
    FUrl: String;
    FFile: String;
    FTypeDownload: TTypeDownload;
    FOnEnd: TIntegrationMessage;
    FOnUpdate: TDownloadOnUpdate;
    FOnError: TIntegrationMessage;
  private
    procedure DoError(AValue: Variant);
    procedure DownloadWinInet;
    procedure DownloadIndy;
    procedure DownloadHTTPClient;
  public
    function TypeDownload: String;
    function OnEnd(AValue: TIntegrationMessage): IDownload;
    function OnUpdate(AValue: TDownloadOnUpdate): IDownload;
    function OnError(AValue: TIntegrationMessage): IDownload;
    function Download(AUrl, AFile: String): IDownload;
    class function New: IDownload;
  end;

  TDownloadThreeTools =  class(TInterfacedObject, IDownloadTools)
  public
    function SimpleDownloadFile(AUrl, AFile: String): Boolean;
    function CheckMD5File(AUrl, AFile, AMD5: String): Boolean;
    class function New: IDownloadTools;
  end;

implementation

{ TDownloadThree }

procedure TDownloadThree.DoError(AValue: Variant);
begin
  if FTypeDownload = TTypeDownload.WinInet then
    DownloadIndy
  else
    if FTypeDownload = TTypeDownload.Indy then
      DownloadHTTPClient
    else
      if FTypeDownload = TTypeDownload.HTTPClient then
      begin
        FTypeDownload := TTypeDownload.None;
        FOnError(AValue);
      end;
end;

function TDownloadThree.Download(AUrl, AFile: String): IDownload;
begin
  Result := Self;
  FUrl := AUrl;
  FFile := AFile;
  DownloadWinInet;
end;

procedure TDownloadThree.DownloadHTTPClient;
begin
  FTypeDownload := TTypeDownload.HTTPClient;
  TDownloadHTTPClient
    .New
      .OnEnd(FOnEnd)
      .OnUpdate(FOnUpdate)
      .OnError(DoError)
    .Download(FUrl, FFile);
end;

procedure TDownloadThree.DownloadIndy;
begin
  FTypeDownload := TTypeDownload.Indy;
  TDownloadIndy
    .New
      .OnEnd(FOnEnd)
      .OnUpdate(FOnUpdate)
      .OnError(DoError)
    .Download(FUrl, FFile);
end;

procedure TDownloadThree.DownloadWinInet;
begin
  FTypeDownload := TTypeDownload.WinInet;
  TDownloadWinInet
    .New
      .OnEnd(FOnEnd)
      .OnUpdate(FOnUpdate)
      .OnError(DoError)
    .Download(FUrl, FFile);
end;

class function TDownloadThree.New: IDownload;
begin
  Result := Self.Create;
end;

function TDownloadThree.OnEnd(AValue: TIntegrationMessage): IDownload;
begin
  Result := Self;
  FOnEnd := AValue;
end;

function TDownloadThree.OnError(AValue: TIntegrationMessage): IDownload;
begin
  Result := Self;
  FOnError := AValue;
end;

function TDownloadThree.OnUpdate(AValue: TDownloadOnUpdate): IDownload;
begin
  Result := Self;
  FOnUpdate := AValue;
end;

function TDownloadThree.TypeDownload: String;
begin
  case FTypeDownload of
    TTypeDownload.WinInet: Result := 'WinInet';
    TTypeDownload.Indy: Result := 'Indy';
    TTypeDownload.HTTPClient: Result := 'HTTPClient';
    TTypeDownload.None: Result := 'None';
  end;
end;

{ TDownloadThreeTools }

function TDownloadThreeTools.CheckMD5File(AUrl, AFile, AMD5: String): Boolean;
begin
  Result := TDownloadToolsWinInet.New.CheckMD5File(AUrl, AFile, AMD5);
  if Result then
    Exit;

  Result := TDownloadToolsIndy.New.CheckMD5File(AUrl, AFile, AMD5);
  if Result then
    Exit;

  Result := TDownloadToolsHTTPClient.New.CheckMD5File(AUrl, AFile, AMD5);
end;

class function TDownloadThreeTools.New: IDownloadTools;
begin
  Result := Self.Create;
end;

function TDownloadThreeTools.SimpleDownloadFile(AUrl, AFile: String): Boolean;
begin
  Result := TDownloadToolsWinInet.New.SimpleDownloadFile(AUrl, AFile);
  if Result then
    Exit;

  Result := TDownloadToolsIndy.New.SimpleDownloadFile(AUrl, AFile);
  if Result then
    Exit;

  Result := TDownloadToolsHTTPClient.New.SimpleDownloadFile(AUrl, AFile);
end;

end.

