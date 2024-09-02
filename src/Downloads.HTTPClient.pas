unit Downloads.HTTPClient;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Net.URLClient, System.Net.HttpClient, System.Net.HttpClientComponent, System.ImageList,
  Downloads.Interfaces;

type
  TDownloadHTTPClient = class(TInterfacedObject, IDownload)
  private
    FHttp: THTTPClient;
    FGlobalStart: Cardinal;
    FDownloadStream: TStream;
    FFileName: String;

    FOnEnd: TIntegrationMessage;
    FOnUpdate: TDownloadOnUpdate;
    FOnError: TIntegrationMessage;
  private
    constructor Create;
    procedure DoEnd;
    procedure DoUpdate(const Sender: TObject; AContentLength: Int64; AReadCount: Int64; var Abort: Boolean);
  public
    destructor Destroy; override;
    function OnEnd(AValue: TIntegrationMessage): IDownload;
    function OnUpdate(AValue: TDownloadOnUpdate): IDownload;
    function OnError(AValue: TIntegrationMessage): IDownload;
    function TypeDownload: String;
    function Download(AUrl, AFile: String): IDownload;
    class function New: IDownload;
  end;

  TDownloadToolsHTTPClient = class(TInterfacedObject, IDownloadTools)
  public
    function SimpleDownloadFile(AUrl, AFile: String): Boolean;
    function CheckMD5File(AUrl, AFile, AMD5: String): Boolean;
    class function New: IDownloadTools;
  end;

  TInternalThreadDownload = class(TThread)
  private
    FOnThreadData: TDownloadThreadDataEvent;
  protected
    FURL, FFileName: String;
    FStartPoint: Int64;
    FEndPoint: Int64;
    FThreadNo: Integer;
    FTimeStart: Cardinal;
  protected
    procedure ReceiveDataEvent(const Sender: TObject; AContentLength: Int64; AReadCount: Int64; var Abort: Boolean);
  public
    property OnThreadData: TDownloadThreadDataEvent write FOnThreadData;
    constructor Create(const URL, FileName: string; ThreadNo: Integer; StartPoint, EndPoint: Int64);
    destructor Destroy; override;
    procedure Execute; override;
  end;

  TDownloadThreadHTTPClient = class(TInterfacedObject, IDownloadThread)
  private
    FOnThreadData: TDownloadThreadDataEvent;
    FOnEnd: TIntegrationMessage;
    FOnError: TIntegrationMessage;
    FOnStart: TIntegrationMessage;
    FOnUpdate: TDownloadOnUpdate;
  const
    NumThreads: Integer = 4;
  public
    function OnThreadData(AValue: TDownloadThreadDataEvent): IDownloadThread;
    function OnEnd(AValue: TIntegrationMessage): IDownloadThread;
    function OnUpdate(AValue: TDownloadOnUpdate): IDownloadThread;
    function OnError(AValue: TIntegrationMessage): IDownloadThread;
    function OnStart(AValue: TIntegrationMessage): IDownloadThread;
    function Download(AUrl, AFile: String): IDownloadThread;
    class function New: IDownloadThread;
  end;

implementation

uses
  System.Hash, Downloads.Language;

{ TDownloadToolsHTTPClient }

class function TDownloadToolsHTTPClient.New: IDownloadTools;
begin
  Result := Self.Create;
end;

function TDownloadToolsHTTPClient.SimpleDownloadFile(AUrl, AFile: String): Boolean;
var
  FClient: THTTPClient;
begin
  Result := False;
  FClient := THTTPClient.Create;
  try
    var LResponse := FClient.Head(AUrl);
    if LResponse.StatusCode >= 400 then
      Exit;

    // Create the file that is going to be dowloaded
    var FDownloadStream := TFileStream.Create(AFile, fmCreate);
    try
      FDownloadStream.Position := 0;
      // Start the download process
      try
        FClient.Get(AUrl, FDownloadStream);
        Result := True
      except
        Result := False;
      end;
    finally
      FDownloadStream.Free;
    end;
  finally
    FClient.Free;
  end;
end;

function TDownloadToolsHTTPClient.CheckMD5File(AUrl, AFile, AMD5: String): Boolean;
begin
  Result := True;
  If ((not FileExists(AFile)) or (THashMD5.GetHashStringFromFile(AFile) <> AMD5)) Then
  begin
    If FileExists(AFile) Then
      DeleteFile(AFile);
    Result := SimpleDownloadFile(AUrl, AFile);
  end;
end;

{ TDownloadHTTPClient }

constructor TDownloadHTTPClient.Create;
begin
  FHttp := THTTPClient.Create;
  FHttp.OnReceiveData := DoUpdate;
end;

destructor TDownloadHTTPClient.Destroy;
begin
  FHttp.Free;
  inherited;
end;

procedure TDownloadHTTPClient.DoEnd;
begin
  FDownloadStream.Free;
  try
    if Assigned(FOnEnd) then
      FOnEnd(FFileName);
  except on E: Exception do
    begin
      if Assigned(FOnError) then
        FOnError(E.Message);
    end;
  end;
end;

function TDownloadHTTPClient.Download(AUrl, AFile: String): IDownload;
begin
  Result := Self;
  try
    var LResponse := FHttp.Head(AUrl);
    if LResponse.StatusCode >= 400 then
    begin
      FOnError(LangNotFound[Integer(DownloadLanguage)]);
      Exit;
    end;

    // Create the file that is going to be dowloaded
    FFileName := AFile;
    FDownloadStream := TFileStream.Create(AFile, fmCreate);
    FDownloadStream.Position := 0;
    FGlobalStart := TThread.GetTickCount;
    // Start the download process
    FHttp.Get(AUrl, FDownloadStream);
    DoEnd;
  except on E: Exception do
    begin
      FFileName := EmptyStr;
      if Assigned(FOnError) then
        FOnError(E.Message);
    end;
  end;
end;

class function TDownloadHTTPClient.New: IDownload;
begin
  Result := Self.Create;
end;

function TDownloadHTTPClient.OnEnd(AValue: TIntegrationMessage): IDownload;
begin
  Result := Self;
  FOnEnd := AValue;
end;

function TDownloadHTTPClient.OnError(AValue: TIntegrationMessage): IDownload;
begin
  Result := Self;
  FOnError := AValue;
end;

function TDownloadHTTPClient.OnUpdate(AValue: TDownloadOnUpdate): IDownload;
begin
  Result := Self;
  FOnUpdate := AValue;
end;

function TDownloadHTTPClient.TypeDownload: String;
begin
  Result := 'HTTPClient';
end;

procedure TDownloadHTTPClient.DoUpdate(const Sender: TObject; AContentLength, AReadCount: Int64; var Abort: Boolean);
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

{ TDownloadThreadHTTPClient }

function TDownloadThreadHTTPClient.Download(AUrl, AFile: String): IDownloadThread;
begin
  TThread.CreateAnonymousThread(
  procedure
  var
    LClient: THTTPClient;
    URL: string;
    LResponse: IHTTPResponse;
    StFile: TFileStream;
    LFileName: string;
    LStart, LEnd, LSize, LFragSize: Int64;
    I: Integer;
    LDownloadThreads: array of TInternalThreadDownload;
    LFinished: Boolean;
  begin
    LClient   := THTTPClient.Create;
    LFileName := AFile;
    URL       := AURL;
    try
      try
        LResponse := LClient.Head(URL);
        if LResponse.StatusCode >= 400 then
        begin
          FOnError(LangNotFound[Integer(DownloadLanguage)]);
          Exit;
        end;

        // Checking if the server has resume capability
        if not LClient.CheckDownloadResume(URL) then
        begin
          TThread.Synchronize(TThread.CurrentThread,
          procedure
          begin
            if Assigned(FOnError) then
              FOnError(LangCheckDownloadResume[Integer(DownloadLanguage)]);
          end);
          Exit;
        end;

        // Get space for the file that is going to be dowloaded
        LSize  := LResponse.ContentLength;
        StFile := TFileStream.Create(LFileName, fmCreate);
        try
          STFile.Size := LSize;
        finally
          STFile.Free;
        end;
        if Assigned(FOnStart) then
          FOnStart(LSize);
        // Split the file in four blocks
        LFragSize := LSize div NumThreads;
        LStart    := 0;
        LEnd      := LStart + LFragSize;
        SetLength(LDownloadThreads, NumThreads);
        for I := 0 to NumThreads - 1 do
        begin
          // Create the Thread
          LDownloadThreads[I] := TInternalThreadDownload.Create(URL, LFileName, I, LStart, LEnd);
          if Assigned(FOnThreadData) then
            LDownloadThreads[I].OnThreadData := FOnThreadData;
          TThread.Synchronize(nil, procedure
          begin
            // Adjust the ProgressBar Max Value
            if LEnd >= LSize then
            begin
              LEnd := LSize;
            end;
          end);
          // Update Start and End Values
          LStart := LStart + LFragSize;
          LEnd   := LStart + LFragSize;
        end;
        // Start the download process
        for I := 0 to NumThreads - 1 do
          LDownloadThreads[I].Start;
        // Wait until all threads finish
        LFinished := False;
        while not LFinished do
        begin
          LFinished := True;
          for I := 0 to NumThreads - 1 do
            LFinished := LFinished and LDownloadThreads[I].Finished;
        end;
        // Cleanup Threads
        for I := 0 to NumThreads - 1 do
          LDownloadThreads[I].Free;
      except on E: Exception do
        FOnError(E.Message);
      end;
    finally
      LClient.Free;
      TThread.Synchronize(TThread.CurrentThread, procedure
      begin
        if LFinished and Assigned(FOnEnd) then
          FOnEnd(AFile);
      end);
    end;
  end).Start;
end;

class function TDownloadThreadHTTPClient.New: IDownloadThread;
begin
  Result := Self.Create;
end;

function TDownloadThreadHTTPClient.OnEnd(AValue: TIntegrationMessage): IDownloadThread;
begin
  Result := Self;
  FOnEnd := AValue;
end;

function TDownloadThreadHTTPClient.OnError(AValue: TIntegrationMessage): IDownloadThread;
begin
  Result := Self;
  FOnError := AValue;
end;

function TDownloadThreadHTTPClient.OnStart(AValue: TIntegrationMessage): IDownloadThread;
begin
  Result := Self;
  FOnStart := AValue;
end;

function TDownloadThreadHTTPClient.OnThreadData(AValue: TDownloadThreadDataEvent): IDownloadThread;
begin
  Result := Self;
  FOnThreadData := AValue;
end;

function TDownloadThreadHTTPClient.OnUpdate(AValue: TDownloadOnUpdate): IDownloadThread;
begin
  Result := Self;
  FOnUpdate := AValue;
end;

{ TInternalThreadDownload }

constructor TInternalThreadDownload.Create(const URL, FileName: string; ThreadNo: Integer; StartPoint, EndPoint: Int64);
begin
  inherited Create(True);
  FURL        := URL;
  FFileName   := FileName;
  FThreadNo   := ThreadNo;
  FStartPoint := StartPoint;
  FEndPoint   := EndPoint;
end;

destructor TInternalThreadDownload.Destroy;
begin

  inherited;
end;

procedure TInternalThreadDownload.Execute;
var
  LResponse  : IHTTPResponse;
  LStream    : TFileStream;
  LHttpClient: THTTPClient;
begin
  inherited;
  LHttpClient := THTTPClient.Create;
  try
    LHttpClient.OnReceiveData := ReceiveDataEvent;
    LStream := TFileStream.Create(FFileName, fmOpenWrite or fmShareDenyNone);
    try
      FTimeStart := GetTickCount;
      LStream.Seek(FStartPoint, TSeekOrigin.soBeginning);
      LResponse := LHttpClient.GetRange(FURL, FStartPoint, FEndPoint, LStream);
    finally
      LStream.Free;
    end;
  finally
    LHttpClient.Free;
  end;
end;

procedure TInternalThreadDownload.ReceiveDataEvent(const Sender: TObject; AContentLength, AReadCount: Int64; var Abort: Boolean);
var
  LTime: Cardinal;
  LSpeed: Integer;
begin
  if Assigned(FOnThreadData) then
  begin
    LTime := GetTickCount - FTimeStart;
    if AReadCount = 0 then
      LSpeed := 0
    else
      LSpeed := (AReadCount * 1000) div LTime;
    FOnThreadData(Sender, FThreadNo, LSpeed, AContentLength, AReadCount, Abort);
  end;
end;

end.
