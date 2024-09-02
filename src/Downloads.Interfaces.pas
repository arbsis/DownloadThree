unit Downloads.Interfaces;

interface

type
  TDownloadOnUpdate = procedure(AProgress, ASize: Integer) of object;
  TIntegrationMessage = procedure(AValue: Variant) of object;
  TDownloadThreadDataEvent = procedure(const Sender: TObject; ThreadNo, ASpeed: Integer; AContentLength: Int64; AReadCount: Int64; var Abort: Boolean) of object;

  IDownload = interface
    ['{2997C0A3-EC21-4A9B-BC1E-606D1E00A676}']
    function OnEnd(AValue: TIntegrationMessage): IDownload;
    function OnUpdate(AValue: TDownloadOnUpdate): IDownload;
    function OnError(AValue: TIntegrationMessage): IDownload;
    function TypeDownload: String;
    function Download(AUrl, AFile: String): IDownload;
  end;

  IDownloadTools = interface //Ctrl + Shift + G, para gerar o hash a baixo
    ['{5923C53F-A97C-4A8B-874F-A1799809118D}']
    function SimpleDownloadFile(AUrl, AFile: String): Boolean;
    function CheckMD5File(AUrl, AFile, AMD5: String): Boolean;
  end;

  IDownloadThread = interface
    ['{92710B8F-9995-4A50-A053-AAD9D1F9D8B8}']
    function OnThreadData(AValue: TDownloadThreadDataEvent): IDownloadThread;
    function OnEnd(AValue: TIntegrationMessage): IDownloadThread;
    function OnUpdate(AValue: TDownloadOnUpdate): IDownloadThread;
    function OnError(AValue: TIntegrationMessage): IDownloadThread;
    function OnStart(AValue: TIntegrationMessage): IDownloadThread;
    function Download(AUrl, AFile: String): IDownloadThread;
  end;

implementation

end.
