unit Testes.ServidorLocal;

// Servidor HTTP local para os testes (127.0.0.1), sem depender da internet. Responde HTTP escrito à mão para controlar
// cada caso e conta os pedidos por método e caminho ("GET /arquivo.txt").
//   /arquivo.txt  200 com CONTEUDO_DO_ARQUIVO
//   /grande       200 com Content-Length: 1234567890 (10 dígitos) e sem corpo; fecha a conexão
//   outro         404

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.SyncObjs,
  IdContext, IdTCPServer, IdGlobal;

const
  CONTEUDO_DO_ARQUIVO = 'Arquivo de teste do DownloadThree 0123456789';
  TAMANHO_GRANDE = '1234567890';

type
  TServidorLocal = class
  private
    FServidor: TIdTCPServer;
    FPorta: Integer;
    FPedidos: TDictionary<String, Integer>;
    FTrava: TCriticalSection;
    procedure Executar(AContext: TIdContext);
  public
    constructor Create;
    destructor Destroy; override;
    function Url(const ACaminho: String): String;
    function Pedidos(const AMetodoECaminho: String): Integer;
    procedure ZerarPedidos;
    property Porta: Integer read FPorta;
  end;

implementation

{ TServidorLocal }

constructor TServidorLocal.Create;
begin
  FPedidos := TDictionary<String, Integer>.Create;
  FTrava := TCriticalSection.Create;
  FServidor := TIdTCPServer.Create(nil);
  FServidor.OnExecute := Executar;
  // Primeira porta livre a partir de 18431
  for var LPorta := 18431 to 18480 do
  begin
    FServidor.Bindings.Clear;
    var LBinding := FServidor.Bindings.Add;
    LBinding.IP := '127.0.0.1';
    LBinding.Port := LPorta;
    try
      FServidor.Active := True;
      FPorta := LPorta;
      Break;
    except
      FServidor.Active := False;
    end;
  end;
  if FPorta = 0 then
    raise Exception.Create('Nenhuma porta livre para o servidor de testes');
end;

destructor TServidorLocal.Destroy;
begin
  FServidor.Active := False;
  FServidor.Free;
  FTrava.Free;
  FPedidos.Free;
  inherited;
end;

procedure TServidorLocal.Executar(AContext: TIdContext);
begin
  var LIO := AContext.Connection.IOHandler;
  var LLinha := LIO.ReadLn; // "GET /arquivo.txt?x=1 HTTP/1.1"
  repeat
  until LIO.ReadLn = ''; // cabeçalhos do pedido

  var LPartes := LLinha.Split([' ']);
  var LMetodo := '';
  var LCaminho := '';
  if Length(LPartes) >= 2 then
  begin
    LMetodo := LPartes[0];
    LCaminho := LPartes[1];
    if LCaminho.Contains('?') then
      LCaminho := LCaminho.Substring(0, LCaminho.IndexOf('?'));
  end;

  FTrava.Enter;
  try
    var LChave := LMetodo + ' ' + LCaminho;
    var LTotal: Integer;
    if not FPedidos.TryGetValue(LChave, LTotal) then
      LTotal := 0;
    FPedidos.AddOrSetValue(LChave, LTotal + 1);
  finally
    FTrava.Leave;
  end;

  const FIM = #13#10;
  var LResposta: String;
  if LCaminho = '/arquivo.txt' then
  begin
    LResposta := 'HTTP/1.1 200 OK' + FIM + 'Content-Type: text/plain' + FIM +
      'Content-Length: ' + Length(CONTEUDO_DO_ARQUIVO).ToString + FIM + 'Cache-Control: no-store' + FIM +
      'Connection: close' + FIM + FIM;
    if LMetodo <> 'HEAD' then
      LResposta := LResposta + CONTEUDO_DO_ARQUIVO;
  end
  else if LCaminho = '/grande' then
    LResposta := 'HTTP/1.1 200 OK' + FIM + 'Content-Type: application/octet-stream' + FIM +
      'Content-Length: ' + TAMANHO_GRANDE + FIM + 'Cache-Control: no-store' + FIM + 'Connection: close' + FIM + FIM
  else
  begin
    LResposta := 'HTTP/1.1 404 Not Found' + FIM + 'Content-Type: text/plain' + FIM + 'Content-Length: 9' + FIM +
      'Cache-Control: no-store' + FIM + 'Connection: close' + FIM + FIM;
    if LMetodo <> 'HEAD' then
      LResposta := LResposta + 'not found';
  end;

  LIO.Write(LResposta, IndyTextEncoding_ASCII);
  AContext.Connection.Disconnect;
end;

function TServidorLocal.Pedidos(const AMetodoECaminho: String): Integer;
begin
  FTrava.Enter;
  try
    if not FPedidos.TryGetValue(AMetodoECaminho, Result) then
      Result := 0;
  finally
    FTrava.Leave;
  end;
end;

function TServidorLocal.Url(const ACaminho: String): String;
begin
  // Um número diferente por pedido: nenhum cache responde no lugar do servidor
  Result := Format('http://127.0.0.1:%d%s?n=%s', [FPorta, ACaminho, TGUID.NewGuid.ToString.Substring(1, 8)]);
end;

procedure TServidorLocal.ZerarPedidos;
begin
  FTrava.Enter;
  try
    FPedidos.Clear;
  finally
    FTrava.Leave;
  end;
end;

end.
