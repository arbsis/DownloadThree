unit Testes.Downloads;

// Defeitos corrigidos na 0.0.7 (SAAC-1553), com o servidor HTTP local:
// 1. erro no OnEnd de quem chama fazia o TDownloadThree baixar de novo por outro método (OnEnd até 3 vezes);
// 2. o Indy conferia GetHashCode (número do TObject) no lugar do código HTTP: todo erro virava "não encontrado";
// 3. o WinInet informava 20 bytes para um buffer de 20 caracteres: Content-Length com 10 dígitos ficava -1.

interface

uses
  DUnitX.TestFramework, Testes.ServidorLocal;

type
  [TestFixture]
  TTestesDownloads = class
  private
    FServidor: TServidorLocal;
    FPasta: String;
    FFins: Integer;
    FErros: TArray<String>;
    FErroNoFim: Boolean;
    procedure AoTerminar(AValue: Variant);
    procedure AoErrar(AValue: Variant);
    function Arquivo(const ANome: String): String;
  public
    [SetupFixture]
    procedure IniciarServidor;
    [TearDownFixture]
    procedure PararServidor;
    [Setup]
    procedure Preparar;

    // 1. OnEnd de quem chama
    [Test]
    procedure Three_OnEndComErro_BaixaUmaVezSo;
    [Test]
    procedure Three_Normal_OnEndUmaVezSemErro;
    [Test]
    procedure Three_ArquivoInexistente_UmErroNoFimSemOnEnd;

    // 2. Indy
    [Test]
    procedure Indy_Download404_AvisaNaoEncontrado;
    [Test]
    procedure Indy_DownloadSemServidor_AvisaOErroVerdadeiro;
    [Test]
    procedure Indy_SimpleDownloadFile_200E404;

    // 3. WinInet
    [Test]
    procedure WinInet_GetContentLength_DezDigitos;
    [Test]
    procedure WinInet_GetContentStatusCode_200E404;
    [Test]
    procedure WinInet_SimpleDownloadFile_200E404;
  end;

implementation

uses
  System.SysUtils, System.IOUtils, System.Variants, Winapi.WinInet,
  Downloads.Interfaces, Downloads.Language, Downloads.Three, Downloads.WinInet, Downloads.Indy;

const
  ERRO_DE_QUEM_CHAMA = 'falha no OnEnd de quem chama';

{ TTestesDownloads }

procedure TTestesDownloads.IniciarServidor;
begin
  FServidor := TServidorLocal.Create;
  FPasta := TPath.Combine(TPath.GetTempPath, 'DownloadThreeTestes');
  TDirectory.CreateDirectory(FPasta);
end;

procedure TTestesDownloads.PararServidor;
begin
  FServidor.Free;
  try
    TDirectory.Delete(FPasta, True);
  except
  end;
end;

procedure TTestesDownloads.Preparar;
begin
  FServidor.ZerarPedidos;
  FFins := 0;
  FErros := [];
  FErroNoFim := False;
end;

function TTestesDownloads.Arquivo(const ANome: String): String;
begin
  Result := TPath.Combine(FPasta, ANome);
  if TFile.Exists(Result) then
    TFile.Delete(Result);
end;

procedure TTestesDownloads.AoTerminar(AValue: Variant);
begin
  Inc(FFins);
  if FErroNoFim then
    raise Exception.Create(ERRO_DE_QUEM_CHAMA);
end;

procedure TTestesDownloads.AoErrar(AValue: Variant);
begin
  FErros := FErros + [VarToStr(AValue)];
end;

procedure TTestesDownloads.Three_OnEndComErro_BaixaUmaVezSo;
begin
  var LArquivo := Arquivo('fim_com_erro.txt');
  FErroNoFim := True;
  TDownloadThree.New.OnEnd(AoTerminar).OnError(AoErrar).Download(FServidor.Url('/arquivo.txt'), LArquivo);

  // Antes: GET 3 vezes (WinInet, Indy e HTTPClient) e o OnEnd 3 vezes
  Assert.AreEqual(1, FServidor.Pedidos('GET /arquivo.txt'), 'pedidos GET ao servidor');
  Assert.AreEqual(1, FFins, 'vezes que o OnEnd rodou');
  Assert.AreEqual(1, Integer(Length(FErros)),'avisos de erro');
  Assert.IsTrue(FErros[0].Contains(ERRO_DE_QUEM_CHAMA), 'o erro de quem chama chega no OnError: ' + FErros[0]);
  Assert.AreEqual(CONTEUDO_DO_ARQUIVO, TFile.ReadAllText(LArquivo), 'o arquivo baixado continua no disco');
end;

procedure TTestesDownloads.Three_Normal_OnEndUmaVezSemErro;
begin
  var LArquivo := Arquivo('normal.txt');
  TDownloadThree.New.OnEnd(AoTerminar).OnError(AoErrar).Download(FServidor.Url('/arquivo.txt'), LArquivo);

  Assert.AreEqual(1, FServidor.Pedidos('GET /arquivo.txt'), 'pedidos GET ao servidor');
  Assert.AreEqual(1, FFins, 'vezes que o OnEnd rodou');
  Assert.AreEqual(0, Integer(Length(FErros)),'avisos de erro');
  Assert.AreEqual(CONTEUDO_DO_ARQUIVO, TFile.ReadAllText(LArquivo));
end;

procedure TTestesDownloads.Three_ArquivoInexistente_UmErroNoFimSemOnEnd;
begin
  var LArquivo := Arquivo('inexistente.txt');
  TDownloadThree.New.OnEnd(AoTerminar).OnError(AoErrar).Download(FServidor.Url('/inexistente'), LArquivo);

  Assert.AreEqual(0, FFins, 'o OnEnd não roda');
  Assert.AreEqual(1, Integer(Length(FErros)),'um aviso só, depois dos três métodos');
  Assert.IsFalse(TFile.Exists(LArquivo), 'sem arquivo no disco');
end;

procedure TTestesDownloads.Indy_Download404_AvisaNaoEncontrado;
begin
  TDownloadIndy.New.OnEnd(AoTerminar).OnError(AoErrar).Download(FServidor.Url('/inexistente'), Arquivo('indy404.txt'));

  Assert.AreEqual(1, Integer(Length(FErros)),'avisos de erro');
  Assert.AreEqual(LangNotFound[Integer(DownloadLanguage)], FErros[0]);
end;

procedure TTestesDownloads.Indy_DownloadSemServidor_AvisaOErroVerdadeiro;
begin
  // Porta sem servidor: erro de conexão, não "não encontrado" (antes GetHashCode >= 400 dava sempre "não encontrado")
  var LUrl := Format('http://127.0.0.1:%d/arquivo.txt', [FServidor.Porta + 1000]);
  TDownloadIndy.New.OnEnd(AoTerminar).OnError(AoErrar).Download(LUrl, Arquivo('indy_sem_servidor.txt'));

  Assert.AreEqual(1, Integer(Length(FErros)),'avisos de erro');
  Assert.AreNotEqual(LangNotFound[Integer(DownloadLanguage)], FErros[0], 'a causa verdadeira aparece');
end;

procedure TTestesDownloads.Indy_SimpleDownloadFile_200E404;
begin
  var LArquivo := Arquivo('indy_simples.txt');
  Assert.IsTrue(TDownloadToolsIndy.New.SimpleDownloadFile(FServidor.Url('/arquivo.txt'), LArquivo), '200');
  Assert.AreEqual(CONTEUDO_DO_ARQUIVO, TFile.ReadAllText(LArquivo));
  Assert.IsFalse(TDownloadToolsIndy.New.SimpleDownloadFile(FServidor.Url('/inexistente'), Arquivo('indy_404.txt')), '404');
end;

procedure TTestesDownloads.WinInet_GetContentLength_DezDigitos;
begin
  var LSessao := InternetOpen('', INTERNET_OPEN_TYPE_DIRECT, nil, nil, 0);
  Assert.IsTrue(LSessao <> nil, 'InternetOpen');
  try
    var LUrl := InternetOpenURL(LSessao, PChar(FServidor.Url('/grande')), nil, 0, INTERNET_FLAG_RELOAD, 0);
    Assert.IsTrue(LUrl <> nil, 'InternetOpenURL');
    try
      // Antes: -1 (o buffer de 20 caracteres era informado como 20 bytes, e só cabiam 9 dígitos)
      Assert.AreEqual(Int64(1234567890), GetContentLength(LUrl));
    finally
      InternetCloseHandle(LUrl);
    end;
  finally
    InternetCloseHandle(LSessao);
  end;
end;

procedure TTestesDownloads.WinInet_GetContentStatusCode_200E404;
begin
  var LSessao := InternetOpen('', INTERNET_OPEN_TYPE_DIRECT, nil, nil, 0);
  try
    var LUrl := InternetOpenURL(LSessao, PChar(FServidor.Url('/arquivo.txt')), nil, 0, INTERNET_FLAG_RELOAD, 0);
    try
      Assert.AreEqual(200, GetContentStatusCode(LUrl), '200');
    finally
      InternetCloseHandle(LUrl);
    end;
    LUrl := InternetOpenURL(LSessao, PChar(FServidor.Url('/inexistente')), nil, 0, INTERNET_FLAG_RELOAD, 0);
    try
      Assert.AreEqual(404, GetContentStatusCode(LUrl), '404');
    finally
      InternetCloseHandle(LUrl);
    end;
  finally
    InternetCloseHandle(LSessao);
  end;
end;

procedure TTestesDownloads.WinInet_SimpleDownloadFile_200E404;
begin
  var LArquivo := Arquivo('wininet_simples.txt');
  Assert.IsTrue(TDownloadToolsWinInet.New.SimpleDownloadFile(FServidor.Url('/arquivo.txt'), LArquivo), '200');
  Assert.AreEqual(CONTEUDO_DO_ARQUIVO, TFile.ReadAllText(LArquivo));
  var L404 := Arquivo('wininet_404.txt');
  Assert.IsFalse(TDownloadToolsWinInet.New.SimpleDownloadFile(FServidor.Url('/inexistente'), L404), '404');
  Assert.IsFalse(TFile.Exists(L404), 'sem arquivo no disco');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestesDownloads);

end.
