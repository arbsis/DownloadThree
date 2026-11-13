{
  LangLanguage = 0 := pt_BR;
  LangLanguage = 1 := EN;
}

unit Downloads.Language;

interface

uses
  System.Classes;

type
 {$SCOPEDENUMS ON}
  TDownloadLanguage = (ptBR, EN);

var
  DownloadLanguage: TDownloadLanguage;
  LangNotFound: TArray<String>;
  LangCheckDownloadResume: TArray<String>;
  LangNoConnection: TArray<String>;
  LangReadError: TArray<String>;
  LangEndError: TArray<String>;

implementation

procedure Create;
begin
  DownloadLanguage := TDownloadLanguage.ptBR;

  SetLength(LangNotFound, 2);
  SetLength(LangCheckDownloadResume, 2);
  SetLength(LangNoConnection, 2);
  SetLength(LangReadError, 2);
  SetLength(LangEndError, 2);

  LangNotFound[Integer(TDownloadLanguage.ptBR)] := 'Arquivo não encontrado!';
  LangNotFound[Integer(TDownloadLanguage.EN)] := 'File not found!';

  LangCheckDownloadResume[Integer(TDownloadLanguage.ptBR)] := 'O arquivo não existe no servidor ou o servidor não possui recurso para continuar o download';
  LangCheckDownloadResume[Integer(TDownloadLanguage.EN)] := 'The file does not exist on the server or the server does not have the resources to continue the download';

  LangNoConnection[Integer(TDownloadLanguage.ptBR)] := 'Não foi possível conectar ao servidor (erro %d).';
  LangNoConnection[Integer(TDownloadLanguage.EN)] := 'Could not connect to the server (error %d).';

  LangReadError[Integer(TDownloadLanguage.ptBR)] := 'O download foi interrompido (erro %d).';
  LangReadError[Integer(TDownloadLanguage.EN)] := 'The download was interrupted (error %d).';

  LangEndError[Integer(TDownloadLanguage.ptBR)] := 'O arquivo foi baixado, mas houve erro ao processá-lo: %s';
  LangEndError[Integer(TDownloadLanguage.EN)] := 'The file was downloaded, but processing it failed: %s';
end;

initialization
  Create;

end.
