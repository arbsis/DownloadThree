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

implementation

procedure Create;
begin
  DownloadLanguage := TDownloadLanguage.ptBR;

  SetLength(LangNotFound, 2);
  SetLength(LangCheckDownloadResume, 2);

  LangNotFound[Integer(TDownloadLanguage.ptBR)] := 'Arquivo não encontrado!';
  LangNotFound[Integer(TDownloadLanguage.EN)] := 'File not found!';

  LangCheckDownloadResume[Integer(TDownloadLanguage.ptBR)] := 'O arquivo não existe no servidor ou o servidor não possui recurso para continuar o download';
  LangCheckDownloadResume[Integer(TDownloadLanguage.EN)] := 'The file does not exist on the server or the server does not have the resources to continue the download';
end;

initialization
  Create;

end.
