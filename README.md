# DownloadThree
Delphi component to download through 3 available modes: Indy, WinInet and HTTPClient.

Instalação pelo Boss: `"github.com/git@github.com:arbsis/downloadthree": "^0.0.7"`

`TDownloadThree` tenta o WinInet; se falhar, o Indy; se falhar, o HTTPClient. O `OnError` de quem chama recebe um aviso só, depois dos três. O `OnEnd` roda uma vez, quando o arquivo terminou de baixar; um erro levantado dentro do `OnEnd` de quem chama chega ao `OnError` dele e não faz baixar de novo.

## Testes

DUnitX em `tests`, com um servidor HTTP local (127.0.0.1), sem depender da internet: `tests\testar.bat [Win32|Win64]` compila e executa (código de saída 0 quando todos passam).

## Versões

- **0.0.7:** erro no `OnEnd` de quem chama não baixa mais o arquivo de novo pelos outros métodos (o `OnEnd` rodava até 3 vezes); o Indy confere o código HTTP de verdade (antes `GetHashCode`, e todo erro podia virar "Arquivo não encontrado!"); o WinInet lê `Content-Length` de 10 dígitos (antes arquivos acima de ~1 GB ficavam sem tamanho e o WinInet recusava o download); o HTTPClient não deixa o arquivo aberto nem pela metade quando o download falha no meio; testes DUnitX.
- **0.0.6:** download sem conexão não dá mais "I/O error 1784" (Windows 7 sem TLS 1.2); `TLS12Desligado`.
