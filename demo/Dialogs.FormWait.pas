unit Dialogs.FormWait;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, System.Math, Vcl.ExtCtrls;

type

  TFormWait = class(TForm)
    lbl: TLabel;
    ProgressBar: TProgressBar;
    lblMensagem: TLabel;
    lblInformacao: TLabel;
    pnl: TGridPanel;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
    FFormPai         : TObject;
    FFormSender      : TObject;
    FEsmaecer        : Boolean;
    FPermitirCancelar: Boolean;
    FCancelou        : Boolean;
    FMonitorNum      : Integer;

    procedure AjustarTamanhoFonteLabel(ALabel: TLabel; ATamanhoMaximoFonte: Integer);
  public
    { Public declarations }
    procedure Exibir(ASender: TObject; AExibirAguarde: Boolean = False; AMensagem: String = '';
      AExibirBarraDeProgresso: Boolean = False; APermitirCancelar: Boolean = False; AExibirTelaCheia: Boolean = False);
    procedure Fechar;

    Procedure Atualizar(Const AMensagem: String)        ; Overload;
    Procedure Atualizar(Const APercentual: Real)        ; Overload;
    Procedure Atualizar(Const APosição, ATotal: Integer); Overload;
    Function  Cancelou: Boolean;

    constructor CreateNew(AOwner: TComponent; AEsmaecer: Boolean = True; AMonitorNum: Integer = 0);
  end;

  TFormWaitThread = class(TThread)
  private
    FFundo: TFormWait;
  public
    constructor Create(AFundo: TFormWait); reintroduce;
    procedure Execute; override;
    procedure Sincronizar;
  end;
var
  FormFundo: TFormWait;

implementation

{$R *.dfm}

procedure TFormWait.Atualizar(const AMensagem: String);
begin
  Application.ProcessMessages;
  lblMensagem.Caption := AMensagem;
  lblMensagem.Visible := AMensagem <> EmptyStr;
  Application.ProcessMessages;
end;

procedure TFormWait.Atualizar(const APercentual: Real);
begin
  Application.ProcessMessages;
  ProgressBar.Visible  := True;
  ProgressBar.Max      := 100;
  ProgressBar.Position := Abs(Floor(APercentual));
  Application.ProcessMessages;
end;

procedure TFormWait.AjustarTamanhoFonteLabel(ALabel: TLabel; ATamanhoMaximoFonte: Integer);
var
  lbl: TLabel;
begin
  lbl           := TLabel.Create(Self);
  lbl.Left      := 0;
  lbl.Caption   := ALabel.Caption;
  lbl.AutoSize  := True;
  lbl.WordWrap  := False;
  lbl.Font      := ALabel.Font;
  lbl.Font.Size := 1;

  while (lbl.Width < Self.Width) and (lbl.Font.Size < ATamanhoMaximoFonte) do
    lbl.Font.Size := lbl.Font.Size + 1;

  ALabel.Font.Size := lbl.Font.Size -1;
  Application.ProcessMessages;

  lbl.Free;
end;

procedure TFormWait.Atualizar(const APosição, ATotal: Integer);
begin
  Application.ProcessMessages;
  ProgressBar.Visible  := True;
  ProgressBar.Max      := ATotal;
  ProgressBar.Position := APosição;
  Application.ProcessMessages;
end;

function TFormWait.Cancelou: Boolean;
begin
  Exit(FCancelou);
end;

/// <summary> Cria o FormFundo na aplicação</summary>
/// <param name="AOwner">Form pai (form principal da aplicação)</param>
/// <param name="AEsmaecer">Para deixar com fundo escuro</param>
/// <param name="AMonitorNum">Para exibir no monitor específico caso AOwner = nill</param>
constructor TFormWait.CreateNew(AOwner: TComponent; AEsmaecer: Boolean; AMonitorNum: Integer);
begin
  FFormPai  := AOwner;
  FEsmaecer := AEsmaecer;
  FMonitorNum := AMonitorNum;
  inherited CreateNew(AOwner);
  Left := 0;
  Top := 0;

  AlphaBlend  := True;
  BorderStyle := bsNone;
  Color       := clBlack;

  pnl        := TGridPanel.Create(Self);
  pnl.Parent := Self;
  pnl.Align  := alClient;

  pnl.RowCollection.Clear;
  pnl.RowCollection.Add;
  pnl.RowCollection.Add;
  pnl.RowCollection.Add;
  pnl.RowCollection.Add;
  pnl.RowCollection.Add;
  pnl.RowCollection.Add;
  pnl.ColumnCollection.Clear;
  pnl.ColumnCollection.Add;

  lbl                  := TLabel.Create(pnl);
  lbl.Parent           := pnl;
  lblMensagem          := TLabel.Create(pnl);
  lblMensagem.Parent   := pnl;
  ProgressBar          := TProgressBar.Create(pnl);
  ProgressBar.Parent   := pnl;
  lblInformacao        := TLabel.Create(pnl);
  lblInformacao.Parent := pnl;

  pnl.ControlCollection[0].SetLocation(0, 1, True);
  pnl.ControlCollection[1].SetLocation(0, 2, True);
  pnl.ControlCollection[2].SetLocation(0, 3, True);
  pnl.ControlCollection[3].SetLocation(0, 5, True);

  pnl.ColumnCollection[0].SizeStyle := ssPercent;
  pnl.ColumnCollection[0].Value     := 100;

  pnl.RowCollection[0].SizeStyle := ssPercent;
  pnl.RowCollection[0].Value     := 53;
  pnl.RowCollection[1].SizeStyle := ssAbsolute;
  pnl.RowCollection[1].Value     := 70;
  pnl.RowCollection[2].SizeStyle := ssAbsolute;
  pnl.RowCollection[2].Value     := 50;
  pnl.RowCollection[3].SizeStyle := ssAbsolute;
  pnl.RowCollection[3].Value     := 30;
  pnl.RowCollection[4].SizeStyle := ssPercent;
  pnl.RowCollection[4].Value     := 46;
  pnl.RowCollection[5].SizeStyle := ssAbsolute;
  pnl.RowCollection[5].Value     := 35;

  lbl.Align       := alClient;
  lbl.Caption     := 'Please wait while processing request...';
  lbl.Font.Name   := 'Tahoma';
  lbl.Font.Color  := clWhite;
  lbl.Font.Size   := 40;
  lbl.Font.Style  := [fsBold];
  lbl.Alignment   := taCenter;
  lbl.Layout      := tlCenter;
  lbl.Transparent := False;
  lbl.ParentColor := False;
  lbl.Color       := $00790138;
  lbl.Visible     := False;

  lblMensagem.Caption     := EmptyStr;
  lblMensagem.Font.Name   := 'Tahoma';
  lblMensagem.Font.Color  := clWhite;
  lblMensagem.Font.Size   := 16;
  lblMensagem.Font.Style  := [fsBold];
  lblMensagem.Alignment   := taCenter;
  lblMensagem.Layout      := tlCenter;
  lblMensagem.Transparent := True;
  lblMensagem.ParentColor := False;
  lblMensagem.Visible     := False;

  lblInformacao.Caption     := EmptyStr;
//  lblInformacao.Font.Name   := 'Tahoma';
//  lblInformacao.Font.Color  := clWhite;
//  lblInformacao.Font.Size   := 16;
//  lblInformacao.Font.Style  := [fsBold];
//  lblInformacao.Align       := alClient;
//  lblInformacao.Alignment   := taLeftJustify;
//  lblInformacao.Layout      := tlCenter;
//  lblInformacao.Transparent := True;
//  lblInformacao.ParentColor := False;
  lblInformacao.Visible     := False;

  ProgressBar.Width  := 451;
  ProgressBar.Height := 25;
end;

procedure TFormWait.Exibir(ASender: TObject; AExibirAguarde: Boolean; AMensagem: String;
                     AExibirBarraDeProgresso: Boolean; APermitirCancelar: Boolean; AExibirTelaCheia: Boolean);
begin
  if not Assigned(Self) then
    Exit;
  if Assigned(ASender) then
    FFormSender := ASender
  else
    FFormSender := nil;

  FPermitirCancelar     := APermitirCancelar;
  lbl.Visible           := AExibirAguarde;
  lblMensagem.Visible   := AMensagem <> EmptyStr;
  ProgressBar.Visible   := AExibirBarraDeProgresso;
  ProgressBar.Position  := 0;

//  lblInformação.Visible := APermitirCancelar;

  if AExibirAguarde then
  begin
    AlphaBlendValue := 200;
    AjustarTamanhoFonteLabel(lbl, 40);
  end else
  begin
    if FEsmaecer then
      AlphaBlendValue := 128
    else
      AlphaBlendValue := 0;
  end;
//  if APermitirCancelar then
//    lblInformação.Caption := 'Pressione a tecla ESC para cancelar'
//  else
//    lblInformação.Caption := '';

  if AMensagem <> EmptyStr then
  begin
    lblMensagem.Caption := AMensagem;
    AjustarTamanhoFonteLabel(lblMensagem, 16);
  end;

  Show;
  if AExibirTelaCheia then
  begin
    Position := poDesigned;
    if not Assigned(FFormPai) then
    begin
      if (Screen.MonitorCount > 1) and (FMonitorNum > 0) then { Abrir em monitor específico }
      begin
        Left := Screen.Monitors[Pred(FMonitorNum)].Left;
        Top := Screen.Monitors[Pred(FMonitorNum)].Top;
      end;
      WindowState := TWindowState.wsMaximized;
    end else
    begin
      Width := (FFormPai as TForm).Monitor.Width;
      Height := (FFormPai as TForm).Monitor.Height - 50;
      Position := poScreenCenter;
    end;
  end else
  begin
    Width    := TForm(FFormPai).Width - 14;
    Height   := TForm(FFormPai).Height - 7;
    Left     := TForm(FFormPai).Left + 7;
    Top      := TForm(FFormPai).Top;
  end;
  Application.ProcessMessages;
end;

/// <summary> Fechar o form </summary>
/// <param name="Item">  </param>
/// <param name="Collection">  </param>
procedure TFormWait.Fechar;
begin
  Hide;
  if Assigned(FFormSender) then
    if Assigned(TForm(FFormSender).ActiveControl)then
      PostMessage(TForm(FFormSender).ActiveControl.Handle,WM_SETFOCUS,0,0);
end;

procedure TFormWait.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FPermitirCancelar;
end;

procedure TFormWait.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  Application.ProcessMessages;
  If Visible And FPermitirCancelar And (Key = VK_ESCAPE) Then
  Begin
    FCancelou := True;
    Fechar;
  End;
end;

{ TFormWaitThread }

constructor TFormWaitThread.Create(AFundo: TFormWait);
begin
  inherited Create(True);
  Self.FreeOnTerminate := True;
  FFundo := AFundo;
end;

procedure TFormWaitThread.Execute;
begin
  inherited;
  FFundo.Exibir(nil, True, 'Teste...');
end;

procedure TFormWaitThread.Sincronizar;
begin
  FFundo.Fechar;
end;

end.
