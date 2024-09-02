object FrmTest: TFrmTest
  Left = 0
  Top = 0
  Caption = 'Form Teste Download'
  ClientHeight = 395
  ClientWidth = 527
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 13
  object Label1: TLabel
    Left = 32
    Top = 24
    Width = 23
    Height = 13
    Align = alCustom
    Caption = 'URL:'
  end
  object Label2: TLabel
    Left = 32
    Top = 51
    Width = 16
    Height = 13
    Align = alCustom
    Caption = 'File'
  end
  object edtUrl: TEdit
    Left = 56
    Top = 21
    Width = 433
    Height = 21
    Align = alCustom
    TabOrder = 0
  end
  object edtFileName: TEdit
    Left = 56
    Top = 48
    Width = 433
    Height = 21
    Align = alCustom
    TabOrder = 1
  end
  object ProgressBar1: TProgressBar
    Left = 32
    Top = 200
    Width = 455
    Height = 17
    Align = alCustom
    TabOrder = 2
  end
  object ProgressBar2: TProgressBar
    Left = 32
    Top = 232
    Width = 455
    Height = 17
    Align = alCustom
    TabOrder = 3
  end
  object ProgressBar3: TProgressBar
    Left = 32
    Top = 264
    Width = 455
    Height = 17
    Align = alCustom
    TabOrder = 4
  end
  object ProgressBar4: TProgressBar
    Left = 32
    Top = 295
    Width = 455
    Height = 17
    Align = alCustom
    TabOrder = 5
  end
  object btnSimpleDownloadFileInterface: TButton
    Left = 269
    Top = 125
    Width = 220
    Height = 25
    Align = alCustom
    Caption = 'Simple Download File'
    TabOrder = 6
    OnClick = btnSimpleDownloadFileInterfaceClick
  end
  object btnDownloadWinInet: TButton
    Left = 32
    Top = 85
    Width = 140
    Height = 25
    Align = alCustom
    Caption = 'Download WinInet'
    TabOrder = 7
    OnClick = btnDownloadWinInetClick
  end
  object Button1: TButton
    Left = 269
    Top = 156
    Width = 220
    Height = 25
    Align = alCustom
    Caption = 'Open Folder'
    TabOrder = 8
    OnClick = Button1Click
  end
  object btnVerificarMD5ImagemLogoInterface: TButton
    Left = 32
    Top = 156
    Width = 220
    Height = 25
    Align = alCustom
    Caption = 'Check MD5 File'
    TabOrder = 9
    OnClick = btnVerificarMD5ImagemLogoInterfaceClick
  end
  object btnDownloadThreadInterface: TButton
    Left = 32
    Top = 125
    Width = 220
    Height = 25
    Align = alCustom
    Caption = 'Download Thread'
    TabOrder = 10
    OnClick = btnDownloadThreadInterfaceClick
  end
  object btnThreeDownload: TButton
    Left = 32
    Top = 318
    Width = 220
    Height = 25
    Align = alCustom
    Caption = 'Download - Three'
    TabOrder = 11
    OnClick = btnThreeDownloadClick
  end
  object btnThreeSimple: TButton
    Left = 269
    Top = 318
    Width = 220
    Height = 25
    Align = alCustom
    Caption = 'Simple Download File'
    TabOrder = 12
    OnClick = btnThreeSimpleClick
  end
  object btnThreeMD5: TButton
    Left = 269
    Top = 349
    Width = 220
    Height = 25
    Align = alCustom
    Caption = 'Check MD5 File - Three'
    TabOrder = 13
    OnClick = btnThreeMD5Click
  end
  object btnDownloadHTTPClient: TButton
    Left = 191
    Top = 85
    Width = 140
    Height = 25
    Align = alCustom
    Caption = 'Download HTTPClient'
    TabOrder = 14
    OnClick = btnDownloadHTTPClientClick
  end
  object btnDownloadIndy: TButton
    Left = 349
    Top = 85
    Width = 140
    Height = 25
    Align = alCustom
    Caption = 'Download Indy'
    TabOrder = 15
    OnClick = btnDownloadIndyClick
  end
end
