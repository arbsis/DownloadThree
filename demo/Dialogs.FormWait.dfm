object FormWait: TFormWait
  Left = 0
  Top = 0
  Cursor = crHourGlass
  AlphaBlend = True
  AlphaBlendValue = 128
  Anchors = []
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'FormFundo'
  ClientHeight = 615
  ClientWidth = 1062
  Color = clBlack
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poDefault
  StyleElements = []
  OnCloseQuery = FormCloseQuery
  OnKeyDown = FormKeyDown
  TextHeight = 13
  object pnl: TGridPanel
    Left = 0
    Top = 0
    Width = 1062
    Height = 615
    Cursor = crHourGlass
    Align = alClient
    BevelOuter = bvNone
    ColumnCollection = <
      item
        Value = 100.000000000000000000
      end>
    ControlCollection = <
      item
        Column = 0
        Control = lbl
        Row = 1
      end
      item
        Column = 0
        Control = lblMensagem
        Row = 2
      end
      item
        Column = 0
        Control = ProgressBar
        Row = 3
      end
      item
        Column = 0
        Control = lblInformacao
        Row = 5
      end>
    ParentColor = True
    RowCollection = <
      item
        Value = 53.747873785745780000
      end
      item
        SizeStyle = ssAbsolute
        Value = 70.000000000000000000
      end
      item
        SizeStyle = ssAbsolute
        Value = 50.000000000000000000
      end
      item
        SizeStyle = ssAbsolute
        Value = 30.000000000000000000
      end
      item
        Value = 46.252126214254210000
      end
      item
        SizeStyle = ssAbsolute
        Value = 35.000000000000000000
      end>
    ShowCaption = False
    TabOrder = 0
    StyleElements = []
    DesignSize = (
      1062
      615)
    object lbl: TLabel
      Left = 0
      Top = 231
      Width = 1062
      Height = 70
      Align = alClient
      Alignment = taCenter
      Caption = 'Please wait while processing request...'
      Color = 7930168
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -53
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      Transparent = False
      Layout = tlCenter
      StyleElements = []
      ExplicitWidth = 1034
      ExplicitHeight = 64
    end
    object lblMensagem: TLabel
      AlignWithMargins = True
      Left = 0
      Top = 311
      Width = 1062
      Height = 40
      Margins.Left = 0
      Margins.Top = 10
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alClient
      Alignment = taCenter
      BiDiMode = bdLeftToRight
      Caption = 'AMensagem'
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -21
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentBiDiMode = False
      ParentColor = False
      ParentFont = False
      StyleElements = []
      ExplicitWidth = 113
      ExplicitHeight = 25
    end
    object ProgressBar: TProgressBar
      Left = 305
      Top = 353
      Width = 451
      Height = 25
      Anchors = []
      TabOrder = 0
      Visible = False
      StyleElements = []
    end
    object lblInformacao: TLabel
      AlignWithMargins = True
      Left = 20
      Top = 583
      Width = 1039
      Height = 29
      Margins.Left = 20
      Align = alClient
      Caption = 'lblInformacao'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -21
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
      Visible = False
      StyleElements = []
      ExplicitWidth = 129
      ExplicitHeight = 25
    end
  end
end
