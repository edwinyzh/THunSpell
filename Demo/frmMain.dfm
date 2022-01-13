object MainForm: TMainForm
  Left = 210
  Top = 101
  Caption = 'THunSpell Demo'
  ClientHeight = 687
  ClientWidth = 936
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'MS Shell Dlg'
  Font.Style = []
  Menu = nmuMain
  OldCreateOrder = False
  Position = poDefault
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 130
  TextHeight = 16
  object txtMain: THunSpellMemo
    Left = 0
    Top = 0
    Width = 936
    Height = 667
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'MS Shell Dlg'
    Font.Style = []
    HideSelection = False
    Lines.Strings = (
      
        'Wendsday, March 26 -- Too Atlantic bottle-nosed dolphins were pu' +
        't in to '
      
        'active duty today by the Navy. The two specially traned dolphins' +
        ' are '
      
        'searching the waters for explosives around the port citty of Umm' +
        ' Qasr.'
      
        'Makai, 33, and Tacoma, 22, both mails, use their natural ability' +
        'es to '
      
        'locate explosives and mark them with floats. The dolfins are wor' +
        'king to '
      'clear a path for ships carrying humanitarian aid to Iraq.'
      
        'The Navy has 20 trained dolphins as part of the Marine Mammal Pr' +
        'oject '
      
        'based in San Diego, California. Nine of those dolphins where flo' +
        'wn to the '
      
        'Persian Gulf recently. They are staying in specially bilt tanks ' +
        'abord a U.S. '
      'warship. '
      
        'The Navy dolphins are tauhgt to avoid touching the explasives. A' +
        'ccording '
      
        'to won bomb expert, the dolphins are more at risk from local dol' +
        'phins than '
      
        'the explosives. Dolphins are teritorial an cud drive away the tw' +
        'o '
      'newcomers.')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
    WantTabs = True
    OnKeyPress = txtMainKeyPress
    HunSpell = HunSpell
    AddMenu = True
    RightClickMoveCaret = True
    RightEdge = 80
  end
  object sbMain: TStatusBar
    Left = 0
    Top = 667
    Width = 936
    Height = 20
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Action = actUpdateStatusbar
    Panels = <
      item
        Width = 50
      end
      item
        Width = 50
      end>
  end
  object nmuMain: TMainMenu
    Left = 400
    Top = 376
    object File1: TMenuItem
      Caption = '&File'
      object New1: TMenuItem
        Action = actFileNew
      end
      object Open1: TMenuItem
        Action = actFileOpen
      end
      object Save1: TMenuItem
        Action = actFileSave
      end
      object SaveAs1: TMenuItem
        Action = actFileSaveAs
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Action = actFileExit
      end
    end
    object Edit1: TMenuItem
      Caption = '&Edit'
      object Undo1: TMenuItem
        Action = actEditUndo
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Cut1: TMenuItem
        Action = actEditCut
      end
      object Copy1: TMenuItem
        Action = actEditCopy
      end
      object Paste1: TMenuItem
        Action = actEditPaste
      end
      object Delete1: TMenuItem
        Action = actEditDelete
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object actEditFind1: TMenuItem
        Action = actEditFind
      end
      object FindNext1: TMenuItem
        Action = actEditFindNext
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object SelectAll1: TMenuItem
        Action = actEditSelectAll
      end
      object DateTime1: TMenuItem
        Action = actEditDateTime
      end
    end
    object Format1: TMenuItem
      Caption = '&Format'
      object WordWrap1: TMenuItem
        Action = actFormatWordWrap
      end
      object Font1: TMenuItem
        Action = actFormatFont
      end
    end
    object View1: TMenuItem
      Caption = '&View'
      object RightEdge1: TMenuItem
        Action = actViewRightEdge
      end
      object StatusBar1: TMenuItem
        Action = actViewStatusbar
      end
    end
    object Tools1: TMenuItem
      Caption = '&Tools'
      object CheclSpelling1: TMenuItem
        Action = actToolsCheckSpelling
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object AutoSpellcheck1: TMenuItem
        Action = actToolsAutoSpellcheck
      end
      object Ignorewordswithnumbers1: TMenuItem
        Action = actToolsIgnoreWordsWithNumbers
      end
      object IgnoreAllCaps1: TMenuItem
        Action = actToolsIgnoreAllCaps
      end
      object mnuDictionary: TMenuItem
        Caption = '&Dictionary'
        OnClick = mnuDictionaryClick
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object TabstoSpace1: TMenuItem
        Action = actToolsTabsToSpace
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object WebSite1: TMenuItem
        Action = actHelpWeb
      end
      object Aboud1: TMenuItem
        Action = actHelpAbout
      end
    end
  end
  object actlMain: TActionList
    Left = 504
    Top = 368
    object actFileNew: TAction
      Category = 'File'
      Caption = '&New'
      ShortCut = 16462
      OnExecute = actFileNewExecute
    end
    object actFileOpen: TAction
      Category = 'File'
      Caption = 'Open'
      ShortCut = 16463
      OnExecute = actFileOpenExecute
    end
    object actFileSave: TAction
      Category = 'File'
      Caption = '&Save'
      ShortCut = 16467
      OnExecute = actFileSaveExecute
      OnUpdate = actFileSaveUpdate
    end
    object actFileSaveAs: TAction
      Category = 'File'
      Caption = 'Save As...'
      OnExecute = actFileSaveAsExecute
      OnUpdate = actFileSaveUpdate
    end
    object actFileExit: TAction
      Category = 'File'
      Caption = 'E&xit'
      OnExecute = actFileExitExecute
    end
    object actEditUndo: TAction
      Category = 'Edit'
      Caption = 'Undo'
      ShortCut = 16474
      OnExecute = actEditUndoExecute
      OnUpdate = actEditUndoUpdate
    end
    object actEditCut: TAction
      Category = 'Edit'
      Caption = 'Cut'
      ShortCut = 16472
      OnExecute = actEditCutExecute
      OnUpdate = actEditCutUpdate
    end
    object actEditCopy: TAction
      Category = 'Edit'
      Caption = 'Copy'
      ShortCut = 16451
      OnExecute = actEditCopyExecute
      OnUpdate = actEditCutUpdate
    end
    object actEditPaste: TAction
      Category = 'Edit'
      Caption = 'Paste'
      ShortCut = 16470
      OnExecute = actEditPasteExecute
      OnUpdate = actEditPasteUpdate
    end
    object actHelpAbout: TAction
      Category = 'Help'
      Caption = 'About'
      OnExecute = actHelpAboutExecute
    end
    object actToolsCheckSpelling: TAction
      Category = 'Tools'
      Caption = 'Check Spelling...'
      OnExecute = actToolsCheckSpellingExecute
    end
    object actFormatWordWrap: TAction
      Category = 'Format'
      Caption = 'Word Wrap'
      OnExecute = actFormatWordWrapExecute
      OnUpdate = actFormatWordWrapUpdate
    end
    object actFormatFont: TAction
      Category = 'Format'
      Caption = 'Font...'
      OnExecute = actFormatFontExecute
    end
    object actEditSelectAll: TAction
      Category = 'Edit'
      Caption = 'Select All'
      ShortCut = 16449
      OnExecute = actEditSelectAllExecute
    end
    object actEditDelete: TAction
      Category = 'Edit'
      Caption = 'Delete'
      OnExecute = actEditDeleteExecute
      OnUpdate = actEditDeleteUpdate
    end
    object actViewStatusbar: TAction
      Category = 'View'
      Caption = 'Status Bar'
      OnExecute = actViewStatusbarExecute
      OnUpdate = actViewStatusbarUpdate
    end
    object actUpdateStatusbar: TAction
      OnUpdate = actUpdateStatusbarUpdate
    end
    object actEditFind: TAction
      Category = 'Edit'
      Caption = 'Find...'
      ShortCut = 16454
      OnExecute = actEditFindExecute
      OnUpdate = actEditFindUpdate
    end
    object actEditFindNext: TAction
      Category = 'Edit'
      Caption = 'Find Next'
      ShortCut = 114
      OnExecute = actEditFindNextExecute
      OnUpdate = actEditFindNextUpdate
    end
    object actToolsAutoSpellcheck: TAction
      Category = 'Tools'
      Caption = 'Auto Spellcheck'
      OnExecute = actToolsAutoSpellcheckExecute
      OnUpdate = actToolsAutoSpellcheckUpdate
    end
    object actToolsIgnoreWordsWithNumbers: TAction
      Category = 'Tools'
      Caption = 'Ignore words with numbers'
      OnExecute = actToolsIgnoreWordsWithNumbersExecute
      OnUpdate = actToolsIgnoreWordsWithNumbersUpdate
    end
    object actEditDateTime: TAction
      Category = 'Edit'
      Caption = 'Date/Time'
      Hint = 'Date/Time|Insert Date/Time'
      OnExecute = actEditDateTimeExecute
    end
    object actViewRightEdge: TAction
      Category = 'View'
      Caption = 'Right Edge'
      OnExecute = actViewRightEdgeExecute
      OnUpdate = actViewRightEdgeUpdate
    end
    object actToolsIgnoreAllCaps: TAction
      Category = 'Tools'
      Caption = 'Ignore All Caps'
      OnExecute = actToolsIgnoreAllCapsExecute
      OnUpdate = actToolsIgnoreAllCapsUpdate
    end
    object actToolsTabsToSpace: TAction
      Category = 'Tools'
      Caption = 'Tabs to Space'
      Hint = 'Tabs to Space|Replace Tabs with Space'
      OnExecute = actToolsTabsToSpaceExecute
      OnUpdate = actToolsTabsToSpaceUpdate
    end
    object actHelpWeb: TAction
      Category = 'Help'
      Caption = 'Web Site'
      Hint = 'Web Site|Open Web Site of THunSpell'
      OnExecute = actHelpWebExecute
    end
  end
  object imlMain: TImageList
    Left = 560
    Top = 376
  end
  object HunSpell: THunSpell
    CustomDic = True
    IgnoreWordsWithNumbers = False
    IgnoreAllCaps = False
    Left = 616
    Top = 504
  end
  object HunSpellDialog: THunSpellDialog
    HunSpell = HunSpell
    Memo = txtMain
    Left = 736
    Top = 504
  end
  object OpenDialog: TOpenDialog
    Filter = 'All files (*.*)|*.*'
    Left = 352
    Top = 480
  end
  object SaveDialog: TSaveDialog
    Filter = 'All files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 488
    Top = 472
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Options = []
    Left = 592
    Top = 440
  end
  object FindDialog: TFindDialog
    Options = [frDown, frHideWholeWord, frHideUpDown]
    OnFind = FindDialogFind
    Left = 656
    Top = 392
  end
end
