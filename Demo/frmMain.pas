{
  THunSpell Demo
  Copyright (C) 2010, Stefan Ascher

  @abstract(Demo application for THunSpell)

  @author(Stefan Ascher <stievie@inode.at>)
  @created(28-11-2010)
  @cvs($Date: 2011/03/23 12:18:49 $)

  $Id: frmMain.pas,v 1.6 2011/03/23 12:18:49 Stefan Ascher Exp $
}

unit frmMain;

{$i HunSpell.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ActnList, ToolWin, ComCtrls, ImgList, HunSpellDialog, HunSpell,
  HunSpellMemo, StdCtrls, System.Actions;

type
  TMainForm = class(TForm)
    nmuMain: TMainMenu;
    actlMain: TActionList;
    imlMain: TImageList;
    HunSpell: THunSpell;
    HunSpellDialog: THunSpellDialog;
    actFileOpen: TAction;
    File1: TMenuItem;
    Open1: TMenuItem;
    actFileNew: TAction;
    actFileSave: TAction;
    actFileSaveAs: TAction;
    actFileExit: TAction;
    New1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    actEditUndo: TAction;
    actEditCut: TAction;
    actEditCopy: TAction;
    actEditPaste: TAction;
    actHelpAbout: TAction;
    Edit1: TMenuItem;
    Help1: TMenuItem;
    Undo1: TMenuItem;
    N2: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Aboud1: TMenuItem;
    actToolsCheckSpelling: TAction;
    Tools1: TMenuItem;
    CheclSpelling1: TMenuItem;
    txtMain: THunSpellMemo;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    mnuDictionary: TMenuItem;
    FontDialog: TFontDialog;
    actFormatFont: TAction;
    Font1: TMenuItem;
    Format1: TMenuItem;
    actFormatWordWrap: TAction;
    WordWrap1: TMenuItem;
    actEditSelectAll: TAction;
    N3: TMenuItem;
    SelectAll1: TMenuItem;
    actEditDelete: TAction;
    Delete1: TMenuItem;
    sbMain: TStatusBar;
    actViewStatusbar: TAction;
    View1: TMenuItem;
    StatusBar1: TMenuItem;
    actUpdateStatusbar: TAction;
    FindDialog: TFindDialog;
    actEditFind: TAction;
    N4: TMenuItem;
    actEditFind1: TMenuItem;
    actEditFindNext: TAction;
    FindNext1: TMenuItem;
    actToolsAutoSpellcheck: TAction;
    AutoSpellcheck1: TMenuItem;
    actToolsIgnoreWordsWithNumbers: TAction;
    Ignorewordswithnumbers1: TMenuItem;
    N5: TMenuItem;
    actEditDateTime: TAction;
    DateTime1: TMenuItem;
    actViewRightEdge: TAction;
    RightEdge1: TMenuItem;
    actToolsIgnoreAllCaps: TAction;
    IgnoreAllCaps1: TMenuItem;
    actToolsTabsToSpace: TAction;
    N6: TMenuItem;
    TabstoSpace1: TMenuItem;
    actHelpWeb: TAction;
    WebSite1: TMenuItem;
    procedure actFileNewExecute(Sender: TObject);
    procedure actFileOpenExecute(Sender: TObject);
    procedure actToolsCheckSpellingExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mnuDictionaryClick(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure actFileSaveAsExecute(Sender: TObject);
    procedure actFileSaveUpdate(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure actEditUndoExecute(Sender: TObject);
    procedure actEditUndoUpdate(Sender: TObject);
    procedure actEditCutExecute(Sender: TObject);
    procedure actEditCutUpdate(Sender: TObject);
    procedure actEditCopyExecute(Sender: TObject);
    procedure actEditPasteExecute(Sender: TObject);
    procedure actEditPasteUpdate(Sender: TObject);
    procedure actHelpAboutExecute(Sender: TObject);
    procedure actFormatFontExecute(Sender: TObject);
    procedure actFormatWordWrapUpdate(Sender: TObject);
    procedure actFormatWordWrapExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actEditSelectAllExecute(Sender: TObject);
    procedure actEditDeleteExecute(Sender: TObject);
    procedure actEditDeleteUpdate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure actViewStatusbarUpdate(Sender: TObject);
    procedure actViewStatusbarExecute(Sender: TObject);
    procedure actUpdateStatusbarUpdate(Sender: TObject);
    procedure FindDialogFind(Sender: TObject);
    procedure actEditFindUpdate(Sender: TObject);
    procedure actEditFindExecute(Sender: TObject);
    procedure actEditFindNextExecute(Sender: TObject);
    procedure actEditFindNextUpdate(Sender: TObject);
    procedure actToolsAutoSpellcheckUpdate(Sender: TObject);
    procedure actToolsAutoSpellcheckExecute(Sender: TObject);
    procedure txtMainKeyPress(Sender: TObject; var Key: Char);
    procedure actToolsIgnoreWordsWithNumbersUpdate(Sender: TObject);
    procedure actToolsIgnoreWordsWithNumbersExecute(Sender: TObject);
    procedure actEditDateTimeExecute(Sender: TObject);
    procedure actViewRightEdgeUpdate(Sender: TObject);
    procedure actViewRightEdgeExecute(Sender: TObject);
    procedure actToolsIgnoreAllCapsExecute(Sender: TObject);
    procedure actToolsIgnoreAllCapsUpdate(Sender: TObject);
    procedure actToolsTabsToSpaceExecute(Sender: TObject);
    procedure actToolsTabsToSpaceUpdate(Sender: TObject);
    procedure actHelpWebExecute(Sender: TObject);
  private
    { Private declarations }
    fFilename: string;
    fSelPos: integer;
    fTabToSpace: boolean;
    fSpaceCount: integer;
    procedure WMDropFiles(var Message: TWMDropFiles); message WM_DROPFILES;
    procedure Dictionary_Click(Sender: TObject);
    function AskSave: boolean;
    function DoSave: boolean;
    procedure LoadFile(const AFilename: string);
    procedure UpdateCaption;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure SetWordWrap(const AWrap: boolean);
    procedure App_Hint(Sender: TObject);
    procedure FindText(const ANext: boolean);
    procedure WMSyscommand(var Message: TWmSysCommand); message WM_SYSCOMMAND;
  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public declarations }
    { Application.MessageBox would be modal to the Application window. We want it
      to be modal to the active form or this window. }
    function MessageBox(const Text, Caption: PChar; Flags: Longint): Integer;
  end;

var
  MainForm: TMainForm;

implementation

uses
  ClipBrd, ShellApi, Registry;

resourcestring
  SAppTitle = 'THunSpell Demo';

{$R *.DFM}

procedure TMainForm.WMDropFiles(var Message: TWMDropFiles);
var
  buffer: array[0..MAX_PATH] of Char;
begin
  inherited;
  //we're only interested in the first file...
  DragQueryFile(Message.Drop, 0, @buffer, SizeOf(buffer));
  Message.Result := Integer(true);
  if AskSave then
    LoadFile(buffer);
end;

procedure TMainForm.CreateWnd;
begin
  inherited;
  DragAcceptFiles(Handle, true);
end;

procedure TMainForm.DestroyWnd;
begin
  DragAcceptFiles(Handle, false);
  inherited;
end;

procedure TMainForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);  
  Params.ExStyle := Params.ExStyle and not WS_EX_TOOLWINDOW or
    WS_EX_APPWINDOW;
end;

procedure TMainForm.WMSyscommand(var Message: TWmSysCommand);
begin
  case (Message.CmdType and $FFF0) of
    SC_MINIMIZE:
    begin
      ShowWindow(Handle, SW_MINIMIZE);
      Message.Result := 0;
    end;
    SC_RESTORE:
    begin
      ShowWindow(Handle, SW_RESTORE);
      Message.Result := 0;
    end;
  else
    inherited;  
  end;
end;

function TMainForm.MessageBox(const Text, Caption: PChar; Flags: Longint): Integer;
var
  o: HWND;
begin
  if Assigned(Screen.ActiveForm) then
    o := Screen.ActiveForm.Handle
  else
    o := Self.Handle;
  Result := Windows.MessageBox(o, Text, Caption, Flags);
end;

procedure TMainForm.actFileNewExecute(Sender: TObject);
begin
  if AskSave then begin
    txtMain.Text := '';
    fFilename := '';
  end;
end;

procedure TMainForm.actFileOpenExecute(Sender: TObject);
begin
  if AskSave then begin
    if OpenDialog.Execute then begin
      LoadFile(OpenDialog.Filename);
    end;
  end;
end;

procedure TMainForm.actToolsCheckSpellingExecute(Sender: TObject);
begin
  if HunSpellDialog.Execute then
    Self.MessageBox('Spell checking finished.', 'Spell Check', MB_ICONINFORMATION);
end;

function TMainForm.AskSave: boolean;
begin
  if txtMain.Modified then begin
    case Self.MessageBox('File was modified. Save it?', PChar(SAppTitle), MB_ICONQUESTION or MB_YESNOCANCEL) of
      IDYES: Result := DoSave;
      IDNO: Result := true;
      IDCANCEL: Result := false;
    else
      Result := true;
    end;
  end else
    Result := true;
end;

procedure TMainForm.Dictionary_Click(Sender: TObject);
begin
  HunSpell.Dict := StripAccelChar((Sender as TMenuItem).Caption);
  txtMain.Refresh;
end;

function TMainForm.DoSave: boolean;
begin
  if fFilename = '' then begin
    Result := SaveDialog.Execute;
    if Result then
      fFilename := SaveDialog.FileName;
  end else
    Result := true;
  if Result then begin
    txtMain.Lines.SaveToFile(fFilename);
    txtMain.Modified := false;
  end;
  UpdateCaption;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  AppPath: string;
  DictPath: string;
  sl: TStringList;
  mi: TMenuItem;
  i: integer;
begin
  ShowWindow(Application.Handle, SW_HIDE);
  SetWindowLong(Application.Handle, GWL_EXSTYLE,
    GetWindowLong(Application.Handle, GWL_EXSTYLE) and not WS_EX_APPWINDOW
    or WS_EX_TOOLWINDOW);
  ShowWindow(Application.Handle, SW_SHOW);
  AppPath := ExcludeTrailingBackslash(ExtractFilePath(Application.ExeName));
  DictPath := ExcludeTrailingBackslash(AppPath) + '\Dict';
  HunSpell.DictDir := DictPath;
  sl := TStringList.Create;
  try
    HunSpell.GetDicts(sl);
    for i := 0 to sl.Count - 1 do begin
      mi := TMenuItem.Create(nmuMain);
      mi.Caption := sl[i];
      mi.OnClick := Dictionary_Click;
      mnuDictionary.Add(mi);
    end;
    if sl.Count > 0 then
      HunSpell.Dict := sl[0];
  finally
    sl.Free;
  end;
  LoadSettings;
  Application.OnHint := App_Hint;
  UpdateCaption;
  if (ParamCount > 0) and FileExists(ParamStr(1)) then
    LoadFile(ParamStr(1));
end;

procedure TMainForm.mnuDictionaryClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to mnuDictionary.Count - 1 do begin
    mnuDictionary.Items[i].Checked := StripAccelChar(mnuDictionary.Items[i].Caption) = HunSpell.Dict;
  end;
end;

procedure TMainForm.actFileSaveExecute(Sender: TObject);
begin
  DoSave;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := AskSave;
end;

procedure TMainForm.actFileSaveAsExecute(Sender: TObject);
begin
  if fFilename <> '' then begin
    SaveDialog.Filename := ExtractFileName(fFilename);
  end;
  if SaveDialog.Execute then begin
    fFilename := SaveDialog.FileName;
    DoSave;
  end;
end;

procedure TMainForm.actFileSaveUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := txtMain.Text <> '';
end;

procedure TMainForm.actFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.actEditUndoExecute(Sender: TObject);
begin
  txtMain.Undo;
end;

procedure TMainForm.actEditUndoUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := txtMain.CanUndo;
end;

procedure TMainForm.actEditCutExecute(Sender: TObject);
begin
  txtMain.CutToClipboard;
end;

procedure TMainForm.actEditCutUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := txtMain.SelLength > 0;
end;

procedure TMainForm.actEditCopyExecute(Sender: TObject);
begin
  txtMain.CopyToClipboard;
end;

procedure TMainForm.actEditPasteExecute(Sender: TObject);
begin
  txtMain.PasteFromClipboard;
end;

procedure TMainForm.actEditPasteUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Clipboard.HasFormat(CF_TEXT);
end;

procedure TMainForm.actHelpAboutExecute(Sender: TObject);
begin
  Self.MessageBox(
    'THunSpell Demo by Stefan Ascher <stievie@inode.at>'#13#10'<http://stievie.bplaced.net/dev/#thunspell>'#13#10'Hunspell <http://hunspell.sourceforge.net/>',
    'About', MB_ICONINFORMATION);
end;

procedure TMainForm.LoadFile(const AFilename: string);
begin
  fFilename := AFilename;
  txtMain.Lines.LoadFromFile(fFilename);
  txtMain.Modified := false;
  UpdateCaption;
end;

procedure TMainForm.UpdateCaption;
var
  fn: string;
begin
  if fFilename <> '' then
    fn := ExtractFileName(fFilename)
  else
    fn := 'Unnamed';
  Caption := fn + ' - ' + SAppTitle;
end;

procedure TMainForm.actFormatFontExecute(Sender: TObject);
begin
  FontDialog.Font := txtMain.Font;
  if FontDialog.Execute then
    txtMain.Font.Assign(FontDialog.Font);
end;

procedure TMainForm.actFormatWordWrapUpdate(Sender: TObject);
begin
  (Sender as TAction).Checked := txtMain.WordWrap;
end;

procedure TMainForm.actFormatWordWrapExecute(Sender: TObject);
begin
  SetWordWrap(not txtMain.WordWrap);
end;

procedure TMainForm.LoadSettings;
var
  ff: TFontStyles;
begin
  with TRegistry.Create(KEY_READ) do try
    RootKey := HKEY_CURRENT_USER;
    if OpenKey('Software\Ascher\THunSpell Demo', false) then begin
      try
        HunSpell.Dict := ReadString('Dict');
        SetWordWrap(ReadBool('WordWrap'));
        if not ReadBool('AutoSpellcheck') then
          txtMain.HunSpell := nil;
        sbMain.Visible := ReadBool('Statusbar');
        fTabToSpace := ReadBool('TabToSpace');
        fSpaceCount := ReadInteger('SpaceCount');
        if fSpaceCount = 0 then
          fSpaceCount := 2;
        with txtMain.Font do begin
          Name := ReadString('Font.Name');
          Size := ReadInteger('Font.Size');
          ff := [];
          if ReadBool('Font.Style.Bold') then
            Include(ff, fsBold);
          if ReadBool('Font.Style.Italic') then
            Include(ff, fsItalic);
          if ReadBool('Font.Style.Underline') then
            Include(ff, fsUnderline);
          if ReadBool('Font.Style.StrikeOut') then
            Include(ff, fsStrikeOut);
          Style := ff;
        end;
        HunSpell.IgnoreWordsWithNumbers := ReadBool('IgnoreWordsWithNumbers');
        txtMain.RightEdge := ReadInteger('RightEdge');
      except end;
    end;
  finally
    Free;
  end;
end;

procedure TMainForm.SaveSettings;
begin
  with TRegistry.Create(KEY_WRITE) do try
    RootKey := HKEY_CURRENT_USER;
    if OpenKey('Software\Ascher\THunSpell Demo', true) then begin
      WriteString('Dict', HunSpell.Dict);
      WriteBool('WordWrap', txtMain.WordWrap);
      WriteBool('AutoSpellcheck',Assigned(txtMain.HunSpell));
      WriteBool('TabToSpace', fTabToSpace);
      WriteInteger('SpaceCount', fSpaceCount);
      WriteBool('Statusbar', sbMain.Visible);
      with txtMain.Font do begin
        WriteString('Font.Name', Name);
        WriteInteger('Font.Size', Size);
        WriteBool('Font.Style.Bold', fsBold in Style);
        WriteBool('Font.Style.Italic', fsItalic in Style);
        WriteBool('Font.Style.Underline', fsUnderline in Style);
        WriteBool('Font.Style.StrikeOut', fsStrikeOut in Style);
      end;
      WriteBool('IgnoreWordsWithNumbers', HunSpell.IgnoreWordsWithNumbers);
      WriteInteger('RightEdge', txtMain.RightEdge);
    end;
  finally
    Free;
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveSettings;
end;

procedure TMainForm.SetWordWrap(const AWrap: boolean);
begin
  txtMain.WordWrap := AWrap;
  if AWrap then begin
    txtMain.ScrollBars := ssVertical;
  end else begin
    txtMain.ScrollBars := ssBoth;
  end;
end;

procedure TMainForm.actEditSelectAllExecute(Sender: TObject);
begin
  txtMain.SelectAll;
end;

procedure TMainForm.actEditDeleteExecute(Sender: TObject);
begin
  txtMain.SelText := '';
end;

procedure TMainForm.actEditDeleteUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := txtMain.SelLength > 0;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  sbMain.Panels[0].Width := ClientWidth - 200;
end;

procedure TMainForm.actViewStatusbarUpdate(Sender: TObject);
begin
  (Sender as TAction).Checked := sbMain.Visible;
end;

procedure TMainForm.actViewStatusbarExecute(Sender: TObject);
begin
  sbMain.Visible := not sbMain.Visible;
end;

procedure TMainForm.actUpdateStatusbarUpdate(Sender: TObject);
var
  cp: TPoint;
begin
  if (Sender is TWinControl) then
    (Sender as TWinControl).Enabled := true;
  cp := txtMain.CaretPos;
  sbMain.Panels[1].Text := Format('  Ln %d, Col %d', [cp.y+1, cp.x+1]);
end;

procedure TMainForm.App_Hint(Sender: TObject);
begin
  sbMain.Panels[0].Text := GetLongHint(Application.Hint);
end;

procedure TMainForm.FindDialogFind(Sender: TObject);
begin
  with TFindDialog(Sender) do begin
    {If the stored position is 0 this cannot be a find next. }
    if fSelPos = 0 then
      Options := Options - [frFindNext];
    Self.FindText(frFindNext in Options);
  end;
end;

procedure TMainForm.actEditFindUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := txtMain.Text <> '';
end;

procedure TMainForm.actEditFindExecute(Sender: TObject);
begin
  fSelPos := 0;
  FindDialog.Execute;
end;

procedure TMainForm.FindText(const ANext: boolean);
var
  S, ft: string;
  StartPos: integer;
begin
  with FindDialog do begin
    { Figure out where to start the search and get the corresponding
      text from the memo. }
    if ANext then begin
      { This is a find next, start after the end of the last found word. }
      StartPos := fSelPos + Length(FindText);
      S := Copy(txtMain.Lines.Text, StartPos, MaxInt);
    end else begin
      { This is a find first, start at the, well, start. }
      S := txtMain.Lines.Text;
      StartPos := 1;
    end;
    { Perform a global search for FindText in S }
    if frMatchCase in Options then begin
      ft := FindText;
    end else begin
      ft := LowerCase(FindText);
      S := LowerCase(S);
    end;
    fSelPos := Pos(ft, S);
    if fSelPos > 0 then begin
      { Found something, correct position for the location of the start
        of search. }
      fSelPos := fSelPos + StartPos - 1;
      txtMain.SelStart := fSelPos - 1;
      txtMain.SelLength := Length(FindText);
      txtMain.SetFocus;
    end else begin
      { No joy, show a message. }
      if ANext then
        S := Concat('There are no further occurences of "', FindText, '".')
      else
        S := Concat('Could not find "', FindText, '".');
      Self.MessageBox(PChar(S), 'Find', MB_ICONINFORMATION);
    end;
  end;
end;

procedure TMainForm.actEditFindNextExecute(Sender: TObject);
begin
  FindText(true);
end;

procedure TMainForm.actEditFindNextUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (txtMain.Text <> '') and (FindDialog.FindText <> '');
end;

procedure TMainForm.actToolsAutoSpellcheckUpdate(Sender: TObject);
begin
  (Sender as TAction).Checked := Assigned(txtMain.HunSpell);
end;

procedure TMainForm.actToolsAutoSpellcheckExecute(Sender: TObject);
begin
  if Assigned(txtMain.HunSpell) then
    txtMain.HunSpell := nil
  else
    txtMain.HunSpell := HunSpell;
end;

procedure TMainForm.txtMainKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #9) and fTabToSpace then begin
    txtMain.SelText := StringOfChar(' ', fSpaceCount);
    Key := #0;
  end;
end;

procedure TMainForm.actToolsIgnoreWordsWithNumbersUpdate(Sender: TObject);
begin
  (Sender as TAction).Checked := HunSpell.IgnoreWordsWithNumbers;
end;

procedure TMainForm.actToolsIgnoreWordsWithNumbersExecute(Sender: TObject);
begin
  HunSpell.IgnoreWordsWithNumbers := not HunSpell.IgnoreWordsWithNumbers;
  txtMain.Refresh;
end;

procedure TMainForm.actEditDateTimeExecute(Sender: TObject);
begin
  txtMain.SelText := DateTimeToStr(Now);
end;

procedure TMainForm.actViewRightEdgeUpdate(Sender: TObject);
begin
  (Sender as TAction).Checked := txtMain.RightEdge > 0;
end;

procedure TMainForm.actViewRightEdgeExecute(Sender: TObject);
begin
  if txtMain.RightEdge = 0 then
    txtMain.RightEdge := 80
  else
    txtMain.RightEdge := 0;
end;

procedure TMainForm.actToolsIgnoreAllCapsExecute(Sender: TObject);
begin
  HunSpell.IgnoreAllCaps := not HunSpell.IgnoreAllCaps;
  txtMain.Refresh;
end;

procedure TMainForm.actToolsIgnoreAllCapsUpdate(Sender: TObject);
begin
  (Sender as TAction).Checked := HunSpell.IgnoreAllCaps;
end;

procedure TMainForm.actToolsTabsToSpaceExecute(Sender: TObject);
begin
  fTabToSpace := not fTabToSpace;
end;

procedure TMainForm.actToolsTabsToSpaceUpdate(Sender: TObject);
begin
  (Sender as TAction).Checked := fTabToSpace;
end;

procedure TMainForm.actHelpWebExecute(Sender: TObject);
begin
  ShellExecute(0, nil, 'http://stievie.bplaced.net/dev/index.php#thunspell', nil, nil, SW_SHOWNORMAL);
end;

end.
