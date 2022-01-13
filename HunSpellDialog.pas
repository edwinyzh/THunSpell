{
  THunSpellDialog
  Copyright (C) 2010, Stefan Ascher

  @abstract(Component to implement spell checking with a Dialog.)

  @author(Stefan Ascher <stievie@inode.at>)
  @created(25-11-2010)
  @cvs($Date: 2010/12/13 03:21:41 $)

  $Id: HunSpellDialog.pas,v 1.3 2010/12/13 03:21:41 Stefan Ascher Exp $
}

unit HunSpellDialog;

{$i HunSpell.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  HunSpell, StdCtrls, dlgCheckSpelling;

type
  { Result of @link(CorrectWord) }
  TCorrectResult = (
    crChange,        {< Change the word }
    crNoChange,      {< Don't change the word }
    crCancel         {< Stop checking }
  );
  {
    Fired before correcting a word.
    @param Sender The sending object.
    @param AWord The word to correct.
    @param ASkip If @true it skips this word.
  }
  TCorrectEvent = procedure(Sender: TObject; AWord: string; var ASkip: boolean) of object;
  {
    Finished with checking the Memo.
    @param Sender The sending object.
    @param ACanceled Is @true when checking was canceled.
  }
  TFinishedEvent = procedure(Sender: TObject; const ACanceled: boolean) of object;

  {
    Component for check spelling a TMemo with a dialog.
  }
  THunSpellDialog = class(TComponent)
  private
    { Private declarations }
    fHunSpell: THunSpell;
    fMemo: TCustomMemo;
    fOnCorrect: TCorrectEvent;
    fOnCheckStart: TNotifyEvent;
    fOnCheckFinished: TFinishedEvent;
    function InternalExecute: boolean;
    {
      Get the word at a certain position.
      @param APos The line and col of the word.
      @param AStart The start position of the returned word.
      @param AEnd The end position of the word.
      @param The word.
    }
    function GetWordAt(const APos: TPoint; out AStart, AEnd: integer): string;
    {
      Check a single word.
      @param AWord The word to check.
      @return @true when the word is correctly spelled.
    }
    function SpellCheck(const AWord: string): boolean;
    {
      Correct a wrong word. Shows the dialog.
      @param ADlg The spell checker dialog.
      @param AWord The wrong word.
      @param AChange The new word.
      @return crChange to change the word; crNoChange to not change the word; crCancel to close the dialog.
    }
    function CorrectWord(ADlg: TCheckSpellingDialog; const AWord: string; out AChange: string): TCorrectResult;
    function GetWordPos(const APos: TPoint; out AStart, AEnd: integer): boolean;
    function GetLineOffset(const ALn: integer): integer;
    function GetVisibleLines: integer;
  protected
    { Protected declarations }
    procedure DoOnCorrect(AWord: string; var ASkip: boolean); dynamic;
  public
    { Public declarations }
    {
      Check spelling.
      @return @true when spell checking was not canceled.
    }
    function Execute: boolean;
  published
    { Published declarations }
    { @link(THunSpell) object to use for spell checking. }
    property HunSpell: THunSpell read fHunSpell write fHunSpell;
    { Memo to check for spelling errors. }
    property Memo: TCustomMemo read fMemo write fMemo;

    { Correct a word. }
    property OnCorrect: TCorrectEvent read fOnCorrect write fOnCorrect;
    { Start checking }
    property OnCheckStart: TNotifyEvent read fOnCheckStart write fOnCheckStart;
    { Finished with checking }
    property OnCheckFinished: TFinishedEvent read fOnCheckFinished write fOnCheckFinished;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Additional', [THunSpellDialog]);
end;

{ THunSpellDialog }

function THunSpellDialog.CorrectWord(ADlg: TCheckSpellingDialog; const AWord: string; out AChange: string): TCorrectResult;
begin
  ADlg.Word := AWord;
  fHunSpell.Suggest(AWord, ADlg.Suggestions);
  case ADlg.ShowModal of
    mrOK:
      begin
        // Change
        AChange := ADlg.ChangeTo;
        Result := crChange;
      end;
    mrIgnore:
      begin
        // Ignore all
        fHunSpell.Ignore(AWord);
        Result := crNoChange;
      end;
    mrYes:
      begin
        // Add
        fHunSpell.Add(AWord);
        Result := crNoChange;
      end;
    mrNo:
      begin
        // Ignore this
        Result := crNoChange;
      end;
    else
      // Close
      Result := crCancel;
  end;
end;

function THunSpellDialog.Execute: boolean;
begin
  if Assigned(fHunSpell) and fHunSpell.Initialized and Assigned(fMemo) then begin
    Result := InternalExecute;
  end else
    Result := false;
end;

procedure THunSpellDialog.DoOnCorrect(AWord: string; var ASkip: boolean);
begin
  if Assigned(fOnCorrect) then
    fOnCorrect(Self, AWord, ASkip);
end;

function THunSpellDialog.InternalExecute: boolean;
  procedure ScollIntoView(const ALine: integer);
  var
    fl, ll: integer;
  begin
    fl := SendMessage(fMemo.Handle, EM_GETFIRSTVISIBLELINE, 0, 0);
    ll := fl + GetVisibleLines;
    if (Aline < fl) or (ALine >= ll) then
      SendMessage(fMemo.Handle, EM_SCROLLCARET, 0, 0);
  end;
var
  i, c: integer;
  ln, Word, WordChange: string;
  dlg: TCheckSpellingDialog;
  p: TPoint;
  ws, we, ss, se, off: integer;
  cancel, skip: boolean;
begin
  if Assigned(fOnCheckStart) then
    fOnCheckStart(Self);
  dlg := TCheckSpellingDialog.Create(Owner);
  try
    dlg.btnAdd.Visible := fHunSpell.CustomDic;
    cancel := false;
    c := fMemo.Lines.Count;
    for i := 0 to c - 1 do begin
      ln := fMemo.Lines[i];
      p.x := 0;
      if Trim(ln) <> '' then begin
        p.y := i;
        Word := GetWordAt(p, ws, we);
        while (we < Length(fMemo.Lines[i])) or (Word <> '') do begin
          p.x := we + 1;
          if Length(Word) > 1 then begin
            if not SpellCheck(Word) then begin
              skip := false;
              DoOnCorrect(Word, skip);
              if not skip then begin
                off := GetLineOffset(i);
                ss := ws + off;
                se := we + off;
                SendMessage(fMemo.Handle, EM_SETSEL, ss, se-1);
                ScollIntoView(i);
                case CorrectWord(dlg, Word, WordChange) of
                  crChange:
                    begin
                      fMemo.SelText := WordChange;
                      p.x := fMemo.SelStart + 1;
                    end;
                  crCancel:
                    begin
                      cancel := true;
                      Break;
                    end;
                end; { case }
              end; { if not skip }
            end; { if not SpellCheck }
          end; { if Word <> '' }
          Word := GetWordAt(p, ws, we);
        end; { while }
      end; { if Trim(ln) <> '' }
      if cancel then
        Break;
    end; { for }
  finally
    dlg.Free;
  end;
  if Assigned(fOnCheckFinished) then
    fOnCheckFinished(Self, cancel);
  Result := not cancel;
end;

function THunSpellDialog.SpellCheck(const AWord: string): boolean;
begin
  if Assigned(fHunSpell) then
    Result := fHunSpell.Spell(AWord)
  else
    Result := true;
end;

function THunSpellDialog.GetWordAt(const APos: TPoint; out AStart, AEnd: integer): string;
var
  sln: string;
begin
  if GetWordPos(APos, AStart, AEnd) then begin
    sln := fMemo.Lines[APos.y];
    Result := Copy(sln, AStart + 1, AEnd - AStart - 1);
  end else
    Result := '';
end;

function THunSpellDialog.GetWordPos(const APos: TPoint; out AStart, AEnd: integer): boolean;
var
  ln: integer;
  sln: string;
begin
  ln := APos.y;
  if ln < fMemo.Lines.Count then begin
    sln := fMemo.Lines[ln];
    if (Length(sln) > 0) and (APos.x <= Length(sln)) then begin
      AStart := APos.x;
      while (AStart > 0) and not IsSeparator(sln[AStart]) do
        Dec(AStart);
      AEnd := APos.x;
      if AEnd = 0 then
        AEnd := 1;
      while (AEnd <= Length(sln)) and not IsSeparator(sln[AEnd]) do
        Inc(AEnd);
      if (AStart < AEnd) and (AStart >= 0) and (AEnd <= Length(sln)) then begin
        if (sln[AStart+1] = '''') and (sln[AEnd-1] = '''') then begin
          // Delete single quotes
          Inc(AStart);
          Dec(AEnd);
        end;
      end;
      Result := true;
    end else
      Result := false;
  end else
    Result := false;
end;

function THunSpellDialog.GetLineOffset(const ALn: integer): integer;
begin
  Result := SendMessage(fMemo.Handle, EM_LINEINDEX, ALn, 0);
end;

function THunSpellDialog.GetVisibleLines: integer;
var
  OldFont: HFont;
  DC: THandle;
  TextMetric: TTextMetric;
  cr: TRect;
begin
  DC := GetDC(fMemo.Handle);
  try
    OldFont := SelectObject(DC, TMemo(fMemo).Font.Handle);
    try
      GetTextMetrics(DC, TextMetric);
      cr := fMemo.ClientRect;
      Result := (cr.Bottom - cr.Top) div (TextMetric.tmHeight + TextMetric.tmExternalLeading);
    finally
      SelectObject(DC, OldFont);
    end;
  finally
    ReleaseDC(fMemo.Handle, DC);
  end;
end;

end.
 