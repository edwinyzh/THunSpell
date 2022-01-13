{
  THunSpellMemo
  Copyright (C) 2010, Stefan Ascher

  @abstract(TMemo descendant for real-time spell checking.)

  It checks only words visible to the user, so there shouldn't be performance
  issues.

  Flickers a bit, and, well, leaves lot of space for improvements...

  @author(Stefan Ascher <stievie@inode.at>)
  @created(02-10-2010)
  @cvs($Date: 2011/02/23 08:56:14 $)

  $Id: HunSpellMemo.pas,v 1.8 2011/02/23 08:56:14 Stefan Ascher Exp $
}

unit HunSpellMemo;

{$i HunSpell.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, HunSpell, Menus;

type
  THunSpellMemo = class(TMemo)
  private
    { Private declarations }
    fHunSpell: THunSpell;
    fUnderlineColor: TColor;
    fSelLen: integer;
    fRightClickMoveCaret: boolean;
    fSuggMenu: TPopupMenu;
    fAutoSuggest: boolean;
    fAddMenu: boolean;
    fWordToAdd: string;
    fRightEdge: integer;
    fRightEdgeColor: TColor;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMNotify(var Message: TWMNotify); message WM_NOTIFY;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMContextMenu(var Message: TWMContextMenu); message WM_CONTEXTMENU;
    procedure EMSetSel(var Message: TMessage); message EM_SETSEL;
    procedure EMCharFromPos(var Message: TMessage); message EM_CHARFROMPOS;
    function GetVisibleLines: integer;
    function NextWord(var Line, Prev: string): string;
    function SpellCheck(const AWord: string): boolean;
    function GetTopLine: integer;
    procedure SetHunSpell(Value: THunSpell);
    procedure SetUnderlineColor(Value: TColor);
    function SelChanged: boolean;
    function GetWordPos(const APos: TPoint; out AStart, AEnd: integer): boolean;
    procedure Sugg_Click(Sender: TObject);
    procedure Ignore_Click(Sender: TObject);
    procedure Add_Click(Sender: TObject);
    function GetBorderWidth: integer;
    procedure SetRightEdge(Value: integer);
    procedure SetRightEdgeColor(Value: TColor);
    function GetMaxLineWidth: integer;
  protected
    { Protected declarations }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Change; override;
    procedure SetCaretPos(Value: TPoint);
    function GetCaretPos: TPoint; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    { Count of visible lines. }
    property VisibleLines: integer read GetVisibleLines;
    { Caret position. }
    property CaretPos: TPoint read GetCaretPos write SetCaretPos;
    {
      Get word at position.
      @param APos The position.
      @return The word.
    }
    function GetWordAt(const APos: TPoint): string;
    {
      Get word at caret position.
      @return The word.
    }
    function GetWordAtCaret: string;
    {
      Replace word at position.
      @param APos The position.
      @param ANew The new word.
    }
    procedure ReplaceWordAt(const APos: TPoint; const ANew: string);
    {
      Replace word at caret position.
      @param ANew The new word.
    }
    procedure ReplaceWordAtCaret(const ANew: string);
    { Redraw the window }
    procedure Refresh;

    { Max line width in pixel. }
    property MaxLineWidth: integer read GetMaxLineWidth;
  published
    { Published declarations }
    { @link(THunSpell) object to use for spell checking. }
    property HunSpell: THunSpell read fHunSpell write SetHunSpell;
    { If @true it automatically pop ups a menu with suggestions for wrong words. }
    property AutoSuggest: boolean read fAutoSuggest write fAutoSuggest default true;
    { If @true appends an Add word menu item to the suggestion menu. }
    property AddMenu: boolean read fAddMenu write fAddMenu default false;
    { If @true the caret pos is moved to the cursor pos with a right click. }
    property RightClickMoveCaret: boolean read fRightClickMoveCaret write fRightClickMoveCaret;
    { Color to underline words. }
    property UnderlineColor: TColor read fUnderlineColor write SetUnderlineColor default clRed;
    { Show a line at char pos. }
    property RightEdge: integer read fRightEdge write SetRightEdge;
    { Color of the line. }
    property RightEdgeColor: TColor read fRightEdgeColor write SetRightEdgeColor default clSilver;
  end;

procedure Register;
{ Remove the accelerator char (&) from a string }
function StripAccelChar(const s: string): string;

implementation

uses
  Math;
  
procedure Register;
begin
  RegisterComponents('Additional', [THunSpellMemo]);
end;

function StripAccelChar(const s: string): string;
var
  i, c: integer;
begin
  Result := s;
  c := Length(s);
  for i := c downto 1 do
    if Result[i] = '&' then
      Delete(Result, i, 1);
end;

{ THunSpellMemo }

constructor THunSpellMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fHunSpell := nil;
  fUnderlineColor := clRed;
  fSelLen := 0;
  fSuggMenu := TPopupMenu.Create(Self);
  fAutoSuggest := true;
  fRightEdgeColor := clSilver;
end;

procedure THunSpellMemo.Change;
begin
  Refresh;
  inherited;
end;

procedure THunSpellMemo.WMNotify(var Message: TWMNotify);
begin
  inherited;
end;

procedure THunSpellMemo.EMSetSel(var Message: TMessage);
begin
  inherited;
  fSelLen := SelLength;
end;

procedure THunSpellMemo.Sugg_Click(Sender: TObject);
var
  mi: TMenuItem;
begin
  mi := TMenuItem(Sender);
  ReplaceWordAtCaret(StripAccelChar(mi.Caption));
end;

procedure THunSpellMemo.Ignore_Click(Sender: TObject);
begin
  fHunSpell.Ignore(fWordToAdd);
  Refresh;
end;

procedure THunSpellMemo.Add_Click(Sender: TObject);
begin
  fHunSpell.Add(fWordToAdd);
  Refresh;
end;

procedure THunSpellMemo.WMContextMenu(var Message: TWMContextMenu);
resourcestring
  SIgnoreCaption = 'Ignore "%s"';
  SIgnoreHint = '|Ignore all occurrences of "%s"';
  SAddCaption = 'Add "%s"';
  SAddHint = '|Add "%s" to the custom dictionary';
  SChangeHint = '|Change word to "%s"';
var
  pt: TPoint;
  w: string;
  sl: TStringList;
  c, i: integer;
  mi: TMenuItem;
begin
  if Message.Result <> 0 then Exit;
  if csDesigning in ComponentState then Exit;

  if PopupMenu <> nil then
    inherited
  else if fAutoSuggest and Assigned(fHunSpell) and fHunSpell.Initialized then begin
    fSuggMenu.Items.Clear;
    w := GetWordAtCaret;
    if not fHunSpell.Spell(w) then begin
      sl := TStringList.Create;
      try
        c := fHunSpell.Suggest(w, sl);
        for i := 0 to c - 1 do begin
          mi := TMenuItem.Create(fSuggMenu);
          fSuggMenu.Items.Add(mi);
          mi.Caption := sl[i];
          mi.Hint := Format(SChangeHint, [sl[i]]);
          mi.OnClick := Sugg_Click;
        end;
        if fAddMenu then begin
          mi := TMenuItem.Create(fSuggMenu);
          fSuggMenu.Items.Add(mi);
          mi.Caption := '-';
          mi := TMenuItem.Create(fSuggMenu);
          fSuggMenu.Items.Add(mi);
          mi.Caption := Format(SIgnoreCaption, [w]);
          mi.Hint := Format(SIgnoreHint, [w]);
          mi.OnClick := Ignore_Click;

          if fHunSpell.CustomDic then begin
            mi := TMenuItem.Create(fSuggMenu);
            fSuggMenu.Items.Add(mi);
            mi.Caption := Format(SAddCaption, [w]);
            mi.OnClick := Add_Click;
            mi.Hint := Format(SAddHint, [w]);
          end;
          fWordToAdd := w;
        end else
          fWordToAdd := '';
      finally
        sl.Free;
      end;
    end;

    if fSuggMenu.Items.Count > 0 then begin
      pt := SmallPointToPoint(Message.Pos);
      fSuggMenu.Popup(pt.x, pt.y);
      Message.Result := 1;
    end;
    if Message.Result = 0 then
      inherited;
  end else
    inherited;
end;

procedure THunSpellMemo.WMPaint(var Message: TWMPaint);
var
  lCanvas: TCanvas;
  borderw: integer;
  dw, dh: integer;

  procedure SaveBitmap(DC: HDC);
  var
    bmp: TBitmap;
  begin
    bmp := TBitmap.Create;
    bmp.Width := dw;
    bmp.Height := dh;
    BitBlt(bmp.Canvas.Handle, 0, 0, dw, dh, DC, 0, 0, SRCCOPY);
    bmp.SaveToFile('C:\Documents and Settings\Stefan Ascher\My Documents\Projects\units\THunSpell\test.bmp');
    bmp.Free;
  end;

  // Draw the line
  procedure UnderLine(const x1, x2, y: integer);
  const
    MW_POINTS: array[boolean] of ShortInt = (2, 0);
  var
    x, l: integer;
  begin
    with lCanvas do begin
      l := x1;
      MoveTo(l, y);
      x := x1;
      while (x2 > x) do begin
        if (x > dw) then
          // Outside viewing area
          Break;
        if (y < dh) then
          LineTo(x, y + MW_POINTS[(l - x) mod 4 = 0])
        else
          MoveTo(x, y + MW_POINTS[(l - x) mod 4 = 0]);
        Inc(x, 2);
      end;
    end;
  end;

var
  DC, DCW, MemDC: HDC;
  MemBitmap: HBITMAP;
  PS: TPaintStruct;
  i, x, y: Integer;
  Size: TSize;
  Max, TopLine: Integer;
  s, Word, WordWoQuotes,
  PrevWord: string;
  hscrollpos: integer;
  cr: TRect;
  quotew: integer;
begin
  // Clear previous drawings
  InvalidateRect(Handle, nil, not (csOpaque in ControlStyle));
  // Draw text to the window
  inherited;
  // Underline words, does not work with Tabs
  if ([csLoading, csReading, csDesigning] * ComponentState = []) and Assigned(fHunSpell) and fHunSpell.Initialized then begin
    cr := ClientRect;
    hscrollpos := GetScrollPos(Handle, SB_HORZ);
    borderw := GetBorderWidth;
    DC := GetDC(0);
    dw := cr.Right - (borderw * 2);
    dh := cr.Bottom - (borderw * 2);
    MemBitmap := CreateCompatibleBitmap(DC, dw, dh);
    ReleaseDC(0, DC);
    MemDC := CreateCompatibleDC(0);
    SelectObject(MemDC, MemBitmap);
    BeginPaint(Handle, PS);

    lCanvas := TCanvas.Create;
    try
      with lCanvas do begin
        Lock;
        Handle := MemDC;
        try
          Brush.Color := Self.Color;
          Font.Assign(Self.Font);
          // Get a picture of the text
          DCW := GetDC(Self.Handle);
          if not BitBlt(Handle, 0, 0, dw, dh, DCW, borderw, borderw, SRCCOPY) then begin
{$ifdef DEBUG}
            OutputDebugString(PChar('THunSpellMemo.WMPaint(): ' + SysErrorMessage(GetLastError)));
{$endif}
          end;
          Pen.Width := 1;
          if (fRightEdge > 0) then begin
            // Draw right line
            x := TextWidth(StringOfChar('M', fRightEdge)) - hscrollpos;
            Pen.Color := fRightEdgeColor;
            MoveTo(x, 0);
            LineTo(x, cr.Bottom);
          end;
          Pen.Color := fUnderlineColor;
          TopLine := GetTopLine;
          Max := TopLine + GetVisibleLines;
          if Max > Pred(Lines.Count) then
            Max := Pred(Lines.Count);

          y := 1;
          // Underline words
          for i := TopLine to Max do begin
            x := 3;
            s := Lines[i];
            if Trim(s) <> '' then begin
              // Line contains text
              Word := NextWord(s, PrevWord);
              if (Word <> '') then begin
                while Word <> '' do begin
                  // Does not work with Tabs :-(
                  GetTextExtentPoint32(Handle, PChar(PrevWord), Length(PrevWord), Size);
                  Inc(x, Size.cx);
                  GetTextExtentPoint32(Handle, PChar(Word), Length(Word), Size);
                  WordWoQuotes := StripQuotes(Word);
                  if WordWoQuotes <> Word then
                    quotew := TextWidth('''')
                  else
                    quotew := 0;
                  if (Length(WordWoQuotes) > 1) and (not SpellCheck(WordWoQuotes)) then begin
                    UnderLine((x + quotew) - hscrollpos - 3,                    // x1
                      ((x + Size.cx) - hscrollpos) - quotew - 3,                // x2
                      (y + Size.cy) - borderw - Round((1 / Size.cy) * 20));     // y
                  end;
                  Word := NextWord(s, PrevWord);
                  Inc(x, Size.cx);
                end;
              end else
                Size.cy := TextHeight('O');
            end else
              // Blank line
              Size.cy := TextHeight('O');
            Inc(y, Size.cy);
          end;
        finally
          Handle := 0;
          Unlock;
        end;
        // Draw text with underlined words back to the window
//        SaveBitmap(MemDC);
        if not BitBlt(DCW, borderw, borderw, dw, dh, MemDC, 0, 0, SRCCOPY) then begin
{$ifdef DEBUG}
          OutputDebugString(PChar('THunSpellMemo.WMPaint(): ' + SysErrorMessage(GetLastError)));
{$endif}
        end;
        ReleaseDC(Self.Handle, DCW);
      end;
    finally
      lCanvas.Free;
      EndPaint(Handle, PS);
      DeleteDC(MemDC);
      DeleteObject(MemBitmap);
    end;
  end;
end;

procedure THunSpellMemo.EMCharFromPos(var Message: TMessage);
var
  len, fl, lh, curx, lnindex: integer;
  lineindex, charindex: integer;
  x, y: integer;
  cr: TRect;
  hscrollpos, borderw, dw, dh: Integer;
  DC: HDC;
  size: TSize;
  sln: string;
  lnlen: integer;
  ch: string;
  oldfont: HFONT;
begin
  len := SendMessage(Handle, WM_GETTEXTLENGTH, 0, 0);
  if len < 65536 then begin
    inherited;
  end else if HandleAllocated then begin
    // Hack, only for TEdit, TRichEdit handles EM_CHARFROMPOS fine
    // Again, does not work with Tabs
    fl := GetTopLine;
    x := LoWord(Message.LParam);
    y := HiWord(Message.LParam);

    cr := ClientRect;
    borderw := GetBorderWidth;
    dw := cr.Right - (borderw * 2);
    dh := cr.Bottom - (borderw * 2);
    if (x > dw) or (y > dh) then begin
      // Outside client area
      Message.Result := MakeLong(65535, 65535);
      Exit;
    end;
    hscrollpos := GetScrollPos(Handle, SB_HORZ);
    DC := GetDC(Handle);
    oldfont := SelectObject(DC, Font.Handle);
    try
      // Get the line index
      Windows.GetTextExtentPoint32(DC, 'O', 1, size);
      lh := size.cy;
      lineindex := ((y div lh)+1) + fl;
      sln := Lines[lineindex-1];
      curx := 0;
      charindex := 1;
      lnlen := Length(sln);
      // Move to right until curx > x
      while (curx < (x - hscrollpos)) and (curx < (dw + hscrollpos)) and (charindex <= lnlen) do begin
        ch := sln[charindex];
        Windows.GetTextExtentPoint32(DC, PChar(ch), 1, size);
        Inc(curx, size.cx);
        if (curx < (x - hscrollpos)) then
          Inc(charindex)
        else
          Break;
      end;
      // Get char offset of line
      lnindex := SendMessage(Handle, EM_LINEINDEX, lineindex-1, 0);
      Message.Result := (charindex-1) + lnindex;
    finally
      SelectObject(DC, oldfont);
      ReleaseDC(Handle, DC);
    end;
  end;
end;

procedure THunSpellMemo.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  p, len: integer;
begin
  inherited;
  SetFocus;
  if (Button = mbRight) and fRightClickMoveCaret and (SelLength = 0) then begin
    len := SendMessage(Handle, WM_GETTEXTLENGTH, 0, 0);
    if len < 65536 then
      p := LoWord(SendMessage(Handle, EM_CHARFROMPOS, 0, MakeLParam(X, Y)))
    else
      p := SendMessage(Handle, EM_CHARFROMPOS, 0, MakeLParam(X, Y));
    SendMessage(Handle, EM_SETSEL, p, p);
  end;
end;

procedure THunSpellMemo.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if SelChanged then
    Refresh;
end;

procedure THunSpellMemo.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Refresh;
end;

function THunSpellMemo.GetVisibleLines: integer;
var
  OldFont: HFont;
  DC: THandle;
  TextMetric: TTextMetric;
  cr: TRect;
begin
  if HandleAllocated then begin
    DC := GetDC(Handle);
    try
      OldFont := SelectObject(DC, Font.Handle);
      try
        GetTextMetrics(DC, TextMetric);
        cr := ClientRect;
        Result := (cr.Bottom - cr.Top) div (TextMetric.tmHeight + TextMetric.tmExternalLeading);
      finally
        SelectObject(DC, OldFont);
      end;
    finally
      ReleaseDC(Handle, DC);
    end;
  end else
    Result := 0;
end;

function THunSpellMemo.GetMaxLineWidth: integer;
var
  i: integer;
  size: TSize;
  s: string;
  DC: THandle;
  OldFont: HFont;
begin
  Result := 0;
  if HandleAllocated then begin
    DC := GetDC(Handle);
    try
      OldFont := SelectObject(DC, Font.Handle);
      try
        for i := 0 to Lines.Count - 1 do begin
          s := Lines[i];
          Windows.GetTextExtentPoint32(DC, PChar(s), Length(s), size);
          Result := Max(Result, size.cx);
        end;
      finally
        SelectObject(DC, OldFont);
      end;
    finally
      ReleaseDC(Handle, DC);
    end;
  end;
end;

procedure THunSpellMemo.SetCaretPos(Value: TPoint);
var
  ln: integer;
  ss: integer;
begin
  ln := Value.y - 1;
  ss := Value.x + Perform(EM_LINEINDEX, ln, 0);
  SendMessage(Handle, EM_SETSEL, ss, ss);
end;

function THunSpellMemo.GetCaretPos: TPoint;
var
  ss, se: Cardinal;
begin
  SendMessage(Handle, EM_GETSEL, Integer(@ss), Integer(@se));
  Result.X := se;
  Result.Y := SendMessage(Handle, EM_LINEFROMCHAR, se, 0);
  Result.X := Result.X - SendMessage(Handle, EM_LINEINDEX, -1, 0);
end;

function THunSpellMemo.NextWord(var Line, Prev: string): string;
begin
  Result := '';
  Prev := '';
  if Line = '' then
    Exit;
  while(Line <> '') and IsSeparator(Line[1]) do begin
    Prev := Prev + Line[1];
    Delete(Line, 1, 1);
  end;
  while(Line <> '') and not IsSeparator(Line[1]) do begin
    Result := Result + Line[1];
    Delete(Line, 1, 1);
  end;
end;

function THunSpellMemo.SpellCheck(const AWord: string): boolean;
begin
  if Assigned(fHunSpell) then
    Result := fHunSpell.Spell(AWord)
  else
    Result := true;
end;

function THunSpellMemo.GetTopLine: integer;
begin
  Result := SendMessage(Handle, EM_GETFIRSTVISIBLELINE, 0, 0);
end;

procedure THunSpellMemo.SetHunSpell(Value: THunSpell);
begin
  if fHunSpell <> Value then begin
    fHunSpell := Value;
    if not (csDesigning in ComponentState) then
      Refresh;
  end;
end;

procedure THunSpellMemo.SetUnderlineColor(Value: TColor);
begin
  if fUnderlineColor <> Value then begin
    fUnderlineColor := Value;
    if not (csDesigning in ComponentState) then
      Refresh;
  end;
end;

function THunSpellMemo.GetWordPos(const APos: TPoint; out AStart, AEnd: integer): boolean;
var
  ln: integer;
  sln: string;
begin
  ln := APos.y;
  sln := Lines[ln];
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
end;

function THunSpellMemo.GetWordAt(const APos: TPoint): string;
var
  ln: integer;
  s, e: integer;
  sln: string;
begin
  if GetWordPos(APos, s, e) then begin
    ln := APos.y;
    sln := Lines[ln];
    Result := Copy(sln, s + 1, e - s - 1);
  end else
    Result := '';
end;

function THunSpellMemo.GetWordAtCaret: string;
begin
  Result := GetWordAt(CaretPos);
end;

procedure THunSpellMemo.ReplaceWordAt(const APos: TPoint;
  const ANew: string);
var
  li: integer;
  s, e: integer;
  ss, se: integer;
begin
  if GetWordPos(APos, s, e) then begin
    // Get current selection
    SendMessage(Handle, EM_GETSEL, Longint(@ss), Longint(@se));
    li := SendMessage(Handle, EM_LINEINDEX, APos.y, 0);
    // Select the word to replace
    SendMessage(Handle, EM_SETSEL, s + li, e + li - 1);
    // Replace sel
    SelText := ANew;
    // Restore selection
    SendMessage(Handle, EM_SETSEL, ss, se);
  end;
end;

procedure THunSpellMemo.ReplaceWordAtCaret(const ANew: string);
begin
  ReplaceWordAt(CaretPos, ANew);
end;

function THunSpellMemo.SelChanged: boolean;
var
  sl: integer;
begin
  sl := SelLength;
  Result := (fSelLen <> sl);
  if Result then begin
    fSelLen := sl;
  end;
end;

function THunSpellMemo.GetBorderWidth: integer;
begin
  if BorderStyle = bsNone then
    Result := 0
  else
    if NewStyleControls and Ctl3D then
      Result := 2
    else
      Result := 1;
end;

procedure THunSpellMemo.Refresh;
begin
  if HandleAllocated then
    SendMessage(Handle, WM_PAINT, 0, 0);
end;

procedure THunSpellMemo.SetRightEdge(Value: integer);
begin
  if fRightEdge <> Value then begin
    fRightEdge := Value;
    if not (csDesigning in ComponentState) then
      Refresh;
  end;
end;

procedure THunSpellMemo.SetRightEdgeColor(Value: TColor);
begin
  if fRightEdgeColor <> Value then begin
    fRightEdgeColor := Value;
    if not (csDesigning in ComponentState) then
      Refresh;
  end;
end;

end.
