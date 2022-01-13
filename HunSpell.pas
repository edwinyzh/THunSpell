{
  THunSpell
  Copyright (C) 2010, Stefan Ascher

  @abstract(Component wrapper for Hunspell <http://hunspell.sourceforge.net/>)

  Hunspell is the spell checker used by OpenOffice.org (LibreOffice???) and Mozilla
  (Firefox, Thunderbird etc.).

  Get Dictionaries from <http://wiki.services.openoffice.org/wiki/Dictionaries>.
  It seems that the *.oxt files are ordinary ZIP files, you can open them with
  e.g. 7-Zip <http://www.7-zip.org/> and extract the files to the dictionary
  directory. 

  @author(Stefan Ascher <stievie@inode.at>)
  @created(02-10-2010)
  @cvs($Date: 2011/03/23 12:18:49 $)

  $Id: HunSpell.pas,v 1.9 2011/03/23 12:18:49 Stefan Ascher Exp $
}

unit HunSpell;

{$i HunSpell.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

const
  THUNSPELL_VERSION = '1.13';
  
type
  {
    Component wrapper for the Hunspell library <http://hunspell.sourceforge.net/>.
  }
  THunSpell = class(TComponent)
  private
    { Private declarations }
    fSpell: Pointer;
    fDict: string;
    fDictDir: string;
    fEnabled: boolean;
    fCustomDic: boolean;
    fCustom: TStringList;
    fIgnoreWordsWithNumbers: boolean;
    fIgnoreAllCaps: boolean;
    fCodePage: Word;
    fIsNativeCodePage: boolean;
    fOnInit: TNotifyEvent;
    fOnUninit: TNotifyEvent;
    procedure SetDict(Value: string);
    procedure SetDictDir(Value: string);
    procedure Init;
    procedure Uninit;
    function GetInitialized: boolean;
    procedure SetEnabled(Value: boolean);
    function GetDictEncoding: string;
    procedure SetCustomDic(Value: boolean);
    procedure LoadCustom;
    procedure SaveCustom;
    function GetCustomDictName: string;
    function EncodeString(const S: UnicodeString): AnsiString;
    function DecodeString(const S: AnsiString): UnicodeString;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    {
      Check a word
      @param AWord The word to check.
      @return @true if it's properly spelled.
    }
    function Spell(const AWord: string): boolean;
    {
      Get suggestions for a not found word.
      @param AWord The word
      @param AString This List is filled with the suggestions.
      @return Count of suggestions.
    }
    function Suggest(const AWord: string; AStrings: TStrings): integer;
    {
      Ignore all occurrences of the word.
      @param AWord The word to ignore.
      @return @true when the word was added.
    }
    function Ignore(const AWord: string): boolean;
    {
      If @link(CustomDic) = @true the word is added to custom.dic, otherwise
      it is the same as @link(Ignore).
      @param AWord The word to ignore.
      @return @true when the word was added.
    }
    function Add(const AWord: string): boolean;
    {
      Get all dictionaries in the dictionaries directory (@link(DictDir)).
      @param AList The list to fill with the dictionaries.
      @return Count of found dictionaries.
    }
    function GetDicts(AList: TStrings): integer;

    {
      Search given directory for dictionaries.
      @param ADir The directory to look for dictionaries.
      @param AList List with filled dictionaries.
      @return The count of found dictionaries.
     }
    class function SearchDicts(const ADir: string; AList: TStrings): integer;

    { Returns @true when the spell checker is ready to use. }
    property Initialized: boolean read GetInitialized;
    { Return the encoding of the loaded dictionary. }
    property DictEncoding: string read GetDictEncoding;
    { Return codepage of loaded dictionary. }
    property Codepage: Word read fCodePage;
    { Full filename of the custom dictionary. }
    property CustomDistName: string read GetCustomDictName;
  published
    { Published declarations }
    { Filename of dictionary to use without the extension (.dic, .aff), e.g. @code(en_US) }
    property Dict: string read fDict write SetDict;
    { Directory where the dictionary is located. If @link(CustomDic) = @true then
      the program should have write permissions for this directory, because
      custom.dic is saved to it. }
    property DictDir: string read fDictDir write SetDictDir;
    { En/-disable spell checking. }
    property Enabled: boolean read fEnabled write SetEnabled default true;
    { En-/Disable custom dictionary. Added words are saved to dict\custom.dic }
    property CustomDic: boolean read fCustomDic write SetCustomDic;
    { Ignore words containing numbers. }
    property IgnoreWordsWithNumbers: boolean read fIgnoreWordsWithNumbers write fIgnoreWordsWithNumbers;
    { Ignore words with all capital letters, e.g. acronyms. }
    property IgnoreAllCaps: boolean read fIgnoreAllCaps write fIgnoreAllCaps;

    { Hunspell was initialized. }
    property OnInit: TNotifyEvent read fOnInit write fOnInit;
    { Hunspell was uninitialized. }
    property OnUninit: TNotifyEvent read fOnUninit write fOnUninit;
  end;

const
  DIC_EXT = '.dic';              {< Extension of dic files }
  AFF_EXT = '.aff';              {< Extension of aff files }
  CUSTOM_NAME = 'custom.dic';    {< Name of custom dictionary }
  SEP_CHARS_COUNT = 36;
  { Separator characters used to detect word boundaries. Single quote (') is not
    included this is a special case. }
  SEP_CHARS: array[0..SEP_CHARS_COUNT-1] of Char = (
    '.', ';', ',', ':', '!', '·', '"', '^', '+', '-', '*', '/', '\', '¨', ' ',
    #9, '`', '[', ']', '(', ')', 'º', 'ª', '{', '}', '?', '¿', '%', '=', '<',
    '>', '$', '@', '|', '~', '&'
  );

  CP_ISOLATIN1 = 28591;
  CP_WINLATIN1 = 1252;

procedure Register;
{
  Check if a character is a separator.
  @param AChar The character to check.
  @return @true if AChar is a separator.
}
function IsSeparator(const AChar: Char): boolean;
{
  Strip single quotes from a string.
  @param AWord The word.
  @return AWord without single quotes.
}
function StripQuotes(const AWord: string): string;

implementation

uses
  HunSpellApi;

resourcestring
  SNotInitialized = 'Hunspell not initialized';

procedure Register;
begin
  RegisterComponents('Additional', [THunSpell]);
end;

function IsSeparator(const AChar: Char): boolean;
var
  i: integer;
begin
  for i := 0 to SEP_CHARS_COUNT - 1 do begin
    if SEP_CHARS[i] = AChar then begin
      Result := true;
      Exit;
    end;
  end;
  Result := false;
end;

// Strip single quotes from string
function StripQuotes(const AWord: string): string;
begin
  Result := AWord;
  if Length(Result) > 2 then begin
    if (Result[1] = '''') and (Result[Length(Result)] = '''') then begin
      // Quoted
      Delete(Result, 1, 1);
      Delete(Result, Length(Result), 1);
    end;
  end;
end;

function SameText(const S1, S2: string): boolean;
begin
  Result := CompareText(S1, S2) = 0;
end;

{ THunSpell }

class function THunSpell.SearchDicts(const ADir: string; AList: TStrings): integer;
var
  sr: TSearchRec;
  d: string;
  aff: string;
begin
  Result := 0;
  AList.Clear;
  if ADir <> '' then begin
    d := IncludeTrailingBackslash(ADir);
    if FindFirst(d + '*' + DIC_EXT, faAnyFile, sr) = 0 then begin
      repeat
        if (sr.Name <> '.') and (sr.Name <> '..') then begin
          aff := d + ChangeFileExt(sr.Name, AFF_EXT);
          // Check for .aff file
          if FileExists(aff) then begin
            AList.Add(ChangeFileExt(sr.Name, ''));
            Inc(Result);
          end;
        end;
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;
  end;
end;

constructor THunSpell.Create(AOwner: TComponent);
begin
  inherited;
  fSpell := nil;
  fEnabled := true;
  fCustom := TStringList.Create;
  fCustom.Duplicates := dupIgnore;
end;

destructor THunSpell.Destroy;
begin
  Uninit;
  fCustom.Free;
  inherited;
end;

function THunSpell.GetInitialized: boolean;
begin
  Result := fSpell <> nil;
end;

function THunSpell.GetDictEncoding: string;
begin
  if GetInitialized then
    Result := StrPas(hunspell_get_dic_encoding(fSpell))
  else
    Result := '';
end;

function THunSpell.GetDicts(AList: TStrings): integer;
begin
  Result := THunSpell.SearchDicts(fDictDir, AList);
end;

procedure THunSpell.Init;
var
  d: string;
  dic, aff, cp: string;
  scp: Word;
begin
  if not (csDesigning in ComponentState) then begin
    if HunSpellApi.LoadLib then begin
      Uninit;
      if (fDictDir <> '') and (fDict <> '') then begin
        d := IncludeTrailingBackslash(fDictDir);
        aff := d + fDict + AFF_EXT;
        dic := d + fDict + DIC_EXT;
        if FileExists(aff) and FileExists(dic) then begin
          fSpell := hunspell_initialize(PAnsiChar(AnsiString(aff)), PAnsiChar(AnsiString(dic)));

          // Codepage
          scp := GetACP;
          fCodePage := scp;
          cp := GetDictEncoding;
          if SameText(cp, 'utf8') or SameText(cp, 'utf-8') then
            fCodePage := CP_UTF8
          else if SameText(cp, 'iso-8859-1') or SameText(cp, 'iso8859-1') then
            fCodePage := CP_ISOLATIN1
          else if SameText(cp, 'iso-8859-2') or SameText(cp, 'iso8859-2') then
            fCodePage := 28592
          else if SameText(cp, 'iso-8859-3') or SameText(cp, 'iso8859-3') then
            fCodePage := 28593
          else if SameText(cp, 'iso-8859-4') or SameText(cp, 'iso8859-4') then
            fCodePage := 28594
          else if SameText(cp, 'iso-8859-5') or SameText(cp, 'iso8859-5') then
            fCodePage := 28595
          else if SameText(cp, 'iso-8859-6') or SameText(cp, 'iso8859-6') then
            fCodePage := 28596
          else if SameText(cp, 'iso-8859-7') or SameText(cp, 'iso8859-7') then
            fCodePage := 28597
          else if SameText(cp, 'iso-8859-8') or SameText(cp, 'iso8859-8') then
            fCodePage := 28598
          else if SameText(cp, 'iso-8859-9') or SameText(cp, 'iso8859-9') then
            fCodePage := 28599
          else if SameText(cp, 'iso-8859-13') or SameText(cp, 'iso8859-13') then
            fCodePage := 28603
          else if SameText(cp, 'iso-8859-15') or SameText(cp, 'iso8859-15') then
            fCodePage := 28605
          else if SameText(cp, 'koi8-r') or SameText(cp, 'koi8-u') then
            fCodePage := 20866;
          fIsNativeCodePage := (fCodePage = scp) or ((fCodePage = CP_ISOLATIN1) and (scp = CP_WINLATIN1));

          LoadCustom;
          if Assigned(fOnInit) then
            fOnInit(Self);
        end;
      end;
    end;
  end;
end;

procedure THunSpell.Uninit;
begin
  if Assigned(fSpell) then begin
    SaveCustom;
    hunspell_uninitialize(fSpell);
    fSpell := nil;
    if Assigned(fOnUninit) then
      fOnUninit(Self);
  end;
end;

function THunSpell.Spell(const AWord: string): boolean;
  function HasNumber(const S: string): boolean;
  var
    i: integer;
  begin
    for i := 1 to Length(S) do
      if (S[i] in ['0'..'9']) then begin
        Result := true;
        Exit;
      end;
    Result := false;
  end;
  function AllCaps(const S: string): boolean;
  var
    i: integer;
  begin
    for i := 1 to Length(S) do
      if not IsCharUpper(S[i]) then begin
        Result := false;
        Exit;
      end;
    Result := true;
  end;
begin
  if not (csDesigning in ComponentState) then begin
    if GetInitialized then begin
      if (fIgnoreWordsWithNumbers and HasNumber(AWord)) or (fIgnoreAllCaps and AllCaps(AWord)) then
        Result := true
      else
        Result := hunspell_spell(fSpell, PAnsiChar(EncodeString(AWord)));
    end else
      raise Exception.Create(SNotInitialized);
  end else
    Result := false;
end;

function THunSpell.Suggest(const AWord: string; AStrings: TStrings): integer;
var
  i: integer;
  sugg: PPAnsiChar;
  s: string;
begin
  if not (csDesigning in ComponentState) then begin
    AStrings.Clear;
    if GetInitialized then begin
      try
        Result := hunspell_suggest(fSpell, PAnsiChar(EncodeString(AWord)), sugg);
        for i := 0 to Result-1 do begin
          s := DecodeString(StrPas(PPAnsiChar(Cardinal(sugg) + Cardinal(i) * SizeOf(Pointer))^));
          AStrings.Add(s);
        end;
        hunspell_suggest_free(fSpell, sugg, Result);
      except
        Result := 0;
      end;
    end else
      raise Exception.Create(SNotInitialized);
  end else
    Result := 0;
end;

procedure THunSpell.SetDict(Value: string);
begin
  // Filenames are not case sensisitve on Win
  if {$ifdef WIN32}CompareText{$else}CompareStr{$endif}(fDict, Value) <> 0 then begin
    fDict := Value;
    Init;
  end;
end;

procedure THunSpell.SetDictDir(Value: string);
begin
  if {$ifdef WIN32}CompareText{$else}CompareStr{$endif}(fDictDir, Value) <> 0 then begin
    fDictDir := Value;
    Init;
  end;
end;

function THunSpell.Ignore(const AWord: string): boolean;
begin
  if not (csDesigning in ComponentState) then begin
    if GetInitialized then begin
      Result := hunspell_add(fSpell, PAnsiChar(EncodeString(AWord))) <> 0;
    end else
      raise Exception.Create(SNotInitialized);
  end else
    Result := false;
end;

function THunSpell.Add(const AWord: string): boolean;
begin
  if not (csDesigning in ComponentState) then begin
    if GetInitialized then begin
      if fCustomDic then
        fCustom.Add(AWord);
      Result := hunspell_add(fSpell, PAnsiChar(EncodeString(AWord))) <> 0;
    end else
      raise Exception.Create(SNotInitialized);
  end else
    Result := false;
end;

procedure THunSpell.SetEnabled(Value: boolean);
begin
  fEnabled := Value;
  if fEnabled then
    Init
  else
    Uninit;
end;

procedure THunSpell.SetCustomDic(Value: boolean);
begin
  if fCustomDic <> Value then begin
    fCustomDic := Value;
    if fCustomDic and GetInitialized then
      // Reinitialize
      Init;
  end;
end;

procedure THunSpell.LoadCustom;
var
  i: integer;
  fn: string;
begin
  fn := GetCustomDictName;
  if fCustomDic and FileExists(fn) then begin
    fCustom.LoadFromFile(fn);
    for i := 0 to fCustom.Count - 1 do begin
      // Ignore all words from custom.dic
      hunspell_add(fSpell, PAnsiChar(EncodeString(fCustom[i])))
    end;
  end;
end;

procedure THunSpell.SaveCustom;
begin
  if fCustomDic then begin
    try
      fCustom.SaveToFile(GetCustomDictName);
    except
      // Ignore all errors
    end;
  end;
end;

function THunSpell.GetCustomDictName: string;
begin
  Result := IncludeTrailingBackslash(fDictDir) + CUSTOM_NAME;
end;

function THunSpell.EncodeString(const S: UnicodeString): AnsiString;
var
  AnsiBuffer: array[0..1023] of AnsiChar;
  AnsiLen: Integer;
begin
  AnsiLen := WideCharToMultiByte(fCodePage, 0, PWideChar(S),
    Length(S), AnsiBuffer, Length(AnsiBuffer), nil, nil);
  SetString(Result, AnsiBuffer, AnsiLen);
end;

function THunSpell.DecodeString(const S: AnsiString): UnicodeString;
var
  WideBuffer: array [0..511] of WideChar;
  WideLen: Integer;
  //{$ifndef UNICODE}
  //  WideRes: UnicodeString;
  //{$endif}
begin
  //{$ifndef UNICODE}
  //  if fIsNativeCodePage then
  //  begin
  //    Result := S;
  //  end else
  //  begin
  //{$endif}

  // Convert string from DLL to Delphi string
  WideLen := MultiByteToWideChar(fCodePage, 0, PAnsiChar(S), -1, WideBuffer, Length(WideBuffer)) - 1;
  SetString(Result, WideBuffer, WideLen);
end;

end.
