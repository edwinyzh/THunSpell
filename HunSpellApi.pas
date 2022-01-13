{
  THunSpell
  Copyright (C) 2010, Stefan Ascher

  @abstract(Interface to the libhunspell.dll)

  @author(Stefan Ascher <stievie@inode.at>)
  @created(02-10-2010)
  @cvs($Date: 2010/11/30 06:55:58 $)

  $Id: HunSpellApi.pas,v 1.1 2010/11/30 06:55:58 Stefan Ascher Exp $
}

unit HunSpellApi;

{$i HunSpell.inc}

interface

uses
  Windows, SysUtils;

type
  Thunspell_initialize = function(aff_file: PAnsiChar; dict_file: PAnsiChar): Pointer; cdecl;
  Thunspell_uninitialize = procedure(spell: Pointer); cdecl;
  Thunspell_spell = function(spell: Pointer; word: PAnsiChar): BOOL; cdecl;
  Thunspell_suggest = function(spell: Pointer; word: PAnsiChar; var suggestions: PPAnsiChar): Integer; cdecl;
  Thunspell_suggest_auto = function(spell: Pointer; word: PAnsiChar; var sugg: PPAnsiChar): Integer; cdecl;
  Thunspell_suggest_free = procedure(spell: Pointer; sugg: PPAnsiChar; len: Integer); cdecl;
  Thunspell_get_dic_encoding = function(spell: Pointer): PAnsiChar; cdecl;
  Thunspell_add = function(spell: Pointer; word: PAnsiChar): Integer; cdecl;

var
  hunspell_initialize: Thunspell_initialize;
  hunspell_uninitialize: Thunspell_uninitialize;
  hunspell_spell: Thunspell_spell;
  hunspell_suggest: Thunspell_suggest;
  hunspell_suggest_auto: Thunspell_suggest_auto;
  hunspell_suggest_free: Thunspell_suggest_free;
  hunspell_get_dic_encoding: Thunspell_get_dic_encoding;
  hunspell_add: Thunspell_add;

function LoadLib: boolean;

implementation

const
  HUNSPELLLIB_NAME  = 'libhunspell.dll';

var
  hLib: HMODULE = 0;

function LoadLib: boolean;
begin
  if hLib <> 0 then begin
    Result := true;
    Exit;
  end;
  hLib := Loadlibrary(PChar(HUNSPELLLIB_NAME));
  Result := hLib <> 0;

  if Result then begin
    @hunspell_initialize       := GetProcAddress(hLib, 'hunspell_initialize');
    @hunspell_uninitialize     := GetProcAddress(hLib, 'hunspell_uninitialize');
    @hunspell_spell            := GetProcAddress(hLib, 'hunspell_spell');
    @hunspell_suggest          := GetProcAddress(hLib, 'hunspell_suggest');
    @hunspell_suggest_auto     := GetProcAddress(hLib, 'hunspell_suggest_auto');
    // New, do not use -> AV
//    @hunspell_suggest_free     := GetProcAddress(hLib, 'hunspell_free_list');
//    if not Assigned(@hunspell_suggest_free) then
      // Old
      @hunspell_suggest_free   := GetProcAddress(hLib, 'hunspell_suggest_free');
    @hunspell_get_dic_encoding := GetProcAddress(hLib, 'hunspell_get_dic_encoding');
    @hunspell_add              := GetProcAddress(hLib, 'hunspell_put_word');
    if not Assigned(@hunspell_add) then
      @hunspell_add            := GetProcAddress(hLib, 'hunspell_add');
  end;
end;

initialization

finalization
  if hLib <> 0 then
    FreeLibrary(hLib);

end.
