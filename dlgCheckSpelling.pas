{
  THunSpellDialog
  Copyright (C) 2010, Stefan Ascher

  @abstract(Spell checking dialog.)

  @author(Stefan Ascher <stievie@inode.at>)
  @created(25-11-2010)
  @cvs($Date: 2010/12/13 03:21:41 $)

  $Id: dlgCheckSpelling.pas,v 1.3 2010/12/13 03:21:41 Stefan Ascher Exp $
}

unit dlgCheckSpelling;

{$i HunSpell.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  {
    @abstract(The dialog used for spell checking)

    Modify it to fit your needs, but keep the ModalResult values.
  }
  TCheckSpellingDialog = class(TForm)
    Label1: TLabel;
    txtNotFound: TEdit;
    Label2: TLabel;
    txtChangeTo: TEdit;
    Label3: TLabel;
    lstSuggestions: TListBox;
    btnChange: TButton;
    btnIgnore: TButton;
    btnAdd: TButton;
    btnClose: TButton;
    btnNoChange: TButton;
    procedure lstSuggestionsClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  private
    { Private declarations }
    function GetWord: string;
    procedure SetWord(value: string);
    function GetChangeTo: string;
    function GetSuggestions: TStrings;
  public
    { Public declarations }
    { The unknown word }
    property Word: string read GetWord write SetWord;
    { New word }
    property ChangeTo: string read GetChangeTo;
    { List with suggestions }
    property Suggestions: TStrings read GetSuggestions;
  end;

var
  CheckSpellingDialog: TCheckSpellingDialog;

implementation

resourcestring
  SSpellNoSugg = 'No suggestions';
  
{$R *.DFM}

{ TCheckSpellingDialog }

procedure TCheckSpellingDialog.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  // This program (demo.exe) removes the application window from the Taskbar.
  // This may be a problem to determine to which window a modal dialog should
  // be shown. Here we set the Owner if it's a TWinControl or otherwise to the
  // Application.MainForm.
  // If you use this dialog in an other application you can remove it.
  Params.ExStyle := Params.ExStyle and not WS_EX_APPWINDOW;
  if (Owner is TWinControl) and IsWindow((Owner as TWinControl).Handle) then
    Params.WndParent := (Owner as TWinControl).Handle
  else if Assigned(Screen.ActiveForm) then
    Params.WndParent := Screen.ActiveForm.Handle
  else
    Params.WndParent := Application.MainForm.Handle;
end;

function TCheckSpellingDialog.GetWord: string;
begin
  Result := txtNotFound.Text;
end;

procedure TCheckSpellingDialog.SetWord(value: string);
begin
  txtNotFound.Text := Value;
end;

procedure TCheckSpellingDialog.lstSuggestionsClick(Sender: TObject);
begin
  txtChangeTo.Text := lstSuggestions.Items[lstSuggestions.ItemIndex];
end;

procedure TCheckSpellingDialog.FormShow(Sender: TObject);
begin
  if lstSuggestions.Items.Count = 0 then begin
    lstSuggestions.Items.Add(SSpellNoSugg);
    lstSuggestions.Enabled := false;
    txtChangeTo.Text := txtNotFound.Text;
  end else begin
    lstSuggestions.Enabled := true;
    lstSuggestions.ItemIndex := 0;
    txtChangeTo.Text := lstSuggestions.Items[0];
  end;
end;

procedure TCheckSpellingDialog.FormCreate(Sender: TObject);
begin
  with Constraints do begin
    MinHeight := Height;
    MinWidth := Width;
  end;
end;

function TCheckSpellingDialog.GetChangeTo: string;
begin
  Result := txtChangeTo.Text;
end;

function TCheckSpellingDialog.GetSuggestions: TStrings;
begin
  Result := lstSuggestions.Items;
end;

end.
