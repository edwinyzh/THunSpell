program demo;

{%File '..\HunSpell.inc'}

uses
  Forms,
  frmMain in 'frmMain.pas' {MainForm},
  HunSpellMemo in '..\HunSpellMemo.pas',
  dlgCheckSpelling in '..\dlgCheckSpelling.pas' {CheckSpellingDialog},
  HunSpell in '..\HunSpell.pas',
  HunSpellApi in '..\HunSpellApi.pas',
  HunSpellDialog in '..\HunSpellDialog.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TCheckSpellingDialog, CheckSpellingDialog);
  Application.Run;
end.
