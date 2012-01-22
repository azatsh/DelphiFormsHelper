program DataExtractor;

uses
  Forms,
  uMainForm in 'uMainForm.pas' {MainForm},
  uSelectDirectoryBox in 'uSelectDirectoryBox.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
