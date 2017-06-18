program DataExtractor;

uses
  Forms,
  uMainForm in 'uMainForm.pas' {MainForm},
  ProjectInfo in 'ProjectInfo.pas',
  AboutForm in 'AboutForm.pas' {AboutFrm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TAboutFrm, AboutFrm);
  Application.Run;
end.
