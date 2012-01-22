unit uMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ShellAPI;

type
  TMainForm = class(TForm)
    edProject: TEdit;
    Label1: TLabel;
    btnOpenProject: TButton;
    StatusBar: TStatusBar;
    btnGo: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    procedure btnOpenProjectClick(Sender: TObject);
    procedure btnGoClick(Sender: TObject);
    procedure Edit1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    function CheckRootDir(Dir: string): Boolean;
    function Path(Dir: string): Boolean;
    function Compile(Dir: string): Boolean;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  uSelectDirectoryBox;

procedure TMainForm.btnOpenProjectClick(Sender: TObject);
var
  i: Integer;
  Dir: string;
begin
  if GetFolderDialog(Application.Handle, '', Dir) then
  begin
    if Dir[Length(Dir)] <> '\' then
      Dir := Dir + '\';
    if not CheckRootDir(Dir) then
      ShowMessage('¬ выбранной директории не найден проект')
    else
    begin
      edProject.Text := Dir;
    end;
  end;
end;

function TMainForm.CheckRootDir(Dir: string): Boolean;
begin
  Result := DirectoryExists(Dir + '.git') and DirectoryExists(Dir + '.compile');
end;

function TMainForm.Path(Dir: string): Boolean;
const
  CHARS = ['A'..'Z','a'..'z','0'..'9',' '];
var
  i,k,m: Integer;
  str: string;
  FileName: string;
  FormType: string;
  ProcName: string;
  Params: string;
  SrcStrings, DfmStrings: TStrings;
begin
  Result := False;

  FileName := Dir + 'kernel\client\ClientPack\StdDocList_Form';
  if not (FileExists(FileName + '.pas') and FileExists(FileName + '.dfm')) then
    Exit;

  SrcStrings := TStringList.Create;
  DfmStrings := TStringList.Create;
  SrcStrings.LoadFromFile(FileName + '.pas');
  DfmStrings.LoadFromFile(FileName + '.dfm');

  for i := 0 to SrcStrings.Count - 1 do
    if Pos('uses', SrcStrings.Strings[i]) > 0 then
      Break;
  SrcStrings.Insert(i+1, 'uDocInfo, uInfoForm,');

  str := DfmStrings.Strings[0];
  i := Pos(':', str);
  if i > 0 then
    FormType := Trim(Copy(str, i+1, Length(str)-i))
  else
    Exit;

  for i := 0 to SrcStrings.Count - 1 do
    if Pos(FormType, SrcStrings.Strings[i]) > 0 then
      Break;
  SrcStrings.Insert(i+1, 'btnExtract: TButton;');
  for k := i+2 to SrcStrings.Count - 1 do
  begin
    str := SrcStrings.Strings[k];
    i := Pos('procedure ', str);
    if i <= 0 then
      i := Pos('function ', str);
    if i > 0 then
      Break;
  end;
  SrcStrings.Insert(k, 'procedure btnExtractClick(Sender: TObject);');
  for m := i+8 to Length(str) do
    if not (str[m] in CHARS) then
      Break;
  ProcName := Trim(Copy(str, i+9, m - (i+9)));

  for i := k to SrcStrings.Count - 1 do
    if Pos(FormType + '.' + ProcName, SrcStrings.Strings[i]) > 0 then
      Break;
  str := 'procedure ' + FormType + '.btnExtractClick(Sender: TObject);'#13#10'{$I ExtractProcTemplate.pas}';
  SrcStrings.Insert(i, str);

  for i := 1 to DfmStrings.Count - 1 do
    if Pos('TToolBar', DfmStrings.Strings[i]) > 0 then
      Break;
  for k := i+1 to DfmStrings.Count - 1 do
    if Pos('  end', DfmStrings.Strings[k]) = 1 then
      Break;
  DfmStrings.Insert(k, 'object btnExtract: TButton'#13#10'Top = 0'#13#10'Caption = #1055#1086#1082#1072#1079#1072#1090#1100'#13#10'end');

  SrcStrings.SaveToFile(FileName + '.pas');
  DfmStrings.SaveToFile(FileName + '.dfm');

  SrcStrings.Free;
  DfmStrings.Free;

  CopyFile(PChar('ExtractProcTemplate.pas'), PChar(Dir + 'kernel\client\ClientPack\ExtractProcTemplate.pas'), false);
  CopyFile(PChar('..\DataShower\uDocInfo.pas'), PChar(Dir + 'kernel\client\ClientPack\uDocInfo.pas'), false);
  CopyFile(PChar('..\DataShower\uInfoForm.pas'), PChar(Dir + 'kernel\client\ClientPack\uInfoForm.pas'), false);
  CopyFile(PChar('..\DataShower\uInfoForm.dfm'), PChar(Dir + 'kernel\client\ClientPack\uInfoForm.dfm'), false);

  Result := True;
end;

function TMainForm.Compile(Dir: string): Boolean;
var
  FileName: string;
  Params: string;
begin
  Result := False;
  Dir := Dir + 'kernel\Client\Make\';
  FileName := Dir + 'ClientPack';
  if FileExists(FileName + '.cfg') then
    Params := ''
  else
  begin
    Params := ' -AMainTLB=MainController_TLB' +
              ' -I"..\shared"' +
              ' -U"..\..\..\.compile\client"' +
              ' -O"..\..\..\.compile\client"' +
              ' -I"..\..\..\.compile\client"' +
              ' -R"..\..\..\.compile\client"' +
              ' -N"..\..\..\.compile\client\ClientPack"' +
              ' -LE"..\..\..\build\client"' +
              ' -LN"..\..\..\.compile\client"' +
              ' -LUEhLib;az_lib_rt;KerPack;rtl';
  end;
  ShellExecute(Self.Handle, PChar('open'), PChar('dcc32.exe'), PChar(Params + ' ' + FileName + '.dpk'), PChar(Dir), 0);
  Result := True;
end;

procedure TMainForm.btnGoClick(Sender: TObject);
begin
  if Path(edProject.Text) then
    Compile(edProject.Text);
end;

procedure TMainForm.Edit1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  i: Integer;
  text: PWideChar;
  str1,str2: string;
begin
  str1 := Edit1.Text;
  if str1 = '' then Exit;
  GetMem(text, 2*(Length(str1)+1));
  StringToWideChar(str1, text, Length(str1)+1);
  str2 := '';
  for i := 1 to Length(str1) do
  begin
    str2 := str2 + '#' + IntToStr(Word(text^));
    Inc(text);
  end;
  Edit2.Text := str2;
end;

end.
