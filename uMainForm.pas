unit uMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FileCtrl, StdCtrls, ExtCtrls, Menus, ComCtrls,
  Grids, AdvObj, BaseGrid, AdvGrid, ShellAPI, TreeList, Buttons,
  ActnList, AdvMenus, ToolWin;

type

  TMainForm = class(TForm)
    OpenDialog: TOpenDialog;
    TreeList: TTreeList;
    edtFilter: TLabeledEdit;
    btnApply: TSpeedButton;
    chbAutoApply: TCheckBox;
    ActionList: TActionList;
    actApplyFilter: TAction;
    actExecuteFile: TAction;
    actAutoApply: TAction;
    MainMenu: TMainMenu;
    N1: TMenuItem;
    ToolBar: TToolBar;
    Open1: TMenuItem;
    actOpenProject: TAction;
    Settings1: TMenuItem;
    StatusBar: TStatusBar;
    PopupMenu: TPopupMenu;
    Open2: TMenuItem;
    View1: TMenuItem;
    actShowAbout: TAction;
    About1: TMenuItem;
    About2: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actApplyFilterExecute(Sender: TObject);
    procedure actExecuteFileExecute(Sender: TObject);
    procedure actAutoApplyExecute(Sender: TObject);
    procedure edtFilterChange(Sender: TObject);
    procedure actOpenProjectExecute(Sender: TObject);
    procedure actShowAboutExecute(Sender: TObject);
  private
    FProjectRootDir: string;
    FProjects: TList;
  protected
    procedure ParseBPGFile(aFileName: string; out aProjectsFiles: TStrings);
    procedure ParseProjectFile(aFileName: string; out aFiles: TStrings);
    procedure OpenBPG(aFileName: string);

    procedure BuildTree();
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  IniFiles,
  ProjectInfo,
  AboutForm;

procedure Split(aStr: string; aDelimiter: Char; out aWords: TStrings);
var
  Strings: TStrings;
begin
  Strings := TStringList.Create();
  try
    Strings.Delimiter := aDelimiter;
    Strings.DelimitedText := aStr;
    aWords.AddStrings(Strings);
  finally
    Strings.Free();
  end;
end;

procedure TMainForm.ParseBPGFile(aFileName: string; out aProjectsFiles: TStrings);
var
  i, k: Integer;
  Str, Name, Value: string;
  Found: Boolean;
  FileStrings,
  Projects,
  ProjectsFiles: TStrings;
begin
  FileStrings := TStringList.Create();
  Projects := TStringList.Create();
  ProjectsFiles := THashedStringList.Create();
  try
    FileStrings.LoadFromFile(aFileName);
    i := 0;
    Found := False;
    while (not Found) and (i < FileStrings.Count) do
    begin
      Str := FileStrings.Strings[i];
      Found := (Pos('PROJECTS =', Str) > 0);
      Inc(i);
    end;

    if not Found then Exit;

    Dec(i);
    while (i < FileStrings.Count) do
    begin
      Str := FileStrings.Strings[i];
      if (Str[1] = '#') then
        Break;
      Split(Str, ' ', Projects);
      Inc(i);
    end;

    k := 0;
    while (k < Projects.Count) do
    begin
      Str := ExtractFileExt(Projects.Strings[k]);
      if (Str <> '.bpl') and (Str <> '.ocx') and (Str <> '.exe') and (Str <> '.dll') then
        Projects.Delete(k)
      else
        Inc(k);
    end;

    ProjectsFiles.NameValueSeparator := ':';
    while (i < FileStrings.Count) do
    begin
      ProjectsFiles.Add(FileStrings.Strings[i]);
      Inc(i);
    end;

    aProjectsFiles.Clear();
    aProjectsFiles.NameValueSeparator := ';';
    for i := 0 to Projects.Count - 1 do
    begin
      Name := Projects.Strings[i];
      Value := Trim(ProjectsFiles.Values[Name]);
      if (Value <> '') then
        aProjectsFiles.Add(Name + ';' + Value);
    end;

  finally
    ProjectsFiles.Free();
    Projects.Free();
    FileStrings.Free();
  end;
end;

procedure TMainForm.ParseProjectFile(aFileName: string; out aFiles: TStrings);
var
  i, k: Integer;
  Str, Ext, Text,
  Name, FileName, FormName: string;
  Found: Boolean;
  FileStrings,
  Files,
  Words: TStrings;
begin
  aFiles.Clear();
  aFiles.NameValueSeparator := ';';

  Ext := ExtractFileExt(aFileName);
  if (Ext = '.dpk') then
    Text := 'contains'
  else
  if (Ext = '.dpr') then
    Text := 'uses'
  else
    Exit;

  FileStrings := TStringList.Create();
  Files := TStringList.Create();
  Words := TStringList.Create();
  try
    FileStrings.LoadFromFile(aFileName);
    i := 0;
    Found := False;
    while (not Found) and (i < FileStrings.Count) do
    begin
      Str := FileStrings.Strings[i];
      Found := (Pos(Text, Str) > 0);
      Inc(i);
    end;

    if (not Found) then Exit;

    while (i < FileStrings.Count) do
    begin
      Str := Trim(FileStrings.Strings[i]);
      Files.StrictDelimiter := True;
      Files.CommaText := Str;
      for k := 0 to Files.Count - 1 do
      begin
        Name := '';
        FileName := '';
        FormName := '';
        Words.Clear();
        Split(Files.Strings[k], ' ', Words);
        if (Words.Count > 0) then
        begin
          Name := Words.Strings[0];
          if (Words.Count > 2) and (Words.Strings[1] = 'in') then
          begin
            FileName := Words.Strings[2];
            FileName := Copy(FileName, 2, Length(FileName) - 2);
          end;
          if (Words.Count > 3) then
          begin
            FormName := Words.Strings[3];
            if (FormName[1] = '{') then
              FormName := Copy(FormName, 2, Length(FormName) - 2)
            else
              FormName := '';
          end;
        end;

        if (Name <> '') then
          aFiles.Add(Name + ';' + FileName + ';' + FormName);
      end;

      if (Str[Length(Str)] = ';') then
        Break;

      Inc(i);
    end;
  finally
    Words.Free();
    Files.Free();
    FileStrings.Free();
  end;
end;

procedure TMainForm.OpenBPG(aFileName: string);
var
  i: Integer;
  Files,
  ProjectsFiles: TStrings;
  Name, Value: string;
  Project: TProjectInfo;
begin
  FProjectRootDir := ExtractFilePath(aFileName);
  FProjects.Clear();

  Files := TStringList.Create();
  ProjectsFiles := THashedStringList.Create();
  try
    ParseBPGFile(aFileName, ProjectsFiles);
    for i := 0 to ProjectsFiles.Count - 1 do
    begin
      Name := ProjectsFiles.Names[i];
      Value := ProjectsFiles.Values[Name];
      if (Value = '') then
        Continue;
      if (Value[2] <> ':') then
        Value := FProjectRootDir + Value;

      Files.Clear();
      ParseProjectFile(Value, Files);

      Project := TProjectInfo.Create(Name, Value, Files);
      FProjects.Add(Project);
    end;
  finally
    ProjectsFiles.Free();
    Files.Free();
  end;
end;

procedure TMainForm.BuildTree();
  procedure AddProjectToTree(aProject: TProjectInfo);
  var
    i: Integer;
    ProjectNode,
    FileNode: TTreeNode;
  begin
    if (aProject.Count = 0) then Exit;

    ProjectNode := TreeList.Items.AddChild(nil, aProject.ProjectName);
    ProjectNode.Data := aProject;
    for i := 0 to aProject.Count - 1 do
    begin
      FileNode := TreeList.Items.AddChild(ProjectNode, aProject.UnitName[i]);
      TreeList.SetNodeColumn(FileNode, 1, aProject.FileName[i]);
      TreeList.SetNodeColumn(FileNode, 2, aProject.FormName[i]);
      TreeList.SetNodeColumn(FileNode, 3, aProject.FormCaption[i]);
    end;
  end;

var
  i: Integer;
begin
  TreeList.Items.Clear();
  for i := 0 to FProjects.Count - 1 do
  begin
    AddProjectToTree(TProjectInfo(FProjects.Items[i]));
  end;
end;

procedure TMainForm.edtFilterChange(Sender: TObject);
begin
  if chbAutoApply.Checked then
    actApplyFilterExecute(Sender);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FProjects := TList.Create();
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to FProjects.Count - 1 do
  try
    TObject(FProjects.Items[i]).Free();
  except
  end;
  FProjects.Free();
end;

procedure TMainForm.actApplyFilterExecute(Sender: TObject);
var
  i: Integer;
  Filter: string;
begin
  Filter := edtFilter.Text;
  for i := 0 to FProjects.Count - 1 do
  begin
    TProjectInfo(FProjects.Items[i]).ApplyFilter(Filter);
  end;
  BuildTree();
  TreeList.FullExpand();
end;

procedure TMainForm.actAutoApplyExecute(Sender: TObject);
begin
  if chbAutoApply.Checked then
    actApplyFilterExecute(Sender);
end;

procedure TMainForm.actExecuteFileExecute(Sender: TObject);
var
  Node: TTreeNode;
  Project: TProjectInfo;
  Str,
  ProjectRootDir: string;
begin
  Node := TreeList.Selected;
  if not Assigned(Node) then Exit;

  if Assigned(Node.Parent) then
    Project := TProjectInfo(Node.Parent.Data)
  else
    Project := TProjectInfo(Node.Data);

  if Assigned(Project) then
  begin
    if not Assigned(Node.Parent) then
    begin
      Str := Project.ProjectFileName
    end
    else
    begin
      ProjectRootDir := ExtractFilePath(Project.ProjectFileName);
      Str := ProjectRootDir + TreeList.GetNodeColumn(Node, 1);
    end;
    ShellExecute(Self.Handle, PChar('open'), PChar(Str), nil, nil, 0);
  end;
end;

procedure TMainForm.actOpenProjectExecute(Sender: TObject);
var
  i: Integer;
  ProjectsFiles: TStrings;
  Path,
  Name, Value: string;
begin
  if not OpenDialog.Execute() then Exit;

  OpenBPG(OpenDialog.FileName);
  BuildTree();
  StatusBar.SimpleText := OpenDialog.FileName;
end;

procedure TMainForm.actShowAboutExecute(Sender: TObject);
begin
  AboutFrm.ShowModal();
end;

end.
