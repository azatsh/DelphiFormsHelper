unit ProjectInfo;

interface

uses
  Classes;

type
  TProjectInfo = class
  private
    FName: string;
    FFileName: string;

    FFiles: TStrings;
    FFormsCaptions: TStrings;

    FFiltered: Boolean;
    FFilteredLines: array of Integer;
    FFilteredCount: Integer;
  protected
    function GetUnitName(aIndex: Integer): string;
    function GetFileName(aIndex: Integer): string;
    function GetFormName(aIndex: Integer): string;
    function GetFormCaption(aIndex: Integer): string;

    procedure ExtractFormsCaptions();
    function ExtractFormType(FileName: string): string;
    function ExtractFormCaption(FileName: string): string;

    function GetCount(): Integer;
  public
    constructor Create(aName, aFileName: string; aFiles: TStrings);
    destructor Destroy(); override;

    procedure ApplyFilter(aFilter: string);
    procedure ClearFilter();

    property ProjectName: string read FName;
    property ProjectFileName: string read FFileName;

    property UnitName[Index: Integer]: string read GetUnitName;
    property FileName[Index: Integer]: string read GetFileName;
    property FormName[Index: Integer]: string read GetFormName;
    property FormCaption[Index: Integer]: string read GetFormCaption;

    property Count: Integer read GetCount;
    property Filtered: Boolean read FFiltered;
  end;

implementation

uses
  IniFiles, SysUtils;

function TProjectInfo.GetUnitName(aIndex: Integer): string;
begin
  if (aIndex < 0) or (aIndex >= Count) then
    Result := ''
  else
  begin
    if Filtered then
      aIndex := FFilteredLines[aIndex];
    Result := FFiles.Names[aIndex];
  end;
end;

function TProjectInfo.GetFileName(aIndex: Integer): string;
var
  i: Integer;
begin
  if (aIndex < 0) or (aIndex >= Count) then
    Result := ''
  else
  begin
    if Filtered then
      aIndex := FFilteredLines[aIndex];

    Result := FFiles.Values[FFiles.Names[aIndex]];
    i := Pos(FFiles.NameValueSeparator, Result);
    if (i > 0) then
      Result := Copy(Result, 1, i-1);
  end;
end;

function TProjectInfo.GetFormName(aIndex: Integer): string;
var
  i: Integer;
  Str: string;
begin
  Result := '';
  if (aIndex < 0) or (aIndex >= Count) then Exit;

  if Filtered then
    aIndex := FFilteredLines[aIndex];

  Str := FFiles.Values[FFiles.Names[aIndex]];
  i := Pos(FFiles.NameValueSeparator, Str);
  if (i > 0) then
    Result := Copy(Str, i+1, Length(Str)-i);
end;

function TProjectInfo.GetFormCaption(aIndex: Integer): string;
begin
  Result := '';
  if (aIndex < 0) or (aIndex >= Count) then Exit;

  if Filtered then
    aIndex := FFilteredLines[aIndex];

  ExtractFormsCaptions();
  Result := FFormsCaptions.Strings[aIndex];
end;

procedure TProjectInfo.ExtractFormsCaptions();
var
  i: Integer;
  Str, Caption,
  ProjectRootDir: string;
begin
  if (FFormsCaptions.Count > 0) then Exit;

  ProjectRootDir := ExtractFilePath(FFileName);
  for i := 0 to FFiles.Count - 1 do
  begin
    Caption := '';
    Str := FileName[i];
    Str := ProjectRootDir + Copy(Str, 1, Length(Str)-3) + 'dfm';
    if FileExists(Str) then
    begin
      Caption := ExtractFormCaption(Str);
    end;
    FFormsCaptions.Add(Caption);
  end;
end;

function TProjectInfo.ExtractFormType(FileName: string): string;
var
  i: Integer;
  str: string;
  List: TStrings;
begin
  Result := '';
  if not FileExists(FileName) then Exit;

  List := TStringList.Create;
  List.LoadFromFile(FileName);
  str := List.Strings[0];
  List.Free;

  i := Pos(':', str);
  if i > 0 then
    Result := Trim(Copy(str, i+1, Length(str)-i));
end;

function TProjectInfo.ExtractFormCaption(FileName: string): string;
var
  Text: string;
  Caption: string;
  IsNum: Boolean;
  Word: string;
  IsWord: Boolean;
  i: Integer;
  Strings: TStringList;
begin
  Result := '';
  if not FileExists(FileName) then Exit;

  Strings := TStringList.Create;
  Strings.LoadFromFile(FileName);
  Text := Strings.Text;
  Strings.Free;

  i := Pos(#13#10'  Caption ', Text);
  if i > 0 then
  begin
    while (i <= Length(Text)) and (Text[i] <> '=') do
      Inc(i);
    Inc(i);
    Caption := '';
    repeat
      Caption := Caption + Text[i];
      Inc(i);
    until (i > Length(Text)) or (Text[i] = #13);
    if (Caption <> '') then
    begin
      Word := '';
      IsNum := False;
      IsWord := False;
      for i := 1 to Length(Caption) do
      begin
        case Caption[i] of
          '#':
          begin
            if IsNum then
            begin
              Result := Result + WideChar(StrToInt(Word));
              Word := '';
            end
            else
              IsNum := True;
          end;

          #39:
          begin
            if IsNum then
            begin
              Result := Result + WideChar(StrToInt(Word));
              IsNum := False;
              Word := '';
            end;

            if IsWord then
            begin
              Result := Result + Word;
              Word := '';
            end;

            IsWord := not IsWord;
          end;

          else
          begin
            Word := Word + Caption[i];
          end;
        end;
      end;
      if IsNum then
        Result := Result + WideChar(StrToInt(Word));
      if IsWord then
        Result := Result + Word;
    end;
  end;
end;

function TProjectInfo.GetCount(): Integer;
begin
  if Filtered then
    Result := FFilteredCount
  else
    Result := FFiles.Count;
end;

constructor TProjectInfo.Create(aName, aFileName: string; aFiles: TStrings);
begin
  FName := aName;
  FFileName := aFileName;

  FFiles := THashedStringList.Create();
  FFiles.NameValueSeparator := aFiles.NameValueSeparator;
  FFiles.AddStrings(aFiles);

  FFormsCaptions := TStringList.Create();

  FFiltered := False;
  FFilteredCount := 0;
  SetLength(FFilteredLines, FFiles.Count);
end;

destructor TProjectInfo.Destroy();
begin
  FFiles.Free();
  SetLength(FFilteredLines, 0);
  FFormsCaptions.Free();

  inherited;
end;

procedure TProjectInfo.ApplyFilter(aFilter: string);
var
  i: Integer;
  Accept: Boolean;
begin
  ClearFilter();
  if (aFilter <> '') then
  begin
    FFilteredCount := 0;
    aFilter := AnsiLowerCase(aFilter);
    for i := 0 to FFiles.Count - 1 do
    begin
      Accept := (Pos(aFilter, AnsiLowerCase(UnitName[i])) > 0) or
                (Pos(aFilter, AnsiLowerCase(FormName[i])) > 0) or
                (Pos(aFilter, AnsiLowerCase(FormCaption[i])) > 0);
      if Accept then
      begin
        FFilteredLines[FFilteredCount] := i;
        Inc(FFilteredCount);
      end;
    end;
    FFiltered := True;
  end;
end;

procedure TProjectInfo.ClearFilter();
begin
  FFiltered := False;
  FFilteredCount := 0;
end;

end.
