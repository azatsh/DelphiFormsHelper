var
  i: Integer;
  MainGrid: TNDBGrid;
  DataSet: TDataSet;
  Field: DB.TField;
  Info: uDocInfo.TDocInfo;
begin
  for i := 0 to ComponentCount - 1 do
    if (Components[i] is TNDBGrid) then
      Break;
  MainGrid := (Components[i] as TNDBGrid);
  DataSet := MainGrid.DataSource.DataSet;

  Info.ProviderName := '';
  if (DataSet is TNDataSet) then
    Info.ProviderName := (DataSet as TNDataSet).DataStore.ProviderName;
  SetLength(Info.Fields, DataSet.Fields.Count);
  for i := 0 to DataSet.Fields.Count - 1 do
  with DataSet.Fields do
  begin
    Field := Fields[i];
    Info.Fields[i].Name := Field.FieldName;
    Info.Fields[i].Caption := Field.DisplayName;
    Info.Fields[i].KeyFields := Field.KeyFields;
    Info.Fields[i].LookupKeyFields := Field.LookupKeyFields;
    Info.Fields[i].LookupResultField := Field.LookupResultField;

    if (Field is TStringField) then
      Info.Fields[i].FieldType := FT_STRING
    else
    if (Field is TDateField) or (Field is TDateTimeField) then
      Info.Fields[i].FieldType := FT_DATE
    else
    if (Field is TLargeintField) or (Field is TIntegerField) then
      Info.Fields[i].FieldType := FT_LONG
    else
    if (Field is TBCDField) then
      Info.Fields[i].FieldType := FT_MONEY
    else
      Info.Fields[i].FieldType := FT_NONE;
  end;
  TInfoForm.CreateAndShow(Info);
end;  