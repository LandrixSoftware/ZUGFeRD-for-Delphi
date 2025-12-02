program PSDelphiDefinitionMapper;

{$APPTYPE CONSOLE}

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.RegularExpressions,
  System.Generics.Collections;

type
  TEnumValue = record
    Name: string;
    StringValues: TArray<string>;
  end;

  TMapGenerator = class
  private
    FDelphiFile: string;
    function ParseCommandLine: Boolean;
    function ReadFile(const FileName: string): string;
    procedure WriteFile(const FileName, Content: string);
    function ExtractDefinition(const Content: string): string;
    function ParseEnumValues(const Definition: string): TArray<TEnumValue>;
    function GenerateMappings(const EnumValues: TArray<TEnumValue>): string;
    function ReplaceMappings(const DelphiContent, NewMappings: string): string;
  public
    procedure Execute;
  end;

function TMapGenerator.ParseCommandLine: Boolean;
begin
  Result := False;

  if ParamCount <> 1 then
  begin
    WriteLn('Usage: PSDelphiDefinitionMapper.exe <DelphiFile>');
    WriteLn('Example: PSDelphiDefinitionMapper.exe intf.ZUGFeRDAllowanceReasonCodes.pas');
    Exit;
  end;

  FDelphiFile := ParamStr(1);

  Result := True;
end;

function TMapGenerator.ReadFile(const FileName: string): string;
var
  FileStream: TFileStream;
  Bytes: TBytes;
  StartIndex: Integer;
  HasUTF8BOM: Boolean;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Bytes, FileStream.Size);
    if FileStream.Size > 0 then
      FileStream.ReadBuffer(Bytes[0], FileStream.Size);

    // Check for UTF-8 BOM (EF BB BF)
    HasUTF8BOM := (Length(Bytes) >= 3) and
                  (Bytes[0] = $EF) and
                  (Bytes[1] = $BB) and
                  (Bytes[2] = $BF);

    if HasUTF8BOM then
    begin
      // Skip BOM and read as UTF-8
      StartIndex := 3;
      Result := TEncoding.UTF8.GetString(Bytes, StartIndex, Length(Bytes) - StartIndex);
    end
    else
    begin
      // No BOM - assume ANSI
      Result := TEncoding.Default.GetString(Bytes);
    end;
  finally
    FileStream.Free;
  end;
end;

procedure TMapGenerator.WriteFile(const FileName, Content: string);
var
  FileStream: TFileStream;
  Bytes: TBytes;
  Preamble: TBytes;
begin
  // Write UTF-8 with BOM
  Preamble := TEncoding.UTF8.GetPreamble;
  Bytes := TEncoding.UTF8.GetBytes(Content);

  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    if Length(Preamble) > 0 then
      FileStream.WriteBuffer(Preamble[0], Length(Preamble));
    if Length(Bytes) > 0 then
      FileStream.WriteBuffer(Bytes[0], Length(Bytes));
  finally
    FileStream.Free;
  end;
end;

function TMapGenerator.ExtractDefinition(const Content: string): string;
var
  StartMarker, EndMarker: string;
  StartPos, EndPos: Integer;
begin
  StartMarker := '{.DefinitionStart}';
  EndMarker := '{.DefinitionEnd}';

  StartPos := Pos(StartMarker, Content);
  EndPos := Pos(EndMarker, Content);

  if (StartPos = 0) or (EndPos = 0) then
  begin
    WriteLn('Error: Definition markers not found in Delphi file');
    WriteLn('Please add ', StartMarker, ' and ', EndMarker, ' markers to the file');
    Result := '';
    Exit;
  end;

  Result := Copy(Content, StartPos + Length(StartMarker), EndPos - StartPos - Length(StartMarker));
end;

function TMapGenerator.ParseEnumValues(const Definition: string): TArray<TEnumValue>;
var
  Lines: TStringList;
  i, j: Integer;
  Line, TrimmedLine: string;
  EnumValue: TEnumValue;
  List: TList<TEnumValue>;
  AttributeValues: TArray<string>;
  Match: TMatch;
  MatchCollection: TMatchCollection;
  ValueName: string;
begin
  List := TList<TEnumValue>.Create;
  try
    Lines := TStringList.Create;
    try
      Lines.Text := Definition;
      SetLength(AttributeValues, 0);

      for i := 0 to Lines.Count - 1 do
      begin
        Line := Lines[i];
        TrimmedLine := Trim(Line);

        // Skip empty lines and comments
        if (TrimmedLine = '') or (Pos('///', TrimmedLine) = 1) then
          Continue;

        // Check for EnumStringValue attribute
        if Pos('[EnumStringValue(', TrimmedLine) > 0 then
        begin
          // Extract all values between single quotes (supports multiple parameters)
          MatchCollection := TRegEx.Matches(TrimmedLine, '''([^'']+)''');
          SetLength(AttributeValues, MatchCollection.Count);
          for j := 0 to MatchCollection.Count - 1 do
            AttributeValues[j] := MatchCollection[j].Groups[1].Value;
          Continue;
        end;

        // Check if this is an enum value (not a type declaration or bracket)
        if (Pos('(', TrimmedLine) <> 1) and
           (Pos(')', TrimmedLine) <> 1) and
           (Pos('type', TrimmedLine) <> 1) and
           (TrimmedLine <> '') then
        begin
          // Extract enum value name (before any comma or comment)
          ValueName := TrimmedLine;

          // Remove value assignment if present (e.g., "EnumValue = 4" -> "EnumValue")
          if Pos('=', ValueName) > 0 then
            ValueName := Copy(ValueName, 1, Pos('=', ValueName) - 1);

          // Remove trailing comma
          if Pos(',', ValueName) > 0 then
            ValueName := Copy(ValueName, 1, Pos(',', ValueName) - 1);

          // Remove inline comment
          if Pos('//', ValueName) > 0 then
            ValueName := Copy(ValueName, 1, Pos('//', ValueName) - 1);

          ValueName := Trim(ValueName);

          if ValueName <> '' then
          begin
            EnumValue.Name := ValueName;
            if Length(AttributeValues) > 0 then
              EnumValue.StringValues := Copy(AttributeValues)
            else
            begin
              SetLength(EnumValue.StringValues, 1);
              EnumValue.StringValues[0] := ValueName;
            end;

            List.Add(EnumValue);
            SetLength(AttributeValues, 0); // Reset for next value
          end;
        end;
      end;

      Result := List.ToArray;
    finally
      Lines.Free;
    end;
  finally
    List.Free;
  end;
end;

function TMapGenerator.GenerateMappings(const EnumValues: TArray<TEnumValue>): string;
var
  Output: TStringBuilder;
  i, j: Integer;
  MaxNameLength: Integer;
  IsFirst: Boolean;
begin
  Output := TStringBuilder.Create;
  try
    // Calculate max name length for alignment
    MaxNameLength := 0;
    for i := 0 to High(EnumValues) do
      if Length(EnumValues[i].Name) > MaxNameLength then
        MaxNameLength := Length(EnumValues[i].Name);

    // Generate Map calls
    IsFirst := True;
    for i := 0 to High(EnumValues) do
    begin
      // Generate a Map call for each string value
      for j := 0 to High(EnumValues[i].StringValues) do
      begin
        if not IsFirst then
          Output.AppendLine;
        IsFirst := False;

        Output.Append('  Map(');
        Output.Append(EnumValues[i].Name);
        Output.Append(',');
        Output.Append(StringOfChar(' ', MaxNameLength - Length(EnumValues[i].Name) + 1));
        Output.Append('''');
        Output.Append(EnumValues[i].StringValues[j]);
        Output.Append('''');
        Output.Append(');');
      end;
    end;

    Result := Output.ToString;
  finally
    Output.Free;
  end;
end;

function TMapGenerator.ReplaceMappings(const DelphiContent, NewMappings: string): string;
var
  StartMarker, EndMarker: string;
  StartPos, EndPos: Integer;
  BeforeMarker, AfterMarker: string;
begin
  StartMarker := '{.MapStart}';
  EndMarker := '{.MapEnd}';

  StartPos := Pos(StartMarker, DelphiContent);
  EndPos := Pos(EndMarker, DelphiContent);

  if (StartPos = 0) or (EndPos = 0) then
  begin
    WriteLn('Error: Map markers not found in Delphi file');
    WriteLn('Please add ', StartMarker, ' and ', EndMarker, ' markers to the file');
    Result := DelphiContent;
    Exit;
  end;

  BeforeMarker := Copy(DelphiContent, 1, StartPos + Length(StartMarker) - 1);
  AfterMarker := Copy(DelphiContent, EndPos, Length(DelphiContent) - EndPos + 1);

  Result := BeforeMarker + sLineBreak +
            '  // Mapping generated by PSDelphiDefinitionMapper' + sLineBreak +
            NewMappings + sLineBreak + AfterMarker;
end;

procedure TMapGenerator.Execute;
var
  DelphiContent: string;
  Definition: string;
  EnumValues: TArray<TEnumValue>;
  Mappings: string;
  UpdatedDelphi: string;
  i: Integer;
begin
  if not ParseCommandLine then
    Exit;

  WriteLn('Generating Delphi enum mappings...');
  WriteLn('  Delphi File: ', FDelphiFile);
  WriteLn;

  try
    // Read Delphi file
    if not FileExists(FDelphiFile) then
    begin
      WriteLn('Error: Delphi file not found: ', FDelphiFile);
      Exit;
    end;
    DelphiContent := ReadFile(FDelphiFile);
    WriteLn('Read Delphi file: ', Length(DelphiContent), ' bytes');

    // Extract definition
    Definition := ExtractDefinition(DelphiContent);
    if Definition = '' then
      Exit;
    WriteLn('Extracted enum definition');

    // Parse enum values
    EnumValues := ParseEnumValues(Definition);
    WriteLn('Found ', Length(EnumValues), ' enum values');

    if Length(EnumValues) = 0 then
    begin
      WriteLn('Warning: No enum values found');
      Exit;
    end;

    // Display found values
    for i := 0 to High(EnumValues) do
    begin
      Write('  ', EnumValues[i].Name, ' -> ');
      for var j := 0 to High(EnumValues[i].StringValues) do
      begin
        if j > 0 then Write(', ');
        Write('''', EnumValues[i].StringValues[j], '''');
      end;
      WriteLn;
    end;
    WriteLn;

    // Generate mappings
    Mappings := GenerateMappings(EnumValues);
    WriteLn('Generated mapping code');

    // Replace mappings
    UpdatedDelphi := ReplaceMappings(DelphiContent, Mappings);

    // Write updated file
    WriteFile(FDelphiFile, UpdatedDelphi);
    WriteLn('Updated Delphi file successfully');
    WriteLn;
    WriteLn('Mapping generation completed!');

  except
    on E: Exception do
      WriteLn('Error: ', E.Message);
  end;
end;

var
  Generator: TMapGenerator;
begin
  // Set console to UTF-8 to handle Unicode characters
  SetConsoleOutputCP(CP_UTF8);

  try
    Generator := TMapGenerator.Create;
    try
      Generator.Execute;
    finally
      Generator.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
