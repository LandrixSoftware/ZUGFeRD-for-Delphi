program PSCSharpToDelphiDefinition;

{$APPTYPE CONSOLE}

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Generics.Collections;

type
  TEnumConverter = class
  private
    FCSharpFile: string;
    FDelphiFile: string;
    FCSharpEnum: string;
    function ParseCommandLine: Boolean;
    function ReadFile(const FileName: string): string;
    procedure WriteFile(const FileName, Content: string);
    function ExtractEnumDefinition(const Content: string): string;
    function IsDelphiReservedWord(const Word: string): Boolean;
    function ExtractEnumValueName(const Line: string): string;
    function ConvertToDelphiSyntax(const CSharpEnum: string): string;
    function GetIndentation(const Line: string): string;
    function IndentLines(const Content, Indentation: string): string;
    function ReplaceDelphiDefinition(const DelphiContent, NewDefinition: string): string;
  public
    procedure Execute;
  end;

function TEnumConverter.ParseCommandLine: Boolean;
begin
  Result := False;

  if ParamCount <> 3 then
  begin
    WriteLn('Usage: PSC#ToDelphiDefinition.exe <C#Enum> <C#File> <DelphiFile>');
    WriteLn('Example: PSC#ToDelphiDefinition.exe AllowanceReasonCodes AllowanceReasonCodes.cs intf.ZUGFeRDAllowanceReasonCodes.pas');
    Exit;
  end;

  FCSharpEnum := ParamStr(1);
  FCSharpFile := ParamStr(2);
  FDelphiFile := ParamStr(3);

  Result := True;
end;

function TEnumConverter.ReadFile(const FileName: string): string;
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
      // No BOM - read as UTF-8
      Result := TEncoding.UTF8.GetString(Bytes);
    end;
  finally
    FileStream.Free;
  end;
end;

procedure TEnumConverter.WriteFile(const FileName, Content: string);
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

function TEnumConverter.ExtractEnumDefinition(const Content: string): string;
var
  StartPos, EndPos: Integer;
  EnumKeyword: string;
  BraceCount: Integer;
  i: Integer;
begin
  Result := '';
  EnumKeyword := 'public enum ' + FCSharpEnum;

  StartPos := Pos(EnumKeyword, Content);
  if StartPos = 0 then
  begin
    EnumKeyword := 'enum ' + FCSharpEnum;
    StartPos := Pos(EnumKeyword, Content);
  end;

  if StartPos = 0 then
  begin
    WriteLn('Error: Enum "', FCSharpEnum, '" not found in C# file');
    Exit;
  end;

  // Find the opening brace
  i := StartPos;
  while (i <= Length(Content)) and (Content[i] <> '{') do
    Inc(i);

  if i > Length(Content) then
  begin
    WriteLn('Error: Opening brace not found for enum');
    Exit;
  end;

  Inc(i); // Skip opening brace
  BraceCount := 1;
  EndPos := i;

  // Find matching closing brace
  while (EndPos <= Length(Content)) and (BraceCount > 0) do
  begin
    if Content[EndPos] = '{' then
      Inc(BraceCount)
    else if Content[EndPos] = '}' then
      Dec(BraceCount);
    Inc(EndPos);
  end;

  if BraceCount <> 0 then
  begin
    WriteLn('Error: Matching brace not found for enum');
    Exit;
  end;

  // Extract content between braces (excluding the braces themselves)
  Result := Copy(Content, i, EndPos - i - 1);
end;

function TEnumConverter.IsDelphiReservedWord(const Word: string): Boolean;
const
  ReservedWords: array[0..64] of string = (
    'AND', 'ARRAY', 'AS', 'ASM', 'BEGIN', 'CASE', 'CLASS', 'CONST', 'CONSTRUCTOR',
    'DESTRUCTOR', 'DISPINTERFACE', 'DIV', 'DO', 'DOWNTO', 'ELSE', 'END', 'EXCEPT',
    'EXPORTS', 'FILE', 'FINALIZATION', 'FINALLY', 'FOR', 'FUNCTION', 'GOTO', 'IF',
    'IMPLEMENTATION', 'IN', 'INHERITED', 'INITIALIZATION', 'INLINE', 'INTERFACE',
    'IS', 'LABEL', 'LIBRARY', 'MOD', 'NIL', 'NOT', 'OBJECT', 'OF', 'OR', 'OUT',
    'PACKED', 'PROCEDURE', 'PROGRAM', 'PROPERTY', 'RAISE', 'RECORD', 'REPEAT',
    'RESOURCESTRING', 'SET', 'SHL', 'SHR', 'STRING', 'THEN', 'THREADVAR', 'TO',
    'TRY', 'TYPE', 'UNIT', 'UNTIL', 'USES', 'VAR', 'WHILE', 'WITH', 'XOR'
  );
var
  i: Integer;
  UpperWord: string;
begin
  UpperWord := UpperCase(Word);
  Result := False;
  for i := Low(ReservedWords) to High(ReservedWords) do
    if ReservedWords[i] = UpperWord then
    begin
      Result := True;
      Exit;
    end;
end;

function TEnumConverter.ExtractEnumValueName(const Line: string): string;
var
  TrimmedLine: string;
begin
  TrimmedLine := Trim(Line);

  // Remove trailing comma
  if (Length(TrimmedLine) > 0) and (TrimmedLine[Length(TrimmedLine)] = ',') then
    TrimmedLine := Copy(TrimmedLine, 1, Length(TrimmedLine) - 1);

  // Remove value assignment (e.g., "EnumValue = 4" -> "EnumValue")
  if Pos('=', TrimmedLine) > 0 then
    TrimmedLine := Copy(TrimmedLine, 1, Pos('=', TrimmedLine) - 1);

  // Remove inline comment
  if Pos('//', TrimmedLine) > 0 then
    TrimmedLine := Copy(TrimmedLine, 1, Pos('//', TrimmedLine) - 1);

  Result := Trim(TrimmedLine);
end;

function TEnumConverter.ConvertToDelphiSyntax(const CSharpEnum: string): string;
var
  Lines: TStringList;
  i: Integer;
  Line, TrimmedLine, EnumValueName, LineIndent: string;
  Output: TStringBuilder;
  EnumValues: TList<Integer>;
  IsEnumValue: Boolean;
  HasAttribute: Boolean;
  HasUnknown: Boolean;
  LastEnumLine: string;
begin
  Lines := TStringList.Create;
  EnumValues := TList<Integer>.Create;
  try
    Lines.Text := CSharpEnum;

    // First pass: find all enum value line indices and check for "Unknown"
    HasUnknown := False;
    for i := 0 to Lines.Count - 1 do
    begin
      TrimmedLine := Trim(Lines[i]);
      IsEnumValue := (TrimmedLine <> '') and
                     (Pos('///', TrimmedLine) <> 1) and
                     (Pos('[', TrimmedLine) <> 1);
      if IsEnumValue then
      begin
        EnumValues.Add(i);

        // Check if this is "Unknown"
        EnumValueName := ExtractEnumValueName(Lines[i]);
        if SameText(EnumValueName, 'Unknown') then
          HasUnknown := True;
      end;
    end;

    Output := TStringBuilder.Create;
    try
      // Second pass: process all lines
      i := 0;
      while i < Lines.Count do
      begin
        Line := Lines[i];
        TrimmedLine := Trim(Line);

        // Skip empty lines
        if TrimmedLine = '' then
        begin
          Output.AppendLine(Line);
          Inc(i);
          Continue;
        end;

        // Handle XML comments
        if Pos('///', TrimmedLine) = 1 then
        begin
          Output.AppendLine(Line);
          Inc(i);
          Continue;
        end;

        // Convert EnumStringValue attribute: double quotes to single quotes
        if Pos('[EnumStringValue(', Line) > 0 then
        begin
          Line := StringReplace(Line, '[EnumStringValue("', '[EnumStringValue(''', [rfReplaceAll]);
          Line := StringReplace(Line, '", "', ''', ''', [rfReplaceAll]); // Handle two parameters
          Line := StringReplace(Line, '")]', ''')]', [rfReplaceAll]);
          Output.AppendLine(Line);
          Inc(i);
          Continue;
        end;

        // Check if this is an enum value
        IsEnumValue := EnumValues.Contains(i);

        if IsEnumValue then
        begin
          // Extract enum value name
          EnumValueName := ExtractEnumValueName(Line);

          // Check if previous line has an attribute (look back for [EnumStringValue])
          HasAttribute := False;
          if i > 0 then
          begin
            TrimmedLine := Trim(Lines[i - 1]);
            HasAttribute := Pos('[EnumStringValue(', TrimmedLine) > 0;
          end;

          // If enum value is a reserved word...
          if IsDelphiReservedWord(EnumValueName) then
          begin
            // Get line indentation
            LineIndent := Copy(Line, 1, Length(Line) - Length(TrimLeft(Line)));

            // if has no Attribute Add EnumStringValue attribute with original name
            if not HasAttribute then
              Output.AppendLine(LineIndent + '[EnumStringValue(''' + EnumValueName + ''')]');

            // Modify enum value name by adding underscore
            Line := StringReplace(Line, EnumValueName, EnumValueName + '_', []);
          end;

          // Store the line for getting indentation later
          if EnumValues.Count > 0 then
            LastEnumLine := Line;

          // Remove trailing comma if present
          Line := TrimRight(Line);
          if (Length(Line) > 0) and (Line[Length(Line)] = ',') then
            Line := Copy(Line, 1, Length(Line) - 1);

          // Add comma after value (always, we'll add Unknown at the end if needed)
          if i <> EnumValues[EnumValues.Count - 1] then
            Line := Line + ','
          else if not HasUnknown then
            // This is the last enum value and we need to add Unknown, so add comma
            Line := Line + ',';

          Output.AppendLine(Line);
        end
        else
        begin
          Output.AppendLine(Line);
        end;

        Inc(i);
      end;

      // Add "Unknown" at the end if it doesn't exist
      if not HasUnknown and (EnumValues.Count > 0) then
      begin
        // Get indentation from last enum line
        LineIndent := Copy(LastEnumLine, 1, Length(LastEnumLine) - Length(TrimLeft(LastEnumLine)));

        Output.AppendLine('');
        Output.AppendLine(LineIndent + '/// <summary>');
        Output.AppendLine(LineIndent + '/// Unknown value');
        Output.AppendLine(LineIndent + '/// </summary>');
        Output.Append(LineIndent + 'Unknown');
      end;

      Result := Output.ToString;
    finally
      Output.Free;
    end;
  finally
    EnumValues.Free;
    Lines.Free;
  end;
end;

function TEnumConverter.GetIndentation(const Line: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(Line) do
  begin
    if (Line[i] = ' ') or (Line[i] = #9) then
      Result := Result + Line[i]
    else
      Break;
  end;
end;

function TEnumConverter.IndentLines(const Content, Indentation: string): string;
var
  Lines: TStringList;
  i: Integer;
  Output: TStringBuilder;
  TrimmedLine: string;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := Content;
    Output := TStringBuilder.Create;
    try
      for i := 0 to Lines.Count - 1 do
      begin
        TrimmedLine := TrimLeft(Lines[i]); // Remove leading whitespace
        if TrimmedLine <> '' then
          Output.AppendLine(Indentation + TrimmedLine)
        else
          Output.AppendLine(''); // Empty lines stay empty
      end;
      Result := Output.ToString;
    finally
      Output.Free;
    end;
  finally
    Lines.Free;
  end;
end;

function TEnumConverter.ReplaceDelphiDefinition(const DelphiContent, NewDefinition: string): string;
var
  StartMarker, EndMarker: string;
  StartPos, EndPos, LineStart: Integer;
  BeforeMarker, AfterMarker, MarkerLine: string;
  Indentation, IndentedDefinition: string;
  i: Integer;
begin
  StartMarker := '{.DefinitionStart}';
  EndMarker := '{.DefinitionEnd}';

  StartPos := Pos(StartMarker, DelphiContent);
  EndPos := Pos(EndMarker, DelphiContent);

  if (StartPos = 0) or (EndPos = 0) then
  begin
    WriteLn('Error: Definition markers not found in Delphi file');
    WriteLn('Please add ', StartMarker, ' and ', EndMarker, ' markers to the file');
    Result := DelphiContent;
    Exit;
  end;

  // Find the beginning of the line containing StartMarker
  LineStart := StartPos;
  while (LineStart > 1) and (DelphiContent[LineStart - 1] <> #10) do
    Dec(LineStart);

  // Extract the line and get its indentation
  i := StartPos;
  while (i <= Length(DelphiContent)) and (DelphiContent[i] <> #13) and (DelphiContent[i] <> #10) do
    Inc(i);
  MarkerLine := Copy(DelphiContent, LineStart, i - LineStart);
  Indentation := GetIndentation(MarkerLine);

  // Apply indentation to all lines in the new definition
  IndentedDefinition := IndentLines(NewDefinition, Indentation);

  BeforeMarker := Copy(DelphiContent, 1, StartPos + Length(StartMarker) - 1);
  AfterMarker := Copy(DelphiContent, EndPos, Length(DelphiContent) - EndPos + 1);

  Result := BeforeMarker + sLineBreak +
            Indentation + '// automatically converted by PSC#ToDelphiDefinition' + sLineBreak +
            IndentedDefinition + Indentation + AfterMarker;
end;

procedure TEnumConverter.Execute;
var
  CSharpContent, DelphiContent: string;
  ExtractedEnum, ConvertedEnum: string;
  UpdatedDelphi: string;
begin
  if not ParseCommandLine then
    Exit;

  WriteLn('Converting C# enum to Delphi...');
  WriteLn('  C# File: ', FCSharpFile);
  WriteLn('  Delphi File: ', FDelphiFile);
  WriteLn('  Enum Name: ', FCSharpEnum);
  WriteLn;

  try
    // Read C# file
    if not FileExists(FCSharpFile) then
    begin
      WriteLn('Error: C# file not found: ', FCSharpFile);
      Exit;
    end;
    CSharpContent := ReadFile(FCSharpFile);
    WriteLn('Read C# file: ', Length(CSharpContent), ' bytes');

    // Extract enum definition
    ExtractedEnum := ExtractEnumDefinition(CSharpContent);
    if ExtractedEnum = '' then
      Exit;
    WriteLn('Extracted enum definition');

    // Convert to Delphi syntax
    ConvertedEnum := ConvertToDelphiSyntax(ExtractedEnum);
    WriteLn('Converted to Delphi syntax');

    // Read Delphi file
    if not FileExists(FDelphiFile) then
    begin
      WriteLn('Error: Delphi file not found: ', FDelphiFile);
      Exit;
    end;
    DelphiContent := ReadFile(FDelphiFile);
    WriteLn('Read Delphi file: ', Length(DelphiContent), ' bytes');

    // Replace definition
    UpdatedDelphi := ReplaceDelphiDefinition(DelphiContent, ConvertedEnum);

    // Write updated file
    WriteFile(FDelphiFile, UpdatedDelphi);
    WriteLn('Updated Delphi file successfully');
    WriteLn;
    WriteLn('Conversion completed!');

  except
    on E: Exception do
      WriteLn('Error: ', E.Message);
  end;
end;

var
  Converter: TEnumConverter;
begin
  // Set console to UTF-8 to handle Unicode characters
  SetConsoleOutputCP(CP_UTF8);

  try
    Converter := TEnumConverter.Create;
    try
      Converter.Execute;
    finally
      Converter.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
