{* Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.}

unit intf.ZUGFeRDHelper;

interface

uses
  Winapi.Windows, Winapi.Messages
  ,System.SysUtils,System.Classes,System.Types,System.DateUtils,System.Rtti
  ,System.Variants,System.IOUtils,System.Win.COMObj
  ,System.NetEncoding
  ,Generics.Defaults
  ;

type

  TZUGFeRDHelper = class(TObject)
  public
    class function CreateUuid : String;
    class function GetDataAsBase64(_Stream : TStream) : String;
  end;

  IZUGFeRDPdfHelper = interface
    ['{EB5E0786-D33B-4811-9749-E58DE90102F7}']
    function SetPdfTkServerPath(const _Path : String) : IZUGFeRDPdfHelper;
    function PdfTkServerGetZUGFeRDPdfAttachment(const _PdfFilename : String; out _Attachment : TStream; out _CmdOutput : String) : Boolean;

    function SetJavaRuntimeEnvironmentPath(const _Path : String) : IZUGFeRDPdfHelper;
    function SetMustangprojectLibPath(const _Path : String) : IZUGFeRDPdfHelper;

//    function Validate(const _InvoiceXMLData : String; out _CmdOutput,_ValidationResultAsXML,_ValidationResultAsHTML : String) : Boolean;
    function ValidateFile(const _InvoiceXMLFilename : String; out _CmdOutput,_ValidationResultAsXML : String) : Boolean;
    function Visualize(const _InvoiceXMLData : String; out _CmdOutput,_VisualizationAsHTML : String) : Boolean;
    function VisualizeFile(const _InvoiceXMLFilename : String; out _CmdOutput,_VisualizationAsHTML : String) : Boolean;
    function VisualizeFileAsPdf(const _InvoiceXMLFilename : String; out _CmdOutput : String; out _VisualizationAsPdf : TMemoryStream) : Boolean;
  end;

  function GetZUGFeRDPdfHelper : IZUGFeRDPdfHelper;

type
  // ZUGFeRDNullableParam wird für die Parameterübergabe verwendet, da dies die Möglichkeit schafft, Default Parameter zu realisieren
  // entweder Nil, dann ohne Wert oder mit Wert, das <> Nil
  IZUGFeRDNullableParam<T> = interface
    function GetValue: T;
    procedure SetValue(const AValue: T);
    Property Value: T read GetValue Write SetValue;
  end;

  TZUGFeRDNullableParam<T> = class (TInterfacedObject, IZUGFeRDNullableParam<T>)
    FValue: T;
    function GetValue: T;
    procedure SetValue(const AValue: T);
  public
    Property Value: T read GetValue Write SetValue;
    constructor Create (AValue: T);
  end;

  // ZUGFeRDNullable as managed record
  ZUGFeRDNullable<T> = record
  private
    FValue: T;
    FHasValue: Boolean;
    class operator Initialize (out Dest: ZUGFeRDNullable<T>);
    function GetValue: T;
  public
    constructor Create (Dummy: Boolean); overload;
    constructor Create(AValue: T); overload;
    function GetValueOrDefault: T; overload;
    function GetValueOrDefault(Default: T): T; overload;
    property HasValue: Boolean read FHasValue;
    property Value: T read GetValue;

    class operator NotEqual(ALeft, ARight: ZUGFeRDNullable<T>): Boolean;
    class operator Equal(ALeft, ARight: ZUGFeRDNullable<T>): Boolean;

    class operator Implicit(Value: ZUGFeRDNullable<T>): T;
    class operator Implicit(Value: T): ZUGFeRDNullable<T>;
    class operator Implicit(Param: IZUGFeRDNullableParam<T>): ZUGFeRDNullable<T>;
    class operator Implicit(Param: ZUGFeRDNullable<T>): IZUGFeRDNullableParam<T>;
    class operator Explicit(Value: ZUGFeRDNullable<T>): T;
    procedure ClearValue;
  end;

  ZUGFeRDNullableDouble = ZUGFeRDNullable<Double>;
  ZUGFeRDNullableInt = ZUGFeRDNullable<Integer>;
  ZUGFeRDNullableDateTime = ZUGFeRDNullable<TDateTime>;
  ZUGFeRDNullableCurrency = ZUGFeRDNullable<Currency>;

implementation

type
  TZUGFeRDPdfHelper = class(TInterfacedObject,IZUGFeRDPdfHelper)
  private
    PdfTkServerPath : String;
    JavaRuntimeEnvironmentPath : String;
    //https://github.com/ZUGFeRD/mustangproject/blob/master/Mustang-CLI/src/main/java/org/mustangproject/commandline/Main.java
    //https://www.mustangproject.org/kommandozeile/?lang=de
    MustangprojectPath : String;
    CmdOutput : TStringList;
    function ExecAndWait(_Filename, _Params: string): Boolean;
    function QuoteIfContainsSpace(const _Value : String) : String;
  public
    constructor Create;
    destructor Destroy; override;
    function SetPdfTkServerPath(const _Path : String) : IZUGFeRDPdfHelper;
    function PdfTkServerGetZUGFeRDPdfAttachment(const _PdfFilename : String; out _Attachment : TStream; out _CmdOutput : String) : Boolean;

    function SetJavaRuntimeEnvironmentPath(const _Path : String) : IZUGFeRDPdfHelper;
    function SetMustangprojectLibPath(const _Path : String) : IZUGFeRDPdfHelper;

    function Visualize(const _InvoiceXMLData : String; out _CmdOutput,_VisualizationAsHTML : String) : Boolean;
    function VisualizeFile(const _InvoiceXMLFilename : String; out _CmdOutput,_VisualizationAsHTML : String) : Boolean;
    function VisualizeFileAsPdf(const _InvoiceXMLFilename : String; out _CmdOutput : String; out _VisualizationAsPdf : TMemoryStream) : Boolean;
    function ValidateFile(const _InvoiceXMLFilename : String; out _CmdOutput,_ValidationResultAsXML : String) : Boolean;
  end;

function GetZUGFeRDPdfHelper : IZUGFeRDPdfHelper;
begin
  Result := TZUGFeRDPdfHelper.Create;
end;

{ TZUGFeRDPdfHelper }

constructor TZUGFeRDPdfHelper.Create;
begin
  CmdOutput := TStringList.Create;
end;

destructor TZUGFeRDPdfHelper.Destroy;
begin
  if Assigned(CmdOutput) then begin CmdOutput.Free; CmdOutput := nil; end;
  inherited;
end;

function TZUGFeRDPdfHelper.ExecAndWait(_Filename, _Params: string): Boolean;
var
  SA: TSecurityAttributes;
  SI: TStartupInfoA;
  PI: TProcessInformation;
  StdOutPipeRead, StdOutPipeWrite: THandle;
  WasOK: Boolean;
  Buffer: array[0..255] of AnsiChar;
  BytesRead: Cardinal;
  Handle:Boolean;
  ProcessExitCode : DWORD;
  ReadLine : AnsiString;
begin
  Result := false;
  CmdOutput.Clear;

  _Filename := QuoteIfContainsSpace(_Filename);

  SA.nLength := SizeOf(SA);
  SA.bInheritHandle := True;
  SA.lpSecurityDescriptor := nil;

  CreatePipe(StdOutPipeRead, StdOutPipeWrite, @SA, 0);
  try

    FillChar(SI, SizeOf(SI), 0);
    SI.cb := SizeOf(SI);
    SI.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    SI.wShowWindow := SW_HIDE;
    SI.hStdInput := GetStdHandle(STD_INPUT_HANDLE); // don't redirect stdin
    SI.hStdOutput := StdOutPipeWrite;
    SI.hStdError := StdOutPipeWrite;

    Handle := CreateProcessA(nil, PAnsiChar(AnsiString(_Filename+ ' ' + _Params)),
                            nil, nil, True, 0, nil,
                            PAnsiChar(AnsiString(ExtractFileDir(ParamStr(0)))), SI, PI);
    CloseHandle(StdOutPipeWrite);
    if Handle then
      try
        repeat
          WasOK := ReadFile(StdOutPipeRead, Buffer, 255, BytesRead, nil);
          if BytesRead > 0 then
          begin
            ReadLine := Copy(Buffer,0,BytesRead);
            CmdOutput.add(Trim(String(ReadLine)));
          end;
        until not WasOK or (BytesRead = 0);
        WaitForSingleObject(PI.hProcess, INFINITE);
        Result := GetExitCodeProcess(pi.hProcess, ProcessExitCode);
        if Result then
          Result := ProcessExitCode = 0;
      finally
        CloseHandle(PI.hThread);
        CloseHandle(PI.hProcess);
      end;
  finally
    CloseHandle(StdOutPipeRead);
  end;
end;

function TZUGFeRDPdfHelper.QuoteIfContainsSpace(const _Value: String): String;
begin
  if Pos(' ',_Value)>0 then
    Result := '"'+_Value+'"'
  else
    Result := _Value;
end;

function TZUGFeRDPdfHelper.PdfTkServerGetZUGFeRDPdfAttachment(const _PdfFilename: String;
  out _Attachment: TStream; out _CmdOutput: String): Boolean;
var
  cmd: TStringList;
  tmpPath : String;
  tmpFilename : String;
  lList: TStringDynArray;
  i : Integer;
begin
  //https://www.pdflabs.com/docs/pdftk-man-page/#dest-op-unpack

  Result := false;
  _Attachment := nil;

  if not FileExists(_PdfFilename) then
    exit;
  if not FileExists(PdfTkServerPath+'bin\pdftk.exe') then
    exit;

  tmpPath := TPath.GetTempPath+TZUGFeRDHelper.CreateUuid;

  if not ForceDirectories(tmpPath) then
    exit;

  tmpFilename := TPath.GetTempFileName;
  tmpPath := IncludeTrailingPathDelimiter(tmpPath);

  cmd := TStringList.Create;
  try
    cmd.Add('pushd '+QuoteIfContainsSpace(ExtractFilePath(tmpFilename)));
    cmd.Add(QuoteIfContainsSpace(PdfTkServerPath+'bin\pdftk.exe')+' '+
             QuoteIfContainsSpace(_PdfFilename)+' unpack_files output '+
             tmpPath);
    cmd.SaveToFile(tmpFilename+'.bat',TEncoding.ANSI);

    Result := ExecAndWait(tmpFilename+'.bat','');

    _CmdOutput := CmdOutput.Text;

    TFile.Delete(tmpFilename+'.bat');
    TFile.Delete(tmpFilename);

    if Result then
    begin
      lList := TDirectory.GetFiles(tmpPath,'*.xml', TSearchOption.soTopDirectoryOnly);
      for i := 0 to Length(LList) - 1 do
      if (Pos('zugferd',LowerCase(ExtractFilename(LList[i]))) = 1) or
         (Pos('factur-x',LowerCase(ExtractFilename(LList[i]))) = 1) then
      begin
        _Attachment := TMemoryStream.Create;
        TMemoryStream(_Attachment).LoadFromFile(LList[i]);
        break;
      end;

      Result := _Attachment <> nil;
    end;

    TDirectory.Delete(tmpPath,true);
  finally
    cmd.Free;
  end;
end;

function TZUGFeRDPdfHelper.SetJavaRuntimeEnvironmentPath(
  const _Path: String): IZUGFeRDPdfHelper;
begin
  JavaRuntimeEnvironmentPath := IncludeTrailingPathDelimiter(_Path);
  Result := self;
end;

function TZUGFeRDPdfHelper.SetMustangprojectLibPath(
  const _Path: String): IZUGFeRDPdfHelper;
begin
  MustangprojectPath := IncludeTrailingPathDelimiter(_Path);
  Result := self;
end;

function TZUGFeRDPdfHelper.SetPdfTkServerPath(
  const _Path: String): IZUGFeRDPdfHelper;
begin
  PdfTkServerPath := IncludeTrailingPathDelimiter(_Path);
  Result := self;
end;

function TZUGFeRDPdfHelper.ValidateFile(const _InvoiceXMLFilename: String;
  out _CmdOutput, _ValidationResultAsXML: String): Boolean;
var
  cmd: TStringList;
  tmpFilename : String;
begin
  Result := false;
  if not FileExists(_InvoiceXMLFilename) then
    exit;
  if not FileExists(JavaRuntimeEnvironmentPath+'bin\java.exe') then
    exit;
  if not FileExists(MustangprojectPath+'Mustang-CLI-2.14.2.jar') then
    exit;

  tmpFilename := TPath.GetTempFileName;

  cmd := TStringList.Create;
  try
    cmd.Add('pushd '+QuoteIfContainsSpace(ExtractFilePath(tmpFilename)));

    cmd.Add(QuoteIfContainsSpace(JavaRuntimeEnvironmentPath+'bin\java.exe')+' -Xmx1G '+
            '-Dfile.encoding=UTF-8 -jar '+QuoteIfContainsSpace(MustangprojectPath+'Mustang-CLI-2.14.2.jar')+
            ' --action validate' +
            ' --source '+ QuoteIfContainsSpace(_InvoiceXMLFilename)+
            ' --out '+tmpFilename+'.xml');

    cmd.SaveToFile(tmpFilename+'.bat',TEncoding.ANSI);

    Result := ExecAndWait(tmpFilename+'.bat','');

    if Result and FileExists(tmpFilename+'.html') then
    begin
      _ValidationResultAsXML := TFile.ReadAllText(tmpFilename+'.xml',TEncoding.UTF8);
    end;

    _CmdOutput := CmdOutput.Text;

    DeleteFile(tmpFilename+'.bat');
    if FileExists(tmpFilename+'.xml') then
      DeleteFile(tmpFilename+'.xml');
  finally
    cmd.Free;
  end;
end;

function TZUGFeRDPdfHelper.Visualize(const _InvoiceXMLData: String;
  out _CmdOutput, _VisualizationAsHTML: String): Boolean;
var
  hstrl,cmd: TStringList;
  tmpFilename : String;
begin
  Result := false;
  if _InvoiceXMLData = '' then
    exit;
  if not FileExists(JavaRuntimeEnvironmentPath+'bin\java.exe') then
    exit;
  if not FileExists(MustangprojectPath+'Mustang-CLI-2.14.2.jar') then
    exit;

  tmpFilename := TPath.GetTempFileName;

  hstrl := TStringList.Create;
  cmd := TStringList.Create;
  try
    hstrl.Text := _InvoiceXMLData;
    hstrl.SaveToFile(tmpFilename,TEncoding.UTF8);

    cmd.Add('pushd '+QuoteIfContainsSpace(ExtractFilePath(tmpFilename)));

    cmd.Add(QuoteIfContainsSpace(JavaRuntimeEnvironmentPath+'bin\java.exe')+' -Xmx1G '+
            '-Dfile.encoding=UTF-8 -jar '+QuoteIfContainsSpace(MustangprojectPath+'Mustang-CLI-2.14.2.jar')+
            ' --action visualize ' +
            '-source-xml '+ QuoteIfContainsSpace(tmpFilename));

    cmd.SaveToFile(tmpFilename+'.bat',TEncoding.ANSI);

    Result := ExecAndWait(tmpFilename+'.bat','');

    _CmdOutput := CmdOutput.Text;

    DeleteFile(tmpFilename+'.bat');
    DeleteFile(tmpFilename);

  finally
    hstrl.Free;
    cmd.Free;
  end;
end;

function TZUGFeRDPdfHelper.VisualizeFile(const _InvoiceXMLFilename: String;
  out _CmdOutput, _VisualizationAsHTML: String): Boolean;
var
  cmd: TStringList;
  tmpFilename : String;
begin
  Result := false;
  if not FileExists(_InvoiceXMLFilename) then
    exit;
  if not FileExists(JavaRuntimeEnvironmentPath+'bin\java.exe') then
    exit;
  if not FileExists(MustangprojectPath+'Mustang-CLI-2.14.2.jar') then
    exit;

  tmpFilename := TPath.GetTempFileName;

  cmd := TStringList.Create;
  try
    cmd.Add('pushd '+QuoteIfContainsSpace(ExtractFilePath(tmpFilename)));

    cmd.Add(QuoteIfContainsSpace(JavaRuntimeEnvironmentPath+'bin\java.exe')+' -Xmx1G '+
            '-Dfile.encoding=UTF-8 -jar '+QuoteIfContainsSpace(MustangprojectPath+'Mustang-CLI-2.14.2.jar')+
            ' --action visualize' +
            ' --source '+ QuoteIfContainsSpace(_InvoiceXMLFilename)+
            ' --out '+tmpFilename+'.html'+
            ' --language de');

    cmd.SaveToFile(tmpFilename+'.bat',TEncoding.ANSI);

    Result := ExecAndWait(tmpFilename+'.bat','');

    if Result and FileExists(tmpFilename+'.html') then
    begin
      _VisualizationAsHTML := TFile.ReadAllText(tmpFilename+'.html',TEncoding.UTF8);
    end;

    _CmdOutput := CmdOutput.Text;

    DeleteFile(tmpFilename+'.bat');
    if FileExists(tmpFilename+'.html') then
      DeleteFile(tmpFilename+'.html');
    if FileExists(ExtractFilePath(tmpFilename)+'xrechnung-viewer.css') then
      DeleteFile(ExtractFilePath(tmpFilename)+'xrechnung-viewer.css');
    if FileExists(ExtractFilePath(tmpFilename)+'xrechnung-viewer.js') then
      DeleteFile(ExtractFilePath(tmpFilename)+'xrechnung-viewer.js');
  finally
    cmd.Free;
  end;
end;

function TZUGFeRDPdfHelper.VisualizeFileAsPdf(const _InvoiceXMLFilename: String;
  out _CmdOutput: String; out _VisualizationAsPdf: TMemoryStream): Boolean;
var
  cmd: TStringList;
  tmpFilename : String;
begin
  Result := false;
  if not FileExists(_InvoiceXMLFilename) then
    exit;
  if not FileExists(JavaRuntimeEnvironmentPath+'bin\java.exe') then
    exit;
  if not FileExists(MustangprojectPath+'Mustang-CLI-2.14.2.jar') then
    exit;

  tmpFilename := TPath.GetTempFileName;

  cmd := TStringList.Create;
  try
    cmd.Add('pushd '+QuoteIfContainsSpace(ExtractFilePath(tmpFilename)));

    cmd.Add(QuoteIfContainsSpace(JavaRuntimeEnvironmentPath+'bin\java.exe')+' -Xmx1G '+
            '-Dfile.encoding=UTF-8 -jar '+QuoteIfContainsSpace(MustangprojectPath+'Mustang-CLI-2.14.2.jar')+
            ' --action pdf' +
            ' --source '+ QuoteIfContainsSpace(_InvoiceXMLFilename)+
            ' --out '+tmpFilename+'.pdf'+
            ' --language de');

    cmd.SaveToFile(tmpFilename+'.bat',TEncoding.ANSI);

    Result := ExecAndWait(tmpFilename+'.bat','');

    if Result and FileExists(tmpFilename+'.pdf') then
    begin
      _VisualizationAsPdf := TMemoryStream.Create;
      _VisualizationAsPdf.LoadFromFile(tmpFilename+'.pdf');
      _VisualizationAsPdf.Position := 0;
    end else
      _VisualizationAsPdf := nil;

    _CmdOutput := CmdOutput.Text;

    DeleteFile(tmpFilename+'.bat');
    if FileExists(tmpFilename+'.pdf') then
      DeleteFile(tmpFilename+'.pdf');
  finally
    cmd.Free;
  end;
end;

{ TZUGFeRDHelper }

class function TZUGFeRDHelper.CreateUuid: String;
begin
  Result := TGUID.NewGuid.ToString;
  Delete(Result,1,1);
  Delete(Result,Length(Result),1);
end;

class function TZUGFeRDHelper.GetDataAsBase64(_Stream: TStream): String;
var
  str : TMemoryStream;
  base64 : System.NetEncoding.TBase64Encoding;
  internalResult : AnsiString;
begin
  Result := '';
  _Stream.Seek(0,soFromBeginning);
  if _Stream.Size = 0 then
    exit;
  str := TMemoryStream.Create;
  base64 := System.NetEncoding.TBase64Encoding.Create(0); // CharsPerLine = 0 means no line breaks
  try
    base64.Encode(_Stream,str);
    str.Seek(0,soFromBeginning);
    if str.Size = 0 then
      exit;
    SetLength(internalResult,str.Size);
    str.Read(internalResult[1],str.Size);
    Result := String(internalResult);
  finally
    _Stream.Seek(0,soFromBeginning);
    base64.Free;
    str.Free;
  end;
end;

{ ZUGFeRDNullable<T> }

constructor ZUGFeRDNullable<T>.Create(Dummy: Boolean);
// constructor to create an uninitialzed Instance
begin
  FHasValue:= False;
end;

procedure ZUGFeRDNullable<T>.ClearValue;
begin
  FHasValue:= False;
end;

constructor ZUGFeRDNullable<T>.Create(AValue: T);
begin
  FValue := AValue;
  FHasValue:= true
end;

class operator ZUGFeRDNullable<T>.Initialize (out Dest: ZUGFeRDNullable<T>);
begin
  Dest.FHasValue:= false
end;

class operator ZUGFeRDNullable<T>.Equal(ALeft, ARight: ZUGFeRDNullable<T>): Boolean;
var
  Comparer: IEqualityComparer<T>;
begin
  if ALeft.HasValue and ARight.HasValue then
  begin
    Comparer := TEqualityComparer<T>.Default;
    Result := Comparer.Equals(ALeft.Value, ARight.Value);
  end else
    Result := ALeft.HasValue = ARight.HasValue;
end;

class operator ZUGFeRDNullable<T>.Explicit(Value: ZUGFeRDNullable<T>): T;
begin
  Result := Value.Value;
end;

function ZUGFeRDNullable<T>.GetValue: T;
begin
  if not HasValue then
    raise Exception.Create('Invalid operation, ZUGFeRDNullable type has no value');
  Result := FValue;
end;

function ZUGFeRDNullable<T>.GetValueOrDefault: T;
begin
  if HasValue then
    Result := FValue
  else
    Result := Default(T);
end;

function ZUGFeRDNullable<T>.GetValueOrDefault(Default: T): T;
begin
  if not HasValue then
    Result := Default
  else
    Result := FValue;
end;

class operator ZUGFeRDNullable<T>.Implicit(Value: ZUGFeRDNullable<T>): T;
begin
  Result := Value.Value;
end;

class operator ZUGFeRDNullable<T>.Implicit(Value: T): ZUGFeRDNullable<T>;
begin
  Result := ZUGFeRDNullable<T>.Create(Value);
end;

class operator ZUGFeRDNullable<T>.Implicit(Param: IZUGFeRDNullableParam<T>): ZUGFeRDNullable<T>;
begin
  if Param=Nil then
    Result:= ZUGFeRDNullable<T>.Create(false)
  else
    Result:= ZUGFeRDNullable<T>.Create(Param.Value);
end;

class operator ZUGFeRDNullable<T>.Implicit(Param: ZUGFeRDNullable<T>): IZUGFeRDNullableParam<T>;
begin
  if not Param.HasValue then
    Result := nil
  else
    Result := TZUGFeRDNullableParam<T>.Create(Param.Value);
end;

class operator ZUGFeRDNullable<T>.NotEqual(ALeft, ARight: ZUGFeRDNullable<T>): Boolean;
var
  Comparer: IEqualityComparer<T>;
begin
  if ALeft.HasValue and ARight.HasValue then
  begin
    Comparer := TEqualityComparer<T>.Default;
    Result := not Comparer.Equals(ALeft.Value, ARight.Value);
  end else
    Result := ALeft.HasValue <> ARight.HasValue;
end;

{ ZUGFeRDNullableParam<T> }

constructor TZUGFeRDNullableParam<T>.Create(AValue: T);
begin
  Value:= AValue;
end;

function TZUGFeRDNullableParam<T>.GetValue: T;
begin
  Result:= FValue
end;

procedure TZUGFeRDNullableParam<T>.SetValue(const AValue: T);
begin
  FValue:= AValue
end;

end.


