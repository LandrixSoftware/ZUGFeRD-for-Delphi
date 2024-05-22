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
  ;

type
  TZUGFeRDNullable<T> = class
  private
    FHasValue: Boolean;
    FValue: T;
  public
    constructor Create;
    constructor CreateWithValue(const AValue: T);

    function HasValue: Boolean;
    function GetValue: T;
    procedure SetValue(const AValue: T);
    procedure SetValueClass(AValue : TZUGFeRDNullable<T>);
    procedure ClearValue;

    property Value: T read GetValue write SetValue;
  end;

  TZUGFeRDNullableCurrency = class
  private
    FHasValue: Boolean;
    FValue: Currency;
  public
    constructor Create;
    constructor CreateWithValue(const AValue: Currency);

    function HasValue: Boolean;
    function GetValue: Currency;
    procedure SetValue(const AValue: Currency);
    procedure SetValueClass(AValue : TZUGFeRDNullableCurrency);
    procedure ClearValue;

    property Value: Currency read GetValue write SetValue;
  end;

  TZUGFeRDHelper = class(TObject)
  public
    class function CreateUuid : String;
    class function GetDataAsBase64(_Stream : TStream) : String;
  end;

  IZUGFeRDPdfHelper = interface
    ['{EB5E0786-D33B-4811-9749-E58DE90102F7}']
    function SetPdfTkServerPath(const _Path : String) : IZUGFeRDPdfHelper;
    function GetZUGFeRDPdfAttachment(const _PdfFilename : String; out _Attachment : TStream; out _CmdOutput : String) : Boolean;
  end;

  function GetZUGFeRDPdfHelper : IZUGFeRDPdfHelper;

implementation

type
  TZUGFeRDPdfHelper = class(TInterfacedObject,IZUGFeRDPdfHelper)
  private
    PdfTkServerPath : String;
    CmdOutput : TStringList;
    function ExecAndWait(_Filename, _Params: string): Boolean;
    function QuoteIfContainsSpace(const _Value : String) : String;
  public
    constructor Create;
    destructor Destroy; override;
    function SetPdfTkServerPath(const _Path : String) : IZUGFeRDPdfHelper;
    function GetZUGFeRDPdfAttachment(const _PdfFilename : String; out _Attachment : TStream; out _CmdOutput : String) : Boolean;
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

function TZUGFeRDPdfHelper.GetZUGFeRDPdfAttachment(const _PdfFilename: String;
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

function TZUGFeRDPdfHelper.SetPdfTkServerPath(
  const _Path: String): IZUGFeRDPdfHelper;
begin
  PdfTkServerPath := IncludeTrailingPathDelimiter(_Path);
  Result := self;
end;

{ TZUGFeRDNullable }

procedure TZUGFeRDNullable<T>.ClearValue;
begin
  FHasValue := false;
end;

constructor TZUGFeRDNullable<T>.Create;
begin
  FHasValue := False;
end;

constructor TZUGFeRDNullable<T>.CreateWithValue(const AValue: T);
begin
  FHasValue := True;
  FValue := AValue;
end;

function TZUGFeRDNullable<T>.HasValue: Boolean;
begin
  Result := FHasValue;
end;

function TZUGFeRDNullable<T>.GetValue: T;
begin
  if not FHasValue then
    raise Exception.Create('Nullable object does not have a value.');

  Result := FValue;
end;

procedure TZUGFeRDNullable<T>.SetValue(const AValue: T);
begin
  FHasValue := True;
  FValue := AValue;
end;

procedure TZUGFeRDNullable<T>.SetValueClass(AValue: TZUGFeRDNullable<T>);
begin
  if AValue = nil then
    ClearValue
  else
    Value := AValue.Value;
end;

{ TZUGFeRDNullableCurrency }

procedure TZUGFeRDNullableCurrency.ClearValue;
begin
  FHasValue := false;
end;

constructor TZUGFeRDNullableCurrency.Create;
begin
  FHasValue := false;
end;

constructor TZUGFeRDNullableCurrency.CreateWithValue(
  const AValue: Currency);
begin
  FHasValue := True;
  FValue := AValue;
end;

function TZUGFeRDNullableCurrency.GetValue: Currency;
begin
  if not FHasValue then
    raise Exception.Create('Nullable object does not have a value.');

  Result := FValue;
end;

function TZUGFeRDNullableCurrency.HasValue: Boolean;
begin
  Result := FHasValue;
end;

procedure TZUGFeRDNullableCurrency.SetValue(const AValue: Currency);
begin
  FHasValue := True;
  FValue := AValue;
end;

procedure TZUGFeRDNullableCurrency.SetValueClass(
  AValue: TZUGFeRDNullableCurrency);
begin
  if AValue = nil then
    ClearValue
  else
    Value := AValue.Value;
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

end.


