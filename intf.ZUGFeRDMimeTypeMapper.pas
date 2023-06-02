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

unit intf.ZUGFeRDMimeTypeMapper;

interface

uses
  System.SysUtils, System.Generics.Collections;

type
  /// <summary>
  /// Class for mapping between file extensions and mime types
  ///
  /// Only those mime types are present that are supported by the additional reference document according to
  /// XRechnung specification, see e.g. https://projekte.kosit.org/xrechnung/xrechnung/-/issues/59
  /// </summary>
  TZUGFeRDMimeTypeMapper = class
  public
    class function GetMimeType(const filename: string): string;
  end;

implementation

{ TZUGFeRDMimeTypeMapper }

class function TZUGFeRDMimeTypeMapper.GetMimeType(
  const filename: string): string;
var
  extension: string;
begin
  Result := 'application/octet-stream';

  if filename.IsEmpty then
    exit;

  extension := ExtractFileExt(filename);
  if extension.IsEmpty then
    exit;

  if SameText(extension,'.pdf') then Result := 'application/pdf' else
  if SameText(extension,'.png') then Result := 'image/png' else
  if SameText(extension,'.jpg') then Result := 'image/jpeg' else
  if SameText(extension,'.jpeg') then Result := 'image/jpeg' else
  if SameText(extension,'.csv') then Result := 'text/csv' else
  if SameText(extension,'.xlsx') then Result := 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet' else
  if SameText(extension,'.ods') then Result := 'application/vnd.oasis.opendocument.spreadsheet' else
  if SameText(extension,'.xml') then Result := 'application/xml';
end;

end.
