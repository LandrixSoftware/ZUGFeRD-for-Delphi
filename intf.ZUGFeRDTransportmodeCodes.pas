﻿{* Licensed to the Apache Software Foundation (ASF) under one
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

unit intf.ZUGFeRDTransportmodeCodes;

interface

uses
  System.SysUtils,System.TypInfo,
  Intf.ZUGFeRDHelper
  ;

type
  /// <summary>
  ///     Eine Konsignation auf Kopfebene, die mit dieser Handelslieferung zusammenhängt.
  ///     A logistics transport movement specified for this supply chain consignment.
  ///     BT-X-152

  TZUGFeRDTransportmodeCodes = (
  dummyzero, // so Unknown becomes 1 etc.
  /// <summary>
  /// Transport mode not specified
  /// </summary>
  Unknown,

  /// <summary>
  /// Maritime transport
  /// </summary>
  Maritime,

  /// <summary>
  /// Rail Transport
  /// </summary>
  Rail,

  /// <summary>
  /// Road Transport
  /// </summary>
  Road,

  /// <summary>
  /// Air Transport
  /// </summary>
  Air,

  /// <summary>
  /// Via Mail
  /// </summary>
  Mail,

  /// <summary>
  /// Multimodal transport
  /// </summary>
  MultiMode,

  /// <summary>
  /// Fixed transport installation
  /// </summary>
  FixedTransport,

  /// <summary>
  /// Inland water transport
  /// </summary>
  InlandWater,

  /// <summary>
  /// Transport mode not applicable
  /// </summary>
  NotApplicable
);

  TZUGFeRDTransportmodeCodesExtensions = class
  public
    class function FromString(const s: string): TZUGFeRDTransportmodeCodes;
    class function EnumToString(c: ZUGFeRDNullable<TZUGFeRDTransportmodeCodes>): string;
  end;

implementation

{ TZUGFeRDTransportmodeCodesExtensions }

class function TZUGFeRDTransportmodeCodesExtensions.EnumToString(c: ZUGFeRDNullable<TZUGFeRDTransportmodeCodes>): string;
begin
  if not c.HasValue then
    Result:= 'Unknown'
  else
    Result := GetEnumName(TypeInfo(TZUGFeRDTransportmodeCodes), Integer(c.Value));
end;

class function TZUGFeRDTransportmodeCodesExtensions.FromString(const s: string): TZUGFeRDTransportmodeCodes;
var
  enumValue : Integer;
begin
  enumValue := GetEnumValue(TypeInfo(TZUGFeRDTransportmodeCodes), s);
  if enumValue >= 0 then
    Result := TZUGFeRDTransportmodeCodes(enumValue)
  else
    Result := TZUGFeRDTransportmodeCodes.Unknown;
end;

end.
