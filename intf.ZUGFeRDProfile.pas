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

unit intf.ZUGFeRDProfile;

interface

uses System.SysUtils,intf.ZUGFeRDVersion,intf.ZUGFeRDExceptions;

type
  /// <summary>
  /// ZUGFeRD allows reading and writing invoices in various profiles, each containing different density of information.
  /// </summary>
  TZUGFeRDProfile = (
    /// <summary>
    /// Fallback value
    /// </summary>
    Unknown = 0,

    /// <summary>
    /// Contains core line
    /// information required or useful for buyers for their process automation.
    /// </summary>
    Basic = 1,

    /// <summary>
    /// The Comfort profile corresponds to the European standard EN 16931.
    ///
    /// Invoices in this profile contain all necessary information and thus are valid electronic invoices.
    /// </summary>
    Comfort = 2,

    /// <summary>
    /// Based on the Comfort/ EN 16931 profile. Contains additional information.
    /// </summary>
    Extended = 4,

    /// <summary>
    /// corresponding to the minimum invoice information
    /// Invoices in this profile are no valid electronic invoices.
    /// They contain document level invoice information that are mostly required or useful for buyers for their process automation.
    /// </summary>
    Minimum = 8,

    /// <summary>
    /// Invoices in this profile are no valid electronic invoices.
    /// They contain document level invoice information that are mostly required or useful for buyers for their process automation.
    /// </summary>
    BasicWL = 16,

    /// <summary>
    /// Invoice format based on EU Directive 2014/55/EU, adopted to Germany in E-Invoice Law of April 4, 2017 (BGBl. I p. 770)
    /// </summary>
    XRechnung = 32,

    /// <summary>
    /// Invoice format based on EU Directive 2014/55/EU, adopted to Germany in E-Invoice Law of April 4, 2017 (BGBl. I p. 770).
    /// Important note: using this profile will generate a version 1 XRechnung (as valid until 31/12/2020)
    /// </summary>
    XRechnung1 = 64,

    /// <summary>
    /// The e-reporting (https://www.impots.gouv.fr/e-reporting-la-transmission-de-donnees-de-transaction-ladministration)
    /// concern companies subject to VAT in France and trading with private individuals and, more generally, non-taxable persons
    /// (business to consumer or BtoC), with companies not established on French territory (i.e. taxable persons who do not have
    /// an establishment, domicile or habitual residence in France).
    /// </summary>
    EReporting = 128
  );

  TZUGFeRDProfiles = set of TZUGFeRDProfile;

  TZUGFeRDProfileExtensions = class
  public
    class function FromString(const s: string): TZUGFeRDProfile; static;
    class function EnumToString(profile: TZUGFeRDProfile; version: TZUGFeRDVersion): string; static;
  end;

const
  TZUGFERDPROFILES_DEFAULT = [TZUGFeRDProfile.Unknown];

implementation

{ TZUGFeRDProfileExtensions }

class function TZUGFeRDProfileExtensions.FromString(const s: string): TZUGFeRDProfile;
begin
  // v1

  if SameText(s,'urn:ferd:invoice:1.0:basic') or
     SameText(s,'urn:ferd:invoice:rc:basic') or
     SameText(s,'urn:ferd:CrossIndustryDocument:invoice:1p0:basic') then
      Result := Basic
  else
  if SameText(s,'urn:ferd:invoice:1.0:comfort') or
     SameText(s,'urn:ferd:invoice:rc:comfort') or
     SameText(s,'urn:ferd:CrossIndustryDocument:invoice:1p0:comfort') or
     SameText(s,'urn:ferd:CrossIndustryDocument:invoice:1p0:E') then
      Result := Comfort
  else
  if SameText(s,'urn:ferd:invoice:1.0:extended') or
     SameText(s,'urn:ferd:invoice:rc:extended') or
     SameText(s,'urn:ferd:CrossIndustryDocument:invoice:1p0:extended') then
      Result := Extended
  else

  // v2

  if SameText(s,'urn:zugferd.de:2p0:minimum') then
      Result := Minimum
  else
  if SameText(s,'urn:cen.eu:en16931:2017#compliant#urn:zugferd.de:2p0:basic') then
      Result := Basic
  else
  if SameText(s,'urn: cen.eu:en16931: 2017') then // Spaces inserted to prevent clash with v2.1
      Result := Comfort
  else
  if SameText(s,'urn:cen.eu:en16931:2017#conformant#urn:zugferd.de:2p0:extended') then
      Result := Extended
  else

  // v2.1

  if SameText(s,'urn:factur-x.eu:1p0:minimum') then
      Result := Minimum
  else
  if SameText(s,'urn:cen.eu:en16931:2017#compliant#urn:factur-x.eu:1p0:basic') then
      Result := Basic
  else
  if SameText(s,'urn:factur-x.eu:1p0:basicwl') then
      Result := BasicWL
  else
  if SameText(s,'urn:cen.eu:en16931:2017') then
      Result := Comfort
  else
  if SameText(s,'urn:cen.eu:en16931:2017#conformant#urn:factur-x.eu:1p0:extended') then
      Result := Extended
  else
  if SameText(s,'urn:cen.eu:en16931:2017#compliant#urn:xoev-de:kosit:standard:xrechnung_1.2') then
      Result := XRechnung1
  else
  if SameText(s,'urn:cen.eu:en16931:2017#compliant#urn:xoev-de:kosit:standard:xrechnung_2.0') or
     SameText(s,'urn:cen.eu:en16931:2017#compliant#urn:xoev-de:kosit:standard:xrechnung_2.1') or
     SameText(s,'urn:cen.eu:en16931:2017#compliant#urn:xoev-de:kosit:standard:xrechnung_2.2') or
     SameText(s,'urn:cen.eu:en16931:2017#compliant#urn:xoev-de:kosit:standard:xrechnung_2.3') or
     SameText(s,'urn:cen.eu:en16931:2017#compliant#urn:xeinkauf.de:kosit:xrechnung_3.0') then
      Result := XRechnung
  else
  if SameText(s,'urn.cpro.gouv.fr:1p0:ereporting') then
      Result := EReporting
  else
    Result := Unknown;
end;

class function TZUGFeRDProfileExtensions.EnumToString(profile: TZUGFeRDProfile;
  version: TZUGFeRDVersion): string;
begin
  case version of
    TZUGFeRDVersion.Version1:
      case profile of
        TZUGFeRDProfile.Basic:
          Result := 'urn:ferd:CrossIndustryDocument:invoice:1p0:basic';
        TZUGFeRDProfile.Comfort:
          Result := 'urn:ferd:CrossIndustryDocument:invoice:1p0:comfort';
        TZUGFeRDProfile.Extended:
          Result := 'urn:ferd:CrossIndustryDocument:invoice:1p0:extended';
      else
        raise Exception.Create('Unsupported profile for ZUGFeRD version 1');
      end;
    TZUGFeRDVersion.Version20:
      case profile of
        TZUGFeRDProfile.Minimum:
          Result := 'urn:zugferd.de:2p0:minimum';
        TZUGFeRDProfile.Basic:
          Result := 'urn:cen.eu:en16931:2017#compliant#urn:zugferd.de:2p0:basic';
        TZUGFeRDProfile.BasicWL:
          Result := 'urn:zugferd.de:2p0:basicwl';
        TZUGFeRDProfile.Comfort:
          Result := 'urn:cen.eu:en16931:2017';
        TZUGFeRDProfile.Extended:
          Result := 'urn:cen.eu:en16931:2017#conformant#urn:zugferd.de:2p0:extended';
      else
        raise Exception.Create('Unsupported profile for ZUGFeRD version 20');
      end;
    TZUGFeRDVersion.Version22:
      case profile of
        TZUGFeRDProfile.Minimum:
          Result := 'urn:factur-x.eu:1p0:minimum';
        TZUGFeRDProfile.Basic:
          Result := 'urn:cen.eu:en16931:2017#compliant#urn:factur-x.eu:1p0:basic';
        TZUGFeRDProfile.BasicWL:
          Result := 'urn:factur-x.eu:1p0:basicwl';
        TZUGFeRDProfile.Comfort:
          Result := 'urn:cen.eu:en16931:2017';
        TZUGFeRDProfile.Extended:
          Result := 'urn:cen.eu:en16931:2017#conformant#urn:factur-x.eu:1p0:extended';
        TZUGFeRDProfile.XRechnung1:
          Result := 'urn:cen.eu:en16931:2017#compliant#urn:xoev-de:kosit:standard:xrechnung_1.2';
        TZUGFeRDProfile.XRechnung:
          if Now >= EncodeDate(2024, 2, 1) then
            Result := 'urn:cen.eu:en16931:2017#compliant#urn:xeinkauf.de:kosit:xrechnung_3.0'
          else
            Result := 'urn:cen.eu:en16931:2017#compliant#urn:xoev-de:kosit:standard:xrechnung_2.3';
        TZUGFeRDProfile.EReporting :
          Result := 'urn.cpro.gouv.fr:1p0:ereporting';
      else
        raise Exception.Create('Unsupported profile for ZUGFeRD version 21');
      end;
  else
    raise TZUGFeRDUnsupportedException.Create('New ZUGFeRDVersion defined but not implemented!');
  end;
end;

end.
