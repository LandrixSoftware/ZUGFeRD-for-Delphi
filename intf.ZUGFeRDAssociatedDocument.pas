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

unit intf.ZUGFeRDAssociatedDocument;

interface

uses
  System.Generics.Collections, intf.ZUGFeRDNote;

type
  /// <summary>
  /// Representation for general information on item level
  /// </summary>
  TZUGFeRDAssociatedDocument = class
  private
    FNotes: TObjectList<TZUGFeRDNote>;
    FLineID: string;
    FLineStatusCode: string;
    FLineStatusReasonCode: string;
  public
    constructor Create(lineID: string);
    destructor Destroy; override;

    /// <summary>
    ///  Detailed information in free text form
    /// </summary>
    property Notes: TObjectList<TZUGFeRDNote> read FNotes;

    /// <summary>
    /// identifier of the invoice line item
    /// </summary>
    property LineID: string read FLineID write FLineID;

    //Typ der Rechnungsposition (Code)
    //qdt:LineStatusCodeType
    //Zeigt an, ob die Rechnungsposition Preise beinhaltet, die bei der
    //Berechnung des Rechnungsbetrags beruecksichtigt werden muessen, oder
    //aber ob nur Informationen enthalten sind.
    //Folgender Code sollte genutzt werden : TYPE_LINE
    property LineStatusCode: string read FLineStatusCode write FLineStatusCode;

    //Untertyp der Rechnungsposition
    //udt:CodeType
    //Ergaenzt den Typ, um zu praezisieren ob es sich bei der Rechnungsposition um
    //Folgendes handelt:
    //- Detail (normale Position)
    //- Zwischensumme
    //- Ausschliesslich Information
    //Anwendung
    //Wenn das Feld LineStatusCode benutzt wird, muss das Feld LineStatusReasonCode
    //folgende Codes nutzen:
    //- Detail
    //- Gruppierung
    //- Information
    property LineStatusReasonCode: string read FLineStatusReasonCode write FLineStatusReasonCode;
  end;

implementation

constructor TZUGFeRDAssociatedDocument.Create(lineID: string);
begin
  FNotes := TObjectList<TZUGFeRDNote>.Create;
  FLineID := lineID;
end;

destructor TZUGFeRDAssociatedDocument.Destroy;
begin
  if Assigned(FNotes) then begin FNotes.Free; FNotes := nil; end;
  inherited Destroy;
end;


end.
