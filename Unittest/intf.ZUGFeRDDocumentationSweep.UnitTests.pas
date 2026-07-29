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

unit intf.ZUGFeRDDocumentationSweep.UnitTests;

/// <summary>
/// Laedt saemtliche Beispielrechnungen aus documentation\ und prueft, dass die
/// Bibliothek sie einlesen, das Profil erkennen und wieder herausschreiben kann.
///
/// Das ist ein Regressionsnetz, kein fachlicher Test: geprueft wird nur, dass
/// nichts umfaellt - konkrete Feldwerte pruefen die uebrigen Testunits.
///
/// Die Beispieldateien sind nicht vollstaendig eingecheckt (die ZIPs der
/// Dokumentationspakete werden lokal ausgepackt). Fehlt ein Verzeichnis, wird
/// der jeweilige Test uebersprungen statt fehlzuschlagen.
/// </summary>

interface

uses
  DUnitX.TestFramework,
  intf.ZUGFeRDTestBase;

type
  [TestFixture]
  TZUGFeRDDocumentationSweepTests = class(TZUGFeRDTestBase)
  private
    /// <summary>
    /// Sammelt alle Rechnungs-XML unterhalb von documentation\aDocSubDir.
    /// Ergebnis ist leer, wenn das Verzeichnis fehlt.
    /// </summary>
    function CollectInvoiceFiles(const aDocSubDir: string): TArray<string>;
    /// <summary>
    /// Laedt jede gefundene Datei und sammelt alle Beanstandungen ein, damit ein
    /// Lauf saemtliche Probleme meldet und nicht nur das erste.
    /// </summary>
    procedure SweepDocumentationVersion(const aDocSubDir: string);
  public
    [Test]
    procedure TestSweepZUGFeRD10;
    [Test]
    procedure TestSweepZUGFeRD20;
    [Test]
    procedure TestSweepZUGFeRD21;
    [Test]
    procedure TestSweepZUGFeRD211;
    [Test]
    procedure TestSweepZUGFeRD23FacturX1007;
    [Test]
    procedure TestSweepZUGFeRD24FacturX1008;
    [Test]
    procedure TestSweepZUGFeRD25FacturX1009;
  end;

implementation

uses
  System.SysUtils, System.StrUtils, System.Classes, System.IOUtils, System.Types,
  intf.ZUGFeRDInvoiceDescriptor,
  intf.ZUGFeRDProfile,
  intf.ZUGFeRDVersion,
  intf.ZUGFeRDFormats;

{ TZUGFeRDDocumentationSweepTests }

function TZUGFeRDDocumentationSweepTests.CollectInvoiceFiles(
  const aDocSubDir: string): TArray<string>;
var
  root, fn: string;
  found: TStringDynArray;
  list: TStringList;
begin
  Result := [];
  root := DocumentationPath(aDocSubDir);
  if not TDirectory.Exists(root) then
    Exit;

  found := TDirectory.GetFiles(root, '*.xml', TSearchOption.soAllDirectories);

  list := TStringList.Create;
  try
    for fn in found do
    begin
      // Nur die Beispielrechnungen, nicht die Schemata daneben
      if not ContainsText(fn, 'eispiel') then
        Continue;
      // Validierungsberichte der Beispiele sind keine Rechnungen
      if ContainsText(fn, '_fx_validation_report') then
        Continue;
      // Schematron-Testfaelle sind bewusst unvollstaendige Fragmente
      if ContainsText(fn, PathDelim + 'Schematron' + PathDelim) then
        Continue;
      list.Add(fn);
    end;
    list.Sort;
    Result := list.ToStringArray;
  finally
    list.Free;
  end;
end;

procedure TZUGFeRDDocumentationSweepTests.SweepDocumentationVersion(
  const aDocSubDir: string);
var
  files: TArray<string>;
  fn, rel: string;
  desc: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  errors: TStringList;
  checked: Integer;
begin
  files := CollectInvoiceFiles(aDocSubDir);
  if Length(files) = 0 then
  begin
    Log(Format('Uebersprungen: keine Beispieldateien unter documentation\%s', [aDocSubDir]));
    Exit;
  end;

  checked := 0;
  errors := TStringList.Create;
  try
    for fn in files do
    begin
      rel := ExtractFileName(fn);

      try
        desc := TZUGFeRDInvoiceDescriptor.Load(fn);
      except
        on E: Exception do
        begin
          errors.Add(Format('%s: Lesen fehlgeschlagen - %s: %s', [rel, E.ClassName, E.Message]));
          Continue;
        end;
      end;

      try
        Inc(checked);

        if desc.Profile = TZUGFeRDProfile.Unknown then
        begin
          errors.Add(Format('%s: Profil nicht erkannt', [rel]));
          Continue;
        end;

        // Zurueckschreiben im erkannten Profil muss ohne Fehler durchlaufen
        ms := TMemoryStream.Create;
        try
          try
            desc.Save(ms, TZUGFeRDVersion.Version23, desc.Profile, TZUGFeRDFormats.CII);
          except
            on E: Exception do
              errors.Add(Format('%s: Schreiben fehlgeschlagen - %s: %s', [rel, E.ClassName, E.Message]));
          end;
        finally
          ms.Free;
        end;
      finally
        desc.Free;
      end;
    end;

    Log(Format('%d Beispieldateien aus documentation\%s geprueft', [checked, aDocSubDir]));
    Assert.AreEqual(0, errors.Count,
      Format('%d von %d Beispieldateien beanstandet:'#13#10'%s',
        [errors.Count, Length(files), errors.Text]));
  finally
    errors.Free;
  end;
end;

procedure TZUGFeRDDocumentationSweepTests.TestSweepZUGFeRD10;
begin
  SweepDocumentationVersion('zugferd10');
end;

procedure TZUGFeRDDocumentationSweepTests.TestSweepZUGFeRD20;
begin
  SweepDocumentationVersion('zugferd20');
end;

procedure TZUGFeRDDocumentationSweepTests.TestSweepZUGFeRD21;
begin
  SweepDocumentationVersion('zugferd21de');
end;

procedure TZUGFeRDDocumentationSweepTests.TestSweepZUGFeRD211;
begin
  SweepDocumentationVersion('zuferd211');
end;

procedure TZUGFeRDDocumentationSweepTests.TestSweepZUGFeRD23FacturX1007;
begin
  SweepDocumentationVersion('zugferd23-facturx1007');
end;

procedure TZUGFeRDDocumentationSweepTests.TestSweepZUGFeRD24FacturX1008;
begin
  SweepDocumentationVersion('zugferd24-facturx1008');
end;

procedure TZUGFeRDDocumentationSweepTests.TestSweepZUGFeRD25FacturX1009;
begin
  SweepDocumentationVersion('zugferd25-facturx1009');
end;

end.
