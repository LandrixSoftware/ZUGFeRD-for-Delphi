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
/// Lädt sämtliche Beispielrechnungen aus documentation\ und prüft, dass die
/// Bibliothek sie einlesen, das Profil erkennen und wieder herausschreiben kann.
///
/// Das ist ein Regressionsnetz, kein fachlicher Test: Konkrete Feldwerte prüfen
/// die übrigen Testunits. Geprüft wird hier, dass das Lesen überhaupt Daten
/// liefert und dass diese Daten einen Schreib-/Lesezyklus überstehen. Ein reiner
/// Ausfalltest wäre wirkungslos: ein Reader, der sämtliche Positionen verliert,
/// liest die Guideline weiterhin und der Writer schreibt das leere Dokument
/// anstandslos heraus.
///
/// Jeder registrierte Dokumentationsstand benötigt eingecheckte Beispieldateien.
/// Fehlende Verzeichnisse oder XML-Beispiele lassen den Test fehlschlagen, damit
/// ein wirkungsloser Sweep nicht als erfolgreich ausgewiesen wird.
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
    /// Das aufrufende Sweep-Verfahren prüft Verzeichnis und Ergebnismenge.
    /// </summary>
    function CollectInvoiceFiles(const aDocSubDir: string): TArray<string>;
    /// <summary>
    /// Lädt jede gefundene Datei und sammelt alle Beanstandungen ein, damit ein
    /// Lauf sämtliche Probleme meldet und nicht nur das erste.
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
    procedure TestSweepZUGFeRD252FacturX1092;
  end;

implementation

uses
  System.SysUtils, System.StrUtils, System.Classes, System.IOUtils, System.Types,
  intf.ZUGFeRDInvoiceDescriptor,
  intf.ZUGFeRDCurrencyCodes,
  intf.ZUGFeRDHelper,
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
      // Schematron-Testfälle sind bewusst unvollständige Fragmente
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
  root, fn, rel: string;
  desc, reloaded: TZUGFeRDInvoiceDescriptor;
  ms: TMemoryStream;
  errors: TStringList;
  checked: Integer;
begin
  root := DocumentationPath(aDocSubDir);
  Assert.IsTrue(TDirectory.Exists(root),
    Format('Dokumentationsverzeichnis fehlt: documentation\%s', [aDocSubDir]));

  files := CollectInvoiceFiles(aDocSubDir);
  Assert.IsTrue(Length(files) > 0,
    Format('Keine Beispieldateien unter documentation\%s gefunden', [aDocSubDir]));

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

        // BT-1 ist in jedem Profil eine Pflichtangabe. Eine leere Rechnungsnummer
        // ist der deutlichste Hinweis darauf, dass gar nichts gelesen wurde.
        if desc.InvoiceNo.Trim = '' then
        begin
          errors.Add(Format('%s: Keine Rechnungsnummer gelesen', [rel]));
          Continue;
        end;

        // MINIMUM und BASIC-WL führen keine Positionen, alle übrigen Profile schon.
        if not (desc.Profile in [TZUGFeRDProfile.Minimum, TZUGFeRDProfile.BasicWL]) and
           (desc.TradeLineItems.Count = 0) then
        begin
          errors.Add(Format('%s: Keine Rechnungspositionen gelesen (Profil %s)',
            [rel, TZUGFeRDProfileExtensions.EnumToString(desc.Profile, TZUGFeRDVersion.Version23)]));
          Continue;
        end;

        // Zurückschreiben im erkannten Profil muss ohne Fehler durchlaufen und ein
        // wieder lesbares Dokument mit denselben Kerndaten ergeben.
        ms := TMemoryStream.Create;
        try
          try
            desc.Save(ms, TZUGFeRDVersion.Version23, desc.Profile, TZUGFeRDFormats.CII);
          except
            on E: Exception do
            begin
              errors.Add(Format('%s: Schreiben fehlgeschlagen - %s: %s', [rel, E.ClassName, E.Message]));
              Continue;
            end;
          end;

          if ms.Size = 0 then
          begin
            errors.Add(Format('%s: Schreiben liefert ein leeres Dokument', [rel]));
            Continue;
          end;

          ms.Position := 0;
          try
            reloaded := TZUGFeRDInvoiceDescriptor.Load(ms);
          except
            on E: Exception do
            begin
              errors.Add(Format('%s: Erneutes Lesen fehlgeschlagen - %s: %s', [rel, E.ClassName, E.Message]));
              Continue;
            end;
          end;

          try
            if reloaded.InvoiceNo <> desc.InvoiceNo then
              errors.Add(Format('%s: Rechnungsnummer nach Roundtrip [%s] statt [%s]',
                [rel, reloaded.InvoiceNo, desc.InvoiceNo]));

            if reloaded.TradeLineItems.Count <> desc.TradeLineItems.Count then
              errors.Add(Format('%s: %d statt %d Rechnungspositionen nach Roundtrip',
                [rel, reloaded.TradeLineItems.Count, desc.TradeLineItems.Count]));

            if reloaded.Currency <> desc.Currency then
              errors.Add(Format('%s: Währung nach Roundtrip [%s] statt [%s]',
                [rel,
                 TEnumExtensions<TZUGFeRDCurrencyCodes>.EnumToString(reloaded.Currency),
                 TEnumExtensions<TZUGFeRDCurrencyCodes>.EnumToString(desc.Currency)]));

            if desc.GrandTotalAmount.HasValue then
            begin
              if not reloaded.GrandTotalAmount.HasValue then
                errors.Add(Format('%s: BT-112 nach Roundtrip nicht mehr vorhanden', [rel]))
              else if reloaded.GrandTotalAmount.Value <> desc.GrandTotalAmount.Value then
                errors.Add(Format('%s: BT-112 nach Roundtrip [%s] statt [%s]',
                  [rel, CurrToStr(reloaded.GrandTotalAmount.Value), CurrToStr(desc.GrandTotalAmount.Value)]));
            end;
          finally
            reloaded.Free;
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

procedure TZUGFeRDDocumentationSweepTests.TestSweepZUGFeRD252FacturX1092;
begin
  SweepDocumentationVersion('zugferd25-facturx1009-02\zugferd252de');
end;

end.
