[![Donate](https://img.shields.io/badge/Donate-PayPal-green.svg)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=5V8N3XFTU495G)

# ZUGFeRD-for-Delphi

Read and write electronic invoices in Delphi: ZUGFeRD, its successor Factur-X, and the German
XRechnung format. The library works on a single invoice object — you fill it in, then decide which
version, profile and syntax to write it as.

This is a port of [ZUGFeRD-csharp](https://github.com/stephanstapel/ZUGFeRD-csharp) by Stephan
Stapel. The port is free to go its own way where that yields the better result; see
[Relationship to the C# library](#relationship-to-the-c-library).

## Formats and versions

| Format | write | read |
|---|:---:|:---:|
| ZUGFeRD 2.3 / Factur-X 1.0 — CII (Cross Industry Invoice) | yes | yes |
| XRechnung 3.x — UBL (Universal Business Language) | yes | yes |
| XRechnung — CII | yes | yes |
| ZUGFeRD 2.0 — CII | yes | yes |
| ZUGFeRD 1.0 (`rsm:CrossIndustryDocument`) | yes | yes |

UBL is available for version 2.3 only, and there only for the XRechnung profiles — that is what the
format is defined for. Everything else is CII.

## Profiles

`Minimum`, `BasicWL`, `Basic`, `Comfort` (EN 16931), `Extended`, `XRechnung1`, `XRechnung` and
`EReporting`. The profile decides which elements end up in the document: writing is filtered through
[intf.ZUGFeRDProfileAwareXmlTextWriter.pas](intf.ZUGFeRDProfileAwareXmlTextWriter.pas), so the same
invoice object yields a valid MINIMUM or a full EXTENDED document without any changes on your side.

Reading detects version and profile from the document itself. `TZUGFeRDInvoiceDescriptor.GetVersion`
answers that question before you load anything.

## Platform

Delphi 10.4 Sydney or newer, Win32 and Win64 — inline variable declarations are used throughout, and
`ZUGFeRDNullable<T>` relies on the custom managed record operators (`class operator Initialize`)
introduced with 10.4. XML is
handled through MSXML, so the library is Windows-only; apart from the RTL there are no dependencies.

MSXML is a COM component. A VCL application initialises COM for you; a console application has to do
it itself, otherwise the first read or write fails with *"Microsoft MSXML is not installed"*:

```delphi
uses Winapi.ActiveX;
...
CoInitialize(nil);
try
  //...
finally
  CoUninitialize;
end;
```

## Getting started

Put the units of the root directory on your search path. That is all the setup there is.

| Unit | Role |
|---|---|
| [intf.ZUGFeRDInvoiceDescriptor.pas](intf.ZUGFeRDInvoiceDescriptor.pas) | The invoice object: `TZUGFeRDInvoiceDescriptor`, plus `Load` and `Save` |
| [intf.ZUGFeRDInvoiceDescriptor23CIIWriter.pas](intf.ZUGFeRDInvoiceDescriptor23CIIWriter.pas) / [...23CIIReader.pas](intf.ZUGFeRDInvoiceDescriptor23CIIReader.pas) | CII for ZUGFeRD 2.2+ and Factur-X |
| [intf.ZUGFeRDInvoiceDescriptor22UBLWriter.pas](intf.ZUGFeRDInvoiceDescriptor22UBLWriter.pas) / [...22UblReader.pas](intf.ZUGFeRDInvoiceDescriptor22UblReader.pas) | UBL for XRechnung |
| [intf.ZUGFeRDInvoiceDescriptor20Writer.pas](intf.ZUGFeRDInvoiceDescriptor20Writer.pas) / [...20Reader.pas](intf.ZUGFeRDInvoiceDescriptor20Reader.pas) | ZUGFeRD 2.0 |
| [intf.ZUGFeRDInvoiceDescriptor1Writer.pas](intf.ZUGFeRDInvoiceDescriptor1Writer.pas) / [...1Reader.pas](intf.ZUGFeRDInvoiceDescriptor1Reader.pas) | ZUGFeRD 1.0 |
| [intf.ZUGFeRDInvoiceValidator.pas](intf.ZUGFeRDInvoiceValidator.pas) | Recalculates the document totals against the EN 16931 rules |
| [intf.ZUGFeRDHelper.pas](intf.ZUGFeRDHelper.pas) | Nullable wrapper, enum conversion, PDFtk wrapper |

The version-specific writers are picked for you — you never instantiate them directly.

## Writing an invoice

```delphi
uses
  intf.ZUGFeRDInvoiceDescriptor, intf.ZUGFeRDVersion, intf.ZUGFeRDProfile,
  intf.ZUGFeRDFormats, intf.ZUGFeRDCurrencyCodes, intf.ZUGFeRDCountryCodes,
  intf.ZUGFeRDQuantityCodes, intf.ZUGFeRDTaxTypes, intf.ZUGFeRDTaxCategoryCodes,
  intf.ZUGFeRDHelper;

var
  desc : TZUGFeRDInvoiceDescriptor;
begin
  desc := TZUGFeRDInvoiceDescriptor.CreateInvoice('R2026-0815', EncodeDate(2026, 9, 3),
                                                  TZUGFeRDCurrencyCodes.EUR);
  try
    desc.SetSeller('Verkaeufer GmbH', '01234', 'Verkaeuferstadt', 'Hauptstrasse 1',
                   TZUGFeRDNullableParam<TZUGFeRDCountryCodes>.Create(TZUGFeRDCountryCodes.DE));
    desc.SetBuyer('Kaeufer AG', '05678', 'Kaeuferstadt', 'Nebenweg 2',
                  TZUGFeRDNullableParam<TZUGFeRDCountryCodes>.Create(TZUGFeRDCountryCodes.DE));

    //BR-DE-2 and BR-DE-5..7: XRechnung requires a seller contact with name, phone and mail
    desc.SetSellerContact('Meier', '', 'meier@verkaeufer.de', '030 0815');

    desc.AddTradeLineItem(
      {name=}            'Beratungsleistung',
      {netUnitPrice=}    TZUGFeRDNullableParam<Currency>.Create(100),
      {description=}     '',
      {unitCode=}        TZUGFeRDNullableParam<TZUGFeRDQuantityCodes>.Create(TZUGFeRDQuantityCodes.HUR),
      {unitQuantity=}    nil,
      {grossUnitPrice=}  nil,
      {billedQuantity=}  2,
      {lineTotalAmount=} 200,
      {taxType=}         TZUGFeRDNullableParam<TZUGFeRDTaxTypes>.Create(TZUGFeRDTaxTypes.VAT),
      {categoryCode=}    TZUGFeRDNullableParam<TZUGFeRDTaxCategoryCodes>.Create(TZUGFeRDTaxCategoryCodes.S),
      {taxPercent=}      19.0);

    desc.AddApplicableTradeTax({calculatedAmount=} 38.00, {basisAmount=} 200.00,
                               {percent=} 19.0, TZUGFeRDTaxTypes.VAT, TZUGFeRDTaxCategoryCodes.S);

    desc.SetTotals(
      {lineTotalAmount=}      200.00,
      {chargeTotalAmount=}    0,
      {allowanceTotalAmount=} 0,
      {taxBasisAmount=}       200.00,
      {taxTotalAmount=}       38.00,
      {grandTotalAmount=}     238.00,
      {totalPrepaidAmount=}   0,
      {duePayableAmount=}     238.00);

    desc.Save('invoice-cii.xml', TZUGFeRDVersion.Version23, TZUGFeRDProfile.Comfort);
  finally
    desc.Free;
  end;
end;
```

The last call decides the output. The same invoice object also writes as an XRechnung in UBL:

```delphi
desc.Save('invoice-ubl.xml', TZUGFeRDVersion.Version23, TZUGFeRDProfile.XRechnung,
          TZUGFeRDFormats.UBL);
```

`Save` also takes a `TStream`. Amounts and dates are formatted invariantly, so the output does not
depend on the machine's locale.

Writing to a file checks the rules that a profile makes mandatory and raises
`TZUGFeRDMissingDataException` when something required is missing — the seller contact above is
there for exactly that reason. Leave it out and the CII document is still written, while the
XRechnung one is refused. The document is built in full before the target file is opened, so a
rejected invoice leaves an existing file untouched (an I/O error while writing, such as a full
disk, can still truncate it).

The `TStream` overload skips that shared check. Writer-specific checks still apply: UBL rejects any
profile other than `XRechnung`, and CII validates advance payments in the `Extended` profile. Call
`Save` with a file name, or use `TZUGFeRDInvoiceDescriptor23Writer.Validate` yourself, if you want
the full profile rules enforced before writing to a stream.

## Reading an invoice

One call handles every supported version and syntax; the reader is selected from the document:

```delphi
var
  desc : TZUGFeRDInvoiceDescriptor;
begin
  desc := TZUGFeRDInvoiceDescriptor.Load('invoice.xml');
  try
    Writeln(desc.InvoiceNo, ' ', desc.GrandTotalAmount.GetValueOrDefault:0:2);
  finally
    desc.Free;
  end;
end;
```

`Load` is also available for a `TStream` and an `IXMLDocument`. Optional fields come back as
`ZUGFeRDNullable<T>` — check `HasValue` before reading `Value`, because a missing amount and an
amount of zero are not the same thing.

`Profile` reflects what the document declares. For UBL that is `cbc:CustomizationID`, so a Peppol
BIS document reads back as `Unknown` rather than `XRechnung`, and cannot be written out again
unchanged. The raw identifier is always kept in `GuideLine`.

## Validating totals

`TZUGFeRDInvoiceValidator` recalculates the monetary summation of a loaded or newly built invoice and
compares it against the declared values, following the EN 16931 rules BR-CO-10 through BR-CO-17,
BR-DEC-20 and BR-53:

```delphi
var
  res : TZUGFeRDValidationResult;
begin
  res := TZUGFeRDInvoiceValidator.Validate(desc, TZUGFeRDVersion.Version23);
  try
    if not res.IsValid then
      Writeln(res.Messages.Text);   //every step of the recalculation is logged
  finally
    res.Free;
  end;
end;
```

This checks arithmetic consistency, not schema conformance. For the full picture validate the
generated XML against the official Schematron as well — see [Links](#links).

## Extracting XML from a PDF

The library reads and writes invoice XML; it does not open PDF containers itself. To get the XML out
of a ZUGFeRD or Factur-X PDF you need [PDFtk Server](https://www.pdflabs.com/tools/pdftk-server/),
which [intf.ZUGFeRDHelper.pas](intf.ZUGFeRDHelper.pas) wraps:

```delphi
var pdfAsStream : TStream;
var cmdOutput : String;

if GetZUGFeRDPdfHelper.SetPdfTkServerPath('C:\Program Files (x86)\PDFtk Server')
                      .PdfTkServerGetZUGFeRDPdfAttachment('zugferd.pdf',
                      pdfAsStream,
                      cmdOutput) then
try
  System.Write(cmdOutput);
finally
  pdfAsStream.Free;
end;
```

If you would rather not depend on an external tool, the sibling library
[XRechnung-for-Delphi](https://github.com/LandrixSoftware/XRechnung-for-Delphi) extracts PDF
attachments in pure Pascal.

## Samples and tests

- [Samples/](Samples/) — a VCL demo that builds, writes and reads invoices.
- [Unittest/](Unittest/) — DUnitX suite, console (`ZfDUnitTest.dpr`) and GUI runner. Beyond the
  per-version tests it sweeps every example invoice shipped under [documentation/](documentation/):
  each one is read, written back and read again, then the core data is compared (invoice number,
  number of line items, currency and grand total).
- [documentation/](documentation/) — the official specifications, schemas and example invoices from
  ZUGFeRD 1.0 up to 2.5.2 / Factur-X 1.09.2.

## Relationship to the C# library

Synchronization point:
https://github.com/stephanstapel/ZUGFeRD-csharp/commit/0bfdcb10680579c5a866d7301c848a39a4e67feb

The C# library is a reference, not a boundary. Where a finding turns out to be a genuine defect it is
fixed here rather than deferred — several corrections to the totals validation have since been ported
back the other way.

## Related project

[XRechnung-for-Delphi](https://github.com/LandrixSoftware/XRechnung-for-Delphi) covers the same
invoice formats with a data model built around the EN 16931 business terms, and can use this library
as an optional reading path for profile content beyond that model.

## Links

- [FeRD — ZUGFeRD 2.2 specification](https://www.ferd-net.de/standards/zugferd-2.2/zugferd-2.2.html)
- [FACTUR-X / ZUGFeRD](http://www.ferd-net.de/)
- [GEFEG profile browser — CII / Factur-X](https://portal3.gefeg.com/invoice/tthome/index/617afdc4-623f-44e0-a05b-5b878840e508)
- Validate online: [ecosio](https://ecosio.com/de/peppol-und-xml-dokumente-online-validieren/) · [invoice-portal](https://invoice-portal.de/xrechnung-peppol-bis-zugferd-validator/) · [ZUGFeRD Community](https://www.zugferd-community.net/de/open_community/validation)

## License

Subject to the [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0.html).
