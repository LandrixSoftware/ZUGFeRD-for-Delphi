# C# Bugs - gefunden waehrend Delphi-Portierungs-Review

## UBL Reader (InvoiceDescriptor22UBLReader.cs)

### BUG 1: TaxRepresentativeParty XPath falsch [MEDIUM]
**Zeile 200:** `"//cac:TaxRepresentativeParty/cac:Party"`
**Problem:** In UBL 2.1 ist `cac:TaxRepresentativeParty` direkt vom Typ `PartyType` - es gibt kein Kind-Element `cac:Party`. Der XPath findet daher nie einen Knoten, SellerTaxRepresentative bleibt immer null.
**Fix:** `"//cac:TaxRepresentativeParty"` (ohne `/cac:Party`)
**Delphi:** Korrekt implementiert (ohne `/cac:Party`), Kommentar bestaetigt: "Note: in UBL TaxRepresentativeParty contains party info directly"

## v2.3 CII Reader (InvoiceDescriptor23CIIReader.cs)

### BUG 2: BillingPeriod End wird zweimal geprueft statt Start AND End
**Delphi-Review:** Delphi prueft korrekt Start AND End, C# prueft End zweimal.

### BUG 3: DocumentReference mit absolutem XPath `//` statt relativem `.//`
**Delphi-Review:** Delphi nutzt korrekt `.//` (relativ zum Kontext-Knoten), C# nutzt `//` (absolut vom Dokument-Root).

### BUG 4: UnitCode Fallback nur fuer Invoice, nicht CreditNote
**Delphi-Review:** Delphi behandelt korrekt Invoice UND CreditNote, C# nur Invoice.
