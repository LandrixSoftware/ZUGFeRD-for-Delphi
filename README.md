[![Donate](https://img.shields.io/badge/Donate-PayPal-green.svg)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=5V8N3XFTU495G)

# ZUGFeRD-for-Delphi

## Version

Aktuelle ZUGFeRD-Version 2.1

## ZUGFeRD und Factur-X

ZUGFeRD 2.1 ist nun mit dem französischen Standard für die elektronische Rechnungsstellung "Factur-X 1.0" vollständig kompatibel und technisch identisch. Beide Formate verwenden jetzt die gemeinsame Kennung factur-x. Bei der Entwicklung von Software für den Empfang von ZUGFeRD-Rechnungen empfehlen wir daher, ab sofort zusätzlich den Dateinamen "factur-x.xml" für die eingebettete XML-Datei zu berücksichtigen.

## ZUGFeRD und XRechnung

Die Datenstruktur von ZUGFeRD 2.0 basiert ebenso wir die XRechnung 1.0 auf der Norm EN 16931. Das ZUGFeRD 2.0 Profil EN 16931 ist identisch mit der XRechnung. Dieses Profil kann deshalb für den Versand von Rechnungen an die öffentliche Verwaltung genutzt werden.

ZUGFeRD hat zwei Vorteile: Zum einen besteht ZUGFeRD auch aus einem Bildteil in PDF, so dass die Rechnung auch an Rechnungsempfänger ohne automatisierte (Weiter-)Verarbeitung gesandt werden kann.
Die XRechnung ist ein reines XML-Datenformat. Zum anderen können mit ZUGFeRD sowohl geringere Rechnungsanforderungen (Format BASIC) als auch erhöhte Informationsanforderungen an die Rechnung (Profil Extended) realisiert werden. ZUGFeRD optimiert dadurch gezielt den Rechnungsaustausch zwischen Unternehmen und bietet auch eine Lösung für kleine oder nicht technikorientierte Marktteilnehmer.

## Unterstützte Anwendungsprofile

### EXTENDED

Das Profil EXTENDED ist eine Erweiterung der EN 16931-1 zur Unterstützung
komplexerer Geschäftsprozesse (Rechnungen, in denen über mehrere
Lieferungen / Lieferorte abgerechnet wird, strukturierte
Zahlungsbedingungen, weitere Angaben auf Positionsebene zur
Unterstützung der Lagerhaltung etc.)

### EN 16931 (COMFORT)

Das EN 16931 (COMFORT) Profil bildet die EN 16931-1 vollständig ab und
fokussiert auf Kernelemente einer elektronischen Rechnung.

### BASIC

Das Profil BASIC stellt eine Untermenge der EN 16931-1 dar und kann für
einfache UStG-konforme Rechnungen genutzt werden.

### BASIC WL

Das Profil BASIC WL beinhaltet keine Rechnungspositionen und kann somit
keine UStG-konformen Rechnungen abbilden. Es enthält jedoch auf
Dokumentenebene alle Informationen, die zur Buchung der Rechnung
benötigt werden. Es stellt somit eine Buchungshilfe dar.

### MINIMUM

Das Profil MINIMUM enthält die wesentlichen Angaben zu Käufer und
Verkäufer, den Gesamtrechnungsbetrag und die Gesamtumsatzsteuer. Auf
Positionsebene kann nur die Referenz des Käufers angegeben werden. Eine
Aufschlüsselung der Umsatzsteuer wird nicht unterstützt. Es stellt somit eine
Buchungshilfe dar.
