[![Donate](https://img.shields.io/badge/Donate-PayPal-green.svg)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=5V8N3XFTU495G)

# ZUGFeRD-for-Delphi

The ZUGFeRD library allows to create XML files as required by German electronic invoice initiative ZUGFeRD as well invoices in the successor Factur-X. One special profile of Factur-X is the German XRechnung format.
The library is meant to be as simple as possible, however it is not straight forward to use as the resulting XML file contains a complete invoice in XML format. Please take a look at the ZUGFeRD-Test project to find sample creation code. This code creates the same XML file as shipped with the ZUGFeRD information package.

## License
Subject to the Apache license http://www.apache.org/licenses/LICENSE-2.0.html

## TODO
Tests and writing support for ZUGFeRD invoices are still missing.

## Usage
The library is meant to be as simple as possible, however it is not straight forward to use as the resulting XML file contains a complete invoice in XML format. Please take a look at the ZUGFeRD-Test project to find sample creation code. This code creates the same XML file as shipped with the ZUGFeRD information package. More examples will come soon.

## Extracting xml attachments from pdf files
You can use the PDFtk Server tool to extract the xml attachment from a pdf file. The tool is available at
https://www.pdflabs.com/tools/pdftk-server/

A wrapper for the tool is available at unit intf.ZUGFeRDHelper.pas

```delphi
var pdfAsStream : TStream;
var cmdOutput : String;

if GetZUGFeRDPdfHelper.SetPdfTkServerPath('C:\Program Files (x86)\PDFtk Server')
                      .GetZUGFeRDPdfAttachment('zugferd.pdf',
                      pdfAsStream,
                      cmdOutput) then
try
  System.Write(cmdOutput);
finally
  pdfAsStream.Free;
end;
```

## Links
You can find more information about ZUGFeRD here:
http://www.ferd-net.de/
