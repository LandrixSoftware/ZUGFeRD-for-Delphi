unit intf.ZUGFeRDDateTypeCodes;

interface

uses
  System.SysUtils,
  intf.ZUGFeRDHelper;

// UNTID 2475 codes
//

type
  TZUGFeRDDateTypeCodes = (
    /// <summary>
    /// Unknown means, we have a problem ...
    /// </summary>
    Unknown = 0,

    /// <summary>
    /// Date of invoice
    /// </summary>
    InvoiceDate = 5,

    /// <summary>
    /// Date of delivery of goods to establishments/domicile/site
    /// </summary>
    DeliveryDate = 29,

    /// <summary>
    /// Payment date
    /// </summary>
    PaymentDate = 72
  );

  TZUGFeRDDateTypeCodesExtensions = class
  public
    class function FromString(s: string): ZUGFeRDNullable<TZUGFeRDDateTypeCodes>;
    class function EnumToString(c: ZUGFeRDNullable<TZUGFeRDDateTypeCodes>): string;
  end;

implementation

class function TZUGFeRDDateTypeCodesExtensions.FromString(s: string): ZUGFeRDNullable<TZUGFeRDDateTypeCodes>;
begin
  if s='' then
    exit(Nil);
  if SameText(s,'5') then
    Result := TZUGFeRDDateTypeCodes.InvoiceDate else
  if SameText(s,'29') then
    Result := TZUGFeRDDateTypeCodes.DeliveryDate else
  if SameText(s,'72') then
    Result := TZUGFeRDDateTypeCodes.PaymentDate else
  Result := Nil
end;

class function TZUGFeRDDateTypeCodesExtensions.EnumToString(c: ZUGFeRDNullable<TZUGFeRDDateTypeCodes>): string;
begin
  if Not(c.HasValue) then
    exit('');
  case c.Value of
    TZUGFeRDDateTypeCodes.InvoiceDate: Result := '5';
    TZUGFeRDDateTypeCodes.DeliveryDate: Result := '29';
    TZUGFeRDDateTypeCodes.PaymentDate: Result := '72';
  else
    Result := '';
  end;
end;

end.
