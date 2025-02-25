unit intf.ZUGFeRDGlobalIDSchemeIdentifiers;

interface

uses
  System.SysUtils
  ;

// see http://iso6523.info/icd_list.pdf
//

type
  TZUGFeRDGlobalIDSchemeIdentifiers = (
    /// <summary>
    /// Unknown means, we have a problem ...
    /// </summary>
    Unknown = 0,
    /// <summary>
    /// SIRENE (System Information et Repertoire des Entreprise et des Etablissements)
    /// </summary>
    Sirene = 2,
    /// <summary>
    /// SIRENE (System Information et Repertoire des Entreprise et des Etablissements)
    /// </summary>
    SiretCode = 9,
    /// <summary>
    /// SWIFT (BIC)
    /// </summary>
    Swift = 21,
    /// <summary>
    /// D-U-N-S Number
    /// </summary>
    DUNS = 60,
    /// <summary>
    /// GS1 Global Location Number (GLN)
    /// </summary>
    GLN = 88,
    /// <summary>
    /// GS1 Global Trade Item Number (GTIN, EAN)
    /// </summary>
    EAN = 160,
    /// <summary>
    /// OSCAR (Odette)
    /// </summary>
    ODETTE = 177,
    /// <summary>
    /// Numero d'entreprise / ondernemingsnummer / Unternehmensnummer
    /// </summary>
    CompanyNumber = 208,
    /// <summary>
    /// Leitweg-ID
    /// </summary>
    LeitwegID = 204
  );

  TZUGFeRDGlobalIDSchemeIdentifiersExtensions = class
  public
    class function FromString(s: string): TZUGFeRDGlobalIDSchemeIdentifiers;
    class function EnumToString(c: TZUGFeRDGlobalIDSchemeIdentifiers): string;
  end;

implementation

class function TZUGFeRDGlobalIDSchemeIdentifiersExtensions.FromString(s: string): TZUGFeRDGlobalIDSchemeIdentifiers;
begin
  if SameText(s,'0002') then
    Result := TZUGFeRDGlobalIDSchemeIdentifiers.Sirene else
  if SameText(s,'0009') then
    Result := TZUGFeRDGlobalIDSchemeIdentifiers.SiretCode else
  if SameText(s,'0021') then
    Result := TZUGFeRDGlobalIDSchemeIdentifiers.Swift else
  if SameText(s,'0060') then
    Result := TZUGFeRDGlobalIDSchemeIdentifiers.DUNS else
  if SameText(s,'0088') then
    Result := TZUGFeRDGlobalIDSchemeIdentifiers.GLN else
  if SameText(s,'0160') then
    Result := TZUGFeRDGlobalIDSchemeIdentifiers.EAN else
  if SameText(s,'0177') then
    Result := TZUGFeRDGlobalIDSchemeIdentifiers.ODETTE else
  if SameText(s,'0208') then
    Result := TZUGFeRDGlobalIDSchemeIdentifiers.CompanyNumber else
  if SameText(s,'0204') then
    Result := TZUGFeRDGlobalIDSchemeIdentifiers.LeitwegID else

  Result := TZUGFeRDGlobalIDSchemeIdentifiers.Unknown;
end;

class function TZUGFeRDGlobalIDSchemeIdentifiersExtensions.EnumToString(c: TZUGFeRDGlobalIDSchemeIdentifiers): string;
begin
  case c of
    TZUGFeRDGlobalIDSchemeIdentifiers.Sirene: Result := '0002';
    TZUGFeRDGlobalIDSchemeIdentifiers.SiretCode: Result := '0009';
    TZUGFeRDGlobalIDSchemeIdentifiers.Swift: Result := '0021';
    TZUGFeRDGlobalIDSchemeIdentifiers.DUNS: Result := '0060';
    TZUGFeRDGlobalIDSchemeIdentifiers.GLN: Result := '0088';
    TZUGFeRDGlobalIDSchemeIdentifiers.EAN: Result := '0160';
    TZUGFeRDGlobalIDSchemeIdentifiers.ODETTE: Result := '0177';
    TZUGFeRDGlobalIDSchemeIdentifiers.CompanyNumber: Result := '0208';
    TZUGFeRDGlobalIDSchemeIdentifiers.LeitwegID: Result := '0204';
  else
    Result := '0000';
  end;
end;

end.
