unit intf.ZUGFeRDHelper;

interface

uses
  System.SysUtils,System.Classes,System.Types,System.DateUtils,System.Rtti
  ,System.Variants
  ;

type
  TZUGFeRDNullable<T> = class
  private
    FHasValue: Boolean;
    FValue: T;
  public
    constructor Create;
    constructor CreateWithValue(const AValue: T);

    function HasValue: Boolean;
    function GetValue: T;
    procedure SetValue(const AValue: T);

    property Value: T read GetValue write SetValue;
  end;

implementation

constructor TZUGFeRDNullable<T>.Create;
begin
  FHasValue := False;
end;

constructor TZUGFeRDNullable<T>.CreateWithValue(const AValue: T);
begin
  FHasValue := True;
  FValue := AValue;
end;

function TZUGFeRDNullable<T>.HasValue: Boolean;
begin
  Result := FHasValue;
end;

function TZUGFeRDNullable<T>.GetValue: T;
begin
  if not FHasValue then
    raise Exception.Create('Nullable object does not have a value.');

  Result := FValue;
end;

procedure TZUGFeRDNullable<T>.SetValue(const AValue: T);
begin
  FHasValue := True;
  FValue := AValue;
end;

end.


