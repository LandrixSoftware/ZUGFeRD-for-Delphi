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
    procedure SetValueClass(AValue : TZUGFeRDNullable<T>);
    procedure ClearValue;

    property Value: T read GetValue write SetValue;
  end;

  TZUGFeRDNullableCurrency = class
  private
    FHasValue: Boolean;
    FValue: Currency;
  public
    constructor Create;
    constructor CreateWithValue(const AValue: Currency);

    function HasValue: Boolean;
    function GetValue: Currency;
    procedure SetValue(const AValue: Currency);
    procedure SetValueClass(AValue : TZUGFeRDNullableCurrency);
    procedure ClearValue;

    property Value: Currency read GetValue write SetValue;
  end;

implementation

procedure TZUGFeRDNullable<T>.ClearValue;
begin
  FHasValue := false;
end;

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

procedure TZUGFeRDNullable<T>.SetValueClass(AValue: TZUGFeRDNullable<T>);
begin
  if AValue = nil then
    ClearValue
  else
    Value := AValue.Value;
end;

{ TZUGFeRDNullableCurrency }

procedure TZUGFeRDNullableCurrency.ClearValue;
begin
  FHasValue := false;
end;

constructor TZUGFeRDNullableCurrency.Create;
begin
  FHasValue := false;
end;

constructor TZUGFeRDNullableCurrency.CreateWithValue(
  const AValue: Currency);
begin
  FHasValue := True;
  FValue := AValue;
end;

function TZUGFeRDNullableCurrency.GetValue: Currency;
begin
  if not FHasValue then
    raise Exception.Create('Nullable object does not have a value.');

  Result := FValue;
end;

function TZUGFeRDNullableCurrency.HasValue: Boolean;
begin
  Result := FHasValue;
end;

procedure TZUGFeRDNullableCurrency.SetValue(const AValue: Currency);
begin
  FHasValue := True;
  FValue := AValue;
end;

procedure TZUGFeRDNullableCurrency.SetValueClass(
  AValue: TZUGFeRDNullableCurrency);
begin
  if AValue = nil then
    ClearValue
  else
    Value := AValue.Value;
end;

end.


