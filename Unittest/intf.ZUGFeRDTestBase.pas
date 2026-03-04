unit intf.ZUGFeRDTestBase;

interface

uses
  System.SysUtils, System.IOUtils;

type
  TZUGFeRDTestBase = class
  protected
    function DemodataPath(const aRelativePath: string): string;
    function DocumentationPath(const aRelativePath: string): string;
  end;

implementation

{ TZUGFeRDTestBase }

function TZUGFeRDTestBase.DemodataPath(const aRelativePath: string): string;
begin
  // Exe liegt in Unittest/ -- 1 Ebene hoch = ZfD root
  Result := ExpandFileName(TPath.Combine(
    TPath.Combine(ExtractFilePath(ParamStr(0)), '..\demodata'),
    aRelativePath));
end;

function TZUGFeRDTestBase.DocumentationPath(const aRelativePath: string): string;
begin
  // Exe liegt in Unittest/ -- 1 Ebene hoch = ZfD root
  Result := ExpandFileName(TPath.Combine(
    TPath.Combine(ExtractFilePath(ParamStr(0)), '..\documentation'),
    aRelativePath));
end;

end.
