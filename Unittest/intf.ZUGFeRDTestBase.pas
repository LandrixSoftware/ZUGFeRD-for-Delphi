unit intf.ZUGFeRDTestBase;

interface

uses
  System.SysUtils, System.IOUtils;

type
  TZUGFeRDTestBase = class
  protected
    /// <summary>Measures allocated blocks, not reserved heap capacity, for isolated ownership checks.</summary>
    class function GetAllocatedMemory: NativeUInt; static;
    /// <summary>Requires a warmed-up operation to release its objects before the second measurement.</summary>
    class procedure AssertNoMemoryGrowth(const beforeBytes, afterBytes: NativeUInt); static;
    function DemodataPath(const aRelativePath: string): string;
    function DocumentationPath(const aRelativePath: string): string;
  end;

implementation

uses
  DUnitX.TestFramework;

{ TZUGFeRDTestBase }

class function TZUGFeRDTestBase.GetAllocatedMemory: NativeUInt;
var
  memoryManagerState: TMemoryManagerState;
  smallBlockTypeState: TSmallBlockTypeState;
begin
  {$WARN SYMBOL_PLATFORM OFF}
  GetMemoryManagerState(memoryManagerState);
  {$WARN SYMBOL_PLATFORM DEFAULT}
  Result := memoryManagerState.TotalAllocatedMediumBlockSize + memoryManagerState.TotalAllocatedLargeBlockSize;
  for smallBlockTypeState in memoryManagerState.SmallBlockTypeStates do
    Inc(Result, NativeUInt(smallBlockTypeState.UseableBlockSize) * smallBlockTypeState.AllocatedBlockCount);
end;

class procedure TZUGFeRDTestBase.AssertNoMemoryGrowth(const beforeBytes, afterBytes: NativeUInt);
begin
  Assert.IsTrue(afterBytes <= beforeBytes,
    Format('Allocated memory increased from %s to %s bytes.', [UIntToStr(beforeBytes), UIntToStr(afterBytes)]));
end;

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
