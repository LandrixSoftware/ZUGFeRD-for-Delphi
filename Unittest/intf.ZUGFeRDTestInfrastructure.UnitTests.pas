unit intf.ZUGFeRDTestInfrastructure.UnitTests;

interface

uses
  DUnitX.TestFramework,
  intf.ZUGFeRDTestBase;

type
  [TestFixture]
  TZUGFeRDTestInfrastructureTests = class(TZUGFeRDTestBase)
  public
    /// <summary>Proves that the heap assertion detects an object retained across the measurement.</summary>
    [Test]
    procedure TestHeapAssertionDetectsRetainedObject;
    /// <summary>Proves that releasing the measured object does not report a leak.</summary>
    [Test]
    procedure TestHeapAssertionAcceptsReleasedObject;
  end;

implementation

procedure TZUGFeRDTestInfrastructureTests.TestHeapAssertionDetectsRetainedObject;
var
  beforeBytes: NativeUInt;
  afterBytes: NativeUInt;
  retainedObject: TObject;
begin
  beforeBytes := GetAllocatedMemory;
  retainedObject := TObject.Create;
  try
    afterBytes := GetAllocatedMemory;
    Assert.IsTrue(afterBytes > beforeBytes, 'The active memory manager did not report the retained object.');
    Assert.WillRaiseAny(
      procedure
      begin
        AssertNoMemoryGrowth(beforeBytes, afterBytes);
      end,
      'The heap assertion did not detect the retained object.');
  finally
    // Keep the counterexample isolated; the test itself must not leak.
    retainedObject.Free;
  end;
end;

procedure TZUGFeRDTestInfrastructureTests.TestHeapAssertionAcceptsReleasedObject;
var
  beforeBytes: NativeUInt;
  retainedBytes: NativeUInt;
  afterBytes: NativeUInt;
  measuredObject: TObject;
begin
  beforeBytes := GetAllocatedMemory;
  measuredObject := TObject.Create;
  try
    retainedBytes := GetAllocatedMemory;
  finally
    measuredObject.Free;
  end;
  afterBytes := GetAllocatedMemory;
  Assert.IsTrue(retainedBytes > beforeBytes, 'The active memory manager did not report the measured object.');
  AssertNoMemoryGrowth(beforeBytes, afterBytes);
end;

end.
