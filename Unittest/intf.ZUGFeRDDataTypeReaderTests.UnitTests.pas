unit intf.ZUGFeRDDataTypeReaderTests.UnitTests;

interface

uses
  DUnitX.TestFramework,
  intf.ZUGFeRDTestBase;

type
  [TestFixture]
  TZUGFeRDDataTypeReaderTests = class(TZUGFeRDTestBase)
  public
    [Test]
    procedure ReadFormattedIssueDateTime_ReturnsCorrectDateTime_WhenNodeContainsQdt;
    [Test]
    procedure ReadFormattedIssueDateTime_ReturnsCorrectDateTime_WhenNodeContainsUdt;
    [Test]
    procedure ReadFormattedIssueDateTime_ReturnsDefaultValue_WhenNodeIsEmpty;
    [Test]
    procedure ReadFormattedIssueDateTime_ReturnsDefaultValue_WhenNodeDoesNotContainQdtOrUdt;
  end;

implementation

uses
  System.SysUtils, System.DateUtils,
  Winapi.MSXMLIntf, Winapi.msxml,
  intf.ZUGFeRDDataTypeReader;

{ TZUGFeRDDataTypeReaderTests }

procedure TZUGFeRDDataTypeReaderTests.ReadFormattedIssueDateTime_ReturnsCorrectDateTime_WhenNodeContainsQdt;
var
  doc: IXMLDOMDocument2;
  node: IXMLDOMNode;
  res: TDateTime;
begin
  doc := CoDOMDocument60.Create;
  doc.loadXML(
    '<root xmlns="http://www.sample.com/file" xmlns:qdt="http://example.com/1">' +
    '<dateTime>' +
    '<qdt:DateTimeString format="102">20230101</qdt:DateTimeString>' +
    '</dateTime></root>');
  doc.setProperty('SelectionLanguage', 'XPath');
  doc.setProperty('SelectionNamespaces',
    'xmlns:def="http://www.sample.com/file" xmlns:qdt="http://example.com/1"');
  node := doc.selectSingleNode('//def:dateTime');

  res := TZUGFeRDDataTypeReader.ReadFormattedIssueDateTime(node, '.', 0);
  Assert.AreEqual(EncodeDate(2023, 1, 1), res);
end;

procedure TZUGFeRDDataTypeReaderTests.ReadFormattedIssueDateTime_ReturnsCorrectDateTime_WhenNodeContainsUdt;
var
  doc: IXMLDOMDocument2;
  node: IXMLDOMNode;
  res: TDateTime;
begin
  doc := CoDOMDocument60.Create;
  doc.loadXML(
    '<root xmlns="http://www.sample.com/file" xmlns:udt="http://example.com/1">' +
    '<dateTime>' +
    '<udt:DateTimeString format="102">20230101</udt:DateTimeString>' +
    '</dateTime></root>');
  doc.setProperty('SelectionLanguage', 'XPath');
  doc.setProperty('SelectionNamespaces',
    'xmlns:def="http://www.sample.com/file" xmlns:udt="http://example.com/1"');
  node := doc.selectSingleNode('//def:dateTime');

  res := TZUGFeRDDataTypeReader.ReadFormattedIssueDateTime(node, '.', 0);
  Assert.AreEqual(EncodeDate(2023, 1, 1), res);
end;

procedure TZUGFeRDDataTypeReaderTests.ReadFormattedIssueDateTime_ReturnsDefaultValue_WhenNodeIsEmpty;
var
  doc: IXMLDOMDocument2;
  node: IXMLDOMNode;
  expected, res: TDateTime;
begin
  doc := CoDOMDocument60.Create;
  doc.loadXML('<root><dateTime></dateTime></root>');
  doc.setProperty('SelectionLanguage', 'XPath');
  node := doc.selectSingleNode('//dateTime');
  expected := EncodeDate(2023, 1, 1);

  res := TZUGFeRDDataTypeReader.ReadFormattedIssueDateTime(node, '.', expected);
  Assert.AreEqual(expected, res);
end;

procedure TZUGFeRDDataTypeReaderTests.ReadFormattedIssueDateTime_ReturnsDefaultValue_WhenNodeDoesNotContainQdtOrUdt;
var
  doc: IXMLDOMDocument2;
  node: IXMLDOMNode;
  defaultValue, res: TDateTime;
begin
  doc := CoDOMDocument60.Create;
  doc.loadXML('<root><dateTime>NoSpecialTag</dateTime></root>');
  doc.setProperty('SelectionLanguage', 'XPath');
  node := doc.selectSingleNode('//dateTime');
  defaultValue := EncodeDate(2023, 1, 1);

  res := TZUGFeRDDataTypeReader.ReadFormattedIssueDateTime(node, '.', defaultValue);
  Assert.AreEqual(defaultValue, res);
end;

end.
