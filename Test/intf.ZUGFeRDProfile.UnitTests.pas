{* Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.}

unit intf.ZUGFeRDProfile.UnitTests;

interface

uses
  System.SysUtils, DUnitX.TestFramework, intf.ZUGFeRDProfile,intf.ZUGFeRDVersion;

type
  [TestFixture]
  TZUGFeRDProfileTest = class
  public
    [Test]
    procedure TestFromString;
    [Test]
    procedure TestEnumToString;
  end;

implementation

procedure TZUGFeRDProfileTest.TestFromString;
begin
  Assert.AreEqual(TZUGFeRDProfile.Unknown, TZUGFeRDProfileExtensions.FromString('urn:ferd:invoice:1.0:unknown'));

  //v1
  Assert.AreEqual(TZUGFeRDProfile.Basic, TZUGFeRDProfileExtensions.FromString('urn:ferd:invoice:1.0:basic'));
  Assert.AreEqual(TZUGFeRDProfile.Basic, TZUGFeRDProfileExtensions.FromString('urn:ferd:invoice:rc:basic'));
  Assert.AreEqual(TZUGFeRDProfile.Basic, TZUGFeRDProfileExtensions.FromString('urn:ferd:CrossIndustryDocument:invoice:1p0:basic'));

  Assert.AreEqual(TZUGFeRDProfile.Comfort, TZUGFeRDProfileExtensions.FromString('urn:ferd:invoice:1.0:comfort'));
  Assert.AreEqual(TZUGFeRDProfile.Comfort, TZUGFeRDProfileExtensions.FromString('urn:ferd:invoice:rc:comfort'));
  Assert.AreEqual(TZUGFeRDProfile.Comfort, TZUGFeRDProfileExtensions.FromString('urn:ferd:CrossIndustryDocument:invoice:1p0:comfort'));
  Assert.AreEqual(TZUGFeRDProfile.Comfort, TZUGFeRDProfileExtensions.FromString('urn:ferd:CrossIndustryDocument:invoice:1p0:E'));

  Assert.AreEqual(TZUGFeRDProfile.Extended, TZUGFeRDProfileExtensions.FromString('urn:ferd:invoice:1.0:extended'));
  Assert.AreEqual(TZUGFeRDProfile.Extended, TZUGFeRDProfileExtensions.FromString('urn:ferd:invoice:rc:extended'));
  Assert.AreEqual(TZUGFeRDProfile.Extended, TZUGFeRDProfileExtensions.FromString('urn:ferd:CrossIndustryDocument:invoice:1p0:extended'));

  // v2
  Assert.AreEqual(TZUGFeRDProfile.Minimum, TZUGFeRDProfileExtensions.FromString('urn:zugferd.de:2p0:minimum'));
  Assert.AreEqual(TZUGFeRDProfile.Basic, TZUGFeRDProfileExtensions.FromString('urn:cen.eu:en16931:2017#compliant#urn:zugferd.de:2p0:basic'));
  Assert.AreEqual(TZUGFeRDProfile.Comfort, TZUGFeRDProfileExtensions.FromString('urn: cen.eu:en16931: 2017'));
  Assert.AreEqual(TZUGFeRDProfile.Extended, TZUGFeRDProfileExtensions.FromString('urn:cen.eu:en16931:2017#conformant#urn:zugferd.de:2p0:extended'));

  // v2.1
  Assert.AreEqual(TZUGFeRDProfile.Minimum, TZUGFeRDProfileExtensions.FromString('urn:factur-x.eu:1p0:minimum'));
  Assert.AreEqual(TZUGFeRDProfile.Basic, TZUGFeRDProfileExtensions.FromString('urn:cen.eu:en16931:2017#compliant#urn:factur-x.eu:1p0:basic'));
  Assert.AreEqual(TZUGFeRDProfile.BasicWL, TZUGFeRDProfileExtensions.FromString('urn:factur-x.eu:1p0:basicwl'));
  Assert.AreEqual(TZUGFeRDProfile.Comfort, TZUGFeRDProfileExtensions.FromString('urn:cen.eu:en16931:2017'));
  Assert.AreEqual(TZUGFeRDProfile.Extended, TZUGFeRDProfileExtensions.FromString('urn:cen.eu:en16931:2017#conformant#urn:factur-x.eu:1p0:extended'));
  Assert.AreEqual(TZUGFeRDProfile.XRechnung1, TZUGFeRDProfileExtensions.FromString('urn:cen.eu:en16931:2017#compliant#urn:xoev-de:kosit:standard:xrechnung_1.2'));
  Assert.AreEqual(TZUGFeRDProfile.XRechnung, TZUGFeRDProfileExtensions.FromString('urn:cen.eu:en16931:2017#compliant#urn:xoev-de:kosit:standard:xrechnung_2.0'));
  Assert.AreEqual(TZUGFeRDProfile.XRechnung, TZUGFeRDProfileExtensions.FromString('urn:cen.eu:en16931:2017#compliant#urn:xoev-de:kosit:standard:xrechnung_2.1'));
  Assert.AreEqual(TZUGFeRDProfile.XRechnung, TZUGFeRDProfileExtensions.FromString('urn:cen.eu:en16931:2017#compliant#urn:xoev-de:kosit:standard:xrechnung_2.2'));
  Assert.AreEqual(TZUGFeRDProfile.XRechnung, TZUGFeRDProfileExtensions.FromString('urn:cen.eu:en16931:2017#compliant#urn:xeinkauf.de:kosit:xrechnung_3.0'));
  Assert.AreEqual(TZUGFeRDProfile.EReporting, TZUGFeRDProfileExtensions.FromString('urn.cpro.gouv.fr:1p0:ereporting'));
end;

procedure TZUGFeRDProfileTest.TestEnumToString;
begin
  Assert.AreEqual('urn:ferd:CrossIndustryDocument:invoice:1p0:basic',TZUGFeRDProfileExtensions.EnumToString(TZUGFeRDProfile.Basic, TZUGFeRDVersion.Version1));
  Assert.AreEqual('urn:ferd:CrossIndustryDocument:invoice:1p0:comfort',TZUGFeRDProfileExtensions.EnumToString(TZUGFeRDProfile.Comfort, TZUGFeRDVersion.Version1));
  Assert.AreEqual('urn:ferd:CrossIndustryDocument:invoice:1p0:extended',TZUGFeRDProfileExtensions.EnumToString(TZUGFeRDProfile.Extended, TZUGFeRDVersion.Version1));

  Assert.AreEqual('urn:zugferd.de:2p0:minimum',TZUGFeRDProfileExtensions.EnumToString(TZUGFeRDProfile.Minimum, TZUGFeRDVersion.Version20));
  Assert.AreEqual('urn:cen.eu:en16931:2017#compliant#urn:zugferd.de:2p0:basic',TZUGFeRDProfileExtensions.EnumToString(TZUGFeRDProfile.Basic, TZUGFeRDVersion.Version20));
  Assert.AreEqual('urn:zugferd.de:2p0:basicwl',TZUGFeRDProfileExtensions.EnumToString(TZUGFeRDProfile.BasicWL, TZUGFeRDVersion.Version20));
  Assert.AreEqual('urn:cen.eu:en16931:2017',TZUGFeRDProfileExtensions.EnumToString(TZUGFeRDProfile.Comfort, TZUGFeRDVersion.Version20));
  Assert.AreEqual('urn:cen.eu:en16931:2017#conformant#urn:zugferd.de:2p0:extended',TZUGFeRDProfileExtensions.EnumToString(TZUGFeRDProfile.Extended, TZUGFeRDVersion.Version20));

  Assert.AreEqual('urn:factur-x.eu:1p0:minimum',TZUGFeRDProfileExtensions.EnumToString(TZUGFeRDProfile.Minimum, TZUGFeRDVersion.Version22));
  Assert.AreEqual('urn:cen.eu:en16931:2017#compliant#urn:factur-x.eu:1p0:basic',TZUGFeRDProfileExtensions.EnumToString(TZUGFeRDProfile.Basic, TZUGFeRDVersion.Version22));
  Assert.AreEqual('urn:factur-x.eu:1p0:basicwl',TZUGFeRDProfileExtensions.EnumToString(TZUGFeRDProfile.BasicWL, TZUGFeRDVersion.Version22));
  Assert.AreEqual('urn:cen.eu:en16931:2017',TZUGFeRDProfileExtensions.EnumToString(TZUGFeRDProfile.Comfort, TZUGFeRDVersion.Version22));
  Assert.AreEqual('urn:cen.eu:en16931:2017#conformant#urn:factur-x.eu:1p0:extended',TZUGFeRDProfileExtensions.EnumToString(TZUGFeRDProfile.Extended, TZUGFeRDVersion.Version22));
  Assert.AreEqual('urn:cen.eu:en16931:2017#compliant#urn:xoev-de:kosit:standard:xrechnung_1.2',TZUGFeRDProfileExtensions.EnumToString(TZUGFeRDProfile.XRechnung1, TZUGFeRDVersion.Version22));
  if Now >= EncodeDate(2024, 2, 1) then
    Assert.AreEqual('urn:cen.eu:en16931:2017#compliant#urn:xeinkauf.de:kosit:xrechnung_3.0',TZUGFeRDProfileExtensions.EnumToString(TZUGFeRDProfile.XRechnung, TZUGFeRDVersion.Version22))
          else
    Assert.AreEqual('urn:cen.eu:en16931:2017#compliant#urn:xoev-de:kosit:standard:xrechnung_2.3',TZUGFeRDProfileExtensions.EnumToString(TZUGFeRDProfile.XRechnung, TZUGFeRDVersion.Version22));
  Assert.AreEqual('urn.cpro.gouv.fr:1p0:ereporting',TZUGFeRDProfileExtensions.EnumToString(TZUGFeRDProfile.EReporting, TZUGFeRDVersion.Version22));
end;

initialization

//I was hoping to use RTTI to discover the TestFixture classes, however unlike .NET
//if we don't touch the class somehow then the linker will remove
//the class from the resulting exe.
//We could just do this:
//TMyExampleTests.ClassName;
//TExampleFixture2.ClassName;
//which is enough to make the compiler link the classes into the exe, but that seems a
//bit redundent so I guess we'll just use manual registration. If you use the
//{$STRONGLINKTYPES ON} compiler directive then it will link the TestFixtures in and you
//can use RTTI. The downside to that is the resulting exe will potentially much larger.
//Not sure which version {$STRONGLINKTYPES ON} was introduced so we'll allow RTTI and
//manual registration for now.

//  TDUnitX.RegisterTestFixture(TZUGFeRDProfileTest);

end.
