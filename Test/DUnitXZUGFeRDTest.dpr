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

program DUnitXZUGFeRDTest;

{$APPTYPE CONSOLE}

{$STRONGLINKTYPES ON}

uses
  SysUtils,
  Forms,
  DUnitX.ConsoleWriter.Base,
  DUnitX.DUnitCompatibility,
  DUnitX.Generics,
  DUnitX.InternalInterfaces,
  DUnitX.IoC,
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Text,
  DUnitX.Loggers.XML.NUnit,
  DUnitX.Loggers.XML.xUnit,
  DUnitX.MacOS.Console,
  DUnitX.Test,
  DUnitX.TestFixture,
  DUnitX.TestFramework,
  DUnitX.TestResult,
  DUnitX.RunResults,
  DUnitX.TestRunner,
  DUnitX.Utils,
  DUnitX.Utils.XML,
  DUnitX.WeakReference,
  DUnitX.Windows.Console,
  DUnitX.StackTrace.EurekaLog7,
  DUnitX.FixtureResult,
  DUnitX.Loggers.Null,
  DUnitX.MemoryLeakMonitor.Default,
  DUnitX.Extensibility,
  DUnitX.CommandLine.OptionDef,
  DUnitX.CommandLine.Options,
  DUnitX.CommandLine.Parser,
  DUnitX.FixtureProviderPlugin,
  DUnitX.Timeout,
  DUnitX.Attributes,
  DUnitX.Linux.Console,
  intf.ZUGFeRDTaxCategoryCodes.UnitTests,
  intf.ZUGFeRDGlobalIDSchemeIdentifiers.UnitTests,
  intf.ZUGFeRDQuantityCodes.UnitTests,
  intf.ZUGFeRDAccountingAccountTypeCodes.UnitTests,
  intf.ZUGFeRDAdditionalReferencedDocumentTypeCodes.UnitTests,
  intf.ZUGFeRDContentCodes.UnitTests,
  intf.ZUGFeRDCountryCodes.UnitTests,
  intf.ZUGFeRDCurrencyCodes.UnitTests,
  intf.ZUGFeRDInvoiceTypes.UnitTests,
  intf.ZUGFeRDMimeTypeMapper.UnitTests,
  intf.ZUGFeRDTaxRegistrationSchemeID.UnitTests,
  intf.ZUGFeRDTaxTypes.UnitTests,
  intf.ZUGFeRDTaxExemptionReasonCodes.UnitTests,
  intf.ZUGFeRDSubjectCodes.UnitTests,
  intf.ZUGFeRDReferenceTypeCodes.UnitTests,
  intf.ZUGFeRDProfile.UnitTests,
  intf.ZUGFeRDPaymentMeansTypeCodes.UnitTests,

  intf.ZUGFeRDInvoiceDescriptor,intf.ZUGFeRDTradeLineItem,intf.ZUGFeRDInvoiceValidator
  ;

  //Übersetze folgenden C#-Code nach Delphi. Wenn ein Klassenname übersetzt wird, setze ihm den Präfix TZUGFeRD vorran. Stelle alles als Codeblock dar.
  //Translate the following C# code to Delphi. When translating a class name, prefix it with TZUGFeRD.

var
  runner : ITestRunner;
  results : IRunResults;
  logger : ITestLogger;
  nunitLogger : ITestLogger;
begin
  try
    //Create the runner
    runner := TDUnitX.CreateRunner;
    runner.UseRTTI := True;

    //tell the runner how we will log things

    if TDUnitX.Options.ConsoleMode <> TDunitXConsoleMode.Off then
    begin
      logger := TDUnitXConsoleLogger.Create(TDUnitX.Options.ConsoleMode = TDunitXConsoleMode.Quiet);
      runner.AddLogger(logger);
    end;

    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);

    //Run tests
    results := runner.Execute;

    System.Write('Done.. press <Enter> key to quit.');
    System.Readln;
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
end.
