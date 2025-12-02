@echo off
REM ================================================================================
REM ZUGFeRD Enum Mapping Generator
REM ================================================================================
REM Dieses Script ruft PSC#ToDelphiDefinition.exe für alle Enum-Mappings auf
REM ================================================================================

setlocal enabledelayedexpansion

REM Pfade anpassen falls nötig
set TOOL=PSC#ToDelphiDefinition.exe
set TOOL2=PSDelphiDefinitionMapper.exe
set CS_DIR=C:\Wrk\Work\ERechnung\ZUGFeRD-csharp\ZUGFeRD
set DELPHI_DIR=..

echo ================================================================================
echo ZUGFeRD Enum Mapping Generator
echo ================================================================================
echo.
echo Tool: %TOOL%
echo C# Verzeichnis: %CS_DIR%
echo Delphi Verzeichnis: %DELPHI_DIR%
echo.
echo ================================================================================
echo.

REM Prüfe ob Tool existiert
if not exist "%TOOL%" (
    echo FEHLER: %TOOL% nicht gefunden!
    echo Bitte Pfad in der BAT-Datei anpassen oder Tool in aktuelles Verzeichnis kopieren.
    pause
    exit /b 1
)

set COUNT=0
set ERROR_COUNT=0

REM ================================================================================
REM Enum-Mappings (aus Enum-Mapping.txt)
REM ================================================================================

call :ProcessEnum "AccountingAccountTypeCodes" "AccountingAccountTypeCodes.cs" "intf.ZUGFeRDAccountingAccountTypeCodes.pas"
call :ProcessEnum "AdditionalReferencedDocumentTypeCode" "AdditionalReferencedDocumentTypeCodes.cs" "intf.ZUGFeRDAdditionalReferencedDocumentTypeCodes.pas"
call :ProcessEnum "AllowanceReasonCodes" "AllowanceReasonCodes.cs" "intf.ZUGFeRDAllowanceReasonCodes.pas"
call :ProcessEnum "ChargeReasonCodes" "ChargeReasonCodes.cs" "intf.ZUGFeRDChargeReasonCodes.pas"
call :ProcessEnum "ContentCodes" "ContentCodes.cs" "intf.ZUGFeRDContentCodes.pas"
call :ProcessEnum "CountryCodes" "CountryCodes.cs" "intf.ZUGFeRDCountryCodes.pas"
call :ProcessEnum "CurrencyCodes" "CurrencyCodes.cs" "intf.ZUGFeRDCurrencyCodes.pas"
call :ProcessEnum "DateTypeCodes" "DateTypeCodes.cs" "intf.ZUGFeRDDateTypeCodes.pas"
call :ProcessEnum "DesignatedProductClassificationClassCodes" "DesignatedProductClassificationClassCodes.cs" "intf.ZUGFeRDDesignatedProductClassificationClassCodes.pas"
call :ProcessEnum "ElectronicAddressSchemeIdentifiers" "ElectronicAddressSchemeIdentifiers.cs" "intf.ZUGFeRDElectronicAddressSchemeIdentifiers.pas"
call :ProcessEnum "GlobalIDSchemeIdentifiers" "GlobalIDSchemeIdentifiers.cs" "intf.ZUGFeRDGlobalIDSchemeIdentifiers.pas"
call :ProcessEnum "InvoiceType" "InvoiceType.cs" "intf.ZUGFeRDInvoiceTypes.pas"
call :ProcessEnum "PartyTypes" "PartyTypes.cs" "intf.ZUGFeRDPartyTypes.pas"
call :ProcessEnum "PaymentMeansTypeCodes" "PaymentMeansTypeCodes.cs" "intf.ZUGFeRDPaymentMeansTypeCodes.pas"
call :ProcessEnum "QuantityCodes" "QuantityCodes.cs" "intf.ZUGFeRDQuantityCodes.pas"
call :ProcessEnum "ReferenceTypeCodes" "ReferenceTypeCodes.cs" "intf.ZUGFeRDReferenceTypeCodes.pas"
call :ProcessEnum "SubjectCodes" "SubjectCodes.cs" "intf.ZUGFeRDSubjectCodes.pas"
call :ProcessEnum "TaxCategoryCodes" "TaxCategoryCodes.cs" "intf.ZUGFeRDTaxCategoryCodes.pas"
call :ProcessEnum "TaxExemptionReasonCodes" "TaxExemptionReasonCodes.cs" "intf.ZUGFeRDTaxExemptionReasonCodes.pas"
call :ProcessEnum "TaxRegistrationSchemeID" "TaxRegistrationSchemeID.cs" "intf.ZUGFeRDTaxRegistrationSchemeID.pas"
call :ProcessEnum "TaxTypes" "TaxTypes.cs" "intf.ZUGFeRDTaxTypes.pas"
call :ProcessEnum "TransportModeCodes" "TransportModeCodes.cs" "intf.ZUGFeRDTransportmodeCodes.pas"
call :ProcessEnum "ZUGFeRDVersion" "ZUGFeRDVersion.cs" "intf.ZUGFeRDVersion.pas"
call :ProcessEnum "LineStatusCodes" "LineStatusCodes.cs" "intf.ZUGFeRDLineStatusCodes.pas"
call :ProcessEnum "LineStatusReasonCodes" "LineStatusReasonCodes.cs" "intf.ZUGFeRDLineStatusReasonCodes.pas"
call :ProcessEnum "TradeDeliveryTermCodes" "TradeDeliveryTermCodes.cs" "intf.ZUGFeRDTradeDeliveryTermCodes.pas"
call :ProcessEnum "TransportModeCodes" "TransportModeCodes.cs" "intf.ZUGFeRDTransportModeCodes.pas"


echo.
echo ================================================================================
echo Zusammenfassung
echo ================================================================================
echo Verarbeitet: %COUNT% Enums
echo Fehler:      %ERROR_COUNT%
echo ================================================================================
echo.

if %ERROR_COUNT% gtr 0 (
    echo ACHTUNG: Es sind Fehler aufgetreten!
    pause
    exit /b 1
) else (
    echo Alle Enums erfolgreich verarbeitet!
    pause
    exit /b 0
)

REM ================================================================================
REM Hilfsfunktion zum Verarbeiten eines Enums
REM ================================================================================
:ProcessEnum
set ENUM_NAME=%~1
set CS_FILE=%~2
set DELPHI_FILE=%~3

set /a COUNT+=1

echo [%COUNT%] Verarbeite: %ENUM_NAME%
echo     C#:     %CS_FILE%
echo     Delphi: %DELPHI_FILE%

REM Prüfe ob C# Datei existiert
if not exist "%CS_DIR%\%CS_FILE%" (
    echo     WARNUNG: C# Datei nicht gefunden: %CS_DIR%\%CS_FILE%
    set /a ERROR_COUNT+=1
    echo.
    goto :eof
)

REM Prüfe ob Delphi Datei existiert
if not exist "%DELPHI_DIR%\%DELPHI_FILE%" (
    echo     WARNUNG: Delphi Datei nicht gefunden: %DELPHI_DIR%\%DELPHI_FILE%
    set /a ERROR_COUNT+=1
    echo.
    goto :eof
)

REM Rufe Tool auf
"%TOOL%" "%ENUM_NAME%" "%CS_DIR%\%CS_FILE%" "%DELPHI_DIR%\%DELPHI_FILE%"

if errorlevel 1 (
    echo     FEHLER: PSC#ToDelphiDefinition-Aufruf fehlgeschlagen!
    set /a ERROR_COUNT+=1
) else (
    echo     OK
)

REM Rufe Tool2 auf
"%TOOL2%" "%DELPHI_DIR%\%DELPHI_FILE%"

if errorlevel 1 (
    echo     FEHLER: PSDelphiDefinitionMapper-Aufruf fehlgeschlagen!
    set /a ERROR_COUNT+=1
) else (
    echo     OK
)

echo.
goto :eof
