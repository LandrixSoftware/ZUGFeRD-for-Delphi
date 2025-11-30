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

unit intf.ZUGFeRDInvoiceCommentConstants;

interface
type
  TZUGFeRDInvoiceCommentConstants = Class
  public
  const
    IncludedSupplyChainTradeLineItemComment = 'Artikelposition';
    NetPriceProductTradePriceComment = 'Nettopreis';
    SpecifiedTradeSettlementLineMonetarySummationComment = 'Gesamtsummierung pro Position';
    ApplicableHeaderTradeAgreementComment = 'Formulardaten wie Käufer, Verkäufer etc.';
    BuyerReferenceComment = 'Ihr Zeichen bzw. Leitweg-Id';
    SellerTradePartyComment = 'Verkäufer';
    BuyerTradePartyComment = 'Käufer';
    BuyerOrderReferencedDocumentComment = 'Bestelldokument';
    ApplicableHeaderTradeDeliveryComment = 'Lieferdaten samt abw. Lieferadresse';
    DespatchAdviceReferencedDocumentComment = 'Lieferschein';
    ApplicableHeaderTradeSettlementComment = 'Dokumentdaten';
    SpecifiedTradeSettlementPaymentMeansComment = 'Zahlungsart mit Zahlungsinfo';
    ApplicableTradeTaxComment = 'Steuerposition pro Steuersatz für Dokument';
    SpecifiedTradeSettlementHeaderMonetarySummationComment = 'Gesamtsummierung des Dokumentes';
  End;

implementation

end.
