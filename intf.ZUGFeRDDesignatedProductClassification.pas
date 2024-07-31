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

unit intf.ZUGFeRDDesignatedProductClassification;

interface

uses
  intf.ZUGFeRDDesignatedProductClassificationClassCodes;

type
  /// <summary>
  /// Detailed information on the item classification
  /// </summary>
  TZUGFeRDDesignatedProductClassification = class
  private
    FClassName: String;
    FListID: String;
    FListVersionID: String;
    FClassCode: TZUGFeRDDesignatedProductClassificationClassCodes;
  public
    /// <summary>
    /// A code for the classification of an item according to type or kind or nature.
    ///
    /// Classification codes are used for the aggregation of similar products, which might be useful for various
    /// purposes, for instance like public procurement, in accordance with the Common Vocabulary for Public Procurement
    /// [CPV]), e-Commerce(UNSPSC) etc.
    /// </summary>
    property ClassCode : TZUGFeRDDesignatedProductClassificationClassCodes read FClassCode write FClassCode;

    /// <summary>
    /// Product classification name
    /// </summary>
    property ListID : String read FListID write FListID;

    /// <summary>
    /// Version of product classification
    /// </summary>
    property ListVersionID : String read FListVersionID write FListVersionID;

    /// <summary>
    /// Classification name
    /// </summary>
    property ClassName_ : String read FClassName write FClassName;
  end;

implementation

end.
