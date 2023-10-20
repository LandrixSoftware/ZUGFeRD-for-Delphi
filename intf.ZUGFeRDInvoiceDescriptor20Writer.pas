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

unit intf.ZUGFeRDInvoiceDescriptor20Writer;

interface

uses
  System.Classes,
  intf.ZUGFeRDInvoiceDescriptor, intf.ZUGFeRDProfileAwareXmlTextWriter,
  intf.ZUGFeRDInvoiceDescriptorWriter;

type
  TZUGFeRDInvoiceDescriptor20Writer = class(TZUGFeRDInvoiceDescriptorWriter)
  private
    FWriter: TZUGFeRDProfileAwareXmlTextWriter;
    FDescriptor: TZUGFeRDInvoiceDescriptor;
  public
    /// <summary>
    /// Saves the given invoice to the given stream.
    /// Make sure that the stream is open and writeable. Otherwise, an IllegalStreamException will be thron.
    /// </summary>
    /// <param name="descriptor"></param>
    /// <param name="stream"></param>
    procedure Save(descriptor: TZUGFeRDInvoiceDescriptor; stream: TStream); override;
  end;

implementation

{ TZUGFeRDInvoiceDescriptor20Writer }

procedure TZUGFeRDInvoiceDescriptor20Writer.Save(descriptor: TZUGFeRDInvoiceDescriptor; stream: TStream);
begin


end;

end.
