{------------------------------------------------------------------------------
  Project   : SerialProcessor
  Unit      : DataElements (DataElements.pas)
  Description
    Defines typed data elements and collections for representing and formatting
    serial message fields.

  Source
    Original work by Mike Thompson (mike.cornflake@gmail.com)

  Dates
    Created        : 2025-11-14

  License
    This file is part of SerialProcessor.

    It is free software: you can redistribute it and/or modify it under the
    terms of the GNU General Public License as published by the Free Software
    Foundation, either version 3 of the License, or (at your option) any
    later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program.  If not, see <https://www.gnu.org/licenses/>.

    SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------}
Unit DataElements;

{$mode ObjFPC}{$H+}
{$codepage utf8}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
{$WARN 4105 off : Implicit string type conversion with potential data loss from "$1" to "$2"}
Interface

Uses
  Classes, SysUtils, fgl;

Type
  TElementType = (etString, etInt, etFloat, etFloat1dp, etFloat2dp, etFloat3dp, etFloat4dp);
  TDataElement = Class;

  { TDataElement }

  TDataElement = Class(TObject)
  Private
    FName: String;
    FPrefix: String;
    FSourceElement: TDataElement;
    FSuffix: String;
    FTimeRX: TDateTime;
    FType: TElementType;
    FTimeout: TDateTime;
    FValue: String;

    Function GetAsXML: String;
    Procedure SetAsXML(AValue: String);
  Public
    Constructor Create;

    Procedure Process(AInput: String);

    Function DataValid: Boolean;

    Property Name: String Read FName Write FName;
    Property Prefix: String Read FPrefix Write FPrefix;
    Property Suffix: String Read FSuffix Write FSuffix;
    Property Value: String Read FValue Write FValue;
    Property ElementType: TElementType Read FType Write FType;

    // This will only be set for output elements.
    Property SourceElement: TDataElement Read FSourceElement Write FSourceElement;

    Property TimeRX: TDateTime Read FTimeRX Write FTimeRX;
    Property AsXML: String Read GetAsXML Write SetAsXML;
    Property Timeout: TDateTime Read FTimeout Write FTimeout;
  End;

  { TDataElements }

  TDataElements = Class(Specialize TFPGObjectList<TDataElement>)
  Private
    FDelimiter: String;
    FLastString: String;
    FPrefix: String;
    FSuffix: String;
    FTimeout: TDateTime;
    FTimeRX: TDateTime;

    Function GetAsXML: String;
    Procedure SetAsXML(AValue: String);
  Public
    Constructor Create;

    Function Find(AName: String): TDataElement;
    Function IndexOf(AName: String): Integer;
    Function FieldNames: String;
    Function Data: String;

    Function Process(AInput: String): Boolean;
    Function DataValid: Boolean;

    Procedure ShowSetupDialog(AAvailableFields: TStringArray = nil);

    Property Prefix: String Read FPrefix Write FPrefix;
    Property Suffix: String Read FSuffix Write FSuffix;
    Property Delimiter: String Read FDelimiter Write FDelimiter;

    Property LastString: String Read FLastString Write FLastString;
    Property TimeRX: TDateTime Read FTimeRX Write FTimeRX;

    Property AsXML: String Read GetAsXML Write SetAsXML;
    Property Timeout: TDateTime Read FTimeout Write FTimeout;
  End;

Function ElementTypeToStr(AType: TElementType): String;
Function StrToElementType(AType: String): TElementType;

Const
  ELEMENT_TYPES: Array[TElementType] Of String =
    ('String', 'Int', 'Float', 'Float 1dp', 'Float 2dp', 'Float 3dp', 'Float 4dp');

  TIMEOUT_SECONDS: TDateTime = 5 / (24 * 60 * 60);

Implementation

Uses
  StringSupport, DOM, XMLRead, XMLSupport,
  Controls, DialogDataElements;

Function ElementTypeToStr(AType: TElementType): String;
Begin
  Result := ELEMENT_TYPES[AType];
End;

Function StrToElementType(AType: String): TElementType;
Var
  E: TElementType;
Begin
  For E := Low(TElementType) To High(TElementType) Do
    If SameText(ELEMENT_TYPES[E], AType) Then
      Exit(E);

  Raise Exception.CreateFmt('Unknown element type: %s', [AType]);
End;

{ TDataElement }

Constructor TDataElement.Create;
Begin
  FName := '';
  FPrefix := '';
  FSuffix := '';
  FTimeRX := 0;
  FType := etString;
  FValue := '';
  FTimeOut := TIMEOUT_SECONDS;
  FSourceElement := nil;
End;

Procedure TDataElement.Process(AInput: String);
Var
  s, sCurr: String;
  i: Longint;
  d: Extended;
Begin
  sCurr := FValue;
  FValue := '';

  If Trim(AInput) = '' Then
    Exit;

  s := TextBetween(AInput, FPrefix, FSuffix).Trim([' ', #13, #10]);

  If s = '' Then
  Begin
    If (Now - FTimeRx) > FTimeout Then
      FValue := ''
    Else
      FValue := sCurr;

    Exit;
  End;

  FTimeRX := Now;

  Try
    Case FType Of
      etString: FValue := s;
      etInt:
        If TryStrToInt(s, i) Then
          FValue := IntToStr(i)
        Else
          FValue := '';
      etFloat:
        If TryStrToFloat(s, d) Then
          FValue := Format('%f', [d])
        Else
          FValue := '';
      etFloat1dp:
        If TryStrToFloat(s, d) Then
          FValue := Format('%.1f', [d])
        Else
          FValue := '';
      etFloat2dp:
        If TryStrToFloat(s, d) Then
          FValue := Format('%.2f', [d])
        Else
          FValue := '';
      etFloat3dp:
        If TryStrToFloat(s, d) Then
          FValue := Format('%.3f', [d])
        Else
          FValue := '';
      etFloat4dp:
        If TryStrToFloat(s, d) Then
          FValue := Format('%.4f', [d])
        Else
          FValue := '';
    End;
  Except
    FValue := ''; // fallback on unexpected error
  End;
End;


Function TDataElement.DataValid: Boolean;
Begin
  Result := (Now - FTimeRX) < FTimeout;
End;

Function TDataElement.GetAsXML: String;
Var
  s: String;
Begin
  s := '';
  s += TXMLHelper.SetSetting('Name', FName);
  s += TXMLHelper.SetSetting('Prefix', FPrefix);
  s += TXMLHelper.SetSetting('Suffix', FSuffix);
  s += TXMLHelper.SetSetting('Type', ElementTypeToStr(FType));

  Result := TXMLHelper.SetXMLFragment('TDataElement', s);
End;

Procedure TDataElement.SetAsXML(AValue: String);
Var
  s: String;
Begin
  s := TXMLHelper.GetSetting(AValue, 'Name');
  If s <> '' Then
    Name := s
  Else
    Name := 'Data';

  FPrefix := TXMLHelper.GetSetting(AValue, 'Prefix');
  FSuffix := TXMLHelper.GetSetting(AValue, 'Suffix');

  s := TXMLHelper.GetSetting(AValue, 'Type');
  If s = '' Then
    FType := etString
  Else
    FType := StrToElementType(s);
End;

{ TDataElements }

Constructor TDataElements.Create;
Begin
  Inherited Create(True);

  FDelimiter := ',';
  FPrefix := '';
  FSuffix := '';

  FLastString := '';
  FTimeRX := 0;
  FTimeout := TIMEOUT_SECONDS;
End;

Function TDataElements.GetAsXML: String;
Var
  oElement: TDataElement;
  s: String;
Begin
  s := '';
  s += TXMLHelper.SetNodeText('Prefix', FPrefix);
  s += TXMLHelper.SetNodeText('Suffix', FSuffix);
  s += TXMLHelper.SetNodeText('Delimiter', FDelimiter);

  For oElement In Self Do
    s += oElement.AsXML;

  Result := TXMLHelper.SetXMLFragment('TDataElements', s);
End;

Procedure TDataElements.SetAsXML(AValue: String);
Var
  oDoc: TXMLDocument;
  oNode: TDOMNode;
  oDataElement: TDataElement;
  oStream: TStringStream;
Begin
  Clear;

  If Trim(AValue) = '' Then
    Exit;

  oStream := TStringStream.Create(AValue);
  Try
    ReadXMLFile(oDoc, oStream);
    Try
      oNode := oDoc.DocumentElement.FirstChild;
      While Assigned(oNode) Do
      Begin
        If oNode.NodeName = 'TDataElement' Then
        Begin
          oDataElement := TDataElement.Create;
          oDataElement.AsXML := TXMLHelper.NodeToXML(oNode);
          Add(oDataElement);
        End
        Else If oNode.NodeName = 'Prefix' Then
          FPrefix := oNode.TextContent
        Else If oNode.NodeName = 'Suffix' Then
          FSuffix := oNode.TextContent
        Else If oNode.NodeName = 'Delimiter' Then
          FDelimiter := oNode.TextContent;

        oNode := oNode.NextSibling;
      End
    Finally
      oDoc.Free;
    End;
  Finally
    oStream.Free;
  End;
End;

Procedure TDataElements.ShowSetupDialog(AAvailableFields: TStringArray);
Var
  oDlg: TdlgDataElements;
Begin
  oDlg := TdlgDataElements.Create(nil);
  If Assigned(AAvailableFields) Then
    oDlg.AvailableFields := AAvailableFields;

  oDlg.AsXML := Self.AsXML;
  Try
    If oDlg.ShowModal = mrOk Then
      Self.AsXML := oDlg.AsXML;
  Finally
    oDlg.Free;
  End;
End;

Function TDataElements.Find(AName: String): TDataElement;
Var
  oDataElement: TDataElement;
Begin
  Result := nil;

  For oDataElement In self Do
    If AnsiCompareText(AName, oDataElement.Name) = 0 Then
    Begin
      Result := oDataElement;
      Break;
    End;
End;

Function TDataElements.IndexOf(AName: String): Integer;
Var
  i: Integer;
  oDataElement: TDataElement;
Begin
  Result := -1;

  For i := 0 To Count - 1 Do
  Begin
    oDataElement := TDataElement(Items[i]);
    If AnsiCompareText(AName, oDataElement.Name) = 0 Then
    Begin
      Result := i;
      Break;
    End;
  End;
End;

Function TDataElements.Process(AInput: String): Boolean;
Var
  s: String;
  arrData: TStringArray;
  iLen: SizeInt;
  i: Integer;
Begin
  Result := False;
  FLastString := AInput;

  s := TextBetween(AInput, FPrefix, FSuffix).Trim([' ', #13, #10]);

  If s = '' Then
    Exit;

  If FDelimiter = '' Then
    Exit; // or raise an exception?

  arrData := s.Split([FDelimiter], #0, #0);
  iLen := Length(arrData);

  If iLen <> Self.Count Then
    Exit; // or log mismatch?

  For i := 0 To iLen - 1 Do
    TDataElement(Items[i]).Process(arrData[i]);

  FTimeRX := Now;
  Result := True;
End;

Function TDataElements.DataValid: Boolean;
Begin
  Result := (Now - FTimeRX) < FTimeout;
End;

Function TDataElements.FieldNames: String;
Var
  oElement: TDataElement;
  s: String;
Begin
  s := '';
  For oElement In Self Do
    s += oElement.Name + ',';

  If s <> '' Then
    s := Copy(s, 1, Length(s) - 1);

  Result := s;
End;

Function TDataElements.Data: String;
Var
  oElement: TDataElement;
  s: String;
Begin
  s := '';
  For oElement In Self Do
    s += oElement.Value + ',';

  If s <> '' Then
    s := Copy(s, 1, Length(s) - 1);

  Result := s;
End;

End.
