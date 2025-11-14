Unit DataFields;

{$mode ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, fgl;

Type

  { TDataElement }

  TDataElement = Class(TObject)
  Private
    Function GetAsXML: String;
    Procedure SetAsXML(AValue: String);
  Public
    Name: String;
    ID: String;
    Value: String;
    FieldType: TTypeKind;

    Constructor Create;

    Property AsXML: String Read GetAsXML Write SetAsXML;
  End;

  { TDataElements }

  TDataElements = Class(Specialize TFPGObjectList<TDataElement>)
  Private
    Function GetAsXML: String;
    Procedure SetAsXML(AValue: String);
  Public
    Function Find(AName: String): TDataElement;
    Function IndexOf(AName: String): Integer;

    Procedure ShowSetupDialog;
    Property AsXML: String Read GetAsXML Write SetAsXML;
  End;


Implementation

Uses
  StringSupport, DOM, XMLRead, XMLSupport,
  Controls, DialogDataElements;

  { TDataElement }

Constructor TDataElement.Create;
Begin
  Name := 'Data';
  ID := '';
  Value := '';
  FieldType := tkUString;
End;

Function TDataElement.GetAsXML: String;
Begin
  Result := '<TDataElement>';
  Result := Result + Format('<setting name="%s" value="%s"/>', ['Name', Name]);
  Result := Result + Format('<setting name="%s" value="%s"/>', ['ID', ID]);
  Result := Result + Format('<setting name="%s" value="%s"/>', ['LastValue', Value]);
  Result := Result + Format('<setting name="%s" value="%s"/>',
    ['FieldType', FieldTypeToString(FieldType)]);
  Result := Result + '</TDataElement>';
End;

Procedure TDataElement.SetAsXML(AValue: String);

  Function GetValue(AName: String): String;
  Var
    sTemp: String;
  Begin
    sTemp := Trim(TextBetween(AValue, '<setting name="' + AName + '"', '/>'));
    Result := TextBetween(sTemp, '"', '"');
  End;

Begin
  Name := GetValue('Name');
  If Name = '' Then
    Name := 'Data';

  ID := GetValue('ID');
  Value := GetValue('LastValue');
  FieldType := StringToFieldType(GetValue('FieldType'));
End;

{ TDataElements }

Function TDataElements.GetAsXML: String;
Var
  oElement: TDataElement;
Begin
  Result := '<DataElements>' + LineEnding;
  For oElement In Self Do
    Result := Result + oElement.AsXML + LineEnding;
  Result := Result + '</DataElements>' + LineEnding;
End;

Procedure TDataElements.SetAsXML(AValue: String);
Var
  oDoc: TXMLDocument;
  oNode: TDOMNode;
  oDataElement: TDataElement;
  oStream: TStringStream;
Begin
  Clear;
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
          oDataElement.AsXML := NodeToXML(oNode);
          Add(oDataElement);
        End;
        oNode := oNode.NextSibling;
      End;
    Finally
      oDoc.Free;
    End;
  Finally
    oStream.Free;
  End;
End;

Procedure TDataElements.ShowSetupDialog;
var
  oDlg: TdlgDataElements;
Begin
  oDlg := TdlgDataElements.Create(Nil);
  oDlg.AsXML:=Self.AsXML;
  Try
    If oDlg.ShowModal=mrOK Then
      Self.AsXML := oDlg.AsXML;
  finally
    oDlg.Free;
  end;
End;

Function TDataElements.Find(AName: String): TDataElement;
Var
  oDataElement: TDataElement;
Begin
  Result := nil;

  For oDataElement In self Do
    If Uppercase(AName) = Uppercase(oDataElement.Name) Then
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
    If Uppercase(AName) = Uppercase(oDataElement.Name) Then
    Begin
      Result := i;
      Break;
    End;
  End;
End;

End.
