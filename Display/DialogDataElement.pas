Unit DialogDataElement;

{$mode ObjFPC}{$H+}
{$codepage utf8}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, DataElements;

Type

  { TdlgDataElement }

  TdlgDataElement = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    cboName: TComboBox;
    edtName: TEdit;
    edtPrefix: TEdit;
    cboType: TComboBox;
    edtSuffix: TEdit;
    lblSuffix: TLabel;
    lblName: TLabel;
    lblPrefix: TLabel;
    lblType: TLabel;
    pnlButtons: TPanel;
  Private
    FDataElement: TDataElement;

    Function GetAsXML: String;
    Procedure SetAsXML(AValue: String);

    Function GetName: String;
  Public
    Constructor Create(TheOwner: TComponent); Override;
    Destructor Destroy; Override;

    Procedure AvailableFields(Const AFields: TStringArray);

    Property AsXML: String Read GetAsXML Write SetAsXML;
  End;

Implementation

Uses
  StringSupport;

  {$R *.lfm}

  { TdlgDataElement }

Constructor TdlgDataElement.Create(TheOwner: TComponent);
Var
  s: String;
Begin
  Inherited Create(TheOwner);

  FDataElement := TDataElement.Create;

  AsXML := FDataElement.AsXML;

  cboType.Items.Clear;

  For s In ELEMENT_TYPES Do
    cboType.Items.Add(s);

  cboType.ItemIndex := 0;
End;

Destructor TdlgDataElement.Destroy;
Begin
  FDataElement.Free;
  FDataElement := nil;

  Inherited Destroy;
End;

Procedure TdlgDataElement.AvailableFields(Const AFields: TStringArray);
Var
  s: String;
Begin
  If Length(AFields) > 0 Then
  Begin
    cboName.Visible := True;
    edtName.Visible := False;

    cboName.Items.Clear;
    cboName.Items.Add('');

    For s In AFields Do
      cboName.Items.Add(s);
  End
  Else
  Begin
    cboName.Visible := False;
    edtName.Visible := True;
  End;
End;

Function TdlgDataElement.GetAsXML: String;
Begin
  FDataElement.Name := GetName;
  FDataElement.Prefix := edtPrefix.Text;
  FDataElement.Suffix := edtSuffix.Text;
  FDataElement.ElementType := StrToElementType(cboType.Text);

  Result := FDataElement.AsXML;
End;

Procedure TdlgDataElement.SetAsXML(AValue: String);
Begin
  FDataElement.AsXML := AValue;

  edtName.Text := FDataElement.Name;
  cboName.Text := FDataElement.Name;
  edtPrefix.Text := FDataElement.Prefix;
  edtSuffix.Text := FDataElement.Suffix;

  cboType.ItemIndex := Ord(FDataElement.ElementType);
End;

Function TdlgDataElement.GetName: String;
Begin
  If edtName.Visible Then
    Result := edtName.Text
  Else
    Result := cboName.Text;
End;

End.
