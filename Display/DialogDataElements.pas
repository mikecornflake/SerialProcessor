Unit DialogDataElements;

{$mode ObjFPC}{$H+}
{$codepage utf8}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, DataElements;

Type

  { TdlgDataElements }

  TdlgDataElements = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    edtPrefix: TEdit;
    edtSuffix: TEdit;
    edtDelimiter: TEdit;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lvDataElements: TListView;
    pnlTop: TPanel;
    pnlFields: TPanel;
    pnlMain: TPanel;
    pnlButtons: TPanel;
    tbMain: TToolBar;
    btnAdd: TToolButton;
    btnUp: TToolButton;
    btnDown: TToolButton;
    btnEdit: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    btnDelete: TToolButton;
    Procedure btnAddClick(Sender: TObject);
    Procedure btnDeleteClick(Sender: TObject);
    Procedure btnDownClick(Sender: TObject);
    Procedure btnEditClick(Sender: TObject);
    Procedure btnUpClick(Sender: TObject);
    Procedure lvDataElementsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    Procedure lvDataElementsDblClick(Sender: TObject);
    Procedure lvDataElementsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
  Private
    FAvailableFields: TStringArray;
    FDataElements: TDataElements;

    Function GetAsXML: String;
    Procedure RefreshToolbar;
    Procedure RefreshListView;
    Procedure SetAsXML(AValue: String);
    Procedure SetAvailableFields(AValue: TStringArray);
    Procedure SetListItem(AListItem: TListItem; ADataElement: TDataElement);
  Public
    Constructor Create(TheOwner: TComponent); Override;
    Destructor Destroy; Override;

    Property AvailableFields: TStringArray Read FAvailableFields Write SetAvailableFields;

    Property AsXML: String Read GetAsXML Write SetAsXML;
  End;


Implementation

Uses
  StringSupport,
  DialogDataElement;

  {$R *.lfm}

  { TdlgDataElements }


Constructor TdlgDataElements.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);

  FDataElements := TDataElements.Create;
End;

Destructor TdlgDataElements.Destroy;
Begin
  FDataElements.Free;
  FDataElements := nil;

  Inherited Destroy;
End;

Procedure TdlgDataElements.RefreshToolbar;
Var
  iCount, iIndex: Integer;
Begin
  btnAdd.Enabled := Assigned(FDataElements);

  iIndex := lvDataElements.ItemIndex;
  iCount := lvDataElements.Items.Count;

  btnDelete.Enabled := (iCount > 0) And (iIndex >= 0);
  btnUp.Enabled := (iCount > 0) And (iIndex > 0);
  btnDown.Enabled := (iCount > 0) And (iIndex >= 0) And (iIndex < iCount - 1);
  btnEdit.Enabled := (iCount > 0) And (iIndex >= 0);
End;

Procedure TdlgDataElements.RefreshListView;
Var
  oElement: TDataElement;
  oItem: TListItem;
  sName: String;
  iIndex: Integer;
Begin
  // Do we already have a selected item?
  If lvDataElements.ItemIndex >= 0 Then
    sName := lvDataElements.Selected.Caption
  Else
    sName := '';

  lvDataElements.BeginUpdate;
  lvDataElements.Items.BeginUpdate;
  lvDataElements.Clear;
  Try
    For oElement In FDataElements Do
    Begin
      oItem := lvDataElements.Items.Add;
      SetListItem(oItem, oElement);
    End;

  Finally
    lvDataElements.Items.EndUpdate;
    lvDataElements.EndUpdate;
  End;

  If lvDataElements.Items.Count > 0 Then
  Begin
    If sName = '' Then
      lvDataElements.ItemIndex := 0
    Else
    Begin
      iIndex := FDataElements.IndexOf(sName);

      If iIndex = -1 Then
        lvDataElements.ItemIndex := 0
      Else
        lvDataElements.ItemIndex := iIndex;
    End;
  End;

  RefreshToolbar;
End;

Procedure TdlgDataElements.btnAddClick(Sender: TObject);
Var
  oDataElement: TDataElement;
  oDlg: TdlgDataElement;
Begin
  oDlg := TdlgDataElement.Create(Self);
  oDlg.AvailableFields(FAvailableFields);
  Try
    If oDlg.ShowModal = mrOk Then
    Begin
      oDataElement := TDataElement.Create;
      oDataElement.AsXML := oDlg.AsXML;
      FDataElements.Add(oDataElement);

      RefreshListView;
    End;
  Finally
    oDlg.Free;
  End;
End;

Procedure TdlgDataElements.btnDeleteClick(Sender: TObject);
Var
  i: Integer;
  oDataElement: TDataElement;
Begin
  i := lvDataElements.ItemIndex;
  If i <> -1 Then
  Begin
    oDataElement := FDataElements[i];
    If MessageDlg('Are you sure you wish to delete ' + oDataElement.Name +
      '?', mtConfirmation, [mbYes, mbNo], 0) = mrYes Then
    Begin
      FDataElements.Delete(i);
      lvDataElements.Items.Delete(i);
    End;
  End;

  RefreshToolbar;
End;

Procedure TdlgDataElements.btnDownClick(Sender: TObject);
Var
  iIndex: Integer;
Begin
  iIndex := lvDataElements.ItemIndex;

  If (iIndex >= 0) And (iIndex < FDataElements.Count - 1) Then
  Begin
    FDataElements.Move(iIndex, iIndex + 1);
    lvDataElements.Items.Move(iIndex, iIndex + 1);
    lvDataElements.ItemIndex := iIndex + 1;
  End;

  RefreshToolbar;
End;

Procedure TdlgDataElements.SetListItem(AListItem: TListItem; ADataElement: TDataElement);
Begin
  AListItem.Caption := ADataElement.Name;
  AListItem.SubItems.Clear;
  AListItem.SubItems.Add(ADataElement.Prefix);
  AListItem.SubItems.Add(ADataElement.Suffix);
  AListItem.SubItems.Add(ElementTypeToStr(ADataElement.ElementType));
  AListItem.SubItems.Add(ADataElement.Value);
End;

Procedure TdlgDataElements.btnEditClick(Sender: TObject);
Var
  oDataElement: TDataElement;
  oDlg: TdlgDataElement;
  i: Integer;
  oItem: TListItem;
Begin
  i := lvDataElements.ItemIndex;
  oDataElement := FDataElements[i];
  If i <> -1 Then
  Begin
    oDlg := TdlgDataElement.Create(Self);
    oDlg.AvailableFields(FAvailableFields);
    oDlg.AsXML := oDataElement.AsXML;
    Try
      If oDlg.ShowModal = mrOk Then
      Begin
        oDataElement.AsXML := oDlg.AsXML;

        oItem := lvDataElements.Items[i];
        SetListItem(oItem, oDataElement);

        RefreshToolbar;
      End;
    Finally
      oDlg.Free;
    End;
  End;
End;

Procedure TdlgDataElements.btnUpClick(Sender: TObject);
Var
  iIndex: Integer;
Begin
  iIndex := lvDataElements.ItemIndex;

  If (iIndex > 0) And (iIndex < FDataElements.Count) Then
  Begin
    FDataElements.Move(iIndex, iIndex - 1);
    lvDataElements.Items.Move(iIndex, iIndex - 1);
    lvDataElements.ItemIndex := iIndex - 1;
  End;

  RefreshToolbar;
End;

Procedure TdlgDataElements.lvDataElementsChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
Begin
  RefreshToolbar;
End;

Procedure TdlgDataElements.lvDataElementsDblClick(Sender: TObject);
Begin
  If btnEdit.Enabled Then
    btnEdit.Click;
End;

Procedure TdlgDataElements.lvDataElementsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
Begin
  RefreshToolbar;
End;

Function TdlgDataElements.GetAsXML: String;
Begin
  FDataElements.Prefix := edtPrefix.Text;
  FDataElements.Suffix := edtSuffix.Text;
  FDataElements.Delimiter := edtDelimiter.Text;

  Result := FDataElements.AsXML;
End;

Procedure TdlgDataElements.SetAsXML(AValue: String);
Begin
  FDataElements.AsXML := AValue;

  edtPrefix.Text := FDataElements.Prefix;
  edtSuffix.Text := FDataElements.Suffix;
  edtDelimiter.Text := FDataElements.Delimiter;

  RefreshListView;
End;

Procedure TdlgDataElements.SetAvailableFields(AValue: TStringArray);
Begin
  If FAvailableFields = AValue Then
    Exit;
  FAvailableFields := AValue;
End;

End.
