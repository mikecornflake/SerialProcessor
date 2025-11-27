{------------------------------------------------------------------------------
  Project   : SerialProcessor
  Unit      : DialogSerialPorts (DialogSerialPorts.pas)
  Description
    Dialog for maintaining the collection of serial ports, including adding,
    reordering, and configuring individual port entries.

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
Unit DialogSerialPorts;

{$mode ObjFPC}{$H+}
{$codepage utf8}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, SerialPorts;

Type

  { TdlgSerialPorts }

  TdlgSerialPorts = Class(TForm)
    btnAdd: TToolButton;
    btnDelete: TToolButton;
    btnDown: TToolButton;
    btnEdit: TToolButton;
    btnUp: TToolButton;
    Button1: TButton;
    Button2: TButton;
    ImageList1: TImageList;
    lvSerialPorts: TListView;
    pnlButtons: TPanel;
    tbMain: TToolBar;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    Procedure btnAddClick(Sender: TObject);
    Procedure btnDeleteClick(Sender: TObject);
    Procedure btnDownClick(Sender: TObject);
    Procedure btnEditClick(Sender: TObject);
    Procedure btnUpClick(Sender: TObject);
    Procedure lvSerialPortsDblClick(Sender: TObject);
  Private
    FAvailableFields: TStringArray;
    FSerialPorts: TSerialPorts;
    Function GetAsXML: String;
    Procedure SetAsXML(AValue: String);

    Procedure RefreshToolbar;
    Procedure RefreshListView;
    Procedure SetAvailableFields(AValue: TStringArray);
  Public
    Constructor Create(TheOwner: TComponent); Override;
    Destructor Destroy; Override;

    Property AvailableFields: TStringArray Read FAvailableFields Write SetAvailableFields;

    Property AsXML: String Read GetAsXML Write SetAsXML;
  End;

Implementation

Uses
  DialogSerialPort;

  {$R *.lfm}

  { TdlgSerialPorts }

Constructor TdlgSerialPorts.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);

  FSerialPorts := TSerialPorts.Create;
End;

Destructor TdlgSerialPorts.Destroy;
Begin
  FSerialPorts.Free;
  FSerialPorts := nil;

  Inherited Destroy;
End;

Procedure TdlgSerialPorts.btnUpClick(Sender: TObject);
Var
  iIndex: Integer;
Begin
  iIndex := lvSerialPorts.ItemIndex;

  If (iIndex > 0) And (iIndex < FSerialPorts.Count) Then
  Begin
    FSerialPorts.Move(iIndex, iIndex - 1);
    lvSerialPorts.Items.Move(iIndex, iIndex - 1);
    lvSerialPorts.ItemIndex := iIndex - 1;
  End;

  RefreshToolbar;
End;

Procedure TdlgSerialPorts.lvSerialPortsDblClick(Sender: TObject);
Begin
  If btnEdit.Enabled Then
    btnEdit.Click;
End;

Procedure TdlgSerialPorts.btnDownClick(Sender: TObject);
Var
  iIndex: Integer;
Begin
  iIndex := lvSerialPorts.ItemIndex;

  If (iIndex >= 0) And (iIndex < FSerialPorts.Count - 1) Then
  Begin
    FSerialPorts.Move(iIndex, iIndex + 1);
    lvSerialPorts.Items.Move(iIndex, iIndex + 1);
    lvSerialPorts.ItemIndex := iIndex + 1;
  End;

  RefreshToolbar;
End;

Procedure TdlgSerialPorts.btnEditClick(Sender: TObject);
Var
  oSerialPort: TSerialPort;
  oDlg: TdlgSerialPort;
  i: Integer;
  oItem: TListItem;
Begin
  i := lvSerialPorts.ItemIndex;
  oSerialPort := FSerialPorts[i];
  If i <> -1 Then
  Begin
    oDlg := TdlgSerialPort.Create(Self);
    oDlg.AvailableFields := FAvailableFields;
    oDlg.AsXML := oSerialPort.AsXML;
    Try
      If oDlg.ShowModal = mrOk Then
      Begin
        oSerialPort.AsXML := oDlg.AsXML;

        oItem := lvSerialPorts.Items[i];
        oItem.Caption := oSerialPort.Name;
        oItem.SubItems.Clear;
        oItem.SubItems.Add(oSerialPort.LazSerial.Device);
        oItem.SubItems.Add(oSerialPort.DataElements.Fieldnames);

        RefreshToolbar;
      End;
    Finally
      oDlg.Free;
    End;
  End;
End;

Procedure TdlgSerialPorts.btnAddClick(Sender: TObject);
Var
  oSerialPort: TSerialPort;
  oDlg: TdlgSerialPort;
Begin
  oDlg := TdlgSerialPort.Create(Self);
  oDlg.AvailableFields := FAvailableFields;
  Try
    If oDlg.ShowModal = mrOk Then
    Begin
      oSerialPort := TSerialPort.Create;
      oSerialPort.AsXML := oDlg.AsXML;
      FSerialPorts.Add(oSerialPort);

      RefreshListView;
    End;
  Finally
    oDlg.Free;
  End;
End;

Procedure TdlgSerialPorts.btnDeleteClick(Sender: TObject);
Var
  i: Integer;
  oSerialPort: TSerialPort;
Begin
  i := lvSerialPorts.ItemIndex;
  If i <> -1 Then
  Begin
    oSerialPort := FSerialPorts[i];
    If MessageDlg('Are you sure you wish to delete ' + oSerialPort.Name +
      '?', mtConfirmation, [mbYes, mbNo], 0) = mrYes Then
    Begin
      FSerialPorts.Delete(i);
      lvSerialPorts.Items.Delete(i);
    End;
  End;

  RefreshToolbar;
End;

Function TdlgSerialPorts.GetAsXML: String;
Begin
  Result := FSerialPorts.AsXML;
End;

Procedure TdlgSerialPorts.SetAsXML(AValue: String);
Begin
  FSerialPorts.AsXML := AValue;
  RefreshListView;
End;

Procedure TdlgSerialPorts.RefreshToolbar;
Var
  iIndex, iCount: Integer;
Begin
  btnAdd.Enabled := Assigned(FSerialPorts);

  iIndex := lvSerialPorts.ItemIndex;
  iCount := lvSerialPorts.Items.Count;

  btnDelete.Enabled := (iCount > 0) And (iIndex >= 0);
  btnUp.Enabled := (iCount > 0) And (iIndex > 0);
  btnDown.Enabled := (iCount > 0) And (iIndex >= 0) And (iIndex < iCount - 1);
  btnEdit.Enabled := (iCount > 0) And (iIndex >= 0);
End;

Procedure TdlgSerialPorts.RefreshListView;
Var
  oSerialPort: TSerialPort;
  oItem: TListItem;
  sName: String;
  iIndex: Integer;
Begin
  // Do we already have a selected item?
  If lvSerialPorts.ItemIndex >= 0 Then
    sName := lvSerialPorts.Selected.Caption
  Else
    sName := '';

  lvSerialPorts.BeginUpdate;
  lvSerialPorts.Items.BeginUpdate;
  lvSerialPorts.Clear;
  Try
    For oSerialPort In FSerialPorts Do
    Begin
      oItem := lvSerialPorts.Items.Add;
      oItem.Caption := oSerialPort.Name;
      oItem.SubItems.Clear;
      oItem.SubItems.Add(oSerialPort.LazSerial.Device);
      oItem.SubItems.Add(oSerialPort.DataElements.Fieldnames);
    End;

  Finally
    lvSerialPorts.Items.EndUpdate;
    lvSerialPorts.EndUpdate;
  End;

  If lvSerialPorts.Items.Count > 0 Then
  Begin
    If sName = '' Then
      lvSerialPorts.ItemIndex := 0
    Else
    Begin
      iIndex := FSerialPorts.IndexOf(sName);

      If iIndex = -1 Then
        lvSerialPorts.ItemIndex := 0
      Else
        lvSerialPorts.ItemIndex := iIndex;
    End;
  End;

  RefreshToolbar;
End;

Procedure TdlgSerialPorts.SetAvailableFields(AValue: TStringArray);
Begin
  If FAvailableFields = AValue Then
    Exit;
  FAvailableFields := AValue;
End;

End.
