{------------------------------------------------------------------------------
  Project   : SerialProcessor
  Unit      : DialogSerialPort (DialogSerialPort.pas)
  Description
    Dialog for editing an individual serial port configuration, including
    LazSerial settings and associated data field mappings.

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
Unit DialogSerialPort;

{$mode ObjFPC}{$H+}
{$codepage utf8}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, SerialPorts;

Type

  { TdlgSerialPort }

  TdlgSerialPort = Class(TForm)
    btnFieldsSetupDialog: TButton;
    Button1: TButton;
    Button2: TButton;
    btnLazSerialSetupDialog: TButton;
    edtName: TEdit;
    edtFields: TEdit;
    edtSerialPort: TMemo;
    lblName: TLabel;
    lblName1: TLabel;
    lblName2: TLabel;
    pnlButtons: TPanel;
    Procedure btnFieldsSetupDialogClick(Sender: TObject);
    Procedure btnLazSerialSetupDialogClick(Sender: TObject);
    Procedure edtNameChange(Sender: TObject);
  Private
    FAvailableFields: TStringArray;
    FSerialPort: TSerialPort;
    Function GetAsXML: String;
    Procedure SetAsXML(AValue: String);

    Procedure RefreshUI;
    Procedure SetAvailableFields(AValue: TStringArray);
  Public
    Constructor Create(TheOwner: TComponent); Override;
    Destructor Destroy; Override;

    Property AvailableFields: TStringArray Read FAvailableFields Write SetAvailableFields;

    Property AsXML: String Read GetAsXML Write SetAsXML;
  End;


Implementation

Uses
  LazSerialSupport;

  {$R *.lfm}

  { TdlgSerialPort }


Constructor TdlgSerialPort.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);

  FSerialPort := TSerialPort.Create;

  AsXML := FSerialPort.AsXML;
End;

Destructor TdlgSerialPort.Destroy;
Begin
  FSerialPort.Free;
  FSerialPort := nil;

  Inherited Destroy;
End;

Procedure TdlgSerialPort.btnLazSerialSetupDialogClick(Sender: TObject);
Begin
  FSerialPort.LazSerial.ShowSetupDialog;

  RefreshUI;
End;

Procedure TdlgSerialPort.edtNameChange(Sender: TObject);
Begin
  FSerialPort.Name := edtName.Text;
End;

Procedure TdlgSerialPort.btnFieldsSetupDialogClick(Sender: TObject);
Begin
  FSerialPort.DataElements.ShowSetupDialog(FAvailableFields);

  RefreshUI;
End;

Function TdlgSerialPort.GetAsXML: String;
Begin
  Result := FSerialPort.AsXML;
End;

Procedure TdlgSerialPort.SetAsXML(AValue: String);
Begin
  FSerialPort.AsXML := AValue;

  RefreshUI;
End;

Procedure TdlgSerialPort.RefreshUI;
Begin
  edtName.Text := FSerialPort.Name;
  edtFields.Text := FSerialPort.DataElements.Fieldnames;
  edtSerialPort.Text := FSerialPort.LazSerial.ToString;
End;

Procedure TdlgSerialPort.SetAvailableFields(AValue: TStringArray);
Begin
  If FAvailableFields = AValue Then
    Exit;

  FAvailableFields := AValue;
End;

End.
