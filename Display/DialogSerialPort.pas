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
