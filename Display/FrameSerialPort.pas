Unit FrameSerialPort;

{$mode objfpc}{$H+}
{$codepage utf8}

Interface

Uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Grids, ExtCtrls, AdvLed,
  SerialPorts, DataElements, fgl;

Type

  { TfmeSerialPort }

  TfmeSerialPort = Class(TFrame)
    edtName: TEdit;
    edtSerial: TEdit;
    edtFields: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ledStatus: TAdvLed;
    grdResults: TStringGrid;
    tmrRefresh: TTimer;
    Procedure tmrRefreshTimer(Sender: TObject);
  Private
    FSerialPort: TSerialPort;
    Procedure SetSerialPort(AValue: TSerialPort);

    Procedure RefreshGrid;
    Procedure RefreshUI;
  Public
    Constructor Create(TheOwner: TComponent); Override;
    Destructor Destroy; Override;

    Procedure DoDataRX;

    Property SerialPort: TSerialPort Read FSerialPort Write SetSerialPort;
  End;

  { TSerialPortFrames }

  TSerialPortFrames = Class(Specialize TFPGObjectList<TfmeSerialPort>)
  Private
    FFrameIndex: Integer;
  Public
    Constructor Create;
    Procedure AddFrame(AControl: TWinControl; ASerialPort: TSerialPort);

    Function FindFrame(ASerialPort: TSerialPort): TfmeSerialPort;

    Procedure Clear;
  End;

Const
  SERIALPORT_FRAME_HEIGHT: Integer = 105;

Function GetSerialPortFrames: TSerialPortFrames;

Implementation

Uses
  Math, LazSerialSupport;

Var
  LSerialPortFrames: TSerialPortFrames;

  {$R *.lfm}

Function GetSerialPortFrames: TSerialPortFrames;
Begin
  If Not Assigned(LSerialPortFrames) Then
    LSerialPortFrames := TSerialPortFrames.Create;

  Result := LSerialPortFrames;
End;

{ TfmeSerialPort }

Constructor TfmeSerialPort.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);

  FSerialPort := nil;

  GetSerialPortFrames.Add(Self);
End;

Destructor TfmeSerialPort.Destroy;
Begin
  GetSerialPortFrames.Remove(Self);

  Inherited Destroy;
End;

Procedure TfmeSerialPort.DoDataRX;
Begin
  If Not Assigned(FSerialPort) Then
    Exit;

  RefreshGrid;
End;

Procedure TfmeSerialPort.tmrRefreshTimer(Sender: TObject);
Var
  i: Integer;
Begin
  If ledStatus.State = lsDisabled Then
    Exit;

  If Assigned(FSerialPort) Then
  Begin
    If Not FSerialPort.DataElements.DataValid Then
    Begin
      ledStatus.State := lsDisabled;

      For i := 0 To grdResults.ColCount - 1 Do
        grdResults.Cells[i, 1] := '';
    End;
  End;
End;

Procedure TfmeSerialPort.SetSerialPort(AValue: TSerialPort);
Begin
  If FSerialPort = AValue Then
    Exit;
  FSerialPort := AValue;

  RefreshGrid;
  RefreshUI;
End;

Procedure TfmeSerialPort.RefreshGrid;
Var
  oElement: TDataElement;
  iCol, iWidth: Integer;
Begin
  If Not Assigned(FSerialPort) Then
  Begin
    grdResults.RowCount := 1;
    ledStatus.State := lsDisabled;
    Exit;
  End;

  grdResults.RowCount := 2;
  grdResults.FixedRows := 1;
  grdResults.ColCount := FSerialPort.DataElements.Count;

  If FSerialPort.DataElements.DataValid Then
  Begin
    If ledStatus.State = lsOn Then
      ledStatus.State := lsOff
    Else
      ledStatus.State := lsOn;
  End
  Else
    ledStatus.State := lsDisabled;

  iCol := 0;
  For oElement In FSerialPort.DataElements Do
  Begin
    grdResults.Cells[iCol, 0] := oElement.Name;
    grdResults.Cells[iCol, 1] := oElement.Value;

    If Assigned(Parent) Then
    Begin
      iWidth := Max(grdResults.Canvas.TextWidth(oElement.Name),
        grdResults.Canvas.TextWidth(oElement.Value)) + 10;
      //iWidth := Max(50, iWidth);
      grdResults.ColWidths[iCol] := iWidth;
    End
    Else
      grdResults.ColWidths[iCol] := 50;

    iCol := iCol + 1;
  End;
End;

Procedure TfmeSerialPort.RefreshUI;
Begin
  If Not Assigned(FSerialPort) Then
    Exit;

  edtName.Text := FSerialPort.Name;
  edtSerial.Text := FSerialPort.LazSerial.SimpleString;
  edtFields.Text := FSerialPort.DataElements.FieldNames;

  If grdResults.ColCount <> FSerialPort.DataElements.Count Then
    RefreshGrid;
End;

{ TSerialPortFrames }

Constructor TSerialPortFrames.Create;
Begin
  Inherited Create(False);

  FFrameIndex := 0;
End;

Procedure TSerialPortFrames.AddFrame(AControl: TWinControl; ASerialPort: TSerialPort);
Var
  fmeSerialPanel: TfmeSerialPort;
Begin
  // TfmeSerialPort automatically adds itself to this list during create
  fmeSerialPanel := TfmeSerialPort.Create(AControl);
  fmeSerialPanel.Name := Format('fmeSerialPanel%d', [FFrameIndex]);
  fmeSerialPanel.Parent := AControl;
  fmeSerialPanel.SerialPort := ASerialPort;

  // Lets ensure this is placed below existing panels before setting align
  fmeSerialPanel.Top := AControl.Top + AControl.Height - fmeSerialPanel.Height;
  fmeSerialPanel.Align := alTop;

  Inc(FFrameIndex);
End;

Function TSerialPortFrames.FindFrame(ASerialPort: TSerialPort): TfmeSerialPort;
Var
  oFrame: TfmeSerialPort;
Begin
  Result := nil;
  For oFrame In Self Do
  Begin
    If oFrame.SerialPort = ASerialPort Then
    Begin
      Result := oFrame;
      Break;
    End;
  End;
End;

Procedure TSerialPortFrames.Clear;
Var
  i: Integer;
  oFrame: TfmeSerialPort;
Begin
  // TfmeSerialPort automatically removed itself from this list during destroy
  For i := Count - 1 Downto 0 Do
  Begin
    oFrame := Items[i];
    FreeAndNil(oFrame);
  End;

  // By now, I expect the list to be empty, but let's be sure...
  Inherited Clear;
End;

Initialization
  LSerialPortFrames := nil;

Finalization
  If Assigned(LSerialPortFrames) Then
  Begin
    LSerialPortFrames.Free;
    LSerialPortFrames := nil;
  End;
End.
