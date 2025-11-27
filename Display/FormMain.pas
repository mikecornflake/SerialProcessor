{------------------------------------------------------------------------------
  Project   : SerialProcessor
  Unit      : FormMain (FormMain.pas)
  Description
    Main application form orchestrating serial port connections, logging, and
    user interactions for configuring data elements and viewing activity.

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
Unit FormMain;

{$mode objfpc}{$H+}
{$codepage utf8}
{$WARN 5024 off : Parameter "$1" not used}
Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, LazSerial, SerialPorts, DataElements, FrameSerialPort;

Type

  { TfrmMain }

  TfrmMain = Class(TForm)
    ilMain: TImageList;
    pnlLogHeader: TPanel;
    memMain: TMemo;
    pnlInputHeader: TPanel;
    pnlOutputHeader: TPanel;
    pnlSerialPorts: TPanel;
    tmrInputs: TTimer;
    tmrOutputs: TTimer;
    tbMain: TToolBar;
    btnInputs: TToolButton;
    btnOutputs: TToolButton;
    btnStart: TToolButton;
    btnStop: TToolButton;
    ToolButton1: TToolButton;
    btnAbout: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    Procedure btnAboutClick(Sender: TObject);
    Procedure btnInputsClick(Sender: TObject);
    Procedure btnOutputsClick(Sender: TObject);
    Procedure btnStartClick(Sender: TObject);
    Procedure btnStopClick(Sender: TObject);
    Procedure FormActivate(Sender: TObject);
    Procedure tmrInputsTimer(Sender: TObject);

    Procedure DoSerialNotification(Sender: TSerialPort; ANotification: String);
    Procedure tmrOutputsTimer(Sender: TObject);
  Private
    FInputs: TSerialPorts;
    FOutputs: TSerialPorts;
    FSettingsFile: String;
    FActivated: Boolean;
    FProcessing: Boolean;

    Procedure SaveSettings;
    Procedure LoadSettings;

    Procedure LogToUI(AText: String);

    Procedure RefreshUI;
    Procedure CreateSerialPanels;
    Procedure DestroySerialPanels;
  Public
    Constructor Create(TheOwner: TComponent); Override;
    Destructor Destroy; Override;
  End;

Var
  frmMain: TfrmMain;

Implementation

Uses
  StringSupport, XMLSupport, OSSupport, Logger, FormAbout;

  {$R *.lfm}

  { TfrmMain }

Constructor TfrmMain.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);

  FInputs := TSerialPorts.Create;
  FInputs.OnSerialNotification := @DoSerialNotification;

  FOutputs := TSerialPorts.Create;
  FOutputs.OnSerialNotification := @DoSerialNotification;

  FSettingsFile := ChangeFileExt(Application.ExeName, '.xml');

  FActivated := False;
  FProcessing := False;
End;

Destructor TfrmMain.Destroy;
Begin
  SaveSettings;

  FInputs.Free;
  FInputs := nil;

  FOutputs.Free;
  FOutputs := nil;

  Inherited Destroy;
End;

Procedure TfrmMain.FormActivate(Sender: TObject);
Begin
  If Not FActivated Then
  Begin
    FActivated := True;

    LoadSettings;
    RefreshUI;
  End;
End;

Procedure TfrmMain.btnInputsClick(Sender: TObject);
Begin
  LogToUI('Configuring Inputs');

  FInputs.ShowSetupDialog;
End;

Procedure TfrmMain.btnAboutClick(Sender: TObject);
Begin
  ShowAbout;
End;

Procedure TfrmMain.btnOutputsClick(Sender: TObject);
Var
  arrFields: TStringArray;
  oSerial: TSerialPort;
  oElement: TDataElement;
Begin
  LogToUI('Configuring Outputs');

  arrFields := nil;
  SetLength(arrFields, 0);

  For oSerial In FInputs Do
    For oElement In oSerial.DataElements Do
      AddStringToArray(arrFields, oSerial.Name + '.' + oElement.Name);

  FOutputs.AvailableFields := arrFields;

  FOutputs.ShowSetupDialog;
End;

Procedure TfrmMain.btnStartClick(Sender: TObject);
Begin
  If Not tmrInputs.Enabled Then
  Begin
    SetBusy;
    Try
      LogToUI('Linking Inputs to Outputs');
      FOutputs.ResolveSources(FInputs);

      Application.ProcessMessages;

      LogToUI('Opening Ports');
      FInputs.Start;
      Application.ProcessMessages;
      FOutputs.Start;
      Application.ProcessMessages;

      LogToUI('Creating Panels');
      CreateSerialPanels;

      LogToUI('Starting Timers');
      tmrInputs.Enabled := True;
      tmrOutputs.Enabled := True;
    Finally
      ClearBusy;
    End;
  End;

  RefreshUI;
End;

Procedure TfrmMain.btnStopClick(Sender: TObject);
Begin
  If tmrInputs.Enabled Then
  Begin
    SetBusy;
    Try
      LogToUI('Stopping Timers');
      tmrInputs.Enabled := False;
      tmrOutputs.Enabled := False;

      LogToUI('Clearing Panels');
      DestroySerialPanels;

      // Open the Com Ports
      LogToUI('Closing Ports');
      FInputs.Stop;
      FOutputs.Stop;
    Finally
      ClearBusy;
    End;
  End;

  RefreshUI;
End;

Procedure TfrmMain.tmrInputsTimer(Sender: TObject);
Var
  oSerial: TSerialPort;
  sLine: String;
  oFrame: TfmeSerialPort;
Begin
  If FProcessing Then
    Exit;

  FProcessing := True;
  Try
    For oSerial In FInputs Do
    Begin
      While oSerial.RxQueue.Dequeue(sLine) Do
      Begin
        sLine := sLine.Trim([' ', #13, #10]);

        oSerial.DoProcessInput(sLine);
        LogToUI(oSerial.Name + ': ' + oSerial.DataElements.Data);

        oFrame := GetSerialPortFrames.FindFrame(oSerial);
        If Assigned(oFrame) Then
          oFrame.DoDataRX;
      End;
    End;
  Finally
    FProcessing := False;
  End;
End;

Procedure TfrmMain.tmrOutputsTimer(Sender: TObject);
Var
  oSerial: TSerialPort;
Begin
  If FProcessing Then
    Exit;

  FProcessing := True;
  Try
    For oSerial In FOutputs Do
      oSerial.DoBroadcast;
  Finally
    FProcessing := False;
  End;
End;

Procedure TfrmMain.DoSerialNotification(Sender: TSerialPort; ANotification: String);
Var
  oFrame: TfmeSerialPort;
Begin
  LogToUI(ANotification);

  If Assigned(Sender) Then
  Begin
    oFrame := GetSerialPortFrames.FindFrame(Sender);
    If Assigned(oFrame) Then
      oFrame.DoDataRX;
  End;
End;

Procedure TfrmMain.SaveSettings;
Var
  sMain, sFile, sDisplay: String;
Begin
  sMain := '';
  sMain += Format('<Inputs>%s</Inputs>', [FInputs.AsXML]);
  sMain += Format('<Outputs>%s</Outputs>', [FOutputs.AsXML]);

  sDisplay := '';
  sDisplay += Format('<Maximised>%s</Maximised>',
    [BOOLEAN_TRUE_FALSE[Application.MainForm.WindowState = wsMaximized]]);

  sDisplay += Format('<Left>%d</Left>', [Application.MainForm.Left]);
  sDisplay += Format('<Top>%d</Top>', [Application.MainForm.Top]);
  sDisplay += Format('<Width>%d</Width>', [Application.MainForm.Width]);
  sDisplay += Format('<Height>%d</Height>', [Application.MainForm.Height]);
  sDisplay := Format('<Display>%s</Display>', [sDisplay]);

  sFile := Format('<TSerialProcessor>%s%s</TSerialProcessor>', [sMain, sDisplay]);

  SaveTextToFile(FSettingsFile, sFile);
End;

Procedure TfrmMain.LoadSettings;
Var
  sSerialProcessor, sFile, sDisplay, sMsg: String;
  iLeft, iTop, iWidth, iHeight: Integer;
Begin
  sFile := LoadTextFromFile(FSettingsFile);

  sSerialProcessor := TXMLHelper.GetXMLFragment(sFile, 'TSerialProcessor');

  If sSerialProcessor <> '' Then
  Begin
    Try
      FInputs.AsXML := TXMLHelper.GetXMLFragment(sSerialProcessor, 'Inputs');
    Except
      FInputs.Clear;
    End;

    Try
      FOutputs.AsXML := TXMLHelper.GetXMLFragment(sSerialProcessor, 'Outputs');
    Except
      FOutputs.Clear;
    End;

    Try
      sDisplay := TXMLHelper.GetXMLFragment(sSerialProcessor, 'Display');

      If sDisplay <> '' Then
      Begin
        iLeft := TXMLHelper.GetNodeInt(sDisplay, 'Left', Application.MainForm.Left);
        iTop := TXMLHelper.GetNodeInt(sDisplay, 'Top', Application.MainForm.Top);
        iWidth := TXMLHelper.GetNodeInt(sDisplay, 'Width', Application.MainForm.Width);
        iHeight := TXMLHelper.GetNodeInt(sDisplay, 'Height', Application.MainForm.Height);

        SetBounds(iLeft, iTop, iWidth, iHeight);
        MakeFullyVisible;

        If TXMLHelper.GetNodeBool(sDisplay, 'Maximised', False) Then
          Application.MainForm.WindowState := wsMaximized
        Else
          Application.MainForm.WindowState := wsNormal;
      End;
    Except
      On E: Exception Do
      Begin
        sMsg := Format('Failed to load XML from %s.  [%s]', [FSettingsFile, E.Message]);
        SystemLog(sMsg);
        LogToUI(sMsg);
      End;
    End;
  End;
End;

Procedure TfrmMain.LogToUI(AText: String);
Begin
  memMain.Lines.BeginUpdate;
  Try
    memMain.Lines.Add(TimeToStr(Now, GDisplayDateTimeFormat) + ' ' + AText);

    If memMain.Lines.Count > 20 Then
      memMain.Lines.Delete(0);
  Finally
    memMain.Lines.EndUpdate;
  End;

  memMain.SelStart := Length(memMain.Text);
  //memMain.Perform(EM_SCROLLCARET, 0, 0);
End;

Procedure TfrmMain.RefreshUI;
Var
  bStarted: Boolean;
Begin
  bStarted := tmrInputs.Enabled;

  btnInputs.Enabled := Not bStarted;
  btnOutputs.Enabled := Not bStarted;

  btnStart.Down := bStarted;
  btnStop.Down := Not bStarted;
End;

Procedure TfrmMain.CreateSerialPanels;
Var
  oFrames: TSerialPortFrames;
  oSerial: TSerialPort;
  iHeight: Integer;
Begin
  oFrames := GetSerialPortFrames;

  If oFrames.Count > 0 Then
    DestroySerialPanels;

  If (FInputs.Count + FOutputs.Count = 0) Then
  Begin
    pnlSerialPorts.Visible := False;
  End
  Else
  Begin
    pnlSerialPorts.Visible := True;

    iHeight := (FInputs.Count + FOutputs.Count) * SERIALPORT_FRAME_HEIGHT;
    If FInputs.Count > 0 Then
      iHeight += pnlInputHeader.Height;

    If FOutputs.Count > 0 Then
      iHeight += pnlOutputHeader.Height;

    pnlSerialPorts.Height := iHeight;
    pnlSerialPorts.Top := tbMain.Height;

    If FInputs.Count > 0 Then
    Begin
      pnlInputHeader.Visible := True;
      pnlInputHeader.Top := 0;

      For oSerial In FInputs Do
        oFrames.AddFrame(pnlSerialPorts, oSerial);
    End;

    If FOutputs.Count > 0 Then
    Begin
      pnlOutputHeader.Visible := True;
      // Ensure this is positioned below all the Input Frames
      // It's alTop, so will snap back to Top after this...
      pnlOutputHeader.Top := pnlSerialPorts.Top + pnlSerialPorts.Height - pnlOutputHeader.Height;

      For oSerial In FOutputs Do
        oFrames.AddFrame(pnlSerialPorts, oSerial);
    End;
  End;
End;

Procedure TfrmMain.DestroySerialPanels;
Begin
  GetSerialPortFrames.Clear;

  pnlSerialPorts.Height := 10;
  pnlSerialPorts.Visible := False;
End;

End.
