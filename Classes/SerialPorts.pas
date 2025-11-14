Unit SerialPorts;

{$mode ObjFPC}{$H+}
{$codepage utf8}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
{$WARN 4105 off : Implicit string type conversion with potential data loss from "$1" to "$2"}
{$WARN 4104 off : Implicit string type conversion from "$1" to "$2"}
Interface

Uses
  Classes, SysUtils, fgl, LazSerial, DataElements, Logger;

Type
  TSerialPort = Class;
  TStringQueue = Class;
  TSerialReadThread = Class;
  TSerialWriteThread = Class;

  TNotifySerial = Procedure(Sender: TSerialPort; ANotification: String) Of Object;

  { TSerialPort }

  TSerialPort = Class(TObject)
  Private
    FSampleRate: Double;
    FStarted: Boolean;
    FDataElements: TDataElements;
    FLazSerial: TLazSerial;
    FLogger: TLogger;
    FName: String;
    FOnSerialNotification: TNotifySerial;
    FReadThread: TSerialReadThread;
    FRxQueue: TStringQueue;
    FTxQueue: TStringQueue;
    FWriteThread: TSerialWriteThread;

    Function GetAsXML: String;
    Procedure SetAsXML(AValue: String);

    Procedure Notify(ANotification: String);
    Procedure SetName(AValue: String);
    Procedure SetSampleRate(AValue: Double);
  Protected
    Procedure LogToFile;
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Procedure ShowLazSerialSetupDialog;
    Procedure ShowDataElementsSetupDialog;

    Function Start: Boolean;
    Function Stop: Boolean;

    // For this to work, each TDataElement needs a SourceElement defined
    Function DoBroadcast: Boolean;
    Function DoProcessInput(Const ALine: String): Boolean;

    Function Active: Boolean;

    Property AsXML: String Read GetAsXML Write SetAsXML;
    Property LazSerial: TLazSerial Read FLazSerial Write FLazSerial;
    Property DataElements: TDataElements Read FDataElements Write FDataElements;

    Property RxQueue: TStringQueue Read FRxQueue;
    Property TxQueue: TStringQueue Read FTxQueue;

    Property Name: String Read FName Write SetName;

    Property SampleRatePerSec: Double Read FSampleRate Write SetSampleRate;

    Property OnSerialNotification: TNotifySerial Read FOnSerialNotification
      Write FOnSerialNotification;
  End;

  { TSerialPorts }

  TSerialPorts = Class(Specialize TFPGObjectList<TSerialPort>)
  Private
    FAvailableFields: TStringArray;
    FOnSerialNotification: TNotifySerial;
    Function GetAsXML: String;
    Procedure SetAsXML(AValue: String);
    Procedure SetAvailableFields(AValue: TStringArray);
  Public
    Function Add(AItem: TSerialPort): Integer;
    Function FindPort(AName: String): TSerialPort;
    Function FindElement(AName: String): TDataElement;
    Function IndexOf(AName: String): Integer;
    Procedure ResolveSources(SourcePorts: TSerialPorts);

    Function Start: Boolean;
    Function Stop: Boolean;

    Procedure ShowSetupDialog;
    Property AsXML: String Read GetAsXML Write SetAsXML;

    Property AvailableFields: TStringArray Read FAvailableFields Write SetAvailableFields;

    Property OnSerialNotification: TNotifySerial Read FOnSerialNotification
      Write FOnSerialNotification;
  End;

  { TStringQueue }

  TStringQueue = Class
  Private
    FLimit: Integer;
    FList: TStringList;
    FCritical: TRTLCriticalSection;
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Procedure Flush;

    Function Count: Integer;

    Procedure Enqueue(Const S: String);
    Function Dequeue(out S: String): Boolean;

    Property Limit: Integer Read FLimit Write FLimit;
  End;

  { TSerialReadThread }

  TSerialReadThread = Class(TThread)
  Private
    FLastRead: TDateTime;
    FLastReadCS: TRTLCriticalSection;
    FOwner: TSerialPort;
    FLineTermination: String;
    Function GetLastRead: TDateTime;
  Protected
    Procedure Execute; Override;
  Public
    Constructor Create(AOwner: TSerialPort; ALineTermination: String = #13);
    Destructor Destroy; Override;

    Property LastRead: TDateTime Read GetLastRead;
  End;

  { TSerialWriteThWrite }

  { TSerialWriteThread }

  TSerialWriteThread = Class(TThread)
  Private
    FLastWrite: TDateTime;
    FLastWriteCS: TRTLCriticalSection;
    FOwner: TSerialPort;
    FLineTermination: String;
    Function GetLastWrite: TDateTime;
  Protected
    Procedure Execute; Override;
  Public
    Constructor Create(AOwner: TSerialPort; ALineTermination: String = #13);
    Destructor Destroy; Override;

    Property LastWrite: TDateTime Read GetLastWrite;
  End;

Const
  ONE_SECOND = 1 / (24 * 60 * 60);

Implementation

Uses
  LazSerialSupport, StringSupport, DOM, XMLRead, XMLSupport,
  Controls, DialogSerialPorts;

  { TSerialPort }

Constructor TSerialPort.Create;
Begin
  FName := 'SerialPort';

  FOnSerialNotification := nil;

  FLazSerial := TLazSerial.Create(nil);
  FLazSerial.SynSer.DeadlockTimeout := 100;

  FDataElements := TDataElements.Create;

  FRxQueue := TStringQueue.Create;
  FTxQueue := TStringQueue.Create;

  FReadThread := nil;
  FWriteThread := nil;

  FLogger := nil;

  FStarted := False;

  FSampleRate := 0.8;
End;

Destructor TSerialPort.Destroy;
Begin
  Stop;

  If FLazSerial.Active Then
    FLazSerial.Close;

  FLazSerial.Free;
  FLazSerial := nil;

  FDataElements.Free;
  FDataElements := nil;

  FRxQueue.Free;
  FRxQueue := nil;

  FTxQueue.Free;
  FTxQueue := nil;

  // Should already have been free'd by Stop, but let's make sure
  If Assigned(FLogger) Then
    FreeAndNil(FLogger);

  Inherited Destroy;
End;

Function TSerialPort.Start: Boolean;
Var
  dtStart: TDateTime;
Begin
  SystemLog(Name + ' Start');
  Notify('Starting ' + Name);
  Result := False;

  Try
    If Not FLazSerial.Active Then
      FLazSerial.Open;
  Except
    On E: Exception Do
    Begin
      If Assigned(FOnSerialNotification) Then
        Notify(E.Message)
      Else
        Raise;
    End;
  End;

  If FLazSerial.Active Then
  Begin
    // Assume the Port has been receiving data before we started

    If Not Assigned(FReadThread) Then
    Begin
      // These will only work with proper UART chips - will fail on Virtual & USB
      FLazSerial.SynSer.Flush;
      FLazSerial.SynSer.Purge;

      // Only way I can think of clearing USB/virtual buffer:
      //   Let FReadThread read data from the buffer from one second, then delete it all
      //     before the rest the architecture here spends time reading it
      //   Not guaranteed to fully clear a buffer :-(
      // http://lkml.iu.edu//hypermail/linux/kernel/0707.3/1776.html

      dtStart := Now;

      While ((Now - dtStart) < 2 * ONE_SECOND) And (FLazSerial.SynSer.CanRead(50)) Do
        FLazSerial.SynSer.RecvString(50);

      If (Now - dtStart) > 0.5 * ONE_SECOND Then
        SystemLog(Name + ': Serial port flushed')
      Else If (Now - dtStart) > 0.9 * ONE_SECOND Then
        SystemLog(Name + ': Serial port partly flushed');

      If Not Assigned(FReadThread) Then
        FReadThread := TSerialReadThread.Create(Self);

      If Not Assigned(FWriteThread) Then
        FWriteThread := TSerialWriteThread.Create(Self);
    End;

    Result := True;
    FStarted := True;

    Notify('Sucessfully started ' + Name);
  End
  Else
    SystemLog(Name + ' failed to start');
End;

Function TSerialPort.Stop: Boolean;

  Function WaitForWithTimeout(Thread: TThread; TimeoutMS: Integer): Boolean;
  Var
    StartTime: QWord;
  Begin
    Result := False;
    StartTime := GetTickCount64;

    While (GetTickCount64 - StartTime) < QWord(TimeoutMS) Do
    Begin
      If Thread.Finished Then
      Begin
        Thread.WaitFor;
        Result := True;
        Exit;
      End;
      Sleep(10);
    End;
  End;

Begin
  SystemLog(Name + ' Stop');
  Notify('Stopping');
  Result := False;
  FStarted := False;

  Try
    If FLazSerial.Active Then
      FLazSerial.Close;
  Except
    On E: Exception Do
    Begin
      If Assigned(FOnSerialNotification) Then
        Notify(E.Message)
      Else
        Raise;
    End;
  End;

  If Assigned(FReadThread) Then
  Begin
    FReadThread.Terminate;

    If Not WaitForWithTimeout(FReadThread, 5000) Then
      SystemLog('Warning: Read thread did not terminate within 5 seconds');

    FreeAndNil(FReadThread);

    FRxQueue.Flush;
  End;

  If Assigned(FWriteThread) Then
  Begin
    FWriteThread.Terminate;

    If Not WaitForWithTimeout(FWriteThread, 5000) Then
      SystemLog('Warning: Write thread did not terminate within 5 seconds');

    FreeAndNil(FWriteThread);

    FTxQueue.Flush;
  End;

  If Assigned(FLogger) Then
    FreeAndNil(FLogger);

  Result := Not FLazSerial.Active;

  If Not FLazSerial.Active Then
    Notify('Sucessfully stopped')
  Else
  Begin
    SystemLog(Name + ' Failed to stop');
    Notify('Failed to stop');
  End;
End;

Function TSerialPort.DoBroadcast: Boolean;
Var
  oElement: TDataElement;
  s: String;
  bValid: Boolean;
Begin
  Result := True;
  Try
    s := '';

    bValid := False;

    For oElement In FDataElements Do
    Begin
      If Assigned(oElement.SourceElement) Then
      Begin
        If oElement.SourceElement.DataValid Then
        Begin
          oElement.Value := oElement.SourceElement.Value;
          oElement.TimeRX := Now;
          s += oElement.SourceElement.Value + FDataElements.Delimiter;
          bValid := True;
        End
        Else
        Begin
          oElement.Value := '';
          s += FDataElements.Delimiter;
        End;
      End;
    End;

    If s <> '' Then
      s := Copy(s, 1, Length(s) - Length(FDataElements.Delimiter));

    s := FDataElements.Prefix + s + FDataElements.Suffix + LineEnding;

    If FLazSerial.Active Then
      FTxQueue.Enqueue(s);

    LogToFile;

    If bValid Then
      FDataElements.TimeRX := Now;

    If Assigned(FOnSerialNotification) Then
      FOnSerialNotification(Self, Name + ': ' + FDataElements.Data);
  Except
    Result := False;
  End;
End;

Function TSerialPort.DoProcessInput(Const ALine: String): Boolean;
Begin
  Result := FDataElements.Process(ALine);
  LogToFile;
End;

Function TSerialPort.Active: Boolean;
Begin
  Result := Assigned(FReadThread) And Assigned(FWriteThread) And FLazSerial.Active;
End;

Function TSerialPort.GetAsXML: String;
Var
  s: String;
Begin
  s := TXMLHelper.SetNodeText('Name', Name);
  s += TXMLHelper.SetXMLFragment('RS232', FLazSerial.SettingsAsXML);
  s += TXMLHelper.SetXMLFragment('Fields', FDataElements.AsXML);

  Result := TXMLHelper.SetXMLFragment('TSerialPort', s);
End;

Procedure TSerialPort.SetAsXML(AValue: String);
Var
  s: String;
Begin
  If Trim(AValue) <> '' Then
  Begin
    s := TXMLHelper.GetNodeText(AValue, 'Name');
    If s <> '' Then
      Name := s
    Else
      Name := 'Data';

    If FLazSerial.Active Then
      FLazSerial.Close;

    s := TXMLHelper.GetXMLFragment(AValue, 'RS232');
    If s <> '' Then
      FLazSerial.SettingsAsXML := s;

    s := TXMLHelper.GetXMLFragment(AValue, 'Fields');
    If s <> '' Then
      FDataElements.AsXML := s;
  End;
End;

Procedure TSerialPort.Notify(ANotification: String);
Begin
  If Assigned(FOnSerialNotification) Then
    FOnSerialNotification(Self, Format('%s(%s): %s', [Self.Name, FLazSerial.Device,
      ANotification]));
End;

Procedure TSerialPort.SetName(AValue: String);
Begin
  If FName = AValue Then
    Exit;
  FName := AValue;
End;

Procedure TSerialPort.SetSampleRate(AValue: Double);
Begin
  If FSampleRate = AValue Then
    Exit;

  If AValue < 0.5 Then
  Begin
    FSampleRate := 0.5;

    SystemLog(Format('%s: User tried to set sample rate to %0.1f which is below minimum allowable of 0.5',
      [FName, AValue]));
  End
  Else
    FSampleRate := AValue;
End;

Procedure TSerialPort.LogToFile;
Begin
  If Not FStarted Then
    Exit;

  If Not Assigned(FLogger) Then
  Begin
    FLogger := TLogger.Create(FName, '', '.csv', False);
    FLogger.Log('LocalTime,' + FDataElements.FieldNames);
  End;

  FLogger.Log(FormatDateTimeAsISO8601(Now) + ',' + FDataElements.Data);
End;

Procedure TSerialPort.ShowLazSerialSetupDialog;
Begin
  FLazSerial.ShowSetupDialog;
End;

Procedure TSerialPort.ShowDataElementsSetupDialog;
Begin
  FDataElements.ShowSetupDialog;
End;

{ TSerialPorts }

Procedure TSerialPorts.ResolveSources(SourcePorts: TSerialPorts);
Var
  oPort: TSerialPort;
  oElement: TDataElement;
Begin
  For oPort In Self Do
    For oElement In oPort.DataElements Do
      oElement.SourceElement := SourcePorts.FindElement(oElement.Name);
End;

Function TSerialPorts.GetAsXML: String;
Var
  oSerialPort: TSerialPort;
  s: String;
Begin
  s := '';
  For oSerialPort In Self Do
    s += oSerialPort.AsXML;

  Result := TXMLHelper.SetXMLFragment('TSerialPorts', s);
End;

Procedure TSerialPorts.SetAsXML(AValue: String);
Var
  oDoc: TXMLDocument;
  oNode: TDOMNode;
  oSerialPort: TSerialPort;
  oStream: TStringStream;
Begin
  Clear;
  If Trim(AValue) <> '' Then
  Begin
    oStream := TStringStream.Create(AValue);
    Try
      ReadXMLFile(oDoc, oStream);
      Try
        oNode := oDoc.DocumentElement.FirstChild;
        While Assigned(oNode) Do
        Begin
          If oNode.NodeName = 'TSerialPort' Then
          Begin
            oSerialPort := TSerialPort.Create;
            Try
              oSerialPort.AsXML := TXMLHelper.NodeToXML(oNode);
              Add(oSerialPort);
            Except
              On E: Exception Do
                If Assigned(FOnSerialNotification) Then
                  FOnSerialNotification(oSerialPort, E.Message);
            End;
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
End;

Procedure TSerialPorts.SetAvailableFields(AValue: TStringArray);
Begin
  If FAvailableFields = AValue Then
    Exit;

  FAvailableFields := AValue;
End;

Function TSerialPorts.Add(AItem: TSerialPort): Integer;
Begin
  If IndexOf(AItem.Name) <> -1 Then
    Raise Exception.Create('Duplicate serial port name: ' + AItem.Name);

  If Assigned(FOnSerialNotification) Then
    AItem.OnSerialNotification := FOnSerialNotification;

  Result := Inherited Add(AItem);
End;

Function TSerialPorts.FindPort(AName: String): TSerialPort;
Var
  oSerialPort: TSerialPort;
Begin
  Result := nil;

  For oSerialPort In self Do
    If AnsiCompareText(AName, oSerialPort.Name) = 0 Then
    Begin
      Result := oSerialPort;
      Break;
    End;
End;

Function TSerialPorts.FindElement(AName: String): TDataElement;
Var
  iSplit, iLen: Integer;
  oSerial: TSerialPort;
  sPort, sElement: String;
Begin
  Result := nil;

  iSplit := Pos('.', AName);
  iLen := Length(AName);

  sPort := '';
  sElement := '';

  If iSplit > 0 Then
  Begin
    sPort := Copy(AName, 1, iSplit - 1);
    sElement := Copy(AName, iSplit + 1, iLen - iSplit);

    oSerial := FindPort(sPort);

    If Assigned(oSerial) Then
      Result := oSerial.DataElements.Find(sElement);
  End;

  If Not Assigned(oSerial) And (sPort = '') Then
  Begin
    //  Oh, we failed because the ELementName wasn't defined as 'Port.Element'
    // we're just going to have to search all serial ports and return the
    // first matching field.
    For oSerial In Self Do
    Begin
      Result := oSerial.DataElements.Find(AName);
      If Assigned(Result) Then
        Break;
    End;
  End;
End;

Function TSerialPorts.IndexOf(AName: String): Integer;
Var
  i: Integer;
  oSerialPort: TSerialPort;
Begin
  Result := -1;

  For i := 0 To Count - 1 Do
  Begin
    oSerialPort := TSerialPort(Items[i]);
    If AnsiCompareText(AName, oSerialPort.Name) = 0 Then
    Begin
      Result := i;
      Break;
    End;
  End;
End;

Function TSerialPorts.Start: Boolean;
Var
  oPort: TSerialPort;
Begin
  Result := True;

  For oPort In Self Do
    Result := Result And oPort.Start;
End;

Function TSerialPorts.Stop: Boolean;
Var
  oPort: TSerialPort;
Begin
  Result := True;

  For oPort In Self Do
    Result := Result And oPort.Stop;
End;

Procedure TSerialPorts.ShowSetupDialog;
Var
  oDlg: TdlgSerialPorts;
Begin
  oDlg := TdlgSerialPorts.Create(nil);
  oDlg.AvailableFields := FAvailableFields;
  oDlg.AsXML := Self.AsXML;
  Try
    If oDlg.ShowModal = mrOk Then
      Self.AsXML := oDlg.AsXML;
  Finally
    oDlg.Free;
  End;
End;

{ TStringQueue }

Constructor TStringQueue.Create;
Begin
  Inherited Create;

  InitCriticalSection(FCritical);
  FList := TStringList.Create;

  // Operationally, I would expect the String Queue's to be no more than 2 lines deep
  // Anything larger and we should be discarding older, stale, values
  // However, let's set the queue size to 10 for safety
  FLimit := 10;
End;

Destructor TStringQueue.Destroy;
Begin
  FList.Free;
  FList := nil;

  DoneCriticalSection(FCritical);
  Inherited Destroy;
End;

Procedure TStringQueue.Flush;
Begin
  EnterCriticalSection(FCritical);
  Try
    FList.Clear;
  Finally
    LeaveCriticalSection(FCritical);
  End;
End;

Function TStringQueue.Count: Integer;
Begin
  EnterCriticalSection(FCritical);
  Try
    Result := FList.Count;
  Finally
    LeaveCriticalSection(FCritical);
  End;
End;

Procedure TStringQueue.Enqueue(Const S: String);
Begin
  EnterCriticalSection(FCritical);
  Try
    FList.Add(S);

    If FList.Count > FLimit Then
      FList.Delete(0);
  Finally
    LeaveCriticalSection(FCritical);
  End;
End;

Function TStringQueue.Dequeue(out S: String): Boolean;
Begin
  Result := False;
  EnterCriticalSection(FCritical);
  Try
    If FList.Count > 0 Then
    Begin
      S := FList[0];
      FList.Delete(0);
      Result := True;
    End;
  Finally
    LeaveCriticalSection(FCritical);
  End;
End;

{ TSerialReadThread }

Constructor TSerialReadThread.Create(AOwner: TSerialPort; ALineTermination: String);
Begin
  Inherited Create(False);

  FreeOnTerminate := False;
  FOwner := AOwner;
  FLineTermination := ALineTermination;

  InitCriticalSection(FLastReadCS);
End;

Destructor TSerialReadThread.Destroy;
Begin
  DoneCriticalSection(FLastReadCS);

  Inherited Destroy;
End;

Function TSerialReadThread.GetLastRead: TDateTime;
Begin
  EnterCriticalSection(FLastReadCS);
  Try
    Result := FLastRead;
  Finally
    LeaveCriticalSection(FLastReadCS);
  End;
End;

Procedure TSerialReadThread.Execute;
Var
  S: String;
  dtSampleTime: TDateTime;
  iSkipped: Integer;
  iEmptyReads: Integer;

  Function CanSample: Boolean;
  Begin
    Result := (Now - dtSampleTime) >= (FOwner.SampleRatePerSec * ONE_SECOND);
  End;

Begin
  iSkipped := 0;
  iEmptyReads := 0;

  // Ensure we start sampling right away
  dtSampleTime := Now - 2 * (FOwner.SampleRatePerSec * ONE_SECOND);

  // Thread...
  While Not Terminated Do
  Begin
    If (FOwner.LazSerial.SynSer.CanRead(50)) Then
    Begin
      Try
        S := FOwner.LazSerial.SynSer.RecvString(50);
      Except
        On E: Exception Do
        Begin
          SystemLog('RecvString exception: ' + E.Message);
          Sleep(100);
          Continue;
        End;
      End;

      If s = '' Then
        Inc(iEmptyReads);

      // We'll hve to monitor this limit after the next operational sesssion
      If iEmptyReads > 5 Then
      Begin
        SystemLog(Format('%s: %d empty RecvString results after CanRead=true',
          [FOwner.Name, iEmptyReads]));

        iEmptyReads := 0;
      End;

      If CanSample And (S <> '') Then
      Begin
        // Add to processing queue
        FOwner.RxQueue.Enqueue(S);

        // Can't use FLastRead as that has wider implications
        dtSampleTime := Now;

        If iSkipped > 0 Then
        Begin
          SystemLog(Format('%s: Skipped %d input lines at sample rate %0.1f samples per second.',
            [FOwner.Name, iSkipped, FOwner.SampleRatePerSec]));
          iSkipped := 0;
        End;

        // Allow the rest of the app to monitor data "staleness"
        EnterCriticalSection(FLastReadCS);
        FLastRead := Now;
        LeaveCriticalSection(FLastReadCS);
      End
      Else
      Begin
        Inc(iSkipped);
        Sleep(10);
      End;
    End
    Else
      Sleep(50);
  End;
End;

{ TSerialWriteThread }

Constructor TSerialWriteThread.Create(AOwner: TSerialPort; ALineTermination: String);
Begin
  Inherited Create(False);

  FreeOnTerminate := False;
  FOwner := AOwner;
  FLineTermination := ALineTermination;

  InitCriticalSection(FLastWriteCS);
End;

Destructor TSerialWriteThread.Destroy;
Begin
  DoneCriticalSection(FLastWriteCS);

  Inherited Destroy;
End;

Function TSerialWriteThread.GetLastWrite: TDateTime;
Begin
  EnterCriticalSection(FLastWriteCS);
  Try
    Result := FLastWrite;
  Finally
    LeaveCriticalSection(FLastWriteCS);
  End;
End;

Procedure TSerialWriteThread.Execute;
Var
  S: String;
Begin
  S := '';

  While Not Terminated Do
  Begin
    If (FOwner.TxQueue.Count > 0) And FOwner.Active Then
    Begin
      If FOwner.TxQueue.Dequeue(S) Then
      Begin
        If FOwner.LazSerial.SynSer.CanWrite(0) Then
        Begin
          Try
            FOwner.LazSerial.SynSer.SendString(S);

            EnterCriticalSection(FLastWriteCS);
            FLastWrite := Now;
            LeaveCriticalSection(FLastWriteCS);
          Except
            on E: Exception Do
              SystemLog(FOwner.Name + ': Send failed: ' + E.Message);
          End;
        End
        Else
          SystemLog(FOwner.Name + ': Port not writable — skipping send');
      End;
    End;
    Sleep(5);
  End;
End;

End.
