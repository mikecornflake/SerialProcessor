Program SerialProcessor;

{$mode ObjFPC}{$H+}

Uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  SysUtils,
  LazSerialPort,
  FormMain, pl_win_directx;

  {$R *.res}

Begin
  // If the following line is uncommented then all memory leaks are written to file
  // If the following line is commented then leaks are reported immediately in the IDE
  SetHeapTraceOutput(ChangeFileExt(Application.Exename, '.trc'));

  RequireDerivedFormResource := True;
  Application.Title:='Serial Port Processor';
  Application.Scaled:=True;
  {$PUSH}
  {$WARN 5044 OFF}
  Application.MainFormOnTaskbar := True;
  {$POP}
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
End.
