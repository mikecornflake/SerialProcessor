{------------------------------------------------------------------------------
  Project   : SerialProcessor
  Unit      : SerialProcessor (SerialProcessor.lpr)
  Description
    Application entry point that initializes the Lazarus interfaces and loads
    the main form for managing serial port processing.

  Source
    Original work by Mike Thompson (mike.cornflake@gmail.com)

  Dates
    Created        : 2025-11-14  // TODO: confirm from SourceForge
    Added to Git   : 2025-11-14

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
