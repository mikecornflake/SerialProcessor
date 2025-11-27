{------------------------------------------------------------------------------
  Project   : SerialProcessor
  Unit      : Logger (Logger.pas)
  Description
    Provides simple file-based logging with optional timestamps for recording
    application activity.

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
Unit Logger;

{$mode ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils;

Type

  { TLogger }

  TLogger = Class
  Private
    FExt: String;
    FLogTime: Boolean;
    FPath, FName: String;

    FStream: TFileStream;
    FCritical: TRTLCriticalSection;
  Public
    Constructor Create(Const AName: String; Const APath: String = '';
      AExt: String = '.log'; ALogTime: Boolean = True);
    Destructor Destroy; Override;

    Procedure NewFile(Const AName: String = '');

    Procedure Log(Const Msg: String);
  End;

Procedure SystemLog(Const AMessage: String);

Implementation

Uses
  StringSupport, Forms, FileSupport;

Var
  LLog: TLogger;
  LIsFinalising: Boolean;

Procedure SystemLog(Const AMessage: String);
Begin
  If Not Assigned(LLog) And (Not LIsFinalising) Then
  Begin
    // Ensure a new log per session
    LLog := TLogger.Create(ChangeFileExt(ExtractFileName(Application.ExeName), ''));
  End;

  If Assigned(LLog) Then
    LLog.Log(AMessage);
End;

Constructor TLogger.Create(Const AName: String; Const APath: String; AExt: String;
  ALogTime: Boolean);
Begin
  InitCriticalSection(FCritical);

  FLogTime := ALogTime;
  FExt := AExt;

  If APath = '' Then
    FPath := IncludeTrailingBackslash(ExtractFileDir(Application.ExeName)) + 'Logs\'
  Else
    FPath := APath;

  ForceDirectories(FPath);

  FName := AName;

  FStream := nil;

  NewFile;
End;

Destructor TLogger.Destroy;
Begin
  If Assigned(FStream) Then
    FreeAndNil(FStream);

  DoneCriticalSection(FCritical);

  Inherited;
End;

Procedure TLogger.NewFile(Const AName: String);
Var
  sFilename: String;
Begin
  If Assigned(FStream) Then
    FreeAndNil(FStream);

  If AName <> '' Then
    FName := AName;

  // Ensure path exists
  ForceDirectories(FPath);

  sFilename := IncludeTrailingBackslash(FPath) + FormatDateTime(
    'yyyy-mm-dd_hh-nn-ss', Now) + ' ' + FName + FExt;

  FStream := TFileStream.Create(sFilename, fmCreate Or fmOpenWrite Or fmShareDenyNone);
End;

Procedure TLogger.Log(Const Msg: String);
Var
  s: String;
Begin
  EnterCriticalSection(FCritical);
  Try
    If Not Assigned(FStream) Then
      NewFile(FName);

    If FLogTime Then
      s := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now) + ' ' + Msg
    Else
      s := Msg;

    s += LineEnding;

    Try
      FStream.WriteBuffer(Pointer(s)^, Length(s));

      // Ensures OS-level commit
      FlushFileStreamToDisk(FStream);
    Except
      On E: Exception Do
      Begin
        // On Exception, start a new file...
        NewFile(FName);

        // Now re-raise the exception with slightly more info
        Raise Exception.CreateFmt('TLogger.Log "%s" failed: %s', [FName, E.Message]);
      End;
    End;
  Finally
    LeaveCriticalSection(FCritical);
  End;
End;

Initialization
  LLog := nil;
  LIsFinalising := False;

Finalization;
  LIsFinalising := True;

  If Assigned(LLog) Then
  Begin
    LLog.Free;
    LLog := nil;
  End;

End.
