unit HKLogFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs, LResources, Forms, Controls, Graphics, Dialogs,
  HKCompPacksTypes;

type

  { THKLogFile }

  THKLogFile = class(TComponent)
  private
    FAbsPath: String;
    FDirPath: String;
    FLogExtension: String;
    FLogFormat: String;
    FCSession:TCriticalSection;
    FLogLevel: Integer;
    procedure SetDirPath(AValue: String);
    procedure SetLogExtension(AValue: String);
    procedure SetLogFormat(AValue: String);
    procedure SetLogLevel(AValue: Integer);

  protected

  public
    constructor Create(AOwner: TComponent); override;
    procedure WriteLog(aMessage:String; aDebugType:TDebugType);
    Procedure InitCs;
    destructor Destroy; override;
  published
    property DirPath:String read FDirPath write SetDirPath;
    //property AbsPath:String read FAbsPath;
    property LogExtension:String read FLogExtension write SetLogExtension;
    property LogFormat:String read FLogFormat write SetLogFormat;
    property LogLevel:Integer read FLogLevel write SetLogLevel;
  end;

procedure Register;

implementation


procedure Register;
begin
  {$I hklogfile_icon.lrs}
  RegisterComponents('HKCompPacks',[THKLogFile]);
end;

{ THKLogFile }

procedure THKLogFile.SetDirPath(AValue: String);
var
  path: String;
begin
  if FDirPath=AValue then Exit;
  FDirPath:=AValue;
  FAbsPath:=ExtractFileDir(ParamStr(1))+PathDelim+AValue+PathDelim;
  if Not DirectoryExists(FAbsPath) then MkDir(FAbsPath);
end;

procedure THKLogFile.SetLogExtension(AValue: String);
begin
  if FLogExtension=AValue then Exit;
  FLogExtension:=AValue;
end;

procedure THKLogFile.SetLogFormat(AValue: String);
begin
  if FLogFormat=AValue then Exit;
  FLogFormat:=AValue;
end;

procedure THKLogFile.SetLogLevel(AValue: Integer);
begin
  if FLogLevel=AValue then Exit;
  FLogLevel:=AValue;
end;

constructor THKLogFile.Create(AOwner: TComponent);
begin
  LogLevel:=1;
  LogFormat:='YYYYMMDD';
  DirPath:='log';
  LogExtension:='log';
  FAbsPath:=ExtractFileDir(ParamStr(0))+PathDelim+DirPath+PathDelim;
  inherited Create(AOwner);
end;

procedure THKLogFile.WriteLog(aMessage: String; aDebugType: TDebugType);
var Log:Text;
    logFile, message:String;
begin
  if Not DirectoryExists(FAbsPath) then MkDir(FAbsPath);

  if ((aDebugType=DTInfo2)and(LogLevel<2))or((aDebugType=DTInfo3)and(LogLevel<3)) then
  begin
    Exit;
  end;

  FCSession.Enter;
  try
    logFile:=FAbsPath+'Log_'+FormatDateTime(LogFormat, Now)+ExtensionSeparator+LogExtension;

    message:=GetLogMessage(aMessage, aDebugType);

    AssignFile(Log, logFile);
    if FileExists(logFile)then Append(Log)
    else Rewrite(Log);

    WriteLn(Log, message);
    Flush(Log);
  finally
    CloseFile(Log);
    FCSession.Leave;
  end;
end;

procedure THKLogFile.InitCs;
begin
  FCSession:=TCriticalSection.Create;
end;

destructor THKLogFile.Destroy;
begin
  if FCSession<>nil then FCSession.Free;
  inherited Destroy;
end;

end.
