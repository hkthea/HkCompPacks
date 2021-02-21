unit HKConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, FileInfo;

type

  { THKConfig }

  THKConfig=class
  private
    FBaseURL: String;
    fIni:TIniFile;
    fFileInfo:TStrings;
    procedure SetBaseURL(AValue: String);
  protected
    procedure LoadConfig;virtual;
  public
    constructor Create;
    destructor Destroy; override;
    property FileInfo:TStrings read fFileInfo;
    property BaseURL:String read FBaseURL write SetBaseURL;
  end;

implementation

{ THKConfig }

procedure THKConfig.SetBaseURL(AValue: String);
begin
  if FBaseURL=AValue then Exit;
  FBaseURL:=AValue;
  fIni.WriteString('Server','BaseUrl', AValue);
end;

procedure THKConfig.LoadConfig;
begin
  FBaseURL:=fIni.ReadString('Server', 'BaseUrl', 'http://localhost:11611/');
end;

constructor THKConfig.Create;
var fInfo:TFileVersionInfo;
begin
  fIni:=TIniFile.Create(ExtractFileDir(ParamStr(1))+PathDelim+'setting.ieu');
  fFileInfo:=TStringList.Create;
  fInfo:=TFileVersionInfo.Create(Nil);
  try
    fInfo.ReadFileInfo;
    fFileInfo.Assign(fInfo.VersionStrings);
  finally
    fInfo.Free;
  end;
  LoadConfig;
end;

destructor THKConfig.Destroy;
begin
  fIni.Free;
  fFileInfo.Free;
  inherited Destroy;
end;

end.

