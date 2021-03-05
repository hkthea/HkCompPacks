unit HKSelfUpdater;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs;

type
  THKUpdaterProtocol=(UPHTTP, UPHTTPS, UPFTP);

  TOnUpdateFound=procedure(aSender:TObject; aChecksum, aVersion:String;
    var DoDownload:Boolean)of object;

  { THKSelfUpdater }

  THKSelfUpdater = class(TComponent)
  private
    FArchiveFolder: String;
    FAuthToken: String;
    FAuthTokenHeaderName: String;
    FHost: String;
    FOnUpdateFound: TOnUpdateFound;
    FPassword: String;
    FPath: String;
    FPort: Integer;
    FProtocol: THKUpdaterProtocol;
    FUsername: String;
    FVersion: String;
    procedure SetArchiveFolder(AValue: String);
    procedure SetAuthToken(AValue: String);
    procedure SetAuthTokenHeaderName(AValue: String);
    procedure SetHost(AValue: String);
    procedure SetOnUpdateFound(AValue: TOnUpdateFound);
    procedure SetPassword(AValue: String);
    procedure SetPath(AValue: String);
    procedure SetPort(AValue: Integer);
    procedure SetProtocol(AValue: THKUpdaterProtocol);
    procedure SetUsername(AValue: String);
  protected
    procedure DoFoundUpdate(aChecksum, aVersion:String);
    procedure DoDownload;
    procedure HTTPCheckUpdate;
    procedure FTPCheckUpdate;
    Function CompareVersion(CloudVersion:String):Boolean;
    Procedure CheckVersion;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CheckUpdate;

  published
    property Host:String read FHost write SetHost;
    property Port:Integer read FPort write SetPort;
    property Protocol:THKUpdaterProtocol read FProtocol write SetProtocol default UPHTTP;
    property Path:String read FPath write SetPath;
    property Username:String read FUsername write SetUsername;
    property Password:String read FPassword write SetPassword;
    property AuthToken:String read FAuthToken write SetAuthToken;
    property AuthTokenHeaderName:String read FAuthTokenHeaderName write SetAuthTokenHeaderName;
    property OnUpdateFound:TOnUpdateFound read FOnUpdateFound write SetOnUpdateFound;
    property ArchiveFolder:String read FArchiveFolder write SetArchiveFolder;
    property AppVersion:String read FVersion;
  end;

procedure Register;

implementation

uses FileInfo;

procedure Register;
begin
  {$I hkselfupdater_icon.lrs}
  RegisterComponents('HKCompPacks',[THKSelfUpdater]);
end;

{ THKSelfUpdater }

procedure THKSelfUpdater.SetAuthToken(AValue: String);
begin
  if FAuthToken=AValue then Exit;
  FAuthToken:=AValue;
end;

procedure THKSelfUpdater.SetArchiveFolder(AValue: String);
begin
  if FArchiveFolder=AValue then Exit;
  FArchiveFolder:=AValue;
end;

procedure THKSelfUpdater.SetAuthTokenHeaderName(AValue: String);
begin
  if FAuthTokenHeaderName=AValue then Exit;
  FAuthTokenHeaderName:=AValue;
end;

procedure THKSelfUpdater.SetHost(AValue: String);
begin
  if FHost=AValue then Exit;
  FHost:=AValue;
end;

procedure THKSelfUpdater.SetOnUpdateFound(AValue: TOnUpdateFound);
begin
  if FOnUpdateFound=AValue then Exit;
  FOnUpdateFound:=AValue;
end;

procedure THKSelfUpdater.SetPassword(AValue: String);
begin
  if FPassword=AValue then Exit;
  FPassword:=AValue;
end;

procedure THKSelfUpdater.SetPath(AValue: String);
begin
  if FPath=AValue then Exit;
  FPath:=AValue;
end;

procedure THKSelfUpdater.SetPort(AValue: Integer);
begin
  if FPort=AValue then Exit;
  FPort:=AValue;
end;

procedure THKSelfUpdater.SetProtocol(AValue: THKUpdaterProtocol);
begin
  if FProtocol=AValue then Exit;
  FProtocol:=AValue;
end;

procedure THKSelfUpdater.SetUsername(AValue: String);
begin
  if FUsername=AValue then Exit;
  FUsername:=AValue;
end;

procedure THKSelfUpdater.DoFoundUpdate(aChecksum, aVersion: String);
var
  doContinue: Boolean;
begin
  doContinue:=False;
  if Assigned(OnUpdateFound) then OnUpdateFound(Self, aChecksum, aVersion, doContinue);
  if doContinue then DoDownload;
end;

procedure THKSelfUpdater.DoDownload;
begin

end;

procedure THKSelfUpdater.HTTPCheckUpdate;
begin

end;

procedure THKSelfUpdater.FTPCheckUpdate;
begin

end;

function THKSelfUpdater.CompareVersion(CloudVersion: String): Boolean;
begin

end;

procedure THKSelfUpdater.CheckVersion;
var ver:TFileVersionInfo;
begin
  ver:=TFileVersionInfo.Create(Self);
  with ver do
  begin
    try
      ReadFileInfo;
      FVersion:=VersionStrings.Values['FileVersion'];
    finally
      Free;
    end;
  end;
end;

constructor THKSelfUpdater.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Host:='localhost';
  Port:=80;
  Path:='/';
end;

procedure THKSelfUpdater.CheckUpdate;
begin
  case Protocol of
    UPHTTPS,
    UPHTTP:HTTPCheckUpdate;
    UPFTP:FTPCheckUpdate;
  end;
end;

end.
