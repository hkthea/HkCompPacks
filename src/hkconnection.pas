unit HKConnection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, LResources, Forms, Controls, Graphics, Dialogs,
  Requester, HKCompPacksTypes, HKLogFile;

type

  { THKConnection }

  THKConnection = class(TComponent)
  private
    FBaseUrl: String;
    FLogComponent: THKLogFile;
    FonDebugMsg: TOnDebugMessage;
    FonPrepareData: TNotifyEvent;
    FonProcessRequestFinished: TNotifyEvent;
    FonProcessRequestStart: TNotifyEvent;
    FOnTokenInvalid: TNotifyEvent;
    FToken: String;
    FTokenName: String;
    procedure doDebug(Sender: TObject; aMessage: String; aDebugType: TDebugType
      );
    procedure SetBaseUrl(AValue: String);
    procedure SetLogComponent(AValue: THKLogFile);
    procedure SetonDebugMsg(AValue: TOnDebugMessage);
    procedure SetonPrepareData(AValue: TNotifyEvent);
    procedure SetonProcessRequestFinished(AValue: TNotifyEvent);
    procedure SetonProcessRequestStart(AValue: TNotifyEvent);
    procedure SetOnTokenInvalid(AValue: TNotifyEvent);
    procedure SetToken(AValue: String);
    procedure SetTokenName(AValue: String);
  protected
    //procedure doDebug();
  public
    Function CreateConnection:TJsonRequester;
    Function CreateAsyncConnection():TAsyncJsonRequester;
    Procedure TestConnection(aUrl:String);
  published
    property LogComponent:THKLogFile read FLogComponent write SetLogComponent;
    property BaseUrl:String read FBaseUrl write SetBaseUrl;
    property Token:String read FToken write SetToken;
    property TokenName:String read FTokenName write SetTokenName;
    property onDebugMsg:TOnDebugMessage read FonDebugMsg write SetonDebugMsg;
    property onProcessRequestStart:TNotifyEvent read FonProcessRequestStart write SetonProcessRequestStart;
    property onProcessRequestFinished:TNotifyEvent read FonProcessRequestFinished write SetonProcessRequestFinished;
    property onPrepareData:TNotifyEvent read FonPrepareData write SetonPrepareData;
    property OnTokenInvalid:TNotifyEvent read FOnTokenInvalid write SetOnTokenInvalid;
  end;

procedure Register;

implementation


procedure Register;
begin
  {$I hkconnection_icon.lrs}
  RegisterComponents('HKCompPacks',[THKConnection]);
end;

{ THKConnection }

procedure THKConnection.SetBaseUrl(AValue: String);
begin
  if FBaseUrl=AValue then Exit;
  FBaseUrl:=AValue;
end;

procedure THKConnection.doDebug(Sender: TObject; aMessage: String;
  aDebugType: TDebugType);
begin
  if Assigned(FonDebugMsg)then FonDebugMsg(Sender, aMessage, aDebugType);
  if Assigned(FLogComponent)then FLogComponent.WriteLog(aMessage, aDebugType);
end;

procedure THKConnection.SetLogComponent(AValue: THKLogFile);
begin
  if FLogComponent=AValue then Exit;
  FLogComponent:=AValue;
end;

procedure THKConnection.SetonDebugMsg(AValue: TOnDebugMessage);
begin
  if FonDebugMsg=AValue then Exit;
  FonDebugMsg:=AValue;
end;

procedure THKConnection.SetonPrepareData(AValue: TNotifyEvent);
begin
  if FonPrepareData=AValue then Exit;
  FonPrepareData:=AValue;
end;

procedure THKConnection.SetonProcessRequestFinished(AValue: TNotifyEvent);
begin
  if FonProcessRequestFinished=AValue then Exit;
  FonProcessRequestFinished:=AValue;
end;

procedure THKConnection.SetonProcessRequestStart(AValue: TNotifyEvent);
begin
  if FonProcessRequestStart=AValue then Exit;
  FonProcessRequestStart:=AValue;
end;

procedure THKConnection.SetOnTokenInvalid(AValue: TNotifyEvent);
begin
  if FOnTokenInvalid=AValue then Exit;
  FOnTokenInvalid:=AValue;
end;

procedure THKConnection.SetToken(AValue: String);
begin
  if FToken=AValue then Exit;
  FToken:=AValue;
end;

procedure THKConnection.SetTokenName(AValue: String);
begin
  if FTokenName=AValue then Exit;
  FTokenName:=AValue;
end;

function THKConnection.CreateConnection: TJsonRequester;
begin
  Result:=TJsonRequester.Create(Self);
  Result.BaseUrl:=BaseUrl;
  Result.Token:=Token;
  Result.TokenName:=TokenName;
  Result.onDebugMsg:=@doDebug;
  Result.onResponseTokenInvalid:=OnTokenInvalid;
  Result.OnPrepareData:=onPrepareData;
  Result.OnProcessRequestFinished:=onProcessRequestFinished;
  Result.OnProcessRequestStart:=onProcessRequestStart;
end;

function THKConnection.CreateAsyncConnection(): TAsyncJsonRequester;
begin
  Result:=TAsyncJsonRequester.Create(Self);
  Result.BaseUrl:=BaseUrl;
  Result.Token:=Token;
  Result.TokenName:=TokenName;
  Result.onDebugMsg:=@doDebug;
  Result.onResponseTokenInvalid:=OnTokenInvalid;
  Result.OnPrepareData:=onPrepareData;
  Result.OnProcessRequestFinished:=onProcessRequestFinished;
  Result.OnProcessRequestStart:=onProcessRequestStart;
end;

procedure THKConnection.TestConnection(aUrl: String);
var
  con: TJsonRequester;
  resp: TJSONData;
begin
  con:=CreateConnection;
  try
    con.BaseUrl:=aUrl;
    resp:=con.JsonGet('');
    if resp<>nil then ShowMessage('Connection Success');
  finally
    con.Free;
  end;
end;

end.
