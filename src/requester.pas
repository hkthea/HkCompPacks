unit Requester;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, HKCompPacksTypes, fpjson, IdHTTP;

type
  TRequestCallback=procedure(Sender:TObject; aRequest:TIdHTTPRequest;
    aResponse:TIdHTTPResponse; aRespContent:TJSONData)of object;

  { TJsonRequester }

  TJsonRequester=class(TIdCustomHTTP)
  private
    FBaseUrl: String;
    FonDebugMsg: TOnDebugMessage;
    FOnPrepareData: TNotifyEvent;
    FOnProcessRequestFinished: TNotifyEvent;
    FOnProcessRequestStart: TNotifyEvent;
    FonRequestCallback: TRequestCallback;
    FonResponseTokenInvalid: TNotifyEvent;
    FToken: String;
    FTokenName: String;
    procedure SetBaseUrl(AValue: String);
    procedure SetonDebugMsg(AValue: TOnDebugMessage);
    procedure SetOnPrepareData(AValue: TNotifyEvent);
    procedure SetOnProcessRequestFinished(AValue: TNotifyEvent);
    procedure SetOnProcessRequestStart(AValue: TNotifyEvent);
    procedure SetonRequestCallback(AValue: TRequestCallback);
    procedure SetonResponseTokenInvalid(AValue: TNotifyEvent);
    procedure SetToken(AValue: String);
    procedure SetTokenName(AValue: String);
  protected
    procedure doDebug(aMessage:String; aDebugType:TDebugType);virtual;
    function ProcessResponse(aResponse, aDoc: String): TJSONData;
    function ProcessJsonResp(aJson: TJSONObject; aDoc: String): TJSONData;
    procedure doProcessRequestStart;virtual;
    procedure doProcessRequestFinished;virtual;
    procedure doPrepareData;virtual;
    procedure doCallback(aData:TJSONData);
    procedure Prepare;
  public
    function JsonGet(aDoc:String):TJSONData;
    function JsonPost(aDoc:String; data:TJSONObject):TJSONData;
  published
    property BaseUrl:String read FBaseUrl write SetBaseUrl;
    property onDebugMsg:TOnDebugMessage read FonDebugMsg write SetonDebugMsg;
    property Token:String read FToken write SetToken;
    property TokenName:String read FTokenName write SetTokenName;
    property OnProcessRequestStart:TNotifyEvent read FOnProcessRequestStart write SetOnProcessRequestStart;
    property OnProcessRequestFinished:TNotifyEvent read FOnProcessRequestFinished write SetOnProcessRequestFinished;
    property OnPrepareData:TNotifyEvent read FOnPrepareData write SetOnPrepareData;
    property onRequestCallback:TRequestCallback read FonRequestCallback write SetonRequestCallback;
    property onResponseTokenInvalid:TNotifyEvent read FonResponseTokenInvalid write SetonResponseTokenInvalid;
  end;

  //TRequesterThread=class;

  { TAsyncJsonRequester }

  TAsyncJsonRequester=class(TJsonRequester)
  private
    FThread:TThread;
    FDebugMsg:String;
    FDebugType:TDebugType;
  protected
    procedure SyncDebug;
    procedure SyncProcessReqFinish;
    procedure SyncProcessReqStart;
    procedure doDebug(aMessage: String; aDebugType: TDebugType); override;
    procedure doProcessRequestFinished; override;
    procedure doProcessRequestStart; override;
    procedure doPrepareData; override;
  public
    Procedure AsyncJsonGet(aDoc:String; aCallback:TRequestCallback);
    Procedure AsyncJsonPost(ADoc:String; aBody:TJSONObject; aCallback:TRequestCallback);
    constructor Create(AOwner: TComponent); reintroduce; overload;
  end;

  { TGetRequesterThread }

  TGetRequesterThread=class(TThread)
  private
    fRequester:TAsyncJsonRequester;
    fCallback:TRequestCallback;
    fDoc:String;
  protected
    fResp:TJSONData;
    procedure Execute; override;
    procedure doCallback;
  public
    constructor Create(ARequester:TAsyncJsonRequester; ADoc:String;
      aCallback:TRequestCallback);
    destructor Destroy; override;
  end;

  { TPostRequesterThread }

  TPostRequesterThread=class(TGetRequesterThread)
  private
    fBody:TJSONObject;
  protected
    procedure Execute; override;
  public
    constructor Create(ARequester:TAsyncJsonRequester; ADoc:String;
       aCallback:TRequestCallback;ABody:TJSONObject);
    destructor Destroy; override;
  end;

implementation

{ TPostRequesterThread }

procedure TPostRequesterThread.Execute;
begin
  fResp:=fRequester.JsonPost(fDoc, fBody);
  Synchronize(@doCallback);
end;

constructor TPostRequesterThread.Create(ARequester: TAsyncJsonRequester;
  ADoc: String; aCallback: TRequestCallback; ABody: TJSONObject);
begin
  fBody:=ABody;
  inherited Create(ARequester, ADoc, aCallback);
end;

destructor TPostRequesterThread.Destroy;
begin
  inherited Destroy;
end;

{ TGetRequesterThread }

procedure TGetRequesterThread.Execute;
begin
  fResp:=fRequester.JsonGet(fDoc);
  Synchronize(@doCallback);
end;

procedure TGetRequesterThread.doCallback;
begin
  if Assigned(fCallback)then
    fCallback(fRequester, fRequester.Request, fRequester.Response, fResp);
end;

constructor TGetRequesterThread.Create(ARequester: TAsyncJsonRequester;
  ADoc: String; aCallback: TRequestCallback);
begin
  fRequester:=ARequester;
  fDoc:=ADoc;
  fCallback:=aCallback;
  FreeOnTerminate:=True;
  inherited Create(False);
end;

destructor TGetRequesterThread.Destroy;
begin
  fRequester.Free;
  inherited Destroy;
end;

{ TAsyncJsonRequester }

procedure TAsyncJsonRequester.SyncDebug;
begin
  inherited doDebug(FDebugMsg, FDebugType);
end;

procedure TAsyncJsonRequester.SyncProcessReqFinish;
begin
  inherited doProcessRequestFinished;
end;

procedure TAsyncJsonRequester.SyncProcessReqStart;
begin
  inherited doProcessRequestStart;
end;

procedure TAsyncJsonRequester.doDebug(aMessage: String; aDebugType: TDebugType);
begin
  FDebugMsg:=aMessage;
  FDebugType:=aDebugType;
  if (Assigned(FThread))and (FThread<>nil) then
    TThread.Synchronize(FThread, @SyncDebug);
end;

procedure TAsyncJsonRequester.doProcessRequestFinished;
begin
  if (Assigned(FThread))and (FThread<>nil) then
    TThread.Synchronize(FThread, @SyncProcessReqFinish);
end;

procedure TAsyncJsonRequester.doProcessRequestStart;
begin
  if (Assigned(FThread))and (FThread<>nil) then
    TThread.Synchronize(FThread ,@SyncProcessReqStart);
end;

procedure TAsyncJsonRequester.doPrepareData;
begin
  inherited doPrepareData;
end;

procedure TAsyncJsonRequester.AsyncJsonGet(aDoc: String;
  aCallback: TRequestCallback);
begin
  FThread:=TGetRequesterThread.Create(Self, aDoc, aCallback);
end;

procedure TAsyncJsonRequester.AsyncJsonPost(ADoc: String; aBody: TJSONObject;
  aCallback: TRequestCallback);
begin
  FThread:=TPostRequesterThread.Create(Self, ADoc, aCallback, aBody);
end;

constructor TAsyncJsonRequester.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;


{ TJsonRequester }

procedure TJsonRequester.SetBaseUrl(AValue: String);
begin
  if FBaseUrl=AValue then Exit;
  FBaseUrl:=AValue;
end;

procedure TJsonRequester.SetonDebugMsg(AValue: TOnDebugMessage);
begin
  if FonDebugMsg=AValue then Exit;
  FonDebugMsg:=AValue;
end;

procedure TJsonRequester.SetOnPrepareData(AValue: TNotifyEvent);
begin
  if FOnPrepareData=AValue then Exit;
  FOnPrepareData:=AValue;
end;

procedure TJsonRequester.SetOnProcessRequestFinished(AValue: TNotifyEvent);
begin
  if FOnProcessRequestFinished=AValue then Exit;
  FOnProcessRequestFinished:=AValue;
end;

procedure TJsonRequester.SetOnProcessRequestStart(AValue: TNotifyEvent);
begin
  if FOnProcessRequestStart=AValue then Exit;
  FOnProcessRequestStart:=AValue;
end;

procedure TJsonRequester.SetonRequestCallback(AValue: TRequestCallback);
begin
  if FonRequestCallback=AValue then Exit;
  FonRequestCallback:=AValue;
end;

procedure TJsonRequester.SetonResponseTokenInvalid(AValue: TNotifyEvent);
begin
  if FonResponseTokenInvalid=AValue then Exit;
  FonResponseTokenInvalid:=AValue;
end;

procedure TJsonRequester.SetToken(AValue: String);
begin
  if FToken=AValue then Exit;
  FToken:=AValue;
end;

procedure TJsonRequester.SetTokenName(AValue: String);
begin
  if FTokenName=AValue then Exit;
  FTokenName:=AValue;
end;

procedure TJsonRequester.doDebug(aMessage: String; aDebugType: TDebugType);
begin
  if Assigned(FonDebugMsg) then FonDebugMsg(Self, aMessage, aDebugType);
end;

function TJsonRequester.ProcessResponse(aResponse, aDoc: String): TJSONData;
var
  aJson: TJSONObject;
begin
  aJson:=GetJSON(aResponse) as TJSONObject;
  try
    Result:=ProcessJsonResp(aJson, aDoc);
  finally
    FreeAndNil(aJson);
  end;
end;

function TJsonRequester.ProcessJsonResp(aJson: TJSONObject; aDoc:String): TJSONData;
var
  errCode: Integer;
  res: TJSONData;
  errMessage: String;
begin
  Result:=nil;
  errCode:=GetJsonNumberValue(aJson, 'error');
  if errCode=0 then
  begin
    if aJson.FindPath('data')<>nil then
    begin
      res:=aJson.FindPath('data');
      Result:=GetJSON(res.AsJSON);
    end;
  end
  else if (errCode=403)or(errCode=401) then
  begin
    doDebug(format('Response %s Error [ Token Invalid/Expired ]',[aDoc]), DTError);
    if Assigned(FonResponseTokenInvalid)then FonResponseTokenInvalid(Self);
  end
  else
  begin
    errMessage:=GetJsonStringValue(aJson, 'message');
    doDebug(format('Response %s Error [%s]',[aDoc, errMessage]), DTError);
  end;
end;

procedure TJsonRequester.doProcessRequestStart;
begin
  if Assigned(OnProcessRequestStart) then OnProcessRequestStart(Self);
end;

procedure TJsonRequester.doProcessRequestFinished;
begin
  if Assigned(OnProcessRequestFinished) then OnProcessRequestFinished(Self);
end;

procedure TJsonRequester.doPrepareData;
begin
  if Assigned(OnPrepareData) then OnPrepareData(Self);
end;

procedure TJsonRequester.doCallback(aData: TJSONData);
begin
  if Assigned(onRequestCallback) then
    onRequestCallback(Self, Request, Response, aData);
end;

procedure TJsonRequester.Prepare;
begin
  Request.ContentType:='application/json';
  if Token<>'' then Request.CustomHeaders.Values[TokenName]:=Token;
  doPrepareData;
end;

function TJsonRequester.JsonGet(aDoc: String): TJSONData;
var
  resp: String;
begin
  Result:=nil;
  Prepare;
  doDebug(Format('GET Command "%s" ',[aDoc]), DTInfo3);
  doProcessRequestStart;
  try
    try
      resp:=Get(BaseUrl+aDoc);
      Result:=ProcessResponse(resp, aDoc);
      doDebug(Format('GET Command "%s" Success',[aDoc]), DTInfo3);
      //doCallback(Result);
    except
      on e:exception do
        doDebug(Format('GET Command "%s" Error [%s]',[aDoc, e.Message]), DTException);
    end;
  finally
    doProcessRequestFinished;
    Disconnect;
  end;
end;

function TJsonRequester.JsonPost(aDoc: String; data: TJSONObject): TJSONData;
var
  stream: TStringStream;
  resp: String;
  strJson: String;
  len: Integer;
begin
  Result:=nil;
  Prepare;
  strJson:=data.AsJSON;
  len:=Length(strJson);
  doDebug(Format('POST Command URL "%s" Body: %s',[aDoc, strJson]), DTInfo3);
  stream:=TStringStream.Create(strJson);
  doProcessRequestStart;
  try
    try
      resp:=Post(BaseUrl+aDoc, stream);
      Result:=ProcessResponse(resp, aDoc);
      doDebug(Format('POST Command URL "%s" Success',[aDoc]), DTInfo3);
    except
      on e:exception do
        doDebug(Format('POST Command "%s" Error [%s]',[aDoc, e.Message]), DTException);
    end;
  finally
    doProcessRequestFinished;
    data.Free;
    stream.Free;
    Disconnect;
  end;
end;

end.

