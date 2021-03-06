unit AuthModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, LResources, Forms, Controls, Graphics, Dialogs,
  HKConnection, IdHTTP;

CONST
  DEFSESSIONNAME='.gz-session';
  DEFHASHEDPASSW='.gz-auth';

  DEFSALT='BTT8SLb0FoZAiX07P9z0';
  DEFKEY='TJVDoLmUMwQzZG1efxmw11qRcfcAuZCPjz6divsj';

type
  TOnHashingPassword=procedure(sender:TObject; const aUsername, aPassword:String;
    var Result:String)of object;
  { TAuthModel }

  TAuthModel = class(TComponent)
  private
    FAuthToken: String;
    FConnection: THKConnection;
    FEmail: String;
    FHashedPassword: String;
    Flevel: Integer;
    FName: String;
    FonHashinPassword: TOnHashingPassword;
    FPhone: String;
    FPlainPassword: String;
    FsessionFileName: String;
    FTokenName: String;
    FUserId: String;
    FUsername: String;
    procedure RefreshTokenCallback(Sender: TObject; aRequest: TIdHTTPRequest;
      aResponse: TIdHTTPResponse; aRespContent: TJSONData);
    procedure SetAuthToken(AValue: String);
    procedure SetConnection(AValue: THKConnection);
    procedure SetEmail(AValue: String);
    procedure SetHashedPassword(AValue: String);
    procedure Setlevel(AValue: Integer);
    procedure SetName(AValue: String);
    procedure SetonHashinPassword(AValue: TOnHashingPassword);
    procedure SetPhone(AValue: String);
    procedure SetPlainPassword(AValue: String);
    procedure SetsessionFileName(AValue: String);
    procedure SetTokenName(AValue: String);
    procedure SetUserId(AValue: String);
    procedure SetUsername(AValue: String);
  protected
    function DefaultHashingPassword(aPassword: String=''; aUsername:String=''): String;
    procedure LoginCallback(Sender:TObject; aRequest:TIdHTTPRequest;
      aResponse:TIdHTTPResponse; aRespContent:TJSONData);
    procedure DoHashingPassword;
    function HashPassword(aPassword:String):String;
    procedure ProcessToken;
    procedure SaveToken;
    function EncryptToken(aToken:String):String;
    function DecryptToken(EncToken:String):String;
    procedure loadHashedPassw;
    procedure saveHashedPassw;
    procedure DeleteAuth;
  public
    function Login(aUsername, aPassword:String):Boolean;
    procedure AsyncLogin(aUsername, aPassword:String);
    procedure Logout;

    procedure LoadToken;

    procedure CreateDefaultUser;

    Function GetHashedPassword(aUsername, aPassword:String):String;

    procedure RefreshToken;
    procedure UpdateProfile;
    function ChangePassword(oldPassword, newPassword, ConfirmPassword:String):Boolean;
  published
    property Connection:THKConnection read FConnection write SetConnection;
    property Username:String read FUsername write SetUsername;
    property PlainPassword:String read FPlainPassword write SetPlainPassword;
    property HashedPassword:String read FHashedPassword write SetHashedPassword;
    property Name:String read FName write SetName;
    property Email:String read FEmail write SetEmail;
    property Phone:String read FPhone write SetPhone;
    property level:Integer read Flevel write Setlevel;
    property UserId:String read FUserId write SetUserId;
    property AuthToken:String read FAuthToken write SetAuthToken;
    property TokenName:String read FTokenName write SetTokenName;
    property onHashinPassword:TOnHashingPassword read FonHashinPassword write SetonHashinPassword;
    property sessionFileName:String read FsessionFileName write SetsessionFileName;
    //property AuthTokenStorage:String;
  end;

procedure Register;

implementation

uses Requester, HKCompPacksTypes, base64, DCPsha256, DCPice;

procedure Register;
begin
  {$I authmodel_icon.lrs}
  RegisterComponents('HKCompPacks',[TAuthModel]);
end;

{ TAuthModel }

procedure TAuthModel.SetConnection(AValue: THKConnection);
begin
  if FConnection=AValue then Exit;
  FConnection:=AValue;
end;

procedure TAuthModel.SetAuthToken(AValue: String);
begin
  if FAuthToken=AValue then Exit;
  FAuthToken:=AValue;
end;

procedure TAuthModel.RefreshTokenCallback(Sender: TObject;
  aRequest: TIdHTTPRequest; aResponse: TIdHTTPResponse; aRespContent: TJSONData
  );
begin
  try
    if aRespContent is TJSONString then
    begin
      AuthToken:=aRespContent.AsString;
      ProcessToken;
    end;
  finally
    if aRespContent<>nil then aRespContent.Free;
  end;

end;

procedure TAuthModel.SetEmail(AValue: String);
begin
  if FEmail=AValue then Exit;
  FEmail:=AValue;
end;

procedure TAuthModel.SetHashedPassword(AValue: String);
begin
  if FHashedPassword=AValue then Exit;
  FHashedPassword:=AValue;
end;

procedure TAuthModel.Setlevel(AValue: Integer);
begin
  if Flevel=AValue then Exit;
  Flevel:=AValue;
end;

procedure TAuthModel.SetName(AValue: String);
begin
  if FName=AValue then Exit;
  FName:=AValue;
end;

procedure TAuthModel.SetonHashinPassword(AValue: TOnHashingPassword);
begin
  if FonHashinPassword=AValue then Exit;
  FonHashinPassword:=AValue;
end;

procedure TAuthModel.SetPhone(AValue: String);
begin
  if FPhone=AValue then Exit;
  FPhone:=AValue;
end;

procedure TAuthModel.SetPlainPassword(AValue: String);
begin
  if FPlainPassword=AValue then Exit;
  FPlainPassword:=AValue;
end;

procedure TAuthModel.SetsessionFileName(AValue: String);
begin
  if FsessionFileName=AValue then Exit;
  FsessionFileName:=AValue;
end;

procedure TAuthModel.SetTokenName(AValue: String);
begin
  if FTokenName=AValue then Exit;
  FTokenName:=AValue;
end;

procedure TAuthModel.SetUserId(AValue: String);
begin
  if FUserId=AValue then Exit;
  FUserId:=AValue;
end;

procedure TAuthModel.SetUsername(AValue: String);
begin
  if FUsername=AValue then Exit;
  FUsername:=AValue;
end;

function TAuthModel.DefaultHashingPassword(aPassword: String; aUsername: String
  ): String;
  function DigestToStr(Digest: array of byte): string;
  var
    i: Integer;
  begin
    Result := '';
    for i := 0 to Length(Digest) -1 do
    Result := Result + LowerCase(IntToHex(Digest[i], 2));
  end;
var
  chp: TDCP_sha256;
  Digest: array[0..31] of Byte;
  tempPass, tempUsername: String;
begin
  Result:='';

  if aPassword<>'' then tempPass:=aPassword
  else tempPass:=PlainPassword;

  if aUsername<>'' then tempUsername:=aUsername
  else tempUsername:=Username;

  chp:=TDCP_sha256.Create(nil);
  try
    chp.Init;
    chp.UpdateStr(tempUsername);
    chp.UpdateStr(tempPass);
    chp.UpdateStr(DEFSALT);
    chp.Final(Digest);
    Result:=DigestToStr(Digest);
  finally
    chp.Free;
  end;
end;

procedure TAuthModel.LoginCallback(Sender: TObject; aRequest: TIdHTTPRequest;
  aResponse: TIdHTTPResponse; aRespContent: TJSONData);
begin
  AuthToken:=aRespContent.AsString;
  ProcessToken;
end;

procedure TAuthModel.DoHashingPassword;
var aHashed:String;
begin
  if Assigned(onHashinPassword) then
  begin
    onHashinPassword(Self, Username, PlainPassword, aHashed);
    FHashedPassword:=aHashed;
  end
  else FHashedPassword:=DefaultHashingPassword;
end;

function TAuthModel.HashPassword(aPassword: String): String;
begin
  Result:=aPassword;
  if Assigned(onHashinPassword) then
    onHashinPassword(Self, Username, aPassword, Result)
  else
    Result:=DefaultHashingPassword(aPassword);
end;

procedure TAuthModel.ProcessToken;
var
  jwtPart: TStringArray;
  jsonPart: String;
  aJSON: TJSONObject;
begin
  jwtPart:=AuthToken.Split('.');
  jsonPart:=jwtPart[1];
  aJSON:=(GetJSON(DecodeStringBase64(jsonPart))as TJSONObject) ;
  try
    Username:=GetJsonStringValue(aJSON,'username');
    Name:=GetJsonStringValue(aJSON, 'name');
    Phone:=GetJsonStringValue(aJSON,'phone');
    Email:=GetJsonStringValue(aJSON,'email');
    UserId:=GetJsonStringValue(aJSON,'_id');
    level:=GetJsonNumberValue(aJSON, 'level');
    SaveToken;
    FConnection.Token:=AuthToken;
  finally
    aJSON.Free;
  end;
end;

procedure TAuthModel.SaveToken;
var
  path: String;
  fs: TFileStream;
begin
  path:=GetUserDir+PathDelim+sessionFileName;
  fs:=TFileStream.Create(path, fmCreate or fmOpenReadWrite);
  try
    fs.WriteAnsiString(EncryptToken(AuthToken));
  finally
    fs.Free;
  end;
end;

function TAuthModel.EncryptToken(aToken: String): String;
var enc:TDCP_ice;
begin
  Result:=aToken;
  enc:=TDCP_ice.Create(nil);
  with enc do
  begin
    try
      InitStr(DEFKEY, TDCP_sha256);
      Result:=EncryptString(aToken);
    finally
      Free;
    end;
  end;
end;

function TAuthModel.DecryptToken(EncToken: String): String;
var enc:TDCP_ice;
begin
  Result:=EncToken;
  enc:=TDCP_ice.Create(nil);
  with enc do
  begin
    try
      InitStr(DEFKEY, TDCP_sha256);
      Result:=DecryptString(EncToken);
    finally
      Free;
    end;
  end;
end;

procedure TAuthModel.loadHashedPassw;
var
  path: String;
  fs: TFileStream;
begin
  //if sessionFileName='' then sessionFileName:=DEFHASHEDPASSW;
  path:=GetUserDir+DEFHASHEDPASSW;

  if FileExists(path)then
  begin
    fs:=TFileStream.Create(path, fmOpenRead);
    try
      FHashedPassword:=DecryptToken(fs.ReadAnsiString);
    finally
      fs.Free;
    end;
  end;
end;

procedure TAuthModel.saveHashedPassw;
var
  path: String;
  fs: TFileStream;
begin
  path:=GetUserDir+PathDelim+DEFHASHEDPASSW;
  fs:=TFileStream.Create(path, fmCreate or fmOpenReadWrite);
  try
    fs.WriteAnsiString(EncryptToken(FHashedPassword));
  finally
    fs.Free;
  end;
end;

procedure TAuthModel.DeleteAuth;
var
  path: String;
begin
  Connection.Token:='';
  if sessionFileName='' then sessionFileName:=DEFSESSIONNAME;
  path:=GetUserDir+sessionFileName;
  DeleteFile(path);
  DeleteFile(GetUserDir+DEFHASHEDPASSW);
end;

function TAuthModel.Login(aUsername, aPassword: String): Boolean;
var
  Req: TJsonRequester;
  Resp: TJSONData;
begin
  Result:=False;
  Username:=aUsername;
  PlainPassword:=aPassword;
  DoHashingPassword;
  if FConnection<>nil then
  begin
    Req:=FConnection.CreateConnection;
    try
      Resp:=Req.JsonPost('auth/login', TJSONObject.Create([
        'username', Username,
        'password', FHashedPassword
      ]));
      if Resp<>nil then
      begin
        LoginCallback(Self, Req.Request, req.Response, Resp);
        saveHashedPassw;
        Result:=AuthToken<>'';
      end;
    finally
      Req.Free;
      if Assigned(Resp) then FreeAndNil(Resp);
    end;
  end;
end;

procedure TAuthModel.AsyncLogin(aUsername, aPassword: String);
var
  req: TAsyncJsonRequester;
begin
  Username:=aUsername;
  PlainPassword:=aPassword;
  DoHashingPassword;
  if Assigned(FConnection)then
  begin
    req:=FConnection.CreateAsyncConnection();
    req.AsyncJsonPost('auth/login', TJSONObject.Create([
      'username', Username,
      'password', FHashedPassword
    ]), @LoginCallback);
  end;
end;

procedure TAuthModel.Logout;
begin
  DeleteAuth;
end;

procedure TAuthModel.LoadToken;
var
  path: String;
  fs: TFileStream;
begin
  if sessionFileName='' then sessionFileName:=DEFSESSIONNAME;
  path:=GetUserDir+sessionFileName;

  if FileExists(path)then
  begin
    fs:=TFileStream.Create(path, fmOpenRead);
    try
      AuthToken:=DecryptToken(fs.ReadAnsiString);
    finally
      fs.Free;
    end;
  end;
  if AuthToken<>'' then ProcessToken;
  loadHashedPassw;
end;

procedure TAuthModel.CreateDefaultUser;
var
  req: TJsonRequester;
  resp, res: TJSONData;
  oldPlainPassw, oldHashedPassw: String;
begin
  if Assigned(FConnection) then
  begin
    oldPlainPassw:=PlainPassword;
    oldHashedPassw:=FHashedPassword;
    PlainPassword:='rahasia1';
    DoHashingPassword;
    req:=FConnection.CreateConnection;
    try
      res:=req.JsonPost('auth/createDefaultUser', TJSONObject.Create([
        'username','administrator',
        'password',FHashedPassword
      ]));
    finally
      PlainPassword:=oldPlainPassw;
      FHashedPassword:=oldHashedPassw;
      if Assigned(res)then FreeAndNil(res);
      req.Free;
    end;
  end;
end;

function TAuthModel.GetHashedPassword(aUsername, aPassword: String): String;
begin
  if Assigned(onHashinPassword)then
  begin
    onHashinPassword(Self, aUsername, aPassword, Result);
  end
  else Result:=DefaultHashingPassword(aPassword, aUsername);
end;

procedure TAuthModel.RefreshToken;
var
  req: TAsyncJsonRequester;
begin
  if Assigned(FConnection)then
  begin
    req:=FConnection.CreateAsyncConnection();
    req.AsyncJsonGet('auth/refreshToken',@RefreshTokenCallback);
  end;
end;

procedure TAuthModel.UpdateProfile;
var
  req: TJsonRequester;
  res: TJSONData;
begin
  if Assigned(FConnection) then
  begin
    req:=FConnection.CreateConnection;
    try
      res:=req.JsonPost('auth/profile', TJSONObject.Create([
        'name', Name,
        'email', Email,
        'phone', Phone
      ]));
      if res<>nil then ShowMessage('Update Profile Success');
    finally
      if Assigned(res) then FreeAndNil(res);
      req.Free;
    end;
  end;
end;

function TAuthModel.ChangePassword(oldPassword, newPassword,
  ConfirmPassword: String): Boolean;
var
  req: TJsonRequester;
  res: TJSONData;
  hPass: String;
begin
  Result:=False;
  hPass:=HashPassword(oldPassword);
  if hPass<>FHashedPassword then
  begin
    ShowMessage('Old Password Not Match!');
    Exit;
  end;
  if newPassword<>oldPassword then
  begin
    ShowMessage('Confirmation Password Not Match');
    Exit;
  end;

  if Assigned(FConnection) then
  begin
    req:=FConnection.CreateConnection;
    try
      PlainPassword:=newPassword;
      DoHashingPassword;
      res:=req.JsonPost('auth/changePassword', TJSONObject.Create([
        'password', FHashedPassword
      ]));
      if res<>nil then
      begin
        if res.AsString = FHashedPassword then saveHashedPassw;
        Result:=res.AsString=FHashedPassword;
      end;
    finally
      if assigned(res)then FreeAndNil(res);
      req.Free;
    end;
  end;
end;

end.
