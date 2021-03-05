unit HKCompPacksTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson;

type
  TDebugType=(DTInfo, DTInfo2, DTInfo3, DTException, DTError);
  TOnDebugMessage=procedure(Sender:TObject; aMessage:String;
    aDebugType:TDebugType)of object;

  function GetJsonStringValue(aJson:TJSONObject; aPath:String):String;
  function GetJsonNumberValue(aJson:TJSONObject; aPath:String):Integer;
  function GetJsonArrayValue(aJson:TJSONObject; aPath:String):TJSONArray;

  function GetLogMessage(aMessage:String; aDebugType:TDebugType):String;

implementation

function GetJsonStringValue(aJson: TJSONObject; aPath: String): String;
begin
  Result:='';
  if (aJson.FindPath(aPath)<>nil)and(not aJson.FindPath(aPath).IsNull) then
  begin
    Result:=aJson.FindPath(aPath).AsString;
  end;
end;

function GetJsonNumberValue(aJson: TJSONObject; aPath: String): Integer;
begin
  Result:=-1;
  if aJson.FindPath(aPath)<>nil then
  begin
    try
      if not aJson.FindPath(aPath).IsNull then
      begin
        Result:=aJson.FindPath(aPath).AsInteger;
      end
      else
        Result:=0;
    except
      Result:=0;
    end;
  end;
end;

function GetJsonArrayValue(aJson: TJSONObject; aPath: String): TJSONArray;
begin
  Result:=nil;
  if aJson.FindPath(aPath)<>nil then
  begin
    Result:=aJson.FindPath(aPath) as TJSONArray;
  end;
end;

function GetLogMessage(aMessage: String; aDebugType: TDebugType): String;
var
  lType: String;
begin
  case aDebugType of
    DTInfo:lType:='Info';
    DTInfo2:lType:='Info';
    DTInfo3:lType:='Info';
    DTError:lType:='Error';
    DTException:lType:='Exception';
  end;
  Result:=Format('%s [%s] => %s',[DateTimeToStr(Now), lType,
    aMessage.Replace(#$0d,' ',[rfReplaceAll]).Replace(#$0a,' ',[rfReplaceAll])]);
end;

end.

