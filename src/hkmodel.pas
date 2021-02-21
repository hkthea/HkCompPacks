unit HKModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, LResources, Forms, Controls, Graphics, Dialogs,
  HKConnection, HKStringGrid, Requester;

type
  TOnFilteringData=procedure(Sender:TObject; aRow:TJSONObject; Query:String;
    var aResult:Boolean)of object;
  { THKModel }

  THKModel = class(TComponent)
  private
    FConnection: THKConnection;
    FDocument: String;
    FHKStringGrid: THKStringGrid;
    FData:TJSONArray;
    FonFilterData: TOnFilteringData;
    fFilter:String;

    procedure SetConnection(AValue: THKConnection);
    procedure SetHKStringGrid(AValue: THKStringGrid);
    procedure SetonFilterData(AValue: TOnFilteringData);
  protected
    function GetDocument: String;virtual;
    procedure SetDocument(AValue: String);virtual;
    procedure ProcessGrid;
    function DoResponseData(aResp:TJSONData):TJSONData;virtual;
    //procedure ProcessResponse;virtual;
    function doFilterData(aRow: TJSONObject): Boolean;
  public
    procedure Refresh;
    function CreateNew(aData:TJSONObject):Boolean;
    function UpdateData(aData:TJSONObject; aId:String):Boolean;
    procedure Filter(aStr:String);
    procedure ClearFilter;
    destructor Destroy; override;
  published
    property Connection:THKConnection read FConnection write SetConnection;
    property Document:String read GetDocument write SetDocument;
    property HKStringGrid:THKStringGrid read FHKStringGrid write SetHKStringGrid;
    property onFilterData:TOnFilteringData read FonFilterData write SetonFilterData;
  end;

procedure Register;

implementation

uses HKCompPacksTypes, DateUtils;


procedure Register;
begin
  {$I hkmodel_icon.lrs}
  RegisterComponents('HKCompPacks',[THKModel]);
end;

{ THKModel }

procedure THKModel.SetConnection(AValue: THKConnection);
begin
  if FConnection=AValue then Exit;
  FConnection:=AValue;
end;

function THKModel.GetDocument: String;
begin
  Result:=FDocument;
end;

procedure THKModel.SetDocument(AValue: String);
begin
  if FDocument=AValue then Exit;
  FDocument:=AValue;
end;

procedure THKModel.SetHKStringGrid(AValue: THKStringGrid);
begin
  if FHKStringGrid=AValue then Exit;
  FHKStringGrid:=AValue;
end;

procedure THKModel.SetonFilterData(AValue: TOnFilteringData);
begin
  if FonFilterData=AValue then Exit;
  FonFilterData:=AValue;
end;

procedure THKModel.ProcessGrid;
var
  aaa, bbb, newCount: Integer;
  vCol: THKGridColumn;
  //row: TStrings;
  aJson: TJSONObject;
  str: String;
begin
  HKStringGrid.RowCount:=1;
  for aaa:=0 to FData.Count-1 do
  begin
    aJson:=FData.Objects[aaa];
    if not doFilterData(aJson)then Continue;
    newCount:=HKStringGrid.RowCount;
    HKStringGrid.RowCount:=newCount+1;
    //row:=HKStringGrid.Rows[newCount-1];
    //row.Add((aaa+1).ToString);
    HKStringGrid.Cells[0, newCount]:=(aaa+1).ToString;
    for bbb:=0 to HKStringGrid.HKGridColumns.Count-1 do
    begin
      vCol:=HKStringGrid.HKGridColumns.Items[bbb] as THKGridColumn;
      case vCol.ColumnType of
        CTTxt:str:=GetJsonStringValue(aJson, vCol.FieldName);
        CTNumber:str:=FormatFloat('#,##0', GetJsonNumberValue(aJson, vCol.FieldName));
        CTBtnEdit:str:='Edit';
        CTBtnDelete:str:='Remove';
        CTButton:str:='Action';
        CTDateTimeISO:
          begin
            str:=GetJsonStringValue(aJson, vCol.FieldName);
            str:=FormatDateTime('DD-MMM-YYYY HH:mm',ISO8601ToDate(str, False));
          end;
      end;
      //row.Add(str);
      HKStringGrid.Cells[bbb+1, newCount]:=str;
    end;
  end;
end;

function THKModel.DoResponseData(aResp: TJSONData): TJSONData;
begin
  Result:=aResp;
end;

function THKModel.doFilterData(aRow: TJSONObject):Boolean;
begin
  Result:=True;
  if fFilter='' then exit;
  if Assigned(onFilterData)then onFilterData(Self, aRow, fFilter, Result);
end;

procedure THKModel.Refresh;
var
  con: TJsonRequester;
  resp: TJSONData;
begin
  if FData<>nil then FreeAndNil(FData);

  if FConnection<>nil then
  begin
    con:=FConnection.CreateConnection;
    try
      resp:=con.JsonGet(Document);
      if resp<>nil then
      begin
        FData:=DoResponseData(resp) as TJSONArray;
        ProcessGrid;
      end;
    finally
      con.Free;
    end;
  end;
end;

function THKModel.CreateNew(aData: TJSONObject): Boolean;
var
  con: TJsonRequester;
  resp: TJSONData;
begin
  Result:=False;
  if FConnection<>nil then
  begin
    con:=FConnection.CreateConnection;
    try
      aData.Booleans['isCreate']:=True;
      resp:=con.JsonPost(Document, aData);
      if resp<>nil then
      begin
        ShowMessage('Create Data Success');
        Result:=True;
        Refresh;
      end;
    finally
      if resp<>nil then FreeAndNil(resp);
      con.Free;
    end;
  end;
end;

function THKModel.UpdateData(aData: TJSONObject; aId: String): Boolean;
var
  con: TJsonRequester;
  resp: TJSONData;
begin
  Result:=False;
  if FConnection<>nil then
  begin
    con:=FConnection.CreateConnection;
    try
      aData.Booleans['isCreate']:=False;
      aData.Strings['id']:=aId;
      resp:=con.JsonPost(Document, aData);
      if resp<>nil then
      begin
        ShowMessage('Edit Data Success');
        Result:=True;
        Refresh;
      end;
    finally
      if resp<>nil then FreeAndNil(resp);
      con.Free;
    end;
  end;
end;

procedure THKModel.Filter(aStr: String);
begin
  fFilter:=aStr;
  ProcessGrid;
end;

procedure THKModel.ClearFilter;
begin
  Filter('');
end;

destructor THKModel.Destroy;
begin
  if FData<>nil then FData.Free;
  inherited Destroy;
end;

end.
