unit HKPaginationModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, LResources, Forms, Controls, Graphics, Dialogs,
  HKModel;

type

  { THKPaginationModel }

  THKPaginationModel = class(THKModel)
  private
    FCurrentPage: Integer;
    FOnPageChanged: TNotifyEvent;
    FOnPerPageChanged: TNotifyEvent;
    FOnTotalRecordsChanged: TNotifyEvent;
    FPaginationQuery: String;
    FRowPerPage: Integer;
    FSearchQry: String;
    FTotalPage: Integer;
    FTotalRecords: Integer;
    FDoc:String;
    procedure SetCurrentPage(AValue: Integer);
    procedure SetOnPageChanged(AValue: TNotifyEvent);
    procedure SetOnPerPageChanged(AValue: TNotifyEvent);
    procedure SetOnTotalRecordsChanged(AValue: TNotifyEvent);
    procedure SetPaginationQuery(AValue: String);
    procedure SetRowPerPage(AValue: Integer);
    procedure SetSearchQry(AValue: String);
    procedure SetTotalPage(AValue: Integer);
    procedure SetTotalRecords(AValue: Integer);
  protected
    function GetDocument: String; override;
    procedure SetDocument(AValue: String); override;
    function DoResponseData(aResp: TJSONData): TJSONData; override;
    procedure DoPageChanged;
    procedure DoPerPageChanged;
    procedure DoTotalRowsChanged;
  public
    constructor Create(AOwner: TComponent); override;
    procedure NextPage;
    procedure GoToPage(aPage:Integer);
    procedure PrevPage;
    procedure FirstPage;
    procedure LastPage;
    procedure Search(aFilter:String);
  published
    property SearchQry:String read FSearchQry write SetSearchQry;
    property PaginationQuery:String read FPaginationQuery write SetPaginationQuery;
    property RowPerPage:Integer read FRowPerPage write SetRowPerPage;
    property CurrentPage:Integer read FCurrentPage write SetCurrentPage;
    property TotalPage:Integer read FTotalPage write SetTotalPage;
    property TotalRecords:Integer read FTotalRecords write SetTotalRecords;
    property OnPageChanged:TNotifyEvent read FOnPageChanged write SetOnPageChanged;
    property OnPerPageChanged:TNotifyEvent read FOnPerPageChanged write SetOnPerPageChanged;
    property OnTotalRecordsChanged:TNotifyEvent read FOnTotalRecordsChanged write SetOnTotalRecordsChanged;
  end;

procedure Register;

implementation

uses HKCompPacksTypes, Math;

procedure Register;
begin
  {$I hkpaginationmodel_icon.lrs}
  RegisterComponents('HKCompPacks',[THKPaginationModel]);
end;

{ THKPaginationModel }

procedure THKPaginationModel.SetCurrentPage(AValue: Integer);
begin
  if FCurrentPage=AValue then Exit;
  FCurrentPage:=AValue;
  DoPageChanged;
end;

procedure THKPaginationModel.SetOnPageChanged(AValue: TNotifyEvent);
begin
  if FOnPageChanged=AValue then Exit;
  FOnPageChanged:=AValue;
end;

procedure THKPaginationModel.SetOnPerPageChanged(AValue: TNotifyEvent);
begin
  if FOnPerPageChanged=AValue then Exit;
  FOnPerPageChanged:=AValue;
end;

procedure THKPaginationModel.SetOnTotalRecordsChanged(AValue: TNotifyEvent);
begin
  if FOnTotalRecordsChanged=AValue then Exit;
  FOnTotalRecordsChanged:=AValue;
end;

procedure THKPaginationModel.SetPaginationQuery(AValue: String);
begin
  if FPaginationQuery=AValue then Exit;
  FPaginationQuery:=AValue;
end;

procedure THKPaginationModel.SetRowPerPage(AValue: Integer);
begin
  if FRowPerPage=AValue then Exit;
  FRowPerPage:=AValue;
  DoPerPageChanged;
end;

procedure THKPaginationModel.SetSearchQry(AValue: String);
begin
  if FSearchQry=AValue then Exit;
  FSearchQry:=AValue;
end;

procedure THKPaginationModel.SetTotalPage(AValue: Integer);
begin
  if FTotalPage=AValue then Exit;
  FTotalPage:=AValue;
end;

procedure THKPaginationModel.SetTotalRecords(AValue: Integer);
begin

  FTotalRecords:=AValue;

  if (FTotalRecords=0) or (RowPerPage=0) then Exit;

  TotalPage:=Ceil(FTotalRecords/RowPerPage);// FTotalRecords div FRowPerPage;
  DoTotalRowsChanged;
end;

function THKPaginationModel.GetDocument: String;
begin
  Result:=FDoc+Format(PaginationQuery, [RowPerPage, CurrentPage, SearchQry]);
end;

procedure THKPaginationModel.SetDocument(AValue: String);
var
  a: Integer;
begin
  //inherited SetDocument(AValue);String;
  a:=Pos('?',AValue);
  if a>0 then
    FDoc:=LeftStr(AValue, a-1)
  else
    FDoc:=AValue;
end;

function THKPaginationModel.DoResponseData(aResp: TJSONData): TJSONData;
var
  aJson: TJSONObject;
  arr: TJSONArray;
begin
  aJson:=aResp as TJSONObject;
  try
    arr:=GetJsonArrayValue(aJson, 'data');
    Result:=arr.Clone;
    TotalRecords:=GetJsonNumberValue(aJson, 'total');
  finally
    aJson.Free;
  end;
end;

procedure THKPaginationModel.DoPageChanged;
begin
  if Assigned(OnPageChanged)then OnPageChanged(Self);
end;

procedure THKPaginationModel.DoPerPageChanged;
begin
  if Assigned(OnPerPageChanged)then OnPerPageChanged(Self);
end;

procedure THKPaginationModel.DoTotalRowsChanged;
begin
  if Assigned(OnTotalRecordsChanged)then OnTotalRecordsChanged(Self);
end;

constructor THKPaginationModel.Create(AOwner: TComponent);
begin
  TotalPage:=0;
  CurrentPage:=0;
  TotalRecords:=0;
  RowPerPage:=25;
  PaginationQuery:='?perPage=%d&page=%d&search=%s';
  inherited Create(AOwner);
end;

procedure THKPaginationModel.NextPage;
begin
  if CurrentPage>TotalPage then exit;
  GoToPage(CurrentPage+1);
end;

procedure THKPaginationModel.GoToPage(aPage: Integer);
begin
  CurrentPage:=aPage;
  Refresh;
end;

procedure THKPaginationModel.PrevPage;
begin
  if CurrentPage<=1 then exit;
  GoToPage(CurrentPage-1);
end;

procedure THKPaginationModel.FirstPage;
begin
  GoToPage(1);
end;

procedure THKPaginationModel.LastPage;
begin
  GoToPage(TotalPage);
end;

procedure THKPaginationModel.Search(aFilter: String);
begin
  SearchQry:=aFilter;
  FirstPage;
end;

end.
