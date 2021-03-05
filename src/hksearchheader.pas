unit HKSearchHeader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, HKPaginationModel, ZConnection, ZDataset;

type

  { THKSearchHeader }

  { THKSearchPanelHeader }

  THKSearchPanelHeader=class(TPanel)
  private
    FOnPerPageChange: TNotifyEvent;
    FOnReloadBtnClick: TNotifyEvent;
    FOnSearchKeyPress: TKeyPressEvent;
    FPerPageValue: Integer;
    FSearch:TEdit;
    FBtn:TButton;
    FCbPerPage:TComboBox;
    fLabel:TLabel;
    procedure BtnClick(Sender: TObject);
    procedure CbPerPageChanged(Sender: TObject);
    function GetSearchText: String;
    procedure SearchKeyPress(Sender: TObject; var Key: char);
    procedure SetOnPerPageChange(AValue: TNotifyEvent);
    procedure SetOnReloadBtnClick(AValue: TNotifyEvent);
    procedure SetOnSearchKeyPress(AValue: TKeyPressEvent);
  protected
    procedure DoOnResize; override;
    procedure DoReloadBtnClick;virtual;
    procedure DoCbPerPageChange(newPerPage:Integer);virtual;
    procedure DoSearchKeyPress(var key:Char);virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property OnReloadBtnClick:TNotifyEvent read FOnReloadBtnClick write SetOnReloadBtnClick;
    property OnSearchKeyPress:TKeyPressEvent read FOnSearchKeyPress write SetOnSearchKeyPress;
    property OnPerPageChange:TNotifyEvent read FOnPerPageChange write SetOnPerPageChange;
    property PerPageValue:Integer read FPerPageValue;
    property SearchText:String read GetSearchText;
  end;

  THKSearchHeader = class(THKSearchPanelHeader)
  private
    FPaginationModel: THKPaginationModel;

    procedure SetPaginationModel(AValue: THKPaginationModel);
  protected

    procedure DoReloadBtnClick; override;
    procedure DoCbPerPageChange(newPerPage: Integer); override;
    procedure DoSearchKeyPress(var key: Char); override;
  published
    property PaginationModel:THKPaginationModel read FPaginationModel write SetPaginationModel;

  end;

  THKSearchHeaderComp=class(THKSearchPanelHeader)
  published
    property OnPerPageChange;
    property OnReloadBtnClick;
    property OnSearchKeyPress;
    property PerPageValue;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I hksearchheader_icon.lrs}
  RegisterComponents('HKCompPacks',[THKSearchHeader]);
  RegisterComponents('HKCompPacks',[THKSearchHeaderComp]);
end;

{ THKSearchPanelHeader }

procedure THKSearchPanelHeader.BtnClick(Sender: TObject);
begin
  FSearch.Text:='';
  DoReloadBtnClick;
end;

procedure THKSearchPanelHeader.CbPerPageChanged(Sender: TObject);
var
  newPage: Longint;
begin
  TryStrToInt(FCbPerPage.Text, newPage);
  DoCbPerPageChange(newPage);
end;

function THKSearchPanelHeader.GetSearchText: String;
begin
  Result:=FSearch.Text;
end;

procedure THKSearchPanelHeader.SearchKeyPress(Sender: TObject; var Key: char);
begin
  DoSearchKeyPress(Key);
end;

procedure THKSearchPanelHeader.SetOnPerPageChange(AValue: TNotifyEvent);
begin
  if FOnPerPageChange=AValue then Exit;
  FOnPerPageChange:=AValue;
end;

procedure THKSearchPanelHeader.SetOnReloadBtnClick(AValue: TNotifyEvent);
begin
  if FOnReloadBtnClick=AValue then Exit;
  FOnReloadBtnClick:=AValue;
end;

procedure THKSearchPanelHeader.SetOnSearchKeyPress(AValue: TKeyPressEvent);
begin
  if FOnSearchKeyPress=AValue then Exit;
  FOnSearchKeyPress:=AValue;
end;

procedure THKSearchPanelHeader.DoOnResize;
begin
  inherited DoOnResize;
  FBtn.Left:=Width-80;
  FSearch.Left:=Width-290;
end;

procedure THKSearchPanelHeader.DoReloadBtnClick;
begin
  if Assigned(OnReloadBtnClick)then OnReloadBtnClick(Self);
end;

procedure THKSearchPanelHeader.DoCbPerPageChange(newPerPage: Integer);
begin
  FPerPageValue:=newPerPage;
  if Assigned(OnPerPageChange)then OnPerPageChange(Self);
end;

procedure THKSearchPanelHeader.DoSearchKeyPress(var key: Char);
begin
  if Assigned(OnSearchKeyPress)then OnSearchKeyPress(FSearch, key);
end;

constructor THKSearchPanelHeader.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FPerPageValue:=25;
  BevelOuter:=bvNone;
  BevelInner:=bvNone;
  Width:=400;
  Height:=50;

  FBtn:=TButton.Create(Self);
  FBtn.Parent:=Self;
  FBtn.Width:=60;
  FBtn.Left:=350;
  FBtn.Height:=40;
  FBtn.Top:=5;
  FBtn.Caption:='Reload';
  FBtn.OnClick:=@BtnClick;
  FBtn.Anchors:=[akTop, akRight, akBottom];

  FSearch:=TEdit.Create(Self);
  FSearch.Parent:=Self;
  FSearch.Width:=200;
  FSearch.Left:=120;
  FSearch.Height:=30;
  FSearch.Top:=10;
  FSearch.OnKeyPress:=@SearchKeyPress;
  FSearch.Anchors:=[akTop, akRight, akBottom];

  fLabel:=TLabel.Create(Self);
  fLabel.Parent:=Self;
  fLabel.Left:=10;
  fLabel.Width:=105;
  fLabel.Height:=30;
  fLabel.Top:=12;
  fLabel.Caption:='Row Per Page';
  fLabel.Anchors:=[akTop, akLeft, akBottom];

  FCbPerPage:=TComboBox.Create(Self);
  FCbPerPage.Parent:=Self;
  FCbPerPage.Left:=110;
  FCbPerPage.Width:=65;
  FCbPerPage.Height:=30;
  FCbPerPage.Top:=10;
  FCbPerPage.Anchors:=[akTop, akLeft, akBottom];
  FCbPerPage.Items.Add('10');
  FCbPerPage.Items.Add('25');
  FCbPerPage.Items.Add('50');
  FCbPerPage.Items.Add('100');
  FCbPerPage.ItemIndex:=1;
  FCbPerPage.OnChange:=@CbPerPageChanged;
end;

destructor THKSearchPanelHeader.Destroy;
begin
  FBtn.Free;
  FCbPerPage.Free;
  fLabel.Free;
  FSearch.Free;
  inherited Destroy;
end;

{ THKSearchHeader }

procedure THKSearchHeader.SetPaginationModel(AValue: THKPaginationModel);
begin
  if FPaginationModel=AValue then Exit;
  FPaginationModel:=AValue;
  FCbPerPage.Text:=FPaginationModel.RowPerPage.ToString;
end;

procedure THKSearchHeader.DoReloadBtnClick;
begin
  if not Assigned(PaginationModel) then Exit;
  FPaginationModel.SearchQry:='';
  FSearch.Text:='';
  FPaginationModel.FirstPage;
end;

procedure THKSearchHeader.DoCbPerPageChange(newPerPage: Integer);
begin
  if not Assigned(PaginationModel) then Exit;
  if newPerPage<=0 then exit;
  FPaginationModel.RowPerPage:=newPerPage;
  FPaginationModel.Refresh;
end;

procedure THKSearchHeader.DoSearchKeyPress(var key: Char);
begin
  if not Assigned(PaginationModel) then Exit;
  if Key=#$0d then
  begin
    FPaginationModel.Search(FSearch.Text);
    Key:=#$0;
  end;
end;

end.
