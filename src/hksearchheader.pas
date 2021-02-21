unit HKSearchHeader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, HKPaginationModel;

type

  { THKSearchHeader }

  THKSearchHeader = class(TPanel)
  private
    FPaginationModel: THKPaginationModel;
    FSearch:TEdit;
    FBtn:TButton;
    FCbPerPage:TComboBox;
    fLabel:TLabel;
    procedure BtnClick(Sender: TObject);
    procedure CbPerPageChanged(Sender: TObject);
    procedure SearchKeyPress(Sender: TObject; var Key: char);
    procedure SetPaginationModel(AValue: THKPaginationModel);
  protected
    procedure DoOnResize; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
    property PaginationModel:THKPaginationModel read FPaginationModel write SetPaginationModel;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I hksearchheader_icon.lrs}
  RegisterComponents('HKCompPacks',[THKSearchHeader]);
end;

{ THKSearchHeader }

procedure THKSearchHeader.SearchKeyPress(Sender: TObject; var Key: char);
begin
  if Key=#$0d then
  begin
    FPaginationModel.Search(FSearch.Text);
    Key:=#$0;
  end;
end;

procedure THKSearchHeader.SetPaginationModel(AValue: THKPaginationModel);
begin
  if FPaginationModel=AValue then Exit;
  FPaginationModel:=AValue;
  FCbPerPage.Text:=FPaginationModel.RowPerPage.ToString;
end;

procedure THKSearchHeader.DoOnResize;
begin
  inherited DoOnResize;
  FBtn.Left:=Width-80;
  FSearch.Left:=Width-290;
end;

procedure THKSearchHeader.CbPerPageChanged(Sender: TObject);
var
  ppage: Longint;
begin
  TryStrToInt(FCbPerPage.Text, ppage);
  if ppage<=0 then exit;
  FPaginationModel.RowPerPage:=ppage;
  FPaginationModel.Refresh;
end;

procedure THKSearchHeader.BtnClick(Sender: TObject);
begin
  FPaginationModel.SearchQry:='';
  FSearch.Text:='';
  FPaginationModel.FirstPage;
end;

constructor THKSearchHeader.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
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
  FCbPerPage.OnChange:=@CbPerPageChanged;
end;

destructor THKSearchHeader.Destroy;
begin
  FBtn.Free;
  FSearch.Free;
  FCbPerPage.Free;
  fLabel.Free;
  inherited Destroy;
end;

end.
