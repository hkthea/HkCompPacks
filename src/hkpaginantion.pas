unit HKPaginantion;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, HKPaginationModel;

type

  { THKPaginantion }

  THKPaginantion = class(TPanel)
  private
    FFirstBtn:TButton;
    FLastBtn:TButton;
    FNextBtn:TButton;
    FPrevBtn:TButton;
    FLabel:TLabel;
    FPaginationModel: THKPaginationModel;
    procedure FirstBtnClick(Sender: TObject);
    procedure LastBtnClick(Sender: TObject);
    procedure NextBtnClick(Sender: TObject);
    procedure PageChanged(Sender: TObject);
    procedure PerPageChanged(Sender: TObject);
    procedure PrevBtnClick(Sender: TObject);
    procedure SetPaginationModel(AValue: THKPaginationModel);
    procedure TotalRecordChanged(Sender: TObject);
    Function CreateBtn(aCaption:String):TButton;
    //Procedure CreateLabel():
  protected
    procedure EnDisBtn;
    procedure UpdateInfo;
    procedure UpdatePagination;
    procedure DoOnResize; override;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure CreatePaginationControl;
    destructor Destroy; override;
  published
    property PaginationModel:THKPaginationModel read FPaginationModel write SetPaginationModel;
  end;

  { THKPaginationComp }

  THKPaginationComp=class(TPanel)
  private
    FCurrentPage: Integer;
    FFirstBtn:TButton;
    FLastBtn:TButton;
    FNextBtn:TButton;
    FOnFirstBtnClick: TNotifyEvent;
    FOnLastBtnClick: TNotifyEvent;
    FOnNextBtnClick: TNotifyEvent;
    FOnPrevBtnClick: TNotifyEvent;
    FPerPage: Integer;
    FPrevBtn:TButton;
    FLabel:TLabel;
    FTotalPage: Integer;
    FTotalRecords: Integer;
    procedure FirstBtnClick(Sender: TObject);
    procedure LastBtnClick(Sender: TObject);
    procedure NextBtnClick(Sender: TObject);
    procedure PrevBtnClick(Sender: TObject);
    Function CreateBtn(aCaption:String):TButton;
    procedure SetCurrentPage(AValue: Integer);

    procedure SetOnFirstBtnClick(AValue: TNotifyEvent);
    procedure SetOnLastBtnClick(AValue: TNotifyEvent);
    procedure SetOnNextBtnClick(AValue: TNotifyEvent);
    procedure SetOnPrevBtnClick(AValue: TNotifyEvent);

    procedure SetPerPage(AValue: Integer);
    procedure SetTotalRecords(AValue: Integer);
  protected
    procedure DoFirstBtnClick;virtual;
    procedure DoLastBtnClick;virtual;
    procedure DoPrevstBtnClick;virtual;
    procedure DoNextBtnClick;virtual;
    procedure UpdateInfo;virtual;
    procedure EnDisBtn;virtual;
    procedure DoOnResize; override;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure CreatePaginationControl;
    destructor Destroy; override;
  published
    property OnFirstBtnClick:TNotifyEvent read FOnFirstBtnClick write SetOnFirstBtnClick;
    property OnLastBtnClick:TNotifyEvent read FOnLastBtnClick write SetOnLastBtnClick;
    property OnPrevBtnClick:TNotifyEvent read FOnPrevBtnClick write SetOnPrevBtnClick;
    property OnNextBtnClick:TNotifyEvent read FOnNextBtnClick write SetOnNextBtnClick;

    property CurrentPage:Integer read FCurrentPage write SetCurrentPage;
    property TotalRecords:Integer read FTotalRecords write SetTotalRecords;
    property PerPage:Integer read FPerPage write SetPerPage;
    property TotalPage:Integer read FTotalPage;
  end;

procedure Register;

implementation

uses Math;


procedure Register;
begin
  {$I hkpaginantion_icon.lrs}
  RegisterComponents('HKCompPacks',[THKPaginantion]);
  {$I hkpaginantioncomp_icon.lrs}
  RegisterComponents('HKCompPacks',[THKPaginationComp]);
end;

{ THKPaginationComp }

procedure THKPaginationComp.FirstBtnClick(Sender: TObject);
begin
  CurrentPage:=1;
  DoFirstBtnClick;
end;

procedure THKPaginationComp.LastBtnClick(Sender: TObject);
begin
  FTotalPage:=Math.Ceil(TotalRecords/PerPage);
  CurrentPage:=FTotalPage;
  DoLastBtnClick;
end;

procedure THKPaginationComp.NextBtnClick(Sender: TObject);
begin
  CurrentPage:=CurrentPage+1;
  DoNextBtnClick;
end;

procedure THKPaginationComp.PrevBtnClick(Sender: TObject);
begin
  //Dec(CurrentPage);
  CurrentPage:=CurrentPage-1;
  DoPrevstBtnClick;
end;

function THKPaginationComp.CreateBtn(aCaption: String): TButton;
begin
  Result:=TButton.Create(Self);
  Result.Parent:=Self;
  Result.Anchors:=[akRight, akTop, akBottom];
  Result.Width:=40;
  Result.Height:=40;
  Result.Caption:=aCaption;
end;

procedure THKPaginationComp.SetCurrentPage(AValue: Integer);
begin
  if FCurrentPage=AValue then Exit;
  FCurrentPage:=AValue;
  UpdateInfo;
end;

procedure THKPaginationComp.SetOnFirstBtnClick(AValue: TNotifyEvent);
begin
  if FOnFirstBtnClick=AValue then Exit;
  FOnFirstBtnClick:=AValue;

end;

procedure THKPaginationComp.SetOnLastBtnClick(AValue: TNotifyEvent);
begin
  if FOnLastBtnClick=AValue then Exit;
  FOnLastBtnClick:=AValue;
end;

procedure THKPaginationComp.SetOnNextBtnClick(AValue: TNotifyEvent);
begin
  if FOnNextBtnClick=AValue then Exit;
  FOnNextBtnClick:=AValue;

end;

procedure THKPaginationComp.SetOnPrevBtnClick(AValue: TNotifyEvent);
begin
  if FOnPrevBtnClick=AValue then Exit;
  FOnPrevBtnClick:=AValue;

end;

procedure THKPaginationComp.SetPerPage(AValue: Integer);
begin
  if FPerPage=AValue then Exit;
  FPerPage:=AValue;
  UpdateInfo;
end;

procedure THKPaginationComp.SetTotalRecords(AValue: Integer);
begin
  if FTotalRecords=AValue then Exit;
  FTotalRecords:=AValue;
  UpdateInfo;
end;

procedure THKPaginationComp.DoFirstBtnClick;
begin
  if Assigned(OnFirstBtnClick)then OnFirstBtnClick(Self);
end;

procedure THKPaginationComp.DoLastBtnClick;
begin
  if Assigned(OnLastBtnClick)then OnLastBtnClick(Self);
end;

procedure THKPaginationComp.DoPrevstBtnClick;
begin
  if Assigned(OnPrevBtnClick)Then OnPrevBtnClick(Self);
end;

procedure THKPaginationComp.DoNextBtnClick;
begin
  if Assigned(OnNextBtnClick)then OnNextBtnClick(Self);
end;

procedure THKPaginationComp.UpdateInfo;
var
  recNo, lastRecNo: Integer;
begin
  recNo:=((CurrentPage-1) * PerPage)+1;
  if TotalRecords>(CurrentPage*PerPage) then
    lastRecNo:=CurrentPage*PerPage
  else
    lastRecNo:=TotalRecords;

  FTotalPage:=Math.Ceil(TotalRecords/PerPage);

  FLabel.Caption:=format('%d-%d Of %s Records Page %d Of %d',
    [recNo, lastRecNo, FormatFloat('#,##0', TotalRecords),
      CurrentPage, TotalPage]);

  EnDisBtn;
end;

procedure THKPaginationComp.EnDisBtn;
begin
  FPrevBtn.Enabled:=CurrentPage>1;
  FFirstBtn.Enabled:=CurrentPage>1;
  FNextBtn.Enabled:=CurrentPage<TotalPage;
  FLastBtn.Enabled:=CurrentPage<TotalPage;
end;

procedure THKPaginationComp.DoOnResize;
begin
  inherited DoOnResize;
  FLastBtn.Left:=Width-60;
  FNextBtn.Left:=Width-105;
  FPrevBtn.Left:=Width-150;
  FFirstBtn.Left:=Width-195;
end;

constructor THKPaginationComp.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Caption:='';
  Width:=500;
  Height:=50;
  BevelOuter:=bvNone;
  BevelInner:=bvNone;
  CreatePaginationControl;
  PerPage:=25;
end;

procedure THKPaginationComp.CreatePaginationControl;
begin
  FLastBtn:=CreateBtn('Last');
  FLastBtn.Left:=440;
  FLastBtn.Top:=5;
  FLastBtn.OnClick:=@LastBtnClick;

  FNextBtn:=CreateBtn('Next');
  FNextBtn.Left:=395;
  FNextBtn.Top:=5;
  FNextBtn.OnClick:=@NextBtnClick;

  FPrevBtn:=CreateBtn('Prev');
  FPrevBtn.Left:=350;
  FPrevBtn.Top:=5;
  FPrevBtn.OnClick:=@PrevBtnClick;

  FFirstBtn:=CreateBtn('First');
  FFirstBtn.Left:=305;
  FFirstBtn.Top:=5;
  FFirstBtn.OnClick:=@FirstBtnClick;

  FLabel:=TLabel.Create(Self);
  FLabel.Caption:='0-0 of 0 Records';
  FLabel.Parent:=Self;
  FLabel.Anchors:=[akLeft, akTop, akBottom];
  FLabel.Width:=100;
  FLabel.Height:=20;
  FLabel.Left:=20;
  FLabel.Top:=15;
end;

destructor THKPaginationComp.Destroy;
begin
  FLabel.Free;
  FFirstBtn.Free;
  FLastBtn.Free;
  FNextBtn.Free;
  FPrevBtn.Free;
  inherited Destroy;
end;

{ THKPaginantion }

procedure THKPaginantion.SetPaginationModel(AValue: THKPaginationModel);
begin
  if FPaginationModel=AValue then Exit;
  FPaginationModel:=AValue;
  FPaginationModel.OnPageChanged:=@PageChanged;
  FPaginationModel.OnPerPageChanged:=@PerPageChanged;
  FPaginationModel.OnTotalRecordsChanged:=@TotalRecordChanged;
end;

procedure THKPaginantion.PerPageChanged(Sender: TObject);
begin
  UpdatePagination;
end;

procedure THKPaginantion.TotalRecordChanged(Sender: TObject);
begin
  if Assigned(PaginationModel) then UpdatePagination;
end;

procedure THKPaginantion.PageChanged(Sender: TObject);
begin
  if Assigned(PaginationModel) then UpdatePagination;
end;

procedure THKPaginantion.PrevBtnClick(Sender: TObject);
begin
  if Assigned(PaginationModel) then FPaginationModel.PrevPage;
end;


procedure THKPaginantion.LastBtnClick(Sender: TObject);
begin
  if Assigned(PaginationModel) then FPaginationModel.LastPage;
end;

procedure THKPaginantion.FirstBtnClick(Sender: TObject);
begin
  if Assigned(PaginationModel) then FPaginationModel.FirstPage;
end;

procedure THKPaginantion.NextBtnClick(Sender: TObject);
begin
  if Assigned(PaginationModel) then FPaginationModel.NextPage;
end;

function THKPaginantion.CreateBtn(aCaption: String): TButton;
begin
  Result:=TButton.Create(Self);
  Result.Parent:=Self;
  Result.Anchors:=[akRight, akTop, akBottom];
  Result.Width:=40;
  Result.Height:=40;
  Result.Caption:=aCaption;
end;

procedure THKPaginantion.EnDisBtn;
begin
  if Assigned(PaginationModel) then
  begin
    FPrevBtn.Enabled:=FPaginationModel.CurrentPage>1;
    FFirstBtn.Enabled:=FPaginationModel.CurrentPage>1;
    FNextBtn.Enabled:=FPaginationModel.CurrentPage<FPaginationModel.TotalPage;
    FLastBtn.Enabled:=FPaginationModel.CurrentPage<FPaginationModel.TotalPage;
  end;

end;

procedure THKPaginantion.UpdateInfo;
var
  recNo, lastRecNo: Integer;
begin
  if Assigned(PaginationModel) then
  begin
    recNo:=((FPaginationModel.CurrentPage-1) * FPaginationModel.RowPerPage)+1;
    if FPaginationModel.TotalRecords>(FPaginationModel.CurrentPage*FPaginationModel.RowPerPage) then
      lastRecNo:=FPaginationModel.CurrentPage*FPaginationModel.RowPerPage
    else
      lastRecNo:=FPaginationModel.TotalRecords;

    FLabel.Caption:=format('%d-%d Of %s Records Page %d Of %d',
      [recNo, lastRecNo, FormatFloat('#,##0', FPaginationModel.TotalRecords),
        FPaginationModel.CurrentPage, FPaginationModel.TotalPage]);
  end;
end;

procedure THKPaginantion.UpdatePagination;
begin
  EnDisBtn;
  UpdateInfo;
end;

procedure THKPaginantion.DoOnResize;
begin
  inherited DoOnResize;
  FLastBtn.Left:=Width-60;
  FNextBtn.Left:=Width-105;
  FPrevBtn.Left:=Width-150;
  FFirstBtn.Left:=Width-195;
end;

constructor THKPaginantion.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Caption:='';
  Width:=500;
  Height:=50;
  BevelOuter:=bvNone;
  BevelInner:=bvNone;
  CreatePaginationControl;
end;

procedure THKPaginantion.CreatePaginationControl;
//var
  //btn: TButton;
begin
  FLastBtn:=CreateBtn('Last');
  FLastBtn.Left:=440;
  FLastBtn.Top:=5;
  FLastBtn.OnClick:=@LastBtnClick;

  FNextBtn:=CreateBtn('Next');
  FNextBtn.Left:=395;
  FNextBtn.Top:=5;
  FNextBtn.OnClick:=@NextBtnClick;

  FPrevBtn:=CreateBtn('Prev');
  FPrevBtn.Left:=350;
  FPrevBtn.Top:=5;
  FPrevBtn.OnClick:=@PrevBtnClick;

  FFirstBtn:=CreateBtn('First');
  FFirstBtn.Left:=305;
  FFirstBtn.Top:=5;
  FFirstBtn.OnClick:=@FirstBtnClick;

  FLabel:=TLabel.Create(Self);
  FLabel.Caption:='0-0 of 0 Records';
  FLabel.Parent:=Self;
  FLabel.Anchors:=[akLeft, akTop, akBottom];
  FLabel.Width:=100;
  FLabel.Height:=20;
  FLabel.Left:=20;
  FLabel.Top:=15;
end;

destructor THKPaginantion.Destroy;
begin
  FLabel.Free;
  FFirstBtn.Free;
  FLastBtn.Free;
  FNextBtn.Free;
  FPrevBtn.Free;
  inherited Destroy;
end;

end.
