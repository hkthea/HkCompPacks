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

procedure Register;

implementation


procedure Register;
begin
  {$I hkpaginantion_icon.lrs}
  RegisterComponents('HKCompPacks',[THKPaginantion]);
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
  UpdatePagination;
end;

procedure THKPaginantion.PageChanged(Sender: TObject);
begin
  UpdatePagination;
end;

procedure THKPaginantion.PrevBtnClick(Sender: TObject);
begin
  FPaginationModel.PrevPage;
end;


procedure THKPaginantion.LastBtnClick(Sender: TObject);
begin
  FPaginationModel.LastPage;
end;

procedure THKPaginantion.FirstBtnClick(Sender: TObject);
begin
  FPaginationModel.FirstPage;
end;

procedure THKPaginantion.NextBtnClick(Sender: TObject);
begin
  FPaginationModel.NextPage;
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
  FPrevBtn.Enabled:=FPaginationModel.CurrentPage>1;
  FFirstBtn.Enabled:=FPaginationModel.CurrentPage>1;
  FNextBtn.Enabled:=FPaginationModel.CurrentPage<FPaginationModel.TotalPage;
  FLastBtn.Enabled:=FPaginationModel.CurrentPage<FPaginationModel.TotalPage;
end;

procedure THKPaginantion.UpdateInfo;
var
  recNo, lastRecNo: Integer;
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
