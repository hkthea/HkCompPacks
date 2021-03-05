unit HKStringGrid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Grids,
  HKCompPacksTypes;

type
  THKGridColumn=class;

  TOnBtnAction=procedure(Sender:TObject; const vCol:THKGridColumn;
    const aRows:TStrings)of object;

  TColumnType=(CTTxt, CTNumber, CTButton, CTDateTimeISO, CTBtnEdit, CTBtnDelete);
  THKStringGrid=class;
  { THKGridColumn }

  THKGridColumn=class(TCollectionItem)
  private
    FAlignment: TAlignment;
    FColumnType: TColumnType;
    FFieldName: String;
    FImageIndex: Integer;
    FImageList: TImageList;
    FTitle: String;
    FWidth: Integer;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetColumnType(AValue: TColumnType);
    procedure SetFieldName(AValue: String);
    procedure SetImageIndex(AValue: Integer);
    procedure SetImageList(AValue: TImageList);
    procedure SetTitle(AValue: String);
    procedure SetWidth(AValue: Integer);
  public
    constructor Create(ACollection: TCollection); override;
  published
    property FieldName:String read FFieldName write SetFieldName;
    property Title:String read FTitle write SetTitle;
    property ColumnType:TColumnType read FColumnType write SetColumnType;
    property Alignment:TAlignment read FAlignment write SetAlignment;
    property Width:Integer read FWidth write SetWidth default 80;
    property ImageList:TImageList read FImageList write SetImageList;
    property ImageIndex:Integer read FImageIndex write SetImageIndex;
  end;

  { THKGridColumns }

  THKGridColumns=class(TCollection)
  private
    fOwnerGrid:THKStringGrid;
  public
    function Add: THKGridColumn;
    procedure clear;
    procedure UpdateColumn;
    constructor Create(OwnerGrid:THKStringGrid; AItemClass: TCollectionItemClass);
  end;

  { THKStringGrid }

  THKStringGrid = class(TCustomStringGrid)
  private
    FEvenColor: TColor;
    FHKGridColumns: THKGridColumns;
    FNegativeColor: TColor;
    FNegativeColorActive: Boolean;
    FOddColor: TColor;
    FOnBtnCustomClicked: TOnBtnAction;
    FOnBtnEditClicked: TOnBtnAction;
    FOnBtnRemoveClicked: TOnBtnAction;
    procedure SetEvenColor(AValue: TColor);
    procedure SetHKGridColumns(AValue: THKGridColumns);
    procedure SetNegativeColor(AValue: TColor);
    procedure SetNegativeColorActive(AValue: Boolean);
    procedure SetOddColor(AValue: TColor);
    procedure SetOnBtnCustomClicked(AValue: TOnBtnAction);
    procedure SetOnBtnEditClicked(AValue: TOnBtnAction);
    procedure SetOnBtnRemoveClicked(AValue: TOnBtnAction);
  protected
    procedure DoEditClicked(aCol: THKGridColumn);
    procedure DoRemoveClicked(aCol: THKGridColumn);
    procedure DoCustomeClicked(aCol: THKGridColumn);
    procedure DoEditBtnClicked(Sender: TObject; aCol, aRow:Integer);
    procedure DoPrepareCanvas(aCol, aRow: Integer; aState: TGridDrawState);
      override;
    function GetNegativeData(aCol, aRow:Integer):Boolean;virtual;
    //procedure SelectEditor; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property HKGridColumns:THKGridColumns read FHKGridColumns write SetHKGridColumns;

    property OnBtnEditClicked:TOnBtnAction read FOnBtnEditClicked write SetOnBtnEditClicked;
    property OnBtnRemoveClicked:TOnBtnAction read FOnBtnRemoveClicked write SetOnBtnRemoveClicked;
    property OnBtnCustomClicked:TOnBtnAction read FOnBtnCustomClicked write SetOnBtnCustomClicked;
    property OddColor:TColor read FOddColor write SetOddColor;
    property EvenColor:TColor read FEvenColor write SetEvenColor;
    property NegativeColor:TColor read FNegativeColor write SetNegativeColor;
    property NegativeColorActive:Boolean read FNegativeColorActive write SetNegativeColorActive;

    property Align;
    property AlternateColor;
    property Anchors;
    property AutoAdvance;
    property AutoEdit;
    property AutoFillColumns;
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle;
    property CellHintPriority;
    property Color;
    property ColCount;
    property ColumnClickSorts;
    property Columns;
    property Constraints;
    property DefaultColWidth;
    property DefaultDrawing;
    property DefaultRowHeight;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property FixedColor;
    property FixedCols;
    property FixedRows;
    property Flat;
    property Font;
    property GridLineWidth;
    property HeaderHotZones;
    property HeaderPushZones;
    property ImageIndexSortAsc;
    property ImageIndexSortDesc;
    property MouseWheelOption;
    property Options;
    property Options2;
    property ParentBiDiMode;
    property ParentColor default false;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RangeSelectMode;
    property RowCount;
    property ScrollBars;
    property ShowHint;
    property TabAdvance;
    property TabOrder;
    property TabStop;
    property TitleFont;
    property TitleImageList;
    property TitleStyle;
    property UseXORFeatures;
    property Visible;
    property VisibleColCount;
    property VisibleRowCount;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I hkstringgrid_icon.lrs}
  RegisterComponents('HKCompPacks',[THKStringGrid]);
end;

{ THKStringGrid }

procedure THKStringGrid.SetHKGridColumns(AValue: THKGridColumns);
begin
  if FHKGridColumns=AValue then Exit;
  FHKGridColumns:=AValue;
end;

procedure THKStringGrid.SetNegativeColor(AValue: TColor);
begin
  if FNegativeColor=AValue then Exit;
  FNegativeColor:=AValue;
end;

procedure THKStringGrid.SetNegativeColorActive(AValue: Boolean);
begin
  if FNegativeColorActive=AValue then Exit;
  FNegativeColorActive:=AValue;
end;

procedure THKStringGrid.SetEvenColor(AValue: TColor);
begin
  if FEvenColor=AValue then Exit;
  FEvenColor:=AValue;
end;

procedure THKStringGrid.SetOddColor(AValue: TColor);
begin
  if FOddColor=AValue then Exit;
  FOddColor:=AValue;
end;

procedure THKStringGrid.SetOnBtnCustomClicked(AValue: TOnBtnAction);
begin
  if FOnBtnCustomClicked=AValue then Exit;
  FOnBtnCustomClicked:=AValue;
end;

procedure THKStringGrid.SetOnBtnEditClicked(AValue: TOnBtnAction);
begin
  if FOnBtnEditClicked=AValue then Exit;
  FOnBtnEditClicked:=AValue;
end;

procedure THKStringGrid.SetOnBtnRemoveClicked(AValue: TOnBtnAction);
begin
  if FOnBtnRemoveClicked=AValue then Exit;
  FOnBtnRemoveClicked:=AValue;
end;

procedure THKStringGrid.DoEditClicked(aCol:THKGridColumn);
begin
  if Assigned(OnBtnEditClicked) then OnBtnEditClicked(Self, aCol, Rows[Row]);
end;

procedure THKStringGrid.DoRemoveClicked(aCol:THKGridColumn);
begin
  if Assigned(OnBtnRemoveClicked) then OnBtnRemoveClicked(Self, aCol, Rows[Row]);
end;

procedure THKStringGrid.DoCustomeClicked(aCol:THKGridColumn);
begin
  if Assigned(OnBtnCustomClicked) then OnBtnCustomClicked(Self, aCol, Rows[Row]);
end;

procedure THKStringGrid.DoEditBtnClicked(Sender: TObject; aCol, aRow: Integer);
var
  vCol: THKGridColumn;
begin
  vCol:=HKGridColumns.Items[aCol-1] as THKGridColumn;
  case vCol.ColumnType of
    CTBtnDelete:DoRemoveClicked(vCol);
    CTBtnEdit:DoEditClicked(vCol);
    CTButton:DoCustomeClicked(vCol);
    else Exit;
  end;
end;

procedure THKStringGrid.DoPrepareCanvas(aCol, aRow: Integer;
  aState: TGridDrawState);
var isNegative:Boolean;
  aaa: Integer;
begin
  inherited DoPrepareCanvas(aCol, aRow, aState);
  if aRow=0 then exit;

  isNegative:=False;

  if NegativeColorActive then
  begin
    for aaa:=0 to ColCount-1 do
    begin
      isNegative:=GetNegativeData(aaa, aRow);
      if isNegative then break;
    end;
  end;

  if isNegative then
  begin
    Canvas.Font.Color:=NegativeColor;
    Canvas.Font.Style:=[fsBold];
  end
  else
    Canvas.Font.Color:=clBlack;

  if (aRow mod 2)=1 then
  begin
    Canvas.Brush.Color:=OddColor;
  end
  else
  begin
    Canvas.Brush.Color:=EvenColor;
  end;
end;

function THKStringGrid.GetNegativeData(aCol, aRow: Integer): Boolean;
var
  vCol: THKGridColumn;
  str: String;
  xxx: Longint;
begin
  Result:=False;
  if aCol=0 then exit;

  vCol:=HKGridColumns.Items[aCol-1] as THKGridColumn;

  if vCol=nil then exit;
  if vCol.ColumnType<>CTNumber then exit;
  str:=Cells[aCol, aRow].Replace('.','',[rfReplaceAll]).Replace(',','',[rfReplaceAll]);
  TryStrToInt(str,xxx);
  Result:=xxx < 0;
end;

constructor THKStringGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHKGridColumns:=THKGridColumns.Create(Self, THKGridColumn);
  OnButtonClick:=@DoEditBtnClicked;
  //OddColor:=clWhite;
  OddColor:=$00FEFEFE;
  EvenColor:=$00EFEFEF;
  NegativeColor:=$0000D7;
  NegativeColorActive:=True;
  Options:=[goHorzLine, goVertLine, goFixedHorzLine, goFixedVertLine];
  MouseWheelOption:=mwGrid;
end;

destructor THKStringGrid.Destroy;
begin
  Clear;
  FreeAndNil(FHKGridColumns);
  inherited Destroy;
end;

{ THKGridColumns }

function THKGridColumns.Add: THKGridColumn;
begin
  result := THKGridColumn( inherited add );
end;

procedure THKGridColumns.clear;
begin
  BeginUpdate;
  inherited Clear;
  EndUpdate;
end;

procedure THKGridColumns.UpdateColumn;
var
  aaa: Integer;
  vCol: TGridColumn;
  temp: THKGridColumn;
begin
  if Count=0 then exit;
  //oldFixedRow:=fOwnerGrid.FixedRows;
  fOwnerGrid.RowCount:=1;
  fOwnerGrid.FixedCols:=0;
  fOwnerGrid.FixedRows:=1;
  fOwnerGrid.Columns.Clear;
  vCol:=fOwnerGrid.Columns.Add;
  vCol.Title.Caption:='#';
  vCol.Title.Alignment:=taRightJustify;
  vCol.Alignment:=taRightJustify;
  vCol.Width:=50;
  for aaa:=0 to Count-1 do
  begin
    vCol:=fOwnerGrid.Columns.Add;
    temp:=THKGridColumn(Items[aaa]);
    vCol.Title.Caption:= temp.Title;
    vCol.Alignment:=temp.Alignment;
    vCol.Title.Alignment:=temp.Alignment;
    vCol.Width:=temp.Width;
    if temp.ColumnType in [CTBtnDelete, CTBtnEdit, CTButton] then
    begin
      vCol.ButtonStyle:=cbsButtonColumn;
    end;
  end;
end;

constructor THKGridColumns.Create(OwnerGrid: THKStringGrid;
  AItemClass: TCollectionItemClass);
begin
  fOwnerGrid:=OwnerGrid;
  Inherited Create(AItemClass);
end;

{ THKGridColumn }

procedure THKGridColumn.SetAlignment(AValue: TAlignment);
begin
  if FAlignment=AValue then Exit;
  FAlignment:=AValue;
end;

procedure THKGridColumn.SetColumnType(AValue: TColumnType);
begin
  if FColumnType=AValue then Exit;
  FColumnType:=AValue;
end;

procedure THKGridColumn.SetFieldName(AValue: String);
begin
  if FFieldName=AValue then Exit;
  FFieldName:=AValue;
end;

procedure THKGridColumn.SetImageIndex(AValue: Integer);
begin
  if FImageIndex=AValue then Exit;
  FImageIndex:=AValue;
end;

procedure THKGridColumn.SetImageList(AValue: TImageList);
begin
  if FImageList=AValue then Exit;
  FImageList:=AValue;
end;

procedure THKGridColumn.SetTitle(AValue: String);
begin
  if FTitle=AValue then Exit;
  FTitle:=AValue;
end;

procedure THKGridColumn.SetWidth(AValue: Integer);
begin
  if FWidth=AValue then Exit;
  FWidth:=AValue;
end;

constructor THKGridColumn.Create(ACollection: TCollection);
begin
  FAlignment:=taLeftJustify;
  FColumnType:=CTTxt;
  FWidth:=80;
  inherited Create(ACollection);
end;

end.
