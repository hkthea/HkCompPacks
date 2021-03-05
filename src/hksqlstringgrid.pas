unit HKSqlStringGrid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Grids,
  ZDataset;

type

  { THKSqlStringGrid }
  TOnWriteCell=Procedure(aSender:TObject; aFieldName, aValue:String; var aResult:String) of object;

  THKSqlStringGrid = class(TStringGrid)
  private
    FEvenColor: TColor;
    FOddColor: TColor;
    FOnWriteCell: TOnWriteCell;
    FzQuery: TZQuery;
    procedure SetEvenColor(AValue: TColor);
    procedure SetOddColor(AValue: TColor);
    procedure SetOnWriteCell(AValue: TOnWriteCell);
    procedure SetzQuery(AValue: TZQuery);

  protected
    Function DoPrepareField:TStringArray;
    Function DoWriteCell(aFieldName, aValue:String):String;
    procedure DoPrepareCanvas(aCol, aRow: Integer; aState: TGridDrawState);
      override;
  public
    procedure Refresh;
    constructor Create(AOwner: TComponent); override;
  published
    property OddColor:TColor read FOddColor write SetOddColor;
    property EvenColor:TColor read FEvenColor write SetEvenColor;

    property zQuery:TZQuery read FzQuery write SetzQuery;
    property OnWriteCell:TOnWriteCell read FOnWriteCell write SetOnWriteCell;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I hksqlstringgrid_icon.lrs}
  RegisterComponents('HKCompPacks',[THKSqlStringGrid]);
end;

{ THKSqlStringGrid }

procedure THKSqlStringGrid.SetzQuery(AValue: TZQuery);
begin
  if FzQuery=AValue then Exit;
  FzQuery:=AValue;
end;

procedure THKSqlStringGrid.SetOnWriteCell(AValue: TOnWriteCell);
begin
  if FOnWriteCell=AValue then Exit;
  FOnWriteCell:=AValue;
end;

procedure THKSqlStringGrid.SetEvenColor(AValue: TColor);
begin
  if FEvenColor=AValue then Exit;
  FEvenColor:=AValue;
end;

procedure THKSqlStringGrid.SetOddColor(AValue: TColor);
begin
  if FOddColor=AValue then Exit;
  FOddColor:=AValue;
end;

function THKSqlStringGrid.DoPrepareField: TStringArray;
var
  aaa: Integer;
  str: String;
begin
  RowCount:=1;
  FixedCols:=1;
  FixedRows:=1;
  ColCount:=zQuery.FieldCount+1;
  SetLength(Result, ColCount);
  Cells[0, 0]:='#';
  for aaa:=0 to zQuery.FieldCount-1 do
  begin
    str:=zQuery.Fields[aaa].FieldName;
    Cells[aaa+1, 0]:=str;
    Result[aaa]:=str;
  end;
  AutoFillColumns:=True;
  //Options:=;
end;

function THKSqlStringGrid.DoWriteCell(aFieldName, aValue: String): String;
begin
  Result:=aValue;
  if Assigned(OnWriteCell)then OnWriteCell(Self, aFieldName, aValue, Result);
end;

procedure THKSqlStringGrid.DoPrepareCanvas(aCol, aRow: Integer;
  aState: TGridDrawState);
var //isNegative:Boolean;
  aaa: Integer;
begin
  inherited DoPrepareCanvas(aCol, aRow, aState);
  if aRow=0 then exit;

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

procedure THKSqlStringGrid.Refresh;
var
  aaa, bbb: Integer;
  colNames: TStringArray;
  str, vname: String;
begin
  RowCount:=1;
  if Assigned(zQuery) then
  begin
    with zQuery do
    begin
      if Active then Close;
      Open;
      colNames:=DoPrepareField;
      aaa:=1;
      while not EOF do
      begin
        RowCount:=RowCount+1;
        Cells[0, aaa]:=RecNo.ToString;
        for bbb:=0 to Length(colNames)-1 do
        begin
          if colNames[bbb]='' then Continue;
          vname:=colNames[bbb];
          str:=FieldByName(vname).AsString;
          Cells[bbb+1, aaa]:=DoWriteCell(vname, str);
        end;
        Inc(aaa);
        Next;
      end;
    end;
  end;
end;

constructor THKSqlStringGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OddColor:=$00FEFEFE;
  EvenColor:=$00EFEFEF;
end;

end.
