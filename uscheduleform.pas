unit UScheduleForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, Buttons, CheckLst, ExtCtrls, metadata, sqldb, DB, querycreate,
  ufilter;

type

  TCurrColRow = record
    aCol: integer;
    aRow: integer;
  end;

  TColumnRowName = record
    ID: integer;
    Caption: string;
  end;

  TColumnRowNames = array of TColumnRowName;

  { TScheduleForm }

  TScheduleForm = class(TForm)
    AddFilterBtn: TBitBtn;
    CheckGroup: TCheckGroup;
    DeleteBtn: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    ResetBtn: TBitBtn;
    RefreshButton: TButton;
    DataSource: TDataSource;
    LabelX: TLabel;
    LabelY: TLabel;
    FilterScrollBox: TScrollBox;
    SQLQuery: TSQLQuery;
    XFieldCmbBox: TComboBox;
    YFieldCmbBox: TComboBox;
    Grid: TDrawGrid;
    procedure AddFilterBtnClick(Sender: TObject);
    procedure CheckGroupItemClick(Sender: TObject; Index: integer);
    procedure DeleteBtnClick(Sender: TObject);
    procedure GridDblClick(Sender: TObject);
    procedure GridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure FormCreate(Sender: TObject);
    procedure RefreshButtonClick(Sender: TObject);
    procedure ResetBtnClick(Sender: TObject);
  private
    XTitles: array of TColumnRowName;
    YTitles: array of TColumnRowName;
    Items: array of array of array of array of string;
    FCols: array of TColumnInfo;
    FFlag: Boolean;
    CurrentHeight: integer;
    VisibleColumn: array of Boolean;
    CurrColRow: TCurrColRow;
    function CreateScheduleQuery: string;
    procedure FillItems;
    procedure FillRowTitles;
    procedure FillColumnTitles;
    procedure FillCmbBoxes;
    procedure SetColsRows;
    procedure Refresh;
    function GetLookUpResult(aTable: TTableInfo): TColumnRowNames;
  public
    { public declarations }
  end;

var
  ScheduleForm: TScheduleForm;
  ScheduleTable: TTableInfo;
  ListOfFilters: TListOfFilters;

implementation

{$R *.lfm}

{ TScheduleForm }

procedure TScheduleForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to high(ScheduleTable.ColumnInfos) do
    if (ScheduleTable.ColumnInfos[i].VisableColumn) then begin
      SetLength(FCols, length(FCols) + 1);
      FCols[high(FCols)] := ScheduleTable.ColumnInfos[i];
    end;

  FillCmbBoxes;
  for i := 0 to high (FCols) do begin
    SetLength(VisibleColumn, length (VisibleColumn) + 1);
    VisibleColumn[high (VisibleColumn)] := true;
    CheckGroup.Items.AddStrings (FCols[i].Caption);
    CheckGroup.Checked[i] := true;
  end;

  FFlag := False;
  ListOfFilters := TListOfFilters.Create;
  XFieldCmbBox.ItemIndex := 4;
  YFieldCmbBox.ItemIndex := 5;
  Refresh;
end;

procedure TScheduleForm.RefreshButtonClick(Sender: TObject);
begin
  Refresh;
end;

procedure TScheduleForm.ResetBtnClick(Sender: TObject);
begin
  ListOfFilters.Clear();
  Refresh;
end;

procedure TScheduleForm.Refresh;
begin
  FillRowTitles;
  FillColumnTitles;
  FillItems;
  SetColsRows;
  Grid.Invalidate;
end;

procedure TScheduleForm.GridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
const
  ElemHeight = 20;
  LeftSpace = 5;
var
  i, j: Integer;
  CurrHeight: Integer;
begin
  CurrHeight := 0;
  if (aRow = 0) and (aCol = 0) then
    Exit;

  with Grid.Canvas do begin
    Pen.Color := clBlack;
    if aCol = 0 then
      TextOut (aRect.Left, aRect.Top, YTitles[aRow - 1].Caption)
    else
      if aRow = 0 then
        TextOut (aRect.Left, aRect.Top, XTitles[aCol - 1].Caption)
    else
      if (((aRow - 1) <= high (Items)) and ((aCol - 1) <= high (Items[aRow - 1]))) then
        for i := 0 to high (Items[aRow - 1][aCol - 1]) do begin
          for j := 0 to high (Items[aRow - 1][aCol - 1][i]) do begin
            if (VisibleColumn[j]) then begin
              TextOut (aRect.Left + LeftSpace, aRect.Top + CurrHeight, FCols[j].Caption + ': ' + Items[aRow - 1][aCol - 1][i][j]);
              CurrHeight += ElemHeight;
            end;
          end;
          Line (aRect.Left, aRect.Top + CurrHeight, aRect.Right, aRect.Top + CurrHeight);
          CurrHeight += 5;
          CurrentHeight := CurrHeight;
        end;
  end;
end;

procedure TScheduleForm.GridDblClick(Sender: TObject);
begin
  with Grid do begin
    if ((CurrColRow.aCol = Col) and (CurrColRow.aRow = Row)) then begin
      RowHeights[Row] := 160;
      CurrColRow.aCol := 0;
      CurrColRow.aRow := 0;
    end
    else begin
      RowHeights[Row] := 165 * length (Items[Row - 1][Col - 1]);
      CurrColRow.aCol := Col;
      CurrColRow.aRow := Row;
    end;
  end;
end;

procedure TScheduleForm.AddFilterBtnClick(Sender: TObject);
begin
  ListOfFilters.AddFilter (FilterScrollBox, ScheduleTable);
end;

procedure TScheduleForm.CheckGroupItemClick(Sender: TObject; Index: integer);
begin
  VisibleColumn[Index] := TCheckGroup (Sender).Checked[Index];
  Grid.Invalidate;
end;

procedure TScheduleForm.DeleteBtnClick(Sender: TObject);
var
  i: integer;
  flag: Boolean;
begin
  flag := false;
  while (not flag) do begin

    flag := true;
    for i := 0 to high (ListOfFilters.Filters) do begin
      if (ListOfFilters.Filters[i].FDestroyCheckBox.Checked) then begin
        ListOfFilters.DeleteFilter (i);
        flag := false;
        break;
      end;
    end;

  end;
end;

function TScheduleForm.CreateScheduleQuery: string;
var
  Table: TTableInfo;
  i, j: integer;
begin
  Result := '';

  Result += 'Select';

  with (ScheduleTable) do begin
    for i := 0 to high (ColumnInfos) do
      Result += Format (' %s.%s as %s,', [Name, ColumnInfos[i].Name, ColumnInfos[i].AliasName]);

    for i := 0 to high (ColumnInfos) do begin
      if (ColumnInfos[i].Reference) then begin
        Table := GetTableByName (ColumnInfos[i].ReferenceTable);
        with (Table) do begin
          for j := 0 to high (ColumnInfos) do begin
            if (ColumnInfos[j].VisableColumn) then begin
              if (i <> high (ScheduleTable.ColumnInfos)) then
                Result += Format (' %s.%s as %s,', [Name, ColumnInfos[j].Name,
                  ColumnInfos[j].AliasName])
              else
                Result += Format (' %s.%s as %s', [Name, ColumnInfos[j].Name,
                  ColumnInfos[j].AliasName]);
            end;
          end;
        end;
      end;
    end;

    Result += Format (' from %s ', [ScheduleTable.Name]);

    for i := 0 to high (ColumnInfos) do begin
      with (ColumnInfos[i]) do
        if (VisableColumn) then
          Result += Format ('inner join %s on %s.%s = %s.ID ',
            [ReferenceTable, ScheduleTable.Name, Name, ReferenceTable]);
    end;

  end;
end;

procedure TScheduleForm.FillItems;
var
  i, j, k: integer;
  YFieldID, XFieldID, ColumnLen: integer;
  XIDCol, YIDCol: TColumnInfo;
  XLen, YLen: integer;
  RefTbl: TTableInfo;
  Query: string;
  Param: array of string;
  flag: Boolean;
begin
    Items := nil;

    XLen := length(XTitles);
    YLen := length(YTitles);
    ColumnLen := length(FCols);

    XIDCol := FCols[XFieldCmbBox.ItemIndex];
    YIDCol := FCols[YFieldCmbBox.ItemIndex];

    Query += CreateScheduleQuery;

    if (FFlag) then begin
      with ListOfFilters do begin
       if (Count() <> 0) then begin
           for i := 0 to high (Filters) do begin
             flag := true;
             if ((Filters[i].FComBoColumn.Caption = '') or
             (Filters[i].FComBoCondition.Caption = '') or
             (Filters[i].FEdit.Caption = '')) then begin
               ShowMessage ('Заполните все поля');
               flag := false;
               break;
             end;
           end;
         if (flag) then begin
           for i := 0 to high (Filters) do begin
             with (Filters[i]) do begin
               SetLength (Param, length (Param) + 1);
               Param[i] := Format (FCondition.Conditions[FComBoCondition.ItemIndex].ParamFormat,
               [FEdit.Caption]);
             end;
           end;
           Query += CreateFQuery;
         end;
       end
       else
         ShowMessage ('Добавьте фильтры');
      end;

    end;

    Query += ' order by ' +
      FCols[YFieldCmbBox.ItemIndex].ReferenceTable + '.ID' +
      ', ' + FCols[XFieldCmbBox.ItemIndex].ReferenceTable + '.ID' + ', Times.ID';


    //ShowMessage(Query);

    if (ListOfFilters.Count() = 0) then
      SetQuery(SQLQuery, Query)
    else
      SetParamQuery(SQLQuery, Query, Param);

    XFieldID := SQLQuery.FieldByName(XIDCol.AliasName).Index;
    YFieldID := SQLQuery.FieldByName(YIDCol.AliasName).Index;

    SetLength(Items, YLen);
    for i := 0 to YLen - 1 do
      SetLength(Items[i], XLen);

    i := 0;
    j := 0;

    with SQLQuery do
    begin

      while (not EOF) do
      begin
        //ShowMessage(Fields[YFieldID].AsString);
        while (Fields[YFieldID].AsInteger > YTitles[i].ID) do
        begin
          Inc(i);
          j := 0;
        end;

        //ShowMessage(Fields[XFieldID].AsString);
        while (Fields[XFieldID].AsInteger > XTitles[j].ID) do
          Inc(j);

        SetLength(Items[i][j], length(Items[i][j]) + 1);
        SetLength(Items[i][j][high(Items[i][j])], ColumnLen);

        for k := 0 to high(FCols) do
        begin
          //ShowMessage (ColumnInfos[k].AliasName);
          Items[i][j][high(Items[i][j])][k] :=
            FieldByName(GetTableByName(FCols[k].ReferenceTable).ColumnInfos[1].AliasName).AsString;

          //ShowMessage (Items[i][j][high (Items[i][j])][k]);
        end;

        Next;
      end;

    end;

    FFlag := true;
end;

procedure TScheduleForm.FillRowTitles;
var
  aTable: TTableInfo;
begin
  aTable := GetTableByName(FCols[YFieldCmbBox.ItemIndex].ReferenceTable);
  //ShowMessage(FCols[YFieldCmbBox.ItemIndex].ReferenceTable);
  //ShowMessage(FCols[YFieldCmbBox.ItemIndex].Caption);
  YTitles := GetLookUpResult(aTable);
end;

procedure TScheduleForm.FillColumnTitles;
var
  aTable: TTableInfo;
begin
  aTable := GetTableByName(FCols[XFieldCmbBox.ItemIndex].ReferenceTable);
  XTitles := GetLookUpResult(aTable);
end;

procedure TScheduleForm.FillCmbBoxes;
var
  i: integer;
begin
  XFieldCmbBox.Items.Clear;
  YFieldCmbBox.Items.Clear;
  for i := 0 to high(FCols) do
   // if ColumnInfos[i].VisableColumn then
    begin
      XFieldCmbBox.Items.Add(FCols[i].Caption);
      YFieldCmbBox.Items.Add(FCols[i].Caption);
    end;
end;

function TScheduleForm.GetLookUpResult(aTable: TTableInfo): TColumnRowNames;
var
  LookUpQuery: TSQLQuery;
  DataSrc: TDataSource;
begin
  LookUpQuery := TSQLQuery.Create(nil);
  DataSrc := TDataSource.Create(nil);

  LookUpQuery.DataSource := DataSrc;

  LookUpQuery.DataBase := SQLQuery.DataBase;
  LookUpQuery.Transaction := SQLQuery.Transaction;

  SetQuery(LookUpQuery, CreateQuery(aTable));

  while (not LookUpQuery.EOF) do begin
    SetLength(Result, length(Result) + 1);
    Result[high(Result)].ID := LookUpQuery.Fields[0].AsInteger;
    Result[high(Result)].Caption := LookUpQuery.Fields[1].AsString;
    LookUpQuery.Next;
  end;

  FreeAndNil(LookUpQuery);
  FreeAndNil(DataSrc);
end;

procedure TScheduleForm.SetColsRows;
begin
  with Grid do begin
    DefaultColWidth := 300;
    DefaultRowHeight := 160;
    RowCount := length(YTitles) + 1;
    ColCount := length(XTitles) + 1;
    ColWidths[0] := 80;
    RowHeights[0] := 30;
  end;
  CurrentHeight := 160;
end;

initialization

  ScheduleTable := GetTableByName('Schedule_items');

end.
