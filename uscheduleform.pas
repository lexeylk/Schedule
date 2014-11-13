unit UScheduleForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, Buttons, metadata, sqldb, DB, querycreate;

type

  TColumnRowName = record
    ID: integer;
    Caption: string;
  end;

  TColumnRowNames = array of TColumnRowName;

  { TScheduleForm }

  TScheduleForm = class(TForm)
    AddFilterBtn: TBitBtn;
    DeleteBtn: TBitBtn;
    ResetBtn: TBitBtn;
    RefreshButton: TButton;
    DataSource: TDataSource;
    Label1: TLabel;
    Label2: TLabel;
    LabelX: TLabel;
    LabelY: TLabel;
    ScrollBox1: TScrollBox;
    SQLQuery: TSQLQuery;
    XFieldCmbBox: TComboBox;
    YFieldCmbBox: TComboBox;
    Grid: TDrawGrid;
    procedure AddFilterBtnClick(Sender: TObject);
    procedure GridDblClick(Sender: TObject);
    procedure GridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure FormCreate(Sender: TObject);
    procedure RefreshButtonClick(Sender: TObject);
  private
    XTitles: array of TColumnRowName;
    YTitles: array of TColumnRowName;
    Items: array of array of array of array of string;
    FCols: array of TColumnInfo;
    function CreateScheduleQuery: string;
    procedure FillItems();
    procedure FillRowTitles;
    procedure FillColumnTitles;
    procedure FillCmbBoxes();
    procedure SetColsRows();
    procedure Refresh();
    function GetLookUpResult(aTable: TTableInfo): TColumnRowNames;
  public
    { public declarations }
  end;

var
  ScheduleForm: TScheduleForm;
  ScheduleTable: TTableInfo;

const
  ScheduleQuery = 'select schedule_items.id as schedule_itemsid, schedule_items.subject_id'
    + ' as schedule_itemssubject_id, schedule_items.subject_type_id as schedule_itemssubject_type_id,'
    + ' schedule_items.professor_id as schedule_itemsprofessor_id, schedule_items.time_id as schedule_itemstime_id,'
    + ' schedule_items.day_id as schedule_itemsday_id, schedule_items.week_id as schedule_itemsweek_id,'
    + ' schedule_items.group_id as schedule_itemsgroup_id, schedule_items.room_id as schedule_itemsroom_id,'
    + ' subjects.name as subjectsname, subject_types.name as subject_typesname, professors.name as professorsname,'
    + ' times.begin_end_time as timesbegin_end_time, days.name as daysname,' +
    ' weeks.name as weeksname, groups.name as groupsname, rooms.name as roomsname from schedule_items' +
    ' inner join subjects on schedule_items.subject_id = subjects.id  inner join subject_types'
    + ' on schedule_items.subject_type_id = subject_types.id  inner join professors on schedule_items.professor_id'
    + ' = professors.id  inner join times on schedule_items.time_id = times.id  inner join days on'
    + ' schedule_items.day_id = days.id  inner join weeks on schedule_items.week_id = weeks.id'
    + ' inner join groups on schedule_items.group_id = groups.id  inner join rooms on' +
    ' schedule_items.room_id = rooms.id  ';

implementation

{$R *.lfm}

{ TScheduleForm }

procedure TScheduleForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to High(ScheduleTable.ColumnInfos) do
    if (ScheduleTable.ColumnInfos[i].VisableColumn) then begin
      SetLength(FCols, Length(FCols) + 1);
      FCols[High(FCols)] := ScheduleTable.ColumnInfos[i];
    end;
  FillCmbBoxes();
  XFieldCmbBox.ItemIndex := 4;
  YFieldCmbBox.ItemIndex := 5;
  Refresh();
end;

procedure TScheduleForm.RefreshButtonClick(Sender: TObject);
begin
  Refresh();
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
      TextOut(aRect.Left, aRect.Top, YTitles[aRow-1].Caption)
    else if aRow = 0 then
      TextOut(aRect.Left, aRect.Top, XTitles[aCol-1].Caption)
    else if ((aRow-1) <= High(Items)) and ((aCol-1) <= High(Items[aRow-1])) then
      for i := 0 to High(Items[aRow-1][aCol-1]) do begin
        for j := 0 to High(Items[aRow-1][aCol-1][i]) do begin
          TextOut(aRect.Left + LeftSpace, aRect.Top + CurrHeight, FCols[j].Caption + ': ' + Items[aRow-1][aCol-1][i][j]);
          CurrHeight += ElemHeight;
        end;
        Line(aRect.Left, aRect.Top + CurrHeight, aRect.Right, aRect.Top + CurrHeight);
        CurrHeight += 5;
      end;
  end;
end;

procedure TScheduleForm.GridDblClick(Sender: TObject);
begin

end;

procedure TScheduleForm.AddFilterBtnClick(Sender: TObject);
begin

end;

function TScheduleForm.CreateScheduleQuery: string;
var
  Table: TTableInfo;
  i, j: integer;
begin
  Result := '';
  {
  for i := 0 to high(FCols) do
  begin
    if (FCols[i].Reference) then
    begin
      Table := GetTableByName(FCols[i].ReferenceTable);
      for j := 0 to (high(FCols)) do
      begin
        SetLength(FCols, length(FCols) + 1);
        FCols[high(FCols)] := FCols[j];
      end;
    end;
  end;

  Result += Format('Select %s', [ScheduleTable.Name]);

  for i := 0 to high(FColumns) do
  begin

  end; }

end;

procedure TScheduleForm.FillItems;
var
  i, j, k: integer;
  YFieldID, XFieldID, ColumnLen: integer;
  XIDCol, YIDCol: TColumnInfo;
  XLen, YLen: integer;
  RefTbl: TTableInfo;
  Query: string;
begin
    Items := nil;

    XLen := length(XTitles);
    YLen := length(YTitles);
    ColumnLen := length(FCols);

    XIDCol := FCols[XFieldCmbBox.ItemIndex];
    YIDCol := FCols[YFieldCmbBox.ItemIndex];

    Query := ScheduleQuery + 'order by ' +
      FCols[YFieldCmbBox.ItemIndex].ReferenceTable + '.ID' +
      ', ' + FCols[XFieldCmbBox.ItemIndex].ReferenceTable + '.ID' + ', Times.ID';

    //ShowMessage(Query);

    SetQuery(SQLQuery, Query);

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

        for k := 0 to High(FCols) do
        begin
          //ShowMessage (ColumnInfos[k].AliasName);
          Items[i][j][high(Items[i][j])][k] :=
            FieldByName(GetTableByName(FCols[k].ReferenceTable).ColumnInfos[1].AliasName).AsString;

          //ShowMessage (Items[i][j][high (Items[i][j])][k]);
        end;

        Next;
      end;

    end;

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
  for i := 0 to High(FCols) do
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

  while (not LookUpQuery.EOF) do
  begin
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)].ID := LookUpQuery.Fields[0].AsInteger;
    Result[High(Result)].Caption := LookUpQuery.Fields[1].AsString;
    LookUpQuery.Next;
  end;

  FreeAndNil(LookUpQuery);
  FreeAndNil(DataSrc);
end;

procedure TScheduleForm.SetColsRows;
const
  RowHeight = 140;
  ColWidth = 300;
  ColHeaderWidth = 80;
  RowHeaderHeight = 30;
begin
  with Grid do begin
    DefaultColWidth := ColWidth;
    DefaultRowHeight := RowHeight;
    RowCount := Length(YTitles) + 1;
    ColCount := Length(XTitles) + 1;
    ColWidths[0] := ColHeaderWidth;
    RowHeights[0] := RowHeaderHeight;
  end;

end;

initialization

  ScheduleTable := GetTableByName('Schedule_items');

end.
