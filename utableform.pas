unit utableform;

{$mode objfpc}{$H+}{$R+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  DBGrids, ExtCtrls, Buttons, metadata, ufilter, querycreate,
  ueditform;

type

  { TTableForm }

  TTableForm = class(TForm)
    AddFilterBitBtn: TBitBtn;
    InsertDBBitBtn: TBitBtn;
    ResetBitBtn: TBitBtn;
    FiltersClose: TBitBtn;
    DestroyBitBtn: TBitBtn;
    SQLTransaction: TSQLTransaction;
    UpdateBitBtn: TBitBtn;
    FilterBitBtn: TBitBtn;
    Datasource: TDatasource;
    DBGrid: TDBGrid;
    FilterScrollBox: TScrollBox;
    SQLQuery: TSQLQuery;
    FTimer: TTimer;
    STimer: TTimer;
    procedure AddFilterBitBtnClick(Sender: TObject);
    procedure DBGridDblClick(Sender: TObject);
    procedure DBGridTitleClick(Column: TColumn);
    procedure InsertDBBitBtnClick(Sender: TObject);
    procedure DestroyBitBtnClick(Sender: TObject);
    procedure FilterBitBtnClick(Sender: TObject);
    procedure FiltersCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FTimerTimer(Sender: TObject);
    procedure ResetBitBtnClick(Sender: TObject);
    procedure STimerTimer(Sender: TObject);
    procedure UpdateBitBtnClick(Sender: TObject);
  private
    Order: Boolean;
    EditForms: array of TEditForm;
  public
    MTable: TTableInfo;
    constructor Create (aOwner: TControl; aTable: TTableInfo);
  end;

var
  TableForm: TTableForm;
  ListOfFilters: TListOfFilters;

implementation

{$R *.lfm}

{ TTableForm }

procedure TTableForm.FTimerTimer(Sender: TObject);
begin
  if (Width <= 814) then Width := Width + 10 else FTimer.Enabled := False;
end;

procedure TTableForm.ResetBitBtnClick(Sender: TObject);
begin
  ListOfFilters.Clear();
  ShowTable (SQLQuery, DBGrid, MTable);
end;

procedure TTableForm.STimerTimer(Sender: TObject);
begin
  if (Width >= 409) then Width := Width - 10 else STimer.Enabled := False;
end;

procedure TTableForm.UpdateBitBtnClick(Sender: TObject);
var
  i: integer;
  flag: Boolean;
  Param: array of string;
begin
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
        ShowFilterTable (SQLQuery, DBGrid, MTable, CreateFQuery, Param);
      end;
    end
    else
        ShowMessage ('Добавьте фильтры');
  end;
end;

procedure TTableForm.AddFilterBitBtnClick(Sender: TObject);
begin
  FTimer.Enabled := true;
  AddFilterBitBtn.Enabled := false;
end;

procedure TTableForm.DBGridDblClick(Sender: TObject);
var
  i: integer;
  NewForm: TEditForm;
  flag: Boolean;
begin
  {flag := true;
  for i := 0 to high (EditForms) do begin
    if (SQLQuery.Fields[0].AsInteger = EditForms[i].ID) then begin
      flag := false;
      break;
    end;
  end;

  if (flag) then begin
    SetLength (EditForms, length (EditForms) + 1);
    EditForms[high (EditForms)] := TEditForm.Create (TableForm, MTable, SQLQuery, DBGrid, SQLTransaction, false);
    EditForms[high (EditForms)].Show;
  end;}
  NewForm := TEditForm.Create (TableForm, MTable, SQLQuery, DBGrid, SQLTransaction, false);
  NewForm.Show;
end;

procedure TTableForm.DBGridTitleClick(Column: TColumn);
var
  Query: string;
begin
  Order := not Order;
  Query := '';
  if (ListOfFilters.Count() <> 0) then
    Query := ListOfFilters.CreateFQuery;

  ShowSortTable (SQLQuery, DBGrid, MTable, Query, Column.Index, Order);
end;

procedure TTableForm.InsertDBBitBtnClick(Sender: TObject);
var
  NewForm: TEditForm;
begin
  NewForm := TEditForm.Create (TableForm, MTable, SQLQuery, DBGrid, SQLTransaction, true);
  NewForm.Show;
end;

procedure TTableForm.DestroyBitBtnClick(Sender: TObject);
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

procedure TTableForm.FilterBitBtnClick(Sender: TObject);
begin
  ListOfFilters.AddFilter (FilterScrollBox, MTable);
end;

procedure TTableForm.FiltersCloseClick(Sender: TObject);
begin
  STimer.Enabled := true;
  AddFilterBitBtn.Enabled := true;
  //if (length (ListOfFilters.Filters) <> 0) then ListOfFilters.Destroy;
  Datasource.DataSet.Active := true;
  //ShowTable (SQLQuery, DBGrid, MTable);
end;

procedure TTableForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if (length (ListOfFilters.Filters) <> 0) then ListOfFilters.Destroy;
end;

constructor TTableForm.Create(aOwner: TControl; aTable: TTableInfo);
begin
  inherited Create(aOwner);
  MTable := aTable;
  ListOfFilters := TListOfFilters.Create;
  FTimer.Enabled := false;
  STimer.Enabled := false;
end;

end.

