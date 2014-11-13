unit ufilter;

{$mode objfpc}{$H+}{$R+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  DBGrids, ExtCtrls, Buttons, StdCtrls, metadata;

type

  { TCondition }

  TCondition = record
    Caption: string;
    QueryFormat: string;
    ParamFormat: string;
  end;

  { TListOfCondition }

  TListOfCondition = class
    Conditions: array of TCondition;
    constructor Create ();
    procedure AddCondition (aCaption, aQueryFormat, aParamFormat: string);
  end;

  { TFilter }

  TFilter = class
    FPanel: TPanel;
    FComBoColumn: TComboBox;
    FComBoCondition: TComboBox;
    FEdit: TEdit;
    FDestroyCheckBox: TCheckBox;
    FCondition: TListOfCondition;
    Column: string;
    Text: string;
    Query: string;
    Tag: integer;
    FTable: TTableInfo;
    FieldType: TFieldType;
    constructor Create (aOwner: TWinControl; aTable: TTableInfo);
    destructor Destroy; override;
    procedure FilterInterfae (aOwner: TWinControl);
    function GetQuery (aIndex: integer): string;
    function GetCondition: string;
  end;

  { TListOfFilters }

  TListOfFilters = class
    Filters: array of TFilter;
    destructor Destroy; override;
    procedure AddFilter (aOwner: TWinControl; aTable: TTableInfo);
    procedure DeleteFilter (aIndex: integer);
    procedure Clear();
    function CreateFQuery: string;
    function Count(): Integer;
  end;


implementation

var
  ListOfCondition: TListOfCondition;

{ TConditions }

constructor TListOfCondition.Create;
begin
  AddCondition('>', ' %s.%s > :p%s', '%s');
  AddCondition('<', ' %s.%s < :p%s', '%s');
  AddCondition('>=', ' %s.%s >= :p%s', '%s');
  AddCondition('<=', ' %s.%s <= :p%s', '%s');
  AddCondition('=', ' %s.%s = :p%s', '%s');
  AddCondition('начинается на', ' %s.%s like :p%s', '%s%%');
  AddCondition('заканчивается на', ' %s.%s like :p%s', '%%%s');
  AddCondition('содержит', ' %s.%s like :p%s', '%%%s%%');
end;

procedure TListOfCondition.AddCondition(aCaption, aQueryFormat,
  aParamFormat: string);
begin
  SetLength (Conditions, length (Conditions) + 1);
  Conditions[high (Conditions)].Caption := aCaption;
  Conditions[high (Conditions)].QueryFormat := aQueryFormat;
  Conditions[high (Conditions)].ParamFormat := aParamFormat;
end;

{ TListOfFilters }

destructor TListOfFilters.Destroy;
var
  i: integer;
begin
  Clear();
  inherited Destroy;
end;

procedure TListOfFilters.AddFilter(aOwner: TWinControl; aTable: TTableInfo);
begin
  SetLength (Filters, length (Filters) + 1);
  Filters[high (Filters)] := TFilter.Create (aOwner, aTable);
  Filters[high (Filters)].Tag := high (Filters);
end;

procedure TListOfFilters.DeleteFilter(aIndex: integer);
var
  i: integer;
begin
  FreeAndNil (Filters[aIndex]);

  for i := aIndex to high (Filters) - 1 do begin
    Filters[i] := Filters[i + 1];
    Filters[i].Tag := i;
  end;

  SetLength (Filters, length (Filters) - 1);
end;

procedure TListOfFilters.Clear;
var
  i: Integer;
begin
  if Count() <> 0 then begin
     for i := high (Filters) downto 0 do begin
       FreeAndNil (Filters[i]);
       SetLength (Filters, length (Filters) - 1);
     end;
  end;
end;

function TListOfFilters.CreateFQuery: string;
var
  i: integer;
begin
  Result += ' Where ';
  for i := 0 to high (Filters) do begin
    Result += Filters[i].GetQuery (i);
    if (i <> high (Filters)) then Result += ' and ';
  end;

  //ShowMessage (Result);
end;

function TListOfFilters.Count: Integer;
begin
  Result := Length(Filters);
end;

{ TFilter }

constructor TFilter.Create(aOwner: TWinControl; aTable: TTableInfo);
begin
  FCondition := TListOfCondition.Create();
  FTable := aTable;
  FilterInterfae (aOwner);
end;

destructor TFilter.Destroy;
begin
  FreeAndNil (FEdit);
  FreeAndNil (FComBoColumn);
  FreeAndNil (FComBoCondition);
  FreeAndNil (FDestroyCheckBox);
  FreeAndNil (FPanel);
  inherited Destroy;
end;

procedure TFilter.FilterInterfae(aOwner: TWinControl);
var
  i: integer;
begin
  FPanel := TPanel.Create (aOwner);
  with FPanel do begin
    Parent := aOwner; Align := alTop;
  end;

  FComBoColumn := TComboBox.Create (FPanel);
  with FComBoColumn do begin
    Parent := FPanel; ReadOnly := true;
    Top := 11; Left := 20;
    Height := 23; Width := 83;
    for i := 0 to high (FTable.ColumnInfos) do
       if (FTable.ColumnInfos[i].VisableColumn = true) then
          Items.Add (FTable.ColumnInfos[i].Caption);
  end;

  FComBoCondition := TComboBox.Create (FPanel);
  with FComBoCondition do begin
    Parent := FPanel; ReadOnly := true;
    Top := 11; Left := 125;
    Height := 23; Width := 83;
    for i := 0 to high (FCondition.Conditions) do
      Items.Add (FCondition.Conditions[i].Caption);
  end;

  FEdit := TEdit.Create (FPanel);
  with FEdit do begin
    Parent := FPanel;
    Top := 11; Left := 230;
    Height := 23; Width := 83;
  end;

  FDestroyCheckBox := TCheckBox.Create (FPanel);
  with FDestroyCheckBox do begin
    Parent := FPanel;
    Top := 13; Left := 330;
    Height := 23; Width := 23;
  end;
end;

function TFilter.GetQuery(aIndex: integer): string;
begin
  if (not FTable.ColumnInfos[FComBoColumn.ItemIndex + 1].Reference) then
     Result += Format(FCondition.Conditions[FComBoCondition.ItemIndex].QueryFormat,
         [FTable.Name, FTable.ColumnInfos[FComBoColumn.ItemIndex + 1].Name,
         IntToStr (aIndex)])
     else
     Result += Format(FCondition.Conditions[FComBoCondition.ItemIndex].QueryFormat,
         [FTable.ColumnInfos[FComBoColumn.ItemIndex + 1].ReferenceTable,
         FTable.ColumnInfos[FComBoColumn.ItemIndex + 1].ReferenceColumn,
         IntToStr (aIndex)]);
  //ShowMessage (Result);
end;

function TFilter.GetCondition: string;
begin
  Result := FCondition.Conditions[FComBoCondition.ItemIndex].Caption;
end;

end.

