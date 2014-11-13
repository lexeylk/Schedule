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
    EditFormat: string;
  end;

  { TConditions }

  TListOfCondition = class
    Conditions: array of TCondition;
    constructor Create ();
    procedure AddCondition (aCaption, aQueryFormat, aEditFormat: string);
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
    Param: string;
    constructor Create (aOwner: TWinControl; aTable: TTableInfo);
    destructor Destroy; override;
    procedure FilterInterfae (aOwner: TWinControl);
    function GetQuery: string;
  end;

  { TListOfFilters }

  TListOfFilters = class
    Filters: array of TFilter;
    constructor Create ();
    destructor Destroy; override;
    procedure AddFilter (aOwner: TWinControl; aTable: TTableInfo);
    procedure DeleteFilter (aIndex: integer);
    function CreateFQuery: string;
  end;


implementation

var
  ListOfCondition: TListOfCondition;

{ TConditions }

constructor TListOfCondition.Create;
begin
  AddCondition('>', ' %s.%s > :%s ', '%s');
  AddCondition('<', ' %s.%s < :%s ', '%s');
  AddCondition('>=', ' %s.%s >= :%s ', '%s');
  AddCondition('<=', ' %s.%s <= :%s ', '%s');
  AddCondition('=', ' %s.%s = :%s ', '%s');
  AddCondition('начинается на', ' %s.%s like :%s ', '%s%%');
  AddCondition('заканчивается на', ' %s.%s like :%s ', '%%%s');
  AddCondition('содержит', ' %s.%s like :%s ', '%%%s%%');
end;

procedure TListOfCondition.AddCondition(aCaption, aQueryFormat, aEditFormat: string);
begin
  SetLength (Conditions, length (Conditions) + 1);
  Conditions[high (Conditions)].Caption := aCaption;
  Conditions[high (Conditions)].QueryFormat := aQueryFormat;
  Conditions[high (Conditions)].EditFormat := aEditFormat;
end;

{ TListOfFilters }

constructor TListOfFilters.Create;
begin
  inherited Create;
end;

destructor TListOfFilters.Destroy;
var
  i: integer;
begin
  inherited Destroy;
  for i := 0 to high (Filters) do
    FreeAndNil (Filters[i]);
end;

procedure TListOfFilters.AddFilter(aOwner: TWinControl; aTable: TTableInfo);
begin
  SetLength (Filters, length (Filters) + 1);
  Filters[high (Filters)] := TFilter.Create (aOwner, aTable);
  Filters[high (Filters)].Tag := high (Filters);
  Filters[high (Filters)].Param := 'p' + IntToStr(high (Filters));
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

function TListOfFilters.CreateFQuery: string;
var
  i: integer;
begin
  Result += ' where ';
  for i := 0 to high (Filters) - 1 do begin
    Result += Filters[i].GetQuery;
    Result += ' and ';
  end;
  Result += Filters[high (Filters)].GetQuery;
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

function TFilter.GetQuery: string;
begin
  if (not FTable.ColumnInfos[FComBoColumn.ItemIndex].Reference) then
     Result += Format(FCondition.Conditions[FComBoCondition.ItemIndex].QueryFormat,
         [FTable.Name, FTable.ColumnInfos[FComBoColumn.ItemIndex].Name,
         Param])
     else
     Result += Format(FCondition.Conditions[FComBoCondition.ItemIndex].QueryFormat,
         [FTable.ColumnInfos[FComBoColumn.ItemIndex].ReferenceTable,
         FTable.ColumnInfos[FComBoColumn.ItemIndex].ReferenceColumn,
         Param]);
  ShowMessage (Result);
end;

end.

