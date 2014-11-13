unit metadata;

{$mode objfpc}{$H+}{$R+}

interface

uses
  Classes, SysUtils, db, sqldb, FileUtil, Forms, Controls, Graphics, Dialogs;

const
  NOT_STRING_VALUE = '';

type

  Strings = array of string;

  TColumnInfo = record
    Name: string;
    Caption: string;
    Size: integer;
    FieldType: TFieldType;
    Reference: boolean;
    VisableColumn: Boolean;
    ReferenceColumn: string;
    ReferenceTable: string;
    AliasName: string;
  end;

  { TTableInfo }

  TTableInfo = class
    ColumnInfos: array of TColumnInfo;
    Name: string;
    Caption: string;
    GenerateName: string;
    constructor Create (aCaption, aName: string);
    function AddColumn (aCaption, aName: string;
      aFieldType: TFieldType; aSize: integer;
      aReference: boolean; aVisible: Boolean;
      aReferenceColumn, aReferenceTable: string): TTableInfo;
  end;

  { TListOfTable }

  TListOfTable = class
    TableInfos: array of TTableInfo;
    constructor Create ();
    function AddTable (aCaption, aName: string): TTableInfo;
    function GetTableCaption (): Strings;
  end;

  function GetTableByName(aName: String): TTableInfo;

var
  ListOfTable: TListOfTable;

implementation

function GetTableByName(aName: String): TTableInfo;
var
  i:  Integer;
begin
  with ListOfTable do
       for i := 0 to High(TableInfos) do
           if LowerCase(aName) = LowerCase(TableInfos[i].Name) then
              Exit(TableInfos[i]);

  Result := nil;
end;

{ TListOfTable }

constructor TListOfTable.Create();
begin

  AddTable ('Преподаватели', 'Professors').
           AddColumn ('ID', 'ID', ftInteger, 10,
                     false, false, NOT_STRING_VALUE, NOT_STRING_VALUE).
           AddColumn ('Преподаватель', 'Name', ftString, 110,
                     false, true, NOT_STRING_VALUE, NOT_STRING_VALUE);
  AddTable ('Предметы', 'Subjects').
           AddColumn ('ID', 'ID', ftInteger, 10,
                     false, false, NOT_STRING_VALUE, NOT_STRING_VALUE).
           AddColumn ('Предмет', 'Name', ftString, 385,
                     false, true, NOT_STRING_VALUE, NOT_STRING_VALUE);
  AddTable ('Кабинеты', 'Rooms').
           AddColumn ('ID', 'ID', ftInteger, 10,
                     false, false, NOT_STRING_VALUE, NOT_STRING_VALUE).
           AddColumn ('Кабинет', 'Name', ftString, 100,
                     false, true, NOT_STRING_VALUE, NOT_STRING_VALUE).
           AddColumn ('Вместимость', 'Room_size', ftInteger, 80,
                     false, true, NOT_STRING_VALUE, NOT_STRING_VALUE);
  AddTable ('Группы', 'Groups').
           AddColumn ('ID', 'ID', ftInteger, 10,
                     false, false, NOT_STRING_VALUE, NOT_STRING_VALUE).
           AddColumn ('Группа', 'Name', ftString, 70,
                     false, true, NOT_STRING_VALUE, NOT_STRING_VALUE).
           AddColumn ('Количество человек', 'Group_size', ftInteger, 30,
                     false, true, NOT_STRING_VALUE, NOT_STRING_VALUE);
  AddTable ('Дни недели', 'Days').
           AddColumn('ID', 'ID', ftInteger, 10,
                     false, false, NOT_STRING_VALUE, NOT_STRING_VALUE).
           AddColumn('Название', 'Name', ftString, 100,
                     false, true, NOT_STRING_VALUE, NOT_STRING_VALUE);
  AddTable ('Типы занятий', 'Subject_Types').
           AddColumn('ID', 'ID', ftInteger, 10,
                     false, false, NOT_STRING_VALUE, NOT_STRING_VALUE).
           AddColumn('Название', 'Name', ftString, 30,
                     false, true, NOT_STRING_VALUE, NOT_STRING_VALUE);
  AddTable ('Неделя', 'Weeks').
           AddColumn('ID', 'ID', ftInteger, 10,
                     false, false, NOT_STRING_VALUE, NOT_STRING_VALUE).
           AddColumn('Тип недели', 'Name', ftString, 100,
                     false, true, NOT_STRING_VALUE, NOT_STRING_VALUE);
  AddTable ('Расписание звонков', 'Times').
           AddColumn('ID', 'ID', ftInteger, 10,
                     false, false, NOT_STRING_VALUE, NOT_STRING_VALUE).
           AddColumn('Время', 'Begin_End_Time', ftString, 100,
                     false, true, NOT_STRING_VALUE, NOT_STRING_VALUE);

  AddTable ('Расписание', 'Schedule_items').
           AddColumn ('ID', 'ID', ftInteger, 10,
                     false, false, NOT_STRING_VALUE, NOT_STRING_VALUE).
           AddColumn ('Предмет', 'Subject_id', ftString, 385,
                     true, true, 'Name', 'Subjects').
           AddColumn ('Тип', 'Subject_type_id', ftString, 30,
                     true, true, 'Name', 'Subject_types').
           AddColumn ('Преподаватель', 'Professor_id', ftString, 110,
                     true, true, 'Name', 'Professors').
           AddColumn ('Время', 'Time_id', ftString, 90,
                     true, true, 'Begin_End_Time', 'Times').
           AddColumn ('День недели', 'Day_id', ftDate, 90,
                     true, true, 'Name', 'Days').
           AddColumn ('Группа', 'Group_id', ftString, 70,
                     true, true, 'Name', 'Groups').
           AddColumn ('Кабинет', 'Room_id', ftString, 55,
                     true, true, 'Name', 'Rooms').
           AddColumn ('Неделя', 'Week_id', ftString, 90,
                     true, true, 'Name', 'Weeks');

end;

function TListOfTable.AddTable(aCaption, aName: string): TTableInfo;
begin
  Result := TTableInfo.Create (aCaption, aName);
  SetLength (TableInfos, length (TableInfos) + 1);
  TableInfos[high (TableInfos)] := Result;
end;

function TListOfTable.GetTableCaption: Strings;
var
  i: integer;
begin
  SetLength (Result, length (TableInfos));
  for i := 0 to high (TableInfos) do
      Result[i] := TableInfos[i].Caption;
end;

{ TTableInfo }

constructor TTableInfo.Create(aCaption, aName: string);
begin
  Caption := aCaption;
  Name := aName;
  GenerateName := Name + '_GEN';
end;

function TTableInfo.AddColumn(aCaption, aName: string; aFieldType: TFieldType;
  aSize: integer; aReference: boolean; aVisible: Boolean; aReferenceColumn,
  aReferenceTable: string): TTableInfo;
begin
  SetLength (ColumnInfos, length (ColumnInfos) + 1);
  with ColumnInfos[high (ColumnInfos)] do begin
    Name := aName; Caption := aCaption;
    Reference := aReference;
    VisableColumn := aVisible;


    if (Reference) then begin
      ReferenceTable := aReferenceTable;
      ReferenceColumn := aReferenceColumn;
    end;

    AliasName := Self.Name + aName;

    Size := aSize; FieldType := aFieldType;
  end;

  Result := Self;
end;

initialization

ListOfTable := TListOfTable.Create();

end.

