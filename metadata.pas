unit metadata;

{$mode objfpc}{$H+}

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
    ReferenceColumn: string;
    ReferenceTable: string;
  end;

  { TTableInfo }

  TTableInfo = class
    ColumnInfos: array of TColumnInfo;
    Name: string;
    Caption: string;
    constructor Create (aCaption, aName: string);
    function AddColumn (aCaption, aName: string;
             aFieldType: TFieldType; aSize: integer;
             aReference: boolean;
             aReferenceColumn, aReferenceTable: string): TTableInfo;
  end;

  { TListOfTable }

  TListOfTable = class
    TableInfos: array of TTableInfo;
    constructor Create ();
    function AddTable (aCaption, aName: string): TTableInfo;
    function GetTableCaption (): Strings;
  end;

implementation

{ TListOfTable }

constructor TListOfTable.Create;
begin

  AddTable ('Преподаватели', 'Professors').
           AddColumn ('Преподаватель', 'Name', ftString, 110,
                     false, NOT_STRING_VALUE, NOT_STRING_VALUE);
  AddTable ('Предметы', 'Subjects').
           AddColumn ('Предмет', 'Name', ftString, 385,
                     false, NOT_STRING_VALUE, NOT_STRING_VALUE);
  AddTable ('Кабинеты', 'Rooms').
           AddColumn ('Кабинет', 'Name', ftString, 100,
                     false, NOT_STRING_VALUE, NOT_STRING_VALUE).
           AddColumn ('Вместимость', 'Room_size', ftInteger, 80,
                     false, NOT_STRING_VALUE, NOT_STRING_VALUE);
  AddTable ('Группы', 'Groups').
           AddColumn ('Группа', 'Name', ftString, 70,
                     false, NOT_STRING_VALUE, NOT_STRING_VALUE).
           AddColumn ('Количество человек', 'Group_size', ftInteger, 30,
                     false, NOT_STRING_VALUE, NOT_STRING_VALUE);

  AddTable ('Расписание', 'Schedule_items').
           AddColumn ('Преподаватель', 'Professor_id', ftString, 110,
                     true, 'Name', 'Professors').
           AddColumn ('Предмет', 'Subject_id', ftString, 385,
                     true, 'Name', 'Subjects').
           AddColumn ('Тип', 'Subject_type_id', ftString, 30,
                     true, 'Name', 'Subject_types').
           AddColumn ('Кабинет', 'Room_id', ftString, 55,
                     true, 'Name', 'Rooms').
           AddColumn ('Группа', 'Group_id', ftString, 70,
                     true, 'Name', 'Groups').
           AddColumn ('День недели', 'Day_id', ftDate, 90,
                     true, 'Name', 'Days');

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
end;

function TTableInfo.AddColumn(aCaption, aName: string; aFieldType: TFieldType;
  aSize: integer;
  aReference: boolean; aReferenceColumn, aReferenceTable: string): TTableInfo;
begin
  SetLength (ColumnInfos, length (ColumnInfos) + 1);
  with ColumnInfos[high (ColumnInfos)] do begin
    Name := aName; Caption := aCaption;
    Reference := aReference;

    if (Reference) then begin
      ReferenceTable := aReferenceTable;
      ReferenceColumn := aReferenceColumn;
    end;

    Size := aSize; FieldType := aFieldType;
  end;

  Result := Self;
end;

end.

