unit querycreate;

{$mode objfpc}{$H+}{$R+}

interface

uses
  Classes, SysUtils, sqldb, FileUtil, Forms, Controls, DBGrids, Graphics,
  Dialogs, metadata;

procedure ShowTable (aSQLQuery: TSQLQuery; aDBGrid: TDBGrid; aTable: TTableInfo);
procedure ShowFilterTable (aSQLQuery: TSQLQuery; aDBGrid: TDBGrid;
                          aTable: TTableInfo; aFQuery: string);
procedure ShowSortTable (aSQLQuery: TSQLQuery; aDBGrid: TDBGrid;
                          aTable: TTableInfo; aFQuery: string; aIndex: integer;
                          aOrder: Boolean);
procedure SetQuery (aSQLQuery: TSQLQuery; aQuery: string);
function CreateQuery (aTable: TTableInfo): string;
procedure SetCaption (aDBGrid: TDBGrid; aTable: TTableInfo);

implementation

procedure ShowTable(aSQLQuery: TSQLQuery; aDBGrid: TDBGrid; aTable: TTableInfo);
begin
  SetQuery (aSQLQuery, CreateQuery (aTable));
  SetCaption (aDBGrid, aTable);
end;

procedure ShowFilterTable (aSQLQuery: TSQLQuery; aDBGrid: TDBGrid;
                          aTable: TTableInfo; aFQuery: string);
begin
  SetQuery (aSQLQuery, CreateQuery (aTable) + aFQuery);
  SetCaption (aDBGrid, aTable);
end;

procedure ShowSortTable(aSQLQuery: TSQLQuery; aDBGrid: TDBGrid;
  aTable: TTableInfo; aFQuery: string; aIndex: integer; aOrder: Boolean);
begin
  if (aOrder) then
    SetQuery (aSQLQuery, CreateQuery (aTable) + aFQuery + ' Order By ' +
              aTable.Name + '.' + aTable.ColumnInfos[aIndex].Name)
  else
    SetQuery (aSQLQuery, CreateQuery (aTable) + aFQuery + ' Order By ' +
              aTable.Name + '.' + aTable.ColumnInfos[aIndex].Name + ' Desc ');

  SetCaption (aDBGrid, aTable);
end;

procedure SetQuery(aSQLQuery: TSQLQuery; aQuery: string);
begin
  //ShowMessage (aQuery);
  with aSQLQuery do begin
    Close;
    Params.Clear;
    SQL.Text := aQuery;
    Open;
  end;
end;

function CreateQuery (aTable: TTableInfo): string;
var
  i: integer;
begin
  Result := 'Select ';

  with aTable do begin
    for i := 0 to High(ColumnInfos) do begin
      if (ColumnInfos[i].Reference) then
        Result += ColumnInfos[i].ReferenceTable + '.' +
        ColumnInfos[i].ReferenceColumn
      else
        Result += Name + '.' + ColumnInfos[i].Name + ' ';
      if i < High(ColumnInfos) then Result += ', ';
    end;

    Result += ' From ' + Name;

    for i := 0 to High(ColumnInfos) do begin
      if not ColumnInfos[i].Reference then continue;
      Result += ' inner join ';
      Result += ColumnInfos[i].ReferenceTable;
      Result += ' on ' + Name + '.' + ColumnInfos[i].Name + ' = ' +
           ColumnInfos[i].ReferenceTable + '.ID';
    end;
  end;

  //Result += ';';

  //ShowMessage (Result);
end;

procedure SetCaption(aDBGrid: TDBGrid; aTable: TTableInfo);
var
  i: integer;
begin
  for i := 0 to high (aTable.ColumnInfos) do begin
    aDBGrid.Columns[i].Width := aTable.ColumnInfos[i].Size;
    aDBGrid.Columns[i].Title.Caption := aTable.ColumnInfos[i].Caption;
    aDBGrid.Columns[i].ReadOnly := true;
  end;
end;

end.

