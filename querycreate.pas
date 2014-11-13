unit querycreate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, FileUtil, Forms, Controls, Graphics, Dialogs, utableform;

procedure ShowTable (aTableForm: TTableForm);
procedure SetQuery (aTableForm: TTableForm);
function CreateQuery (aTableForm: TTableForm): string;
procedure SetCaption (aTableForm: TTableForm);

implementation

procedure ShowTable(aTableForm: TTableForm);
begin
  SetQuery (aTableForm);
  SetCaption (aTableForm);
end;

procedure SetQuery(aTableForm: TTableForm);
begin
  with aTableForm.SQLQuery do begin
    Close;
    Params.Clear;
    SQL.Text := CreateQuery (aTableForm);
    Open;
  end;
end;

function CreateQuery (aTableForm: TTableForm): string;
var
  i: integer;
begin
  Result := 'Select ';

  with aTableForm.MTable do begin
      for i := 0 to High(ColumnInfos) do begin
        if (ColumnInfos[i].Reference) then
          Result += ColumnInfos[i].ReferenceTable + '.' +  ColumnInfos[i].ReferenceColumn
        else
          Result += Name + '.' + ColumnInfos[i].Name + ' ';
        if i < High(ColumnInfos) then Result += ', ';
      end;

    Result += ' From ' + Name;
    for i := 0 to High(ColumnInfos) do begin
      if not ColumnInfos[i].Reference then continue;
      Result += ' inner join ';
      Result += ColumnInfos[i].ReferenceTable;
      Result += ' on ' + Name + '.' + ColumnInfos[i].Name + ' = ' + ColumnInfos[i].ReferenceTable + '.ID';
    end;
  end;

  Result += ';';

  //ShowMessage (Result);
end;

procedure SetCaption(aTableForm: TTableForm);
var
  i: integer;
begin
  with aTableForm do begin
    for i := 0 to high (MTable.ColumnInfos) do begin
      DBGrid.Columns[i].Width := MTable.ColumnInfos[i].Size;
      DBGrid.Columns[i].Title.Caption := MTable.ColumnInfos[i].Caption;
    end;
  end;
end;

end.

