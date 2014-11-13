unit utableform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  DBGrids, metadata;

type

  { TTableForm }

  TTableForm = class(TForm)
    Datasource: TDatasource;
    DBGrid: TDBGrid;
    SQLQuery: TSQLQuery;
  private
    { private declarations }
  public
    MTable: TTableInfo;
    constructor Create (aOwner: TControl; aTable: TTableInfo);
  end;

var
  TableForm: TTableForm;

implementation

{$R *.lfm}

{ TTableForm }

constructor TTableForm.Create(aOwner: TControl; aTable: TTableInfo);
begin
  inherited Create(aOwner);
  MTable := aTable;
end;

end.

