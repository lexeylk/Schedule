unit ueditform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, DBGrids, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, DbCtrls, metadata, querycreate;

type

  { TDBEditor }

  TDBEditor = class
    EPanel: TPanel;
    ELabel: TLabel;
    EDBEdit: TEdit;
    CurrentCaption: string;
    IDs: array of Integer;
    EDBComBo: TComboBox;
    EColumn: TColumnInfo;
    EDBSQLQuery: TSQLQuery;
    ColumnIndex: integer;
  private
    IsInsert: Boolean;
    procedure CreateInrterface (aOwner: TWinControl; aReference: Boolean);
  public
    constructor Create (aColumn: TColumnInfo; aOwner: TWinControl;
      aCaption: string; aReference: Boolean; aSQLQuery: TSQLQuery;
      aIndex: integer; aIsInsert: Boolean);
    destructor Destroy; override;
  end;

  { TEditForm }

  TEditForm = class(TForm)
    DeleteBtn: TButton;
    ChanelBtn: TButton;
    OKBtn: TButton;
    CancelBtn: TButton;
    procedure CancelBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure OKBtnClick(Sender: TObject);
  private
    { private declarations }
  public
    DBEditors: array of TDBEditor;
    Params: array of string;
    ETable: TTableInfo;
    ID: integer;
    MSQLQuery: TSQLQuery;
    MDBGrid: TDBGrid;
    IsInsert: Boolean;
    MTransaction: TSQLTransaction;
    constructor Create(TheOwner: TComponent; aTable: TTableInfo;
      aSQLQuery: TSQLQuery; aDBGrid: TDBGrid; aTransaction: TSQLTransaction;
      aIsInsert: Boolean);
    procedure AddEditor(aColumn: TColumnInfo; aOwner: TWinControl;
      aCaption: string; aReference: Boolean; aSQLQuery: TSQLQuery;
      aIndex: integer; aIsInsert: Boolean);
    function DeleteQuery: string;
    function InsertEdit: string;
    function UpdateEdit: string;
    procedure UpdateQuery;
    procedure InsertQuery;
    destructor Destroy; override;
  end;

var
  EditForm: TEditForm;

implementation

{ TDBEditor }

constructor TDBEditor.Create(aColumn: TColumnInfo; aOwner: TWinControl;
  aCaption: string; aReference: Boolean; aSQLQuery: TSQLQuery; aIndex: integer;
  aIsInsert: Boolean);
begin
  EDBSQLQuery := aSQLQuery;
  EColumn := aColumn;
  ColumnIndex := aIndex;
  IsInsert := aIsInsert;
  if (not IsInsert) then CurrentCaption := aCaption else CurrentCaption := '';
  CreateInrterface (aOwner, aReference);
end;

procedure TDBEditor.CreateInrterface(aOwner: TWinControl; aReference: Boolean);
var
  LookUpQuery: TSQLQuery;
  DataSrc: TDataSource;
  RefTable: TTableInfo;
begin
  EPanel := TPanel.Create (aOwner);
  with EPanel do begin
    Parent := aOwner;
    Align := alTop;
  end;

  ELabel := TLabel.Create (EPanel);
  with ELabel do begin
    Parent := EPanel;
    Top := 20; Left := 30;
    Caption := EColumn.Caption;
  end;

  if (aReference) then begin
     LookUpQuery := TSQLQuery.Create (nil);
     DataSrc := TDataSource.Create (nil);

     LookUpQuery.DataSource := DataSrc;

     LookUpQuery.DataBase := EDBSQLQuery.DataBase;
     LookUpQuery.Transaction := EDBSQLQuery.Transaction;LookUpQuery := TSQLQuery.Create (nil);
     DataSrc := TDataSource.Create (nil);

     LookUpQuery.DataSource := DataSrc;

     LookUpQuery.DataBase := EDBSQLQuery.DataBase;
     LookUpQuery.Transaction := EDBSQLQuery.Transaction;

     EDBComBo := TComboBox.Create (EPanel);
     with EDBComBo do begin
       Parent := EPanel;
       Top := 18; Left := 125;
       Width := 105;
       ReadOnly := true;
       Caption := CurrentCaption;

       RefTable := GetTableByName(EColumn.ReferenceTable);

       if RefTable = nil then begin
         ShowMessage('Не найдена таблица: ' + EColumn.ReferenceTable);
         Exit;
       end;

       SetQuery(LookUpQuery, CreateQuery(RefTable));

       LookUpQuery.First;
       while (not LookUpQuery.EOF) do begin
         //ShowMessage (EDBSQLQuery.SQL.Text);
         Items.Add (LookUpQuery.Fields[1].AsString);
         SetLength(IDs, Length(IDs) + 1);
         IDs[High(IDs)] := LookUpQuery.FieldByName('ID').AsInteger;
         LookUpQuery.Next;
       end;
     end;
     FreeAndNil(LookUpQuery);
     FreeAndNil(DataSrc);
  end else begin
    EDBEdit := TEdit.Create (EPanel);
    with EDBEdit do begin
      Parent := EPanel;
      Top := 18; Left := 125;
      Width := 105;
      Caption := CurrentCaption;
    end;
  end;
end;

destructor TDBEditor.Destroy;
begin
  inherited Destroy;
  FreeAndNil (ELabel);
  FreeAndNil (EDBComBo);
  FreeAndNil (EPanel);
end;

{$R *.lfm}


{ TEditForm }

procedure TEditForm.OKBtnClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to high (DBEditors) do begin
    SetLength (Params, length (Params) + 1);
    Params[i] := DBEditors[i].EDBEdit.Caption;
  end;

  if (not IsInsert) then UpdateQuery else InsertQuery;
  MTransaction.Commit;
  ShowTable(MSQLQuery, MDBGrid, ETable);
  Close;
end;

procedure TEditForm.CancelBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TEditForm.DeleteBtnClick(Sender: TObject);
var
  BtnClick: integer;
begin
  ShowUpdateTable (MSQLQuery, DeleteQuery, Params);
  BtnClick := MessageDlg ('Вы уверенны, что хотите удалить поле?', mtConfirmation,
  mbOKCancel, 0);
  if (BtnClick = mrOK) then MTransaction.Commit else MTransaction.Rollback;
  ShowTable(MSQLQuery, MDBGrid, ETable);
  Close;
end;

procedure TEditForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

constructor TEditForm.Create(TheOwner: TComponent; aTable: TTableInfo;
  aSQLQuery: TSQLQuery; aDBGrid: TDBGrid; aTransaction: TSQLTransaction;
  aIsInsert: Boolean);
var
  i: integer;
begin
  inherited Create(TheOwner);
  ETable := aTable;
  MSQLQuery := aSQLQuery;
  MDBGrid := aDBGrid;
  MTransaction := aTransaction;

  ID := MSQLQuery.Fields[0].AsInteger;
  IsInsert := aIsInsert;

  for i := 0 to high (ETable.ColumnInfos) do begin
    if (ETable.ColumnInfos[i].VisableColumn = true) then begin
       AddEditor (ETable.ColumnInfos[i], Self, MSQLQuery.Fields[i].AsString,
       ETable.ColumnInfos[i].Reference, MSQLQuery, i, aIsInsert);
    end;
  end;
end;

procedure TEditForm.AddEditor(aColumn: TColumnInfo; aOwner: TWinControl;
  aCaption: string; aReference: Boolean; aSQLQuery: TSQLQuery; aIndex: integer;
  aIsInsert: Boolean);
begin
  SetLength (DBEditors, length(DBEditors) + 1);
  DBEditors[high (DBEditors)] := TDBEditor.Create (aColumn, aOwner, aCaption,
  aReference, aSQLQuery, aIndex, aIsInsert);
end;

function TEditForm.DeleteQuery: string;
begin
  Result += Format ('Delete from %s Where %s.ID = %s',
  [ETable.Name, ETable.Name, IntToStr (ID)]);
end;

function TEditForm.InsertEdit: string;
var
  i: integer;
begin
  Result += Format ('Insert into %s', [ETable.Name]);
  Result += Format (' Values (next value for %s, ', [ETable.GenerateName]);

  for i := 0 to high (DBEditors) do begin
    with DBEditors[i] do begin
      if (EColumn.Reference) then
        Result += QuotedStr(IntToStr(IDs[EDBComBo.ItemIndex]))
      else
        Result += ':p' + IntToStr (i);
      if (i <> high (DBEditors)) then Result += ', ' else Result += ')';
    end;
  end;

  //ShowMessage (Result);
end;

function TEditForm.UpdateEdit: string;
var
  i: integer;
begin
  Result += Format ('Update %s Set', [ETable.Name]);

  for i := 0 to high (DBEditors) do begin
    with (DBEditors[i]) do begin
      if (EColumn.Reference) then
        Result += Format (' %s = %s', [EColumn.Name, QuotedStr (IntToStr(IDs[EDBComBo.ItemIndex]))])
      else
        Result += Format (' %s = :p%s', [EColumn.Name, IntToStr (i)]);
      end;

    if (i <> high (DBEditors)) then Result += ', ' else Result += ' ';
  end;

  Result += Format (' Where %s.ID = %s', [ETable.Name, IntToStr (ID)]);
  //ShowMessage (Result);
end;

procedure TEditForm.UpdateQuery();
begin
  ShowUpdateTable (MSQLQuery, UpdateEdit, Params);
end;

procedure TEditForm.InsertQuery;
begin
  ShowUpdateTable (MSQLQuery, InsertEdit, Params);
end;

destructor TEditForm.Destroy;
var
  i: integer;
begin

  for i := high (DBEditors) downto 0 do
    FreeAndNil(DBEditors[i]);

  inherited Destroy;
end;

end.

