unit main;

{$mode objfpc}{$H+} {$R+}

interface

uses
  Classes, SysUtils, IBConnection, sqldb, FileUtil, Forms, Controls, Graphics,
  Dialogs, metadata, utableform, Menus, querycreate;

type

  { TMainForm }

  TMainForm = class(TForm)
    IBConnection: TIBConnection;
    MainMenu: TMainMenu;
    MenuFile: TMenuItem;
    CaptionExit: TMenuItem;
    MenuAbout: TMenuItem;
    MenuTable: TMenuItem;
    SQLTransaction: TSQLTransaction;
    procedure CaptionExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuAboutClick(Sender: TObject);
    procedure NewFormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;
  ListOfTable: TListOfTable;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.CaptionExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: integer;
  MenuItem: TMenuItem;
  Names: Strings;
begin
  IBConnection.Connected := true;

  ListOfTable := TListOfTable.Create ();

  Names := ListOfTable.GetTableCaption ();
  for i := 0 to high (Names) do begin
    MenuItem := TMenuItem.Create (MainMenu);
    MenuItem.Caption := Names[i];
    MainMenu.Items.Items[1].Add (MenuItem);
    MenuItem.Tag := i;
    MenuItem.OnClick := @NewFormCreate;
  end;

end;

procedure TMainForm.MenuAboutClick(Sender: TObject);
begin
  ShowMessage ('Цой Алексей Б8103а');
end;

procedure TMainForm.NewFormCreate(Sender: TObject);
var
  NewForm: TTableForm;
  index: integer;
begin
  index := TMenuItem(Sender).Tag;
  NewForm := TTableForm.Create (MainForm, ListOfTable.TableInfos[index]);
  NewForm.Caption := ListOfTable.GetTableCaption[index];
  NewForm.MTable := ListOfTable.TableInfos[index];
  NewForm.Show;
  with NewForm do
    ShowTable (SQLQuery, DBGrid, MTable);
end;

end.

