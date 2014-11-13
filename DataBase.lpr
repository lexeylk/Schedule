program DataBase;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, metadata, utableform, querycreate, ufilter, ueditform,
UScheduleForm
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TTableForm, TableForm);
  Application.CreateForm(TEditForm, EditForm);
  Application.CreateForm(TScheduleForm, ScheduleForm);
  Application.Run;
end.

