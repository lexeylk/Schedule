unit UExport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UScheduleForm, metadata, ufilter, comobj, FileUtil, Dialogs;

type

  TSchExportFormat = class
    procedure ExportToFile(aData: TScheduleData; XTitles, YTitles: array of TColumnRowName;
      XHeaderCol, YHeaderCol: TColumnInfo; Filters: TListOfFilters; Columns: TColumnInfos;
      aFileName: String); virtual; abstract;
  end;

  TSchExportClass = class of TSchExportFormat;

  TExportFormat = record
    ExportClass: TSchExportClass;
    Caption: String;
    FileFormat: String;
  end;

  { TSchExportToHTML }

  TSchExportToHTML = class(TSchExportFormat)
    procedure ExportToFile(aData: TScheduleData; XTitles, YTitles: array of TColumnRowName;
      XHeaderCol, YHeaderCol: TColumnInfo; Filters: TListOfFilters; Columns: TColumnInfos;
      aFileName: String); override;
  end;

  { TSchExportToExcel }

  TSchExportToExcel = class(TSchExportFormat)
    procedure ExportToFile(aData: TScheduleData; XTitles, YTitles: array of TColumnRowName;
      XHeaderCol, YHeaderCol: TColumnInfo; Filters: TListOfFilters; Columns: TColumnInfos;
      aFileName: String); override;
  end;

  { TSchExportFormatList }

  TSchExportFormatList = class
  private
    Formats: array of TExportFormat;
    procedure Add(aFormat: TSchExportClass; aCaption, aFileFormat: String);
  public
    procedure ExportToFile(aData: TScheduleData; XTitles, YTitles: array of TColumnRowName;
      XHeaderCol, YHeaderCol: TColumnInfo; Filters: TListOfFilters; Columns: TColumnInfos);
  end;

var
  SchExport: TSchExportFormatList;

implementation

{ TSchExportFormatList }

procedure TSchExportFormatList.Add(aFormat: TSchExportClass; aCaption,
  aFileFormat: String);
begin
  SetLength(Formats, Length(Formats) + 1);
  with Formats[High(Formats)] do begin
    ExportClass := aFormat;
    Caption := aCaption;
    FileFormat := aFileFormat;
  end;
end;

procedure TSchExportFormatList.ExportToFile(aData: TScheduleData; XTitles,
  YTitles: array of TColumnRowName; XHeaderCol, YHeaderCol: TColumnInfo;
  Filters: TListOfFilters; Columns: TColumnInfos);
var
  SaveDialog: TSaveDialog;
  i: Integer;
  FileFormats: String;
  FileName: String;
  ExpFormat: TSchExportFormat;
begin
  SaveDialog := TSaveDialog.Create(nil);
  with SaveDialog do begin
    Title := 'Экспорт расписания в файл';
    FileFormats := '';
    for i := 0 to High(Formats) do begin
      FileFormats += Format('%s | *.%s ', [Formats[i].Caption, Formats[i].FileFormat]);
      if i <> High(Formats) then
        FileFormats += '|';
    end;
    Filter := FileFormats;
  end;

  if SaveDialog.Execute then begin
    ExpFormat := Formats[SaveDialog.FilterIndex - 1].ExportClass.Create();
    FileName := UTF8ToSys(SaveDialog.FileName);
    ExpFormat.ExportToFile(aData, XTitles, YTitles, XHeaderCol, YHeaderCol, Filters, Columns, FileName);
  end;

end;

{ TSchExportToExcel }

procedure TSchExportToExcel.ExportToFile(aData: TScheduleData; XTitles,
  YTitles: array of TColumnRowName; XHeaderCol, YHeaderCol: TColumnInfo;
  Filters: TListOfFilters; Columns: TColumnInfos; aFileName: String);
var
  ExcelApp: Variant;
  i, j, X, Y, k, l: Integer;
  s: String;
begin
  ExcelApp := CreateOleObject('Excel.Application');
  ExcelApp.Application.EnableEvents := False;
  ExcelApp.Workbooks.Add;
  ExcelApp.Worksheets[1].Name := WideString(UTF8ToSys('Расписание'));

  X := High(XTitles);
  Y := High(YTitles);

  for i := 0 to High(aData) do begin
    for j := 0 to High(aData[i]) do begin
      if (i = 0) and (j = 0) then Continue;
      if (i = 0) xor (j = 0) then begin
        ExcelApp.Cells[i + 1, j+ 1].Font.Bold := True;
        ExcelApp.Cells[i + 1, j + 1].HorizontalAlignment := 3;
      end;

      ExcelApp.Cells[i + 1, j + 1].VerticalAlignment := 1;
      ExcelApp.Cells[i + 1, j + 1].ColumnWidth := 50;
      ExcelApp.Cells[i + 1, j + 1].WrapText := True;
      ExcelApp.Cells[i + 1, j + 1].Borders.LineStyle := 1;

      if j = 0 then begin
        ExcelApp.Cells[i + 1, j + 1] :=  WideString(UTF8ToSys(YTitles[i].Caption));
        Continue;
      end;

      if i = 0 then begin
        ExcelApp.Cells[i + 1, j + 1] :=  WideString(UTF8ToSys(XTitles[j].Caption));
        Continue;
      end;

      s := '';
      for k := 0 to High(aData[i][j]) do begin
        for l := 0 to High(ScheduleForm.VisibleColumn) do begin
          if not ScheduleForm.VisibleColumn[l] then Continue;
          s += Format('%s: %s' + PChar(#10), [Columns[l].Caption, aData[i][j][k].Item[l]])
        end;
        s += '-----------------------------------------' + PChar(#10);
      end;
      ExcelApp.Cells[i + 1, j + 1] :=  WideString(UTF8ToSys(s));
    end;
  end;

  ExcelApp.Cells[Y + 3, 1].Font.Bold := True;
  ExcelApp.Cells[Y + 3, 1].WrapText := True;
  ExcelApp.Cells[Y + 3, 1].HorizontalAlignment := 3;
  ExcelApp.Cells[Y + 3, 1] := WideString(UTF8ToSys('Активные фильтры'));
  ExcelApp.Cells[Y + 4, 1] := WideString(UTF8ToSys(Format('1. По горизонтали: %s', [XHeaderCol.Caption])));
  ExcelApp.Cells[Y + 5, 1] := WideString(UTF8ToSys(Format('2. По вертикали: %s', [YHeaderCol.Caption])));

  k := 3;
  for i := 0 to High(Filters.Filters) do
    with Filters.Filters[i] do begin
      ExcelApp.Cells[Y + 6 + i, 1] := WideString(UTF8ToSys(Format('%d. %s %s %s', [k, Column, GetCondition, FEdit.Caption])));
      Inc(k);
    end;

  ExcelApp.DisplayAlerts := False;
  ExcelApp.Worksheets[1].SaveAs(WideString(UTF8ToSys(aFileName)));
  ExcelApp.Application.Quit;
end;

{ TSchExportToHTML }

procedure TSchExportToHTML.ExportToFile(aData: TScheduleData; XTitles,
  YTitles: array of TColumnRowName; XHeaderCol, YHeaderCol: TColumnInfo;
  Filters: TListOfFilters; Columns: TColumnInfos; aFileName: String);
var
  HeaderFile, Fout: Text;
  tmpStr: String;
  i, j, k, l: Integer;
begin
  AssignFile(HeaderFile, 'data\header.dat');
  Reset(HeaderFile);
  AssignFile(fout, aFileName);
  Rewrite(fout);

  while not EOF(HeaderFile) do begin
    ReadLn(HeaderFile, tmpStr);
    WriteLn(fout, tmpStr);
  end;
  CloseFile(HeaderFile);

  k := 0;
  WriteLn(fout, '<fieldset><legend><strong>Активные фильтры</strong></legend><ol>');
  WriteLn(fout, Format('<li><i>По горизонтали:</i> %s</li>', [XHeaderCol.Caption]));
  WriteLn(fout, Format('<li><i>По вертикали:</i> %s</li>', [YHeaderCol.Caption]));

  for i := 0 to High(Filters.Filters) do
    with Filters.Filters[i] do begin
      WriteLn(fout, Format('<li> <i>%s %s</i> %s </li>', [Column, GetCondition, FEdit.Caption]));
      Inc(k);
    end;

  WriteLn(fout, '</ol></fieldset><table border = "0" cellspacing = "0" cellpadding = "0">');

  for i := 0 to High(aData) do begin
    Writeln(fout, '<tr valign = "top">');
    for j := 0 to High(aData[i]) do begin
      if (i = 0) xor (j = 0) then
        Write(fout, '<td class = "h">')
      else
        Write(fout, '<td>');

      if (i = 0) and (j = 0) then begin
        Writeln(fout, '</td>');
        Continue;
      end;

      if j = 0 then begin
        Writeln(fout, YTitles[i].Caption + '</td>');
        Continue;
      end;

      if i = 0 then begin
        Writeln(fout, XTitles[j].Caption + '</td>');
        Continue;
      end;

      for k := 0 to High(aData[i][j]) do begin

        for l := 1 to High(ScheduleForm.VisibleColumn) do begin
          if not ScheduleForm.VisibleColumn[i] then Continue;
          Write(fout, '<strong>' + Columns[l].Caption + ':</strong> ' + aData[i][j][k].Item[l] + '<br />');
        end;

        if k <> High(aData[i][j]) then
          WriteLn(fout, '<div class = "separator">&nbsp</div>');

      end;
      WriteLn(fout, '</td>');
    end;
    Writeln(fout, '</tr>');
  end;
  WriteLn(fout, '</table></body></html>');
  CloseFile(fout);
end;

initialization
  SchExport := TSchExportFormatList.Create();
  with SchExport do begin
    Add(TSchExportToHTML, 'HTML', 'html');
    Add(TSchExportToExcel, 'Microsoft Excel 2007', 'xlsx');
  end;

end.

