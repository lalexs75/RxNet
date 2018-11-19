unit rxwStrTools; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, fpTemplate, HTTPDefs;

function MissignTag(const TagString: string): string;
function IsIE(const AgentStr:string):boolean;
function IsFireFox(const AgentStr:string):boolean;
function IsOpera(const AgentStr:string):boolean;

function FindNameInList(const SL:TStrings; const N:String):String;
function FindValueInList(const SL:TStrings; const Sess:String):String;
procedure RemoveValueIfExists(SL:TStrings; const S_ID:String);
procedure RemoveNameIfExists(SL:TStrings; const N:String);

function DateTimeToHTTPDate(ADate:TDateTime):string;

function MakeDBSelectControl(DS:TDataSet;const AControlName, AFieldNames, AFieldValues, ADefValue:string; LF:boolean):string;

procedure SetFileTemplate(Template:TFPCustomTemplate; FileName:string);

function HTMLEscapeChars(S:string):string;

var
  defTemplatesFolder : string = '';
implementation
uses rxwStrConsts;

procedure SetFileTemplate(Template:TFPCustomTemplate; FileName:string);
var
  Src:TFileStream;
  S:string;
begin
  if ExtractFileExt(FileName) = '' then
    FileName:=FileName + '.html';
  Template.FileName:='';
  Src:=TFileStream.Create(defTemplatesFolder + FileName, fmOpenRead);
  try
    if Src.Size > 0 then
    begin
      SetLength(S, Src.Size);
      Src.ReadBuffer(S[1], Src.Size);
    end
    else
      S:='';
  finally
    Src.Free;
  end;
  Template.Template:=S;
end;

function HTMLEscapeChars(S: string): string;
begin
  Result := StringReplace(S, '&', '&amp;', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '&quot;', [rfReplaceAll]);
  Result := StringReplace(Result, '«', '&laquo;', [rfReplaceAll]);
  Result := StringReplace(Result, '»', '&raquo;', [rfReplaceAll]);
  Result := StringReplace(Result, '''', '&#039;', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
end;

function MissignTag(const TagString: string): string;
begin
  Result:=Format(rxwMissingTag, [TagString]);
end;

function IsIE(const AgentStr: string): boolean;
begin
  Result:=Pos('MSIE', AgentStr) > 0;
end;

function IsFireFox(const AgentStr: string): boolean;
begin
  Result:=Pos('Firefox/', AgentStr) > 0;
end;

function IsOpera(const AgentStr: string): boolean;
begin
  Result:=Pos('Opera/', AgentStr) > 0;
end;

function FindNameInList(const SL:TStrings; const N:String):String;
var
    i : Integer;
begin
  Result := '';
  for i := 0 to SL.Count - 1 do
    if SL.Names[i] = N then
    begin
      Result := SL.Values[SL.Names[i]];//return with the sessionID
      break;
    end;
end;

function FindValueInList(const SL:TStrings; const Sess:String):String;
var
  s : String;
  i : Integer;
begin
  Result := '';
  if SL.Count <= 0 then Exit;
  s := '=' + Sess;
  i := 0;
  repeat
    if pos(s, SL[i]) > 0 then
    begin
      Result := SL.Names[i];
      break;
    end;
    inc(i);
  until i >= SL.Count;
end;

procedure RemoveValueIfExists(SL:TStrings; const S_ID:String);
var
  s : String;
  i : Integer;
begin
  if SL.Count <= 0 then Exit;
  s := '=' + S_ID;
  i := 0;
  repeat
    if pos(s, SL[i]) > 0 then
      SL.Delete(i)
    else
      inc(i);
  until i >= SL.Count;
end;

procedure RemoveNameIfExists(SL:TStrings; const N:String);
var
  i: Integer;
begin
  if SL.Count <= 0 then Exit;
  i := 0;
  repeat
    if SL.Names[i] = N then
      SL.Delete(i)
    else
      inc(i);
  until i >= SL.Count;
end;

function DateTimeToHTTPDate(ADate: TDateTime): string;
var
  Y, M, D:word;
begin
  DecodeDate(ADate, Y, M, D);
  Result:=Format(FormatDateTime(HTTPDateFmt, ADate),
                          [HTTPDays[DayOfWeek(ADate)], HTTPMonths[M]]);
end;

function MakeDBSelectControl(DS: TDataSet; const AControlName, AFieldNames,
  AFieldValues, ADefValue: string; LF:boolean): string;
var
  NF, VF:TField;
  ACloseFlag:boolean;
const
  LFL : array [boolean] of string = ('', LineEnding);
begin
  Result:='';
  ACloseFlag:=not DS.Active;
  if DS.Active then
    DS.First
  else
    DS.Open;

  NF:=DS.FieldByName(AFieldNames);
  VF:=DS.FieldByName(AFieldValues);
  while not DS.EOF do
  begin
    if ADefValue = VF.AsString then
      Result:=Result +
        '<option value="' + VF.AsString + '" selected="true">' + NF.DisplayText + '</option>'+LFL[LF]
    else
      Result:=Result +
        '<option value="' + VF.AsString + '">' + NF.DisplayText + '</option>'+LFL[LF];
    DS.Next;
  end;

  Result:='<SELECT name="' + AControlName + '">'+LFL[LF] + Result + '</SELECT>'+LFL[LF];

  if ACloseFlag then
    DS.Close;
end;

end.

