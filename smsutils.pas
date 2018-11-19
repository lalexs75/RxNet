unit smsUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

function TranslitRus2Lat(S: string): string;

function ConvertSMToUTF8(SMBuf:string; SMCoding:integer):string;
function ConvertUTF8ToSM(AValue: string; ACodePage: integer; out MsgLen: byte): string;
function ChekcPhoneNumber(ANum:string):boolean;
implementation
uses LCLProc, LazUTF8;

function TranslitRus2Lat(S: string): string;
const
  RArrayL = 'абвгдеёжзийклмнопрстуфхцчшщьыъэюя';
  RArrayU = 'АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯ';
  colChar = 33;
  arr: array[1..2, 1..ColChar] of string =
  (('a', 'b', 'v', 'g', 'd', 'e', 'yo', 'zh', 'z', 'i', 'y',
    'k', 'l', 'm', 'n', 'o', 'p', 'r', 's', 't', 'u', 'f',
    'kh', 'ts', 'ch', 'sh', 'shch', '''', 'y', '''', 'e', 'yu', 'ya'),
    ('A', 'B', 'V', 'G', 'D', 'E', 'Yo', 'Zh', 'Z', 'I', 'Y',
    'K', 'L', 'M', 'N', 'O', 'P', 'R', 'S', 'T', 'U', 'F',
    'Kh', 'Ts', 'Ch', 'Sh', 'Shch', '''', 'Y', '''', 'E', 'Yu', 'Ya'));
var
  i: Integer;
  LenS: Integer;
  p: integer;
  d: byte;
  uC:string;
begin
  result := '';
//  LenS := length(str);
  while S<>'' do
  begin
    uC:=UTF8Copy(S, 1, 1);
    Delete(S, 1, Length(Uc));
    d := 1;
    p := UTF8Pos(uC, RArrayL);

    if p = 0 then
    begin
      p := UTF8Pos(uC, RArrayU);
      d := 2
    end;
    if p <> 0 then
      result := result + arr[d, p]
    else
      result := result + uC; //если не русская буква, то берем исходную
  end;
end;

function ConvertSMToUTF8(SMBuf: string; SMCoding: integer): string;
var
  i, c:integer;
  b:byte;
  SS:UTF16String;
begin
  case SMCoding of
    0:Result:=AnsiToUtf8(SMBuf);
    8:begin
        C:=Length(SMBuf) div 2;
        for i:=1 to C do
        begin
          b:=byte(SMBuf[i*2]);
          SMBuf[i*2]:=SMBuf[i*2-1];
          byte(SMBuf[i*2-1]):=b;
        end;
        SetLength(SS, C);
        Move(SMBuf[1], SS[1], C*2);
        Result:=UTF16ToUTF8(SS);
      end
  else
    Result:=SMBuf;
  end;
end;

function ConvertUTF8ToSM(AValue: string; ACodePage: integer; out
  MsgLen: byte): string;
var
  c,i:integer;
  SS:UTF16String;
  b:byte;
begin
  MsgLen:=Length(AValue);
  case ACodePage of
    0:begin
        Result:=Utf8ToAnsi(AValue);
      end;
    1:begin
        SS:=UTF8ToUTF16(AValue);
        C:=Length(SS);
        SetLength(Result, C*2);
        Move(SS[1], Result[1], C*2);
        for i:=1 to C do
        begin
          b:=byte(Result[i*2]);
          Result[i*2]:=Result[i*2-1];
          byte(Result[i*2-1]):=b;
        end;
        MsgLen:=C * 2;
      end;
    2:begin
        Result:=TranslitRus2Lat(AValue);
        MsgLen:=Length(Result);
      end;
    3:Result:=AValue;
  end;
end;

const
  ValidPhoneChars = ['0'..'9', '-', '+'];

function ChekcPhoneNumber(ANum: string): boolean;
var
  i:integer;
begin
  Result:=false;
  if ANum = '' then exit;
  i:=1;
  while i<Length(ANum) do
  begin
    if not (ANum[i] in ValidPhoneChars) then
      exit;
    inc(i);
  end;
  Result:=true;
end;

end.

