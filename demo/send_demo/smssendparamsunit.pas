unit smssendParamsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ButtonPanel, Spin, SMPP;

type

  { TsmssendParamsForm }

  TsmssendParamsForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    edtSMSCodePage: TComboBox;
    edtSenderNumber: TEdit;
    edtBindType: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    edtSrcNPI: TSpinEdit;
    edtSrcTON: TSpinEdit;
    edtDstNPI: TSpinEdit;
    edtDstTON: TSpinEdit;
    edtServerPort: TSpinEdit;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    procedure DoSetConst;
  public
    { public declarations }
  end; 

var
  smssendParamsForm: TsmssendParamsForm;

var
  SrcAdrNPI    : integer = 9;
  SrcAdrTon    : integer = 9;
  DstAdrNPI    : integer = 9;
  DstAdrTon    : integer = 9;
  ServerPort   : integer = 2775;
  SenderNumber : string = '';
  SMSCodePage  : integer = 0;
  BindType     : TBindType = btTransmitter;

procedure WriteLogFile(const AMsg:string);
procedure LoadConst;
procedure StoreConst;

implementation
uses IniFiles;

{$R *.lfm}
var
  LogFileName:string = '';

function ApplicationIniFile: TIniFile;
begin
  Result:=TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
end;

procedure WriteLogFile(const AMsg:string);
var
  F:TextFile;
begin
  AssignFile(F, LogFileName);
  if FileExists(LogFileName) then
    Append(F)
  else
    Rewrite(F);
  WriteLn(f, AMsg);
  CloseFile(F);
end;

procedure LoadConst;
var
  Ini:TIniFile;
begin
  Ini:=ApplicationIniFile;
  SrcAdrNPI := Ini.ReadInteger('system', 'SrcAdrNPI', SrcAdrNPI);
  SrcAdrTon := Ini.ReadInteger('system', 'SrcAdrTon', SrcAdrTon);
  DstAdrNPI := Ini.ReadInteger('system', 'DstAdrNPI', DstAdrNPI);
  DstAdrTon := Ini.ReadInteger('system', 'DstAdrTon', DstAdrTon);
  ServerPort:= Ini.ReadInteger('system', 'ServerPort', ServerPort);
  SenderNumber:=Ini.ReadString('system', 'SenderNumber', SenderNumber);
  SMSCodePage  := Ini.ReadInteger('system', 'SMSCodePage', SMSCodePage);
  Ini.Free;
end;

procedure StoreConst;
var
  Ini:TIniFile;
begin
  Ini:=ApplicationIniFile;
  Ini.WriteInteger('system', 'SrcAdrNPI', SrcAdrNPI);
  Ini.WriteInteger('system', 'SrcAdrTon', SrcAdrTon);
  Ini.WriteInteger('system', 'DstAdrNPI', DstAdrNPI);
  Ini.WriteInteger('system', 'DstAdrTon', DstAdrTon);
  Ini.WriteInteger('system', 'ServerPort', ServerPort);
  Ini.WriteString('system', 'SenderNumber', SenderNumber);
  Ini.WriteInteger('system', 'SMSCodePage', SMSCodePage);
  Ini.Free;
end;

{ TsmssendParamsForm }

procedure TsmssendParamsForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if ModalResult = mrOk then
  begin
    DoSetConst;
    StoreConst;
  end;
end;

procedure TsmssendParamsForm.FormCreate(Sender: TObject);
begin
  edtSrcNPI.Value:=SrcAdrNPI;
  edtSrcTON.Value:=SrcAdrTon;
  edtDstNPI.Value:=DstAdrNPI;
  edtDstTON.Value:=DstAdrTon;
  edtServerPort.Value:=ServerPort;
  edtSenderNumber.Text:=SenderNumber;
  edtSMSCodePage.ItemIndex:=SMSCodePage;

end;

procedure TsmssendParamsForm.DoSetConst;
begin
  SrcAdrNPI    := edtSrcNPI.Value;
  SrcAdrTon    := edtSrcTON.Value;
  DstAdrNPI    := edtDstNPI.Value;
  DstAdrTon    := edtDstTON.Value;
  ServerPort   := edtServerPort.Value;
  SenderNumber := edtSenderNumber.Text;
  SMSCodePage:=edtSMSCodePage.ItemIndex;
end;

initialization
  LogFileName:=ChangeFileExt(ParamStr(0), '.log');
  LoadConst;
end.

