unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RxIniPropStorage, SMSGsmInfo, Forms, Controls,
  Graphics, Dialogs, StdCtrls, ComCtrls, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    btnGetBalance: TButton;
    btnChekStatus: TButton;
    CheckBox1: TCheckBox;
    cbTrafifcType: TComboBox;
    cbMegafonKind: TComboBox;
    cbSendNow: TCheckBox;
    edtMsgSender: TEdit;
    edtPhoneNums: TEdit;
    edtMessage: TEdit;
    edtProxyHost: TEdit;
    edtProxyPort: TEdit;
    edtProxyUserName: TEdit;
    edtProxyUserPass: TEdit;
    edtInfoTypeCurr: TEdit;
    edtInfoBalance: TEdit;
    edtAPIUrl: TEdit;
    edtUserID: TEdit;
    edtAPIKey: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Memo1: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    RxIniPropStorage1: TRxIniPropStorage;
    RxSMSGsmInfo1: TRxSMSGsmInfo;
    Splitter1: TSplitter;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure btnGetBalanceClick(Sender: TObject);
    procedure btnChekStatusClick(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure edtMessageChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure SetInfoParams;
    procedure DisplaySmsList;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation
uses LazUTF8;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  SetInfoParams;
  try
    if RxSMSGsmInfo1.SendMessages(edtPhoneNums.Text, edtMessage.Text) then
      DisplaySmsList;
  except
    on E:Exception do
      Memo1.Lines.Add(E.ClassName +' : '+ E.Message);
  end;
  Memo1.Lines.Add(RxSMSGsmInfo1.RequestInfo.Text);
end;

procedure TForm1.btnGetBalanceClick(Sender: TObject);
begin
  SetInfoParams;
  try
    RxSMSGsmInfo1.GetBalance;
    edtInfoTypeCurr.Text:=RxSMSGsmInfo1.KindCurrency;
    edtInfoBalance.Text:=CurrToStr(RxSMSGsmInfo1.Balance);
  except
    on E:Exception do
      Memo1.Lines.Add(E.ClassName +' : '+ E.Message);
  end;
  Memo1.Lines.Add(RxSMSGsmInfo1.RequestInfo.Text);
end;

procedure TForm1.btnChekStatusClick(Sender: TObject);
begin
  SetInfoParams;
  try
    if RxSMSGsmInfo1.CheckStatus then
      DisplaySmsList;
  except
    on E:Exception do
      Memo1.Lines.Add(E.ClassName +' : '+ E.Message);
  end;
  Memo1.Lines.Add(RxSMSGsmInfo1.RequestInfo.Text);
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  edtProxyHost.Enabled:=CheckBox1.Checked;
  edtProxyPort.Enabled:=CheckBox1.Checked;
  edtProxyUserName.Enabled:=CheckBox1.Checked;
  edtProxyUserPass.Enabled:=CheckBox1.Checked;
  Label7.Enabled:=CheckBox1.Checked;
  Label8.Enabled:=CheckBox1.Checked;
  Label9.Enabled:=CheckBox1.Checked;
  Label10.Enabled:=CheckBox1.Checked;
end;

procedure TForm1.edtMessageChange(Sender: TObject);
begin
  Label3.Caption:=Format('Длинна сообщения: %d', [UTF8Length(edtMessage.Text)]);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Memo1.Lines.Clear;
  CheckBox1Change(nil);
  edtMessageChange(nil);
end;

procedure TForm1.SetInfoParams;
begin
  RxSMSGsmInfo1.APIKey:=edtAPIKey.Text;
  RxSMSGsmInfo1.UserID:=edtUserID.Text;
  RxSMSGsmInfo1.MsgSender:=edtMsgSender.Text;
  RxSMSGsmInfo1.TrafficType:=TTrafficType(cbTrafifcType.ItemIndex);
  RxSMSGsmInfo1.MegafonKind:=TMegafonKind(cbMegafonKind.ItemIndex);
  RxSMSGsmInfo1.SendNow:=cbSendNow.Checked;


  if CheckBox1.Checked then
  begin
    RxSMSGsmInfo1.ProxyHost:=edtProxyHost.Text;
    RxSMSGsmInfo1.ProxyPort:=edtProxyPort.Text;
    RxSMSGsmInfo1.ProxyUser:=edtProxyUserName.Text;
    RxSMSGsmInfo1.ProxyPass:=edtProxyUserPass.Text;
  end
  else
  begin
    RxSMSGsmInfo1.ProxyHost:='';
    RxSMSGsmInfo1.ProxyPort:='';
    RxSMSGsmInfo1.ProxyUser:='';
    RxSMSGsmInfo1.ProxyPass:='';
  end;
end;

procedure TForm1.DisplaySmsList;
var
  i: Integer;
begin
  for i:=0 to RxSMSGsmInfo1.MessagesList.Count-1 do
    Memo1.Lines.Add(Format('Phone=%s : SmsID=%d : SendStatus=%d', [
      RxSMSGsmInfo1.MessagesList[i].Phone,
      RxSMSGsmInfo1.MessagesList[i].SmsID,
      RxSMSGsmInfo1.MessagesList[i].SendStatus
      ]));
end;

end.

