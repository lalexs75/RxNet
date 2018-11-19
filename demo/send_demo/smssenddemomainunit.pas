unit smsSendDemoMainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, StdCtrls,
  XMLPropStorage, ActnList, Menus, Buttons, ComCtrls, SMPP;

type

  { TsmsSendDemoMainForm }

  TsmsSendDemoMainForm = class(TForm)
    BitBtn1: TBitBtn;
    Label6: TLabel;
    ListBox1: TListBox;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    smsSend: TAction;
    StatusBar1: TStatusBar;
    sysParams: TAction;
    hlpAbout: TAction;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    sysExit: TAction;
    ActionList1: TActionList;
    edtPhoneNumber: TEdit;
    edtServerAddr: TEdit;
    edtUserName: TEdit;
    edtPassword: TEdit;
    edtText: TEdit;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    MainMenu1: TMainMenu;
    RxSMPP1: TRxSMPP;
    XMLPropStorage1: TXMLPropStorage;
    procedure FormCreate(Sender: TObject);
    procedure hlpAboutExecute(Sender: TObject);
    procedure RxSMPP1Connect(Sender: TObject);
    procedure RxSMPP1SmtpError(Sender: TRXSMPP; const Msg: string);
    procedure RxSMPP1SmtpStatus(Sender: TRXSMPP; CmdLength, Command,
      Sequence_Number, Status: integer);
    procedure smsSendExecute(Sender: TObject);
    procedure sysExitExecute(Sender: TObject);
    procedure sysParamsExecute(Sender: TObject);
  private
    procedure UpdateStatus;
  public
    procedure WriteLog(const AMsg:string);
  end; 

var
  smsSendDemoMainForm: TsmsSendDemoMainForm;

implementation
uses hlpAboutUnit, LCLType, smssendParamsUnit, smsPDUConstsUnit, Dialogs;

{$R *.lfm}

{ TsmsSendDemoMainForm }

procedure TsmsSendDemoMainForm.hlpAboutExecute(Sender: TObject);
begin
  hlpAboutForm:=ThlpAboutForm.Create(Application);
  hlpAboutForm.ShowModal;
  hlpAboutForm.Free;
end;

procedure TsmsSendDemoMainForm.FormCreate(Sender: TObject);
begin
  UpdateStatus;
end;

procedure TsmsSendDemoMainForm.RxSMPP1Connect(Sender: TObject);
begin
  UpdateStatus;
end;

procedure TsmsSendDemoMainForm.RxSMPP1SmtpError(Sender: TRXSMPP;
  const Msg: string);
begin
  WriteLog('Error!: '+Msg);
end;

procedure TsmsSendDemoMainForm.RxSMPP1SmtpStatus(Sender: TRXSMPP; CmdLength,
  Command, Sequence_Number, Status: integer);
begin
  WriteLog(Format('CmdLength = %d, Command = %d, Sequence_Number = %d, Status = %d (%s)',
    [CmdLength, Command, Sequence_Number, Status, SMPPErrorCodeStr(Status)]));
end;

procedure TsmsSendDemoMainForm.smsSendExecute(Sender: TObject);
var
  Res:boolean;
begin
  RxSMPP1.CodePage:=0;
  RxSMPP1.SourceAdress:=SenderNumber;
  RxSMPP1.ServerName:=edtServerAddr.Text;
  RxSMPP1.UserName:=edtUserName.Text;
  RxSMPP1.Password:=edtPassword.Text;
  RxSMPP1.SrcAdrNPI:=SrcAdrNPI;
  RxSMPP1.SrcAdrTON:=TTONValue(SrcAdrTon);
  RxSMPP1.DstAdrNPI:=DstAdrNPI;
  RxSMPP1.DstAdrTON:=TTONValue(DstAdrTon);
  RxSMPP1.RemotePort:=ServerPort;
  RxSMPP1.CodePage:=SMSCodePage;

  //RxSMPP1.CodePage:=0;

  Res:=false;
  RxSMPP1.Connect;
  if RxSMPP1.Active then
  begin
    try
      Res:=RxSMPP1.SendSms(edtText.Text, edtPhoneNumber.Text);
      RxSMPP1.Disconect;
    except
      on E:Exception do
        ShowMessage(E.Message);
    end;
  end;
  if Res then
    Application.MessageBox('SMS!', 'SMS!', MB_ICONINFORMATION + MB_OK)
  else
    Application.MessageBox('Error send SMS', 'SMS!', MB_ICONERROR + MB_OK);
end;

procedure TsmsSendDemoMainForm.sysExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TsmsSendDemoMainForm.sysParamsExecute(Sender: TObject);
begin
  smssendParamsForm:=TsmssendParamsForm.Create(Application);
  smssendParamsForm.ShowModal;
  smssendParamsForm.Free;
end;

procedure TsmsSendDemoMainForm.UpdateStatus;
begin
  if RxSMPP1.Active then
    StatusBar1.Panels[0].Text:='Connect'
  else
    StatusBar1.Panels[0].Text:='Disconnect';
end;

procedure TsmsSendDemoMainForm.WriteLog(const AMsg: string);
begin
  ListBox1.Items.Add(AMsg);
  ListBox1.ItemIndex:=ListBox1.Items.Count - 1;
  WriteLogFile(AMsg);
end;

end.

