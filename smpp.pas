unit SMPP;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, smsPDUConstsUnit, smsPDUTypesUnit, ssockets;

type
  TBindType = (btTransmitter, btReceiver, btTransceiver);

type

  { ESMPPError }
  ESMPPError = class(Exception);

  ESMPPServerError = class(ESMPPError)
  private
    FCommand: integer;
    FSequence_Number: integer;
    FStatus: integer;
  public
    constructor Create(ACommand, ASequence_Number, AStatus:integer);
    property Command:integer read FCommand;
    property Sequence_Number:integer read FSequence_Number;
    property Status:integer read FStatus;
  end;

type
  TSmppMessage = class
    SourceAddr:string;
  end;

type
  TRxSMPP = class;
  TSmppError = procedure(Sender:TRXSMPP; const Msg:string) of object;
  TSmppStatus = procedure(Sender:TRXSMPP; CmdLength, Command, Sequence_Number, Status:TPDUInteger32) of object;
  TSmppDeliverSM = procedure(Sender:TRXSMPP; const Msg:TSmppMessage) of object;

  { TRxSMPP }

  TRxSMPP = class(TComponent)
  private
    FBindType: TBindType;
    FCodePage: integer;
    FConvertPhoneNum: boolean;
    FDstAdrNPI: integer;
    FDstAdrTON: TTONValue;
    FMsgID: integer;
    FOnConnect: TNotifyEvent;
    FOnDisconnect: TNotifyEvent;
    FOnSmppDeliverSM: TSmppDeliverSM;
    FOnSmppError: TSmppError;
    FOnSmppStatus: TSmppStatus;
    FPassword: string;
    FRemotePort: integer;
    FSequenceNumber:TPDUInteger32;
    FClntSocket:TInetSocket;
    FServerName: string;
    FSourceAdress: string;
    FSrcAdrNPI: integer;
    FSrcAdrTON: TTONValue;
    FStatus: integer;
    FSystemID: string;
    FUserName: string;
    function GetActive: boolean;
    procedure SetActive(const AValue: boolean);
    procedure SetBindType(const AValue: TBindType);
    procedure SetRemotePort(const AValue: integer);
    procedure SetServerName(const AValue: string);
    procedure Bind;
    procedure WriteBuffer(ABuf:PByteArray);
    procedure ParseResultBuffer(ABuf:PByteArray);
    procedure SendQnquireLinkResp(ASmppSeq:integer);
    procedure SendDeliverSmResp(ASmppSeq:integer);
    function DoConvetPnoneNum(const ANum:string):string;
    procedure ParseDeliverSm(Hdr:PPDUHeader; ABuf: PByteArray);
  protected
    procedure InternalOnConnect;virtual;
    procedure InternalOnDisconect;virtual;
    procedure InternalError(const Msg:string);virtual;
    procedure InternalSmtpStatus(Length, Command, Sequence_Number, Status:integer);virtual;
  public
    function LoadFromSock:boolean;

    //
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconect;
    property SequenceNumber:TPDUInteger32 read FSequenceNumber write FSequenceNumber;
    function SendSms(SmsMsg, SmsPhone:string):boolean;
    property MsgID:integer read FMsgID;
    property Status:integer read FStatus;
    property ClntSocket:TInetSocket read FClntSocket;
  published
    property Active:boolean read GetActive write SetActive;
    property ServerName:string read FServerName write SetServerName;
    property RemotePort:integer read FRemotePort write SetRemotePort default 2775;
    property BindType:TBindType read FBindType write SetBindType;
    property Password:string read FPassword write FPassword;
    property UserName:string read FUserName write FUserName;
    property SystemID:string read FSystemID write FSystemID;
    property SourceAdress:string read FSourceAdress write FSourceAdress; //ShortNumber
    property ConvertPhoneNum:boolean read FConvertPhoneNum write FConvertPhoneNum;//Приводим номер телефона к класическому (+7...)
    property CodePage:integer read FCodePage write FCodePage;   {0, 1, 2, 3}
    property SrcAdrTON:TTONValue read FSrcAdrTON write FSrcAdrTON;
    property SrcAdrNPI:integer read FSrcAdrNPI write FSrcAdrNPI;
    property DstAdrTON:TTONValue read FDstAdrTON write FDstAdrTON;
    property DstAdrNPI:integer read FDstAdrNPI write FDstAdrNPI;

    property OnConnect:TNotifyEvent read FOnConnect write FOnConnect;
    property OnDisconnect:TNotifyEvent read FOnDisconnect write FOnDisconnect;
    property OnSmppError:TSmppError read FOnSmppError write FOnSmppError;
    property OnSmppStatus:TSmppStatus read FOnSmppStatus write FOnSmppStatus;
    property OnSmppDeliverSM:TSmppDeliverSM read FOnSmppDeliverSM write FOnSmppDeliverSM;
  end;

implementation
uses LCLType, LCLProc, smsUtils, LazUTF8;


{ TRxSMPP }

procedure TRxSMPP.SetActive(const AValue: boolean);
begin
  if AValue then
    Connect
  else
    Disconect;
end;

procedure TRxSMPP.SetBindType(const AValue: TBindType);
begin
  if FBindType=AValue then exit;
  FBindType:=AValue;
end;

procedure TRxSMPP.SetRemotePort(const AValue: integer);
begin
  if FRemotePort=AValue then exit;
  FRemotePort:=AValue;
end;

procedure TRxSMPP.SetServerName(const AValue: string);
begin
  if FServerName=AValue then exit;
  FServerName:=AValue;
end;

procedure TRxSMPP.Bind;
var
  Buf:PByteArray;
  B:array [0..1000] of byte;
  Len:integer;
begin
  //Создаём буфер команды
  case FBindType of
    btTransmitter:Buf:=bufBIND_TRANSMITTER(FSequenceNumber,
                      FUserName, //User name
                      FPassword,
                      FSystemID, //system_typed
                      SMPP_Version, //interface_version
                      0, //addr_ton,
                      0, //addr_npi:
                      FSourceAdress// address_range
                      );
    btReceiver:Buf:=bufBIND_RECEIVER(FSequenceNumber,
                      FUserName, //User name
                      FPassword,
                      FSystemID, //system_typed
                      SMPP_Version, //interface_version
                      0, //addr_ton,
                      0, //addr_npi:
                      FSourceAdress// address_range
                      );
    btTransceiver:Buf:=bufBIND_TRANSCEIVER(FSequenceNumber,
                      FUserName, //User name
                      FPassword,
                      FSystemID, //system_typed
                      SMPP_Version, //interface_version
                      0, //addr_ton,
                      0, //addr_npi:
                      FSourceAdress// address_range
                      );
  else
    raise ESMPPError.Create(sUnknowBindType);
  end;

  //Посылаем команду
  WriteBuffer(Buf);
  //Уничтожаем буфер команды
  FreePDUBuffer(Buf);
  //Следующий шаг
  inc(FSequenceNumber);
  LoadFromSock;
end;

procedure TRxSMPP.WriteBuffer(ABuf: PByteArray);
var
  Len:integer;
begin
  if Assigned(ABuf) then
  begin
    Len:=SwapIntPDU32(PPDUHeader(ABuf)^.Command_Length);
    FClntSocket.Write(ABuf^, Len);
  end;
end;

function TRxSMPP.LoadFromSock: boolean;
var
  Len, CntForRead, FactRead:integer;
  Buf:PByteArray;
  Hdr:TPDUHeader;
begin
  //if not defShowResponce then exit;
  if Assigned(FClntSocket) then
  begin
//    if Len>0 then
    begin
      CntForRead:=SizeOf(TPDUHeader);

      FactRead:=FClntSocket.Read(Hdr, CntForRead);
      if FactRead<CntForRead then
      begin
        InternalError('Считан неполный заголовок '+IntToStr(FactRead) +' - из - '+IntToStr(CntForRead));
        Result:=false;
      end
      else
      begin
        Len:=SwapIntPDU32(Hdr.Command_Length);
        CntForRead:=Len - SizeOf(TPDUHeader);
        Getmem(Buf, Len);
        Move(Hdr, Buf^, SizeOf(TPDUHeader));
        if CntForRead>0 then
          FactRead:=FClntSocket.Read(Buf^[SizeOf(TPDUHeader)], CntForRead);
        ParseResultBuffer(Buf);
        FreePDUBuffer(Buf);
        Result:=FStatus = ESME_ROK;
      end
    end;
  end;
end;

procedure TRxSMPP.ParseResultBuffer(ABuf: PByteArray);
var
  Hdr:PPDUHeader;
begin
  Hdr:=Pointer(ABuf);
  Hdr^.Command_Length:=SwapIntPDU32(Hdr^.Command_Length);
  Hdr^.Command_ID:=SwapIntPDU32(Hdr^.Command_ID);
  Hdr^.Command_Status:=SwapIntPDU32(Hdr^.Command_Status);
  Hdr^.Sequence_Number:=SwapIntPDU32(Hdr^.Sequence_Number);

  InternalSmtpStatus( Hdr^.Command_Length, Hdr^.Command_ID, Hdr^.Sequence_Number, Hdr^.Command_Status);

  if Hdr^.Command_ID = enquire_link then
    SendQnquireLinkResp(Hdr^.Sequence_Number)
  else
  if Hdr^.Command_ID = deliver_sm then
  begin
    ParseDeliverSm(Hdr, ABuf);
  end;
//  WriteLog('');
end;

procedure TRxSMPP.SendQnquireLinkResp(ASmppSeq: integer);
var
  Buf:PByteArray;
begin
  Buf:=bufEnquireLinkResp(ASmppSeq);
  WriteBuffer(Buf);
  //Уничтожаем буфер команды
  FreePDUBuffer(Buf);
  inc(FSequenceNumber);

  LoadFromSock;
end;

procedure TRxSMPP.SendDeliverSmResp(ASmppSeq: integer);
var
  Buf:PByteArray;
begin
  Buf:=bufDeliverSmResp(ASmppSeq);
  WriteBuffer(Buf);
  //Уничтожаем буфер команды
  FreePDUBuffer(Buf);
  inc(FSequenceNumber);
//  LoadFromSock;
end;

function TRxSMPP.DoConvetPnoneNum(const ANum: string): string;
begin
  Result:=ANum;
  if Result<> '' then
  begin
    // Необходимо убрать из номера телефона символ +
    if Result[1] = '+' then
      Delete(Result, 1, 1);
    //Если первая цифра номера 8 - то заменим её на 7 - так билайн трактует
    if Length(Result) = 10 then
      Result:='7'+Result
    else
    if Result[1] = '8' then
      Result[1]:='7';
  end;
end;

{
ACodePage
 0 - Ansi (win1251)
 1 - UTF16BE
 2 - translit
 3 - UTF8
}

procedure TRxSMPP.ParseDeliverSm(Hdr: PPDUHeader; ABuf: PByteArray);
var
  AMsg:TSmppMessage;
begin
  if Assigned(FOnSmppDeliverSM) then
  begin
    AMsg:=TSmppMessage.Create;
    try
      FOnSmppDeliverSM(Self, AMsg);
    finally
      AMsg.Free;
    end;
  end;
  SendDeliverSmResp(Hdr^.Sequence_Number);
end;

procedure TRxSMPP.InternalOnConnect;
begin
  if Assigned(OnConnect) then
    OnConnect(Self)
end;

procedure TRxSMPP.InternalOnDisconect;
begin
  if Assigned(OnDisconnect) then
    OnDisconnect(Self)
end;

procedure TRxSMPP.InternalError(const Msg: string);
begin
  if Assigned(FOnSmppError) then
    FOnSmppError(Self, Msg);
end;

procedure TRxSMPP.InternalSmtpStatus(Length, Command, Sequence_Number,
  Status: integer);
begin
  FStatus:=Status;
  if Assigned(FOnSmppStatus) then
  begin
    FOnSmppStatus(Self, Length, Command, Sequence_Number, Status);

  end
  else
    if Status<>0 then
    raise ESMPPServerError.Create(Command, Sequence_Number, Status);
end;

function TRxSMPP.GetActive: boolean;
begin
  Result:=Assigned(FClntSocket)
end;

constructor TRxSMPP.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSystemID:='SMPP';
  FRemotePort:=2775;
  FMsgID:=0;
  SequenceNumber:=1;
end;

destructor TRxSMPP.Destroy;
begin
  inherited Destroy;
end;

procedure TRxSMPP.Connect;
begin
  if Assigned(FClntSocket) then exit;
//  FSequenceNumber:=1;
  //S:=InetAddrToStr(StrToInetAddr(defServerAddr), true);
  try
    FClntSocket:=TInetSocket.Create(FServerName, FRemotePort);
  except
    on E:Exception do
    begin
      InternalError(E.Message);
      FClntSocket:=nil;
    end;
  end;
  if Assigned(FClntSocket) then
  begin
    InternalOnConnect;
    Bind;
    if FStatus <> ESME_ROK then
      Disconect;
  end;
end;

procedure TRxSMPP.Disconect;
var
  Buf:PByteArray;
begin
  if not Assigned(FClntSocket) then
    exit;

  Buf:=bufUNBIND(FSequenceNumber);

  WriteBuffer(Buf);
  //Уничтожаем буфер команды
  FreePDUBuffer(Buf);

  inc(FSequenceNumber);
  LoadFromSock;
  //Уничтожим объект сокета
  FreeAndNil(FClntSocket);
  InternalOnDisconect;
end;

function TRxSMPP.SendSms(SmsMsg, SmsPhone: string): boolean;
var
  ACodePage, CntMsg, FullLen, i:integer;
  Buf:PByteArray;
  AShortMsg:string;
  MsgLen:TPDUInteger8;
  UDHTextSegment:TUDHTextSegment;
begin
  Result:=false;
  if FStatus <> ESME_ROK then
    exit;

  if SmsPhone<>'' then
  begin
    if FConvertPhoneNum then
      SmsPhone:=DoConvetPnoneNum(SmsPhone);

    if FCodePage in [1, 3] then
      ACodePage:=8
    else
      ACodePage:=0;


    if ACodePage<>8 then
    begin
      AShortMsg:=ConvertUTF8ToSM(SmsMsg, FCodePage, MsgLen);
      //Отсылаем СМС как одно сообщение
      Buf:=bufSUBMIT_SM(FSequenceNumber,
                      '', //service_type,
                      FSourceAdress, //source_addr,
                      SmsPhone, //destination_addr,
                      '', //schedule_delivery_time,
                      '', //validity_period,
                      AShortMsg,
                      ord(FSrcAdrTON),
                      FSrcAdrNPI,
                      ord(FDstAdrTON),
                      FDstAdrNPI,
                      ACodePage,
                      MsgLen,
                      nil,
                      nil);

      WriteBuffer(Buf);
      //Уничтожаем буфер команды
      FreePDUBuffer(Buf);
      Result:=LoadFromSock;
      inc(FSequenceNumber);
    end
    else
    begin
      FullLen:=UTF8Length(SmsMsg);
      CntMsg:=FullLen div 67;
      if (FullLen mod 67) > 0 then
        inc(CntMsg);
      UDHTextSegment.UDHLen:=5;
      UDHTextSegment.IE_ID:=0;
      UDHTextSegment.IE_IDLen:=3;
      UDHTextSegment.MsgID:=FMsgID;
      UDHTextSegment.MsgPartCnt:=CntMsg;

//      if CntMsg>1 then
      begin
        for i:=1 to CntMsg do
        begin
          AShortMsg:=UTF8Copy(SmsMsg, 1, 67);
          UTF8Delete(SmsMsg, 1, 67);

          AShortMsg:=ConvertUTF8ToSM(AShortMsg, FCodePage, MsgLen);
          UDHTextSegment.MsgPartNum:=i;

          //Отсылаем СМС как одно сообщение
          Buf:=bufSUBMIT_SM(FSequenceNumber,
                          '', //service_type,
                          FSourceAdress, //source_addr,
                          SmsPhone, //destination_addr,
                          '', //schedule_delivery_time,
                          '', //validity_period,
                          AShortMsg,
                          ord(FSrcAdrTON),
                          FSrcAdrNPI,
                          ord(FDstAdrTON),
                          FDstAdrNPI,
                          ACodePage,
                          MsgLen,
                          nil,
                          @UDHTextSegment
                          );

          WriteBuffer(Buf);
          FreePDUBuffer(Buf);
          Result:=LoadFromSock;
          //Уничтожаем буфер команды
          inc(FSequenceNumber);
          if not Result then
            break;
        end
      end
{      else
      begin
          AShortMsg:=ConvertString(SmsMsg, FCodePage, MsgLen);
          //Отсылаем СМС как одно сообщение
          Buf:=bufSUBMIT_SM(FSequenceNumber,
                          '', //service_type,
                          FSourceAdress, //source_addr,
                          SmsPhone, //destination_addr,
                          '', //schedule_delivery_time,
                          '', //validity_period,
                          AShortMsg,
                          ord(FSrcAdrTON),
                          FSrcAdrNPI,
                          ord(FDstAdrTON),
                          FDstAdrNPI,
                          CodePage,
                          MsgLen,
                          nil,
                          nil);

          WriteBuffer(Buf);
          //Уничтожаем буфер команды
          FreePDUBuffer(Buf);
          Result:=LoadFromSock;
          inc(FSequenceNumber);
      end;}
    end;
    Inc(FMsgID);
  end
  else
    InternalError('Error in phone number - sms = '+SmsMsg);
end;

{ ESMPPError }

constructor ESMPPServerError.Create(ACommand, ASequence_Number, AStatus: integer);
begin
  inherited Create(SMPPErrorCodeStr(AStatus));
  FCommand:=ACommand;
  FSequence_Number:=ASequence_Number;
  FStatus:=AStatus;
end;

end.

