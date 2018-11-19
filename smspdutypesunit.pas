unit smsPDUTypesUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, smsPDUConstsUnit, smsCommandTypes;

type
  PPDUHeader = ^TPDUHeader;
  TPDUHeader = packed record
    Command_Length : TPDUInteger32;//  4 Integer The command_length field defines the total octet length of the SMPP PDU packet including the length field.
    Command_ID     : TPDUInteger32;//  4 Integer The command_id field identifies the particular SMPP PDU
    Command_Status : TPDUInteger32;// 4 Integer The command_status field indicates the success or failure of an SMPP request.
    Sequence_Number: TPDUInteger32;// This field contains a sequence number which allows SMPP requests and responses to be associated for correlation purposes
  end;

  PPDUOptParam = ^TPDUOptParam;
  TPDUOptParam = packed record
    Tag    : TPDUInteger16;//2 Integer The Tag field is used to uniquely identify the particular optional parameter in question.
    Length : TPDUInteger16;//2 Integer The Length field indicates the length of the Value field in octets.
  end;

  PPDUOptMetaParam = ^TPDUOptMetaParam;
  TPDUOptMetaParam = packed record
    ParLength:TPDUInteger16;
    Next:PPDUOptMetaParam;
    Param: record end;//array [0..0] of byte;
  end;

  PUDHTextSegment = ^TUDHTextSegment;
  TUDHTextSegment = packed record
    UDHLen:TPDUInteger8;
    IE_ID:TPDUInteger8;
    IE_IDLen:TPDUInteger8;
    MsgID:TPDUInteger8;
    MsgPartCnt:TPDUInteger8;
    MsgPartNum:TPDUInteger8;
  end;

type

  { TDeliverSM }

  TDeliverSM = class
    ServiceType:string;
    SourceAddrTON:TTONValue;
    SourceAddrNPI:TPDUInteger8;
    SourceAddr:string;
    DestAddrTON:TTONValue;
    DestAddrNPI:TPDUInteger8;
    DestAddr:string;
    EsmClass:TPDUInteger8;
    ProtocolId:TPDUInteger8;
    PriorityFlag:TPDUInteger8;
    ScheduleDeliveryTime:string;
    ValidityPeriod:string;
    RegisteredDelivery:TPDUInteger8;
    ReplaceIfPresentFlag:TPDUInteger8;
    DataCoding:TPDUInteger8;
    SMDefaultMsgId:TPDUInteger8;
    ShortMessage:string;
    UDHTextSegment:TUDHTextSegment;
    constructor Create;
  end;

type

  { TPDUCommand }

  TPDUCommand = class
  public
  public
    SequenceNumber:TPDUInteger32;
    system_id:string;
    password:string;
    system_typed:string;
    interface_version:TPDUInteger8;
    addr_ton:TPDUInteger8;
    addr_npi:TPDUInteger8;
    address_range:string;
  end;

//Методы для кодирования информации
function bufBIND_TRANSMITTER(SequenceNumber:TPDUInteger32; system_id:string;
              password:string; system_typed:string; interface_version:TPDUInteger8;
              addr_ton:TPDUInteger8; addr_npi:TPDUInteger8; address_range:string):PByteArray;

function bufBIND_RECEIVER(SequenceNumber:TPDUInteger32; system_id:string;
              password:string; system_typed:string; interface_version:TPDUInteger8;
              addr_ton:TPDUInteger8; addr_npi:TPDUInteger8; address_range:string):PByteArray;

function bufBIND_TRANSCEIVER(SequenceNumber:TPDUInteger32; system_id:string;
              password:string; system_typed:string; interface_version:TPDUInteger8;
              addr_ton:TPDUInteger8; addr_npi:TPDUInteger8; address_range:string):PByteArray;

function bufSUBMIT_SM(SequenceNumber: TPDUInteger32; service_type, source_addr,
  destination_addr, schedule_delivery_time, validity_period, short_message:string;
  source_addr_ton, source_addr_npi, dest_addr_ton, dest_addr_npi,
  data_coding:TPDUInteger8; short_message_length:TPDUInteger8;
  OptParams:PPDUOptMetaParam; UDHTextSegment:PUDHTextSegment): PByteArray;

function bufUNBIND(SequenceNumber:TPDUInteger32):PByteArray;

function bufEnquireLink(SequenceNumber:TPDUInteger32):PByteArray;
function bufEnquireLinkResp(SequenceNumber:TPDUInteger32):PByteArray;

function bufDeliverSmResp(SequenceNumber:TPDUInteger32):PByteArray;

procedure FreePDUBuffer(var Buf:PByteArray);

function SwapIntPDU32(AValue:TPDUInteger32):TPDUInteger32;
function SwapIntPDU16(AValue:TPDUInteger16):TPDUInteger16;

  //Уничтожение цепочки опциональных параметров
procedure FreePDUOptMetaParam(P:PPDUOptMetaParam);
  //Создание цепочки опциональных параметров
function OPNew_SarTotalSegments(Value:TPDUInteger8; ANext:PPDUOptMetaParam):PPDUOptMetaParam;
function OPNew_SarSegmentSeqnum(Value:TPDUInteger8; ANext:PPDUOptMetaParam):PPDUOptMetaParam;
function OPNew_SarMsgRefNum(Value:TPDUInteger16; ANext:PPDUOptMetaParam):PPDUOptMetaParam;

//Методы для раскодировани информации
procedure DecodeDeliverSM(Hdr:PPDUHeader; out DeliverSM:TDeliverSM);
implementation
uses math, smsUtils;

const
  SizeOfTPDUOptMetaParamHeader = SizeOf(TPDUInteger16) + SizeOf(PPDUOptMetaParam);

function SwapIntPDU32(AValue:TPDUInteger32):TPDUInteger32;
var
  A:array[1..4] of byte absolute AValue;
  B:array[1..4] of byte absolute Result;
begin
  B[4]:=A[1];
  B[3]:=A[2];
  B[2]:=A[3];
  B[1]:=A[4];
end;

function SwapIntPDU16(AValue: TPDUInteger16): TPDUInteger16;
var
  A:array[1..2] of byte absolute AValue;
  B:array[1..2] of byte absolute Result;
begin
  B[2]:=A[1];
  B[1]:=A[2];
end;

//Осовободим память из под цепочки опциональных параметров
procedure FreePDUOptMetaParam(P: PPDUOptMetaParam);
var
  Next:PPDUOptMetaParam;
begin
  while Assigned(P) do
  begin
    Next:=P^.Next;
    Freemem(P, P^.ParLength);
    P:=Next;
  end;
end;

function OPNew_SarTotalSegments(Value:TPDUInteger8; ANext: PPDUOptMetaParam): PPDUOptMetaParam;
begin
  GetMem(Result, SizeOfTPDUOptMetaParamHeader + SizeOf(TOP_sar_total_segments));
  FillChar(Result^, SizeOfTPDUOptMetaParamHeader + SizeOf(TOP_sar_total_segments), 0);
  Result^.ParLength:=SizeOfTPDUOptMetaParamHeader + SizeOf(TOP_sar_total_segments);
  Result^.Next:=ANext;
  POP_sar_total_segments(@(Result^.Param))^.Tag:=SwapIntPDU16(sar_total_segments);
  POP_sar_total_segments(@(Result^.Param))^.Length:=SwapIntPDU16(SizeOf(TOP_sar_total_segments));
  POP_sar_total_segments(@(Result^.Param))^.Value:=Value;
end;

function OPNew_SarSegmentSeqnum(Value:TPDUInteger8; ANext: PPDUOptMetaParam): PPDUOptMetaParam;
begin
  GetMem(Result, SizeOfTPDUOptMetaParamHeader + SizeOf(TOP_sar_segment_seqnum));
  FillChar(Result^, SizeOfTPDUOptMetaParamHeader + SizeOf(TOP_sar_segment_seqnum), 0);
  Result^.ParLength:=SizeOfTPDUOptMetaParamHeader + SizeOf(TOP_sar_segment_seqnum);
  Result^.Next:=ANext;
  POP_sar_segment_seqnum(@(Result^.Param))^.Tag:=SwapIntPDU16(sar_segment_seqnum);
  POP_sar_segment_seqnum(@(Result^.Param))^.Length:=SwapIntPDU16(SizeOf(TOP_sar_segment_seqnum));
  POP_sar_segment_seqnum(@(Result^.Param))^.Value:=Value;
end;

function OPNew_SarMsgRefNum(Value:TPDUInteger16; ANext: PPDUOptMetaParam): PPDUOptMetaParam;
begin
  GetMem(Result, SizeOfTPDUOptMetaParamHeader + SizeOf(TOP_sar_msg_ref_num));
  FillChar(Result^, SizeOfTPDUOptMetaParamHeader + SizeOf(TOP_sar_msg_ref_num), 0);
  Result^.ParLength:=SizeOfTPDUOptMetaParamHeader + SizeOf(TOP_sar_msg_ref_num);
  Result^.Next:=ANext;
  POP_sar_msg_ref_num(@(Result^.Param))^.Tag:=SwapIntPDU16(sar_msg_ref_num);
  POP_sar_msg_ref_num(@(Result^.Param))^.Length:=SwapIntPDU16(SizeOf(TOP_sar_msg_ref_num));
  POP_sar_msg_ref_num(@(Result^.Param))^.Value:=SwapIntPDU16(Value);
end;

procedure DecodeDeliverSM(Hdr: PPDUHeader; out DeliverSM: TDeliverSM);
var
  P:PByteArray;
  iPos:PtrInt absolute P;
  SMLen:Byte;
  SMBuf:string;
begin
  DeliverSM:=TDeliverSM.Create;

  P:=PByteArray(Hdr);
  Inc(iPos, SizeOf(TPDUHeader)); //Skip PDU header

  DeliverSM.ServiceType:=PChar(P); //Get Service type
  Inc(iPos, Length(DeliverSM.ServiceType)+1);

  DeliverSM.SourceAddrTON:=TTONValue(PPDUInteger8(P)^); //Get Source TON value
  Inc(iPos, SizeOf(TPDUInteger8));

  DeliverSM.SourceAddrNPI:=PPDUInteger8(P)^; //Get Source NPI value
  Inc(iPos, SizeOf(TPDUInteger8));

  DeliverSM.SourceAddr:=PChar(P);  //Get Source Adress
  Inc(iPos, Length(DeliverSM.SourceAddr)+1);

  DeliverSM.DestAddrTON:=TTONValue(PPDUInteger8(P)^); //Get destination TON value
  Inc(iPos, SizeOf(TPDUInteger8));

  DeliverSM.DestAddrNPI:=PPDUInteger8(P)^; //Get destination NPI value
  Inc(iPos, SizeOf(TPDUInteger8));

  DeliverSM.DestAddr:=PChar(P);            //Get destination Adress
  Inc(iPos, Length(DeliverSM.DestAddr)+1);

  DeliverSM.EsmClass:=PPDUInteger8(P)^; //Get ESM class
  Inc(iPos, SizeOf(TPDUInteger8));

  DeliverSM.ProtocolId:=PPDUInteger8(P)^; //Get Protocol ID
  Inc(iPos, SizeOf(TPDUInteger8));

  DeliverSM.PriorityFlag:=PPDUInteger8(P)^; //Get Priority Flag
  Inc(iPos, SizeOf(TPDUInteger8));

  DeliverSM.ScheduleDeliveryTime:=PChar(P);            //Get Schedule Delivery Time
  Inc(iPos, Length(DeliverSM.ScheduleDeliveryTime)+1);

  DeliverSM.ValidityPeriod:=PChar(P);          //Get Validity Period
  Inc(iPos, Length(DeliverSM.ValidityPeriod)+1);

  DeliverSM.RegisteredDelivery:=PPDUInteger8(P)^; //Get  Registered Delivery
  Inc(iPos, SizeOf(TPDUInteger8));

  DeliverSM.ReplaceIfPresentFlag:=PPDUInteger8(P)^; //Get Replace If Present Flag
  Inc(iPos, SizeOf(TPDUInteger8));

  DeliverSM.DataCoding:=PPDUInteger8(P)^;           //Get Data Coding
  Inc(iPos, SizeOf(TPDUInteger8));

  DeliverSM.SMDefaultMsgId:=PPDUInteger8(P)^;       //Get SM Default Msg Id
  Inc(iPos, SizeOf(TPDUInteger8));

  SMLen:=PPDUInteger8(P)^;       //Get SM Length
  Inc(iPos, SizeOf(TPDUInteger8));

  if (DeliverSM.EsmClass and $40) <> 0 then
  begin
    SMLen:=SMLen - SizeOf(TUDHTextSegment);
    Move(P^, DeliverSM.UDHTextSegment, SizeOf(TUDHTextSegment));
    Inc(iPos, SizeOf(TUDHTextSegment));
  end;

  SetLength(SMBuf, SMLen);       //Get SM text
  Move(P^, SMBuf[1], SMLen);
  DeliverSM.ShortMessage:=ConvertSMToUTF8(SMBuf, DeliverSM.DataCoding);
  Inc(iPos, SMLen);

  //Далее необходимо обработать опциональные параметры
{

  //esm_class
  if Assigned(UDHTextSegment) then
  begin
    PPDUInteger8(Pos)^:=$40;
  end;


  if Assigned(UDHTextSegment) then
    PPDUInteger8(Pos)^:=short_message_length + SizeOf(TUDHTextSegment)
  else
    PPDUInteger8(Pos)^:=short_message_length;

{  if Len>0 then
    PPDUInteger8(Pos)^:=short_message_length;// Len+1; //sm_length}

  Inc(iPos, 1);

  if Assigned(UDHTextSegment) then
  begin
    Move(UDHTextSegment^, Pos^, SizeOf(TUDHTextSegment));
    Inc(iPos, SizeOf(TUDHTextSegment));
  end;

  if Len>1 then
    Move(short_message[1], Pos^, Len);

  Inc(iPos, Len);

  //Скопируем опциональные параметры
  P:=OptParams;
  while Assigned(P) do
  begin
    Len:=P^.ParLength - SizeOfTPDUOptMetaParamHeader;
    Move(P^.Param, Pos^, Len);
    Inc(iPos, Len);
    P:=P^.Next;
  end;

  FreePDUOptMetaParam(OptParams);}
end;

function bufEnquireLink(SequenceNumber: TPDUInteger32): PByteArray;
var
  Len:integer;
begin
  Len:=SizeOf(TPDUHeader);
  Getmem(Result, Len);
  FillChar(Result^, Len, 0);
  PPDUHeader(Result)^.Command_Length:=SwapIntPDU32(Len);
  PPDUHeader(Result)^.Command_ID:=SwapIntPDU32(enquire_link);
  PPDUHeader(Result)^.Command_Status:=0;
  PPDUHeader(Result)^.Sequence_Number:=SwapIntPDU32(SequenceNumber);
end;

function bufEnquireLinkResp(SequenceNumber: TPDUInteger32): PByteArray;
var
  Len:integer;
begin
  Len:=SizeOf(TPDUHeader);
  Getmem(Result, Len);
  FillChar(Result^, Len, 0);
  PPDUHeader(Result)^.Command_Length:=SwapIntPDU32(Len);
  PPDUHeader(Result)^.Command_ID:=SwapIntPDU32(enquire_link_resp);
  PPDUHeader(Result)^.Command_Status:=ESME_ROK;
  PPDUHeader(Result)^.Sequence_Number:=SwapIntPDU32(SequenceNumber);
end;

function bufDeliverSmResp(SequenceNumber: TPDUInteger32): PByteArray;
var
  Len:integer;
begin
  Len:=SizeOf(TDELIVER_SM_RESP);
  Getmem(Result, Len);
  FillChar(Result^, Len, 0);
  PPDUHeader(Result)^.Command_Length:=SwapIntPDU32(Len);
  PPDUHeader(Result)^.Command_ID:=SwapIntPDU32(deliver_sm_resp);
  PPDUHeader(Result)^.Command_Status:=ESME_ROK;
  PPDUHeader(Result)^.Sequence_Number:=SwapIntPDU32(SequenceNumber);
end;

procedure FreePDUBuffer(var Buf: PByteArray);
var
  Len:integer;
begin
  if Assigned(Buf) then
  begin
    Len:=SwapIntPDU32(PPDUHeader(Buf)^.Command_Length);
    Freemem(Buf, Len);
    Buf:=nil;
  end
end;

function DoBind(Command_ID:TPDUInteger32;  SequenceNumber:TPDUInteger32; system_id:string;
              password:string; system_typed:string; interface_version:TPDUInteger8;
              addr_ton:TPDUInteger8; addr_npi:TPDUInteger8; address_range:string):PByteArray;
var
  Len:integer;
  Pos:PByteArray;
  iPos:PtrInt absolute Pos;
begin
  Len:=SizeOf(TPDUHeader) +
    Min(Length(system_id), 15)+1+
    Min(Length(password),8)+1+
    Min(Length(system_typed),12)+1+
    SizeOf(TPDUInteger8)+
    SizeOf(TPDUInteger8)+
    SizeOf(TPDUInteger8)+
    Min(Length(address_range), 40)+1;

  Getmem(Result, Len);
  FillChar(Result^, Len, 0);
  Pos:=Result;
  PPDUHeader(Pos)^.Command_Length:=SwapIntPDU32(Len);
  PPDUHeader(Pos)^.Command_ID:=SwapIntPDU32(Command_ID);
  PPDUHeader(Pos)^.Command_Status:=0; //not used
  PPDUHeader(Pos)^.Sequence_Number:=SwapIntPDU32(SequenceNumber);
  Inc(iPos, SizeOf(TPDUHeader));

  Len:=Min(Length(system_id), 15);
  if Len>1 then
    Move(system_id[1], Pos^, Len);
  Inc(iPos, Len+1);

  Len:=Min(Length(password), 8);
  if Len>1 then
    Move(password[1], Pos^, Len);
  Inc(iPos, Len+1);

  Len:=Min(Length(system_typed), 12);
  if Len>1 then
    Move(system_typed[1], Pos^, Len);
  Inc(iPos, Len+1);

  PPDUInteger8(Pos)^:=interface_version;
  Inc(iPos, 1);

  PPDUInteger8(Pos)^:=addr_ton;
  Inc(iPos, 1);

  PPDUInteger8(Pos)^:=addr_npi;
  Inc(iPos, 1);

  Len:=Min(Length(address_range), 40);
  if Len>1 then
    Move(address_range[1], Pos^, Len);
end;


function bufBIND_TRANSMITTER(SequenceNumber:TPDUInteger32; system_id:string;
              password:string; system_typed:string; interface_version:TPDUInteger8;
              addr_ton:TPDUInteger8; addr_npi:TPDUInteger8; address_range:string):PByteArray;
begin
  Result:=DoBind(bind_transmitter,
                 SequenceNumber, system_id, password, system_typed,
                 interface_version, addr_ton, addr_npi, address_range)
end;


function bufBIND_RECEIVER(SequenceNumber:TPDUInteger32; system_id:string;
              password:string; system_typed:string; interface_version:TPDUInteger8;
              addr_ton:TPDUInteger8; addr_npi:TPDUInteger8; address_range:string):PByteArray;
begin
  Result:=DoBind(bind_receiver,
                 SequenceNumber, system_id, password, system_typed,
                 interface_version, addr_ton, addr_npi, address_range)
end;

function bufBIND_TRANSCEIVER(SequenceNumber:TPDUInteger32; system_id:string;
              password:string; system_typed:string; interface_version:TPDUInteger8;
              addr_ton:TPDUInteger8; addr_npi:TPDUInteger8; address_range:string):PByteArray;
begin
  Result:=DoBind(bind_transceiver,
                 SequenceNumber, system_id, password, system_typed,
                 interface_version, addr_ton, addr_npi, address_range)
end;

function bufSUBMIT_SM(SequenceNumber: TPDUInteger32; service_type, source_addr,
  destination_addr,  schedule_delivery_time, validity_period, short_message:string;
  source_addr_ton, source_addr_npi, dest_addr_ton, dest_addr_npi,
  data_coding:TPDUInteger8; short_message_length:TPDUInteger8;
  OptParams:PPDUOptMetaParam; UDHTextSegment:PUDHTextSegment): PByteArray;
var
  Len:integer;
  Command_ID:integer;
  Pos:PByteArray;
  iPos:PtrInt absolute Pos;
  P:PPDUOptMetaParam;
begin

  Len:=SizeOf(TPDUHeader) +
    Min(Length(service_type), 5)+1+  //service_type
    SizeOf(TPDUInteger8)+            //source_addr_ton
    SizeOf(TPDUInteger8) +           //source_addr_npi
    Min(Length(source_addr),20)+1+      //source_addr
    SizeOf(TPDUInteger8)+            //dest_addr_ton
    SizeOf(TPDUInteger8) +           //dest_addr_npi
    Min(Length(destination_addr),20)+1+      //destination_addr
    SizeOf(TPDUInteger8) +           //esm_class
    SizeOf(TPDUInteger8) +           //protocol_id
    SizeOf(TPDUInteger8) +           //priority_flag
    Min(Length(schedule_delivery_time),16)+1+      //schedule_delivery_time
    Min(Length(validity_period),16)+1+      //validity_period
    SizeOf(TPDUInteger8) +           //registered_delivery
    SizeOf(TPDUInteger8) +           //replace_if_present_flag
    SizeOf(TPDUInteger8) +           //data_coding
    SizeOf(TPDUInteger8) +           //sm_default_msg_id
    SizeOf(TPDUInteger8) +           //sm_length
      Min(Length(short_message), 253);      //short_message

  if Assigned(UDHTextSegment) then
    Inc(Len, SizeOf(TUDHTextSegment));

  //Расчитаем длинну опциональных параметров
  P:=OptParams;
  while Assigned(P) do
  begin
    inc(Len, P^.ParLength - SizeOfTPDUOptMetaParamHeader);
    P:=P^.Next;
  end;

  Getmem(Result, Len);
  FillChar(Result^, Len, 0);
  Pos:=Result;

  //пока без блока дополнительных параметров - нужно будет - сделаем
  Command_ID:=submit_sm;
  PPDUHeader(Pos)^.Command_Length:=SwapIntPDU32(Len);
  PPDUHeader(Pos)^.Command_ID:=SwapIntPDU32(Command_ID);
  PPDUHeader(Pos)^.Command_Status:=0; //not used
  PPDUHeader(Pos)^.Sequence_Number:=SwapIntPDU32(SequenceNumber);
  Inc(iPos, SizeOf(TPDUHeader));

  Len:=Min(Length(service_type), 5);
  if Len>1 then
    Move(service_type[1], Pos^, Len);
  Inc(iPos, Len+1);

  PPDUInteger8(Pos)^:=source_addr_ton;
  Inc(iPos, 1);
  PPDUInteger8(Pos)^:=source_addr_npi;
  Inc(iPos, 1);


  Len:=Min(Length(source_addr), 20);
  if Len>1 then
    Move(source_addr[1], Pos^, Len);
  Inc(iPos, Len+1);

  PPDUInteger8(Pos)^:=dest_addr_ton;
  Inc(iPos, 1);
  PPDUInteger8(Pos)^:=dest_addr_npi;
  Inc(iPos, 1);

  Len:=Min(Length(destination_addr), 20);
  if Len>1 then
    Move(destination_addr[1], Pos^, Len);
  Inc(iPos, Len+1);

  //esm_class
  if Assigned(UDHTextSegment) then
  begin
    PPDUInteger8(Pos)^:=$40;
  end;

  Inc(iPos, 1);
  //PPDUInteger8(Pos)^:=protocol_id
  Inc(iPos, 1);

  PPDUInteger8(Pos)^:=1; //priority_flag
  Inc(iPos, 1);

  Len:=Min(Length(schedule_delivery_time), 16);
  if Len>1 then
    Move(schedule_delivery_time[1], Pos^, Len);
  Inc(iPos, Len+1);

  Len:=Min(Length(validity_period), 16);
  if Len>1 then
    Move(validity_period[1], Pos^, Len);
  Inc(iPos, Len+1);

  //PPDUInteger8(Pos)^:=registered_delivery
  Inc(iPos, 1);
  //PPDUInteger8(Pos)^:=replace_if_present_flag
  Inc(iPos, 1);
  PPDUInteger8(Pos)^:=data_coding;
  Inc(iPos, 1);
  //PPDUInteger8(Pos)^:=sm_default_msg_id
  Inc(iPos, 1);

  Len:=Min(Length(short_message), 253);

  if Assigned(UDHTextSegment) then
    PPDUInteger8(Pos)^:=short_message_length + SizeOf(TUDHTextSegment)
  else
    PPDUInteger8(Pos)^:=short_message_length;

{  if Len>0 then
    PPDUInteger8(Pos)^:=short_message_length;// Len+1; //sm_length}

  Inc(iPos, 1);

  if Assigned(UDHTextSegment) then
  begin
    Move(UDHTextSegment^, Pos^, SizeOf(TUDHTextSegment));
    Inc(iPos, SizeOf(TUDHTextSegment));
  end;

  if Len>1 then
    Move(short_message[1], Pos^, Len);

  Inc(iPos, Len);

  //Скопируем опциональные параметры
  P:=OptParams;
  while Assigned(P) do
  begin
    Len:=P^.ParLength - SizeOfTPDUOptMetaParamHeader;
    Move(P^.Param, Pos^, Len);
    Inc(iPos, Len);
    P:=P^.Next;
  end;

  FreePDUOptMetaParam(OptParams);


{

   1     Integer Indicates the short message to
                                  send from a list of pre-
                                  defined (‘canned’) short
                                  messages stored on the
                                  SMSC. If not using an SMSC
                                  canned message, set to
                                  NULL.
           1     Integer Length in octets of the        5.2.21
                                  short_message user data.
M
A                                                   5.2.22
                    Var.  Octet   Up to 254 octets of short
N                   0-254 String  message user data.
D                                 The exact physical limit for
A                                 short_message size may vary
T                                 according to the underlying
O                                 network.
R
Y                                 Applications which need to
                                  send messages longer than
P                                 254 octets should use the
A                                 message_payload parameter.
R                                 In this case the sm_length
A                                 field should be set to zero.
M
E                                 Note:
T                                 The short message data
E                                 should be inserted in either
R                                 the short_message or
S                                 message_payload fields.
                                  Both fields must not be used
                                  simultaneously.
}
{  user_message_reference TLV ESME assigned message          5.3.2.17
                             reference number.
  source_port            TLV Indicates the application port 5.3.2.20
                             number associated with the
                             source address of the
                             message. This parameter
                             should be present for WAP
O
                             applications.
P
T source_addr_subunit    TLV The subcomponent in the        5.3.2.2
I                            destination device which
O                            created the user data.
N
A destination_port       TLV Indicates the application port 5.3.2.21
                             number associated with the
L
                             destination address of the
                             message. This parameter
P
                             should be present for WAP
A
                             applications.
R
A
  dest_addr_subunit      TLV The subcomponent in the        5.3.2.1
M
                             destination device for which
E
                             the user data is intended.
T
E sar_msg_ref_num                                           5.3.2.22
                         TLV The reference number for a
R                            particular concatenated short
S                            message.
  sar_total_segments     TLV Indicates the total number of  5.3.2.23
                             short messages within the
                             concatenated short message.
                                                            5.3.2.24
  sar_segment_seqnum     TLV Indicates the sequence
                             number of a particular short
                             message fragment within the
                             concatenated short message.
  more_messages_to_send  TLV Indicates that there are more  5.3.2.34
                             messages to follow for the
                             destination SME.
  payload_type           TLV defines the type of payload
                             (e.g. WDP, WCMP, etc.).

                                                           5.3.2.32
  message_payload       TLV Contains the extended short
                            message user data. Up to 64K
                            octets can be transmitted.
                            Note: The short message data
                            should be inserted in either
                            the short_message or
                            message_payload fields. Both
O
                            fields should not be used
P
                            simultaneously.
T
I
                            The sm_length field should be
O
                            set to zero if using the
N
                            message_payload parameter.
A
L
  privacy_indicator     TLV Indicates the level of privacy 5.3.2.14
                            associated with the message.
P
A callback_num                                             5.3.2.36
                        TLV A callback number associated
R                           with the short message.
A                           This parameter can be
M                           included a number of times
E                           for multiple callback
T                           addresses.
E
                                                           5.3.2.37
                        TLV Defines the callback number
R callback_num_pres_ind
                            presentation and screening.
S
                            If this parameter is present
                            and there are multiple
                            instances of the callback_num
                            parameter then this parameter
                            must occur an equal number
                            of instances and the order of
                            occurrence determines the
                            particular
                            callback_num_pres_ind
                            which corresponds to a
                            particular callback_num.
                                                            5.3.2.38
  callback_num_atag      TLV Associates a displayable
                             alphanumeric tag with the
                             callback number.
                             If this parameter is present
                             and there are multiple
                             instances of the callback_num
                             parameter then this parameter
                             must occur an equal number
                             of instances and the order of
                             occurrence determines the
                             particular callback_num_atag
O
                             which corresponds to a
P
                             particular callback_num.
T
I
O source_subaddress      TLV The subaddress of the          5.3.2.15
                             message originator.
N
A dest_subaddress        TLV The subaddress of the          5.3.2.16
L                            message destination.
P user_response_code     TLV A user response code. The      5.3.2.18
                             actual response codes are
A
                             implementation specific.
R
A
  display_time           TLV Provides the receiving MS      5.3.2.26
M
                             with a display time associated
E
                             with the message.
T
E sms_signal             TLV Indicates the alerting         5.3.2.40
R                            mechanism when the message
S                            is received by an MS.
                                                            5.3.2.27
  ms_validity            TLV Indicates validity information
                             for this message to the
                             recipient MS.
  ms_msg_wait_facilities TLV This parameter controls the    5.3.2.13
                             indication and specifies the
                             message type (of the message
                             associated with the MWI) at
                             the mobile station.
  number_of_messages     TLV Indicates the number of        5.3.2.39
                             messages stored in a mail box
  alert_on_msg_delivery  TLV Request an MS alert signal be  5.3.2.41
                             invoked on message delivery.
  language_indicator     TLV Indicates the language of an   5.3.2.19
                             alphanumeric text message.

O its_reply_type   TLV The MS user’s reply method    5.3.2.42
                       to an SMS delivery message
P
                       received from the network is
T
                       indicated and controlled by
I
                       this parameter.
O
N
A its_session_info TLV Session control information   5.3.2.43
                       for Interactive Teleservice.
L
  ussd_service_op                                    5.3.2.44
                   TLV This parameter is used to
P                      identify the required USSD
A                      Service type when interfacing
R                      to a USSD system.
A
M
E
T
E
R
S
}
end;

function bufUNBIND(SequenceNumber: TPDUInteger32): PByteArray;
var
  Len:integer;
  Command_ID:TPDUInteger32;
begin
  Len:=SizeOf(TPDUHeader);
  Getmem(Result, Len);
  FillChar(Result^, 0, Len);
  Command_ID:=unbind;
  PPDUHeader(Result)^.Command_Length:=SwapIntPDU32(Len);
  PPDUHeader(Result)^.Command_ID:=SwapIntPDU32(Command_ID);
  PPDUHeader(Result)^.Command_Status:=0; //not used
  PPDUHeader(Result)^.Sequence_Number:=SwapIntPDU32(SequenceNumber);
end;



{ TDeliverSM }

constructor TDeliverSM.Create;
begin
  inherited Create;
end;

end.

