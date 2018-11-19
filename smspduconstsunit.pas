unit smsPDUConstsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  SMPP_Version = $33;

type
  //5.2.5        addr_ton, source_addr_ton, dest_addr_ton, esme_addr_ton
  //These fields define the Type of Number (TON) to be used in the SME address parameters. The following TON values are defined:
  //All other values reserved
  TTONValue = (tonUnknown          = $00000000,         // 00000000
               tonInternational    = $00000001,         //00000001
               tonNational         = $00000002,         //00000010
               tonNetworkSpecific  = $00000003,         //00000011
               tonSubscriberNumber = $00000004,         //00000100
               tonAlphanumeric     = $00000005,         //00000101
               tonAbbreviated      = $00000006);        //00000110

type
  PPDUInteger32 = ^TPDUInteger32;
  TPDUInteger32 = Cardinal;

  PPDUInteger16 = ^TPDUInteger16;
  TPDUInteger16 = word;

  PPDUInteger8 = ^TPDUInteger8;
  TPDUInteger8 = byte;

const
  //SMPP Command set
  generic_nack           = $80000000;
  bind_receiver          = $00000001;
  bind_receiver_resp     = $80000001;
  bind_transmitter       = $00000002;
  bind_transmitter_resp  = $80000002;
  query_sm               = $00000003;
  query_sm_resp          = $80000003;
  submit_sm              = $00000004;
  submit_sm_resp         = $80000004;
  deliver_sm             = $00000005;
  deliver_sm_resp        = $80000005;
  unbind                 = $00000006;
  unbind_resp            = $80000006;
  replace_sm             = $00000007;
  replace_sm_resp        = $80000007;
  cancel_sm              = $00000008;
  cancel_sm_resp         = $80000008;
  bind_transceiver       = $00000009;
  bind_transceiver_resp  = $80000009;
//  Reserved               = $0000000A;
//  Reserved               = $8000000A;
  outbind                = $0000000B;
//  Reserved               = $0000000C -  = $00000014
//  Reserved               = $8000000B -  = $80000014
  enquire_link           = $00000015;
  enquire_link_resp      = $80000015;
//  Reserved               = $00000016 -  = $00000020
//  Reserved               = $80000016 -  = $80000020
  submit_multi           = $00000021;
  submit_multi_resp      = $80000021;
//  Reserved               = $00000022 -  = $000000FF
//  Reserved               = $80000022 -  = $800000FF
//  Reserved               = $00000100
//  Reserved               = $80000100
//  Reserved               = $00000101
//  Reserved               = $80000101
  alert_notification     = $00000102;
//  Reserved                     = $80000102
  data_sm                = $00000103;
  data_sm_resp           = $80000103;
{
  Reserved for SMPP extension  = $00000104 -  = $0000FFFF
                               = $80000104 -  = $8000FFFF
  Reserved                     = $00010000 -  = $000101FF
                               = $80010000 -  = $800101FF
  Reserved for SMSC Vendor     = $00010200 -  = $000102FF
                               = $80010200 -  = $800102FF
  Reserved                     = $00010300 -  = $FFFFFFFF
}

const
  //The complete set of SMPP Error Codes and their associated values are defined in the following table.
  ESME_ROK            = $00000000; // No Error
  ESME_RINVMSGLEN     = $00000001; //  Message Length is invalid
  ESME_RINVCMDLEN     = $00000002; //  Command Length is invalid
  ESME_RINVCMDID      = $00000003; //  Invalid Command ID
  ESME_RINVBNDSTS     = $00000004; //  Incorrect BIND Status for given command
  ESME_RALYBND        = $00000005; //  ESME Already in Bound State
  ESME_RINVPRTFLG     = $00000006; //  Invalid Priority Flag
  ESME_RINVREGDLVFLG  = $00000007; //  Invalid Registered Delivery Flag
  ESME_RSYSERR        = $00000008; //  System Error
//  Reserved            = $00000009 Reserved
  ESME_RINVSRCADR     = $0000000A; //  Invalid Source Address
  ESME_RINVDSTADR     = $0000000B; //  Invalid Dest Addr
  ESME_RINVMSGID      = $0000000C; //  Message ID is invalid
  ESME_RBINDFAIL      = $0000000D; //  Bind Failed
  ESME_RINVPASWD      = $0000000E; //  Invalid Password
  ESME_RINVSYSID      = $0000000F; //  Invalid System ID
  Reserved            = $00000010; //  Reserved
  ESME_RCANCELFAIL    = $00000011; //  Cancel SM Failed
//  Reserved            = $00000012 Reserved
  ESME_RREPLACEFAIL   = $00000013; //  Replace SM Failed
  ESME_RMSGQFUL       = $00000014; //   Message Queue Full
  ESME_RINVSERTYP     = $00000015; //  Invalid Service Type
//  Reserved            = $00000016- Reserved
//                      = $00000032
  ESME_RINVNUMDESTS   = $00000033; //   Invalid number of destinations
  ESME_RINVDLNAME     = $00000034; //   Invalid Distribution List name
//  Reserved            = $00000035-  Reserved
//                      = $0000003F
  ESME_RINVDESTFLAG   = $00000040; //   Destination      flag     is    invalid (submit_multi)
//  Reserved            = $00000041;    Reserved
  ESME_RINVSUBREP     = $00000042; //   Invalid ‘submit with replace’ request (i.e. submit_sm with replace_if_present_flag set)
  ESME_RINVESMCLASS   = $00000043; //   Invalid esm_class field data
  ESME_RCNTSUBDL      = $00000044; //   Cannot Submit to Distribution List
  ESME_RSUBMITFAIL    = $00000045; //   submit_sm or submit_multi failed
//  Reserved            = $00000046- Reserved
//                      = $00000047
  ESME_RINVSRCTON     = $00000048; //   Invalid Source address TON
  ESME_RINVSRCNPI     = $00000049; //   Invalid Source address NPI
  ESME_RINVDSTTON     = $00000050; //   Invalid Destination address TON
  ESME_RINVDSTNPI     = $00000051; //   Invalid Destination address NPI
//  Reserved            = $00000052  Reserved
  ESME_RINVSYSTYP     = $00000053; //   Invalid system_type field
  ESME_RINVREPFLAG    = $00000054; //   Invalid replace_if_present flag
  ESME_RINVNUMMSGS    = $00000055; //   Invalid number of messages
//  Reserved            = $00000056-
//                      = $00000057  Reserved
  ESME_RTHROTTLED     = $00000058; //   Throttling error (ESME has exceeded allowed message limits)
//  Reserved            = $00000059- Reserved
//                      = $00000060
  ESME_RUNKNOW1_BEE   = $0000062C; //Ошибочный номер на BeeLine


const
  //Optional Parameter Tag values
  dest_addr_subunit              = $0005;//        GSM
  dest_network_type              = $0006;//        Generic
  dest_bearer_type               = $0007;//        Generic
  dest_telematics_id             = $0008;//        GSM
  source_addr_subunit            = $000D;//        GSM
  source_network_type            = $000E;//        Generic
  source_bearer_type             = $000F;//        Generic
  source_telematics_id           = $0010;//        GSM
  qos_time_to_live               = $0017;//        Generic
  payload_type                   = $0019;//        Generic
  additional_status_info_text    = $001D;//        Generic
  receipted_message_id           = $001E;//        Generic
  ms_msg_wait_facilities         = $0030;//        GSM
  privacy_indicator              = $0201;//        CDMA, TDMA
  source_subaddress              = $0202;//        CDMA, TDMA
  dest_subaddress                = $0203;//        CDMA, TDMA
  user_message_reference         = $0204;//        Generic
  user_response_code             = $0205;//        CDMA, TDMA
  source_port                    = $020A;//        Generic
  destination_port               = $020B;//        Generic
  sar_msg_ref_num                = $020C;//        Generic
  language_indicator             = $020D;//        CDMA, TDMA
  sar_total_segments             = $020E;//        Generic
  sar_segment_seqnum             = $020F;//        Generic
  SC_interface_version           = $0210;//        Generic
  callback_num_pres_ind          = $0302;//        TDMA
  callback_num_atag              = $0303;//        TDMA
  number_of_messages             = $0304;//        CDMA
  callback_num                   = $0381;//        CDMA, TDMA, GSM, iDEN
  dpf_result                     = $0420;//        Generic
  set_dpf                        = $0421;//        Generic
  ms_availability_status         = $0422;//        Generic
  network_error_code             = $0423;//        Generic
  message_payload                = $0424;//        Generic
  delivery_failure_reason        = $0425;//        Generic
  more_messages_to_send          = $0426;//        GSM
  message_state                  = $0427;//        Generic
  ussd_service_op                = $0501;//        GSM (USSD)
  display_time                   = $1201;//        CDMA, TDMA
  sms_signal                     = $1203;//        TDMA
  ms_validity                    = $1204;//        CDMA, TDMA
  alert_on_message_delivery      = $130C;//        CDMA
  its_reply_type                 = $1380;//        CDMA
  its_session_info               = $1383;//        CDMA

type
  TDELIVER_SM_RESP = packed record
    command_length : TPDUInteger32; // 4 Integer  Set to overall length of PDU.   5.1.1
    command_id     : TPDUInteger32; // 4 Integer  deliver_sm_resp                 5.1.2
    command_status : TPDUInteger32; // 4 Integer  Indicates outcome of deliver_sm request.   5.1.3
    sequence_number: TPDUInteger32; // 4 Integer  Set to sequence number of deliver_sm PDU.  5.1.4
    message_id     : TPDUInteger8;  // 1 C-Octet String  This field is unused and is set to NULL.
  end;

type
  POP_sar_msg_ref_num = ^TOP_sar_msg_ref_num;
  TOP_sar_msg_ref_num = packed record
    Tag    : TPDUInteger16;//2 Integer The Tag field is used to uniquely identify the particular optional parameter in question.
    Length : TPDUInteger16;//2 Integer The Length field indicates the length of the Value field in octets.
    Value  : TPDUInteger16;//2 Integer This parameter shall contain a originator generated reference number so that a segmented short message may be
                           //  reassembled into a single original message. This allows the parallel transmission of several segmented messages. This
                           // reference number shall remain constant for every segment which makes up a particular concatenated short message.
                           //When present, the PDU must also contain the sar_total_segments and sar_segment_seqnum parameters.
                           //Otherwise this parameter shall be ignored.
  end;

  POP_sar_total_segments = ^TOP_sar_total_segments;
  TOP_sar_total_segments = packed record
    Tag    : TPDUInteger16;//2 Integer The Tag field is used to uniquely identify the particular optional parameter in question.
    Length : TPDUInteger16;//2 Integer The Length field indicates the length of the Value field in octets.
    Value  : TPDUInteger8; //2 Integer This parameter shall contain a value in the range 1 to 255 indicating the total number
                           //of fragments within the concatenated short message. The value shall start at 1 and remain constant for every short message
                           //which makes up the concatenated short message. When present, the PDU must also contain the sar_msg_ref_num and
                           //sar_segment_seqnum parameters. Otherwise this parameter shall be ignored.
  end;

  POP_sar_segment_seqnum = ^TOP_sar_segment_seqnum;
  TOP_sar_segment_seqnum = packed record
    Tag    : TPDUInteger16;//2 Integer The Tag field is used to uniquely identify the particular optional parameter in question.
    Length : TPDUInteger16;//2 Integer The Length field indicates the length of the Value field in octets.
    Value  : TPDUInteger8; //2 Integer This octet shall contain a value in the range 1 to 255 indicating the sequence number of a particular message
                           //within the concatenated short message. The value shall start at 1 and increment by one for every message sent within the
                           //concatenated short message. When present, the PDU must also contain the sar_total_segments and sar_msg_ref_num parameters.
                           //Otherwise this parameter shall be ignored.
  end;



function SMPPErrorCodeStr(ErrorCode:integer):string;
function SmppCommandName(cmd:TPDUInteger32):string;

resourcestring
  sESME_ROK            = 'No Error';
  sESME_RINVMSGLEN     = 'Message Length is invalid';
  sESME_RINVCMDLEN     = 'Command Length is invalid';
  sESME_RINVCMDID      = 'Invalid Command ID';
  sESME_RINVBNDSTS     = 'Incorrect BIND Status for given command';
  sESME_RALYBND        = 'ESME Already in Bound State';
  sESME_RINVPRTFLG     = 'Invalid Priority Flag';
  sESME_RINVREGDLVFLG  = 'Invalid Registered Delivery Flag';
  sESME_RSYSERR        = 'System Error';
  sESME_RINVSRCADR     = 'Invalid Source Address';
  sESME_RINVDSTADR     = 'Invalid Dest Addr';
  sESME_RINVMSGID      = 'Message ID is invalid';
  sESME_RBINDFAIL      = 'Bind Failed';
  sESME_RINVPASWD      = 'Invalid Password';
  sESME_RINVSYSID      = 'Invalid System ID';
  sESME_RCANCELFAIL    = 'Cancel SM Failed';
  sESME_RREPLACEFAIL   = 'Replace SM Failed';
  sESME_RMSGQFUL       = 'Message Queue Full';
  sESME_RINVSERTYP     = 'Invalid Service Type';
  sESME_RINVNUMDESTS   = 'Invalid number of destinations';
  sESME_RINVDLNAME     = 'Invalid Distribution List name';
  sESME_RINVDESTFLAG   = 'Destination flag is invalid (submit_multi)';
  sESME_RINVSUBREP     = 'Invalid ''submit with replace'' request (i.e. submit_sm with replace_if_present_flag set)';
  sESME_RINVESMCLASS   = 'Invalid esm_class field data';
  sESME_RCNTSUBDL      = 'Cannot Submit to Distribution List';
  sESME_RSUBMITFAIL    = 'submit_sm or submit_multi failed';
  sESME_RINVSRCTON     = 'Invalid Source address TON';
  sESME_RINVSRCNPI     = 'Invalid Source address NPI';
  sESME_RINVDSTTON     = 'Invalid Destination address TON';
  sESME_RINVDSTNPI     = 'Invalid Destination address NPI';
  sESME_RINVSYSTYP     = 'Invalid system_type field';
  sESME_RINVREPFLAG    = 'Invalid replace_if_present flag';
  sESME_RINVNUMMSGS    = 'Invalid number of messages';
  sESME_RTHROTTLED     = 'Throttling error (ESME has exceeded allowed message limits)';
  sESME_RUNKNOW1_BEE   = 'Error in phone numbe on beeline';
  sESME_UNKOW          = 'Unknow';

resourcestring
  sUnknowBindType      = 'Unknow bind type';

implementation

function SMPPErrorCodeStr(ErrorCode: integer): string;
begin
  case ErrorCode of
    ESME_ROK           :Result:=sESME_ROK;
    ESME_RINVMSGLEN    :Result:=sESME_RINVMSGLEN;
    ESME_RINVCMDLEN    :Result:=sESME_RINVCMDLEN;
    ESME_RINVCMDID     :Result:=sESME_RINVCMDID;
    ESME_RINVBNDSTS    :Result:=sESME_RINVBNDSTS;
    ESME_RALYBND       :Result:=sESME_RALYBND;
    ESME_RINVPRTFLG    :Result:=sESME_RINVPRTFLG;
    ESME_RINVREGDLVFLG :Result:=sESME_RINVREGDLVFLG;
    ESME_RSYSERR       :Result:=sESME_RSYSERR;
    ESME_RINVSRCADR    :Result:=sESME_RINVSRCADR;
    ESME_RINVDSTADR    :Result:=sESME_RINVDSTADR;
    ESME_RINVMSGID     :Result:=sESME_RINVMSGID;
    ESME_RBINDFAIL     :Result:=sESME_RBINDFAIL;
    ESME_RINVPASWD     :Result:=sESME_RINVPASWD;
    ESME_RINVSYSID     :Result:=sESME_RINVSYSID;
    ESME_RCANCELFAIL   :Result:=sESME_RCANCELFAIL;
    ESME_RREPLACEFAIL  :Result:=sESME_RREPLACEFAIL;
    ESME_RMSGQFUL      :Result:=sESME_RMSGQFUL;
    ESME_RINVSERTYP    :Result:=sESME_RINVSERTYP;
    ESME_RINVNUMDESTS  :Result:=sESME_RINVNUMDESTS;
    ESME_RINVDLNAME    :Result:=sESME_RINVDLNAME;
    ESME_RINVDESTFLAG  :Result:=sESME_RINVDESTFLAG;
    ESME_RINVSUBREP    :Result:=sESME_RINVSUBREP;
    ESME_RINVESMCLASS  :Result:=sESME_RINVESMCLASS;
    ESME_RCNTSUBDL     :Result:=sESME_RCNTSUBDL;
    ESME_RSUBMITFAIL   :Result:=sESME_RSUBMITFAIL;
    ESME_RINVSRCTON    :Result:=sESME_RINVSRCTON;
    ESME_RINVSRCNPI    :Result:=sESME_RINVSRCNPI;
    ESME_RINVDSTTON    :Result:=sESME_RINVDSTTON;
    ESME_RINVDSTNPI    :Result:=sESME_RINVDSTNPI;
    ESME_RINVSYSTYP    :Result:=sESME_RINVSYSTYP;
    ESME_RINVREPFLAG   :Result:=sESME_RINVREPFLAG;
    ESME_RINVNUMMSGS   :Result:=sESME_RINVNUMMSGS;
    ESME_RTHROTTLED    :Result:=sESME_RTHROTTLED;
    ESME_RUNKNOW1_BEE  :Result:=sESME_RUNKNOW1_BEE;
  else
    Result:=sESME_UNKOW;
  end;
end;

function SmppCommandName(cmd:TPDUInteger32):string;
begin
  case cmd of
    generic_nack          :Result:='generic_nack';
    bind_receiver         :Result:='bind_receiver';
    bind_receiver_resp    :Result:='bind_receiver_resp';
    bind_transmitter      :Result:='bind_transmitter';
    bind_transmitter_resp :Result:='bind_transmitter_resp';
    query_sm              :Result:='query_sm';
    query_sm_resp         :Result:='query_sm_resp';
    submit_sm             :Result:='submit_sm';
    submit_sm_resp        :Result:='submit_sm_resp';
    deliver_sm            :Result:='deliver_sm';
    deliver_sm_resp       :Result:='deliver_sm_resp';
    unbind                :Result:='unbind';
    unbind_resp           :Result:='unbind_resp';
    replace_sm            :Result:='replace_sm';
    replace_sm_resp       :Result:='replace_sm_resp';
    cancel_sm             :Result:='cancel_sm';
    cancel_sm_resp        :Result:='cancel_sm_resp';
    bind_transceiver      :Result:='bind_transceiver';
    bind_transceiver_resp :Result:='bind_transceiver_resp';
  //  Reserved               = $0000000A;
  //  Reserved               = $8000000A;
    outbind               :Result:='outbind';
  //  Reserved               = $0000000C -  = $00000014
  //  Reserved               = $8000000B -  = $80000014
    enquire_link          :Result:='enquire_link';
    enquire_link_resp     :Result:='enquire_link_resp';
  //  Reserved               = $00000016 -  = $00000020
  //  Reserved               = $80000016 -  = $80000020
    submit_multi          :Result:='submit_multi';
    submit_multi_resp     :Result:='submit_multi_resp';
  //  Reserved               = $00000022 -  = $000000FF
  //  Reserved               = $80000022 -  = $800000FF
  //  Reserved               = $00000100
  //  Reserved               = $80000100
  //  Reserved               = $00000101
  //  Reserved               = $80000101
    alert_notification    :Result:='alert_notification';
  //  Reserved                     = $80000102
    data_sm               :Result:='data_sm';
    data_sm_resp          :Result:='data_sm_resp';
  else
    result:='Other :';
  end;
  Result:=Result + ' (' + IntToStr(Cmd) + ')';
end;

end.

