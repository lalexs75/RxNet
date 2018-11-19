unit smsPDUOptParamTypesUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, smsPDUConstsUnit;

type
{- dest_addr_subunit

The dest_addr_subunit parameter is used to route messages when received by a mobile station,
for example to a smart card in the mobile station or to an external device connected to the
mobile station.
                          Size
        Field                         Type                        Description
                         octets
  Parameter Tag        2          Integer         dest_addr_subunit
  Length               2          Integer        Length of Value part in octets
  Value                1          Integer        0x00 = Unknown (default)
                                                 0x01 = MS Display
                                                 0x02 = Mobile Equipment
                                                 0x03 = Smart Card 1 (expected to be SIM if
                                                          a SIM exists in the MS)
                                                 0x04 = External Unit 1
                                                 5 to 255 = reserved
}
  TDestAddrSubunit = packed record
    Tag:TPDUInteger16;//dest_addr_subunit
    Length:TPDUInteger16;//Length of Value part in octets
    Value:TPDUInteger8;
  end;

{- source_addr_subunit

The source_addr_subunit parameter is used to indicate where a message originated in the
mobile station, for example a smart card in the mobile station or an external device connected
to the mobile station.
                          Size
        Field                         Type                        Description
                         octets
  Parameter Tag        2          Integer        source_addr_subunit
  Length               2          Integer        Length of Value part in octets
  Value                1          Integer        see 5.3.2.1

}
  TSourceAddrSubunit = packed record
    Tag:TPDUInteger16;//source_addr_subunit
    Length:TPDUInteger16;//Length of Value part in octets
    Value:TPDUInteger8;
  end;

{- dest_network_type

The dest_network_type parameter is used to indicate a network type associated with the
destination address of a message. In the case that the receiving system (e.g. SMSC) does not
support the indicated network type, it may treat this a failure and return a response PDU
reporting a failure.
                           Size
         Field                         Type                       Description
                          octets
  Parameter Tag         2           Integer      dest_network_type
  Length                2           Integer      Length of Value part in octets
  Value                 1           Integer      0x00 = Unknown
                                                 0x01 = GSM
                                                 0x02 = ANSI-136/TDMA
                                                 0x03 = IS-95/CDMA
                                                 0x04 = PDC
                                                 0x05 = PHS
                                                 0x06 = iDEN
                                                 0x07 = AMPS
                                                 0x08 = Paging Network
                                                 9 to 255 = reserved
}
  TDestNetworkType = packed record
    Tag:TPDUInteger16;//dest_network_type
    Length:TPDUInteger16;//Length of Value part in octets
    Value:TPDUInteger8;
  end;

{- source_network_type
The source_network_type parameter is used to indicate the network type associated with the
device that originated the message.
                           Size
         Field                         Type                       Description
                          octets
  Parameter Tag         2           Integer      source_network_type
  Length                2           Integer      Length of Value part in octets
  Value                 1           Integer      see 5.3.2.3

}
  TSourceNetworkType = packed record
    Tag:TPDUInteger16;//source_network_type
    Length:TPDUInteger16;//Length of Value part in octets
    Value:TPDUInteger8;
  end;

{ - dest_bearer_type
The dest_bearer_type parameter is used to request the desired bearer for delivery of the
message to the destination address. In the case that the receiving system (e.g. SMSC) does not
support the indicated bearer type, it may treat this a failure and return a response PDU reporting
a failure.
                          Size
         Field                           Type                         Description
                         octets
 Parameter Tag         2              Integer        dest_bearer_type
 Length                2              Integer        Length of Value part in octets
 Value                 1              Integer        0x00 = Unknown
                                                     0x01 = SMS
                                                     0x02 = Circuit Switched Data (CSD)
                                                     0x03 = Packet Data
                                                     0x04 = USSD
                                                     0x05 = CDPD
                                                     0x06 = DataTAC
                                                     0x07 = FLEX/ReFLEX
                                                     0x08 = Cell Broadcast (cellcast)
                                                     9 to 255 = reserved
}
  TDestBearerType = packed record
    Tag:TPDUInteger16;//dest_bearer_type
    Length:TPDUInteger16;//Length of Value part in octets
    Value:TPDUInteger8;
  end;

{- source_bearer_type
The source_bearer_type parameter indicates the wireless bearer over which the message
originated.
                          Size
         Field                           Type                         Description
                         octets
 Parameter Tag         2              Integer        source_bearer_type
 Length                2              Integer        Length of Value part in octets
 Value                 1              Integer        see 5.3.2.5

}
  TSourceBearerType = packed record
    Tag:TPDUInteger16;//source_bearer_type
    Length:TPDUInteger16;//Length of Value part in octets
    Value:TPDUInteger8;
  end;


{- dest_telematics_id
This parameter defines the telematic interworking to be used by the delivering system for the
destination address. This is only useful when a specific dest_bearer_type parameter has also
been specified as the value is bearer dependent. In the case that the receiving system (e.g.
SMSC) does not support the indicated telematic interworking, it may treat this a failure and
return a response PDU reporting a failure.
                          Size
         Field                        Type                      Description
                         octets
  Parameter Tag        2           Integer       dest_telematics_id
  Length               2           Integer       Length of Value part in octets
  Value                2           Integer       to be defined
}
  TDestTelematicsID = packed record
    Tag:TPDUInteger16;//dest_telematics_id
    Length:TPDUInteger16;//Length of Value part in octets
    Value:TPDUInteger16;
  end;

{- source_telematics_id
The source_telematics_id parameter indicates the type of telematics interface over which the
message originated.
                          Size
         Field                        Type                      Description
                         octets
  Parameter Tag        2           Integer       source_telematics_id
  Length               2           Integer       Length of Value part in octets
  Value                1           Integer       see 5.3.2.7
}
  TSourceTelematicsID = packed record
    Tag:TPDUInteger16;//source_telematics_id
    Length:TPDUInteger16;//Length of Value part in octets
    Value:TPDUInteger8;
  end;

{- qos_time_to_live
This parameter defines the number of seconds which the sender requests the SMSC to keep the
message if undelivered before it is deemed expired and not worth delivering. If the parameter
is not present, the SMSC may apply a default value.
                          Size
         Field                         Type                      Description
                         octets
  Parameter Tag        2            Integer      qos_time_to_live
  Length               2            Integer      Length of Value part in octets
  Value                4            Integer      number of seconds for message to be
                                                 retained by the receiving system.
}
  TQosTimeToLive = packed record
    Tag:TPDUInteger16;//qos_time_to_live
    Length:TPDUInteger16;//Length of Value part in octets
    Value:TPDUInteger32;
  end;

{- payload_type
The payload_type parameter defines the higher layer PDU type contained in the message
payload.
                          Size
         Field                         Type                      Description
                         octets
  Parameter Tag        2            Integer      payload_type
  Length               2            Integer      Length of Value part in octets
  Value                1            Integer      0       Default. In the case of a WAP
                                                         application, the default higher layer
                                                         message type is a WDP message.
                                                         See [WDP] for details.
                                                 1       WCMP message. Wireless Control
                                                         Message Protocol formatted data.
                                                         See [WCMP] for details.
                                                 values 2 to 255 are reserved
}
  TPayloadType = packed record
    Tag:TPDUInteger16;//payload_type
    Length:TPDUInteger16;//Length of Value part in octets
    Value:TPDUInteger8;
  end;


{- additional_status_info_text
The additional_status_info_text parameter gives an ASCII textual description of the meaning
of a response PDU. It is to be used by an implementation to allow easy diagnosis of problems.
                          Size
         Field                         Type                      Description
                         octets
 Parameter Tag         2            Integer      additional_status_info_text
 Length                2            Integer      Length of Value part in octets
 Value                 1 - 256      C Octet      Free format text to allow implementations
                                    String       to supply the most useful information for
                                                 problem diagnosis. Maximum length is 256
                                                 octets.
}
  TAdditionalStatusInfoText = packed record
    Tag:TPDUInteger16;//additional_status_info_text
    Length:TPDUInteger16;//Length of Value part in octets
    Value:array[1..256] of char;
  end;

{- receipted_message_id
The receipted_message_id parameter indicates the ID of the message being receipted in an
SMSC Delivery Receipt. This is the opaque SMSC message identifier that was returned in the
message_id parameter of the SMPP response PDU that acknowledged the submission of the
original message.
                          Size
         Field                         Type                      Description
                         octets
 Parameter Tag         2            Integer      receipted_message_id
                       2            Integer      Length of Value part in octets
 Length
 Value                 1 - 65       C Octet      SMSC handle of the message being
                                    String       receipted.
}

  TReceiptedMessageID = packed record
    Tag:TPDUInteger16;//receipted_message_id
    Length:TPDUInteger16;//Length of Value part in octets
    Value:array[1..65] of char;
  end;

{- ms_msg_wait_facilities
The ms_msg_wait_facilities parameter allows an indication to be provided to an MS that there
are messages waiting for the subscriber on systems on the PLMN. The indication can be an icon
on the MS screen or other MMI indication.
The ms_msg_wait_facilities can also specify the type of message associated with the message
waiting indication.
                          Size
        Field                          Type                      Description
                        octets
 Parameter Tag        2            Integer       ms_msg_wait_facilities
 Length               2            Integer       Length of Value part in octets
 Value                1            Bit mask                  Bits 7............0
                                                                  I00000TT
                                                 This parameter controls the indication and
                                                 specifies the message type (of the message
                                                 associated with the MWI) at the mobile
                                                 station.
                                                 The Indicator is encoded in bit 7 as
                                                 follows:
                                                     0 = Set Indication Inactive
                                                     1 = Set Indication Active
                                                 The Type of Message associated with the
                                                 MWI is encoded in bits 0 and 1 as follows:
                                                     00 = Voicemail Message Waiting
                                                     01 = Fax Message Waiting
                                                     10 = Electronic Mail Message Waiting
                                                     11 = Other Message Waiting
}
  TMSMsgWaitFacilities = packed record
    Tag:TPDUInteger16;//ms_msg_wait_facilities
    Length:TPDUInteger16;//Length of Value part in octets
    Value:TPDUInteger8;
  end;

{- privacy_indicator
The privacy_indicator indicates the privacy level of the message.
                         Size
        Field                         Type                       Description
                        octets
 Parameter Tag        2            Integer        privacy_indicator
 Length               2            Integer       Length of value part in octets
 Value                1            Integer       0 = Privacy Level 0 (Not Restricted)
                                                      (default)
                                                 1 = Privacy Level 1 (Restricted)
                                                 2 = Privacy Level 2 (Confidential)
                                                 3 = Privacy Level 3 (Secret)
                                                 values 4 to 255 are reserved
}
  TPrivacyIndicator = packed record
    Tag:TPDUInteger16;//privacy_indicator
    Length:TPDUInteger16;//Length of Value part in octets
    Value:TPDUInteger8;
  end;

{- source_subaddress
The source_subaddress parameter specifies a subaddress associated with the originator of the
message.
                       Size
        Field                      Type                         Description
                      octets
 Parameter Tag      2           Integer
 Length             2           Integer        Length of Value part in octets
 Value              Var         Octet          The first octet of the data field is a Type of
                    2 - 23      String         Subaddress tag and indicates the type of
                                               subaddressing information included, and
                                               implies the type and length of
                                               subaddressing information which can
                                               accompany this tag value in the data field.
                                               Valid Tag values are:
                                               00000001 - Reserved
                                               00000010 - Reserved
                                               10000000 - NSAP (Even) [ITUT X.213]
                                               10001000 - NSAP (Odd) [ITUT X.213]
                                               10100000 - User Specified
                                               All other values Reserved
                                                     The remaining octets contain the
                                                                subaddress.
                                                 A NSAP address shall be encoded using
                                                the preferred binary encoding specified in
                                                [ITUT X.213]. In this case the subaddress
                                                 field contains the Authority and Format
                                                                 Identifier.
                                                 A User Specified subaddress is encoded
                                               according to user specification, subject to a
                                                          maximum of 22 octets.
}
  TSourceSubaddress = packed record
    Tag:TPDUInteger16;//source_subaddress
    Length:TPDUInteger16;//Length of Value part in octets
    Value:array[1..23] of char;
  end;

{- dest_subaddress
The dest_subaddress parameter specifies a subaddress associated with the destination of the
message.
                         Size
        Field                         Type                      Description
                        octets
                      2            Integer       dest_subaddress
 Parameter Tag
 Length               2            Integer      Length of Value part in octets
 Value                Var          Octet        See 5.3.2.15 for parameter encoding.
                      2 - 23       String
}
  TDestSubaddress = packed record
    Tag:TPDUInteger16;//dest_subaddress
    Length:TPDUInteger16;//Length of Value part in octets
    Value:array[1..23] of char;
  end;

{- user_message_reference
A reference assigned by the originating SME to the short message.
                         Size
        Field                         Type                      Description
                        octets
 Parameter Tag        2            Integer       user_message_reference
 Length               2            Integer      Length of value part in octets
 Value                2            Integer      All values allowed.
}
  TUserMessageReference = packed record
    Tag:TPDUInteger16;//user_message_reference
    Length:TPDUInteger16;//Length of Value part in octets
    Value:TPDUInteger16;
  end;

{- user_response_code
A response code set by the user in a User Acknowledgement/Reply message. The response
codes are application specific.
                          Size
        Field                        Type                       Description
                         octets
 Parameter Tag         2          Integer        user_response_code
 Length                2          Integer       Length of value part in octets
 Value                 1          Integer       0 to 255 (IS-95 CDMA)
                                                0 to 15 (CMT-136 TDMA)
}

  TUserResponseCode = packed record
    Tag:TPDUInteger16;//user_response_code
    Length:TPDUInteger16;//Length of Value part in octets
    Value:TPDUInteger8;
  end;

{- language_indicator
The language_indicator parameter is used to indicate the language of the short message.
                          Size
        Field                        Type                       Description
                         octets
 Parameter Tag         2          Integer       language_indicator
 Length                2          Integer       Length of value part in octets
 Value                 1          Integer       0 = unspecified (default)
                                                1 = english
                                                2 = french
                                                3 = spanish
                                                4 = german
                                                5 = Portuguese
                                                refer to [CMT-136] for other values
}

  TLanguageIndicator = packed record
    Tag:TPDUInteger16;//language_indicator
    Length:TPDUInteger16;//Length of Value part in octets
    Value:TPDUInteger8;
  end;

{5.3.2.20          source_port
The source_port parameter is used to indicate the application port number associated with the
source address of the message.
                          Size
         Field                        Type                        Description
                         octets
  Parameter Tag        2           Integer        source_port
  Length               2           Integer       Length of value part in octets
                                                 All values allowed.
  Value                2           Integer
}

  TSourcePort = packed record
    Tag:TPDUInteger16;//source_port
    Length:TPDUInteger16;//Length of Value part in octets
    Value:TPDUInteger16;
  end;

{5.3.2.21          destination_port
The destination_port parameter is used to indicate the application port number associated with
the destination address of the message.
                          Size
         Field                        Type                        Description
                         octets
  Parameter Tag        2           Integer        destination_port
  Length               2           Integer       Length of value part in octets
  Value                2           Integer       All values allowed.
}
  TDestinationPort = packed record
    Tag:TPDUInteger16;//destination_port
    Length:TPDUInteger16;//Length of Value part in octets
    Value:TPDUInteger16;
  end;

{- sar_msg_ref_num
The sar_msg_ref_num parameter is used to indicate the reference number for a particular
concatenated short message.
                         Size
        Field                       Type                     Description
                        octets
 Parameter Tag        2          Integer      sar_msg_ref_num
 Length               2          Integer     Length of value part in octets
 Value                2          Integer     This parameter shall contain a originator
                                             generated reference number so that a
                                             segmented short message may be
                                             reassembled into a single original message.
                                             This allows the parallel transmission of
                                             several segmented messages. This
                                             reference number shall remain constant for
                                             every segment which makes up a particular
                                             concatenated short message.
                                             When present, the PDU must also contain
                                             the sar_total_segments and
                                             sar_segment_seqnum parameters.
                                             Otherwise this parameter shall be ignored.

}
  TSarMsgRefNum = packed record
    Tag:TPDUInteger16;//sar_msg_ref_num
    Length:TPDUInteger16;//Length of Value part in octets
    Value:TPDUInteger16;
  end;


{5.3.2.23          sar_total_segments
The sar_total_segments parameter is used to indicate the total number of short messages
within the concatenated short message.
                         Size
         Field                       Type                         Description
                        octets
  Parameter Tag       2           Integer        sar_total_segments
  Length              2           Integer       Length of value part in octets
  Value               1           Integer       This parameter shall contain a value in the
                                                range 1 to 255 indicating the total number
                                                of fragments within the concatenated short
                                                message. The value shall start at 1 and
                                                remain constant for every short message
                                                which makes up the concatenated short
                                                message.
                                                When present, the PDU must also contain
                                                the sar_msg_ref_num and
                                                sar_segment_seqnum parameters.
                                                Otherwise this parameter shall be ignored.
}
  TSarTotalSegments = packed record
    Tag:TPDUInteger16;//sar_total_segments
    Length:TPDUInteger16;//Length of Value part in octets
    Value:TPDUInteger8;
  end;

{5.3.2.24          sar_segment_seqnum
The sar_segment_seqnum parameter is used to indicate the sequence number of a particular
short message within the concatenated short message.
                         Size
         Field                       Type                         Description
                        octets
  Parameter Tag       2           Integer        sar_segment_seqnum
  Length              2           Integer       Length of value part in octets
  Value               1           Integer       This octet shall contain a value in the range
                                                1 to 255 indicating the sequence number of
                                                a particular message within the
                                                concatenated short message. The value
                                                shall start at 1 and increment by one for
                                                every message sent within the concatenated
                                                short message.
                                                When present, the PDU must also contain
                                                the sar_total_segments and
                                                sar_msg_ref_num parameters. Otherwise
                                                this parameter shall be ignored.
}
  TSarSegmentSeqnum = packed record
    Tag:TPDUInteger16;//sar_segment_seqnum
    Length:TPDUInteger16;//Length of Value part in octets
    Value:TPDUInteger8;
  end;

{5.3.2.25          sc_interface_version
The sc_interface_version parameter is used to indicate the SMPP version supported by the
SMSC. It is returned in the bind response PDUs.
                          Size
        Field                          Type                       Description
                         octets
 Parameter Tag         2            Integer        sc_interface_version
 Length                2            Integer       Length of value part in octets
 Value                 1            Integer       values as per 5.2.4. (interface_version)
}
  TScInterfaceVersion = packed record
    Tag:TPDUInteger16;//sc_interface_version
    Length:TPDUInteger16;//Length of Value part in octets
    Value:TPDUInteger8;
  end;

{5.3.2.26          display_time
The display_time parameter is used to associate a display time of the short message on the MS.
                          Size
        Field                          Type                       Description
                         octets
 Parameter Tag         2            Integer        display_time
 Length                2            Integer       Length of value part in octets
 Value                 1            Integer       0 = Temporary
                                                  1 = Default (default)
                                                  2 = Invoke
                                                  values 3 to 255 are reserved
}
  TDisplayTime = packed record
    Tag:TPDUInteger16;//display_time
    Length:TPDUInteger16;//Length of Value part in octets
    Value:TPDUInteger8;
  end;

{5.3.2.27           ms_validity
The ms_validity parameter is used to provide an MS with validity information associated with
the received short message.
                          Size
         Field                          Type                        Description
                         octets
  Parameter Tag        2             Integer         ms_validity
  Length               2             Integer        Length of value part in octets
  Value                1             Integer        0 = Store Indefinitely (default)
                                                    1 = Power Down
                                                    2 = SID based registration area
                                                    3 = Display Only
                                                    values 4 to 255 are reserved
}
  TMSValidity = packed record
    Tag:TPDUInteger16;//ms_validity
    Length:TPDUInteger16;//Length of Value part in octets
    Value:TPDUInteger8;
  end;

{5.3.2.28           dpf_result
The dpf_result parameter is used in the data_sm_resp PDU to indicate if delivery pending flag
(DPF) was set for a delivery failure of the short message..
If the dpf_result parameter is not included in the data_sm_resp PDU, the ESME should assume
that DPF is not set.
Currently this parameter is only applicable for the Transaction message mode.
                          Size
         Field                          Type                        Description
                         octets
  Parameter Tag        2             Integer         dpf_result
  Length               2             Integer        Length of value part in octets
  Value                1             Integer        0 = DPF not set
                                                    1 = DPF set
                                                    values 2 to 255 are reserved
}
  TDPFResult = packed record
    Tag:TPDUInteger16;//dpf_result
    Length:TPDUInteger16;//Length of Value part in octets
    Value:TPDUInteger8;
  end;

{5.3.2.29           set_dpf
An ESME may use the set_dpf parameter to request the setting of a delivery pending flag (DPF)
for certain delivery failure scenarios, such as
         - MS is unavailable for message delivery (as indicated by the HLR)
The SMSC should respond to such a request with an alert_notification PDU when it detects
that the destination MS has become available.
The delivery failure scenarios under which DPF is set is SMSC implementation and network
implementation specific. If a delivery pending flag is set by the SMSC or network (e.g. HLR),
then the SMSC should indicate this to the ESME in the data_sm_resp message via the
dpf_result parameter.
                           Size
         Field                     Type                         Description
                         octets
  Parameter Tag          2        Integer     set_dpf
  Length                 2        Integer    length of value part in octets
  Value                  1        Integer    0 = Setting of DPF for delivery failure to MS not
                                                   requested
                                             1 = Setting of DPF for delivery failure requested
                                                   (default)
                                             values 2 to 255 are reserved
}
  TSetDPF = packed record
    Tag:TPDUInteger16;//set_dpf
    Length:TPDUInteger16;//Length of Value part in octets
    Value:TPDUInteger8;
  end;

{5.3.2.30            ms_availability_status
The ms_availability_status parameter is used in the alert_notification operation to indicate the
availability state of the MS to the ESME.
If the SMSC does not include the parameter in the alert_notification operation, the ESME
should assume that the MS is in an “available” state.
                            Size
         Field                          Type                       Description
                           octets
  Parameter Tag          2           Integer       ms_availability_status
  Length                 2           Integer      Length of value part in octets
  Value                  1           Integer      0 = Available (Default)
                                                  1 = Denied (e.g. suspended, no SMS
                                                      capability, etc.)
                                                  2 = Unavailable
                                                  values 3 to 255 are reserved
}
  TMSAvailabilityStatus = packed record
    Tag:TPDUInteger16;//ms_availability_status
    Length:TPDUInteger16;//Length of Value part in octets
    Value:TPDUInteger8;
  end;

{5.3.2.31           network_error_code
The network_error_code parameter is used to indicate the actual network error code for a
delivery failure. The network error code is technology specific.
                           Size
        Field                          Type                        Description
                          octets
 Parameter Tag          2           Integer        network_error_code
 Length                 2           Integer       Length of value part in octets
 Value                  3           Octet
                                                                    Size    Type
                                    String        Sub-field
                                                  Network Type 1             Integer
                                                  Error Code          2     Integer
                                                                         .
                                                    The first octet indicates the network type.
                                                       The following values are defined:
                                                               1 = ANSI-136
                                                               2 = IS-95
                                                               3 = GSM
                                                               4 = Reserved
                                                               All other values reserved.
                                                  The remaining two octets specify the actual
                                                  network error code appropriate to the
                                                  network type.
}
  TNetworkErrorCode = packed record
    Tag:TPDUInteger16;//network_error_code
    Length:TPDUInteger16;//Length of Value part in octets
    Value:record
            NetworkType:TPDUInteger8;
            ErrorCode:TPDUInteger16;
          end ;
  end;

{5.3.2.32          message_payload
The message_payload parameter contains the user data.
                         Size
        Field                        Type                        Description
                        octets
 Parameter Tag        2          Integer         message_payload
 Length               2          Integer         Set to length of user data
 Value                Variable   Octet           Short message user data. The maximum
                                 String          size is SMSC and network implementation
                                                 specific.
}
  TMessagePayload = packed record
    Tag:TPDUInteger16;//message_payload
    Length:TPDUInteger16;//Length of Value part in octets
    Value:array [0..0] of char;
  end;

{5.3.2.33          delivery_failure_reason
The delivery_failure_reason parameter is used in the data_sm_resp operation to indicate the
outcome of the message delivery attempt (only applicable for transaction message mode). If a
delivery failure due to a network error is indicated, the ESME may check the
network_error_code parameter (if present) for the actual network error code.
The delivery_failure_reason parameter is not included if the delivery attempt was successful.
                         Size
        Field                        Type                        Description
                        octets
 Parameter Tag        2          Integer          delivery_failure_reason
 Length               2          Integer         Length of value part in octets
 Value                1          Integer         0 = Destination unavailable
                                                 1 = Destination Address Invalid (e.g.
                                                     suspended, no SMS capability, etc.)
                                                 2 = Permanent network error
                                                 3 = Temporary network error
                                                 values 4 to are 255 reserved

}
  TDeliveryFailureReason = packed record
    Tag:TPDUInteger16;//delivery_failure_reason
    Length:TPDUInteger16;//Length of Value part in octets
    Value:TPDUInteger8;
  end;

{5.3.2.34          more_messages_to_send
The more_messages_to_send parameter is used by the ESME in the submit_sm and data_sm
operations to indicate to the SMSC that there are further messages for the same destination
SME. The SMSC may use this setting for network resource optimization.
                          Size
        Field                        Type                       Description
                         octets
 Parameter Tag         2          Integer        more_messages_to_send
 Length                2          Integer       Length of value part in octets
 Value                 1                        0 = No more messages to follow
                                                1 = More messages to follow (default)
                                                values 2 to 255 are reserved
}
  TMoreMessagesToSend = packed record
    Tag:TPDUInteger16;//more_messages_to_send
    Length:TPDUInteger16;//Length of Value part in octets
    Value:TPDUInteger8;
  end;

{5.3.2.35          message_state
The message_state optional parameter is used by the SMSC in the deliver_sm and data_sm
PDUs to indicate to the ESME the final message state for an SMSC Delivery Receipt.
                          Size
        Field                        Type                       Description
                         octets
 Parameter Tag         2          Integer        message_state
                       2          Integer       Length of value part in octets
 Length
 Value                 1                        Values as per section 5.2.28
}
  TMessageState = packed record
    Tag:TPDUInteger16;//message_state
    Length:TPDUInteger16;//Length of Value part in octets
    Value:TPDUInteger8;
  end;

{5.3.2.36           callback_num
The callback_num parameter associates a call back number with the message. In TDMA
networks, it is possible to send and receive multiple callback numbers to/from TDMA mobile
stations.
                           Size
         Field                          Type                        Description
                          octets
  Parameter Tag         2           Integer
  Length                2           Integer       Length of Value part in octets
  Value                 Var         Octet                 Bits 7.............0
                        4 - 19      String                     0000000D (octet 1)
                                                               00000TTT (octet 2)
                                                              0000NNNN (octet 3)
                                                             XXXXXXXX (octet 4)
                                                                             :
                                                                             :
                                                             XXXXXXXX (octet N)
                                                    The originating SME can set a Call Back
                                                   Number for the receiving Mobile Station.
                                                     The first octet contains the Digit Mode
                                                                      Indicator.
                                                      Bit D=0 indicates that the Call Back
                                                     Number is sent to the mobile as DTMF
                                                  digits encoded in TBCD. Bit D=1 indicates
                                                    that the Call Back Number is sent to the
                                                        mobile encoded as ASCII digits.
                                                  The 2nd octet contains the Type of Number
                                                      (TON). Encoded as in section 5.2.5.
                                                     The third octet contains the Numbering
                                                   Plan Indicator (NPI). Encoded as specified
                                                                  in section 5.2.6
                                                  The remaining octets contain the Call Back
                                                        Number digits encoded as ASCII
                                                                      characters
}
  TCallbackNum = packed record
    Tag:TPDUInteger16;//callback_num
    Length:TPDUInteger16;//Length of Value part in octets
    Value:array [1..19] of char;
  end;

{5.3.2.37        callback_num_pres_ind
                       Size
        Field                     Type                      Description
                      octets
 Parameter Tag      2          Integer      callback_num_pres_ind
 Length             2          Integer     Length of Value part in octets
 Value              1          Bit mask                Bits 7............0
                                                             0000ppss
                                           This parameter controls the presentation
                                           indication and screening of the
                                           CallBackNumber at the mobile station.If
                                           present, the callback_num parameter must
                                           also be present.
                                           The Presentation Indicator is encoded in
                                           bits 2 and 3 as follows:
                                               00 = Presentation Allowed
                                               01 = Presentation Restricted
                                               10 = Number Not Available
                                               11 = Reserved
                                           The Screening Indicator is encoded in bits
                                           0 and 1 as follows:
                                               00 = User provided, not screened
                                               01 = User provided, verified and passed
                                               10 = User provided, verified and failed
                                               11 = Network Provided.
}
  TCallbackNumPresInd = packed record
    Tag:TPDUInteger16;//callback_num_pres_ind
    Length:TPDUInteger16;//Length of Value part in octets
    Value:TPDUInteger8;
  end;

{5.3.2.38       callback_num_atag
The callback_num_atag parameter associates an alphanumeric display with the call back
number.
                       Size
        Field                       Type                      Description
                      octets
 Parameter Tag      2           Integer      callback_num_atag
 Length             2           Integer     Length of Value part in octets
 Value              Var         Octet        Alphanumeric display tag for call back
                    max         string      number
                    65                            Bits 7...............0
                                                        EEEEEEEE (octet 1)
                                                       XXXXXXXX (octet 2)
                                                                       :
                                                                       :
                                                      XXXXXXXX (octet N)
                                            The first octet contains the encoding
                                            scheme of the Alpha Tag display
                                            characters. This field contains the same
                                            values as for Data Coding Scheme (see
                                            section 5.2.19).
                                            The following octets contain the display
                                            characters:
                                            There is one octet per display character for
                                            7-bit and 8-bit encoding schemes.
                                            There are two octets per display character
                                            for 16-bit encoding schemes.
}
  TCallbackNumAtag = packed record
    Tag:TPDUInteger16;//callback_num_atag
    Length:TPDUInteger16;//Length of Value part in octets
    Value:array [1..65] of char;
  end;

{5.3.2.39          number_of_messages
The number_of_messages parameter is used to indicate the number of messages stored in a
mailbox.
                          Size
        Field                         Type                   Description
                         octets
 Parameter Tag         2           Integer     number_of_messages
 Length                2           Integer    Length of Value part in octets
 Value                 1           Integer    0 to 99 = allowed values.
                                              values 100 to 255 are reserved
}
  TNumberOfMessages = packed record
    Tag:TPDUInteger16;//number_of_messages
    Length:TPDUInteger16;//Length of Value part in octets
    Value:TPDUInteger8;
  end;

{5.3.2.40          sms_signal
The sms_signal parameter is used to provide a TDMA MS with alert tone information
associated with the received short message.
                          Size
        Field                         Type                   Description
                         octets
 Parameter Tag         2           Integer     sms_signal
 Length                2           Integer    Length of Value part in octets
 Value                 2           Integer    Encoded as per [CMT-136]
}
  TSMSSignal = packed record
    Tag:TPDUInteger16;//sms_signal
    Length:TPDUInteger16;//Length of Value part in octets
    Value:TPDUInteger16;
  end;

{5.3.2.41          alert_on_message_delivery
The alert_on_message_delivery parameter is set to instruct a MS to alert the user (in a MS
implementation specific manner) when the short message arrives at the MS.
                          Size
         Field                         Type                      Description
                         octets
  Parameter Tag        2            Integer       alert_on_message_delivery
  Length               2            Integer      Length of Value part in octets (= 0)
  Value                0                         There is no Value part associated with this
                                                 parameter.
}
  TAlertOnMessageDelivery = packed record
    Tag:TPDUInteger16;//alert_on_message_delivery
    Length:TPDUInteger16;//Length of Value part in octets
  end;


{5.3.2.42          its_reply_type
The its_reply_type parameter is a required parameter for the CDMA Interactive Teleservice as
defined by the Korean PCS carriers [KORITS]. It indicates and controls the MS user’s reply
method to an SMS delivery message received from the ESME.
                          Size
         Field                         Type                      Description
                         octets
  Parameter Tag        2            Integer       its_reply_type
  Length               2            Integer      Length of Value part in octets
  Value                1            Integer      0 = Digit
                                                 1 = Number
                                                 2 = Telephone No.
                                                 3 = Password
                                                 4 = Character Line
                                                 5 = Menu
                                                 6 = Date
                                                 7 = Time
                                                 8 = Continue
                                                 values 9 to 255 are reserved
}
  TItsReplyType = packed record
    Tag:TPDUInteger16;//its_reply_type
    Length:TPDUInteger16;//Length of Value part in octets
    Value:TPDUInteger8;
  end;

{5.3.2.43           its_session_info
The its_session_info parameter is a required parameter for the CDMA Interactive Teleservice
as defined by the Korean PCS carriers [KORITS]. It contains control information for the
interactive session between an MS and an ESME.
                          Size
         Field                        Type                          Description
                         octets
  Parameter Tag        2           Integer        its_session_info
  Length               2           Integer       Length of Value part in octets
  Value                2           Octet
                                   String               Bits 7...............0
                                                             SSSS SSSS (octet 1)
                                                            NNNN NNNE (octet 2)
                                                 Octet 1 contains the session number (0 -
                                                 255) encoded in binary. The session
                                                 number remains constant for each session.
                                                 The sequence number of the dialogue unit
                                                 (as assigned by the ESME) within the
                                                 session is encoded in bits 7..1 of octet 2.
                                                 The End of Session Indicator indicates the
                                                 message is the end of the conversation
                                                 session and is encoded in bit 0 of octet 2 as
                                                 follows:
                                                 0 = End of Session Indicator inactive.
                                                 1 = End of Session Indicator active.
}
  TItsSessionInfo = packed record
    Tag:TPDUInteger16;//its_session_info
    Length:TPDUInteger16;//Length of Value part in octets
    Value:TPDUInteger8;
  end;

{5.3.2.44          ussd_service_op
The ussd_service_op parameter is required to define the USSD service operation when SMPP
is being used as an interface to a (GSM) USSD system.
                           Size
        Field                           Type                    Description
                          octets
  Parameter Tag         2            Integer     ussd_service_op
  Length                2            Integer    Length of Value part in octets
  Value                 1            Octet      0 = PSSD indication
                                     String     1 = PSSR indication
                                                2 = USSR request
                                                3 = USSN request
                                                4 to 15 = reserved
                                                16 = PSSD response
                                                17 = PSSR response
                                                18 = USSR confirm
                                                19 = USSN confirm
                                                20 to 31 = reserved
                                                32 to 255 = reserved for vendor specific
                                                            USSD operations

}
  TUSSDServiceOp = packed record
    Tag:TPDUInteger16;//ussd_service_op
    Length:TPDUInteger16;//Length of Value part in octets
    Value:TPDUInteger8;
  end;

implementation

end.

