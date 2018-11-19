{ SMSGsmInfo - интерфейс рассылки СМС с веб-сервисом http://gsm-inform.ru/api/

  © 2015 - Lagunov Aleksey - alexs.at.yandex.ru

  Данная библиотека является свободным программным обеспечением. Вы вправе
  распространять её и/или модифицировать в соответствии с условиями версии
  2 либо по вашему выбору с условиями более поздней версии Стандартной
  Общественной Лицензии Ограниченного Применения GNU, опубликованной Free
  Software Foundation.

  Мы распространяем эту библиотеку в надежде на то, что она будет вам
  полезной, однако НЕ ПРЕДОСТАВЛЯЕМ НА НЕЕ НИКАКИХ ГАРАНТИЙ, в том числе
  ГАРАНТИИ ТОВАРНОГО СОСТОЯНИЯ ПРИ ПРОДАЖЕ и ПРИГОДНОСТИ ДЛЯ ИСПОЛЬЗОВАНИЯ
  В КОНКРЕТНЫХ ЦЕЛЯХ. Для получения более подробной информации
  ознакомьтесь со Стандартной Общественной Лицензией Ограниченного
  Применений GNU.

  Вместе с данной библиотекой вы должны были получить экземпляр
  Стандартной Общественной Лицензии Ограниченного Применения GNU. Если вы
  его не получили, сообщите об этом в Free Software Foundation, Inc., 59 Temple Place — Suite 330, Boston,
  MA 02111-1307, USA.
}

unit SMSGsmInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs;

const
  sGsmInfoURL = 'http://gsm-inform.ru/api/';

type
  TTrafficType = (ttSimple, ttPriority); //тип трафика, simple - обычный, по умолчанию, priority - приоритетный
  TMegafonKind = (mkAsis, mkDel, mkQuality); //условия отправки на Мегафон и МТС,
                                      //asis - с цифровым отправителем, по умолчанию,
                                      //del - игнорировать, не отправлять смс абонентам Мегафона и МТС,
                                      //quality - в качестве отправителя ваш бренд, стоимость сообщения увеличивается согласно вашего тарифного плана


type
  TRxSMSGsmInfo = class;
  ERxSMSGsmInfoError = class(Exception);

  { TMessageItem }

  TMessageItem = class(TCollectionItem)
  private
    FPhone: string;
    FSendStatus: integer;
    FSmsID: integer;
  published
    property Phone:string read FPhone;
    property SendStatus:integer read FSendStatus;
    property SmsID:integer read FSmsID;
  end;

  { TMessagesList }

  TMessagesList = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TMessageItem;
  public
    function FindBySmsID(ASmsId:integer):TMessageItem;
    function FindBySmsPhone(APhone:string):TMessageItem;
    property Items[Index: Integer]: TMessageItem read GetItem; default;
  end;

  { TRxSMSGsmInfo }

  TRxSMSGsmInfo = class(TComponent)
  private
    FAPIKey: string;
    FBalance: Currency;
    FGsmInfoURL: string;
    FKindCurrency: string;
    FMegafonKind: TMegafonKind;
    FMessagesList: TMessagesList;
    FMsgSender: string;
    FProxyHost: string;
    FProxyPass: string;
    FProxyPort: string;
    FProxyUser: string;
    FSendNow: boolean;
    FTrafficType: TTrafficType;
    FUserID: string;
    FRequestInfo:TStrings;
    procedure CheckKeyInfo;
    function HttpRequest(ACmdName:string; AParams:string = ''):boolean;
  protected
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetBalance:boolean;
    function SendMessages(APhoneNumList, AMessage:string):boolean;
    function CheckStatus:boolean;

    property Balance:Currency read FBalance;
    property KindCurrency:string read FKindCurrency;
    property RequestInfo:TStrings read FRequestInfo;
    property MessagesList:TMessagesList read FMessagesList;
  published
    property GsmInfoURL:string read FGsmInfoURL write FGsmInfoURL;
    property APIKey:string read FAPIKey write FAPIKey;
    property UserID:string read FUserID write FUserID;
    property MsgSender:string read FMsgSender write FMsgSender;
    property TrafficType:TTrafficType read FTrafficType write FTrafficType default ttSimple;
    property SendNow:boolean read FSendNow write FSendNow default false;
    property MegafonKind:TMegafonKind read FMegafonKind write FMegafonKind default mkAsis;
    //
    property ProxyHost: string read FProxyHost Write FProxyHost;
    property ProxyPort: string read FProxyPort Write FProxyPort;
    property ProxyUser: string read FProxyUser Write FProxyUser;
    property ProxyPass: string read FProxyPass Write FProxyPass;
  end;

procedure Register;

implementation
uses fpjson, jsonparser, httpsend, SMSGsmInfoStr;

procedure Register;
begin
  {$I smsgsminfo_icon.lrs}
  RegisterComponents('RX Tools',[TRxSMSGsmInfo]);
end;

{ TMessagesList }

function TMessagesList.GetItem(Index: Integer): TMessageItem;
begin
  Result:=(Inherited Items[Index]) as TMessageItem;
end;

function TMessagesList.FindBySmsID(ASmsId: integer): TMessageItem;
var
  i: Integer;
begin
  Result:=nil;
  for i:=0 to Count-1 do
    if GetItem(i).FSmsID = ASmsId then
    begin
      Result:=GetItem(i);
      exit;
    end;
end;

function TMessagesList.FindBySmsPhone(APhone: string): TMessageItem;
var
  i: Integer;
begin
  Result:=nil;
  for i:=0 to Count-1 do
    if GetItem(i).FPhone = APhone then
    begin
      Result:=GetItem(i);
      exit;
    end;
end;

{ TRxSMSGsmInfo }

procedure TRxSMSGsmInfo.CheckKeyInfo;
begin
  if (FGsmInfoURL = '') or (FUserID = '') or (FAPIKey = '') then
    raise ERxSMSGsmInfoError.Create(sErrorAPIKeyUserIdNotSet);
end;

function TRxSMSGsmInfo.HttpRequest(ACmdName: string; AParams:string = ''): boolean;

function DoHttpGetText(const URL: string; const Response: TStrings): Boolean;
var
  HTTP: THTTPSend;
begin
  HTTP := THTTPSend.Create;
  HTTP.ProxyHost:=FProxyHost;
  HTTP.ProxyPort:=FProxyPort;
  HTTP.ProxyUser:=FProxyUser;
  HTTP.ProxyPass:=FProxyPass;
  try
    Result := HTTP.HTTPMethod('GET', URL);
    if Result then
      Response.LoadFromStream(HTTP.Document)
    else
      ;
  finally
    HTTP.Free;
  end;
end;

var
  sURL: String;

begin
  Result:=false;
  if (ACmdName <> 'balance') and (ACmdName <> 'status') and (ACmdName <> 'send') then
    raise ERxSMSGsmInfoError.CreateFmt(sErrorUnknowApiCommand, [ACmdName]);

  FRequestInfo.Clear;
  CheckKeyInfo;

  if (AParams <> '') and (AParams[1]<>'&') then
    AParams:='&'+AParams;

  sURL:='?api_key='+FAPIKey+'&id='+FUserID+'&cmd='+ACmdName+AParams;

  Result:=DoHttpGetText(FGsmInfoURL +  sURL, FRequestInfo);
  if not Result then
    raise ERxSMSGsmInfoError.Create(sErrorSiteRequest);
end;

constructor TRxSMSGsmInfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGsmInfoURL:=sGsmInfoURL;
  FRequestInfo:=TStringList.Create;
  FMessagesList:=TMessagesList.Create(Self, TMessageItem);
  FTrafficType:=ttSimple;
  FSendNow:=false;
  FMegafonKind:=mkAsis
end;

destructor TRxSMSGsmInfo.Destroy;
begin
  FMessagesList.Clear;
  FreeAndNil(FMessagesList);
  FreeAndNil(FRequestInfo);
  inherited Destroy;
end;

function TRxSMSGsmInfo.GetBalance: boolean;
var
  P: TJSONParser;
  J: TJSONData;
begin
  if not HttpRequest('balance') then exit;
  P:=TJSONParser.Create(FRequestInfo.Text);
  if Assigned(P) then
  begin
    J:=P.Parse;
    if Assigned(J) then
    begin
      FBalance:=J.FindPath('balance').AsFloat;
      FKindCurrency:=J.FindPath('currency').AsString;
      J.Free;
    end;
    P.Free;
  end;
end;

function TRxSMSGsmInfo.SendMessages(APhoneNumList, AMessage: string): boolean;
var
  P: TJSONParser;
  J: TJSONData;
  FItems: TJSONData;
  Msg: TMessageItem;
  i: Integer;
begin
  FMessagesList.Clear;
  AMessage:='&message=' + StringReplace(StringReplace(AMessage, '+', '%2B', [rfReplaceAll, rfIgnoreCase]), ' ', '+', [rfReplaceAll, rfIgnoreCase]);
  AMessage:='&to=' +APhoneNumList + AMessage;

  if FMsgSender<>'' then
    AMessage:='&sender=' + FMsgSender + AMessage;

  AMessage:='&send_now='+IntToStr(Ord(FSendNow))+AMessage;

  if FTrafficType = ttSimple then
    AMessage:='&traffic=simple'+AMessage
  else
    AMessage:='&traffic=priority'+AMessage;

  case FMegafonKind of
    mkAsis:AMessage:= '&megafon=asis'+AMessage;
    mkDel:AMessage:= '&megafon=del'+AMessage;
    mkQuality:AMessage:= '&megafon=quality'+AMessage;
  end;

  Result:=HttpRequest('send', AMessage);

  if Result then
  begin
    P:=TJSONParser.Create(FRequestInfo.Text);
    if Assigned(P) then
    begin
      J:=P.Parse;
      if Assigned(J) then
      begin
        FItems:=J.FindPath('items');
        if Assigned(FItems) then
          for i:=0 to FItems.Count-1 do
          begin
            Msg:=FMessagesList.Add as TMessageItem;
            Msg.FPhone:=FItems.Items[i].FindPath('phone').AsString;
            Msg.FSendStatus:=FItems.Items[i].FindPath('error_no').AsInteger;
            Msg.FSmsID:=FItems.Items[i].FindPath('sms_id').AsInteger;
          end;
        J.Free;
      end;
      P.Free;
    end;
  end;
end;

function TRxSMSGsmInfo.CheckStatus: boolean;
var
  i: Integer;
  P: TJSONParser;
  FSmsIDs: String;
  J: TJSONData;
  FItems: TJSONData;
  Msg: TMessageItem;
begin

  FSmsIDs:='';
  for i:=0 to FMessagesList.Count-1 do
  begin
    if FSmsIDs <> '' then
      FSmsIDs:=FSmsIDs + ',';
    FSmsIDs:= FSmsIDs + IntToStr(FMessagesList[i].FSmsID);
  end;

  Result:=HttpRequest('status', 'sms_id='+FSmsIDs);
  if Result then
  begin
    P:=TJSONParser.Create(FRequestInfo.Text);
    if Assigned(P) then
    begin
      J:=P.Parse;
      if Assigned(J) then
      begin
        FItems:=J.FindPath('items');
        if Assigned(FItems) then
          for i:=0 to FItems.Count-1 do
          begin
              Msg:=FMessagesList.FindBySmsID(FItems.Items[i].FindPath('id').AsInteger);
              Msg.FSendStatus:=FItems.Items[i].FindPath('error_no').AsInteger;
            end;
          J.Free;
        end;
        P.Free;
      end;
  end
  else
    raise ERxSMSGsmInfoError.Create(sErrorOnCheckStatus);
end;

end.
