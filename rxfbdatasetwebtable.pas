unit RxFBDataSetWebTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, GraphType, fbcustomdataset,
  UIB, HTTPDefs;

type
  TRxFBDataSetWebTableOption =
    (
      rwtoUseRecNum,
      rwtoUseRowColor,
      rwtoUseRecCount
    );
  TRxFBDataSetWebTableOptions = set of TRxFBDataSetWebTableOption;

type
  TRxFBDataSetWebTable = class;
  TWTReplaceItem = class;

  TGetWTReplaceValueEvent = procedure(Sender:TWTReplaceItem; const ParamName:string;
      var ParamValue:string) of object;
  TGetQueryParamValueEvent = procedure(Sender:TRxFBDataSetWebTable; const ParamName:string;
      var ParamValue:string) of object;


  { TWTReplaceItem }

  TWTReplaceItem = class(TCollectionItem)
  private
    FName: string;
    FOnWTReplaceValue: TGetWTReplaceValueEvent;
    FValue: string;
    function GetValue: string;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignItem(AItem: TWTReplaceItem);
  published
    property Name: string read FName write FName;
    property Value: string read GetValue write FValue;
    property OnWTReplaceValue:TGetWTReplaceValueEvent read FOnWTReplaceValue write FOnWTReplaceValue;
  end;

  { TWTReplaceItems }

  TWTReplaceItems = class(TCollection)
  private
    function GetItem(Index: Integer): TWTReplaceItem;
    function GetItemValue(const ParamName: string): string;
    procedure SetItem(Index: Integer; const Value: TWTReplaceItem);
    procedure SetItemValue(const ParamName: string; const AValue: string);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; overload;
    procedure AssignValues(AValue: TWTReplaceItems);
    function ItemByName(const AValue: string): TWTReplaceItem;
    function FindItem(const AValue: string): TWTReplaceItem;
    function CreateItem(const ParamName: string): TWTReplaceItem;
    property Items[Index: Integer]: TWTReplaceItem read GetItem write SetItem; default;
    property ItemValue[const ParamName: string]: string read GetItemValue write SetItemValue;
//    procedure GetParamList(List: TList; const ParamNames: string);
  end;


  { TRxFBDataSetWebTable }

  TRxFBDataSetWebTable = class(TComponent)
  private
    FColor1: TGraphicsColor;
    FColor2: TGraphicsColor;
    FDataBase: TUIBDataBase;
    FOnGetQueryParamValue: TGetQueryParamValueEvent;
    FOptions: TRxFBDataSetWebTableOptions;
    FTemplateColor: string;
    FTemplateRow: string;
    FTransaction: TUIBTransaction;
    FReplaceItems: TWTReplaceItems;

    //function GetRequest: TRequest;
    function GetReplaceItems: TWTReplaceItems;
    procedure SetDataBase(const AValue: TUIBDataBase);
    procedure SetTransaction(const AValue: TUIBTransaction);
    procedure SetReplaceItems(const AValue: TWTReplaceItems);
    { Private declarations }
  protected
    procedure Loaded; override;
    procedure DoGetQueryParamValue(const ParamName:string;
      var ParamValue:string);virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetContent(const TagParams: TStringList; const Request:TRequest):string;
    //property Request:TRequest read GetRequest;
  published
    property DataBase:TUIBDataBase read FDataBase write SetDataBase;
    property Transaction:TUIBTransaction read FTransaction write SetTransaction;
    property Options:TRxFBDataSetWebTableOptions read FOptions write FOptions;
    property Color1:TGraphicsColor read FColor1 write FColor1;
    property Color2:TGraphicsColor read FColor2 write FColor2;
    property TemplateColor:string read FTemplateColor write FTemplateColor;
    property TemplateRow:string read FTemplateRow write FTemplateRow;
    property ReplaceItems:TWTReplaceItems read GetReplaceItems write SetReplaceItems;

    property OnGetQueryParamValue:TGetQueryParamValueEvent read FOnGetQueryParamValue
             write FOnGetQueryParamValue;
  end;

implementation
uses rxwStrTools, fpWeb, uiblib{, CustApp, custweb};

//type
//  THackWebApp = class(TCustomWebApplication);

{ TRxFBDataSetWebTable }

procedure TRxFBDataSetWebTable.SetDataBase(const AValue: TUIBDataBase);
begin
  if FDataBase=AValue then exit;
  FDataBase:=AValue;

  if not (csLoading in ComponentState) then
  begin
    if Assigned(FTransaction) and (FTransaction.DataBase<>AValue) then
      FTransaction:=nil;
  end;
end;
{
function TRxFBDataSetWebTable.GetRequest: TRequest;
begin
  Result:=THackWebApp(CustomApplication).WebHandler.; Application.Request; //TCustomFPWebModule(Owner).Request;
end;
}
function TRxFBDataSetWebTable.GetReplaceItems: TWTReplaceItems;
begin
  Result:=FReplaceItems;
end;

procedure TRxFBDataSetWebTable.SetTransaction(const AValue: TUIBTransaction);
begin
  if FTransaction=AValue then exit;
  FTransaction:=AValue;
end;

procedure TRxFBDataSetWebTable.SetReplaceItems(const AValue: TWTReplaceItems
  );
begin
  FReplaceItems.Assign(AValue)
end;

procedure TRxFBDataSetWebTable.Loaded;
begin
  inherited Loaded;
  if Assigned(FTransaction) and (FTransaction.DataBase<>FDataBase) then
    FTransaction:=nil;
end;

procedure TRxFBDataSetWebTable.DoGetQueryParamValue(const ParamName: string;
  var ParamValue: string);
begin
  if Assigned(FOnGetQueryParamValue) then
    FOnGetQueryParamValue(Self, ParamName, ParamValue);
end;

constructor TRxFBDataSetWebTable.Create(AOwner: TComponent);
begin
{  if not (Owner is TCustomFPWebModule) then
    raise Exception.Create('Owner need is TCustomFPWebModule');}
  inherited Create(AOwner);
  FReplaceItems:=TWTReplaceItems.Create;
  FOptions:=[ rwtoUseRecNum, rwtoUseRowColor, rwtoUseRecCount];
end;

destructor TRxFBDataSetWebTable.Destroy;
begin
  FreeAndNil(FReplaceItems);
  inherited Destroy;
end;

function TRxFBDataSetWebTable.GetContent(const TagParams: TStringList;
  const Request: TRequest): string;
var
  Q:TFBDataSet;
  StrRow,
  RowColor1,
  S, RowColor2, SPar:string;
  i:integer;

begin

  Result:='';

  RowColor1:=TagParams.Values['ROW_COLOR1'];
  RowColor2:=TagParams.Values['ROW_COLOR2'];

  Q:=TFBDataSet.Create(Self);
  Q.DataBase:=DataBase;
  Q.Transaction:=Transaction;
  try
    Q.SQLSelect.Text:=TagParams.Values['SQL'];
    //Q.QuerySelect.Prepare();
    for i:=0 to Q.Params.FieldCount - 1 do
    begin
      S:=FindNameInList(Request.QueryFields, Q.Params.FieldName[i]);
      if S = '' then
        S:=FindNameInList(Request.ContentFields, Q.Params.FieldName[i]);
      if S<>'' then
      begin
{        if Q.Params.FieldType[i] in [uftSmallint, uftInteger, uftQuad] then
        begin
          Result:='FieldType[i] in [uftSmallint, uftInteger, uftQuad]';}
          if StrToIntDef(S, -1)=-1 then
            Q.Params.IsNull[i]:=true
          else
          begin
            Q.Params.AsInteger[i]:=StrToInt(S);
//            Result:=Result + 'Q.Params.FieldName[i] = '+Q.Params.FieldName[i] + ' = '+IntToStr(Q.Params.AsInteger[i]);
          end
{        else
        begin
          Result:='!!!FieldType[i] not in [uftSmallint, uftInteger, uftQuad] ' + Q.Params.FieldName[i] + ' ' +IntToStr(Ord(Q.Params.FieldType[i]));
          Q.Params.AsString[i]:=S
        end;}
      end
      else
      begin
        SPar:='';
        DoGetQueryParamValue(UpperCase(Q.Params.FieldName[i]), SPar);
        if (SPar = '') and (not (Q.Params.FieldType[i] in [uftChar, uftVarchar, uftCstring, uftSmallint])) then
          Q.Params.IsNull[i]:=true
        else
          Q.Params.AsString[i]:=SPar;
      end;
    end;

    Q.Open;
    while not Q.EOF do
    begin
      StrRow:=TagParams.Values['ROW'];
      for i:=0 to Q.Fields.Count - 1 do
      begin
        if Q.Fields[i] is TFBAnsiMemoField then
          StrRow:=StringReplace(StrRow, '~'+Q.Fields[i].FieldName, TFBAnsiMemoField(Q.Fields[i]).AsString, [rfReplaceAll])
        else
          StrRow:=StringReplace(StrRow, '~'+Q.Fields[i].FieldName, Q.Fields[i].DisplayText, [rfReplaceAll]);
      end;

      if rwtoUseRowColor in FOptions then
      begin
        if Q.RecNo mod 2 = 0 then
          StrRow:=StringReplace(StrRow, '~ROW_COLOR', RowColor1, [])
        else
          StrRow:=StringReplace(StrRow, '~ROW_COLOR', RowColor2, []);
      end;

      if rwtoUseRecNum in FOptions then
        StrRow:=StringReplace(StrRow, '~#REC_NUM', IntToStr(Q.RecNo), [rfReplaceAll]);

      for i:=0 to FReplaceItems.Count - 1 do
        StrRow:=StringReplace(StrRow, FReplaceItems.Items[i].Name, FReplaceItems.Items[i].Value, [rfReplaceAll]);

      Result:=Result + StrRow;
      Q.Next;
    end;

    if TagParams.Values['F_ROW']<>'' then
      Result:=Result + TagParams.Values['F_ROW'];

    if rwtoUseRecCount in FOptions then
      Result:=StringReplace(Result, '~#REC_COUNT', IntToStr(Q.RecordCount), [rfReplaceAll]);

    Q.Close;
  finally
    Q.Free;
  end;

  if Result = '' then
  begin
    Result:=TagParams.Values['NOROW'];
    if Result = '' then
      Result:='&nbsp;'
    else
    for i:=0 to FReplaceItems.Count - 1 do
      Result:=StringReplace(Result, FReplaceItems.Items[i].Name, FReplaceItems.Items[i].Value, [rfReplaceAll]);

  end;
end;

{ TWTReplaceItem }

function TWTReplaceItem.GetValue: string;
begin
  Result:=FValue;
  if Assigned(FOnWTReplaceValue) then
    FOnWTReplaceValue(Self, FName, Result)
end;

function TWTReplaceItem.GetDisplayName: string;
begin
  Result:=FName + ' - ' + FValue;
end;

constructor TWTReplaceItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
end;

procedure TWTReplaceItem.Assign(Source: TPersistent);
begin
  if (Source is TWTReplaceItem) then AssignItem(TWTReplaceItem(Source))
  else
    inherited Assign(Source);
end;

procedure TWTReplaceItem.AssignItem(AItem: TWTReplaceItem);
begin
  FValue:=AItem.FValue;
  FName:=AItem.FName;
  FOnWTReplaceValue:=AItem.FOnWTReplaceValue;
end;

{ TWTReplaceItems }

function TWTReplaceItems.GetItem(Index: Integer): TWTReplaceItem;
begin
  Result := TWTReplaceItem(inherited Items[Index]);
end;

function TWTReplaceItems.GetItemValue(const ParamName: string): string;
begin
  Result := ItemByName(ParamName).Value
end;

procedure TWTReplaceItems.SetItem(Index: Integer; const Value: TWTReplaceItem);
begin
  inherited SetItem(Index, TCollectionItem(Value));
end;

procedure TWTReplaceItems.SetItemValue(const ParamName: string;
  const AValue: string);
begin
  ItemByName(ParamName).Value := AValue;
end;

procedure TWTReplaceItems.AssignTo(Dest: TPersistent);
begin
  if Dest is TWTReplaceItems then
    TWTReplaceItems(Dest).Assign(Self)
  else
      inherited AssignTo(Dest);
end;

constructor TWTReplaceItems.Create;
begin
  inherited CReate(TWTReplaceItem);
end;

procedure TWTReplaceItems.AssignValues(AValue: TWTReplaceItems);
var
  I: Integer;
  P: TWTReplaceItem;
begin
  for I := 0 to AValue.Count - 1 do
  begin
    P := TWTReplaceItem(AValue[I].Name);
    if P <> nil then
      P.Assign(AValue[I]);
  end;
end;

function TWTReplaceItems.ItemByName(const AValue: string): TWTReplaceItem;
begin
  Result := FindItem(AValue);
  if Result = nil then
    raise Exception.CreateFmt('ParameterNotFound - %s', [AValue]);
end;

function TWTReplaceItems.FindItem(const AValue: string): TWTReplaceItem;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := TWTReplaceItem(inherited Items[I]);
    if AnsiCompareText(Result.Name, AValue) = 0 then Exit;
  end;
  Result := nil;
end;

function TWTReplaceItems.CreateItem(const ParamName: string): TWTReplaceItem;
begin
  Result := Add as TWTReplaceItem;
  Result.Name := ParamName;
end;

end.
