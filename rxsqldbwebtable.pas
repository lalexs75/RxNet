unit RxSqlDBWebTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, GraphType, HTTPDefs, sqldb, db;

const
  sSumFunc = '~#SUM_';
  sCountFunc = '~#COUNT';

type
  TRxSqlDBWebTableOption =
    (
      rwtoUseRecNum,
      rwtoUseRowColor,
      rwtoUseRecCount
    );
  TRxSqlDBWebTableOptions = set of TRxSqlDBWebTableOption;

type
  TRxSqlDBWebTable = class;
  TWTReplaceItem = class;

  TGetWTReplaceValueEvent = procedure(Sender:TWTReplaceItem; const ParamName:string;
      var ParamValue:string) of object;
  TGetQueryParamValueEvent = procedure(Sender:TRxSqlDBWebTable; const AParam:TParam) of object;

  { TAgregateItem }

  TAgregateItem = class
    FName:string;
    FValue:Double;
    FField:TField;
    constructor Create(AName:string);
  end;

  { TAgregateItem }

  { TAgregateItems }

  TAgregateItems = class(TList)
  private
    function GetItem(Index: Integer): TAgregateItem;
    procedure PutItem(Index: Integer; AValue: TAgregateItem);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); virtual;
  public
    function FindByName(AName:string; AShowException:boolean = true):TAgregateItem;
    function AddItem(AName:string):TAgregateItem;
    property Items[Index: Integer]: TAgregateItem read GetItem write PutItem; default;
  end;


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


  { TRxSqlDBWebTable }

  TRxSqlDBWebTable = class(TComponent)
  private
    FColor1: TGraphicsColor;
    FColor2: TGraphicsColor;
    FDataBase: TSQLConnection;
    FOnGetQueryParamValue: TGetQueryParamValueEvent;
    FOptions: TRxSqlDBWebTableOptions;
    FTemplateColor: string;
    FTemplateRow: string;
    FTransaction: TSQLTransaction;
    FReplaceItems: TWTReplaceItems;

    FAggList:TAgregateItems;
    FFooterText:string;
    FGroupText: String;
    //function GetRequest: TRequest;
    //
    function GetReplaceItems: TWTReplaceItems;
    procedure SetDataBase(const AValue: TSQLConnection);
    procedure SetTransaction(const AValue: TSQLTransaction);
    procedure SetReplaceItems(const AValue: TWTReplaceItems);
    procedure PrepareAggregateList(AFooterText:string);
    procedure PrepareAggregateGroupList(AGroupText:string);
    procedure BindAggFields(ADataSet:TDataSet);
    function GetFooter(ADataSet:TDataSet):string;
    procedure UpdAggFields;
    procedure ParseGroupCondition(AFooterText:string);
  protected
    procedure Loaded; override;
    procedure DoGetQueryParamValue(const Param: TParam); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetContent(const TagParams: TStringList; const Request:TRequest):string;
  published
    property DataBase:TSQLConnection read FDataBase write SetDataBase;
    property Transaction:TSQLTransaction read FTransaction write SetTransaction;
    property Options:TRxSqlDBWebTableOptions read FOptions write FOptions;
    property Color1:TGraphicsColor read FColor1 write FColor1;
    property Color2:TGraphicsColor read FColor2 write FColor2;
    property TemplateColor:string read FTemplateColor write FTemplateColor;
    property TemplateRow:string read FTemplateRow write FTemplateRow;
    property ReplaceItems:TWTReplaceItems read GetReplaceItems write SetReplaceItems;

    property OnGetQueryParamValue:TGetQueryParamValueEvent read FOnGetQueryParamValue
             write FOnGetQueryParamValue;
  end;

implementation
uses rxwStrTools, fpWeb, strutils;

{ TAgregateItem }

constructor TAgregateItem.Create(AName: string);
begin
  inherited Create;
  FName:=AName;
  FValue:=0;
end;

{ TAgregateItems }

function TAgregateItems.GetItem(Index: Integer): TAgregateItem;
begin
  Result:=TAgregateItem(Get(Index));
end;

procedure TAgregateItems.PutItem(Index: Integer; AValue: TAgregateItem);
begin
  Put(Index, AValue);
end;

procedure TAgregateItems.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Assigned(Ptr) then
    TAgregateItem(Ptr).Free;
end;

function TAgregateItems.FindByName(AName: string; AShowException: boolean
  ): TAgregateItem;
var
  i:integer;
begin
  AName:=UpperCase(AName);
  Result:=nil;
  for I:=0 to Count - 1 do
  begin
    if UpperCase(Items[i].FName) = AName then
    begin
      Result:=Items[i];
    end;
  end;
  if AShowException then
    raise Exception.CreateFmt('Переменная %s не найдена.', [AName]);
end;

function TAgregateItems.AddItem(AName: string): TAgregateItem;
begin
  Result:=FindByName(AName, false);
  if Assigned(Result) then exit;
  Result:=TAgregateItem.Create(AName);
  Add(Result);
end;

//type
//  THackWebApp = class(TCustomWebApplication);

{ TRxSqlDBWebTable }

procedure TRxSqlDBWebTable.SetDataBase(const AValue: TSQLConnection);
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
function TRxSqlDBWebTable.GetRequest: TRequest;
begin
  Result:=THackWebApp(CustomApplication).WebHandler.; Application.Request; //TCustomFPWebModule(Owner).Request;
end;
}
function TRxSqlDBWebTable.GetReplaceItems: TWTReplaceItems;
begin
  Result:=FReplaceItems;
end;

procedure TRxSqlDBWebTable.SetTransaction(const AValue: TSQLTransaction);
begin
  if FTransaction=AValue then exit;
  FTransaction:=AValue;
end;

procedure TRxSqlDBWebTable.SetReplaceItems(const AValue: TWTReplaceItems
  );
begin
  FReplaceItems.Assign(AValue)
end;

procedure TRxSqlDBWebTable.PrepareAggregateList(AFooterText: string);
var
  S, S1:string;
  k:integer;
begin
  FAggList.Clear;
  FFooterText:=AFooterText;
  if AFooterText = '' then exit;
  S:=FFooterText;
  while S<>'' do
  begin
    K:=Pos('~#SUM_', S);
    if K>0 then
    begin
      Delete(S, 1, K + 5);
      S1:=Copy2Space(S);
      if S1<>'' then
        FAggList.AddItem(S1);
    end
    else
      S:='';
  end;
end;

procedure TRxSqlDBWebTable.PrepareAggregateGroupList(AGroupText: string);
begin
//  FAggList.Clear;
  FGroupText:=AGroupText;
{  if AFooterText = '' then exit;
  S:=FFooterText;
  while S<>'' do
  begin
    K:=Pos('~#SUM_', S);
    if K>0 then
    begin
      Delete(S, 1, K + 5);
      S1:=Copy2Space(S);
      if S1<>'' then
        FAggList.AddItem(S1);
    end
    else
      S:='';
  end;}
end;


procedure TRxSqlDBWebTable.UpdAggFields;
var
  i: Integer;
begin
  for i:=0 to FAggList.Count-1 do
    if Assigned(FAggList[i].FField) then
      FAggList[i].FValue:=FAggList[i].FValue + FAggList[i].FField.AsFloat;
end;

procedure TRxSqlDBWebTable.ParseGroupCondition(AFooterText: string);
begin

end;

procedure TRxSqlDBWebTable.BindAggFields(ADataSet: TDataSet);
var
  i: Integer;
begin
  for i:=0 to FAggList.Count-1 do
    FAggList[i].FField:=ADataSet.FindField(FAggList[i].FName);
end;

procedure TRxSqlDBWebTable.Loaded;
begin
  inherited Loaded;
  if Assigned(FTransaction) and (FTransaction.DataBase<>FDataBase) then
    FTransaction:=nil;
end;

procedure TRxSqlDBWebTable.DoGetQueryParamValue(const Param:TParam);
begin
  if Assigned(FOnGetQueryParamValue) then
    FOnGetQueryParamValue(Self, Param);
end;

constructor TRxSqlDBWebTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAggList:=TAgregateItems.Create;
  FReplaceItems:=TWTReplaceItems.Create;
  FOptions:=[ rwtoUseRecNum, rwtoUseRowColor, rwtoUseRecCount];
end;

destructor TRxSqlDBWebTable.Destroy;
begin
  FreeAndNil(FReplaceItems);
  FAggList.Clear;
  FreeAndNil(FAggList);
  inherited Destroy;
end;

function TRxSqlDBWebTable.GetContent(const TagParams: TStringList;
  const Request: TRequest): string;
var
  Q:TSQLQuery;
  StrRow,
  RowColor1,
  S, RowColor2:string;
  i:integer;
  F:TField;
begin

  Result:='';

  RowColor1:=TagParams.Values['ROW_COLOR1'];
  RowColor2:=TagParams.Values['ROW_COLOR2'];

  PrepareAggregateList(TagParams.Values['FOOTER']);
  PrepareAggregateGroupList(TagParams.Values['GROUP_FOOTER']);

  Q:=TSQLQuery.Create(Self);
  Q.DataBase:=DataBase;
  Q.Transaction:=Transaction;
  try
    Q.SQL.Text:=TagParams.Values['SQL'];
    //Q.QuerySelect.Prepare();
    for i:=0 to Q.Params.Count - 1 do
    begin
      S:=FindNameInList(Request.QueryFields, Q.Params[i].Name);
      if S = '' then
        S:=FindNameInList(Request.ContentFields, Q.Params[i].Name);
      if S<>'' then
      begin
          if StrToIntDef(S, -1)=-1 then
            Q.Params[i].Clear
          else
          begin
            Q.Params[i].AsInteger:=StrToInt(S);
          end
      end
      else
        DoGetQueryParamValue(Q.Params[i]);
    end;

    Q.Open;
    BindAggFields(Q);
    while not Q.EOF do
    begin
      StrRow:=TagParams.Values['ROW'];
      for i:=0 to Q.Fields.Count - 1 do
      begin
{        if Q.Fields[i] is TFBAnsiMemoField then
          StrRow:=StringReplace(StrRow, '~'+Q.Fields[i].FieldName, TFBAnsiMemoField(Q.Fields[i]).AsString, [rfReplaceAll])
        else}
          StrRow:=StringReplace(StrRow, '~'+Q.Fields[i].FieldName, HTMLEscapeChars(Q.Fields[i].DisplayText), [rfReplaceAll, rfIgnoreCase]);
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

      UpdAggFields;

      Q.Next;
    end;

    if TagParams.Values['F_ROW']<>'' then
      Result:=Result + TagParams.Values['F_ROW'];

    if rwtoUseRecCount in FOptions then
      Result:=StringReplace(Result, '~#REC_COUNT', IntToStr(Q.RecordCount), [rfReplaceAll]);


    if Result = '' then
    begin
      Result:=TagParams.Values['NOROW'];
      if Result = '' then
        Result:='&nbsp;'
      else
      for i:=0 to FReplaceItems.Count - 1 do
        Result:=StringReplace(Result, FReplaceItems.Items[i].Name, FReplaceItems.Items[i].Value, [rfReplaceAll]);

    end;

    if FFooterText<>'' then
      Result:=Result + GetFooter(Q);

    Q.Close;
  finally
    Q.Free;
  end;
end;

function TRxSqlDBWebTable.GetFooter(ADataSet: TDataSet): string;
var
  i:integer;
begin
  Result:=FFooterText ;
  for I:=0 to FAggList.Count-1 do
    Result:=StringReplace(Result, sSumFunc + FAggList[i].FName, FloatToStr(FAggList[i].FValue), [rfReplaceAll]);

  Result:=StringReplace(Result, sCountFunc, IntToStr(ADataSet.RecordCount), [rfReplaceAll]);
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
