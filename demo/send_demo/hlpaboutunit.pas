unit hlpAboutUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics,
  ButtonPanel, StdCtrls, versionresource;

type

  { ThlpAboutForm }

  ThlpAboutForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ListBox1: TListBox;
    procedure FormCreate(Sender: TObject);
  private
    procedure ParceVersionInfo(V:TVersionResource);
  public
    { public declarations }
  end; 

var
  hlpAboutForm: ThlpAboutForm;

implementation
uses resource, resreader,
{$IFDEF WINDOWS}
  winpeimagereader
{$ELSE}
  elfreader
{$ENDIF}
;

{$R *.lfm}

{ ThlpAboutForm }

procedure ThlpAboutForm.FormCreate(Sender: TObject);
var
  Res:TResources;
  i:integer;
  Reader:TAbstractResourceReader;
  V:TVersionResource;
begin
  ListBox1.Items.Clear;
  {$IFDEF WINDOWS}
  Reader:=TWinPEImageResourceReader.Create;
  {$ELSE}
  Reader:=TElfResourceReader.Create;
  {$ENDIF}
  Res:=TResources.Create;
  Res.LoadFromFile(ParamStr(0), Reader);
  for i:=0 to Res.Count-1 do
  begin
    if Res[i] is TVersionResource then
      V:=Res[i] as TVersionResource;
  end;
  if Assigned(V) then
    ParceVersionInfo(V);
  Res.Free;
  Reader.Free;
end;

procedure ThlpAboutForm.ParceVersionInfo(V: TVersionResource);
var
  i,j:integer;
begin
  for i:=0 to V.StringFileInfo.Count-1 do
  begin
    for j:=0 to V.StringFileInfo[i].Count-1 do
      ListBox1.Items.Add('Vers: ' + V.StringFileInfo[i].Keys[j]+' = ' + SysToUTF8(V.StringFileInfo[i].ValuesByIndex[j]));
  end;
end;

end.

