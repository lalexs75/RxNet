unit RegisterRxNet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

procedure Register;
implementation
uses SMPP, LResources;

const
  sRxToolsPage = 'RX Tools';

procedure Register;
begin
  RegisterComponents(sRxToolsPage,[TRxSMPP]);
end;

initialization
  {$i rxnet.lrs}
end.

