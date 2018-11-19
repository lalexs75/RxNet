unit rxweb_fbdataset_register;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

procedure Register;

implementation
uses RxFBDataSetWebTable, RxSqlDBWebTable;

procedure Register;
begin
  RegisterComponents('RX DBAware',[TRxFBDataSetWebTable, TRxSqlDBWebTable]);
end;

end.

