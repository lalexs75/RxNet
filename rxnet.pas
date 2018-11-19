{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit rxnet;

{$warn 5023 off : no warning about unused units}
interface

uses
  SMPP, smsPDUConstsUnit, smsPDUOptParamTypesUnit, smsPDUTypesUnit, 
  RegisterRxNet, smsUtils, smsCommandTypes, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RegisterRxNet', @RegisterRxNet.Register);
end;

initialization
  RegisterPackage('rxnet', @Register);
end.
