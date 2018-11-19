{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit rxGSM_Inform;

{$warn 5023 off : no warning about unused units}
interface

uses
  SMSGsmInfo, SMSGsmInfoStr, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('SMSGsmInfo', @SMSGsmInfo.Register);
end;

initialization
  RegisterPackage('rxGSM_Inform', @Register);
end.
