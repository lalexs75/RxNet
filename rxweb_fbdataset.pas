{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit rxWeb_FBDataSet;

{$warn 5023 off : no warning about unused units}
interface

uses
  RxFBDataSetWebTable, rxweb_fbdataset_register, RxSqlDBWebTable, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('rxweb_fbdataset_register', @rxweb_fbdataset_register.Register);
end;

initialization
  RegisterPackage('rxWeb_FBDataSet', @Register);
end.
