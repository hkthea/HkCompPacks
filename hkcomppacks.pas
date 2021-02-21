{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit HKCompPacks;

{$warn 5023 off : no warning about unused units}
interface

uses
  HKStringGrid, HKCompPacksTypes, Requester, HKConnection, HKLogFile, 
  PeekingChromium, AuthModel, HKConfig, HKModel, HKPaginationModel, 
  HKPaginantion, HKSearchHeader, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('HKStringGrid', @HKStringGrid.Register);
  RegisterUnit('HKConnection', @HKConnection.Register);
  RegisterUnit('HKLogFile', @HKLogFile.Register);
  RegisterUnit('PeekingChromium', @PeekingChromium.Register);
  RegisterUnit('AuthModel', @AuthModel.Register);
  RegisterUnit('HKModel', @HKModel.Register);
  RegisterUnit('HKPaginationModel', @HKPaginationModel.Register);
  RegisterUnit('HKPaginantion', @HKPaginantion.Register);
  RegisterUnit('HKSearchHeader', @HKSearchHeader.Register);
end;

initialization
  RegisterPackage('HKCompPacks', @Register);
end.
