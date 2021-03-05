{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit HKCompPacks;

{$warn 5023 off : no warning about unused units}
interface

uses
  HKStringGrid, HKCompPacksTypes, Requester, HKConnection, HKLogFile, 
  AuthModel, HKConfig, HKModel, HKPaginationModel, HKPaginantion, 
  HKSearchHeader, HKSelfUpdater, HKSqlStringGrid, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('HKStringGrid', @HKStringGrid.Register);
  RegisterUnit('HKConnection', @HKConnection.Register);
  RegisterUnit('HKLogFile', @HKLogFile.Register);
  RegisterUnit('AuthModel', @AuthModel.Register);
  RegisterUnit('HKModel', @HKModel.Register);
  RegisterUnit('HKPaginationModel', @HKPaginationModel.Register);
  RegisterUnit('HKPaginantion', @HKPaginantion.Register);
  RegisterUnit('HKSearchHeader', @HKSearchHeader.Register);
  RegisterUnit('HKSelfUpdater', @HKSelfUpdater.Register);
  RegisterUnit('HKSqlStringGrid', @HKSqlStringGrid.Register);
end;

initialization
  RegisterPackage('HKCompPacks', @Register);
end.
