{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit hkscrappingcomp;

{$warn 5023 off : no warning about unused units}
interface

uses
  PeekingChromium, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('PeekingChromium', @PeekingChromium.Register);
end;

initialization
  RegisterPackage('hkscrappingcomp', @Register);
end.
