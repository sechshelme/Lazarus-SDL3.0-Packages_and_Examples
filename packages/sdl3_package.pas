{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit SDL3_package;

{$warn 5023 off : no warning about unused units}
interface

uses
  SDL3, SDL3_image, SDL3_rtf, SDL3_ttf, SDL3_mixer, SDL3_net, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('SDL3_package', @Register);
end.
