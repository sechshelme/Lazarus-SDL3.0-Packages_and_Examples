{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit cairo_package;

{$warn 5023 off : no warning about unused units}
interface

uses
  cairo, cairo_svg, cairo_tee, cairo_gobject, cairo_pdf, cairo_ps, 
  cairo_script, cairo_script_interpreter, cairo_xcb, cairo_xlib, 
  cairo_xlib_xrender, cairo_ft, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('cairo_package', @Register);
end.
