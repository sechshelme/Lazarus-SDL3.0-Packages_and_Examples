/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//   Erzeugt am: 22-10-2024   15:23                        //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

type
  PSDL_hid_bus_type = ^TSDL_hid_bus_type;
  TSDL_hid_bus_type = longint;

const
  SDL_HID_API_BUS_UNKNOWN = $00;
  SDL_HID_API_BUS_USB = $01;
  SDL_HID_API_BUS_BLUETOOTH = $02;
  SDL_HID_API_BUS_I2C = $03;
  SDL_HID_API_BUS_SPI = $04;

type
  PSDL_hid_device_info = ^TSDL_hid_device_info;

  TSDL_hid_device_info = record
    path: pansichar;
    vendor_id: word;
    product_id: word;
    serial_number: Pwchar_t;
    release_number: word;
    manufacturer_string: Pwchar_t;
    product_string: Pwchar_t;
    usage_page: word;
    usage: word;
    interface_number: longint;
    interface_class: longint;
    interface_subclass: longint;
    interface_protocol: longint;
    bus_type: TSDL_hid_bus_type;
    Next: PSDL_hid_device_info;
  end;

  TSDL_hid_device = record
  end;
  PSDL_hid_device = ^TSDL_hid_device;

function SDL_hid_init: longint; cdecl; external libSDL3;
function SDL_hid_exit: longint; cdecl; external libSDL3;
function SDL_hid_device_change_count: TUint32; cdecl; external libSDL3;
function SDL_hid_enumerate(vendor_id: word; product_id: word): PSDL_hid_device_info; cdecl; external libSDL3;
procedure SDL_hid_free_enumeration(devs: PSDL_hid_device_info); cdecl; external libSDL3;
function SDL_hid_open(vendor_id: word; product_id: word; serial_number: Pwchar_t): PSDL_hid_device; cdecl; external libSDL3;
function SDL_hid_open_path(path: pansichar): PSDL_hid_device; cdecl; external libSDL3;
function SDL_hid_write(dev: PSDL_hid_device; Data: pansichar; length: Tsize_t): longint; cdecl; external libSDL3;
function SDL_hid_read_timeout(dev: PSDL_hid_device; Data: pansichar; length: Tsize_t; milliseconds: longint): longint; cdecl; external libSDL3;
function SDL_hid_read(dev: PSDL_hid_device; Data: pansichar; length: Tsize_t): longint; cdecl; external libSDL3;
function SDL_hid_set_nonblocking(dev: PSDL_hid_device; nonblock: longint): longint; cdecl; external libSDL3;
function SDL_hid_send_feature_report(dev: PSDL_hid_device; Data: pansichar; length: Tsize_t): longint; cdecl; external libSDL3;
function SDL_hid_get_feature_report(dev: PSDL_hid_device; Data: pansichar; length: Tsize_t): longint; cdecl; external libSDL3;
function SDL_hid_get_input_report(dev: PSDL_hid_device; Data: pansichar; length: Tsize_t): longint; cdecl; external libSDL3;
function SDL_hid_close(dev: PSDL_hid_device): longint; cdecl; external libSDL3;
function SDL_hid_get_manufacturer_string(dev: PSDL_hid_device; _string: Pwchar_t; maxlen: Tsize_t): longint; cdecl; external libSDL3;
function SDL_hid_get_product_string(dev: PSDL_hid_device; _string: Pwchar_t; maxlen: Tsize_t): longint; cdecl; external libSDL3;
function SDL_hid_get_serial_number_string(dev: PSDL_hid_device; _string: Pwchar_t; maxlen: Tsize_t): longint; cdecl; external libSDL3;
function SDL_hid_get_indexed_string(dev: PSDL_hid_device; string_index: longint; _string: Pwchar_t; maxlen: Tsize_t): longint; cdecl; external libSDL3;
function SDL_hid_get_device_info(dev: PSDL_hid_device): PSDL_hid_device_info; cdecl; external libSDL3;
function SDL_hid_get_report_descriptor(dev: PSDL_hid_device; buf: pansichar; buf_size: Tsize_t): longint; cdecl; external libSDL3;
procedure SDL_hid_ble_scan(active: Tbool); cdecl; external libSDL3;

{$ENDIF read_interface}


{$IFDEF read_implementation}


{$ENDIF read_implementation}
