unit SDL3_net;

interface

uses
  SDL3;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

const
  {$IFDEF Linux}
  sdl3_net_lib = 'libSDL3_net.so';
  {$ENDIF}

  {$IFDEF Windows}
  sdl3_net_lib = 'SDL3_net.dll';
  {$ENDIF}

const
  SDL_NET_MAJOR_VERSION = 3;
  SDL_NET_MINOR_VERSION = 0;
  SDL_NET_PATCHLEVEL = 0;

type
  TSDLNet_Address = Pointer;
  PSDLNet_Address = ^TSDLNet_Address;
  PPSDLNet_Address = ^PSDLNet_Address;

  TSDLNet_StreamSocket = Pointer;
  PSDLNet_StreamSocket = ^TSDLNet_StreamSocket;
  PPSDLNet_StreamSocket = ^PSDLNet_StreamSocket;

  TSDLNet_Server = Pointer;
  PSDLNet_Server = ^TSDLNet_Server;

  TSDLNet_DatagramSocket = Pointer;
  PSDLNet_DatagramSocket = ^TSDLNet_DatagramSocket;
  PPSDLNet_DatagramSocket = ^PSDLNet_DatagramSocket;

  TSDLNet_Datagram = record
    addr: PSDLNet_Address;
    port: TUint16;
    buf: PUint8;
    buflen: longint;
  end;
  PSDLNet_Datagram = ^TSDLNet_Datagram;
  PPSDLNet_Datagram = ^PSDLNet_Datagram;

//procedure SDL_NET_VERSION(X: PSDL_Version);

//function SDLNet_LinkedVersion: PSDL_Version; cdecl; external sdl3_net_lib;
function SDLNet_Init: longint; cdecl; external sdl3_net_lib;
procedure SDLNet_Quit; cdecl; external sdl3_net_lib;
function SDLNet_ResolveHostname(host: PChar): PSDLNet_Address; cdecl; external sdl3_net_lib;
function SDLNet_WaitUntilResolved(address: PSDLNet_Address; timeout: TSint32): longint; cdecl; external sdl3_net_lib;
function SDLNet_GetAddressStatus(address: PSDLNet_Address): longint; cdecl; external sdl3_net_lib;
function SDLNet_GetAddressString(address: PSDLNet_Address): PChar; cdecl; external sdl3_net_lib;
function SDLNet_RefAddress(address: PSDLNet_Address): PSDLNet_Address; cdecl; external sdl3_net_lib;
procedure SDLNet_UnrefAddress(address: PSDLNet_Address); cdecl; external sdl3_net_lib;
procedure SDLNet_SimulateAddressResolutionLoss(percent_loss: longint); cdecl; external sdl3_net_lib;
function SDLNet_CompareAddresses(a: PSDLNet_Address; b: PSDLNet_Address): longint; cdecl; external sdl3_net_lib;
function SDLNet_GetLocalAddresses(num_addresses: Plongint): PPSDLNet_Address; cdecl; external sdl3_net_lib;
procedure SDLNet_FreeLocalAddresses(addresses: PPSDLNet_Address); cdecl; external sdl3_net_lib;
function SDLNet_CreateClient(address: PSDLNet_Address; port: TUint16): PSDLNet_StreamSocket; cdecl; external sdl3_net_lib;
function SDLNet_WaitUntilConnected(sock: PSDLNet_StreamSocket; timeout: TSint32): longint; cdecl; external sdl3_net_lib;
function SDLNet_CreateServer(addr: PSDLNet_Address; port: TUint16): PSDLNet_Server; cdecl; external sdl3_net_lib;
function SDLNet_AcceptClient(server: PSDLNet_Server; client_stream: PPSDLNet_StreamSocket): longint; cdecl; external sdl3_net_lib;
procedure SDLNet_DestroyServer(server: PSDLNet_Server); cdecl; external sdl3_net_lib;
function SDLNet_GetStreamSocketAddress(sock: PSDLNet_StreamSocket): PSDLNet_Address; cdecl; external sdl3_net_lib;
function SDLNet_GetConnectionStatus(sock: PSDLNet_StreamSocket): longint; cdecl; external sdl3_net_lib;
function SDLNet_WriteToStreamSocket(sock: PSDLNet_StreamSocket; buf: pointer; buflen: longint): longint; cdecl; external sdl3_net_lib;
function SDLNet_GetStreamSocketPendingWrites(sock: PSDLNet_StreamSocket): longint; cdecl; external sdl3_net_lib;
function SDLNet_WaitUntilStreamSocketDrained(sock: PSDLNet_StreamSocket; timeout: TSint32): longint; cdecl; external sdl3_net_lib;
function SDLNet_ReadFromStreamSocket(sock: PSDLNet_StreamSocket; buf: pointer; buflen: longint): longint; cdecl; external sdl3_net_lib;
procedure SDLNet_SimulateStreamPacketLoss(sock: PSDLNet_StreamSocket; percent_loss: longint); cdecl; external sdl3_net_lib;
procedure SDLNet_DestroyStreamSocket(sock: PSDLNet_StreamSocket); cdecl; external sdl3_net_lib;
function SDLNet_CreateDatagramSocket(addr: PSDLNet_Address; port: TUint16): PSDLNet_DatagramSocket; cdecl; external sdl3_net_lib;
function SDLNet_SendDatagram(sock: PSDLNet_DatagramSocket; address: PSDLNet_Address; port: TUint16; buf: pointer; buflen: longint): longint; cdecl; external sdl3_net_lib;
function SDLNet_ReceiveDatagram(sock: PSDLNet_DatagramSocket; dgram: PPSDLNet_Datagram): longint; cdecl; external sdl3_net_lib;
procedure SDLNet_DestroyDatagram(dgram: PSDLNet_Datagram); cdecl; external sdl3_net_lib;
procedure SDLNet_SimulateDatagramPacketLoss(sock: PSDLNet_DatagramSocket; percent_loss: longint); cdecl; external sdl3_net_lib;
procedure SDLNet_DestroyDatagramSocket(sock: PSDLNet_DatagramSocket); cdecl; external sdl3_net_lib;
function SDLNet_WaitUntilInputAvailable(vsockets: Ppointer; numsockets: longint; timeout: TSint32): longint; cdecl; external sdl3_net_lib;

implementation

//procedure SDL_NET_VERSION(X: PSDL_Version);
//begin
//  X^.major := SDL_NET_MAJOR_VERSION;
//  X^.minor := SDL_NET_MINOR_VERSION;
//  X^.patch := SDL_NET_PATCHLEVEL;
//end;

end.
