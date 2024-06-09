program Project1;

uses
  unixtype,
  ctypes,
  xlib,
  xutil,
  keysym,
  x,
  Cairo,
  cairo_xlib;

type

  TMyWin = class(TObject)
  private
    dis: PDisplay;
    scr: cint;
    win: TWindow;
    widht, Height: cuint;
    wm_delete_window: TAtom;

    cr_surface: Pcairo_surface_t;
    cr: Pcairo_t;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Draw;
    procedure Run;
  end;

  constructor TMyWin.Create;
  begin
    inherited Create;

    dis := XOpenDisplay(nil);
    if dis = nil then begin
      WriteLn('Kann nicht das Display öffnen');
      Halt(1);
    end;
    scr := DefaultScreen(dis);

    widht := 640;
    Height := 480;

    win := XCreateSimpleWindow(dis, RootWindow(dis, scr), 10, 10, widht, Height, 1, BlackPixel(dis, scr), WhitePixel(dis, scr));

    XSelectInput(dis, win, KeyPressMask or ButtonPressMask or ExposureMask);
    XStoreName(dis, win, 'X11-Cairo');
    XMapWindow(dis, win);

    wm_delete_window := XInternAtom(dis, 'WM_DELETE_WINDOW', False);
    XSetWMProtocols(dis, win, @wm_delete_window, 1);

    cr_surface := cairo_xlib_surface_create(dis, win, DefaultVisual(dis, scr), 640, 480);
    cairo_xlib_surface_set_size(cr_surface, 640, 480);
    cr := cairo_create(cr_surface);
  end;

  destructor TMyWin.Destroy;
  begin
    cairo_destroy(cr);
    cairo_surface_destroy(cr_surface);

    XDestroyWindow(dis, win);
    XCloseDisplay(dis);

    inherited Destroy;
  end;

  procedure TMyWin.Draw;
  var
    xc: double = 320;
    yc: double = 240;
    radius: double = 200;
    angele1: double = 45 * (pi / 180);
    angele2: double = 180 * (pi / 180);
    i: integer;
  begin
    for i := 1 to 10000 do begin
      cairo_set_source_rgba(cr, random, 1, 1, 1.0);
      cairo_rectangle(cr, i / 300, 0, 640, 480);
      cairo_fill(cr);

      cairo_set_source_rgba(cr, 0, 0, 0, 1.0);
      cairo_set_line_width(cr, 10.0);
      cairo_arc(cr, xc, yc, radius, angele1, angele2);
      cairo_stroke(cr);

      cairo_set_source_rgba(cr, 1, 0.2, 0.2, 0.6);
      cairo_set_line_width(cr, 6.0);

      cairo_arc(cr, xc, yc, 10.0, 0, 2 * pi);
      cairo_fill(cr);

      cairo_arc(cr, xc, yc, radius, angele1, angele1);
      cairo_line_to(cr, xc, yc);
      cairo_arc(cr, xc, yc, radius, angele2, angele2);
      cairo_line_to(cr, xc, yc);
      cairo_stroke(cr);
    end;
  end;

  procedure TMyWin.Run;
  var
    Event: TXEvent;
    quit: boolean = False;
  begin
    while not quit do begin
      XNextEvent(dis, @Event);
      WriteLn('Event: ', Event._type);

      case Event._type of
        KeyPress: begin
          case XLookupKeysym(@Event.xkey, 0) of
            XK_Escape: begin
              quit := True;
            end;
          end;
        end;
        Expose: begin
          Draw;
        end;
        ClientMessage: begin
          if (Event.xclient.Data.l[0] = wm_delete_window) then begin
            WriteLn('[X] wurde gedrückt');
            quit := True;
          end;
        end;
      end;
    end;
  end;

var
  MyWindows: TMyWin;

begin
  MyWindows := TMyWin.Create;
  MyWindows.Run;
  MyWindows.Free;
end.
//code-
