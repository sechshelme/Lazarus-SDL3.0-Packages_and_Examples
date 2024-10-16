program project1;
uses
  SDL_stdinc,
  SDL_system,
  SDL_guid,
  SDL_properties,
  SDL_log,
  SDL_atomic,
  SDL_power,
  SDL_filesystem,
  SDL_storage,       // SDL_properties, SDL_filesystem
  SDL_hidapi,
  SDL_assert,
  SDL_hints,
  SDL_thread,        // SDL_atomic, SDL_properties
  SDL_mutex,         // SDL_atomic, SDL_thread
  SDL_blendmode,
  SDL_rect,          // inline !!!!!!!!!!!!!!!!
  SDL_pixels,
  SDL_iostream,      // SDL_properties
  SDL_process,       // SDL_properties, SDL_iostream
  SDL_sensor,        // SDL_properties
  SDL_surface,       // SDL_properties, SDL_pixels, SDL_iostream, SDL_blendmode, SDL_rect
  SDL_video,         // SDL_properties, SDL_pixels, SDL_rect, SDL_surface
  SDL_dialog,        // SDL_video
  SDL_scancode,
  SDL_keycode,
  SDL_keyboard,      // SDL_properties, SDL_video, SDL_keycode, SDL_scancode, SDL_rect
  SDL_joystick,      // SDL_properties, SDL_mutex, SDL_guid, SDL_sensor, SDL_power
  SDL_gamepad,       // SDL_properties, SDL_iostream, SDL_guid, SDL_power, SDL_joystick, SDL_sensor
  SDL_haptic,        // SDL_joystick
  SDL_audio,         // SDL_properties, SDL_iostream
  SDL_camera,        // SDL_properties, SDL_pixels, SDL_surface
  SDL_mouse,         // SDL_video, SDL_surface
  SDL_touch,         // SDL_mouse
  SDL_pen,
  SDL_events,        // SDL_video, SDL_keyboard, SDL_keycode, SDL_mouse, SDL_pen, SDL_sensor, SDL_touch, SDL_scancode, SDL_joystick, SDL_power, SDL_audio, SDL_camera
  SDL_init,          // SDL_events
  SDL_render,        // SDL_rect, SDL_pixels, SDL_video, SDL_properties, SDL_surface, SDL_blendmode, SDL_events




  SDL_gpu,

  ctypes;
begin

end.

