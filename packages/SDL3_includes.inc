  //{%MainUnit sdl3.pas}


{$include SDL3/SDL_stdinc.inc}
{$include SDL3/SDL_guid.inc}
{$include SDL3/SDL_properties.inc}
{$include SDL3/SDL_log.inc}
{$include SDL3/SDL_atomic.inc}
{$include SDL3/SDL_timer.inc}
{$include SDL3/SDL_time.inc}
{$include SDL3/SDL_cpuinfo.inc}
{$include SDL3/SDL_error.inc}
{$include SDL3/SDL_loadso.inc}
{$include SDL3/SDL_version.inc}
{$include SDL3/SDL_clipboard.inc}
{$include SDL3/SDL_power.inc}
{$include SDL3/SDL_filesystem.inc}
{$include SDL3/SDL_storage.inc}       // SDL_properties, SDL_filesystem
{$include SDL3/SDL_hidapi.inc}
{$include SDL3/SDL_assert.inc}
{$include SDL3/SDL_hints.inc}
{$include SDL3/SDL_thread.inc}        // SDL_atomic, SDL_properties
{$include SDL3/SDL_mutex.inc}         // SDL_atomic, SDL_thread
{$include SDL3/SDL_blendmode.inc}
{$include SDL3/SDL_rect.inc}          // inline !!!!!!!!!!!!!!!!
{$include SDL3/SDL_pixels.inc}
{$include SDL3/SDL_iostream.inc}      // SDL_properties
{$include SDL3/SDL_process.inc}       // SDL_properties, SDL_iostream
{$include SDL3/SDL_sensor.inc}        // SDL_properties
{$include SDL3/SDL_surface.inc}       // SDL_properties, SDL_pixels, SDL_iostream, SDL_blendmode, SDL_rect
{$include SDL3/SDL_video.inc}         // SDL_properties, SDL_pixels, SDL_rect, SDL_surface
{$include SDL3/SDL_system.inc}        // SDL_video
{$include SDL3/SDL_dialog.inc}        // SDL_video
{$include SDL3/SDL_messagebox.inc}    // SDL_video
{$include SDL3/SDL_vulkan.inc}        // SDL_video
{$include SDL3/SDL_scancode.inc}
{$include SDL3/SDL_keycode.inc}
{$include SDL3/SDL_keyboard.inc}      // SDL_properties, SDL_video, SDL_keycode, SDL_scancode, SDL_rect
{$include SDL3/SDL_joystick.inc}      // SDL_properties, SDL_mutex, SDL_guid, SDL_sensor, SDL_power
{$include SDL3/SDL_gamepad.inc}       // SDL_properties, SDL_iostream, SDL_guid, SDL_power, SDL_joystick, SDL_sensor
{$include SDL3/SDL_haptic.inc}        // SDL_joystick
{$include SDL3/SDL_audio.inc}         // SDL_properties, SDL_iostream
{$include SDL3/SDL_camera.inc}        // SDL_properties, SDL_pixels, SDL_surface
{$include SDL3/SDL_mouse.inc}         // SDL_video, SDL_surface
{$include SDL3/SDL_touch.inc}         // SDL_mouse
{$include SDL3/SDL_pen.inc}
{$include SDL3/SDL_events.inc}        // SDL_video, SDL_keyboard, SDL_keycode, SDL_mouse, SDL_pen, SDL_sensor, SDL_touch, SDL_scancode, SDL_joystick, SDL_power, SDL_audio, SDL_camera
{$include SDL3/SDL_init.inc}          // SDL_events
{$include SDL3/SDL_render.inc}        // SDL_rect, SDL_pixels, SDL_video, SDL_properties, SDL_surface, SDL_blendmode, SDL_events
{$include SDL3/SDL_metal.inc}
{$include SDL3/SDL_locale.inc}
{$include SDL3/SDL_misc.inc}
{$include SDL3/SDL_platform.inc}
{$include SDL3/SDL_revision.inc}
{$include SDL3/SDL_main.inc}          // SDL_init, SDL_events
{$include SDL3/SDL_gpu.inc}           // SDL_properties, SDL_pixels, SDL_surface, SDL_rect, SDL_video

