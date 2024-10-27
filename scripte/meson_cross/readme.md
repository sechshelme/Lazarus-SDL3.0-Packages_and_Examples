# Beispiel Freetype

```bash
git clone https://gitlab.freedesktop.org/freetype/freetype.git
mkdir build_win64
cd build_win64
#  meson setup --cross-file cross.txt build-mingw ../freetype/
meson setup --cross-file ../cross.txt ../freetype/
ninja -j16
sudo ninja install
```

## cross.txt
```
[binaries]ini
c = 'x86_64-w64-mingw32-gcc'
cpp = 'x86_64-w64-mingw32-g++'
ar = 'x86_64-w64-mingw32-ar'
strip = 'x86_64-w64-mingw32-strip'
exe_wrapper = 'wine64'
windres = 'x86_64-w64-mingw32-windres'

[host_machine]
system = 'windows'
cpu_family = 'x86_64'
cpu = 'x86_64'
endian = 'little'
```
### Evtl. noch dies:
```
[properties]
c_args = ['-D_FILE_OFFSET_BITS=64']
cpp_args = ['-D_FILE_OFFSET_BITS=64']
```

## cross.txt ( Muster GTK4 )
```ini
[host_machine]
system = 'windows'
cpu_family = 'x86_64'
cpu = 'x86_64'
endian = 'little'

# Fedora builds GCC with --enable-default-ssp by default, but mingw64 puts its
# stack-protected functions in a separate library which needs to be linked
# explicitly, hence this flag.
[properties]
c_args = []
c_link_args = ['-fstack-protector']

[binaries]
c = 'x86_64-w64-mingw32-gcc'
cpp = 'x86_64-w64-mingw32-g++'
ar = 'x86_64-w64-mingw32-ar'
ld = 'x86_64-w64-mingw32-ld'
objcopy = 'x86_64-w64-mingw32-objcopy'
strip = 'x86_64-w64-mingw32-strip'
pkgconfig = 'x86_64-w64-mingw32-pkg-config'
windres = 'x86_64-w64-mingw32-windres'
```

## Mehr Infos
https://mesonbuild.com/Cross-compilation.html
https://stackoverflow.com/questions/57436089/meson-can-not-find-windows-resource-compiler-on-linux
https://sourceforge.net/p/meson/wiki/Cross%20compilation/

## Keine Tests ( Bei meson )
`-Dtests=disabled`


