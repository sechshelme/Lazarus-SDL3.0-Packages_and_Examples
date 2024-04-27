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
[binaries]
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

## Mehr Infos
https://mesonbuild.com/Cross-compilation.html
https://stackoverflow.com/questions/57436089/meson-can-not-find-windows-resource-compiler-on-linux

