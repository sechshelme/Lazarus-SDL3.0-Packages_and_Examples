- Android-Studio installieren.
- AS starten
- Tools -> SDL-Manager -> Android SDK -> SDK-Tools -> NDK (Side by side) -> apply -> OK -> .... -> Finish
- AS beenden
Dann sollte ein Odner `~/Android/Sdk/nnk/xx.x.xxxxxxxx` da sein.

Dann sollte man mit dieser oder ähnlichen Zeile die Adroid-Libs bauen können.

```
cmake ../SDL/ -DCMAKE_TOOLCHAIN_FILE=/home/tux/Android/Sdk/ndk/28.0.12674087/build/cmake/android.toolchain.cmake -DANDROID_ABI=arm64-v8a -DSDL_ANDROID_HOME=/home/tux/Android/Sdk -DANDROID_PLATFORM=23 -DSDL_TESTS=ON
```

Mehr Infos: 
- (SDL3 git doc)[https://github.com/libsdl-org/SDL/blob/main/docs/README-android.md]
