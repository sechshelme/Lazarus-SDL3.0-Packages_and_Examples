# Einleitung

https://emscripten.org/docs/getting_started/Tutorial.html

# Installieren
```bash
sudo apt install emscripten
```

# Eine HelloWorld kompilieren

```bash
emcc hello.c -o hello.html
```

# Localhist-Server starten

```bash
python3 -m http.server
```
oder
```bash
php -S localhost:8000
```
oder
```bash
busybox httpd -f -p 8000
```

# Browser Ansicht.

Im Broweser: `http://localhost:8000/`


