# Binär Datei in Byte Array Komstante

## Als C-Include
`xxd -i xxx.bin -> xxx.h`

## Als Pascal Constante Array
`hexdump -v -e '16/1 "$%02X, " "\n"' xxx.bin > xxx.txt`


