[org 0x7c00]

mov ah, 0x0e

mov al, 0ah
int 0x10
mov al, '*'
int 0x10
mov al, '*'
int 0x10
mov al, '*'
int 0x10
mov al, 0ah
int 0x10
mov al, 'L'
int 0x10
mov al, 'i'
int 0x10
mov al, 'l'
int 0x10
mov al, 'i'
int 0x10
mov al, 'u'
int 0x10
mov al, 'm'
int 0x10
mov al, ' '
int 0x10
mov al, 'O'
int 0x10
mov al, 'S'
int 0x10
mov al, ' '
int 0x10
mov al, 'B'
int 0x10
mov al, 'o'
int 0x10
mov al, 'o'
int 0x10
mov al, 't'
int 0x10
mov al, 'l'
int 0x10
mov al, 'o'
int 0x10
mov al, 'a'
int 0x10
mov al, 'd'
int 0x10
mov al, 'e'
int 0x10
mov al, 'r'
int 0x10
mov al, 0ah
int 0x10
mov al, '*'
int 0x10
mov al, '*'
int 0x10
mov al, '*'
int 0x10
mov al, 0ah
int 0x10

jmp $ ; Jump to the current address ( i.e. forever ).
   
times 510 -($-$$) db 0 ; Pad the boot sector out with zeros
dw 0xaa55 ; Last two bytes form the magic number ,
