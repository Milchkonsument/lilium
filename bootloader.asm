; [org 0x7c00] ; set the program to start at this exact memory location. only needed without linker.
KERNEL_OFFSET equ 0x1000 ; This is the memory offset to which we will load our kernel

    mov [BOOT_DRIVE], dl ; stores boot drive no.

    call setup_stack_16bit
    call load_kernel

    cli ; disable interrupts, since 16b and 32b interrupts are implemented differently
    lgdt [gdt_descriptor] ; make CPU aware of our GDT descriptor

    mov eax, cr0 ; To make the switch to protected mode , we set
    or eax, 0x1 ; the first bit of CR0 , a control register
    mov cr0, eax ; Update the control register

    jmp CODE_SEG:protected_mode ; make far jump to clear CPU pipeline of any parallel tasks
    jmp $ ; this should not happen ig?

setup_stack_16bit:
    mov bp , 0x9000 ; Set the base of the stack a little above where BIOS
    mov sp , bp ; loads our boot sector - so it won ’t overwrite us.

load_kernel:
    mov bx, KERNEL_OFFSET ; Set -up parameters for our disk_load routine , so
    mov dh, 15 ; that we load the first 15 sectors ( excluding
    mov dl, [BOOT_DRIVE] ; the boot sector ) from the boot disk ( i.e. our
    call read_disk ; kernel code ) to address KERNEL_OFFSET
    ret

read_disk:
    push dx
    mov ah, 0x02 ; BIOS read sector function
    mov al, dh ; read DH x sectors
    mov ch, 0x0 ; cylinder 0
    mov dh, 0x0 ; segment 0
    mov cl, 0x2 ; read starting 2nd sector

    int 0x13 ; read X sectors to RAM

    jc err_disk_read_general
    
    pop dx
    cmp dh, al ; check if sectors read != sectors expected
    jne err_disk_read_unexpected_segments
    ret

print:
    pusha
    call mode_tty_out
    call print_inner
    popa
    ret

print_inner:
    lodsb ; fetch next char from si into al
    cmp al, 0
    je done
    int 0x10 ; print the character in al
    call print_inner

done:
    ret

set_cursor:
    mov dl, 0                 ;◄■■ SCREEN COLUMN 18 (X).
    mov ah, 2                  ;◄■■ SERVICE TO SET CURSOR POSITION.
    mov bh, 0                  ;◄■■ VIDEO PAGE.
    int 10h                    ;◄■■ BIOS SERVICES.
    RET

; ! ERR FUNCTIONS
err_disk_read_general:
    mov si, msg_err_disk_read_general
    call print

err_disk_read_unexpected_segments:
    mov si, msg_err_disk_read_unexpected_segments
    call print

; ! MODE FUNCTIONS
mode_tty_out:
    mov ah, 0x0e
    ret

; =========================================== 32 bit protected mode ===========================================

[bits 32]
; [extern main]
protected_mode:
    mov ax , DATA_SEG ; Now in PM , our old segments are meaningless ,
    mov ds , ax ; so we point our segment registers to the
    mov ss , ax ; data selector we defined in our GDT
    mov es , ax
    mov fs , ax
    mov gs , ax
    mov ebp , 0x90000 ; Update our stack position so it is right
    mov esp , ebp ; at the top of the free space.

    ; call main
    ; jmp $ ; jump to current line (loop)

; data has to be defined below the infinite jump
; because there's no difference between code and data at the low level
; but it has to be defined before the zero bytes padding obviously

; ! data
BOOT_DRIVE:
    db 0x0

boot_msg_0:
    db '============================', 0

boot_msg_1:
    db '=== Lilium OS Bootloader ===', 0

boot_msg_2:
    db '============================', 0

msg_err_disk_read_general:
    db 'Disk read : general error', 0

msg_err_disk_read_unexpected_segments:
    db 'Disk read : unexpected segments', 0

; ! GDT data
gdt_start:

gdt_null: ; the mandatory null descriptor
    dd 0x0 ; ’dd ’ means define double word ( i.e. 4 bytes )
    dd 0x0

gdt_code: ; the code segment descriptor
; base =0x0 , limit =0 xfffff ,
; 1st flags : ( present )1 ( privilege )00 ( descriptor type )1 -> 1001 b
; type flags : ( code )1 ( conforming )0 ( readable )1 ( accessed )0 -> 1010 b
; 2nd flags : ( granularity )1 (32 - bit default )1 (64 - bit seg )0 ( AVL )0 -> 1100 b
    dw 0xffff ; Limit ( bits 0 -15)
    dw 0x0 ; Base ( bits 0 -15)
    db 0x0 ; Base ( bits 16 -23)
    db 10011010b ; 1st flags , type flags
    db 11001111b ; 2nd flags , Limit ( bits 16 -19)
    db 0x0 ; Base ( bits 24 -31)

gdt_data: ; the data segment descriptor
; Same as code segment except for the type flags :
; type flags : ( code )0 ( expand down )0 ( writable )1 ( accessed )0 -> 0010 b
    dw 0xffff ; Limit ( bits 0 -15)
    dw 0x0 ; Base ( bits 0 -15)
    db 0x0 ; Base ( bits 16 -23)
    db 10010010b ; 1st flags , type flags
    db 11001111b ; 2nd flags , Limit ( bits 16 -19)
    db 0x0 ; Base ( bits 24 -31)

gdt_end: ; The reason for putting a label at the end of the
; GDT is so we can have the assembler calculate
; the size of the GDT for the GDT decriptor ( below )
; GDT descriptior

gdt_descriptor:
    dw gdt_end - gdt_start - 1 ; Size of our GDT , always less one
    ; of the true size
    dd gdt_start ; Start address of our GDT
    ; Define some handy constants for the GDT segment descriptor offsets , which
    ; are what segment registers must contain when in protected mode. For example ,
    ; when we set DS = 0 x10 in PM , the CPU knows that we mean it to use the
    ; segment described at offset 0 x10 ( i.e. 16 bytes ) in our GDT , which in our
    ; case is the DATA segment (0 x0 -> NULL ; 0x08 -> CODE ; 0 x10 -> DATA )
    CODE_SEG equ gdt_code - gdt_start
    DATA_SEG equ gdt_data - gdt_start

times 510 -($-$$) db 0 ; Pad the boot sector out with zeros
dw 0xaa55 ; Last two bytes form the magic number
