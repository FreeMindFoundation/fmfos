;=======================================================
;= Menuet OS Bootloader version 2.0.0
;= Feb2004
;=
;= Copyright (C):
;= 
;= Alex Nogueira Teixeira (Jan2002)
;= E-Mail : alexwal@siteplanet.com.br
;=  - Basic booting functions
;=
;= Jiang Yio (Feb2004)
;= E-Mail : jyio@stuy.edu
;=  - Gets kernel filename specified at run-time
;=    - Filename is name.ext, 12 chars max including '.'
;=  - Capitalizes lowercase input
;=  - Converts input to FAT12 entry format
;=  - Boots kernel
;=
;= Distributed under GPL, see file COPYING for details
;=
;= equates for program
;=======================================================
   
lf  equ 0ah
cr  equ 0dh
   
pos_read_tmp       equ 0700h ;position for temporary read
boot_program       equ 07c00h ;position for boot code
seg_read_kernel    equ 01000h ;seguiment to kernel read
   
                   jmp start_program
                   nop
   
oemname            db 'MENUETOS'
bytespersector     dw 512
sectorspercluster  db 1
ressectors         dw 1
numcopiesfat       db 2
maxallocrootdir    dw 224
maxsectors         dw 2880 ;for 1.44 mbytes disk
mediadescriptor    db 0f0h ;fd = 2 sides 18 sectors
sectorsperfat      dw 9
sectorspertrack    dw 18
heads              dw 2
hiddensectors      dd 0
hugesectors        dd 0 ;if sectors > 65536
drivenumber        db 0
                   db 0
bootsignature      db 029h ;extended boot signature
volumeid           dd 0
volumelabel        db 'MENUET DISK'
filesystemtype     db 'FAT12   '


start_program:

  xor ax,ax
  mov ss,ax
  mov sp,boot_program
  push ss
  pop ds

  mov  si,loading+boot_program
loop_loading:
  lodsb
  or al,al
;  jz procura_arquivo_novamente
  jz getfname
  mov ah,0eh
  mov bx,07h
  int 010h
  jmp loop_loading

capitalize:
	cmp	al, 97
	jb	capitalizeret
	cmp	al, 122
	ja	capitalizeret
	sub	al, 32
	capitalizeret:
		ret

fixfname:
	mov	bx, boot_program+filename
	xor	dx, dx
	mov	cx, boot_program/16
	mov	es, cx
	flbl0:	cmp	byte [bx], '.'	; copy filename
		je	flbl1
		mov	al, byte [bx]
		mov	di, arq_boot
		add	di, dx
		stosb
		inc	bx
		inc	dx
		jmp	flbl0
	flbl1:	mov	bx, boot_program+filename+12
	flbl2:	dec	bx
		cmp	byte [bx], ' '
		je	flbl2
		mov	dx, 10
	flbl3:	cmp	byte [bx], '.'	; copy ext
		je	procura_arquivo_novamente
		mov	al, byte [bx]
		mov	di, arq_boot
		add	di, dx
		stosb
		dec	bx
		dec	dx
		jmp	flbl3

getfname:
	mov	bl, 0
	mov	ah, 0
	glbl0:	push	bx
		mov	ax, 10h
		int	16h
		call	capitalize
		pop	bx
		cmp	ah, 0eh
		je	glbl1
		cmp	ah, 1ch
		je	fixfname
		cmp	bl, 13
		je	fixfname
		mov	cx, boot_program/16
		mov	es, cx
		mov	di, filename
		add	di, bx
		stosb
		push	bx
		mov	si, filename+boot_program
		add	si, bx
		lodsb
		or	al, al
		mov	ah, 0eh
		mov	bx, 07h
		int	10h
		pop	bx
		inc	bl
		jmp	glbl0
	glbl1:
		cmp	bl, 0
		je	glbl0
		dec	bl
		push	bx
		mov	dh, 1	; Row
		mov	dl, bl	; Col
		mov	ah, 2	; Move
		xor	bh, bh	; Page
		int	10h
		pop	bx
		jmp	glbl0

procura_arquivo_novamente:

  push ss
  pop es

  mov  bp,16

newtry:

  dec bp
  jz  mensagem_erro_arquivo

  mov ax,020eh ;read, 14 sectors for directory
  mov bx,pos_read_tmp ;es:bx read position
  mov cx,02h ;track 0, sector 2
  mov dx,0100h ;head 1, drive 0 (a:)
  call read_sector
;  int 013h ;read sectors
;  jc  newtry ; mensagem_erro_arquivo

  mov si,bx

loop_compara_entrada_diretorio:

  push si
  mov cx,11 ;file name
  mov di,arq_boot+boot_program
  rep cmpsb
  pop si
  je le_arquivo_kernel
  add si,32
  cmp si,pos_read_tmp+(512*14) ;end of directory
  jb loop_compara_entrada_diretorio

mensagem_erro_arquivo:

  mov si,mens_erro+boot_program

loop_envio_mensagem:

  lodsb
  or al,al
  jz espera_digitar_tecla
  mov ah,0eh
  mov bx,07h
  int 010h
  jmp loop_envio_mensagem

espera_digitar_tecla:

  cli
  hlt
  jmp $


; load kernel

le_arquivo_kernel:

  mov bp,[si+01ah]
  mov ax,0209h ;read, 9 sectors
  mov bx,pos_read_tmp ;es:bx read position
  mov cx,02h ;track 0, sector 2
  xor dx,dx ;head 0, drive 0 (a:)
  call read_sector
;  int 013h  ;read sectors
  jc mensagem_erro_arquivo
  mov ax,seg_read_kernel
  mov es,ax
  xor bx,bx

  ; read kernel to es:bx 

loop_obtem_dados_kernel:
  call le_setor_dados
  jc mensagem_erro_arquivo
;  add bx,0200h ;add one sector

  push bx
  mov bx,es
  add bx,0x20
  mov es,bx
  pop bx

  mov di,bp
  shr di,01h
  pushf
  add di,bp
  add di,pos_read_tmp
  mov ax,[di]
  popf
  jc desloca_4_direita
  and ax,0fffh
  jmp verifica_fim_setores
desloca_4_direita:
  mov cl,4
  shr ax,cl
verifica_fim_setores:
  cmp ax,0ff8h
  jae executa_kernel
  mov bp,ax
  jmp loop_obtem_dados_kernel

executa_kernel:
  push word seg_read_kernel
  push word 00h
  retf


le_setor_dados: ;es:bx -> position in read buffer

  ;bp -> logical sector to read
  ;carry <- 0 read OK
  ;carry <- 1 read error
  push bx
  mov ax,0e2eh ;decimal point
  xor bh,bh
  int 010h
  pop bx
  mov ax,bp ;data sector to read
  add ax,31 ;get logical sector
  mov cx,36 ;sector/track
  xor dx,dx
  div cx
  mov ch,al ;track transfer
  xor dh,dh ;head 0
  cmp dl,18
  jb cabeca_correta
  sub dl,18
  inc dh ;head 1
cabeca_correta:
  inc dl
  mov cl,dl
  xor dl,dl ;drive 0 (a:)
  mov ax,0201h ;read 1 sector
  call read_sector
;  int 013h
  retn

read_sector:

  push bp
  mov  bp,20
 newread:
  dec  bp
  jz   mensagem_erro_arquivo
  push ax bx cx dx
  int  0x13
  pop  dx cx bx ax
  jc   newread
  pop  bp
  retn

times (0x1fe-(13+19+2+1+12))-$ db 00h

filename  db '            ',0		; space for filename.ext
loading   db 'MenuetOS Kernel:',13,10,0
mens_erro db 13,10
errors    db 16
arq_boot  db '           ',0		; kernel filename at 7C00+1F2 = 7DF2
signature db 55h,0aah			; boot signature

