			; SIMPLE - ETI January 1979
			
			; Version 1.4  17-9-2021
			
			; Located at 0x8100
			; User program space at 0x8200
			; ACIA serial routines at 0x9000
			
			
			.ORG 0x8100
			
start:   	ld hl,uprog     ; Point HL to the start of the User Program
			ld sp,hl        ; Set the stack pointer 
tloop:		ld de,tloop     ; start of main loop
			push de
			call ci         ; wait for serial input
			dec hl
			cp 0x5f         ; is it a backspace
			ret z
			
			
			inc hl
			sub 0x26        ; is it & (0x26)
			jp z,type       ; if so, type the next line
			inc a           ; is it % (0x25)
			jp z,pad        ; call the pad routine    
			inc a           ; is it $ (0x24)
			jp z,exec0      ; Execute the User Program
			inc a           ; Is it #  (0x23)
			jp z,start      ; Back to start and reset the stack
			
			; If it is not a command character, restore it
			; and insert it into the User Program RAM
			
			add a,0x23      ; must be user character
			ld (hl),a       ; store to RAM
			inc hl
			cp 0x0d         ; is it CR
			call z,plf      ; print a line feed
			ret             ; and return to tloop
			
			; The pad routine pads out the user RAM with nulls
			; until it finds a carriage return
			; If no CR is found after 64 characters the routine aborts
			; and prints a question mark
			
			
pad:		ld d,0x80       ; set bit 7 of the d register as a flag
com:		push bc
			ld b,0x40       ; a safety counter of 64 characters
com1:		ld a,(hl)       ; get character
			cp 0x0d         ; is it CR ?
			jp z,com2
			ld c,a          ; put character into c for printing
			ld a,d          ; get d (flag)
			rlca            ; rotate bit 7 into carry
			jp nc,nopad     
			ld (hl),0x00    ; store a null (0x00) at current location
nopad:		call nc,co      ; emit character
			inc hl
			dec b           ; decrement safety counter
			jp nz,com1      ; loop around again
			ld c,0x3f       ; if no CR found
			call co         ; print a ? (0x3F) and abort
			jp start
com2:		ld c,(hl)       ; character is a CR
			inc hl
			call co         ; print the CR
plf:		ld c,0x0a       ; and a linefeed (0x0A)
			call co
			pop bc
			ret
			
			
exec0:		pop de
exec:		push hl
			ld hl,exec      ; Put the address of exec on the top of stack
			ex (sp),hl      ; swap (SP) with HL 
			ld a,(hl)       ; HL now contains address of next character
			inc hl
			cp 0x5a               ; Compare with "Z"
			jp nc,error           ; if Z or greater it is an error
			sub 0x41              ; subtract ascii "A"
			ret c                 ; ignore if less than A
			push hl               ; store the character address
			ld hl,tbase           ; form index for command table
			add a,l               ; a contains 0 to 25
			ld l,a                ; which is added back into l
			ld l,(hl)             ; HL contains address of subroutine
			ex (sp),hl            ; Put it onto stack and get original HL
			ret                   ; This will call the subroutine 
			
tbase:		DB 0xf3               ; A                
			DB 0x8c               ; B               
			DB 0xc1               ; C                
			DB 0xcf               ; D                
			DB 0x91 			  ; E	
			DB 0x8c               ; F
			DB 0xda               ; G           
			DB 0x8c               ; H                
			DB 0xd1               ; I                
			DB 0x9e               ; J                
			DB 0xd3 			  ; K
			DB 0xcc               ; L             
			DB 0xb4               ; M                
			DB 0xbe               ; N                
			DB 0x8c               ; O                
			DB 0xf9               ; P                
			DB 0x8c               ; Q                
			DB 0xb0               ; R                
			DB 0x98               ; S                
			DB 0x96               ; T                
			DB 0xf0               ; U                
			DB 0x8c               ; V                
			DB 0x8c               ; W                
			DB 0xc8               ; X                
			DB 0xbd  			  ; Y
			
error:		ld c,0x3f
			call co               ; Print a ?
end1:		dec hl
			ld de, start
			push de
type:		ld d,0
			jp com
subr:		ld (radr),hl
jump:		call vcom
			ld hl,uprog
			ld a,0x2a             ; test for a *
jloop:		cp (hl)
			inc hl
			jp nz,jloop
			dec d
			jp p,jloop
			ret
			
			
retrn:		ld hl,(radr)
			ret
			
			
match:		ld a,(hl)
			inc hl
			sub c
			ld b,0x00
			ret pe
			ld b,0x00
			ret
			
			
testy:		DB 0x3E
testn:		xor a
			cp b
			ret z
			
			
skip:		ld a,0x0d
			ld d,0x00
			jp jloop
exch:		ld a,e
			ld e,c
			ld c,a
			ret
			
			
ld_cnt:		ld e,(hl)           ; load counter
			inc hl
			ret
			
			
dec_cnt:	dec e               ; decrement counter
			ret
			
			
inc_cnt:	inc e               ; increment counter
			ret
			
			
keep:		push hl             ; Store variable
			call vcom
			ld (hl),c
			pop hl
			ret
			
			
get:		push hl             ; Retrieve variable
			call vcom
			ld c,(hl)
			pop hl
			ret
			
			
vcom:		ld a,(hl)
			sub 0x31           ; form a decimal number
			cp 0x09
			jp nc,error
			ld d,a
			ld hl,vbase        ; index into variable table
			add a,l
			ld l,a
			ret
			
			
user:		jp error
ci:			call getchar		; call getchar
			and 0x7f
			ld c,a
co:			call putchar		; call putchar
			ld a,c
			ret
			
			.ORG 0x8200

radr:		nop         ; 2 bytes to hold return address
			nop
			
vbase:		nop			; 9 bytes for user variables
			nop			
			nop
			nop
			nop
			nop
			nop
			nop
			nop
			
stk:		nop			; 10 bytes for stack		
			nop
			nop
			nop
			nop
			nop
			nop
			nop
			nop
			nop
			
uprog:		nop         ; User Program starts here
			nop
			nop
			nop


                        .ORG $9000
                         
; **********************************************************************
; **  Device Driver                             by Stephen C Cousins  **
; **  Hardware:  RC2014                                               **
; **  Interface: Serial 6850 ACIA                                     **
; **********************************************************************

; This module is the driver for the RC2014 serial I/O interface which is
; based on the 6850 Asynchronous Communications Interface Adapter (ACIA)
;
; Base addresses for ACIA externally defined. eg:
kACIA1:    .EQU 0x80           ;Base address of serial ACIA #1
kACIA2:    .EQU 0x80           ;Base address of serial ACIA #2
;
; RC2014 addresses for 68B50 number 2:
; 0x40   Control registers (read and write)
; 0x41   Data registers (read and write)
;
; 6850 #1 registers derived from base address (above)
kACIA1Cont: .EQU kACIA1+0       ;I/O address of control register
kACIA1Data: .EQU kACIA1+1       ;I/O address of data register
; 6850 #2 registers derived from base address (above)
kACIA2Cont: .EQU kACIA2+0       ;I/O address of control register
kACIA2Data: .EQU kACIA2+1       ;I/O address of data register

; Control register values
k6850Reset: .EQU 0b00000011     ;Master reset
k6850Init:  .EQU 0b00010110     ;No int, RTS low, 8+1, /64

; Status (control) register bit numbers
k6850RxRdy: .EQU 0              ;Receive data available bit number
k6850TxRdy: .EQU 1              ;Transmit data empty bit number

; Device detection, test 1
; This test just reads from the devices' status (control) register
; and looks for register bits in known states:
; /CTS input bit = low
; /DCD input bit = low
; WARNING
; Sometimes at power up the Tx data reg empty bit is zero, but
; recovers after device initialised. So test 1 excludes this bit.
k6850Mask1: .EQU  0b00001100    ;Mask for known bits in control reg
k6850Test1: .EQU  0b00000000    ;Test value following masking

; Device detection, test 2
; This test just reads from the devices' status (control) register
; and looks for register bits in known states:
; /CTS input bit = low
; /DCD input bit = low
; Transmit data register empty bit = high
k6850Mask2: .EQU  0b00001110    ;Mask for known bits in control reg
k6850Test2: .EQU  0b00000010    ;Test value following masking

; RC2014 serial 6850 initialise
;   On entry: No parameters required
;   On exit:  Z flagged if device is found and initialised
;             AF BC DE HL not specified
;             IX IY I AF' BC' DE' HL' preserved
; If the device is found it is initialised
serial_init:
; First look to see if the device is present
; Test 1, just read from chip, do not write anything
            IN   A,(kACIA1Cont) ;Read status (control) register
            AND  k6850Mask1     ;Mask for known bits in control reg
            CP   k6850Test1     ;and check for known values
            RET  NZ             ;If not found return with NZ flag
; Attempt to initialise the chip
            LD   A,k6850Reset   ;Master reset
            OUT  (kACIA1Cont),A ;Write to ACIA control register
            LD   A,k6850Init    ;No int, RTS low, 8+1, /64
            OUT  (kACIA1Cont),A ;Write to ACIA control register
; Test 2, perform tests on chip following initialisation
            IN   A,(kACIA1Cont) ;Read status (control) register
            AND  k6850Mask2     ;Mask for known bits in control reg
            CP   k6850Test2     ;Test value following masking
;           RET  NZ             ;Return not found NZ flagged
            RET                 ;Return Z if found, NZ if not


; RC2014 serial 6850 input character
;   On entry: No parameters required
;   On exit:  A = Character input from the device
;             NZ flagged if character input
;             BC DE HL IX IY I AF' BC' DE' HL' preserved
; This function does not return until a character is available
getchar:
            IN   A,(kACIA1Cont) ;Address of status register
            AND  $01            ;Receive byte available
            JR   Z, getchar     ;Return Z if no character
            IN   A,(kACIA1Data) ;Read data byte
            RET                 ;NZ flagged if character input


; RC2014 serial 6850 output character
; On entry: A = Character to be output to the device
; On exit:  If character output successful (eg. device was ready)
; NZ flagged and A != 0
; If character output failed (eg. device busy)
; Z flagged and A = Character to output
; BC DE HL IX IY I AF' BC' DE' HL' preserved

putchar:
            LD   A,C
            PUSH BC
            LD   C,kACIA1Cont   ;ACIA control register
            IN   B,(C)          ;Read ACIA control register
            BIT  k6850TxRdy,B   ;Transmit register full?
            POP  BC
            JR  Z, putchar      ;Return Z as character not output
            OUT  (kACIA1Data),A ;Write data byte
            OR   0xFF           ;Return success A=0xFF and NZ flagged
            RET
