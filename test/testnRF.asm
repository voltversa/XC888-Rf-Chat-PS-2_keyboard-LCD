;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Written by VoltVersa (VV) ;;;;;
;;;;;;;;  May 2023 ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; include file xcez1 by Mr.Roggemans M. (MGM) ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                              

       ;Program for LCD intefacing to 8051 Microcontroller
RS      bit     p4_data.5    ; Register select pin
e      bit      p4_data.6    ; Enable pin

entrymode     EQU    00000110B            ;CURSOR NAAR RECHTS, SCHERM VAST
displayon     EQU    00000001B            ;HOMEN EN CLEAR CURSOR
displayof     EQU    00001000B            ;DISPLAY OFF, CURSOR OFF
cursoroff     EQU    00001100B            ;DISPLAY ON ZONDER CURSOR
cursoronb     EQU    00001111B            ;CURSOR ON AND BLINK
cursoronn     EQU    00001110B            ;CURSOR ON NO BLINK
functions     EQU    00111000B            ;INTERFACE LENGTE EN KARAKTER FONT
cgram         EQU    01000000B            ;SELECTIE KARAKTER GENERATOR RAM
ddram         EQU    10000000B            ;SELECTIE DATA DISPLAY RAM

lcdport       EQU    p3_data                   


            
          

                        KBD_DATA  bit P1_data.4

                        KBD_CLK   bit P1_data.3     

 
                          ascii equ 10h
                          TEMPVAR equ 11h
                          count equ 12h

                          CE bit p0_data.7
                         CSN bit p0_data.6
                          IRQ bit p0_data.5
                          MOSI bit p0_data.4
                          SCK bit p0_data.3
                         MISO bit p0_data.0
                                
rx_dr bit acc.6               ;rx_dr flag, bit 6 of the status register
tx_ds bit acc.5               ;tx_ds flag, bit 5 of the status register
max_rt bit acc.4              ;max_rt flag, bit 4 of the status register

channel equ 046h               ;channel
address equ 047h               ;device address

rxpointer equ 048h              ;pointer to rx buffer
txpointer equ 049h              ;pointer to tx buffer
txbufferaddr equ 050h           ;start address txbuffer
rxbufferaddr equ 065h            ;start address rxbuffer



             org 0000h
            mov     sp,#7fh
          
            lcall initlcd
            lcall initp4
            lcall INIT
            lcall initp1
            lcall initp0
             mov a,#80h
            lcall COMNWRT
           
            mov channel,#04ch ;select channel
           
            lcall initnrfe
             mov address,#0e7h ;choose address (e.g.: 0xe7e7e7e7e7)
              
         loop:   mov a,#'A'
                 lcall pushtxbuffer ;fill buffer
                 lcall txfifofill ;buffer to tx fifo
                  mov a,#'S' ;successfully sent (and ack received)
                 lcall DATAWRT
                 lcall sendpayloade ;send packet (enhanced shockburst)
                  
                 jc main1
                 mov a,#'S' ;successfully sent (and ack received)
                 lcall DATAWRT
                 ljmp eend ;end
main1: 
                  mov a,#'N' ;not successfully sent 
                 lcall DATAWRT
                 lcall flushtxfifo ;tx fifo clear
eend:            ljmp loop ;retry


           
                  
                   
                      

                   
                        
                   
                       
      
initp1:      push   syscon0              ;juiste map selecteren
              mov    syscon0,#004h
              push   port_page            ;tijdelijk bewaren zodat we dat kunnen herstellen
              mov port_page,#001h ;select port page 1
              mov p1_pudsel,#0ffh ;select pull_up device
              mov p1_puden,#0ffh ;enable selection
              mov    port_page,#000h      ;selecteer poort page 0
              mov    p1_dir,#00h         ;poort 3 als output schakelen
              pop    port_page            ;herstellen in oorspronkelijke staat
              pop    syscon0              ;pagina terug herstellen
              ret
         

;----------------------------------------------
INIT:	
   
    lcall delay10us

    MOV A,#functions       ; initialize LCD
	ACALL COMNWRT
    lcall delay2ms  
   
	MOV A,#cursoronb        ;display on, cursor on
	ACALL COMNWRT 
    lcall delay2ms  

	MOV A,#displayon          ;clear LCD
	ACALL COMNWRT 
	lcall delay2ms  
 
	MOV A,#entrymode         ;Entry mode
    ACALL COMNWRT 
	lcall delay2ms  
    
	MOV A,#80H 
	ACALL COMNWRT
	RET

COMNWRT: 		;send command to LCD
	CLR RS 		;RS=0 for command

	SETB e 	;E=1 for high pulse
	MOV lcdport,A 	;copy reg A to port 1
	lcall DELAY
	CLR e 		;E=0 for H-to-L pulse
	RET

DATAWRT: 		;write data to LCD

	SETB RS 	;RS=1 for data
	SETB e 	;E=1 for high pulse
    MOV lcdport,A 	;copy reg A to port 1
    lcall DELAY
	CLR e
	RET

;-----------delay subroutine 12.75 milliseconds--------------------	
DELAY: 	MOV R3,#50
DEL2: 	MOV R4,#255
DEL: 	DJNZ R4,DEL
	DJNZ R3,DEL2
	RET




initlcd:
               push syscon0                  ;set folder correctly
               mov syscon0,#004h                 ;select mainddir
               push port_page                          ;save port page
               mov port_page,#000h                            ;select home page
               orl p3_dir,#0ffh                          ;set port pin as output
               pop    port_page            ;pagina herstellen
               pop    syscon0              ;map herstellen
               ret



initp4:     push   syscon0              ;juiste map selecteren
              mov    syscon0,#004h
              push   port_page            ;tijdelijk bewaren zodat we dat kunnen herstellen
              mov    port_page,#000h      ;selecteer poort page 0
              mov    p4_dir,#0ffh         ;poort 3 als output schakelen
              pop    port_page            ;herstellen in oorspronkelijke staat
              pop    syscon0              ;pagina terug herstellen
              
            ret

initp0:
                      push   syscon0              ;juiste map selecteren
                      mov    syscon0,#004h
                      push   port_page
                      mov     port_page,#001h             ;page 1 selecteren
                                          ;;;PX_PUDEN;;;
                      mov     p0_PUDEN,#0ffh            ;1 = enable, 0 = disable
                                      ;;;PX_PUDSEL;;;                     **pull up/down zorgen voor de stand bij tri-state, up = 1, down = 0**
                      mov     P0_PUDSEL,#0ffh               ;1 = pull up, 0 = pull down
                      mov     port_page,#003h             ;page 3 selecteren
                                           ;;;PX_OD;;;
                      mov     P0_OD,#000h               ;1 = OD, 0 = pull up/down
                      mov    port_page,#000h     
                      ; Configure Port 0 Direction
                      ; 0 = Input, 1 = Output
                      ; Bits: 7 6 5 4 3 2 1 0
                      ; CE  CSN IRQ MOSI SCK - - MISO
                     
                      mov    p0_dir,#11011110b  ; in HEX : DE
                      clr   SCK
                      setb   CSN
                      pop    port_page           
                      pop    syscon0            
                      
                      ret



               ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

delaya0k05s:  push   acc                  ;registers bewaren
              push   psw
              push   b
delaya0k05s2: mov    b,#050               ;loopcounter
delaya0k05s1: lcall  delay1ms
              djnz   b,delaya0k05s1
              djnz   acc,delaya0k05s2
              pop    b                    ;registers herstellen
              pop    psw
              pop    acc
              ret

delay10us:    push   acc                  ;8*42ns=330ns
              push   psw                  ;8*42ns=330ns
              mov    acc,#01ah            ;420ns
              djnz   acc,$                ;420ns*aantal keer doorlopen
              pop    psw                  ;8*42ns=330ns
              pop    acc                  ;8*42ns=330ns
              ret   
              ;165ns
delay2ms:     lcall  delay1ms
              lcall  delay1ms 
              ret

delay1ms:     push   acc                  ;8*42ns=330ns
              push   psw                  ;8*42ns=330ns
              mov    acc,#100             ;420ns
delay1ms1:    lcall  delay10us            ;10us
              djnz   acc,delay1ms1        ;420ns*aantal keer doorlopen
              pop    psw                  ;8*42ns=330ns
              pop    acc                  ;8*42ns=330ns
              ret                         ;165ns
delay50us:
    push   acc                  ; 8 cycles * 42 ns = 336 ns
    push   psw                  ; 8 cycles * 42 ns = 336 ns
    mov    acc, #11h            ; 420 ns (adjust for 50 us delay)
delay_loop:
    djnz   acc, delay_loop      ; 420 ns * aantal keer doorlopen
    pop    psw                  ; 8 cycles * 42 ns = 336 ns
    pop    acc                  ; 8 cycles * 42 ns = 336 ns
    ret                          ; Return from subroutine

                lcdoutmsga:
                outmsgalcd: 
               
                outmsgalcd1:
                                clr a                                                     ;value is in our way
                               movc a,@a+dptr                           ;read value
                               jz outmsgalcde                             ;if 0 then jump to end
                               lcall DATAWRT                             ;get rid of it
                               inc dptr                                            ;point to next ascii code
                               ljmp outmsgalcd1                     ;close loop
                
                outmsgalcde:
                           ret


         

spioutbyte: 
    push psw                 ; Save PSW to preserve carry and other flags
    push 07h                 ; Save R7, used as the bit counter

    mov r7, #08              ; Initialize loop counter for 8 bits
    nop

spioutbyte_loop:
    rlc a                    ; Rotate left the accumulator, MSB moves to carry
    mov MOSI, c              ; Set MOSI pin based on carry flag (contains MSB of A)
    setb SCK                 ; Set SCK high, data is sampled by slave on this rising edge
    mov c, MISO              ; Read MISO pin into carry, preparing it to be shifted into A
    nop                      ; Small delay 
    clr SCK                  ; Clear SCK, prepare for the next data bit
    
    rlc a                    ; Rotate left the accumulator, shifting new bit into LSB of A
    djnz r7, spioutbyte_loop ; Decrement bit counter and loop if not zero

    pop 07h                  ; Restore R7
    pop psw                  ; Restore PSW (including carry flag)
    ret         ; Return from subroutine

spiwrite1b:
    push acc                  ; Save accumulator on stack
    clr CSN                   ; CSn low, start SPI transaction
    lcall spioutbyte          ; Send byte in accumulator over SPI
    setb CSN                  ; CSn high, end SPI transaction
    pop acc                   ; Restore accumulator
    ret                       ; Return from subroutine

; spiwrite2b will send 2 bytes over the SPI interface, msbyte first.
; This routine only manipulates the CSn pin, NOT the SCLK and MOSI!
; input: accumulator (command, msbyte), B register (data, Msbyte)
; output: -
; used: no registers

spiwrite2b:
    push acc                ; Save the accumulator (msbyte) on stack
    push b                  ; Save B register (lsbyte) on stack

    clr CSN                 ; Set CSn low, start SPI transaction

    mov a, acc              ; Ensure msbyte is in accumulator
    lcall spioutbyte        ; Send the msbyte (now in accumulator)
    mov r6, a               ; Store received status byte in R6 ////////// more important status reg

    mov a,b          ; 
    lcall spioutbyte        ; Send the Msbyte
    mov r7, a               ;  received data R7

    setb CSN                ; Set CSn high, end SPI transaction

    pop b                   ; Restore B register
    pop acc                 ; Restore accumulator
    ret                     ; Return from subroutine

      ; spiread1b will receive 1 byte over the spi interface.
; This routine only manipulates the CSn pin, NOT the SCLK and MOSI!
; input/output: accumulator (command/received data)
; used: no registers directly, ensures state preservation

spiread1b:
    push acc                        ; Save the accumulator
    clr CSN                         ; CSn low, start transaction
    lcall spioutbyte                ; Send command in accumulator, receive data
    mov r0, a

    setb CSN                        ; CSn high, end of transaction
    pop acc                         ; Restore the accumulator with received data
    ret
        
; spiread2b will receive 2 bytes over the spi interface, msb first.
; This routine only manipulates the CSn pin, NOT the SCLK and MOSI!
; input: accumulator (command)
; output: accumulator (data), B register (status)
; used: no registers directly, ensures state preservation

spiread2b:
    push acc                       ; Save command to send
    clr CSN                        ; CSn low, start transaction
    lcall spioutbyte               ; Send command in accumulator, receive first byte (status)
    mov b, acc                     ; Move received status to B
    mov a, #00H                    ; Prepare a dummy byte for sending
    lcall spioutbyte               ; Send dummy byte, receive second byte (data)
    setb CSN                       ; CSn high, end of transaction
    pop acc                        ; Restore command, which is now irrelevant
    ret
    ; initnrfe will configure the NRF24L01+ module with enhanced shock burst.
; Uses the SPI interface. Settings:
; Auto retransmit delay: 250 us, count: 15
; Channel: (register 0x46)
; Air data rate: 2 Mbps, Output power: 0 dBm
; Dynamic payload: on, Enable auto ack: on, CRC: 2 bytes

initnrfe:
    push acc                    ; Save registers used
    push b
    push dph
    push dpl

    clr CE                      ; CE low, bring chip to standby mode
    
    mov a, #14h                ; Wait 1 sec for 10uF capacitor (ample time)
    lcall delaya0k05s           ; Custom delay routine, adapt name as needed
    mov r7,#96h              ;power-on: wait 100ms, I wait 150
    initnrfe1:  
    lcall delay1ms
     djnz r7,initnrfe1


    mov a, #04h                ; SETUP_RETR register
    mov b, #0Fh                ; Auto retransmit delay = 250µs, count = 15
    lcall spiwrite2b            ; Write 2 bytes to SETUP_RETR

    mov a, #05h                ; RF_CH register
    mov b, #CHANNEL             ; Load channel number, set earlier in register 0x46
    anl b, #7Fh                ; Ensure only lower 7 bits are used
    lcall spiwrite2b            ; Write channel number

    mov a, #06h                ; RF_SETUP register
    mov b, #0Eh                ; Data rate = 2 Mbps, output power = 0 dBm
    lcall spiwrite2b            ; Write 2 bytes to RF_SETUP

    mov a, #11h                ; RX_PW_P0 register
    mov b, #01h                ; 1 byte payload
    lcall spiwrite2b            ; Write 2 bytes to RX_PW_P0

    mov a, #1Ch                ; DYNPD register
    mov b, #3Fh                ; Enable DYNPD for pipes 0-5
    lcall spiwrite2b            ; Write 2 bytes to DYNPD

    mov a, #1Dh                ; FEATURE register
    mov b, #04h                ; EN_DPL = 1
    lcall spiwrite2b            ; Write 2 bytes to FEATURE

    mov a, #00h                ; CONFIG register
    mov b, #0Eh                ; EN_CRC = 1, CRCO = 1 (2 bytes), PWR_UP = 1
    lcall spiwrite2b            ; Write 2 bytes to CONFIG

    mov r7, #3                  ; Short delay to allow settings to take effect
    initnrfe_delay:  
    lcall delay1ms
    djnz r7, initnrfe_delay

    ; Flush the TX and RX FIFOs
    mov a, #E1h                ; FLUSH_TX command
    lcall spiwrite1b
    mov a, #E2h                ; FLUSH_RX command
    lcall spiwrite1b

    pop dpl                     ; Restore registers
    pop dph
    pop b
    pop acc
    ret
; initnrf will configure the NRF24L01+ module with basic ShockBurst.
; Uses the SPI interface. Settings:
; Auto retransmit: off
; Channel: configured separately (register 0x46)
; Air data rate: 2 Mbps, Output power: 0 dBm
; Dynamic payload: off, Payload size: 1 byte
; Enable auto ack: off, CRC: 2 bytes

initnrf:
    push acc                    ; Save registers
    push b
    push 07h                    ; Including R7

    clr CE                      ; CE low, module to standby-1
    mov a, #014h                ; Wait 1 sec for 10uF capacitor (ample time)
    lcall delaya0k05s           ; Call your delay routine

    mov r7, #096h               ; Additional 150ms wait
initnrf1:
    lcall delay1ms              ; 1ms delay loop
    djnz r7, initnrf1           ; Decrement and loop if not zero

    mov a, #024h                ; SETUP_RETR register
    mov b, #000h                ; Auto retransmit count = 0 (off)
    lcall spiwrite2b            ; Write the setup

    mov a, #025h                ; RF_CH register
    mov b, channel              ; Set channel from external 'channel' variable
    clr b.7                     ; Ensure only lower 7 bits are used
    lcall spiwrite2b            ; Write channel setup

    mov a, #026h                ; RF_SETUP register
    mov b, #00eh                ; Data rate = 2 Mbps, output power = 0 dBm
    lcall spiwrite2b            ; Apply RF setup

    mov a, #031h                ; RX_PW_P0 register
    mov b, #001h                ; Payload length for data pipe 0 = 1 byte
    lcall spiwrite2b            ; Configure payload length

    mov a, #021h                ; EN_AA register
    mov b, #000h                ; Auto ack for all pipes = 0 (disabled)
    lcall spiwrite2b            ; Disable auto ack

    mov a, #020h                ; CONFIG register
    mov b, #00eh                ; Enable CRC, 2-byte CRC, power up
    lcall spiwrite2b            ; Write configuration

    mov r7, #003h               ; Startup delay about 1.5 ms
initnrf2:
    lcall delay1ms              ; Short delay loop
    djnz r7, initnrf2           ; Decrement and loop

    ; Clear the TX and RX FIFOs
    mov a, #0E1h                ; FLUSH_TX command
    lcall spiwrite1b            ; Flush TX FIFO
    mov a, #0E2h                ; FLUSH_RX command
    lcall spiwrite1b            ; Flush RX FIFO

    ; Set up MCU buffer pointers (for completeness in this routine)
    mov txpointer, #txbufferaddr  ; Point txpointer to start of TX buffer
    mov rxpointer, #rxbufferaddr  ; Point rxpointer to start of RX buffer

    pop 07h                     ; Restore registers
    pop b
    pop acc
    ret

; Send a packet with Enhanced ShockBurst, setting recipient's address and configuring for transmission.
; Checks transmission status and handles the module accordingly.
sendpayloade:
    push acc
    push b
    push dph
    push dpl                           ; Preserve registers and data pointers

    clr CE                             ; CE low, set NRF24L01+ to standby-1 mode
    ; Configure the CONFIG register to switch to TX mode
  
    lcall spiread1b                    ; Read current CONFIG settings
    anl a, #0FEh                       ; Clear the PRIM_RX bit to set to TX mode
    orl a, #002h                       ; Set PWR_UP bit
    mov b,a
    mov a, #00h                       ; Address of the CONFIG register
    lcall spiwrite2b
    lcall spiwrite1b                   ; Write back modified CONFIG

    ; Configure RX_ADDR_P0 to ensure auto acknowledgment works with Enhanced ShockBurst
    mov a, #0Ah                       ; Address of RX_ADDR_P0
    lcall spioutbyte                   ; Start command to write RX_ADDR_P0
    mov r7, #5                         ; Address length is 5 bytes
    clr csn                                                   
   
send_address_loop1:
    mov a, ADDRESS                     ; ADDRESS 
    lcall spioutbyte                   ; Send each byte of the address
    djnz r7, send_address_loop1
    setb csn

    ; Configure TX_ADDR for packet transmission
    clr csn                        
    mov a, #10h                       ; Address of TX_ADDR
    lcall spioutbyte                   ; Start command to write TX_ADDR
    mov r7, #5                         ; Reinitialize counter for address bytes
    
                         
send_address_loop2:
    mov a, ADDRESS
    lcall spioutbyte                   ; Send each byte of the address
    djnz r7, send_address_loop2
    setb ce                         ;ce high, start of the puls

    ; Send the packet: CE pulse to start transmission
    setb CE                            ; CE high, enable TX mode
    lcall delay10us                    ; Minimum CE pulse width of 10us
    clr CE                             ; CE low, end of pulse

    ; Wait for packet transmission or max retransmit interrupt
    sendpayloade_wait_irq:
    jb IRQ, sendpayloade_wait_irq      ; Wait for IRQ to go low (active low interrupt)
    mov a,#0ffh                        ;SNOP instruction (does nothing/ no operation)
    lcall spiread1b                    ; Read STATUS to check the interrupt source
    jnb TX_DS, packet_not_sent              ; If TX_DS is set, packet was sent
    clr c                             ; Else, set carry flag for max_rt
    sjmp finish_up                      ; Jump to cleanup and exit

packet_not_sent:
    setb c                              

finish_up:
    ; Clear interrupts and exit
    mov a, #07h                       ; STATUS register
    mov b, #070h                       ; Bits to write 1 to clear interrupts
    lcall spiwrite2b                   ; Clear the interrupt flags

    ; Restore registers and return
    pop dpl
    pop dph
    pop b
    pop acc
    ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;
; poptxbuffer will pop 1 byte from the tx buffer and decrement the tx pointer (pre decrement).
; The buffer is of the lifo (last in first out) type. The first byte popped is the last one inserted into the buffer (the msbyte of the packet).
; ATTENTION!!!!!!!!!!!!!!!
; 1) The user is responsible for the packet and the order of the bytes in the tx buffer.
; The buffer is a lifo (last in first out). Make a choice for yourself: msb first or lsb first.
; 2) The buffer is not protected against over/underflows. Pushing and popping is done at your own responsibility. (just like with the stack)
; tx pointer is at location 49H
; the buffer runs from 50H to 64H
; ex: loop: mov a,txpointer ;load pointer into battery
; cjne a,#txbufferaddr,lus2 ;does the pointer point to the starting address of the buffer? (i.e. is the buffer empty?)
; ljmp end ;end
; loop2: lcall poptxbuffer ;pop 1 byte from the buffer
; mov p3_data,a ;display this byte on port 3 (LEDs)
; ljmp loop ;again
;end:  ...
; input: -
; output: battery
; used: txpointer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;
poptxbuffer:    
push 01h                   ;place r1 on the stack
dec txpointer              ;decrease pointer (post increment)
mov r1,txpointer           ;load the current location, where tx pointer points, into r1
mov a,@r1                  ;write the value of that location to the battery
pop 01h                    ;r1 restore
ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;
; txfifofill will write the contents of the tx buffer (mcu) to the tx fifo (nRF24L01p).
; The length of a package is not important.
; ATTENTION!!!!!!!!!!!!!!!
; 1) The user is responsible for the packet and the order of the bytes in the tx buffer.
; 2) Calling this routine without first filling the buffer will lead to tx lifo underflows.
; input: - (tx buffer)
; output: -
; used: no registers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;
txfifofill: 
push acc
push b
push psw                      ;place regs on stack
clr csn                        ;csn low, start transaction
mov a,#0a0h                   ;w_tx_payload command (1-32 bytes)
lcall spioutbyte                 ;send msbyte
txfifofill1:     
lcall poptxbuffer                ;retrieve byte from buffer (lifo).
lcall spioutbyte                 ;send
mov a,txpointer                 ;load tx pointer
cjne a,#txbufferaddr,txfifofill1  ;tx pointer = buffer start address? (i.e. is the buffer empty?)
setb csn                                   ;csn high, end of transaction
pop psw                                    ;restore regs
pop b
pop acc
ret

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;
; flushtxfifo will erase the tx fifo (nRF24L01p).
; ATTENTION!!!!!!!!!!!!!!!
; The contents of tx buffer are not cleared. (see vb2)
; ex1: lcall flushtxfifo ;only clear the tx fifo
; ex2: lcall flushtxfifo ;clear tx fifo
; mov txpointer,#txbufferaddr ;tx buffer clear
; input: -
; output: -
; used: no registers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;
flushtxfifo:     
push acc               ;save battery
mov a,#0e1h            ;tx clear fifo
lcall spiwrite1b       ;send command
pop acc                ;restore battery
ret
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;
pushtxbuffer:   
push 01h                      ;place r1 on stack
mov r1,txpointer             ;load the current location, where tx pointer points, into r1
mov @r1,a                     ;write the value of the battery at that location
inc txpointer                 ;increase pointer
pop 01h                       ;r1 restore
ret

#include	"c:\xcez1.inc"


 
