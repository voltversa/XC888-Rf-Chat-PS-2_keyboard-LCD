

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

              mov     channel,#04ch           ;kanaal kiezen           
            lcall initnrfe
            mov a,#80h
            lcall COMNWRT



            mov     address,#0e7h           ;adres data pijp 0
   lus:            lcall   standbytorx             ;rx mode
   main1:          lcall   done                    ;wait for irq pin
                   jnc     main1
                   mov a,#'Y'
                  lcall DATAWRT
                lcall rxtostandby ;reset standby mode and irq flags
                 lcall rxfifoempty ;rx fifo to buffer
                 lcall poprxbuffer ;clear buffer
              
            
             lcall DATAWRT
             ljmp lus
                            
                   
                       
      
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
                      ; 1 = Input, 0 = Output
                      ; Bits: 7 6 5 4 3 2 1 0
                      ; CE  CSN IRQ MOSI SCK - - MISO
                      ; 1   1   0   1   1   x x  0
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


standbytorx: 
push acc
push b
push 07h                     ;place regs on stack
mov a,#000h                  ;Read CONFIG register (000AAAAA or 0x00 + 0x00), adjust prim_rx and pwr_up bits
lcall spiread2b              ;received 2 bytes
orl a,#003h                  ;pwr_up = 1, prim_rx = 1 (rx mode)
mov b,a                      ;b = data (lsbyte)
mov a,#00h                  ;write to CONFIG register (001AAAAA or 0x00 + 0x20)
lcall spiwrite2b             ;write 2 bytes
mov a,#07h    ;clear irq flags by writing to the status register (001AAAAA or 0x07 + 0x20)
mov b,#070h               ;clear tx_ds, max_rt and rx_dr by writing 1
lcall spiwrite2b             ;send 2 bytes
clr csn
mov a,#0ah              ;write to the rx_addr_p0 register (001AAAAA or 0x0a + 0x20)
lcall spioutbyte            ;send byte
mov r7,#005h             ;counter: addresses are 5 bytes long (use ADDRESS 5 times, e.g. ADDRESS = 0xe7 then rx_addr_p0 = 0xe7e7e7e7e7)
standbytorx1:    
mov a,address                   ;load address
lcall spioutbyte             ;send address
djnz r7,standbytorx1         ;decrease counter
setb csn                     ;csn high, end of transaction
;tx/rx clear fifos
mov a,#0e1h                  ;flush_tx
lcall spiwrite1b             ;byte write
mov a,#0e2h                  ;flush_rx
lcall spiwrite1b             ;byte write
setb ce                      ;ce high, standby-1 to rx mode
pop 07h                       ;restore regs
pop b
pop acc
ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;
; rxtostandby brings the CE layer, which puts the chip in standby-1 mode.
; Checks whether a package has been received. (rx_dr flag)
; Clears all IRQ flags. (can also be used to go from tx to idle... then just ignore the carry...)
; input: -
; output: carry (0=NO packet received, 1=Packet received)
; used: -
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
rxtostandby:     
push b
push acc                      ;save regs
clr ce                        ;ce low, rx to standby-1
mov a,#0ffh                   ;SNOP command (no operation)
lcall spiread1b               ;status byte received, check irq flags
mov a,r0
jnb rx_dr,rxtostandby1        ;rx_dr flag? (i.e. received package?)
setb c                        ;yes: carry = 1
ljmp rxtostandby2             ;clear flags
rxtostandby1:    clr c                         ;no: carry = 0
rxtostandby2:    mov a,#07h                   ;STATUS register (irq pin reset)
mov b,#070h                   ;clear tx_ds, max_rt and rx_dr by writing 1
lcall spiwrite2b              ;write 2 bytes
pop acc                       ;restore regs
pop b
ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;
; rxfifoempty will extract the payload from the rx fifo of the nRF24L01p and insert it into the rx buffer of the mcu.
; The length of a packet is determined with the r_rx_pl_wid command (0x60).
; input: -
; output: - (rx buffer)
; used: no registers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;
rxfifoempty:   
push acc
push psw
push b
push 07h                ;place regs on stack
mov a,#060h            ;determine the length of the packet (in the rx fifo) using the r_rx_pl_wid command (0x60)
lcall spiread2b          ;read 2 bytes
mov r7,a                 ;load counter with the length of the packet
clr csn                  ;csn low, start transaction
mov a,#061h              ;r_rx_payload command (0x61) (read rx payload)
lcall spioutbyte         ;send msbyte
rxfifoempty1:    
mov a,#0ffh               ;dummy byte
lcall spioutbyte          ;byte received
lcall pushrxbuffer        ;store byte in rx buffer
djnz r7,rxfifoempty1      ;decrease counter
pop 07h                   ;restore regs
pop b
pop psw
pop acc
ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;
; poprxbuffer will pop 1 byte from the rx buffer and decrement the rx pointer (pre decrement).
; The buffer is of the lifo (last in first out) type. The first byte popped is the last one inserted into the buffer (the msbyte of the packet).
; ATTENTION!!!!!!!!!!!!!!!
; 1) The buffer is a lifo (last in first out), the msbyte of the packet will be pushed first and popped last.
; 2) The buffer is not protected against over/underflows. Pushing and popping is done at your own responsibility. (just like with the stack)
; rx pointer is at location 48H
; the buffer runs from 65H to 79H
; ex: loop: mov a,rxpointer ;load pointer into battery
; cjne a,#rxbufferaddr,lus2 ;does the pointer point to the start address of the buffer? (i.e. is the buffer empty?)
; ljmp end ;end
; loop2: lcall poprxbuffer ;pop 1 byte from the buffer
; mov p3_data,a ;display this byte on port 3 (LEDs)
; ljmp loop ;again
; end:  ...
; input: -
; output: battery
; used: rxpointer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;
poprxbuffer:    
push 01h                               ;place r1 on the stack
dec rxpointer                          ;decrease pointer
mov r1,rxpointer             ;load the current location, where tx pointer points, into r1
mov a,@r1                           ;write the value of that location to the battery
pop 01h                             ;r1 restore
ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;
; done checks whether the IRQ pin has been triggered.
; ATTENTION!!!!!!!!!!!!!
; 1) The IRQ pin is active low.
; 2) The IRQ flags are not cleared automatically. As long as the IRQ pin remains triggered, no new packets can be received or sent.
; The cause can be checked in the status register (or status byte). This register contains 3 flags (rx_dr, tx_ds and max_rt).
; Three possible causes are:
; A package was received.
; A package sent (and ack received).
; The maximum number of times a packet may be resent has been reached.
; Reset the IRQ pin by writing a '1' to the appropriate flag in the status register.
; ex1: loop: lcall done ;wait indefinitely until the irq is triggered
; jnc loop
; ex2: mov r7,#10 ;use counter to wait a certain time
; loop: djnz r7,loop2
; ljmp end
; loop2: lcall done
; jnc loop
; end: ... ;reset irq flags
; input: -
; output: carry (0=irq is NOT triggered, 1=irq IS triggered)
; used: -
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;
done:          
push b                ;place regs on stack
jb irq,done1          ;is the irq pin triggered? (active low)
setb c                ;yes: carry = 1
ljmp done2            ;end
done1:           clr c                  ;no: carry = 0
done2:           pop b                  ;restore regs
ret

pushrxbuffer: 
push 01h                ;place r1 on the stack
mov r1,rxpointer        ;load the current location, where tx pointer points, into r1
mov @r1,a               ;write the value of the battery at that location
inc rxpointer           ;increase pointer
pop 01h                 ;r1 restore
ret

    
#include	"c:\xcez1.inc"
