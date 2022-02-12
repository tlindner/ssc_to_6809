 org $8000
 setdp 0
tdec MACRO
 orcc #1
 lda <\1
 bne @skip
 andcc #$fe
@skip deca
 sta <\1
 ENDM
tdecd MACRO
 ldd <\1-1
 pshs a
 subd #1
 std <\1-1
 orcc #1
 puls a
 tsta
 bne @skip
 andcc #$fe
@skip tst <\1-1
 ENDM
cmpa_e MACRO
 ldx <\1-1
 lda <0
 cmpa ,x
 bne @skip
 orcc #1
@skip nop
 ENDM
L0026 equ $0026
L0025 equ $0025
L0035 equ $0035
LFF79 equ $FF79
LFF7A equ $FF7A
LF7F6 equ $F7F6
RESET1 equ LF000

;**************************
; INT0 (reset) vector
;**************************
RESET
LF000
    lda #$4A            F000: 52 4A       MOV %>4A,B
    sta <1
    andcc #$fe
    clra                F002: 0D          LDSP	; Load stack pointer to $4A
    ldb <1
    tfr d,s
    lds #$00ff	; 6809 stack is in the other direction.
    lbsr LF0AE          F003: 8E F0 AE    CALL $F0AE	; Setup Peripheral file
    andcc #$af          F006: 05          EINT
LF007
    lbsr LF051          F007: 8E F0 51    CALL $F051
    lbsr LF2AF          F00A: 8E F2 AF    CALL $F2AF
    lbsr LF3F0          F00D: 8E F3 F0    CALL $F3F0
    lbra LF007          F010: E0 F5       JMP $F007

;**************************
; INT3 vector: New byte from host
;**************************
INT3

    lda <0              F012: B8          PUSH A
    pshs a
    lda <1              F013: C8          PUSH B
    pshs a
    lda <17             F014: D8 11       PUSH R17
    pshs a
    lda <16             F016: D8 10       PUSH R16
    pshs a
    lda 256+4           F018: 80 04       MOVP P4,A	; Load new data into r0
    sta <0
    andcc #$fe
    ldd #$FF00          F01A: 88 FF 00 11 MOVD %>FF00,R17
    std <17-1
    andcc #$fe
    lbsr TRAP_6         F01E: F9          TRAP 6	; Process data?
    lbcc LF026          F01F: E7 05       JL $F026	; If carry clear, set CPU ready and exit
    lda #$05            F021: A3 05 00    ANDP %>05,P0	; Disable INT3. Needs further processing in maip loop?
    anda 256+0
    sta 256+0
    andcc #$fe
    lbra LF02C          F024: E0 06       JMP $F02C	; Cleanup stack and RETI.
LF026
    lda #$7F            F026: A3 7F 08    ANDP %>7F,P8	; Clears C7
    anda 256+8
    sta 256+8
    andcc #$fe
    lda #$80            F029: A4 80 08    ORP %>80,P8	; Sets C7 (transisition indicates CPU is not busy to host?)
    ora 256+8
    sta 256+8
    andcc #$fe
LF02C
    puls a              F02C: D9 10       POP R16
    sta <16
    puls a              F02E: D9 11       POP R17
    sta <17
    puls a              F030: C9          POP B
    sta <1
    puls a              F031: B9          POP A
    sta <0
    rti                 F032: 0B          RETI

;**************************
; INT2 (Timer1) vector:
;**************************
INT2

    lda <0              F033: B8          PUSH A
    pshs a
    lda <1              F034: C8          PUSH B
    pshs a
    lda #$5             F035: 52 05       MOV %>5,B	; Set r1 == 5
    sta <1
    andcc #$fe
    lda #$10            F037: 72 10 47    MOV %>10,R71	; Set r17 == $10
    sta <71
    andcc #$fe
LF03A
    ldb <1              F03A: AA 00 26    LDA @>0026(B)	; Set r0 == [$0026 + B]
    ldx #L0026
    abx
    lda ,x
    sta <0
    tdec 0              F03D: B2          DEC A	; Set r0 == r0 - 1
    lbcs LF047          F03E: E3 07       JHS $F047	; Jump to $f047 if carry set
    lda <71             F040: 46 47 3B 06 BTJO R71,R59,$F04A
    anda <59
    lbne LF04A
    lda <71             F044: 44 47 3B    OR R71,R59
    ora <59
    sta <59
    andcc #$fe
LF047
    lda <0              F047: AB 00 26    STA @>0026(B)
    ldx #L0026
    ldb <1
    abx
    sta ,x
LF04A
    lda <71             F04A: DC 47       RR R71
    tfr a,b
    rorb
    rora
    sta <71
    dec <1              F04C: CA EC       DJNZ B,$F03A	; Decrement r1, Jump to $f03a if not 0
    lbne LF03A
    puls a              F04E: C9          POP B
    sta <1
    puls a              F04F: B9          POP A
    sta <0
    rti                 F050: 0B          RETI

LF051
    lda #$10            F051: A4 10 00    ORP %>10,P0	; Set INT3 enable
    ora 256+0
    sta 256+0
    andcc #$fe
    lda <9              F054: 4D 08 09    CMP R8,R9
    cmpa <8
    lbeq LF0AD          F057: E2 54       JEQ $F0AD	; If R8 = R9, return
    ldd #$0000          F059: 88 00 00 11 MOVD %>0000,R17	; R16 = 0. R17 = 0
    std <17-1
    andcc #$fe
    lbsr TRAP_4         F05D: FB          TRAP 4	; turn off interrupts; Process data
    lda <7              F05E: 7D AF 07    CMP %>AF,R7	; test for psg direct mode
    cmpa #$AF
    lbeq LF083          F061: E2 20       JEQ $F083
    lda <7              F063: 7D 00 07    CMP %>0,R7
    cmpa #$0
    lbeq LF06B          F066: E2 03       JEQ $F06B
    jmp LF157           F068: 8C F1 57    BR $F157
LF06B
    lda <0              F06B: 2D 00       CMP %>0,A	; Test for software reset
    cmpa #$0
    lbeq LF000          F06D: E2 91       JEQ $F000	; Jump to reset
    lda <0              F06F: 2D C7       CMP %>C7,A	; Stop all speech?
    cmpa #$C7
    lbeq LF0ED          F071: E2 7A       JEQ $F0ED
    lda <0              F073: 2D CF       CMP %>CF,A	; Stop all sound?
    cmpa #$CF
    lbeq LF0CA          F075: E2 53       JEQ $F0CA
    lda <0              F077: 2D 80       CMP %>80,A
    cmpa #$80
    lbmi LF09B          F079: E1 20       JN $F09B
    lda <0              F07B: D0 07       MOV A,R7
    sta <7
    andcc #$fe
    lda <0              F07D: 27 40 2D    BTJZ %>40,A,$F0AD	; if bit 7 of A is 0 jump to $F0AD
    coma
    anda #$40
    lbne LF0AD
    jmp LF1BF           F080: 8C F1 BF    BR $F1BF
LF083
    lda #$1             F083: 76 01 16 0C BTJO %>1,R22,$F093	; If bit 1 of R22 is 0 jump to $F093
    anda <22
    lbne LF093
    lda <0              F087: 2D FF       CMP %>FF,A
    cmpa #$FF
    lbne LF08E          F089: E6 03       JNZ $F08E
    jmp LF1AF           F08B: 8C F1 AF    BR $F1AF
LF08E
    lda <0              F08E: D0 3F       MOV A,R63
    sta <63
    andcc #$fe
    com <22             F090: D4 16       INV R22	; 1s complements
    rts                 F092: 0A          RETS
LF093
    lda <0              F093: D0 3C       MOV A,R60
    sta <60
    andcc #$fe
    lda <63             F095: 12 3F       MOV R63,A
    sta <0
    andcc #$fe
    lbsr TRAP_7         F097: F8          TRAP 7	; Load PSG, A = Address, R60 = data
    clr <22             F098: D5 16       CLR R22
    rts                 F09A: 0A          RETS
LF09B
    lda <0              F09B: 2D 0D       CMP %>D,A	; Look for carriage return
    cmpa #$D
    lbne LF0A8          F09D: E6 09       JNZ $F0A8
    lda #$20            F09F: 22 20       MOV %>20,A	; Jump if not zero
    sta <0
    andcc #$fe
    ldd #$FF02          F0A1: 88 FF 02 11 MOVD %>FF02,R17
    std <17-1
    andcc #$fe
    lbsr TRAP_4         F0A5: FB          TRAP 4	; R16 = $FF, R17 = $02
    lda #$D             F0A6: 22 0D       MOV %>D,A	; turn off interrupts; Process data
    sta <0
    andcc #$fe
LF0A8
    ldd #$FF02          F0A8: 88 FF 02 11 MOVD %>FF02,R17
    std <17-1
    andcc #$fe
    lbsr TRAP_4         F0AC: FB          TRAP 4	; R16 = $FF, R17 = $02
LF0AD
    rts                 F0AD: 0A          RETS	; turn off interrupts; Process data

;**************************
; Subroutine: Setup peripheral file
;**************************
LF0AE
    lda #$3C            F0AE: A2 30 00    MOVP %>3C,P0	; Memory mode: Single Chip,
    sta 256+0
    andcc #$fe
***** 	; INT3: Enable, clear (host data)
***** 	; INT2: Enable, clear (timer)
***** 	; INT1: Diable (allo load)
    lda #$7F            F0B1: A2 7F 08    MOVP %>7F,P8	; Write $7f to C data port
    sta 256+8
    andcc #$fe
***** 	; Select nothing, host not busy.
    lda #$FF            F0B4: A2 FF 09    MOVP %>FF,P9	; Port C DDR: All output
    sta 256+9
    andcc #$fe
    lda #$FF            F0B7: A2 FF 08    MOVP %>FF,P8	; Write $ff to C data port
    sta 256+8
    andcc #$fe
***** 	; Select nothing, host busy
    lda #$FF            F0BA: A2 FF 02    MOVP %>FF,P2	; Load timer 1 reload register with $ff
    sta 256+2
    andcc #$fe
    lda #$9F            F0BD: A2 9F 03    MOVP %>9F,P3	; Reload prescaler & decrementer & begin decrememnting
    sta 256+3
    andcc #$fe
***** 	; Timer source: internal clock
***** 	; Prescale reload reg: $1f
    lbsr LF1AF          F0C0: 8E F1 AF    CALL $F1AF
    clr <8              F0C3: D5 08       CLR R8
    clr <9              F0C5: D5 09       CLR R9
    lbsr LF0ED          F0C7: 8E F0 ED    CALL $F0ED
LF0CA
    clr <0              F0CA: B5          CLR A
    lda #$17            F0CB: 52 17       MOV %>17,B
    sta <1
    andcc #$fe
LF0CD
    lda <0              F0CD: AB 00 25    STA @>0025(B)
    ldx #L0025
    ldb <1
    abx
    sta ,x
    dec <1              F0D0: CA FB       DJNZ B,$F0CD	; B--, Jump to $F0CD is zero, status bits not changed
    lbne LF0CD
    lda #$8             F0D2: 22 08       MOV %>8,A
    sta <0
    andcc #$fe
    lbsr TRAP_8         F0D4: F7          TRAP 8	; Load PSG, A = Address, R60 = data
    lda #$9             F0D5: 22 09       MOV %>9,A
    sta <0
    andcc #$fe
    lbsr TRAP_8         F0D7: F7          TRAP 8	; Load PSG, A = Address, R60 = data
    lda #$A             F0D8: 22 0A       MOV %>A,A
    sta <0
    andcc #$fe
    lbsr TRAP_8         F0DA: F7          TRAP 8	; Load PSG, A = Address, R60 = data
    lda #$7             F0DB: 52 07       MOV %>7,B
    sta <1
    andcc #$fe
    com <0              F0DD: B4          INV A
LF0DE
    lda <0              F0DE: AB 00 35    STA @>0035(B)
    ldx #L0035
    ldb <1
    abx
    sta ,x
    dec <1              F0E1: CA FB       DJNZ B,$F0DE	; B--, Jump to $F0DE is zero, status bits not changed
    lbne LF0DE
    clr <14             F0E3: D5 0E       CLR R14
    clr <15             F0E5: D5 0F       CLR R15
    clr <67             F0E7: D5 43       CLR R67
    lda #$7             F0E9: 22 07       MOV %>7,A
    sta <0
    andcc #$fe
    lbsr TRAP_8         F0EB: F7          TRAP 8	; Load PSG, A = Address, R60 = data
    rts                 F0EC: 0A          RETS
LF0ED
    clr <10             F0ED: D5 0A       CLR R10
    clr <11             F0EF: D5 0B       CLR R11
    clr <12             F0F1: D5 0C       CLR R12
    clr <13             F0F3: D5 0D       CLR R13
    clr <73             F0F5: D5 49       CLR R73
    lda #$FF            F0F7: 72 FF 05    MOV %>FF,R5
    sta <5
    andcc #$fe
    lda #$FF            F0FA: A2 FF 0B    MOVP %>FF,P11	; D port DDR: All output
    sta 256+11
    andcc #$fe
    lda #$00            F0FD: A2 00 0A    MOVP %>00,P10	; D port: $00
    sta 256+10
    andcc #$fe
    lda #$08            F100: A2 08 08    MOVP %>08,P8	; Write $08 to C data port, Select: R, RAM, ALD, PSD
    sta 256+8
    andcc #$fe
    lda #$FF            F103: A2 FF 08    MOVP %>FF,P8	; Write $ff to C data port, Select nothing
    sta 256+8
    andcc #$fe
    rts                 F106: 0A          RETS

;**************************
; TRAP 4
;**************************
TRAP_4
    orcc #$50           F107: 06          DINT
    lbsr TRAP_6         F108: F9          TRAP 6
    andcc #$af          F109: 05          EINT
    rts                 F10A: 0A          RETS

;**************************
; SUBROUTINE: TRAP 6: Process data from host?
; Input: A = data from host
;        r16 = flags? (goes into port D data direction register)
;        r17 = flags?
;**************************
TRAP_6
    lda <16             F10B: 77 FF 10 08 BTJZ %>FF,R16,$F117	; If R17 contains any 0 bits jump to $F117
    coma
    anda #$FF
    lbne LF117
    lda <17             F10F: 7D 04 11    CMP %>4,R17	; Compute R17 minus 4
    cmpa #$4
    lbne LF117          F112: E6 03       JNZ $F117	; Jump to $f117 if r17 != 4
    lda #$03            F114: A4 03 00    ORP %>03,P0	; Enable and clear INT1
    ora 256+0
    sta 256+0
    andcc #$fe
LF117
    lda <0              F117: C0          MOV A,B	; Copy incomming data to r1
    sta <1
    andcc #$fe
    ldd #$0009          F118: 88 00 09 23 MOVD %>0009,R35	; r34 = $00, r35 = $09
    std <35-1
    andcc #$fe
    lda <17             F11C: 48 11 23    ADD R17,R35	; R35 = R35 + R17
    adda <35
    sta <35
    ldx <35-1           F11F: 9A 23       LDA *R35	; r0 = [R34:R35]
    lda ,x
    sta <0
    lda <16             F121: 77 FF 10 01 BTJZ %>FF,R16,$F126	; If R16 contains any 0 bits jump to $F126
    coma
    anda #$FF
    lbne LF126
    lda <0              F125: B3          INC A
    adda #1
    sta <0
LF126
    tdec 35             F126: D2 23       DEC R35
    cmpa_e 35           F128: 9D 23       CMPA *R35
    lbeq LF156          F12A: E2 2A       JEQ $F156	; Jump to $f156 if r0 + 1 == r35 - 1
    lda <16             F12C: 77 FF 10 04 BTJZ %>FF,R16,$F134	; If R16 contains any 0 bits jump to $F134
    coma
    anda #$FF
    lbne LF134
    lda <35             F130: D3 23       INC R35
    adda #1
    sta <35
    lbra LF137          F132: E0 03       JMP $F137
LF134
    ldx <35-1           F134: 9A 23       LDA *R35
    lda ,x
    sta <0
    lda <0              F136: B3          INC A
    adda #1
    sta <0
LF137
    lda <0              F137: 9B 23       STA *R35
    ldx <35-1
    sta ,x
    lda <0              F139: 82 06       MOVP A,P6	; Put r0 on B data port (A0-A7 of static RAM)
    sta 256+6
    andcc #$fe
    lda <17             F13B: DC 11       RR R17	; Rotate right, carry also gets bit 0
    tfr a,b
    rorb
    rora
    sta <17
    lda #$EC            F13D: 78 EC 11    ADD %>EC,R17
    adda <17
    sta <17
    lda <17             F140: 12 11       MOV R17,A
    sta <0
    andcc #$fe
    lda <0              F142: 82 08       MOVP A,P8	; Put r0 on C data port
    sta 256+8
    andcc #$fe
    lda <16             F144: 12 10       MOV R16,A
    sta <0
    andcc #$fe
    lda <0              F146: 82 0B       MOVP A,P11	; Put r16 in port D data direction reg
    sta 256+11
    andcc #$fe
    lda 256+10          F148: 80 0A       MOVP P10,A	; Read port D data (Ram?, SP0256?, AY3-8913?)
    sta <0
    andcc #$fe
    lda <16             F14A: 77 FF 10 05 BTJZ %>FF,R16,$F153	; If R16 contains any 0 bits jump to $F153
    coma
    anda #$FF
    lbne LF153
    lda <1              F14E: 92 0A       MOVP B,P10	; Write data from host to D data port (Ram?, SP0256?, A
    sta 256+10
    andcc #$fe
    lda #$F7            F150: A3 F7 08    ANDP %>F7,P8	; Set R/W* signal to W
    anda 256+8
    sta 256+8
    andcc #$fe
LF153
    lda #$FF            F153: A2 FF 08    MOVP %>FF,P8	; Disengage all control signals
    sta 256+8
    andcc #$fe
LF156
    rts                 F156: 0A          RETS
LF157
    lda <7              F157: 7D 8F 07    CMP %>8F,R7	; flag to Load timer base value?
    cmpa #$8F
    lbne LF160          F15A: E6 04       JNZ $F160
    lda <0              F15C: 82 02       MOVP A,P2	; Write timer reload register
    sta 256+2
    andcc #$fe
    lbra LF1AF          F15E: E0 4F       JMP $F1AF
LF160
    lda #$FF            F160: 72 FF 13    MOV %>FF,R19
    sta <19
    andcc #$fe
    lda #$20            F163: 76 20 07 07 BTJO %>20,R7,$F16E	; If bit 6 of R7 is 1 Jump to $F16E
    anda <7
    lbne LF16E
    lda #$8             F167: 76 08 07 05 BTJO %>8,R7,$F170	; if bit 4 of R7 is 1 jump to $F170
    anda <7
    lbne LF170
    lda #$D             F16B: 72 0D 13    MOV %>D,R19
    sta <19
    andcc #$fe
LF16E
    com <22             F16E: D4 16       INV R22	; 1s complement
LF170
    lda <22             F170: 7D 00 16    CMP %>0,R22
    cmpa #$0
    lbne LF17F          F173: E6 0A       JNZ $F17F
    lda #$4             F175: 72 04 15    MOV %>4,R21
    sta <21
    andcc #$fe
    lda <22             F178: D3 16       INC R22
    adda #1
    sta <22
    andcc #~1           F17A: B0          CLRC
    tst <0
    lbmi LF17F          F17B: E1 02       JN $F17F
    lda <21             F17D: D3 15       INC R21
    adda #1
    sta <21
LF17F
    lda <0              F17F: D0 3C       MOV A,R60
    sta <60
    andcc #$fe
    lda <7              F181: 42 07 04    MOV R7,R4
    sta <4
    andcc #$fe
    lda #$FF            F184: 72 FF 10    MOV %>FF,R16
    sta <16
    andcc #$fe
    lda <23             F187: 42 17 12    MOV R23,R18
    sta <18
    andcc #$fe
    lbsr TRAP_5         F18A: FA          TRAP 5
    lda <3              F18B: 7D 00 03    CMP %>0,R3
    cmpa #$0
    lbne LF195          F18E: E6 05       JNZ $F195
    lda <23             F190: D3 17       INC R23
    adda #1
    sta <23
    lda <4              F192: 42 04 07    MOV R4,R7	; R7 = R4
    sta <7
    andcc #$fe
LF195
    dec <21             F195: DA 15 20    DJNZ R21,$F1B8
    lbne LF1B8
    lda <0              F198: 1D 13       CMP R19,A
    cmpa <19
    lbeq LF1AF          F19A: E2 13       JEQ $F1AF
    lda <7              F19C: 77 08 07 13 BTJZ %>8,R7,$F1B3
    coma
    anda #$8
    lbne LF1B3
    lda #$2             F1A0: 72 02 15    MOV %>2,R21
    sta <21
    andcc #$fe
    lda #$20            F1A3: 76 20 07 0F BTJO %>20,R7,$F1B6
    anda <7
    lbne LF1B6
    lda <21             F1A7: D3 15       INC R21
    adda #1
    sta <21
    andcc #~1           F1A9: B0          CLRC
    tst <0
    lbmi LF1B8          F1AA: E1 0C       JN $F1B8
    lda <21             F1AC: D3 15       INC R21
    adda #1
    sta <21
    rts                 F1AE: 0A          RETS
LF1AF
    clr <7              F1AF: D5 07       CLR R7
    clr <23             F1B1: D5 17       CLR R23
LF1B3
    lda #$1             F1B3: 72 01 15    MOV %>1,R21
    sta <21
    andcc #$fe
LF1B6
    clr <22             F1B6: D5 16       CLR R22
LF1B8
    lda <7              F1B8: 77 20 07 02 BTJZ %>20,R7,$F1BE
    coma
    anda #$20
    lbne LF1BE
    clr <22             F1BC: D5 16       CLR R22
LF1BE
    rts                 F1BE: 0A          RETS
LF1BF
    lda #$1F            F1BF: 23 1F       AND %>1F,A
    anda <0
    sta <0
    andcc #$fe
    ldd #$FF02          F1C1: 88 FF 02 11 MOVD %>FF02,R17
    std <17-1
    andcc #$fe
    lda <7              F1C5: 77 20 07 04 BTJZ %>20,R7,$F1CD
    coma
    anda #$20
    lbne LF1CD
    lda #$80            F1C9: 24 80       OR %>80,A
    ora <0
    sta <0
    andcc #$fe
    lda <17             F1CB: DE 11       RL R17
    tfr a,b
    rolb
    rola
    sta <17
LF1CD
    lda <7              F1CD: 77 08 07 07 BTJZ %>8,R7,$F1D8
    coma
    anda #$8
    lbne LF1D8
    lda #$20            F1D1: 76 20 07 06 BTJO %>20,R7,$F1DB
    anda <7
    lbne LF1DB
    lda #$4             F1D5: 78 04 11    ADD %>4,R17
    adda <17
    sta <17
LF1D8
    lbsr TRAP_4         F1D8: FB          TRAP 4
    lbra LF1AF          F1D9: E0 D4       JMP $F1AF
LF1DB
    lda <7              F1DB: 42 07 04    MOV R7,R4
    sta <4
    andcc #$fe
    lda <23             F1DE: 42 17 12    MOV R23,R18
    sta <18
    andcc #$fe
    clr <16             F1E1: D5 10       CLR R16
    lbsr TRAP_5         F1E3: FA          TRAP 5
    lda <3              F1E4: 7D 00 03    CMP %>0,R3
    cmpa #$0
    lbne LF1AF          F1E7: E6 C6       JNZ $F1AF
    lda <0              F1E9: 2D FF       CMP %>FF,A
    cmpa #$FF
    lbeq LF1AF          F1EB: E2 C2       JEQ $F1AF
    lda <0              F1ED: D0 3F       MOV A,R63
    sta <63
    andcc #$fe
    lbsr TRAP_5         F1EF: FA          TRAP 5
    lda <3              F1F0: 7D 00 03    CMP %>0,R3
    cmpa #$0
    lbne LF1AF          F1F3: E6 BA       JNZ $F1AF
    lda #$2             F1F5: 78 02 17    ADD %>2,R23
    adda <23
    sta <23
    lda <4              F1F8: 42 04 07    MOV R4,R7
    sta <7
    andcc #$fe
    lda <0              F1FB: D0 3C       MOV A,R60
    sta <60
    andcc #$fe
    lda <63             F1FD: 12 3F       MOV R63,A
    sta <0
    andcc #$fe
    lbsr TRAP_7         F1FF: F8          TRAP 7	; Load PSG, A = Address, R60 = data
    lbra LF1DB          F200: E0 D9       JMP $F1DB
;**************************
; TRAP 7: Load PSG
; A = Address
; R60 = data
;**************************
TRAP_7
    orcc #$50           F202: 06          DINT
    lbsr TRAP_8         F203: F7          TRAP 8
    andcc #$af          F204: 05          EINT
    rts                 F205: 0A          RETS

;**************************
; TRAP 8: Load PSG
; A = Address
; R60 = data
;**************************
TRAP_8
    lda #$FF            F206: A2 FF 0B    MOVP %>FF,P11	; Set D port (data bus) as all output
    sta 256+11
    andcc #$fe
    lda <0              F209: 82 0A       MOVP A,P10	; Write r0 to D port (data bus)
    sta 256+10
    andcc #$fe
    lda #$BF            F20B: A2 BF 08    MOVP %>BF,P8	; select PSG, BC1 high, BDIR high (inactive)
    sta 256+8
    andcc #$fe
    lda #$B6            F20E: A2 B6 08    MOVP %>B6,P8	; Select PSG, BC1 High, BDIR low (latch address)
    sta 256+8
    andcc #$fe
    lda <60             F211: 12 3C       MOV R60,A
    sta <0
    andcc #$fe
    lda <0              F213: 82 0A       MOVP A,P10	; Write value to Data bus
    sta 256+10
    andcc #$fe
    lda #$BE            F215: A2 BE 08    MOVP %>BE,P8	; Select PSG, BC1 Low, BDIR High (latch Data)
    sta 256+8
    andcc #$fe
    lda #$FF            F218: A2 FF 08    MOVP %>FF,P8	; Deselect
    sta 256+8
    andcc #$fe
    rts                 F21B: 0A          RETS

;**************************
; INT1: Load request from SPO256-AL2
;       This interrupt is generated when the SPO256-AL2 has finished playing
;       the phonem.
;**************************
INT1

    lda <0              F21C: B8          PUSH A
    pshs a
    lda <1              F21D: C8          PUSH B
    pshs a
    lda <17             F21E: D8 11       PUSH R17
    pshs a
    lda <16             F220: D8 10       PUSH R16
    pshs a
    lda <5              F222: 7D FF 05    CMP %>FF,R5	; r5, flag to determine if there is data to send?
    cmpa #$FF
    lbne LF246          F225: E6 1F       JNZ $F246
    ldd #$0004          F227: 88 00 04 11 MOVD %>0004,R17	; Input to TRAP 6
    std <17-1
    andcc #$fe
    lbsr TRAP_6         F22B: F9          TRAP 6	; Get phonem from static ram?
    lbcs LF260          F22C: E3 32       JHS $F260	; If carry set jump to $f260 (carry set on error?)
    lda #$80            F22E: 26 80 0E    BTJO %>80,A,$F23F	; If MSB is set jump to $f23f
    anda <0
    lbne LF23F
LF231
    lda #$FF            F231: A2 FF 0B    MOVP %>FF,P11	; Set port D to output
    sta 256+11
    andcc #$fe
    lda <0              F234: 82 0A       MOVP A,P10	; Write Reg A to port D (data bus)
    sta 256+10
    andcc #$fe
    lda #$DF            F236: A2 DF 08    MOVP %>DF,P8	; Signal SPO256 a byte is available
    sta 256+8
    andcc #$fe
    lda #$FF            F239: A2 FF 08    MOVP %>FF,P8	; select no devices
    sta 256+8
    andcc #$fe
    jmp LF02C           F23C: 8C F0 2C    BR $F02C	; Restore stack and RETI
LF23F
    lda <0              F23F: D0 05       MOV A,R5	; Set "no more data" flag
    sta <5
    andcc #$fe
    clr <6              F241: D5 06       CLR R6
    jmp LF02C           F243: 8C F0 2C    BR $F02C	; Restore stack and RETI
LF246
    clr <16             F246: D5 10       CLR R16
    lda <5              F248: 42 05 04    MOV R5,R4
    sta <4
    andcc #$fe
    lda <6              F24B: 42 06 12    MOV R6,R18
    sta <18
    andcc #$fe
    lda <6              F24E: D3 06       INC R6
    adda #1
    sta <6
    lbsr TRAP_12        F250: F3          TRAP 12
    lda <3              F251: 7D 00 03    CMP %>0,R3
    cmpa #$0
    lbne LF25D          F254: E6 07       JNZ $F25D
    lda <4              F256: 42 04 05    MOV R4,R5
    sta <5
    andcc #$fe
    lda <0              F259: 2D FF       CMP %>FF,A
    cmpa #$FF
    lbne LF231          F25B: E6 D4       JNZ $F231
LF25D
    lda #$FF            F25D: 72 FF 05    MOV %>FF,R5
    sta <5
    andcc #$fe
LF260
    lda #$14            F260: A3 14 00    ANDP %>14,P0	; Clear INT1 and INT2. Enable all interrupts
    anda 256+0
    sta 256+0
    andcc #$fe
    jmp LF02C           F263: 8C F0 2C    BR $F02C	; Restore stack and RETI

;**************************
; TRAP 5:
;**************************
TRAP_5
    orcc #$50           F266: 06          DINT
    lbsr TRAP_12        F267: F3          TRAP 12
    andcc #$af          F268: 05          EINT
    rts                 F269: 0A          RETS

;**************************
; TRAP 12
;**************************
TRAP_12
    clr <3              F26A: D5 03       CLR R3
    lda <4              F26C: 32 04       MOV R4,B
    sta <1
    andcc #$fe
    lda #$F             F26E: 53 0F       AND %>F,B
    anda <1
    sta <1
    andcc #$fe
    lda #$3F            F270: 73 3F 12    AND %>3F,R18
    anda <18
    sta <18
    andcc #$fe
    lda <18             F273: 7D 3F 12    CMP %>3F,R18
    cmpa #$3F
    lbmi LF28B          F276: E1 13       JN $F28B
    lda #$10            F278: 76 10 04 30 BTJO %>10,R4,$F2AC
    anda <4
    lbne LF2AC
    lda <4              F27C: D3 04       INC R4
    adda #1
    sta <4
    lda <1              F27E: D1 21       MOV B,R33
    sta <33
    andcc #$fe
    lda <4              F280: 45 04 21    XOR R4,R33
    eora <33
    sta <33
    andcc #$fe
    lda <33             F283: 77 08 21 04 BTJZ %>8,R33,$F28B
    coma
    anda #$8
    lbne LF28B
    tdec 4              F287: D2 04       DEC R4
    lbra LF2AC          F289: E0 21       JMP $F2AC
LF28B
    lda #$40            F28B: 5C 40       MPY %>40,B
    ldb <1
    mul
    std <0
    lda #$E8            F28D: 28 E8       ADD %>E8,A
    adda <0
    sta <0
    lda <18             F28F: 38 12       ADD R18,B
    adda <1
    sta <1
    lda <1              F291: 92 06       MOVP B,P6
    sta 256+6
    andcc #$fe
    lda <0              F293: 82 08       MOVP A,P8
    sta 256+8
    andcc #$fe
    lda <18             F295: D3 12       INC R18
    adda #1
    sta <18
    lda <16             F297: 12 10       MOV R16,A
    sta <0
    andcc #$fe
    lda <0              F299: 82 0B       MOVP A,P11
    sta 256+11
    andcc #$fe
    lda 256+10          F29B: 80 0A       MOVP P10,A
    sta <0
    andcc #$fe
    lda <16             F29D: 77 FF 10 07 BTJZ %>FF,R16,$F2A8
    coma
    anda #$FF
    lbne LF2A8
    lda <60             F2A1: 12 3C       MOV R60,A
    sta <0
    andcc #$fe
    lda <0              F2A3: 82 0A       MOVP A,P10
    sta 256+10
    andcc #$fe
    lda #$F7            F2A5: A3 F7 08    ANDP %>F7,P8
    anda 256+8
    sta 256+8
    andcc #$fe
LF2A8
    lda #$FF            F2A8: A2 FF 08    MOVP %>FF,P8
    sta 256+8
    andcc #$fe
    rts                 F2AB: 0A          RETS
LF2AC
    com <3              F2AC: D4 03       INV R3
    rts                 F2AE: 0A          RETS

LF2AF
    lda #$5             F2AF: 72 05 18    MOV %>5,R24
    sta <24
    andcc #$fe
    ldd #$002B          F2B2: 88 00 2B 1A MOVD %>002B,R26
    std <26-1
    andcc #$fe
    ldd #$0035          F2B6: 88 00 35 20 MOVD %>0035,R32
    std <32-1
    andcc #$fe
    ldd #$003A          F2BA: 88 00 3A 1C MOVD %>003A,R28
    std <28-1
    andcc #$fe
    ldd #$0030          F2BE: 88 00 30 1E MOVD %>0030,R30
    std <30-1
    andcc #$fe
    lda #$10            F2C2: 72 10 46    MOV %>10,R70
    sta <70
    andcc #$fe
LF2C5
    lda <59             F2C5: 47 46 3B 43 BTJZ R70,R59,$F30C
    coma
    anda <70
    lbne LF30C
LF2C9
    lbsr LF32E          F2C9: 8E F3 2E    CALL $F32E
    lda <3              F2CC: 7D 00 03    CMP %>0,R3
    cmpa #$0
    lbeq LF2EA          F2CF: E2 19       JEQ $F2EA
    ldd #$0006          F2D1: 88 00 06 11 MOVD %>0006,R17
    std <17-1
    andcc #$fe
    ldx <32-1           F2D5: 9A 20       LDA *R32
    lda ,x
    sta <0
    lda <0              F2D7: D0 0E       MOV A,R14
    sta <14
    andcc #$fe
    lda <15             F2D9: 4D 0E 0F    CMP R14,R15
    cmpa <14
    lbeq LF30C          F2DC: E2 2E       JEQ $F30C
    lbsr TRAP_4         F2DE: FB          TRAP 4	; turn off interrupts; Process data
    lda <0              F2DF: 9B 1C       STA *R28
    ldx <28-1
    sta ,x
    clr <0              F2E1: B5          CLR A
    lda <0              F2E2: 9B 1E       STA *R30
    ldx <30-1
    sta ,x
    lda <14             F2E4: 12 0E       MOV R14,A
    sta <0
    andcc #$fe
    lda <0              F2E6: 9B 20       STA *R32
    ldx <32-1
    sta ,x
    lbra LF2C9          F2E8: E0 DF       JMP $F2C9
LF2EA
    lda <60             F2EA: 12 3C       MOV R60,A
    sta <0
    andcc #$fe
    lda <24             F2EC: 7D 04 18    CMP %>4,R24
    cmpa #$4
    lbne LF2F7          F2EF: E6 06       JNZ $F2F7
    lda #$60            F2F1: 23 60       AND %>60,A
    anda <0
    sta <0
    andcc #$fe
    lda <0              F2F3: 2D 60       CMP %>60,A
    cmpa #$60
    lbeq LF309          F2F5: E2 12       JEQ $F309
LF2F7
    lda <24             F2F7: 7D 05 18    CMP %>5,R24
    cmpa #$5
    lbne LF300          F2FA: E6 04       JNZ $F300
    lda #$80            F2FC: 76 80 3C 09 BTJO %>80,R60,$F309
    anda <60
    lbne LF309
LF300
    lda #$E0            F300: 23 E0       AND %>E0,A
    anda <0
    sta <0
    andcc #$fe
    lda <0              F302: B7          SWAP A
    tfr a,b
    rorb
    rora
    rora
    rora
    rora
    sta <0
    lda <0              F303: BC          RR A
    tfr a,b
    rorb
    rora
    sta <0
    lda <0              F304: B3          INC A
    adda #1
    sta <0
    lda <0              F305: 1D 18       CMP R24,A
    cmpa <24
    lbne LF2C9          F307: E6 C0       JNZ $F2C9
LF309
    lbsr LF36D          F309: 8E F3 6D    CALL $F36D
LF30C
    tdecd 26            F30C: DB 1A       DECD R26
    tdecd 32            F30E: DB 20       DECD R32
    tdecd 28            F310: DB 1C       DECD R28
    tdecd 30            F312: DB 1E       DECD R30
    lda <70             F314: DC 46       RR R70
    tfr a,b
    rorb
    rora
    sta <70
    dec <24             F316: DA 18 AC    DJNZ R24,$F2C5
    lbne LF2C5
    lda #$6             F319: 52 06       MOV %>6,B
    sta <1
    andcc #$fe
    ldd #$0035          F31B: 88 00 35 20 MOVD %>0035,R32
    std <32-1
    andcc #$fe
LF31F
    ldx <32-1           F31F: 9A 20       LDA *R32
    lda ,x
    sta <0
    lda <0              F321: 1A 0F       SUB R15,A
    suba <15
    sta <0
    lda <0              F323: 1D 0E       CMP R14,A
    cmpa <14
    tfr cc,a            F325: E4 02       JP $F329
    anda #$0C
    beq LF329
    lda <0              F327: D0 0E       MOV A,R14
    sta <14
    andcc #$fe
LF329
    tdecd 32            F329: DB 20       DECD R32
    dec <1              F32B: CA F2       DJNZ B,$F31F
    lbne LF31F
    rts                 F32D: 0A          RETS
LF32E
    ldx <28-1           F32E: 9A 1C       LDA *R28
    lda ,x
    sta <0
    lda <0              F330: 2D FF       CMP %>FF,A
    cmpa #$FF
    lbeq LF341          F332: E2 0D       JEQ $F341
    lda <0              F334: D0 04       MOV A,R4
    sta <4
    andcc #$fe
    ldx <30-1           F336: 9A 1E       LDA *R30
    lda ,x
    sta <0
    lda <0              F338: D0 12       MOV A,R18
    sta <18
    andcc #$fe
    clr <16             F33A: D5 10       CLR R16
    lbsr TRAP_5         F33C: FA          TRAP 5
    lda <0              F33D: 2D FF       CMP %>FF,A
    cmpa #$FF
    lbne LF344          F33F: E6 03       JNZ $F344
LF341
    lda #$FF            F341: 72 FF 03    MOV %>FF,R3
    sta <3
    andcc #$fe
LF344
    lda <3              F344: 7D 00 03    CMP %>0,R3
    cmpa #$0
    lbne LF36C          F347: E6 23       JNZ $F36C
    lda <0              F349: D0 3C       MOV A,R60
    sta <60
    andcc #$fe
    lda #$2             F34B: 72 02 40    MOV %>2,R64
    sta <64
    andcc #$fe
    lda #$80            F34E: 26 80 02    BTJO %>80,A,$F353
    anda <0
    lbne LF353
    lda <64             F351: D3 40       INC R64
    adda #1
    sta <64
LF353
    ldd #$0024          F353: 88 00 24 42 MOVD %>0024,R66
    std <66-1
    andcc #$fe
LF357
    lbsr TRAP_5         F357: FA          TRAP 5
    lda <3              F358: 7D 00 03    CMP %>0,R3
    cmpa #$0
    lbne LF36C          F35B: E6 0F       JNZ $F36C
    lda <0              F35D: 9B 42       STA *R66
    ldx <66-1
    sta ,x
    lda <66             F35F: D3 42       INC R66
    adda #1
    sta <66
    dec <64             F361: DA 40 F3    DJNZ R64,$F357
    lbne LF357
    lda <4              F364: 12 04       MOV R4,A
    sta <0
    andcc #$fe
    lda <0              F366: 9B 1C       STA *R28
    ldx <28-1
    sta ,x
    lda <18             F368: 12 12       MOV R18,A
    sta <0
    andcc #$fe
    lda <0              F36A: 9B 1E       STA *R30
    ldx <30-1
    sta ,x
LF36C
    rts                 F36C: 0A          RETS
LF36D
    lda <60             F36D: 12 3C       MOV R60,A
    sta <0
    andcc #$fe
    lda #$60            F36F: 23 60       AND %>60,A
    anda <0
    sta <0
    andcc #$fe
    lda <0              F371: 2D 60       CMP %>60,A
    cmpa #$60
    lbeq LF3BF          F373: E2 4A       JEQ $F3BF
    lda <0              F375: B7          SWAP A
    tfr a,b
    rorb
    rora
    rora
    rora
    rora
    sta <0
    lda <0              F376: BC          RR A
    tfr a,b
    rorb
    rora
    sta <0
    lda <0              F377: B3          INC A
    adda #1
    sta <0
    lda <0              F378: B8          PUSH A
    pshs a
    lda #$80            F379: 72 80 22    MOV %>80,R34
    sta <34
    andcc #$fe
    lda <60             F37C: 77 80 3C 07 BTJZ %>80,R60,$F387
    coma
    anda #$80
    lbne LF387
    lda #$4             F380: 72 04 22    MOV %>4,R34
    sta <34
    andcc #$fe
    lda #$80            F383: 76 80 24 03 BTJO %>80,R36,$F38A
    anda <36
    lbne LF38A
LF387
    lda #$7             F387: 28 07       ADD %>7,A
    adda <0
    sta <0
    lbsr TRAP_7         F389: F8          TRAP 7	; Load PSG, A = Address, R60 = data
LF38A
    puls a              F38A: B9          POP A
    sta <0
LF38B
    lda <34             F38B: DE 22       RL R34
    tfr a,b
    rolb
    rola
    sta <34
    dec <0              F38D: BA FC       DJNZ A,$F38B
    lbne LF38B
    orcc #$50           F38F: 06          DINT
    lda #$FF            F390: A2 FF 0B    MOVP %>FF,P11
    sta 256+11
    andcc #$fe
    lda #$07            F393: A2 07 0A    MOVP %>07,P10
    sta 256+10
    andcc #$fe
    lda #$BF            F396: A2 BF 08    MOVP %>BF,P8
    sta 256+8
    andcc #$fe
    lda #$B6            F399: A2 B6 08    MOVP %>B6,P8
    sta 256+8
    andcc #$fe
    lda #$00            F39C: A2 00 0B    MOVP %>00,P11
    sta 256+11
    andcc #$fe
    lda #$B7            F39F: A2 B7 08    MOVP %>B7,P8
    sta 256+8
    andcc #$fe
    lda 256+10          F3A2: 80 0A       MOVP P10,A
    sta <0
    andcc #$fe
    lda #$FF            F3A4: A2 FF 08    MOVP %>FF,P8
    sta 256+8
    andcc #$fe
    andcc #$af          F3A7: 05          EINT
    lda <60             F3A8: 32 3C       MOV R60,B
    sta <1
    andcc #$fe
    lda #$1F            F3AA: 53 1F       AND %>1F,B
    anda <1
    sta <1
    andcc #$fe
    lda <1              F3AC: 5D 00       CMP %>0,B
    cmpa #$0
    lbeq LF3B6          F3AE: E2 06       JEQ $F3B6
    com <34             F3B0: D4 22       INV R34
    lda <34             F3B2: 13 22       AND R34,A
    anda <0
    sta <0
    andcc #$fe
    lbra LF3B8          F3B4: E0 02       JMP $F3B8
LF3B6
    lda <34             F3B6: 14 22       OR R34,A
    ora <0
    sta <0
    andcc #$fe
LF3B8
    lda <0              F3B8: D0 3C       MOV A,R60
    sta <60
    andcc #$fe
    lda #$7             F3BA: 22 07       MOV %>7,A
    sta <0
    andcc #$fe
    lbsr TRAP_7         F3BC: F8          TRAP 7	; Load PSG, A = Address, R60 = data
    lbra LF3C2          F3BD: E0 03       JMP $F3C2
LF3BF
    lda #$D             F3BF: 22 0D       MOV %>D,A
    sta <0
    andcc #$fe
    lbsr TRAP_7         F3C1: F8          TRAP 7
LF3C2
    lda <24             F3C2: 32 18       MOV R24,B
    sta <1
    andcc #$fe
    ldb <1              F3C4: AA F3 EA    LDA @>F3EA(B)
    ldx #LF3EA
    abx
    lda ,x
    sta <0
    lda <0              F3C7: D0 3F       MOV A,R63
    sta <63
    andcc #$fe
    lda <36             F3C9: 42 24 3C    MOV R36,R60
    sta <60
    andcc #$fe
    lbsr TRAP_7         F3CC: F8          TRAP 7
    lda <37             F3CD: 42 25 3C    MOV R37,R60
    sta <60
    andcc #$fe
    lda <24             F3D0: 7D 05 18    CMP %>5,R24
    cmpa #$5
    lbeq LF3DD          F3D3: E2 08       JEQ $F3DD
    tdec 63             F3D5: D2 3F       DEC R63
    lda <63             F3D7: 12 3F       MOV R63,A
    sta <0
    andcc #$fe
    lbsr TRAP_7         F3D9: F8          TRAP 7
    lda <38             F3DA: 42 26 3C    MOV R38,R60
    sta <60
    andcc #$fe
LF3DD
    ldx <26-1           F3DD: 9A 1A       LDA *R26
    lda ,x
    sta <0
    lda <60             F3DF: 12 3C       MOV R60,A
    sta <0
    andcc #$fe
    lda <0              F3E1: 9B 1A       STA *R26
    ldx <26-1
    sta ,x
    com <70             F3E3: D4 46       INV R70
    lda <70             F3E5: 43 46 3B    AND R70,R59
    anda <59
    sta <59
    andcc #$fe
    com <70             F3E8: D4 46       INV R70
LF3EA
    rts                 F3EA: 0A          RETS
    fcb $01,$03,$05,$0c,$06                   .....

LF3F0
    lda <11             F3F0: 4D 0A 0B    CMP R10,R11
    cmpa <10
    lbeq LF44B          F3F3: E2 56       JEQ $F44B
    lda <12             F3F5: 12 0C       MOV R12,A
    sta <0
    andcc #$fe
    lda <0              F3F7: 1A 0D       SUB R13,A
    suba <13
    sta <0
    lbmi LF401          F3F9: E1 06       JN $F401
    lbeq LF401          F3FB: E2 04       JEQ $F401
    lda <0              F3FD: 2D 32       CMP %>32,A
    cmpa #$32
    lbmi LF44B          F3FF: E1 4A       JN $F44B
LF401
    lda #$0             F401: 72 00 21    MOV %>0,R33
    sta <33
    andcc #$fe
    ldd #$0002          F404: 88 00 02 11 MOVD %>0002,R17
    std <17-1
    andcc #$fe
    lbsr TRAP_4         F408: FB          TRAP 4	; turn off interrupts; Process data
    tdec 10             F409: D2 0A       DEC R10
    lda <0              F40B: 2D 18       CMP %>18,A
    cmpa #$18
    lbmi LF413          F40D: E1 04       JN $F413
LF40F
    lbsr LF451          F40F: 8E F4 51    CALL $F451
    rts                 F412: 0A          RETS
LF413
    lda <0              F413: 2D 0D       CMP %>D,A
    cmpa #$D
    lbeq LF40F          F415: E2 F8       JEQ $F40F
    lda #$FF            F417: 72 FF 21    MOV %>FF,R33
    sta <33
    andcc #$fe
    lda <10             F41A: D3 0A       INC R10
    adda #1
    sta <10
    lda <0              F41C: D0 20       MOV A,R32
    sta <32
    andcc #$fe
    clr <72             F41E: D5 48       CLR R72
    lda <7              F420: 7D 00 07    CMP %>0,R7
    cmpa #$0
    lbeq LF42A          F423: E2 05       JEQ $F42A
    lda #$8             F425: 76 08 07 01 BTJO %>8,R7,$F42A
    anda <7
    lbne LF42A
    rts                 F429: 0A          RETS
LF42A
    lda #$0             F42A: 72 00 10    MOV %>0,R16
    sta <16
    andcc #$fe
    lda <73             F42D: 42 49 1F    MOV R73,R31
    sta <31
    andcc #$fe
    lda <32             F430: 42 20 04    MOV R32,R4
    sta <4
    andcc #$fe
    lda <73             F433: 42 49 12    MOV R73,R18
    sta <18
    andcc #$fe
    lbsr TRAP_5         F436: FA          TRAP 5
    lda <4              F437: 42 04 20    MOV R4,R32
    sta <32
    andcc #$fe
    lda <3              F43A: 7D FF 03    CMP %>FF,R3
    cmpa #$FF
    lbeq LF44B          F43D: E2 0C       JEQ $F44B
    lda <0              F43F: 2D 0D       CMP %>D,A
    cmpa #$D
    lbeq LF44B          F441: E2 08       JEQ $F44B
    lbsr TRAP_13        F443: F2          TRAP 13
    lda <31             F444: 42 1F 49    MOV R31,R73
    sta <73
    andcc #$fe
    lda <73             F447: D3 49       INC R73
    adda #1
    sta <73
    lbra LF42A          F449: E0 DF       JMP $F42A
LF44B
    lda #$1             F44B: 72 01 48    MOV %>1,R72
    sta <72
    andcc #$fe
    clr <73             F44E: D5 49       CLR R73
    rts                 F450: 0A          RETS
LF451
    lda <11             F451: 4D 0A 0B    CMP R10,R11
    cmpa <10
    lbeq LF47E          F454: E2 28       JEQ $F47E
    lda #$1             F456: 72 01 48    MOV %>1,R72
    sta <72
    andcc #$fe
    lda <10             F459: D8 0A       PUSH R10
    pshs a
    lda <11             F45B: 42 0B 0A    MOV R11,R10
    sta <10
    andcc #$fe
    tdec 10             F45E: D2 0A       DEC R10
    ldd #$0002          F460: 88 00 02 11 MOVD %>0002,R17
    std <17-1
    andcc #$fe
    lbsr TRAP_4         F464: FB          TRAP 4	; turn off interrupts; Process data
    puls a              F465: D9 0A       POP R10
    sta <10
    lbsr TRAP_14        F467: F1          TRAP 14
    lda <25             F468: 7D 04 19    CMP %>4,R25
    cmpa #$4
    lbne LF47E          F46B: E6 11       JNZ $F47E
    ldd #$0002          F46D: 88 00 02 11 MOVD %>0002,R17
    std <17-1
    andcc #$fe
    lbsr TRAP_4         F471: FB          TRAP 4	; turn off interrupts; Process data
    lda <10             F472: 42 0A 1F    MOV R10,R31
    sta <31
    andcc #$fe
    lbsr TRAP_13        F475: F2          TRAP 13
    lda <31             F476: 42 1F 0A    MOV R31,R10
    sta <10
    andcc #$fe
    lda <25             F479: 7D 04 19    CMP %>4,R25
    cmpa #$4
    lbne LF451          F47C: E6 D3       JNZ $F451
LF47E
    clr <25             F47E: D5 19       CLR R25
    rts                 F480: 0A          RETS

;**************************
; TRAP 14
;**************************
TRAP_14
    lda <0              F481: 2D 30       CMP %>30,A
    cmpa #$30
    lbpl LF487          F483: E5 02       JPZ $F487
    lbra LF4B8          F485: E0 31       JMP $F4B8
LF487
    lda <0              F487: 2D 3A       CMP %>3A,A
    cmpa #$3A
    lbpl LF491          F489: E5 06       JPZ $F491
    ldd #$FF4B          F48B: 88 FF 4B 3E MOVD %>FF4B,R62
    std <62-1
    andcc #$fe
    lbra LF4B7          F48F: E0 26       JMP $F4B7
LF491
    lda <0              F491: 2D 41       CMP %>41,A
    cmpa #$41
    lbpl LF497          F493: E5 02       JPZ $F497
    lbra LF4B8          F495: E0 21       JMP $F4B8
LF497
    lda <0              F497: 2D 5B       CMP %>5B,A
    cmpa #$5B
    lbpl LF4A4          F499: E5 09       JPZ $F4A4
    ldd #$F83E          F49B: 88 F8 3E 3E MOVD %>F83E,R62
    std <62-1
    andcc #$fe
    lda #$1             F49F: 72 01 19    MOV %>1,R25
    sta <25
    andcc #$fe
    lbra LF4B7          F4A2: E0 13       JMP $F4B7
LF4A4
    lda <0              F4A4: 2D 61       CMP %>61,A
    cmpa #$61
    lbpl LF4AA          F4A6: E5 02       JPZ $F4AA
    lbra LF4B8          F4A8: E0 0E       JMP $F4B8
LF4AA
    lda <0              F4AA: 2D 7B       CMP %>7B,A
    cmpa #$7B
    lbpl LF4B8          F4AC: E5 0A       JPZ $F4B8
    lda <0              F4AE: 2A 20       SUB %>20,A
    suba #$20
    sta <0
    ldd #$F83E          F4B0: 88 F8 3E 3E MOVD %>F83E,R62
    std <62-1
    andcc #$fe
    lda #$1             F4B4: 72 01 19    MOV %>1,R25
    sta <25
    andcc #$fe
LF4B7
    rts                 F4B7: 0A          RETS
LF4B8
    lda #$4             F4B8: 72 04 19    MOV %>4,R25
    sta <25
    andcc #$fe
    ldd #$F811          F4BB: 88 F8 11 3E MOVD %>F811,R62
    std <62-1
    andcc #$fe
    lbra LF4B7          F4BF: E0 F6       JMP $F4B7

;**************************
; TRAP 13
;**************************
TRAP_13
    lbsr TRAP_14        F4C1: F1          TRAP 14
    lda <25             F4C2: 7D 01 19    CMP %>1,R25
    cmpa #$1
    lbne LF4D8          F4C5: E6 11       JNZ $F4D8
    clr <1              F4C7: C5          CLR B
    lda <0              F4C8: 2A 41       SUB %>41,A
    suba #$41
    sta <0
    lda #$2             F4CA: 2C 02       MPY %>2,A
    ldb <0
    mul
    std <0
    lda #$2             F4CC: 58 02       ADD %>2,B
    adda <1
    sta <1
    ldb <1              F4CE: AA FF 79    LDA @>FF79(B)
    ldx #LFF79
    abx
    lda ,x
    sta <0
    lda <0              F4D1: D0 3D       MOV A,R61
    sta <61
    andcc #$fe
    ldb <1              F4D3: AA FF 7A    LDA @>FF7A(B)
    ldx #LFF7A
    abx
    lda ,x
    sta <0
    lda <0              F4D6: D0 3E       MOV A,R62
    sta <62
    andcc #$fe
LF4D8
    lbsr LF534          F4D8: 8E F5 34    CALL $F534
LF4DB
    lbsr LF55A          F4DB: 8E F5 5A    CALL $F55A
    lbcs LF523          F4DE: E3 43       JHS $F523
    clr <30             F4E0: D5 1E       CLR R30
    lbsr TRAP_15        F4E2: F0          TRAP 15
    lbcs LF523          F4E3: E3 3E       JHS $F523
    lbsr LF79B          F4E5: 8E F7 9B    CALL $F79B
    lda #$1             F4E8: 72 01 1E    MOV %>1,R30
    sta <30
    andcc #$fe
    lda <31             F4EB: 42 1F 18    MOV R31,R24
    sta <24
    andcc #$fe
    lbsr TRAP_15        F4EE: F0          TRAP 15
    lbcs LF52E          F4EF: E3 3D       JHS $F52E
LF4F1
    lda <27             F4F1: 7D 00 1B    CMP %>0,R27
    cmpa #$0
    lbeq LF4FC          F4F4: E2 06       JEQ $F4FC
    tdec 27             F4F6: D2 1B       DEC R27
    lda <31             F4F8: D3 1F       INC R31
    adda #1
    sta <31
    lbra LF4F1          F4FA: E0 F5       JMP $F4F1
LF4FC
    lda #$2             F4FC: 52 02       MOV %>2,B
    sta <1
    andcc #$fe
    lbsr TRAP_16        F4FE: EF          TRAP 16
    ldx <62-1           F4FF: 9A 3E       LDA *R62
    lda ,x
    sta <0
    lda <0              F501: 2D FF       CMP %>FF,A
    cmpa #$FF
    lbne LF506          F503: E6 01       JNZ $F506
    rts                 F505: 0A          RETS
LF506
    clr <28             F506: D5 1C       CLR R28
    ldx <62-1           F508: 9A 3E       LDA *R62
    lda ,x
    sta <0
    lda <0              F50A: 27 80 03    BTJZ %>80,A,$F510
    coma
    anda #$80
    lbne LF510
    lda #$1             F50D: 72 01 1C    MOV %>1,R28
    sta <28
    andcc #$fe
LF510
    lda #$3F            F510: 23 3F       AND %>3F,A
    anda <0
    sta <0
    andcc #$fe
    ldd #$FF04          F512: 88 FF 04 11 MOVD %>FF04,R17
    std <17-1
    andcc #$fe
    lbsr TRAP_4         F516: FB          TRAP 4	; turn off interrupts; Process data
    lda <62             F517: D3 3E       INC R62
    adda #1
    sta <62
    lbcc LF51D          F519: E7 02       JL $F51D
    lda <61             F51B: D3 3D       INC R61
    adda #1
    sta <61
LF51D
    lda <28             F51D: 7D 01 1C    CMP %>1,R28
    cmpa #$1
    lbne LF506          F520: E6 E4       JNZ $F506
    rts                 F522: 0A          RETS
LF523
    lda <62             F523: D3 3E       INC R62
    adda #1
    sta <62
    lbcc LF529          F525: E7 02       JL $F529
    lda <61             F527: D3 3D       INC R61
    adda #1
    sta <61
LF529
    lda #$2             F529: 52 02       MOV %>2,B
    sta <1
    andcc #$fe
    lbsr TRAP_16        F52B: EF          TRAP 16
    lbra LF4DB          F52C: E0 AD       JMP $F4DB
LF52E
    lda #$3             F52E: 52 03       MOV %>3,B
    sta <1
    andcc #$fe
    lbsr TRAP_16        F530: EF          TRAP 16
    lbra LF4DB          F531: E0 A8       JMP $F4DB
    rts                 F533: 0A          RETS
LF534
    ldx <62-1           F534: 9A 3E       LDA *R62
    lda ,x
    sta <0
    lda <0              F536: 27 40 01    BTJZ %>40,A,$F53A
    coma
    anda #$40
    lbne LF53A
    rts                 F539: 0A          RETS
LF53A
    lda <62             F53A: D3 3E       INC R62
    adda #1
    sta <62
    lbcc LF540          F53C: E7 02       JL $F540
    lda <61             F53E: D3 3D       INC R61
    adda #1
    sta <61
LF540
    lbra LF534          F540: E0 F2       JMP $F534
    rts                 F542: 0A          RETS

;**************************
; TRAP 16
;**************************
TRAP_16
    clr <28             F543: D5 1C       CLR R28
LF545
    ldx <62-1           F545: 9A 3E       LDA *R62
    lda ,x
    sta <0
    lda <0              F547: 27 40 07    BTJZ %>40,A,$F551
    coma
    anda #$40
    lbne LF551
    lda <28             F54A: D3 1C       INC R28
    adda #1
    sta <28
    lda <1              F54C: 3D 1C       CMP R28,B
    cmpa <28
    tfr cc,a            F54E: E4 01       JP $F551
    anda #$0C
    beq LF551
    rts                 F550: 0A          RETS
LF551
    lda <62             F551: D3 3E       INC R62
    adda #1
    sta <62
    lbcc LF557          F553: E7 02       JL $F557
    lda <61             F555: D3 3D       INC R61
    adda #1
    sta <61
LF557
    lbra LF545          F557: E0 EC       JMP $F545
    rts                 F559: 0A          RETS
LF55A
    clr <29             F55A: D5 1D       CLR R29
    clr <27             F55C: D5 1B       CLR R27
    lda <31             F55E: 42 1F 18    MOV R31,R24
    sta <24
    andcc #$fe
    ldx <62-1           F561: 9A 3E       LDA *R62
    lda ,x
    sta <0
    lda <0              F563: 2D FF       CMP %>FF,A
    cmpa #$FF
    lbne LF569          F565: E6 02       JNZ $F569
    andcc #~1           F567: B0          CLRC
    tst <0
    rts                 F568: 0A          RETS
LF569
    lda <25             F569: 7D 01 19    CMP %>1,R25
    cmpa #$1
    lbne LF570          F56C: E6 02       JNZ $F570
LF56E
    lda <24             F56E: D3 18       INC R24
    adda #1
    sta <24
LF570
    lbsr TRAP_10        F570: F5          TRAP 10
    lda <0              F571: 2D 61       CMP %>61,A
    cmpa #$61
    lbmi LF577          F573: E1 02       JN $F577
    lda <0              F575: 2A 20       SUB %>20,A
    suba #$20
    sta <0
LF577
    lda <0              F577: 2A 20       SUB %>20,A
    suba #$20
    sta <0
    lda <0              F579: C0          MOV A,B
    sta <1
    andcc #$fe
    lda #$3F            F57A: 53 3F       AND %>3F,B
    anda <1
    sta <1
    andcc #$fe
    ldx <62-1           F57C: 9A 3E       LDA *R62
    lda ,x
    sta <0
    lda <0              F57E: 27 80 03    BTJZ %>80,A,$F584
    coma
    anda #$80
    lbne LF584
    lda #$1             F581: 72 01 1D    MOV %>1,R29
    sta <29
    andcc #$fe
LF584
    lda #$3F            F584: 23 3F       AND %>3F,A
    anda <0
    sta <0
    andcc #$fe
    lda <1              F586: 3D 00       CMP R0,B
    cmpa <0
    lbeq LF58E          F588: E2 04       JEQ $F58E
    clr <27             F58A: D5 1B       CLR R27
    orcc #5             F58C: 07          SETC
    andcc #~8
    rts                 F58D: 0A          RETS
LF58E
    lda <25             F58E: 7D 01 19    CMP %>1,R25
    cmpa #$1
    lbne LF595          F591: E6 02       JNZ $F595
    lda <27             F593: D3 1B       INC R27
    adda #1
    sta <27
LF595
    lda <29             F595: 7D 01 1D    CMP %>1,R29
    cmpa #$1
    lbne LF59C          F598: E6 02       JNZ $F59C
    andcc #~1           F59A: B0          CLRC
    tst <0
    rts                 F59B: 0A          RETS
LF59C
    lda <62             F59C: D3 3E       INC R62
    adda #1
    sta <62
    lbcc LF5A2          F59E: E7 02       JL $F5A2
    lda <61             F5A0: D3 3D       INC R61
    adda #1
    sta <61
LF5A2
    ldx <62-1           F5A2: 9A 3E       LDA *R62
    lda ,x
    sta <0
    lbra LF56E          F5A4: E0 C8       JMP $F56E
    rts                 F5A6: 0A          RETS
LF5A7
    lda <62             F5A7: D3 3E       INC R62
    adda #1
    sta <62
    lbcc LF5AD          F5A9: E7 02       JL $F5AD
    lda <61             F5AB: D3 3D       INC R61
    adda #1
    sta <61
LF5AD
    ldx <62-1           F5AD: 9A 3E       LDA *R62
    lda ,x
    sta <0
    lda <0              F5AF: 27 40 06    BTJZ %>40,A,$F5B8
    coma
    anda #$40
    lbne LF5B8
    lda #$1             F5B2: 72 01 1D    MOV %>1,R29
    sta <29
    andcc #$fe
    tdecd 62            F5B5: DB 3E       DECD R62
    rts                 F5B7: 0A          RETS
LF5B8
    clr <29             F5B8: D5 1D       CLR R29
    tdecd 62            F5BA: DB 3E       DECD R62
    rts                 F5BC: 0A          RETS

;**************************
; TRAP 9
;**************************
TRAP_9
    lda <1              F5BD: C8          PUSH B
    pshs a
    clr <1              F5BE: C5          CLR B
    lbsr TRAP_10        F5BF: F5          TRAP 10
    lda <0              F5C0: 2D 61       CMP %>61,A
    cmpa #$61
    lbmi LF5C6          F5C2: E1 02       JN $F5C6
    lda <0              F5C4: 2A 20       SUB %>20,A
    suba #$20
    sta <0
LF5C6
    lda <0              F5C6: 2A 41       SUB %>41,A
    suba #$41
    sta <0
    lda <1              F5C8: 68          ADD B,A
    adda <0
    sta <0
    lda <0              F5C9: C0          MOV A,B
    sta <1
    andcc #$fe
    lda <1              F5CA: 5D 1A       CMP %>1A,B
    cmpa #$1A
    tfr cc,a            F5CC: E4 09       JP $F5D7
    anda #$0C
    beq LF5D7
    lda <1              F5CE: 5D 00       CMP %>0,B
    cmpa #$0
    lbmi LF5D7          F5D0: E1 05       JN $F5D7
    ldb <1              F5D2: AA F7 F6    LDA @>F7F6(B)
    ldx #LF7F6
    abx
    lda ,x
    sta <0
    puls a              F5D5: C9          POP B
    sta <1
    rts                 F5D6: 0A          RETS
LF5D7
    clr <0              F5D7: B5          CLR A
    puls a              F5D8: C9          POP B
    sta <1
    rts                 F5D9: 0A          RETS
LF5DA
    lda <24             F5DA: D3 18       INC R24
    adda #1
    sta <24
    lbsr TRAP_9         F5DC: F6          TRAP 9
    lda <1              F5DD: 66 FB       BTJO B,A,$F5DA
    anda <0
    lbne LF5DA
    tdec 24             F5DF: D2 18       DEC R24
    lbsr TRAP_17        F5E1: EE          TRAP 17
    rts                 F5E2: 0A          RETS
LF5E3
    jmp LF70E           F5E3: 8C F7 0E    BR $F70E
    jmp LF720           F5E6: 8C F7 20    BR $F720
    jmp LF6CE           F5E9: 8C F6 CE    BR $F6CE
    jmp LF742           F5EC: 8C F7 42    BR $F742
    jmp LF754           F5EF: 8C F7 54    BR $F754
    jmp LF730           F5F2: 8C F7 30    BR $F730
    jmp LF736           F5F5: 8C F7 36    BR $F736
    jmp LF6B3           F5F8: 8C F6 B3    BR $F6B3
    jmp LF688           F5FB: 8C F6 88    BR $F688
    jmp LF73C           F5FE: 8C F7 3C    BR $F73C
    jmp LF672           F601: 8C F6 72    BR $F672
    jmp LF69A           F604: 8C F6 9A    BR $F69A

;**************************
; TRAP 15
;**************************
TRAP_15
LF607
    lda <30             F607: 7D 00 1E    CMP %>0,R30
    cmpa #$0
    lbne LF616          F60A: E6 0A       JNZ $F616
    lda #$40            F60C: 52 40       MOV %>40,B
    sta <1
    andcc #$fe
    lda <62             F60E: D3 3E       INC R62
    adda #1
    sta <62
    lbcc LF614          F610: E7 02       JL $F614
    lda <61             F612: D3 3D       INC R61
    adda #1
    sta <61
LF614
    lbra LF61A          F614: E0 04       JMP $F61A
LF616
    lda #$80            F616: 52 80       MOV %>80,B
    sta <1
    andcc #$fe
    tdecd 62            F618: DB 3E       DECD R62
LF61A
    ldx <62-1           F61A: 9A 3E       LDA *R62
    lda ,x
    sta <0
    lda <0              F61C: 67 11       BTJZ B,A,$F62F
    coma
    anda <1
    lbne LF62F
    lda <30             F61E: 7D 00 1E    CMP %>0,R30
    cmpa #$0
    lbne LF627          F621: E6 04       JNZ $F627
    tdecd 62            F623: DB 3E       DECD R62
    lbra LF62D          F625: E0 06       JMP $F62D
LF627
    lda <62             F627: D3 3E       INC R62
    adda #1
    sta <62
    lbcc LF62D          F629: E7 02       JL $F62D
    lda <61             F62B: D3 3D       INC R61
    adda #1
    sta <61
LF62D
    andcc #~1           F62D: B0          CLRC
    tst <0
    rts                 F62E: 0A          RETS
LF62F
    lbsr TRAP_11        F62F: F4          TRAP 11
    lbsr TRAP_10        F630: F5          TRAP 10
    lda <0              F631: 2D 61       CMP %>61,A
    cmpa #$61
    lbmi LF637          F633: E1 02       JN $F637
    lda <0              F635: 2A 20       SUB %>20,A
    suba #$20
    sta <0
LF637
    lda <0              F637: 2A 20       SUB %>20,A
    suba #$20
    sta <0
    lda <0              F639: D0 1A       MOV A,R26
    sta <26
    andcc #$fe
    lda <30             F63B: 7D 00 1E    CMP %>0,R30
    cmpa #$0
    lbeq LF64D          F63E: E2 0D       JEQ $F64D
    lda <24             F640: 7D 21 18    CMP %>21,R24
    cmpa #$21
    lbne LF64D          F643: E6 08       JNZ $F64D
    lda #$13            F645: 22 13       MOV %>13,A
    sta <0
    andcc #$fe
    cmpa_e 62           F647: 9D 3E       CMPA *R62
    lbeq LF62D          F649: E2 E2       JEQ $F62D
    orcc #5             F64B: 07          SETC
    andcc #~8
    rts                 F64C: 0A          RETS
LF64D
    ldx <62-1           F64D: 9A 3E       LDA *R62
    lda ,x
    sta <0
    lda <0              F64F: 2D 15       CMP %>15,A
    cmpa #$15
    lbmi LF666          F651: E1 13       JN $F666
LF653
    clr <29             F653: D5 1D       CLR R29
    lda <26             F655: 4D 00 1A    CMP R0,R26
    cmpa <0
    lbeq LF65C          F658: E2 02       JEQ $F65C
    orcc #5             F65A: 07          SETC
    andcc #~8
    rts                 F65B: 0A          RETS
LF65C
    lbsr TRAP_17        F65C: EE          TRAP 17
    lda <29             F65D: 7D 01 1D    CMP %>1,R29
    cmpa #$1
    lbeq LF664          F660: E2 02       JEQ $F664
LF662
    lbra LF607          F662: E0 A3       JMP $F607
LF664
    andcc #~1           F664: B0          CLRC
    tst <0
    rts                 F665: 0A          RETS
LF666
    lda <0              F666: 2D 07       CMP %>7,A
    cmpa #$7
    lbeq LF653          F668: E2 E9       JEQ $F653
    lda <0              F66A: C0          MOV A,B
    sta <1
    andcc #$fe
    lda <1              F66B: 5A 09       SUB %>9,B
    suba #$9
    sta <1
    lda #$3             F66D: 5C 03       MPY %>3,B
    ldb <1
    mul
    std <0
    ldx #LF5E3          F66F: AC F5 E3    BR @>F5E3(B)
    ldb <1
    abx
    jmp ,x
LF672
    lda <26             F672: 7D 21 1A    CMP %>21,R26
    cmpa #$21
    lbmi LF67E          F675: E1 07       JN $F67E
    lda <26             F677: 7D 3A 1A    CMP %>3A,R26
    cmpa #$3A
    tfr cc,a            F67A: E4 02       JP $F67E
    anda #$0C
    beq LF67E
    orcc #5             F67C: 07          SETC
    andcc #~8
    rts                 F67D: 0A          RETS
LF67E
    lbsr TRAP_17        F67E: EE          TRAP 17
    lda <29             F67F: 7D 01 1D    CMP %>1,R29
    cmpa #$1
    lbeq LF686          F682: E2 02       JEQ $F686
    lbra LF662          F684: E0 DC       JMP $F662
LF686
    andcc #~1           F686: B0          CLRC
    tst <0
    rts                 F687: 0A          RETS
LF688
    lbsr TRAP_9         F688: F6          TRAP 9
    lda #$8             F689: 26 08 02    BTJO %>8,A,$F68E
    anda <0
    lbne LF68E
    orcc #5             F68C: 07          SETC
    andcc #~8
    rts                 F68D: 0A          RETS
LF68E
    lda #$8             F68E: 52 08       MOV %>8,B
    sta <1
    andcc #$fe
    lbsr TRAP_18        F690: ED          TRAP 18
    lda <29             F691: 7D 01 1D    CMP %>1,R29
    cmpa #$1
    lbeq LF698          F694: E2 02       JEQ $F698
    lbra LF662          F696: E0 CA       JMP $F662
LF698
    andcc #~1           F698: B0          CLRC
    tst <0
    rts                 F699: 0A          RETS
LF69A
    lbsr TRAP_9         F69A: F6          TRAP 9
    lda #$80            F69B: 26 80 02    BTJO %>80,A,$F6A0
    anda <0
    lbne LF6A0
    orcc #5             F69E: 07          SETC
    andcc #~8
    rts                 F69F: 0A          RETS
LF6A0
    lbsr TRAP_11        F6A0: F4          TRAP 11
    lbsr TRAP_9         F6A1: F6          TRAP 9
    lda #$80            F6A2: 26 80 02    BTJO %>80,A,$F6A7
    anda <0
    lbne LF6A7
    orcc #5             F6A5: 07          SETC
    andcc #~8
    rts                 F6A6: 0A          RETS
LF6A7
    lda #$80            F6A7: 52 80       MOV %>80,B
    sta <1
    andcc #$fe
    lbsr TRAP_18        F6A9: ED          TRAP 18
    lda <29             F6AA: 7D 01 1D    CMP %>1,R29
    cmpa #$1
    lbeq LF6B1          F6AD: E2 02       JEQ $F6B1
    lbra LF662          F6AF: E0 B1       JMP $F662
LF6B1
    andcc #~1           F6B1: B0          CLRC
    tst <0
    rts                 F6B2: 0A          RETS
LF6B3
    lbsr TRAP_9         F6B3: F6          TRAP 9
    lda #$8             F6B4: 26 08 0B    BTJO %>8,A,$F6C2
    anda <0
    lbne LF6C2
    lbsr TRAP_17        F6B7: EE          TRAP 17
    lda <29             F6B8: 7D 01 1D    CMP %>1,R29
    cmpa #$1
    lbeq LF6C0          F6BB: E2 03       JEQ $F6C0
    lbsr TRAP_19        F6BD: EC          TRAP 19
LF6BE
    lbra LF662          F6BE: E0 A2       JMP $F662
LF6C0
    andcc #~1           F6C0: B0          CLRC
    tst <0
    rts                 F6C1: 0A          RETS
LF6C2
    lda #$8             F6C2: 52 08       MOV %>8,B
    sta <1
    andcc #$fe
    lbsr TRAP_18        F6C4: ED          TRAP 18
    lda <29             F6C5: 7D 01 1D    CMP %>1,R29
    cmpa #$1
    lbeq LF6CC          F6C8: E2 02       JEQ $F6CC
LF6CA
    lbra LF6BE          F6CA: E0 F2       JMP $F6BE
LF6CC
    andcc #~1           F6CC: B0          CLRC
    tst <0
    rts                 F6CD: 0A          RETS
LF6CE
    lbsr TRAP_9         F6CE: F6          TRAP 9
    lda #$1             F6CF: 26 01 15    BTJO %>1,A,$F6E7
    anda <0
    lbne LF6E7
    lbsr TRAP_10        F6D2: F5          TRAP 10
    lda <0              F6D3: 2D 49       CMP %>49,A
    cmpa #$49
    lbeq LF6D9          F6D5: E2 02       JEQ $F6D9
LF6D7
    orcc #5             F6D7: 07          SETC
    andcc #~8
    rts                 F6D8: 0A          RETS
LF6D9
    lbsr TRAP_11        F6D9: F4          TRAP 11
    lbsr TRAP_10        F6DA: F5          TRAP 10
    lda <0              F6DB: 2D 4E       CMP %>4E,A
    cmpa #$4E
    lbne LF6D7          F6DD: E6 F8       JNZ $F6D7
    lbsr TRAP_11        F6DF: F4          TRAP 11
    lbsr TRAP_10        F6E0: F5          TRAP 10
    lda <0              F6E1: 2D 47       CMP %>47,A
    cmpa #$47
    lbeq LF706          F6E3: E2 21       JEQ $F706
    orcc #5             F6E5: 07          SETC
    andcc #~8
    rts                 F6E6: 0A          RETS
LF6E7
    lbsr TRAP_11        F6E7: F4          TRAP 11
    lbsr TRAP_10        F6E8: F5          TRAP 10
    lda <0              F6E9: 2D 52       CMP %>52,A
    cmpa #$52
    lbeq LF706          F6EB: E2 19       JEQ $F706
    lda <0              F6ED: 2D 53       CMP %>53,A
    cmpa #$53
    lbeq LF706          F6EF: E2 15       JEQ $F706
    lda <0              F6F1: 2D 44       CMP %>44,A
    cmpa #$44
    lbeq LF706          F6F3: E2 11       JEQ $F706
    lda <0              F6F5: 2D 4C       CMP %>4C,A
    cmpa #$4C
    lbeq LF6FC          F6F7: E2 03       JEQ $F6FC
    lbsr TRAP_19        F6F9: EC          TRAP 19
    lbra LF706          F6FA: E0 0A       JMP $F706
LF6FC
    lbsr TRAP_11        F6FC: F4          TRAP 11
    lbsr TRAP_10        F6FD: F5          TRAP 10
    lda <0              F6FE: 2D 59       CMP %>59,A
    cmpa #$59
    lbeq LF706          F700: E2 04       JEQ $F706
    lbsr TRAP_19        F702: EC          TRAP 19
    lbsr TRAP_19        F703: EC          TRAP 19
    lbra LF726          F704: E0 20       JMP $F726
LF706
    lbsr TRAP_11        F706: F4          TRAP 11
    lbsr TRAP_10        F707: F5          TRAP 10
    lda <0              F708: 2D 20       CMP %>20,A
    cmpa #$20
    lbeq LF726          F70A: E2 1A       JEQ $F726
    orcc #5             F70C: 07          SETC
    andcc #~8
    rts                 F70D: 0A          RETS
LF70E
    lbsr TRAP_9         F70E: F6          TRAP 9
    lda #$80            F70F: 26 80 02    BTJO %>80,A,$F714
    anda <0
    lbne LF714
    orcc #5             F712: 07          SETC
    andcc #~8
    rts                 F713: 0A          RETS
LF714
    lda #$80            F714: 52 80       MOV %>80,B
    sta <1
    andcc #$fe
    lbsr TRAP_18        F716: ED          TRAP 18
    lda <29             F717: 7D 01 1D    CMP %>1,R29
    cmpa #$1
    lbeq LF71E          F71A: E2 02       JEQ $F71E
LF71C
    lbra LF6CA          F71C: E0 AC       JMP $F6CA
LF71E
    andcc #~1           F71E: B0          CLRC
    tst <0
    rts                 F71F: 0A          RETS
LF720
    lbsr TRAP_9         F720: F6          TRAP 9
    lda #$40            F721: 26 40 02    BTJO %>40,A,$F726
    anda <0
    lbne LF726
    orcc #5             F724: 07          SETC
    andcc #~8
    rts                 F725: 0A          RETS
LF726
    lbsr TRAP_17        F726: EE          TRAP 17
    lda <29             F727: 7D 01 1D    CMP %>1,R29
    cmpa #$1
    lbeq LF72E          F72A: E2 02       JEQ $F72E
    lbra LF71C          F72C: E0 EE       JMP $F71C
LF72E
    andcc #~1           F72E: B0          CLRC
    tst <0
    rts                 F72F: 0A          RETS
LF730
    lbsr TRAP_9         F730: F6          TRAP 9
    lda #$8             F731: 26 08 F2    BTJO %>8,A,$F726
    anda <0
    lbne LF726
    orcc #5             F734: 07          SETC
    andcc #~8
    rts                 F735: 0A          RETS
LF736
    lbsr TRAP_9         F736: F6          TRAP 9
    lda #$4             F737: 26 04 EC    BTJO %>4,A,$F726
    anda <0
    lbne LF726
    orcc #5             F73A: 07          SETC
    andcc #~8
    rts                 F73B: 0A          RETS
LF73C
    lbsr TRAP_9         F73C: F6          TRAP 9
    lda #$2             F73D: 26 02 E6    BTJO %>2,A,$F726
    anda <0
    lbne LF726
    orcc #5             F740: 07          SETC
    andcc #~8
    rts                 F741: 0A          RETS
LF742
    lbsr TRAP_9         F742: F6          TRAP 9
    lda #$20            F743: 26 20 02    BTJO %>20,A,$F748
    anda <0
    lbne LF748
    orcc #5             F746: 07          SETC
    andcc #~8
    rts                 F747: 0A          RETS
LF748
    lda #$20            F748: 52 20       MOV %>20,B
    sta <1
    andcc #$fe
    lbsr TRAP_18        F74A: ED          TRAP 18
    lda <29             F74B: 7D 01 1D    CMP %>1,R29
    cmpa #$1
    lbeq LF752          F74E: E2 02       JEQ $F752
LF750
    lbra LF71C          F750: E0 CA       JMP $F71C
LF752
    andcc #~1           F752: B0          CLRC
    tst <0
    rts                 F753: 0A          RETS
LF754
    lbsr TRAP_9         F754: F6          TRAP 9
    lda #$10            F755: 26 10 0F    BTJO %>10,A,$F767
    anda <0
    lbne LF767
    lbsr TRAP_10        F758: F5          TRAP 10
    lda <0              F759: 2D 23       CMP %>23,A
    cmpa #$23
    lbeq LF75F          F75B: E2 02       JEQ $F75F
    orcc #5             F75D: 07          SETC
    andcc #~8
    rts                 F75E: 0A          RETS
LF75F
    lbsr TRAP_11        F75F: F4          TRAP 11
    lbsr TRAP_10        F760: F5          TRAP 10
    lda <0              F761: 2D 48       CMP %>48,A
    cmpa #$48
    lbeq LF726          F763: E2 C1       JEQ $F726
    orcc #5             F765: 07          SETC
    andcc #~8
    rts                 F766: 0A          RETS
LF767
    lbsr TRAP_10        F767: F5          TRAP 10
    lda <0              F768: 2D 34       CMP %>34,A
    cmpa #$34
    lbeq LF770          F76A: E2 04       JEQ $F770
    lda <0              F76C: 2D 33       CMP %>33,A
    cmpa #$33
    lbne LF726          F76E: E6 B6       JNZ $F726
LF770
    lbsr TRAP_11        F770: F4          TRAP 11
    lbsr TRAP_10        F771: F5          TRAP 10
    lda <0              F772: 2D 48       CMP %>48,A
    cmpa #$48
    lbeq LF777          F774: E2 01       JEQ $F777
    lbsr TRAP_19        F776: EC          TRAP 19
LF777
    lbra LF750          F777: E0 D7       JMP $F750
    rts                 F779: 0A          RETS
    orcc #5             F77A: 07          SETC
    andcc #~8
    rts                 F77B: 0A          RETS

;**************************
; TRAP 10
;**************************
TRAP_10
    lda <1              F77C: C8          PUSH B
    pshs a
    lda <72             F77D: 7D 00 48    CMP %>0,R72
    cmpa #$0
    lbne LF78B          F780: E6 09       JNZ $F78B
    lda <32             F782: 42 20 04    MOV R32,R4
    sta <4
    andcc #$fe
    lda <24             F785: 42 18 12    MOV R24,R18
    sta <18
    andcc #$fe
    lbsr TRAP_5         F788: FA          TRAP 5
    puls a              F789: C9          POP B
    sta <1
    rts                 F78A: 0A          RETS
LF78B
    lda <10             F78B: D8 0A       PUSH R10
    pshs a
    lda <24             F78D: 42 18 0A    MOV R24,R10
    sta <10
    andcc #$fe
    tdec 10             F790: D2 0A       DEC R10
    ldd #$0002          F792: 88 00 02 11 MOVD %>0002,R17
    std <17-1
    andcc #$fe
    lbsr TRAP_4         F796: FB          TRAP 4
    puls a              F797: D9 0A       POP R10
    sta <10
    puls a              F799: C9          POP B
    sta <1
    rts                 F79A: 0A          RETS
LF79B
    ldx <62-1           F79B: 9A 3E       LDA *R62
    lda ,x
    sta <0
    lda <0              F79D: 27 40 01    BTJZ %>40,A,$F7A1
    coma
    anda #$40
    lbne LF7A1
    rts                 F7A0: 0A          RETS
LF7A1
    tdecd 62            F7A1: DB 3E       DECD R62
    lbra LF79B          F7A3: E0 F6       JMP $F79B
    rts                 F7A5: 0A          RETS

;**************************
; TRAP 17
;**************************
TRAP_17
    lda <30             F7A6: 7D 00 1E    CMP %>0,R30
    cmpa #$0
    lbne LF7AF          F7A9: E6 04       JNZ $F7AF
    lbsr LF5A7          F7AB: 8E F5 A7    CALL $F5A7
    rts                 F7AE: 0A          RETS
LF7AF
    tdecd 62            F7AF: DB 3E       DECD R62
    ldx <62-1           F7B1: 9A 3E       LDA *R62
    lda ,x
    sta <0
    lda <0              F7B3: 27 80 0A    BTJZ %>80,A,$F7C0
    coma
    anda #$80
    lbne LF7C0
    lda #$1             F7B6: 72 01 1D    MOV %>1,R29
    sta <29
    andcc #$fe
    lda <62             F7B9: D3 3E       INC R62
    adda #1
    sta <62
    lbcc LF7BF          F7BB: E7 02       JL $F7BF
    lda <61             F7BD: D3 3D       INC R61
    adda #1
    sta <61
LF7BF
    rts                 F7BF: 0A          RETS
LF7C0
    clr <29             F7C0: D5 1D       CLR R29
    lda <62             F7C2: D3 3E       INC R62
    adda #1
    sta <62
    lbcc LF7C8          F7C4: E7 02       JL $F7C8
    lda <61             F7C6: D3 3D       INC R61
    adda #1
    sta <61
LF7C8
    rts                 F7C8: 0A          RETS

;**************************
; TRAP 18
;**************************
TRAP_18
    lda <30             F7C9: 7D 00 1E    CMP %>0,R30
    cmpa #$0
    lbne LF7D2          F7CC: E6 04       JNZ $F7D2
    lbsr LF5DA          F7CE: 8E F5 DA    CALL $F5DA
    rts                 F7D1: 0A          RETS
LF7D2
    tdec 24             F7D2: D2 18       DEC R24
    lda <24             F7D4: 7D 21 18    CMP %>21,R24
    cmpa #$21
    lbeq LF7DC          F7D7: E2 03       JEQ $F7DC
    lbsr TRAP_9         F7D9: F6          TRAP 9
    lda <1              F7DA: 66 F6       BTJO B,A,$F7D2
    anda <0
    lbne LF7D2
LF7DC
    lda <24             F7DC: D3 18       INC R24
    adda #1
    sta <24
    lbsr TRAP_17        F7DE: EE          TRAP 17
    rts                 F7DF: 0A          RETS

;**************************
; TRAP 11
;**************************
TRAP_11
    lda <30             F7E0: 7D 00 1E    CMP %>0,R30
    cmpa #$0
    lbne LF7E8          F7E3: E6 03       JNZ $F7E8
    lda <24             F7E5: D3 18       INC R24
    adda #1
    sta <24
    rts                 F7E7: 0A          RETS
LF7E8
    tdec 24             F7E8: D2 18       DEC R24
    rts                 F7EA: 0A          RETS

;**************************
; TRAP 19
;**************************
TRAP_19
    lda <30             F7EB: 7D 00 1E    CMP %>0,R30
    cmpa #$0
    lbne LF7F3          F7EE: E6 03       JNZ $F7F3
    tdec 24             F7F0: D2 18       DEC R24
    rts                 F7F2: 0A          RETS
LF7F3
    lda <24             F7F3: D3 18       INC R24
    adda #1
    sta <24
    rts                 F7F5: 0A          RETS
***** FFD0: 00 00     ; TRAP_23
***** FFD2: 00 00     ; TRAP_22
***** FFD4: 00 00     ; TRAP_21
***** FFD6: 00 00     ; TRAP_20
***** FFD8: F7 EB     ; TRAP_19
***** FFDA: F7 C9     ; TRAP_18
***** FFDC: F7 A6     ; TRAP_17
***** FFDE: F5 43     ; TRAP_16
***** FFE0: F6 07     ; TRAP_15
***** FFE2: F4 81     ; TRAP_14
***** FFE4: F4 C1     ; TRAP_13
***** FFE6: F2 6A     ; TRAP_12
***** FFE8: F7 E0     ; TRAP_11
***** FFEA: F7 7C     ; TRAP_10
***** FFEC: F5 BD     ; TRAP_9
***** FFEE: F2 06     ; TRAP_8
***** FFF0: F2 02     ; TRAP_7
***** FFF2: F1 0B     ; TRAP_6
***** FFF4: F2 66     ; TRAP_5
***** FFF6: F1 07     ; TRAP_4
***** FFF8: F0 12     ; INT3
***** FFFA: F0 33     ; INT2
***** FFFC: F2 1C     ; INT1
***** FFFE: F0 00     ; RESET (INT0)
interrupt_handler
 orcc #$50
 lda $100
check1
 bita #$01
 beq check2
 bita #$02
 lbne INT1
check2
 bita #$04
 beq check3
 bita #$08
 bne TIMER1
check3
 bita #$10
 beq none
 bita #$20
 lbne INT3
none
lock_up bra lock_up ; there should always be a pending interrupt here.
TIMER1
 anda #$D5
 ora #$08
 sta $100 ; clear INT2 (TIMER) interrupt
 lbra INT2
 zmb $f000-*
 INCLUDEBIN pic-7040-510-novectors.bin
 FDB RESET1 ; 6309 Trap Vector
 FDB RESET1 ; SWI3
 FDB RESET1 ; SWI2
 FDB RESET1 ; FIRQ
 FDB interrupt_handler ; IRQ
 FDB RESET1 ; SWI
 FDB RESET1 ; NMI
 FDB RESET1 ; RESET

