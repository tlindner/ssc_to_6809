	org $e000
	setdp 0
	PRAGMA autobranchlength
	PRAGMA noforwardrefmax
L0026 equ $0026
L0025 equ $0025
L0035 equ $0035
;**************************
; INT0 (reset) vector
;**************************
RESET1
RESET
LF000
    lda #$4A            F000: 52 4A       MOV %>4A,B
    sta <1
    andcc #$fe
    clra                F002: 0D          LDSP              ; Load stack pointer to $4A
    ldb <1
    tfr d,s
    lds #$00ff                ; 6809 stack is in the other direction.
    bsr LF0AE           F003: 8E F0 AE    CALL $F0AE        ; Setup Peripheral file
    andcc #$af          F006: 05          EINT
LF007
    bsr LF051           F007: 8E F0 51    CALL $F051
    bsr LF2AF           F00A: 8E F2 AF    CALL $F2AF
    bsr LF3F0           F00D: 8E F3 F0    CALL $F3F0
    bra LF007           F010: E0 F5       JMP $F007

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
    lda 256+4           F018: 80 04       MOVP P4,A         ; Load new data into r0
    sta <0
    andcc #$fe
    ldd #$FF00          F01A: 88 FF 00 11 MOVD %>FF00,R17
    std <17-1
    andcc #$fe
    bsr TRAP_6          F01E: F9          TRAP 6            ; Process data?
    bcc LF026           F01F: E7 05       JL $F026          ; If carry clear, set CPU ready and exit
    lda #$05            F021: A3 05 00    ANDP %>05,P0      ; Disable INT3. Needs further processing in maip loop?
    anda 256+0
    sta 256+0
    andcc #$fe
    bra LF02C           F024: E0 06       JMP $F02C         ; Cleanup stack and RETI.
LF026
    lda #$7F            F026: A3 7F 08    ANDP %>7F,P8      ; Clears C7
    anda 256+8
    sta 256+8
    andcc #$fe
    lda #$80            F029: A4 80 08    ORP %>80,P8       ; Sets C7 (transisition indicates CPU is not busy to host?)
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
TIMER1
    pshs a,b,x
INT2
    lda <0              F033: B8          PUSH A
    pshs a
    lda <1              F034: C8          PUSH B
    pshs a
    lda #$5             F035: 52 05       MOV %>5,B             ; Set r1 == 5
    sta <1
    andcc #$fe
    lda #$10            F037: 72 10 47    MOV %>10,R71          ; Set r17 == $10
    sta <71
    andcc #$fe
LF03A
    ldb <1              F03A: AA 00 26    LDA @>0026(B)         ; Set r0 == [$0026 + B]
    ldx #L0026
    abx
    lda ,x
    sta <0
    dec <0              F03D: B2          DEC A                 ; Set r0 == r0 - 1
    bcs LF047           F03E: E3 07       JHS $F047             ; Jump to $f047 if carry set
    lda <71             F040: 46 47 3B 06 BTJO R71,R59,$F04A
    anda <59
    bne LF04A
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
    ror <71             F04A: DC 47       RR R71
    dec <1              F04C: CA EC       DJNZ B,$F03A          ; Decrement r1, Jump to $f03a if not 0
    bne LF03A
    puls a              F04E: C9          POP B
    sta <1
    puls a              F04F: B9          POP A
    sta <0
    puls a,b,x
    rti                 F050: 0B          RETI
LF051
    lda #$10            F051: A4 10 00    ORP %>10,P0           ; Set INT3 enable
    ora 256+0
    sta 256+0
    andcc #$fe
    lda <8              F054: 4D 08 09    CMP R8,R9
    cmpa <9
    beq LF0AD           F057: E2 54       JEQ $F0AD             ; If R8 = R9, return
    ldd #$0000          F059: 88 00 00 11 MOVD %>0000,R17       ; R16 = 0. R17 = 0
    std <17-1
    andcc #$fe
    bsr TRAP_4          F05D: FB          TRAP 4                ; turn off interrupts; Process data
    lda #$AF            F05E: 7D AF 07    CMP %>AF,R7
    cmpa <7
    beq LF083           F061: E2 20       JEQ $F083
    lda #$0             F063: 7D 00 07    CMP %>0,R7
    cmpa <7
    beq LF06B           F066: E2 03       JEQ $F06B
    jmp LF157           F068: 8C F1 57    BR $F157
LF06B
    lda #$0             F06B: 2D 00       CMP %>0,A
    cmpa <0
    beq LF000           F06D: E2 91       JEQ $F000             ; Jump to reset
    lda #$C7            F06F: 2D C7       CMP %>C7,A
    cmpa <0
    beq LF0ED           F071: E2 7A       JEQ $F0ED
    lda #$CF            F073: 2D CF       CMP %>CF,A
    cmpa <0
    beq LF0CA           F075: E2 53       JEQ $F0CA
    lda #$80            F077: 2D 80       CMP %>80,A
    cmpa <0
    bmi LF09B           F079: E1 20       JN $F09B
    lda <0              F07B: D0 07       MOV A,R7
    sta <7
    andcc #$fe
    lda <0              F07D: 27 40 2D    BTJZ %>40,A,$F0AD     ; if bit 7 of A is 0 jump to $F0AD
    coma
    anda #$40
    bne LF0AD
    jmp LF1BF           F080: 8C F1 BF    BR $F1BF
LF083
    lda #$1             F083: 76 01 16 0C BTJO %>1,R22,$F093    ; If bit 1 of R22 is 0 jump to $F093
    anda <22
    bne LF093
    lda #$FF            F087: 2D FF       CMP %>FF,A
    cmpa <0
    bne LF08E           F089: E6 03       JNZ $F08E
    jmp LF1AF           F08B: 8C F1 AF    BR $F1AF
LF08E
    lda <0              F08E: D0 3F       MOV A,R63
    sta <63
    andcc #$fe
    com <22             F090: D4 16       INV R22               ; 1s complements
    rts                 F092: 0A          RETS
LF093
    lda <0              F093: D0 3C       MOV A,R60
    sta <60
    andcc #$fe
    lda <63             F095: 12 3F       MOV R63,A
    sta <0
    andcc #$fe
    bsr TRAP_7          F097: F8          TRAP 7                ; Load PSG, A = Address, R60 = data
    lda <22             F098: D5 16       CLR R22
    suba #1
    sta <22
    rts                 F09A: 0A          RETS
LF09B
    lda #$D             F09B: 2D 0D       CMP %>D,A
    cmpa <0
    bne LF0A8           F09D: E6 09       JNZ $F0A8
    lda #$20            F09F: 22 20       MOV %>20,A            ; Jump if not zero
    sta <0
    andcc #$fe
    ldd #$FF02          F0A1: 88 FF 02 11 MOVD %>FF02,R17
    std <17-1
    andcc #$fe
    bsr TRAP_4          F0A5: FB          TRAP 4                ; R16 = $FF, R17 = $02
    lda #$D             F0A6: 22 0D       MOV %>D,A             ; turn off interrupts; Process data
    sta <0
    andcc #$fe
LF0A8
    ldd #$FF02          F0A8: 88 FF 02 11 MOVD %>FF02,R17
    std <17-1
    andcc #$fe
    bsr TRAP_4          F0AC: FB          TRAP 4                ; R16 = $FF, R17 = $02
LF0AD
    rts                 F0AD: 0A          RETS                  ; turn off interrupts; Process data

;**************************
; Subroutine: Setup peripheral file
;**************************
LF0AE
    lda #$3C            F0AE: A2 3C 00    MOVP %>3C,P0          ; Memory mode: Single Chip
    sta 256+0
    andcc #$fe
*****         ; INT3: Enable, clear
*****         ; INT2: Enable, clear
*****         ; INT1: Diable
    lda #$7F            F0B1: A2 7F 08    MOVP %>7F,P8          ; Write $7f to C data port
    sta 256+8
    andcc #$fe
    lda #$FF            F0B4: A2 FF 09    MOVP %>FF,P9          ; Port C DDR: All output
    sta 256+9
    andcc #$fe
    lda #$FF            F0B7: A2 FF 08    MOVP %>FF,P8          ; Write $ff to C data port
    sta 256+8
    andcc #$fe
    lda #$FF            F0BA: A2 FF 02    MOVP %>FF,P2          ; Load timer 1 reg with $ff
    sta 256+2
    andcc #$fe
    lda #$9F            F0BD: A2 9F 03    MOVP %>9F,P3          ; Reload prescaler & decrementer & begin decrememnting
    sta 256+3
    andcc #$fe
*****                                         ; Timer source: internal clock
*****                                         ; Prescale reload reg: $1f
    bsr LF1AF           F0C0: 8E F1 AF    CALL $F1AF
    lda <8              F0C3: D5 08       CLR R8
    suba #1
    sta <8
    lda <9              F0C5: D5 09       CLR R9
    suba #1
    sta <9
    bsr LF0ED           F0C7: 8E F0 ED    CALL $F0ED
LF0CA
    lda <0              F0CA: B5          CLR A
    suba #1
    sta <0
    lda #$17            F0CB: 52 17       MOV %>17,B
    sta <1
    andcc #$fe
LF0CD
    lda <0              F0CD: AB 00 25    STA @>0025(B)
    ldx #L0025
    ldb <1
    abx
    sta ,x
    dec <1              F0D0: CA FB       DJNZ B,$F0CD          ; B--, Jump to $F0CD is zero, status bits not changed
    bne LF0CD
    lda #$8             F0D2: 22 08       MOV %>8,A
    sta <0
    andcc #$fe
    bsr TRAP_8          F0D4: F7          TRAP 8                ; Load PSG, A = Address, R60 = data
    lda #$9             F0D5: 22 09       MOV %>9,A
    sta <0
    andcc #$fe
    bsr TRAP_8          F0D7: F7          TRAP 8                ; Load PSG, A = Address, R60 = data
    lda #$A             F0D8: 22 0A       MOV %>A,A
    sta <0
    andcc #$fe
    bsr TRAP_8          F0DA: F7          TRAP 8                ; Load PSG, A = Address, R60 = data
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
    dec <1              F0E1: CA FB       DJNZ B,$F0DE          ; B--, Jump to $F0DE is zero, status bits not changed
    bne LF0DE
    lda <14             F0E3: D5 0E       CLR R14
    suba #1
    sta <14
    lda <15             F0E5: D5 0F       CLR R15
    suba #1
    sta <15
    lda <67             F0E7: D5 43       CLR R67
    suba #1
    sta <67
    lda #$7             F0E9: 22 07       MOV %>7,A
    sta <0
    andcc #$fe
    bsr TRAP_8          F0EB: F7          TRAP 8                ; Load PSG, A = Address, R60 = data
    rts                 F0EC: 0A          RETS
LF0ED
    lda <10             F0ED: D5 0A       CLR R10
    suba #1
    sta <10
    lda <11             F0EF: D5 0B       CLR R11
    suba #1
    sta <11
    lda <12             F0F1: D5 0C       CLR R12
    suba #1
    sta <12
    lda <13             F0F3: D5 0D       CLR R13
    suba #1
    sta <13
    lda <73             F0F5: D5 49       CLR R73
    suba #1
    sta <73
    lda #$FF            F0F7: 72 FF 05    MOV %>FF,R5
    sta <5
    andcc #$fe
    lda #$FF            F0FA: A2 FF 0B    MOVP %>FF,P11     ; D port DDR: All output
    sta 256+11
    andcc #$fe
    lda #$00            F0FD: A2 00 0A    MOVP %>00,P10     ; D port: $ff
    sta 256+10
    andcc #$fe
    lda #$08            F100: A2 08 08    MOVP %>08,P8      ; Write $08 to C data port, Select: R, RAM, ALD, PSD
    sta 256+8
    andcc #$fe
    lda #$FF            F103: A2 FF 08    MOVP %>FF,P8      ; Write $ff to C data port, Select nothing
    sta 256+8
    andcc #$fe
    rts                 F106: 0A          RETS

;**************************
; TRAP 4
;**************************
TRAP_4
    orcc #$50           F107: 06          DINT
    bsr TRAP_6          F108: F9          TRAP 6
    andcc #$af          F109: 05          EINT
    rts                 F10A: 0A          RETS

;**************************
; SUBROUTINE: TRAP 6: Process data from host?
; Input: A = data from host
;        r16 = flags? (goes into port D data direction register)
;        r17 = flags?
;**************************
TRAP_6
    lda <16             F10B: 77 FF 10 08 BTJZ %>FF,R16,$F117       ; If R17 contains any 0 bits jump to $F117
    coma
    anda #$FF
    bne LF117
    lda #$4             F10F: 7D 04 11    CMP %>4,R17               ; Compute R17 minus 4
    cmpa <17
    bne LF117           F112: E6 03       JNZ $F117                 ; Jump to $f117 if r17 != 4
    lda #$03            F114: A4 03 00    ORP %>03,P0               ; Enable and clear INT1
    ora 256+0
    sta 256+0
    andcc #$fe
LF117
    lda <0              F117: C0          MOV A,B                   ; Copy incomming data to r1
    sta <1
    andcc #$fe
    ldd #$0009          F118: 88 00 09 23 MOVD %>0009,R35           ; r34 = $00, r35 = $09
    std <35-1
    andcc #$fe
    lda <17             F11C: 48 11 23    ADD R17,R35               ; R35 = R35 + R17
    adda <35
    sta <35
    ldx <35-1           F11F: 9A 23       LDA *R35                  ; r0 = [R34:R35]
    lda ,x
    sta <0
    lda <16             F121: 77 FF 10 01 BTJZ %>FF,R16,$F126       ; If R16 contains any 0 bits jump to $F126
    coma
    anda #$FF
    bne LF126
    lda <0              F125: B3          INC A
    adda #1
    sta <0
LF126
    dec <35             F126: D2 23       DEC R35
    ldx <35-1           F128: 9D 23       CMPA *R35
    lda <0
    cmpa ,x
    beq LF156           F12A: E2 2A       JEQ $F156                 ; Jump to $f156 if r0 + 1 == r35 - 1
    lda <16             F12C: 77 FF 10 04 BTJZ %>FF,R16,$F134       ; If R16 contains any 0 bits jump to $F134
    coma
    anda #$FF
    bne LF134
    lda <35             F130: D3 23       INC R35
    adda #1
    sta <35
    bra LF137           F132: E0 03       JMP $F137
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
    lda <0              F139: 82 06       MOVP A,P6                 ; Put r0 on B data port (A0-A7 of static RAM)
    sta 256+6
    andcc #$fe
    ror <17             F13B: DC 11       RR R17                    ; Rotate right, carry also gets bit 0
    lda #$EC            F13D: 78 EC 11    ADD %>EC,R17
    adda <17
    sta <17
    lda <17             F140: 12 11       MOV R17,A
    sta <0
    andcc #$fe
    lda <0              F142: 82 08       MOVP A,P8                 ; Put r0 on C data port
    sta 256+8
    andcc #$fe
    lda <16             F144: 12 10       MOV R16,A
    sta <0
    andcc #$fe
    lda <0              F146: 82 0B       MOVP A,P11                ; Put r16 in port D data direction reg
    sta 256+11
    andcc #$fe
    lda 256+10          F148: 80 0A       MOVP P10,A                ; Read port D data (Ram?, SP0256?, AY3-8913?)
    sta <0
    andcc #$fe
    lda <16             F14A: 77 FF 10 05 BTJZ %>FF,R16,$F153       ; If R16 contains any 0 bits jump to $F153
    coma
    anda #$FF
    bne LF153
    lda <1              F14E: 92 0A       MOVP B,P10                ; Write data from host to D data port (Ram?, SP0256?, A
    sta 256+10
    andcc #$fe
    lda #$F7            F150: A3 F7 08    ANDP %>F7,P8              ; Set R/W* signal to W
    anda 256+8
    sta 256+8
    andcc #$fe
LF153
    lda #$FF            F153: A2 FF 08    MOVP %>FF,P8              ; Disengage all control signals
    sta 256+8
    andcc #$fe
LF156
    rts                 F156: 0A          RETS
LF157
    lda #$8F            F157: 7D 8F 07    CMP %>8F,R7
    cmpa <7
    bne LF160           F15A: E6 04       JNZ $F160
    lda <0              F15C: 82 02       MOVP A,P2                 ; Write timer reload register
    sta 256+2
    andcc #$fe
    bra LF1AF           F15E: E0 4F       JMP $F1AF
LF160
    lda #$FF            F160: 72 FF 13    MOV %>FF,R19
    sta <19
    andcc #$fe
    lda #$20            F163: 76 20 07 07 BTJO %>20,R7,$F16E        ; If bit 6 of R7 is 1 Jump to $F16E
    anda <7
    bne LF16E
    lda #$8             F167: 76 08 07 05 BTJO %>8,R7,$F170         ; if bit 4 of R7 is 1 jump to $Ff170
    anda <7
    bne LF170
    lda #$D             F16B: 72 0D 13    MOV %>D,R19
    sta <19
    andcc #$fe
LF16E
    com <22             F16E: D4 16       INV R22                   ; 1s complement
LF170
    lda #$0             F170: 7D 00 16    CMP %>0,R22
    cmpa <22
    bne LF17F           F173: E6 0A       JNZ $F17F
    lda #$4             F175: 72 04 15    MOV %>4,R21
    sta <21
    andcc #$fe
    lda <22             F178: D3 16       INC R22
    adda #1
    sta <22
    andcc #$fe          F17A: B0          CLRC
    bmi LF17F           F17B: E1 02       JN $F17F
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
    bsr TRAP_5          F18A: FA          TRAP 5
    lda #$0             F18B: 7D 00 03    CMP %>0,R3
    cmpa <3
    bne LF195           F18E: E6 05       JNZ $F195
    lda <23             F190: D3 17       INC R23
    adda #1
    sta <23
    lda <4              F192: 42 04 07    MOV R4,R7                 ; R7 = R4
    sta <7
    andcc #$fe
LF195
    dec <21             F195: DA 15 20    DJNZ R21,$F1B8
    bne LF1B8
    lda <19             F198: 1D 13       CMP R19,A
    cmpa <0
    beq LF1AF           F19A: E2 13       JEQ $F1AF
    lda <7              F19C: 77 08 07 13 BTJZ %>8,R7,$F1B3
    coma
    anda #$8
    bne LF1B3
    lda #$2             F1A0: 72 02 15    MOV %>2,R21
    sta <21
    andcc #$fe
    lda #$20            F1A3: 76 20 07 0F BTJO %>20,R7,$F1B6
    anda <7
    bne LF1B6
    lda <21             F1A7: D3 15       INC R21
    adda #1
    sta <21
    andcc #$fe          F1A9: B0          CLRC
    bmi LF1B8           F1AA: E1 0C       JN $F1B8
    lda <21             F1AC: D3 15       INC R21
    adda #1
    sta <21
    rts                 F1AE: 0A          RETS
LF1AF
    lda <7              F1AF: D5 07       CLR R7
    suba #1
    sta <7
    lda <23             F1B1: D5 17       CLR R23
    suba #1
    sta <23
LF1B3
    lda #$1             F1B3: 72 01 15    MOV %>1,R21
    sta <21
    andcc #$fe
LF1B6
    lda <22             F1B6: D5 16       CLR R22
    suba #1
    sta <22
LF1B8
    lda <7              F1B8: 77 20 07 02 BTJZ %>20,R7,$F1BE
    coma
    anda #$20
    bne LF1BE
    lda <22             F1BC: D5 16       CLR R22
    suba #1
    sta <22
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
    bne LF1CD
    lda #$80            F1C9: 24 80       OR %>80,A
    ora <0
    sta <0
    andcc #$fe
    rol <17             F1CB: DE 11       RL R17
LF1CD
    lda <7              F1CD: 77 08 07 07 BTJZ %>8,R7,$F1D8
    coma
    anda #$8
    bne LF1D8
    lda #$20            F1D1: 76 20 07 06 BTJO %>20,R7,$F1DB
    anda <7
    bne LF1DB
    lda #$4             F1D5: 78 04 11    ADD %>4,R17
    adda <17
    sta <17
LF1D8
    bsr TRAP_4          F1D8: FB          TRAP 4
    bra LF1AF           F1D9: E0 D4       JMP $F1AF
LF1DB
    lda <7              F1DB: 42 07 04    MOV R7,R4
    sta <4
    andcc #$fe
    lda <23             F1DE: 42 17 12    MOV R23,R18
    sta <18
    andcc #$fe
    lda <16             F1E1: D5 10       CLR R16
    suba #1
    sta <16
    bsr TRAP_5          F1E3: FA          TRAP 5
    lda #$0             F1E4: 7D 00 03    CMP %>0,R3
    cmpa <3
    bne LF1AF           F1E7: E6 C6       JNZ $F1AF
    lda #$FF            F1E9: 2D FF       CMP %>FF,A
    cmpa <0
    beq LF1AF           F1EB: E2 C2       JEQ $F1AF
    lda <0              F1ED: D0 3F       MOV A,R63
    sta <63
    andcc #$fe
    bsr TRAP_5          F1EF: FA          TRAP 5
    lda #$0             F1F0: 7D 00 03    CMP %>0,R3
    cmpa <3
    bne LF1AF           F1F3: E6 BA       JNZ $F1AF
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
    bsr TRAP_7          F1FF: F8          TRAP 7            ; Load PSG, A = Address, R60 = data
    bra LF1DB           F200: E0 D9       JMP $F1DB
;**************************
; TRAP 7: Load PSG
; A = Address
; R60 = data
;**************************
TRAP_7
    orcc #$50           F202: 06          DINT
    bsr TRAP_8          F203: F7          TRAP 8
    andcc #$af          F204: 05          EINT
    rts                 F205: 0A          RETS

;**************************
; TRAP 8: Load PSG
; A = Address
; R60 = data
;**************************
TRAP_8
    lda #$FF            F206: A2 FF 0B    MOVP %>FF,P11     ; Set D port (data bus) as all output
    sta 256+11
    andcc #$fe
    lda <0              F209: 82 0A       MOVP A,P10        ; Write r0 to D port (data bus)
    sta 256+10
    andcc #$fe
    lda #$BF            F20B: A2 BF 08    MOVP %>BF,P8      ; select PSG, BC1 high, BDIR high (inactive)
    sta 256+8
    andcc #$fe
    lda #$B6            F20E: A2 B6 08    MOVP %>B6,P8      ; Select PSG, BC1 High, BDIR low (latch address)
    sta 256+8
    andcc #$fe
    lda <60             F211: 12 3C       MOV R60,A
    sta <0
    andcc #$fe
    lda <0              F213: 82 0A       MOVP A,P10        ; Write value to Data bus
    sta 256+10
    andcc #$fe
    lda #$BE            F215: A2 BE 08    MOVP %>BE,P8      ; Select PSG, BC1 Low, BDIR High (latch Data)
    sta 256+8
    andcc #$fe
    lda #$FF            F218: A2 FF 08    MOVP %>FF,P8      ; Deselect
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
    lda #$FF            F222: 7D FF 05    CMP %>FF,R5       ; r5, flag to determine if there is data to send?
    cmpa <5
    bne LF246           F225: E6 1F       JNZ $F246
    ldd #$0004          F227: 88 00 04 11 MOVD %>0004,R17   ; Input to TRAP 6
    std <17-1
    andcc #$fe
    bsr TRAP_6          F22B: F9          TRAP 6            ; Get phonem from static ram?
    bcs LF260           F22C: E3 32       JHS $F260         ; If carry set jump to $f260 (carry set on error?)
    lda #$80            F22E: 26 80 0E    BTJO %>80,A,$F23F ; If MSB is set jump to $f23f
    anda <0
    bne LF23F
LF231
    lda #$FF            F231: A2 FF 0B    MOVP %>FF,P11
    sta 256+11
    andcc #$fe
    lda <0              F234: 82 0A       MOVP A,P10
    sta 256+10
    andcc #$fe
    lda #$DF            F236: A2 DF 08    MOVP %>DF,P8
    sta 256+8
    andcc #$fe
    lda #$FF            F239: A2 FF 08    MOVP %>FF,P8
    sta 256+8
    andcc #$fe
    jmp LF02C           F23C: 8C F0 2C    BR $F02C          ; Restore stack and RETI
LF23F
    lda <0              F23F: D0 05       MOV A,R5          ; Set "no more data" flag
    sta <5
    andcc #$fe
    lda <6              F241: D5 06       CLR R6
    suba #1
    sta <6
    jmp LF02C           F243: 8C F0 2C    BR $F02C          ; Restore stack and RETI
LF246
    lda <16             F246: D5 10       CLR R16
    suba #1
    sta <16
    lda <5              F248: 42 05 04    MOV R5,R4
    sta <4
    andcc #$fe
    lda <6              F24B: 42 06 12    MOV R6,R18
    sta <18
    andcc #$fe
    lda <6              F24E: D3 06       INC R6
    adda #1
    sta <6
    bsr TRAP_12         F250: F3          TRAP 12
    lda #$0             F251: 7D 00 03    CMP %>0,R3
    cmpa <3
    bne LF25D           F254: E6 07       JNZ $F25D
    lda <4              F256: 42 04 05    MOV R4,R5
    sta <5
    andcc #$fe
    lda #$FF            F259: 2D FF       CMP %>FF,A
    cmpa <0
    bne LF231           F25B: E6 D4       JNZ $F231
LF25D
    lda #$FF            F25D: 72 FF 05    MOV %>FF,R5
    sta <5
    andcc #$fe
LF260
    lda #$14            F260: A3 14 00    ANDP %>14,P0
    anda 256+0
    sta 256+0
    andcc #$fe
    jmp LF02C           F263: 8C F0 2C    BR $F02C          ; Restore stack and RETI

;**************************
; TRAP 5:
;**************************
TRAP_5
    orcc #$50           F266: 06          DINT
    bsr TRAP_12         F267: F3          TRAP 12
    andcc #$af          F268: 05          EINT
    rts                 F269: 0A          RETS

;**************************
; TRAP 12
;**************************
TRAP_12
    lda <3              F26A: D5 03       CLR R3
    suba #1
    sta <3
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
    lda #$3F            F273: 7D 3F 12    CMP %>3F,R18
    cmpa <18
    bmi LF28B           F276: E1 13       JN $F28B
    lda #$10            F278: 76 10 04 30 BTJO %>10,R4,$F2AC
    anda <4
    bne LF2AC
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
    bne LF28B
    dec <4              F287: D2 04       DEC R4
    bra LF2AC           F289: E0 21       JMP $F2AC
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
    bne LF2A8
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
    bne LF30C
LF2C9
    bsr LF32E           F2C9: 8E F3 2E    CALL $F32E
    lda #$0             F2CC: 7D 00 03    CMP %>0,R3
    cmpa <3
    beq LF2EA           F2CF: E2 19       JEQ $F2EA
    ldd #$0006          F2D1: 88 00 06 11 MOVD %>0006,R17
    std <17-1
    andcc #$fe
    ldx <32-1           F2D5: 9A 20       LDA *R32
    lda ,x
    sta <0
    lda <0              F2D7: D0 0E       MOV A,R14
    sta <14
    andcc #$fe
    lda <14             F2D9: 4D 0E 0F    CMP R14,R15
    cmpa <15
    beq LF30C           F2DC: E2 2E       JEQ $F30C
    bsr TRAP_4          F2DE: FB          TRAP 4            ; turn off interrupts; Process data
    lda <0              F2DF: 9B 1C       STA *R28
    ldx <28-1
    sta ,x
    lda <0              F2E1: B5          CLR A
    suba #1
    sta <0
    lda <0              F2E2: 9B 1E       STA *R30
    ldx <30-1
    sta ,x
    lda <14             F2E4: 12 0E       MOV R14,A
    sta <0
    andcc #$fe
    lda <0              F2E6: 9B 20       STA *R32
    ldx <32-1
    sta ,x
    bra LF2C9           F2E8: E0 DF       JMP $F2C9
LF2EA
    lda <60             F2EA: 12 3C       MOV R60,A
    sta <0
    andcc #$fe
    lda #$4             F2EC: 7D 04 18    CMP %>4,R24
    cmpa <24
    bne LF2F7           F2EF: E6 06       JNZ $F2F7
    lda #$60            F2F1: 23 60       AND %>60,A
    anda <0
    sta <0
    andcc #$fe
    lda #$60            F2F3: 2D 60       CMP %>60,A
    cmpa <0
    beq LF309           F2F5: E2 12       JEQ $F309
LF2F7
    lda #$5             F2F7: 7D 05 18    CMP %>5,R24
    cmpa <24
    bne LF300           F2FA: E6 04       JNZ $F300
    lda #$80            F2FC: 76 80 3C 09 BTJO %>80,R60,$F309
    anda <60
    bne LF309
LF300
    lda #$E0            F300: 23 E0       AND %>E0,A
    anda <0
    sta <0
    andcc #$fe
    lda <0              F302: B7          SWAP A
    rora
    rora
    rora
    rora
    sta <0
    ror <0              F303: BC          RR A
    lda <0              F304: B3          INC A
    adda #1
    sta <0
    lda <24             F305: 1D 18       CMP R24,A
    cmpa <0
    bne LF2C9           F307: E6 C0       JNZ $F2C9
LF309
    bsr LF36D           F309: 8E F3 6D    CALL $F36D
LF30C
    ldd <26-1           F30C: DB 1A       DECD R26
    subd #1
    std <26-1
    tsta
    ldd <32-1           F30E: DB 20       DECD R32
    subd #1
    std <32-1
    tsta
    ldd <28-1           F310: DB 1C       DECD R28
    subd #1
    std <28-1
    tsta
    ldd <30-1           F312: DB 1E       DECD R30
    subd #1
    std <30-1
    tsta
    ror <70             F314: DC 46       RR R70
    dec <24             F316: DA 18 AC    DJNZ R24,$F2C5
    bne LF2C5
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
    lda <15             F321: 1A 0F       SUB R15,A
    suba <0
    sta <0
    lda <14             F323: 1D 0E       CMP R14,A
    cmpa <0
    tfr cc,a            F325: E4 02       JP $F329
    anda #$0C
    beq LF329
    lda <0              F327: D0 0E       MOV A,R14
    sta <14
    andcc #$fe
LF329
    ldd <32-1           F329: DB 20       DECD R32
    subd #1
    std <32-1
    tsta
    dec <1              F32B: CA F2       DJNZ B,$F31F
    bne LF31F
    rts                 F32D: 0A          RETS
LF32E
    ldx <28-1           F32E: 9A 1C       LDA *R28
    lda ,x
    sta <0
    lda #$FF            F330: 2D FF       CMP %>FF,A
    cmpa <0
    beq LF341           F332: E2 0D       JEQ $F341
    lda <0              F334: D0 04       MOV A,R4
    sta <4
    andcc #$fe
    ldx <30-1           F336: 9A 1E       LDA *R30
    lda ,x
    sta <0
    lda <0              F338: D0 12       MOV A,R18
    sta <18
    andcc #$fe
    lda <16             F33A: D5 10       CLR R16
    suba #1
    sta <16
    bsr TRAP_5          F33C: FA          TRAP 5
    lda #$FF            F33D: 2D FF       CMP %>FF,A
    cmpa <0
    bne LF344           F33F: E6 03       JNZ $F344
LF341
    lda #$FF            F341: 72 FF 03    MOV %>FF,R3
    sta <3
    andcc #$fe
LF344
    lda #$0             F344: 7D 00 03    CMP %>0,R3
    cmpa <3
    bne LF36C           F347: E6 23       JNZ $F36C
    lda <0              F349: D0 3C       MOV A,R60
    sta <60
    andcc #$fe
    lda #$2             F34B: 72 02 40    MOV %>2,R64
    sta <64
    andcc #$fe
    lda #$80            F34E: 26 80 02    BTJO %>80,A,$F353
    anda <0
    bne LF353
    lda <64             F351: D3 40       INC R64
    adda #1
    sta <64
LF353
    ldd #$0024          F353: 88 00 24 42 MOVD %>0024,R66
    std <66-1
    andcc #$fe
LF357
    bsr TRAP_5          F357: FA          TRAP 5
    lda #$0             F358: 7D 00 03    CMP %>0,R3
    cmpa <3
    bne LF36C           F35B: E6 0F       JNZ $F36C
    lda <0              F35D: 9B 42       STA *R66
    ldx <66-1
    sta ,x
    lda <66             F35F: D3 42       INC R66
    adda #1
    sta <66
    dec <64             F361: DA 40 F3    DJNZ R64,$F357
    bne LF357
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
    lda #$60            F371: 2D 60       CMP %>60,A
    cmpa <0
    beq LF3BF           F373: E2 4A       JEQ $F3BF
    lda <0              F375: B7          SWAP A
    rora
    rora
    rora
    rora
    sta <0
    ror <0              F376: BC          RR A
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
    bne LF387
    lda #$4             F380: 72 04 22    MOV %>4,R34
    sta <34
    andcc #$fe
    lda #$80            F383: 76 80 24 03 BTJO %>80,R36,$F38A
    anda <36
    bne LF38A
LF387
    lda #$7             F387: 28 07       ADD %>7,A
    adda <0
    sta <0
    bsr TRAP_7          F389: F8          TRAP 7            ; Load PSG, A = Address, R60 = data
LF38A
    puls a              F38A: B9          POP A
    sta <0
LF38B
    rol <34             F38B: DE 22       RL R34
    dec <0              F38D: BA FC       DJNZ A,$F38B
    bne LF38B
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
    lda #$0             F3AC: 5D 00       CMP %>0,B
    cmpa <1
    beq LF3B6           F3AE: E2 06       JEQ $F3B6
    com <34             F3B0: D4 22       INV R34
    lda <34             F3B2: 13 22       AND R34,A
    anda <0
    sta <0
    andcc #$fe
    bra LF3B8           F3B4: E0 02       JMP $F3B8
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
    bsr TRAP_7          F3BC: F8          TRAP 7            ; Load PSG, A = Address, R60 = data
    bra LF3C2           F3BD: E0 03       JMP $F3C2
LF3BF
    lda #$D             F3BF: 22 0D       MOV %>D,A
    sta <0
    andcc #$fe
    bsr TRAP_7          F3C1: F8          TRAP 7
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
    bsr TRAP_7          F3CC: F8          TRAP 7
    lda <37             F3CD: 42 25 3C    MOV R37,R60
    sta <60
    andcc #$fe
    lda #$5             F3D0: 7D 05 18    CMP %>5,R24
    cmpa <24
    beq LF3DD           F3D3: E2 08       JEQ $F3DD
    dec <63             F3D5: D2 3F       DEC R63
    lda <63             F3D7: 12 3F       MOV R63,A
    sta <0
    andcc #$fe
    bsr TRAP_7          F3D9: F8          TRAP 7
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
    fcb $0a,$01,$03,$05,$0c,$06 original location F3EB-F3EF
LF3F0
    lda <10             F3F0: 4D 0A 0B    CMP R10,R11
    cmpa <11
    beq LF44B           F3F3: E2 56       JEQ $F44B
    lda <12             F3F5: 12 0C       MOV R12,A
    sta <0
    andcc #$fe
    lda <13             F3F7: 1A 0D       SUB R13,A
    suba <0
    sta <0
    bmi LF401           F3F9: E1 06       JN $F401
    beq LF401           F3FB: E2 04       JEQ $F401
    lda #$32            F3FD: 2D 32       CMP %>32,A
    cmpa <0
    bmi LF44B           F3FF: E1 4A       JN $F44B
LF401
    lda #$0             F401: 72 00 21    MOV %>0,R33
    sta <33
    andcc #$fe
    ldd #$0002          F404: 88 00 02 11 MOVD %>0002,R17
    std <17-1
    andcc #$fe
    bsr TRAP_4          F408: FB          TRAP 4            ; turn off interrupts; Process data
    dec <10             F409: D2 0A       DEC R10
    lda #$18            F40B: 2D 18       CMP %>18,A
    cmpa <0
    bmi LF413           F40D: E1 04       JN $F413
LF40F
    bsr LF451           F40F: 8E F4 51    CALL $F451
    rts                 F412: 0A          RETS
LF413
    lda #$D             F413: 2D 0D       CMP %>D,A
    cmpa <0
    beq LF40F           F415: E2 F8       JEQ $F40F
    lda #$FF            F417: 72 FF 21    MOV %>FF,R33
    sta <33
    andcc #$fe
    lda <10             F41A: D3 0A       INC R10
    adda #1
    sta <10
    lda <0              F41C: D0 20       MOV A,R32
    sta <32
    andcc #$fe
    lda <72             F41E: D5 48       CLR R72
    suba #1
    sta <72
    lda #$0             F420: 7D 00 07    CMP %>0,R7
    cmpa <7
    beq LF42A           F423: E2 05       JEQ $F42A
    lda #$8             F425: 76 08 07 01 BTJO %>8,R7,$F42A
    anda <7
    bne LF42A
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
    bsr TRAP_5          F436: FA          TRAP 5
    lda <4              F437: 42 04 20    MOV R4,R32
    sta <32
    andcc #$fe
    lda #$FF            F43A: 7D FF 03    CMP %>FF,R3
    cmpa <3
    beq LF44B           F43D: E2 0C       JEQ $F44B
    lda #$D             F43F: 2D 0D       CMP %>D,A
    cmpa <0
    beq LF44B           F441: E2 08       JEQ $F44B
    bsr TRAP_13         F443: F2          TRAP 13
    lda <31             F444: 42 1F 49    MOV R31,R73
    sta <73
    andcc #$fe
    lda <73             F447: D3 49       INC R73
    adda #1
    sta <73
    bra LF42A           F449: E0 DF       JMP $F42A
LF44B
    lda #$1             F44B: 72 01 48    MOV %>1,R72
    sta <72
    andcc #$fe
    lda <73             F44E: D5 49       CLR R73
    suba #1
    sta <73
    rts                 F450: 0A          RETS
LF451
    lda <10             F451: 4D 0A 0B    CMP R10,R11
    cmpa <11
    beq LF47E           F454: E2 28       JEQ $F47E
    lda #$1             F456: 72 01 48    MOV %>1,R72
    sta <72
    andcc #$fe
    lda <10             F459: D8 0A       PUSH R10
    pshs a
    lda <11             F45B: 42 0B 0A    MOV R11,R10
    sta <10
    andcc #$fe
    dec <10             F45E: D2 0A       DEC R10
    ldd #$0002          F460: 88 00 02 11 MOVD %>0002,R17
    std <17-1
    andcc #$fe
    bsr TRAP_4          F464: FB          TRAP 4            ; turn off interrupts; Process data
    puls a              F465: D9 0A       POP R10
    sta <10
    bsr TRAP_14         F467: F1          TRAP 14
    lda #$4             F468: 7D 04 19    CMP %>4,R25
    cmpa <25
    bne LF47E           F46B: E6 11       JNZ $F47E
    ldd #$0002          F46D: 88 00 02 11 MOVD %>0002,R17
    std <17-1
    andcc #$fe
    bsr TRAP_4          F471: FB          TRAP 4            ; turn off interrupts; Process data
    lda <10             F472: 42 0A 1F    MOV R10,R31
    sta <31
    andcc #$fe
    bsr TRAP_13         F475: F2          TRAP 13
    lda <31             F476: 42 1F 0A    MOV R31,R10
    sta <10
    andcc #$fe
    lda #$4             F479: 7D 04 19    CMP %>4,R25
    cmpa <25
    bne LF451           F47C: E6 D3       JNZ $F451
LF47E
    lda <25             F47E: D5 19       CLR R25
    suba #1
    sta <25
    rts                 F480: 0A          RETS

;**************************
; TRAP 14
;**************************
TRAP_14
    lda #$30            F481: 2D 30       CMP %>30,A
    cmpa <0
    tfr cc,a            F483: E5 02       JPZ $F487
    anda #$0C
    cmpa #$08
    beq LF487
    bra LF4B8           F485: E0 31       JMP $F4B8
LF487
    lda #$3A            F487: 2D 3A       CMP %>3A,A
    cmpa <0
    tfr cc,a            F489: E5 06       JPZ $F491
    anda #$0C
    cmpa #$08
    beq LF491
    ldd #$FF4B          F48B: 88 FF 4B 3E MOVD %>FF4B,R62
    std <62-1
    andcc #$fe
    bra LF4B7           F48F: E0 26       JMP $F4B7
LF491
    lda #$41            F491: 2D 41       CMP %>41,A
    cmpa <0
    tfr cc,a            F493: E5 02       JPZ $F497
    anda #$0C
    cmpa #$08
    beq LF497
    bra LF4B8           F495: E0 21       JMP $F4B8
LF497
    lda #$5B            F497: 2D 5B       CMP %>5B,A
    cmpa <0
    tfr cc,a            F499: E5 09       JPZ $F4A4
    anda #$0C
    cmpa #$08
    beq LF4A4
    ldd #$F83E          F49B: 88 F8 3E 3E MOVD %>F83E,R62
    std <62-1
    andcc #$fe
    lda #$1             F49F: 72 01 19    MOV %>1,R25
    sta <25
    andcc #$fe
    bra LF4B7           F4A2: E0 13       JMP $F4B7
LF4A4
    lda #$61            F4A4: 2D 61       CMP %>61,A
    cmpa <0
    tfr cc,a            F4A6: E5 02       JPZ $F4AA
    anda #$0C
    cmpa #$08
    beq LF4AA
    bra LF4B8           F4A8: E0 0E       JMP $F4B8
LF4AA
    lda #$7B            F4AA: 2D 7B       CMP %>7B,A
    cmpa <0
    tfr cc,a            F4AC: E5 0A       JPZ $F4B8
    anda #$0C
    cmpa #$08
    beq LF4B8
    lda #$20            F4AE: 2A 20       SUB %>20,A
    suba <0
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
    bra LF4B7           F4BF: E0 F6       JMP $F4B7

;**************************
; TRAP 13
;**************************
TRAP_13
    bsr TRAP_14         F4C1: F1          TRAP 14
    lda #$1             F4C2: 7D 01 19    CMP %>1,R25
    cmpa <25
    bne LF4D8           F4C5: E6 11       JNZ $F4D8
    lda <1              F4C7: C5          CLR B
    suba #1
    sta <1
    lda #$41            F4C8: 2A 41       SUB %>41,A
    suba <0
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
    bsr LF534           F4D8: 8E F5 34    CALL $F534
LF4DB
    bsr LF55A           F4DB: 8E F5 5A    CALL $F55A
    bcs LF523           F4DE: E3 43       JHS $F523
    lda <30             F4E0: D5 1E       CLR R30
    suba #1
    sta <30
    bsr TRAP_15         F4E2: F0          TRAP 15
    bcs LF523           F4E3: E3 3E       JHS $F523
    bsr LF79B           F4E5: 8E F7 9B    CALL $F79B
    lda #$1             F4E8: 72 01 1E    MOV %>1,R30
    sta <30
    andcc #$fe
    lda <31             F4EB: 42 1F 18    MOV R31,R24
    sta <24
    andcc #$fe
    bsr TRAP_15         F4EE: F0          TRAP 15
    bcs LF52E           F4EF: E3 3D       JHS $F52E
LF4F1
    lda #$0             F4F1: 7D 00 1B    CMP %>0,R27
    cmpa <27
    beq LF4FC           F4F4: E2 06       JEQ $F4FC
    dec <27             F4F6: D2 1B       DEC R27
    lda <31             F4F8: D3 1F       INC R31
    adda #1
    sta <31
    bra LF4F1           F4FA: E0 F5       JMP $F4F1
LF4FC
    lda #$2             F4FC: 52 02       MOV %>2,B
    sta <1
    andcc #$fe
    bsr TRAP_16         F4FE: EF          TRAP 16
    ldx <62-1           F4FF: 9A 3E       LDA *R62
    lda ,x
    sta <0
    lda #$FF            F501: 2D FF       CMP %>FF,A
    cmpa <0
    bne LF506           F503: E6 01       JNZ $F506
    rts                 F505: 0A          RETS
LF506
    lda <28             F506: D5 1C       CLR R28
    suba #1
    sta <28
    ldx <62-1           F508: 9A 3E       LDA *R62
    lda ,x
    sta <0
    lda <0              F50A: 27 80 03    BTJZ %>80,A,$F510
    coma
    anda #$80
    bne LF510
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
    bsr TRAP_4          F516: FB          TRAP 4            ; turn off interrupts; Process data
    lda <62             F517: D3 3E       INC R62
    adda #1
    sta <62
    bcc LF51D           F519: E7 02       JL $F51D
    lda <61             F51B: D3 3D       INC R61
    adda #1
    sta <61
LF51D
    lda #$1             F51D: 7D 01 1C    CMP %>1,R28
    cmpa <28
    bne LF506           F520: E6 E4       JNZ $F506
    rts                 F522: 0A          RETS
LF523
    lda <62             F523: D3 3E       INC R62
    adda #1
    sta <62
    bcc LF529           F525: E7 02       JL $F529
    lda <61             F527: D3 3D       INC R61
    adda #1
    sta <61
LF529
    lda #$2             F529: 52 02       MOV %>2,B
    sta <1
    andcc #$fe
    bsr TRAP_16         F52B: EF          TRAP 16
    bra LF4DB           F52C: E0 AD       JMP $F4DB
LF52E
    lda #$3             F52E: 52 03       MOV %>3,B
    sta <1
    andcc #$fe
    bsr TRAP_16         F530: EF          TRAP 16
    bra LF4DB           F531: E0 A8       JMP $F4DB
    rts                 F533: 0A          RETS
LF534
    ldx <62-1           F534: 9A 3E       LDA *R62
    lda ,x
    sta <0
    lda <0              F536: 27 40 01    BTJZ %>40,A,$F53A
    coma
    anda #$40
    bne LF53A
    rts                 F539: 0A          RETS
LF53A
    lda <62             F53A: D3 3E       INC R62
    adda #1
    sta <62
    bcc LF540           F53C: E7 02       JL $F540
    lda <61             F53E: D3 3D       INC R61
    adda #1
    sta <61
LF540
    bra LF534           F540: E0 F2       JMP $F534
    rts                 F542: 0A          RETS

;**************************
; TRAP 16
;**************************
TRAP_16
    lda <28             F543: D5 1C       CLR R28
    suba #1
    sta <28
LF545
    ldx <62-1           F545: 9A 3E       LDA *R62
    lda ,x
    sta <0
    lda <0              F547: 27 40 07    BTJZ %>40,A,$F551
    coma
    anda #$40
    bne LF551
    lda <28             F54A: D3 1C       INC R28
    adda #1
    sta <28
    lda <28             F54C: 3D 1C       CMP R28,B
    cmpa <1
    tfr cc,a            F54E: E4 01       JP $F551
    anda #$0C
    beq LF551
    rts                 F550: 0A          RETS
LF551
    lda <62             F551: D3 3E       INC R62
    adda #1
    sta <62
    bcc LF557           F553: E7 02       JL $F557
    lda <61             F555: D3 3D       INC R61
    adda #1
    sta <61
LF557
    bra LF545           F557: E0 EC       JMP $F545
    rts                 F559: 0A          RETS
LF55A
    lda <29             F55A: D5 1D       CLR R29
    suba #1
    sta <29
    lda <27             F55C: D5 1B       CLR R27
    suba #1
    sta <27
    lda <31             F55E: 42 1F 18    MOV R31,R24
    sta <24
    andcc #$fe
    ldx <62-1           F561: 9A 3E       LDA *R62
    lda ,x
    sta <0
    lda #$FF            F563: 2D FF       CMP %>FF,A
    cmpa <0
    bne LF569           F565: E6 02       JNZ $F569
    andcc #$fe          F567: B0          CLRC
    rts                 F568: 0A          RETS
LF569
    lda #$1             F569: 7D 01 19    CMP %>1,R25
    cmpa <25
    bne LF570           F56C: E6 02       JNZ $F570
LF56E
    lda <24             F56E: D3 18       INC R24
    adda #1
    sta <24
LF570
    bsr TRAP_10         F570: F5          TRAP 10
    lda #$61            F571: 2D 61       CMP %>61,A
    cmpa <0
    bmi LF577           F573: E1 02       JN $F577
    lda #$20            F575: 2A 20       SUB %>20,A
    suba <0
    sta <0
LF577
    lda #$20            F577: 2A 20       SUB %>20,A
    suba <0
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
    bne LF584
    lda #$1             F581: 72 01 1D    MOV %>1,R29
    sta <29
    andcc #$fe
LF584
    lda #$3F            F584: 23 3F       AND %>3F,A
    anda <0
    sta <0
    andcc #$fe
    lda <0              F586: 3D 00       CMP R0,B
    cmpa <1
    beq LF58E           F588: E2 04       JEQ $F58E
    lda <27             F58A: D5 1B       CLR R27
    suba #1
    sta <27
    orcc #$01           F58C: 07          SETC
    rts                 F58D: 0A          RETS
LF58E
    lda #$1             F58E: 7D 01 19    CMP %>1,R25
    cmpa <25
    bne LF595           F591: E6 02       JNZ $F595
    lda <27             F593: D3 1B       INC R27
    adda #1
    sta <27
LF595
    lda #$1             F595: 7D 01 1D    CMP %>1,R29
    cmpa <29
    bne LF59C           F598: E6 02       JNZ $F59C
    andcc #$fe          F59A: B0          CLRC
    rts                 F59B: 0A          RETS
LF59C
    lda <62             F59C: D3 3E       INC R62
    adda #1
    sta <62
    bcc LF5A2           F59E: E7 02       JL $F5A2
    lda <61             F5A0: D3 3D       INC R61
    adda #1
    sta <61
LF5A2
    ldx <62-1           F5A2: 9A 3E       LDA *R62
    lda ,x
    sta <0
    bra LF56E           F5A4: E0 C8       JMP $F56E
    rts                 F5A6: 0A          RETS
LF5A7
    lda <62             F5A7: D3 3E       INC R62
    adda #1
    sta <62
    bcc LF5AD           F5A9: E7 02       JL $F5AD
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
    bne LF5B8
    lda #$1             F5B2: 72 01 1D    MOV %>1,R29
    sta <29
    andcc #$fe
    ldd <62-1           F5B5: DB 3E       DECD R62
    subd #1
    std <62-1
    tsta
    rts                 F5B7: 0A          RETS
LF5B8
    lda <29             F5B8: D5 1D       CLR R29
    suba #1
    sta <29
    ldd <62-1           F5BA: DB 3E       DECD R62
    subd #1
    std <62-1
    tsta
    rts                 F5BC: 0A          RETS

;**************************
; TRAP 9
;**************************
TRAP_9
    lda <1              F5BD: C8          PUSH B
    pshs a
    lda <1              F5BE: C5          CLR B
    suba #1
    sta <1
    bsr TRAP_10         F5BF: F5          TRAP 10
    lda #$61            F5C0: 2D 61       CMP %>61,A
    cmpa <0
    bmi LF5C6           F5C2: E1 02       JN $F5C6
    lda #$20            F5C4: 2A 20       SUB %>20,A
    suba <0
    sta <0
LF5C6
    lda #$41            F5C6: 2A 41       SUB %>41,A
    suba <0
    sta <0
    lda <1              F5C8: 68          ADD B,A
    adda <0
    sta <0
    lda <0              F5C9: C0          MOV A,B
    sta <1
    andcc #$fe
    lda #$1A            F5CA: 5D 1A       CMP %>1A,B
    cmpa <1
    tfr cc,a            F5CC: E4 09       JP $F5D7
    anda #$0C
    beq LF5D7
    lda #$0             F5CE: 5D 00       CMP %>0,B
    cmpa <1
    bmi LF5D7           F5D0: E1 05       JN $F5D7
    ldb <1              F5D2: AA F7 F6    LDA @>F7F6(B)
    ldx #LF7F6
    abx
    lda ,x
    sta <0
    puls a              F5D5: C9          POP B
    sta <1
    rts                 F5D6: 0A          RETS
LF5D7
    lda <0              F5D7: B5          CLR A
    suba #1
    sta <0
    puls a              F5D8: C9          POP B
    sta <1
    rts                 F5D9: 0A          RETS
LF5DA
    lda <24             F5DA: D3 18       INC R24
    adda #1
    sta <24
    bsr TRAP_9          F5DC: F6          TRAP 9
    lda <1              F5DD: 66 FB       BTJO B,A,$F5DA
    anda <0
    bne LF5DA
    dec <24             F5DF: D2 18       DEC R24
    bsr TRAP_17         F5E1: EE          TRAP 17
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
    lda #$0             F607: 7D 00 1E    CMP %>0,R30
    cmpa <30
    bne LF616           F60A: E6 0A       JNZ $F616
    lda #$40            F60C: 52 40       MOV %>40,B
    sta <1
    andcc #$fe
    lda <62             F60E: D3 3E       INC R62
    adda #1
    sta <62
    bcc LF614           F610: E7 02       JL $F614
    lda <61             F612: D3 3D       INC R61
    adda #1
    sta <61
LF614
    bra LF61A           F614: E0 04       JMP $F61A
LF616
    lda #$80            F616: 52 80       MOV %>80,B
    sta <1
    andcc #$fe
    ldd <62-1           F618: DB 3E       DECD R62
    subd #1
    std <62-1
    tsta
LF61A
    ldx <62-1           F61A: 9A 3E       LDA *R62
    lda ,x
    sta <0
    lda <0              F61C: 67 11       BTJZ B,A,$F630
    coma
    anda <1
    bne LF630
    lda #$0             F61E: 7D 00 1E    CMP %>0,R30
    cmpa <30
    bne LF627           F621: E6 04       JNZ $F627
    ldd <62-1           F623: DB 3E       DECD R62
    subd #1
    std <62-1
    tsta
    bra LF62D           F625: E0 06       JMP $F62D
LF627
    lda <62             F627: D3 3E       INC R62
    adda #1
    sta <62
    bcc LF62D           F629: E7 02       JL $F62D
    lda <61             F62B: D3 3D       INC R61
    adda #1
    sta <61
LF62D
    andcc #$fe          F62D: B0          CLRC
    rts                 F62E: 0A          RETS
    bsr TRAP_11         F62F: F4          TRAP 11
LF630
    bsr TRAP_10         F630: F5          TRAP 10
    lda #$61            F631: 2D 61       CMP %>61,A
    cmpa <0
    bmi LF637           F633: E1 02       JN $F637
    lda #$20            F635: 2A 20       SUB %>20,A
    suba <0
    sta <0
LF637
    lda #$20            F637: 2A 20       SUB %>20,A
    suba <0
    sta <0
    lda <0              F639: D0 1A       MOV A,R26
    sta <26
    andcc #$fe
    lda #$0             F63B: 7D 00 1E    CMP %>0,R30
    cmpa <30
    beq LF64D           F63E: E2 0D       JEQ $F64D
    lda #$21            F640: 7D 21 18    CMP %>21,R24
    cmpa <24
    bne LF64D           F643: E6 08       JNZ $F64D
    lda #$13            F645: 22 13       MOV %>13,A
    sta <0
    andcc #$fe
    ldx <62-1           F647: 9D 3E       CMPA *R62
    lda <0
    cmpa ,x
    beq LF62D           F649: E2 E2       JEQ $F62D
    orcc #$01           F64B: 07          SETC
    rts                 F64C: 0A          RETS
LF64D
    ldx <62-1           F64D: 9A 3E       LDA *R62
    lda ,x
    sta <0
    lda #$15            F64F: 2D 15       CMP %>15,A
    cmpa <0
    bmi LF666           F651: E1 13       JN $F666
LF653
    lda <29             F653: D5 1D       CLR R29
    suba #1
    sta <29
    lda <0              F655: 4D 00 1A    CMP R0,R26
    cmpa <26
    beq LF65C           F658: E2 02       JEQ $F65C
    orcc #$01           F65A: 07          SETC
    rts                 F65B: 0A          RETS
LF65C
    bsr TRAP_17         F65C: EE          TRAP 17
    lda #$1             F65D: 7D 01 1D    CMP %>1,R29
    cmpa <29
    beq LF664           F660: E2 02       JEQ $F664
LF662
    bra LF607           F662: E0 A3       JMP $F607
LF664
    andcc #$fe          F664: B0          CLRC
    rts                 F665: 0A          RETS
LF666
    lda #$7             F666: 2D 07       CMP %>7,A
    cmpa <0
    beq LF653           F668: E2 E9       JEQ $F653
    lda <0              F66A: C0          MOV A,B
    sta <1
    andcc #$fe
    lda #$9             F66B: 5A 09       SUB %>9,B
    suba <1
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
    lda #$21            F672: 7D 21 1A    CMP %>21,R26
    cmpa <26
    bmi LF67E           F675: E1 07       JN $F67E
    lda #$3A            F677: 7D 3A 1A    CMP %>3A,R26
    cmpa <26
    tfr cc,a            F67A: E4 02       JP $F67E
    anda #$0C
    beq LF67E
    orcc #$01           F67C: 07          SETC
    rts                 F67D: 0A          RETS
LF67E
    bsr TRAP_17         F67E: EE          TRAP 17
    lda #$1             F67F: 7D 01 1D    CMP %>1,R29
    cmpa <29
    beq LF686           F682: E2 02       JEQ $F686
    bra LF662           F684: E0 DC       JMP $F662
LF686
    andcc #$fe          F686: B0          CLRC
    rts                 F687: 0A          RETS
LF688
    bsr TRAP_9          F688: F6          TRAP 9
    lda #$8             F689: 26 08 02    BTJO %>8,A,$F68E
    anda <0
    bne LF68E
    orcc #$01           F68C: 07          SETC
    rts                 F68D: 0A          RETS
LF68E
    lda #$8             F68E: 52 08       MOV %>8,B
    sta <1
    andcc #$fe
    bsr TRAP_18         F690: ED          TRAP 18
    lda #$1             F691: 7D 01 1D    CMP %>1,R29
    cmpa <29
    beq LF698           F694: E2 02       JEQ $F698
    bra LF662           F696: E0 CA       JMP $F662
LF698
    andcc #$fe          F698: B0          CLRC
    rts                 F699: 0A          RETS
LF69A
    bsr TRAP_9          F69A: F6          TRAP 9
    lda #$80            F69B: 26 80 02    BTJO %>80,A,$F6A0
    anda <0
    bne LF6A0
    orcc #$01           F69E: 07          SETC
    rts                 F69F: 0A          RETS
LF6A0
    bsr TRAP_11         F6A0: F4          TRAP 11
    bsr TRAP_9          F6A1: F6          TRAP 9
    lda #$80            F6A2: 26 80 02    BTJO %>80,A,$F6A7
    anda <0
    bne LF6A7
    orcc #$01           F6A5: 07          SETC
    rts                 F6A6: 0A          RETS
LF6A7
    lda #$80            F6A7: 52 80       MOV %>80,B
    sta <1
    andcc #$fe
    bsr TRAP_18         F6A9: ED          TRAP 18
    lda #$1             F6AA: 7D 01 1D    CMP %>1,R29
    cmpa <29
    beq LF6B1           F6AD: E2 02       JEQ $F6B1
    bra LF662           F6AF: E0 B1       JMP $F662
LF6B1
    andcc #$fe          F6B1: B0          CLRC
    rts                 F6B2: 0A          RETS
LF6B3
    bsr TRAP_9          F6B3: F6          TRAP 9
    lda #$8             F6B4: 26 08 0B    BTJO %>8,A,$F6C2
    anda <0
    bne LF6C2
    bsr TRAP_17         F6B7: EE          TRAP 17
    lda #$1             F6B8: 7D 01 1D    CMP %>1,R29
    cmpa <29
    beq LF6C0           F6BB: E2 03       JEQ $F6C0
    bsr TRAP_19         F6BD: EC          TRAP 19
LF6BE
    bra LF662           F6BE: E0 A2       JMP $F662
LF6C0
    andcc #$fe          F6C0: B0          CLRC
    rts                 F6C1: 0A          RETS
LF6C2
    lda #$8             F6C2: 52 08       MOV %>8,B
    sta <1
    andcc #$fe
    bsr TRAP_18         F6C4: ED          TRAP 18
    lda #$1             F6C5: 7D 01 1D    CMP %>1,R29
    cmpa <29
    beq LF6CC           F6C8: E2 02       JEQ $F6CC
LF6CA
    bra LF6BE           F6CA: E0 F2       JMP $F6BE
LF6CC
    andcc #$fe          F6CC: B0          CLRC
    rts                 F6CD: 0A          RETS
LF6CE
    bsr TRAP_9          F6CE: F6          TRAP 9
    lda #$1             F6CF: 26 01 15    BTJO %>1,A,$F6E7
    anda <0
    bne LF6E7
    bsr TRAP_10         F6D2: F5          TRAP 10
    lda #$49            F6D3: 2D 49       CMP %>49,A
    cmpa <0
    beq LF6D9           F6D5: E2 02       JEQ $F6D9
LF6D7
    orcc #$01           F6D7: 07          SETC
    rts                 F6D8: 0A          RETS
LF6D9
    bsr TRAP_11         F6D9: F4          TRAP 11
    bsr TRAP_10         F6DA: F5          TRAP 10
    lda #$4E            F6DB: 2D 4E       CMP %>4E,A
    cmpa <0
    bne LF6D7           F6DD: E6 F8       JNZ $F6D7
    bsr TRAP_11         F6DF: F4          TRAP 11
    bsr TRAP_10         F6E0: F5          TRAP 10
    lda #$47            F6E1: 2D 47       CMP %>47,A
    cmpa <0
    beq LF706           F6E3: E2 21       JEQ $F706
    orcc #$01           F6E5: 07          SETC
    rts                 F6E6: 0A          RETS
LF6E7
    bsr TRAP_11         F6E7: F4          TRAP 11
    bsr TRAP_10         F6E8: F5          TRAP 10
    lda #$52            F6E9: 2D 52       CMP %>52,A
    cmpa <0
    beq LF706           F6EB: E2 19       JEQ $F706
    lda #$53            F6ED: 2D 53       CMP %>53,A
    cmpa <0
    beq LF706           F6EF: E2 15       JEQ $F706
    lda #$44            F6F1: 2D 44       CMP %>44,A
    cmpa <0
    beq LF706           F6F3: E2 11       JEQ $F706
    lda #$4C            F6F5: 2D 4C       CMP %>4C,A
    cmpa <0
    beq LF6FC           F6F7: E2 03       JEQ $F6FC
    bsr TRAP_19         F6F9: EC          TRAP 19
    bra LF706           F6FA: E0 0A       JMP $F706
LF6FC
    bsr TRAP_11         F6FC: F4          TRAP 11
    bsr TRAP_10         F6FD: F5          TRAP 10
    lda #$59            F6FE: 2D 59       CMP %>59,A
    cmpa <0
    beq LF706           F700: E2 04       JEQ $F706
    bsr TRAP_19         F702: EC          TRAP 19
    bsr TRAP_19         F703: EC          TRAP 19
    bra LF726           F704: E0 20       JMP $F726
LF706
    bsr TRAP_11         F706: F4          TRAP 11
    bsr TRAP_10         F707: F5          TRAP 10
    lda #$20            F708: 2D 20       CMP %>20,A
    cmpa <0
    beq LF726           F70A: E2 1A       JEQ $F726
    orcc #$01           F70C: 07          SETC
    rts                 F70D: 0A          RETS
LF70E
    bsr TRAP_9          F70E: F6          TRAP 9
    lda #$80            F70F: 26 80 02    BTJO %>80,A,$F714
    anda <0
    bne LF714
    orcc #$01           F712: 07          SETC
    rts                 F713: 0A          RETS
LF714
    lda #$80            F714: 52 80       MOV %>80,B
    sta <1
    andcc #$fe
    bsr TRAP_18         F716: ED          TRAP 18
    lda #$1             F717: 7D 01 1D    CMP %>1,R29
    cmpa <29
    beq LF71E           F71A: E2 02       JEQ $F71E
LF71C
    bra LF6CA           F71C: E0 AC       JMP $F6CA
LF71E
    andcc #$fe          F71E: B0          CLRC
    rts                 F71F: 0A          RETS
LF720
    bsr TRAP_9          F720: F6          TRAP 9
    lda #$40            F721: 26 40 02    BTJO %>40,A,$F726
    anda <0
    bne LF726
    orcc #$01           F724: 07          SETC
    rts                 F725: 0A          RETS
LF726
    bsr TRAP_17         F726: EE          TRAP 17
    lda #$1             F727: 7D 01 1D    CMP %>1,R29
    cmpa <29
    beq LF72E           F72A: E2 02       JEQ $F72E
    bra LF71C           F72C: E0 EE       JMP $F71C
LF72E
    andcc #$fe          F72E: B0          CLRC
    rts                 F72F: 0A          RETS
LF730
    bsr TRAP_9          F730: F6          TRAP 9
    lda #$8             F731: 26 08 F2    BTJO %>8,A,$F726
    anda <0
    bne LF726
    orcc #$01           F734: 07          SETC
    rts                 F735: 0A          RETS
LF736
    bsr TRAP_9          F736: F6          TRAP 9
    lda #$4             F737: 26 04 EC    BTJO %>4,A,$F726
    anda <0
    bne LF726
    orcc #$01           F73A: 07          SETC
    rts                 F73B: 0A          RETS
LF73C
    bsr TRAP_9          F73C: F6          TRAP 9
    lda #$2             F73D: 26 02 E6    BTJO %>2,A,$F726
    anda <0
    bne LF726
    orcc #$01           F740: 07          SETC
    rts                 F741: 0A          RETS
LF742
    bsr TRAP_9          F742: F6          TRAP 9
    lda #$20            F743: 26 20 02    BTJO %>20,A,$F748
    anda <0
    bne LF748
    orcc #$01           F746: 07          SETC
    rts                 F747: 0A          RETS
LF748
    lda #$20            F748: 52 20       MOV %>20,B
    sta <1
    andcc #$fe
    bsr TRAP_18         F74A: ED          TRAP 18
    lda #$1             F74B: 7D 01 1D    CMP %>1,R29
    cmpa <29
    beq LF752           F74E: E2 02       JEQ $F752
LF750
    bra LF71C           F750: E0 CA       JMP $F71C
LF752
    andcc #$fe          F752: B0          CLRC
    rts                 F753: 0A          RETS
LF754
    bsr TRAP_9          F754: F6          TRAP 9
    lda #$10            F755: 26 10 0F    BTJO %>10,A,$F767
    anda <0
    bne LF767
    bsr TRAP_10         F758: F5          TRAP 10
    lda #$23            F759: 2D 23       CMP %>23,A
    cmpa <0
    beq LF75F           F75B: E2 02       JEQ $F75F
    orcc #$01           F75D: 07          SETC
    rts                 F75E: 0A          RETS
LF75F
    bsr TRAP_11         F75F: F4          TRAP 11
    bsr TRAP_10         F760: F5          TRAP 10
    lda #$48            F761: 2D 48       CMP %>48,A
    cmpa <0
    beq LF726           F763: E2 C1       JEQ $F726
    orcc #$01           F765: 07          SETC
    rts                 F766: 0A          RETS
LF767
    bsr TRAP_10         F767: F5          TRAP 10
    lda #$34            F768: 2D 34       CMP %>34,A
    cmpa <0
    beq LF770           F76A: E2 04       JEQ $F770
    lda #$33            F76C: 2D 33       CMP %>33,A
    cmpa <0
    bne LF726           F76E: E6 B6       JNZ $F726
LF770
    bsr TRAP_11         F770: F4          TRAP 11
    bsr TRAP_10         F771: F5          TRAP 10
    lda #$48            F772: 2D 48       CMP %>48,A
    cmpa <0
    beq LF777           F774: E2 01       JEQ $F777
    bsr TRAP_19         F776: EC          TRAP 19
LF777
    bra LF750           F777: E0 D7       JMP $F750
    rts                 F779: 0A          RETS
    orcc #$01           F77A: 07          SETC
    rts                 F77B: 0A          RETS

;**************************
; TRAP 10
;**************************
TRAP_10
    lda <1              F77C: C8          PUSH B
    pshs a
    lda #$0             F77D: 7D 00 48    CMP %>0,R72
    cmpa <72
    bne LF78B           F780: E6 09       JNZ $F78B
    lda <32             F782: 42 20 04    MOV R32,R4
    sta <4
    andcc #$fe
    lda <24             F785: 42 18 12    MOV R24,R18
    sta <18
    andcc #$fe
    bsr TRAP_5          F788: FA          TRAP 5
    puls a              F789: C9          POP B
    sta <1
    rts                 F78A: 0A          RETS
LF78B
    lda <10             F78B: D8 0A       PUSH R10
    pshs a
    lda <24             F78D: 42 18 0A    MOV R24,R10
    sta <10
    andcc #$fe
    dec <10             F790: D2 0A       DEC R10
    ldd #$0002          F792: 88 00 02 11 MOVD %>0002,R17
    std <17-1
    andcc #$fe
    bsr TRAP_4          F796: FB          TRAP 4            ; turn off interrupts; Process data
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
    bne LF7A1
    rts                 F7A0: 0A          RETS
LF7A1
    ldd <62-1           F7A1: DB 3E       DECD R62
    subd #1
    std <62-1
    tsta
    bra LF79B           F7A3: E0 F6       JMP $F79B
    rts                 F7A5: 0A          RETS

;**************************
; TRAP 17
;**************************
TRAP_17
    lda #$0             F7A6: 7D 00 1E    CMP %>0,R30
    cmpa <30
    bne LF7AF           F7A9: E6 04       JNZ $F7AF
    bsr LF5A7           F7AB: 8E F5 A7    CALL $F5A7
    rts                 F7AE: 0A          RETS
LF7AF
    ldd <62-1           F7AF: DB 3E       DECD R62
    subd #1
    std <62-1
    tsta
    ldx <62-1           F7B1: 9A 3E       LDA *R62
    lda ,x
    sta <0
    lda <0              F7B3: 27 80 0A    BTJZ %>80,A,$F7C0
    coma
    anda #$80
    bne LF7C0
    lda #$1             F7B6: 72 01 1D    MOV %>1,R29
    sta <29
    andcc #$fe
    lda <62             F7B9: D3 3E       INC R62
    adda #1
    sta <62
    bcc LF7BF           F7BB: E7 02       JL $F7BF
    lda <61             F7BD: D3 3D       INC R61
    adda #1
    sta <61
LF7BF
    rts                 F7BF: 0A          RETS
LF7C0
    lda <29             F7C0: D5 1D       CLR R29
    suba #1
    sta <29
    lda <62             F7C2: D3 3E       INC R62
    adda #1
    sta <62
    bcc LF7C8           F7C4: E7 02       JL $F7C8
    lda <61             F7C6: D3 3D       INC R61
    adda #1
    sta <61
LF7C8
    rts                 F7C8: 0A          RETS

;**************************
; TRAP 18
;**************************
TRAP_18
    lda #$0             F7C9: 7D 00 1E    CMP %>0,R30
    cmpa <30
    bne LF7D2           F7CC: E6 04       JNZ $F7D2
    bsr LF5DA           F7CE: 8E F5 DA    CALL $F5DA
    rts                 F7D1: 0A          RETS
LF7D2
    dec <24             F7D2: D2 18       DEC R24
    lda #$21            F7D4: 7D 21 18    CMP %>21,R24
    cmpa <24
    beq LF7DC           F7D7: E2 03       JEQ $F7DC
    bsr TRAP_9          F7D9: F6          TRAP 9
    lda <1              F7DA: 66 F6       BTJO B,A,$F7D2
    anda <0
    bne LF7D2
LF7DC
    lda <24             F7DC: D3 18       INC R24
    adda #1
    sta <24
    bsr TRAP_17         F7DE: EE          TRAP 17
    rts                 F7DF: 0A          RETS

;**************************
; TRAP 11
;**************************
TRAP_11
    lda #$0             F7E0: 7D 00 1E    CMP %>0,R30
    cmpa <30
    bne LF7E8           F7E3: E6 03       JNZ $F7E8
    lda <24             F7E5: D3 18       INC R24
    adda #1
    sta <24
    rts                 F7E7: 0A          RETS
LF7E8
    dec <24             F7E8: D2 18       DEC R24
    rts                 F7EA: 0A          RETS

;**************************
; TRAP 19
;**************************
TRAP_19
    lda #$0             F7EB: 7D 00 1E    CMP %>0,R30
    cmpa <30
    bne LF7F3           F7EE: E6 03       JNZ $F7F3
    dec <24             F7F0: D2 18       DEC R24
    rts                 F7F2: 0A          RETS
LF7F3
    lda <24             F7F3: D3 18       INC R24
    adda #1
    sta <24
    rts                 F7F5: 0A          RETS
;**************************
    zmb $f3ea-*
    fcb $0A
    fcb $0a,$01,$03,$05,$0c,$06 original location F3EB-F3EF
    zmb $f7f6-*
    org $F7F6
LF7F6
    fcb $80,$48
    fcb $68
    fcb $58,$85
    fcb $08
    fcb $68
    fcb $28,$84
    fcb $78,$08,$58
    fcb $48,$58,$82
    fcb $08
    fcb $08
    fcb $58,$38
    fcb $18,$82
    fcb $48,$48,$28
    fcb $84,$78
    fcb $FF
    fcb $C0
    fcb $C1
    fcb $CD
    fcb $C0
    fcb $0A
    fcb $47,$B3,$EB,$09
    fcb $10
    fcb $0A
    fcb $25,$47
    fcb $B3
    fcb $EB
    fcb $09
    fcb $47,$B3,$EB,$47
    fcb $B3
    fcb $F7
    fcb $C7
    fcb $FF
    fcb $CC
    fcb $C3
    fcb $FF
    fcb $44,$84,$DF
    fcb $44,$84,$DB
    fcb $C3
    fcb $C2
    fcb $FF
    fcb $C1
    fcb $44,$84,$C8
    fcb $C4
    fcb $C9
    fcb $C4
    fcb $DA,$C4,$13
    fcb $FF
    fcb $13,$D4
    fcb $FF
LF843
    fcb $13,$CF
    fcb $13,$72
    fcb $A5,$13,$FB
    fcb $13,$F2
    fcb $2F,$4F
    fcb $A7,$13,$FF,$0E
LF852
    fcb $32,$CF
    fcb $F2
    fcb $09
    fcb $EF
    fcb $13,$10
    fcb $6E
    fcb $B9
    fcb $47,$0B,$93,$67
    fcb $21
    fcb $29,$AE
    fcb $4F,$01,$3D
    fcb $07
    fcb $07
    fcb $8B,$13,$FF
    fcb $0E
    fcb $09
LF86C
    fcb $CF
    fcb $0E
    fcb $F3
    fcb $09
    fcb $54,$B7
    fcb $FF
    fcb $37,$21,$CF
    fcb $F7
LF877
    fcb $57,$97,$FF
    fcb $0E
    fcb $0F
    fcb $09
    fcb $D4,$09
    fcb $10
    fcb $E7,$25
    fcb $4C,$01,$8A
    fcb $FF
    fcb $0E
    fcb $0B
    fcb $D4,$FF
    fcb $0E
    fcb $0F
    fcb $10
    fcb $09
    fcb $DA,$72,$B2
    fcb $5A,$A7
    fcb $13,$10
    fcb $F2
    fcb $13,$FB
    fcb $F2
    fcb $13,$F3
    fcb $F2
    fcb $FB
    fcb $69
    fcb $B2
    fcb $47,$AF,$E9,$D4
    fcb $F9
    fcb $D4,$F5
    fcb $D7,$09
    fcb $10
    fcb $EC
    fcb $13,$FE
    fcb $09
    fcb $10
    fcb $6C
    fcb $B3
    fcb $13,$7E
    fcb $AB,$6C,$AB
    fcb $57,$02,$A9
    fcb $EC
    fcb $0E
    fcb $57,$AD,$13
    fcb $10
    fcb $62
    fcb $2C,$A5
    fcb $54,$01
    fcb $3F,$BE
    fcb $6E
LF8C6
    fcb $A7,$0F,$54,$0B
    fcb $01
    fcb $8A,$FF,$DA
    fcb $13,$FF
    fcb $13,$41
    fcb $3F,$93
    fcb $13,$E5
    fcb $0E
    fcb $09
    fcb $41
    fcb $3F,$93
    fcb $13,$65
    fcb $25,$AE
    fcb $13,$7F
    fcb $0C
    fcb $8B,$65,$29
    fcb $2E,$A7
    fcb $41
    fcb $3F,$13
    fcb $0C
    fcb $AC,$13,$6F
    fcb $34,$A8
    fcb $13,$41
    fcb $3F,$35
    fcb $9D,$13
    fcb $75,$B3,$09
    fcb $41
    fcb $3F,$0C
    fcb $AB,$75,$29
    fcb $AC,$41,$3F
    fcb $0C
    fcb $0C
    fcb $AD,$FF,$22
    fcb $FF
    fcb $FF
    fcb $13,$41
    fcb $9C,$FF
    fcb $33,$41
    fcb $9C,$13
    fcb $FF
    fcb $0E
    fcb $41
    fcb $9C,$FF
    fcb $41
    fcb $BF
    fcb $13,$FF
    fcb $13,$77
    fcb $37,$93,$13
    fcb $E8
    fcb $0E
    fcb $42,$AA,$0E
    fcb $25,$E8
    fcb $42,$AA,$E8
    fcb $42,$B2,$33
    fcb $E9
    fcb $09
    fcb $77,$37,$86,$E9
    fcb $09
    fcb $E5,$FF
    fcb $0F
    fcb $77,$B7,$23,$FF
    fcb $FF
    fcb $EB
    fcb $09
    fcb $42,$AA,$EB
    fcb $42,$A9,$FF
    fcb $13,$42
    fcb $A9
    fcb $FF
    fcb $33,$42
    fcb $A9
    fcb $FF
    fcb $12,$42
    fcb $88,$FF,$42,$AA
    fcb $13,$FF
    fcb $13,$41
    fcb $21
    fcb $93,$FF
    fcb $24,$FF
    fcb $09
    fcb $10
    fcb $65
    fcb $A4,$13,$41
    fcb $21
    fcb $0C
    fcb $01
    fcb $95,$0A
    fcb $25,$FF
    fcb $13,$41
    fcb $95,$09
    fcb $11
LF96C
    fcb $25,$FF
    fcb $13,$42
    fcb $8D,$13,$E5
LF973
    fcb $0E
    fcb $09
    fcb $41
    fcb $21
    fcb $8C,$EF,$13
    fcb $41
    fcb $21
    fcb $9F
    fcb $6F
    fcb $25,$B3
    fcb $41
    fcb $21
    fcb $0F
    fcb $AB,$6F,$29
    fcb $2E,$A7
    fcb $41
    fcb $21
    fcb $1F,$0C
    fcb $AC,$09,$F5
    fcb $10
    fcb $21
    fcb $41
    fcb $0A
    fcb $96,$E7,$41
    fcb $8A,$EA,$41
    fcb $8A,$FF,$13
    fcb $41
    fcb $95,$FF
    fcb $33,$41
    fcb $95,$FF
    fcb $41
    fcb $A1
    fcb $13,$FF
    fcb $13,$D3
    fcb $09
    fcb $10
    fcb $FF
    fcb $13,$FF
    fcb $07
    fcb $11
    fcb $FF
    fcb $13,$FF
    fcb $13,$10
    fcb $FF
    fcb $13,$D3
    fcb $09
    fcb $E4,$13
    fcb $41
    fcb $95,$09
    fcb $10
    fcb $FF
    fcb $24,$13
    fcb $FF
    fcb $F6
    fcb $25,$32
    fcb $47,$A3,$72,$A9
    fcb $09
    fcb $7C,$93,$09
LF9CE
    fcb $10
    fcb $F2
    fcb $09
    fcb $F3
    fcb $F2
    fcb $09
    fcb $47,$AF,$FF,$0E
    fcb $0B
    fcb $D3,$72
    fcb $A9
    fcb $47,$07,$0E,$8C
    fcb $F2
    fcb $F3
    fcb $13,$76
    fcb $25,$AE
LF9E6
    fcb $13,$53
    fcb $23,$0C
    fcb $8B,$13,$76
    fcb $25,$AE
    fcb $53,$23
    fcb $07
    fcb $07
    fcb $8B,$09,$10
    fcb $FF
    fcb $37,$FF,$0D
    fcb $F7
    fcb $DF,$F7
    fcb $71
    fcb $9F
LF9FF
    fcb $FF
    fcb $2F,$D3
    fcb $09
    fcb $10
    fcb $0C
    fcb $F3
    fcb $13,$4C
    fcb $AB,$09,$10
    fcb $FF
    fcb $33,$13
    fcb $FF
    fcb $09
    fcb $10
    fcb $6C
    fcb $B9
    fcb $13,$6D
    fcb $93,$09
    fcb $10
    fcb $6D
    fcb $25,$2E
    fcb $B4
    fcb $50
    fcb $0C
    fcb $0B
LFA1F
    fcb $02
    fcb $8D,$66,$35
    fcb $AC,$68,$1E
    fcb $AD,$65,$B2
    fcb $FC
    fcb $E5,$D3
    fcb $61
    fcb $32,$AE
    fcb $74,$8B,$13
    fcb $61
    fcb $B2
    fcb $0E
    fcb $F4
    fcb $11
    fcb $61
    fcb $B2
    fcb $FC
    fcb $61
    fcb $A4,$47,$07
    fcb $01
    fcb $95,$09
    fcb $10
    fcb $E1,$13
    fcb $53,$8F
    fcb $E1,$33
    fcb $35,$C7
    fcb $E1,$D3
    fcb $69
    fcb $27,$A8,$D4
    fcb $E9
    fcb $D3,$13
    fcb $F9
    fcb $C6
    fcb $F9
    fcb $D3,$F5
    fcb $D6,$FF
    fcb $C7
    fcb $13,$FF
    fcb $13,$47
    fcb $07
    fcb $A8,$75,$AC,$68
    fcb $1E,$AD
    fcb $6F
    fcb $35,$B2
    fcb $68
    fcb $BA,$FF
    fcb $E8
    fcb $13,$FF
    fcb $13,$41
    fcb $0A
    fcb $93,$69
    fcb $B6
    fcb $41
    fcb $3D,$0C
    fcb $A3,$13,$FF
LFA7B
    fcb $29,$0E
    fcb $41
    fcb $BD
    fcb $E5,$34
    fcb $41
    fcb $3D,$87
    fcb $33,$35
    fcb $67,$25
    fcb $B3
    fcb $41
    fcb $24,$01
    fcb $0A
    fcb $07
    fcb $07
    fcb $B7
    fcb $E7,$41
    fcb $A4,$FF,$13
    fcb $41
    fcb $A2,$13,$22
    fcb $09
    fcb $FF
    fcb $41
    fcb $BD
    fcb $FF
    fcb $0F
    fcb $41
    fcb $8A,$72,$25
    fcb $A1
    fcb $41
    fcb $22,$27
    fcb $94,$09
    fcb $E8
    fcb $E8
    fcb $E8
    fcb $41
    fcb $A4,$FF,$41
    fcb $A4,$13,$FF
    fcb $13,$54
LFAB5
    fcb $02
    fcb $B2
    fcb $13,$61
    fcb $B6
    fcb $5B,$1A
    fcb $A3,$13,$65
    fcb $32,$A5
    fcb $5B,$BC
    fcb $13,$6F
    fcb $35,$B2
    fcb $60
    fcb $B3
    fcb $6F
    fcb $B7
    fcb $5B,$A0
    fcb $65
    fcb $21
    fcb $B2
    fcb $34,$5B
    fcb $BB
LFAD3
    fcb $F9
    fcb $0E
    fcb $2E,$5B
    fcb $8C,$FF,$12
    fcb $F9
    fcb $FF
    fcb $09
    fcb $DB,$FF
    fcb $FF
    fcb $13,$EE
LFAE2
    fcb $4C,$8B,$FF
    fcb $13,$C6
    fcb $EE
    fcb $24,$46
    fcb $8B,$13,$10
    fcb $65
    fcb $A4,$13,$46
    fcb $01
    fcb $95,$09
    fcb $11
    fcb $65
    fcb $A4,$13,$53
    fcb $01
    fcb $95,$65
    fcb $AE,$53,$0C
    fcb $8B,$E5,$34
    fcb $46,$8C,$13,$10
    fcb $FF
    fcb $0B
    fcb $C6
    fcb $65
    fcb $B2
    fcb $53,$B3
    fcb $FF
    fcb $0B
    fcb $D3,$E5
    fcb $D3,$F2
    fcb $09
    fcb $46,$B3,$FF,$0E
    fcb $0B
    fcb $C6
    fcb $FF
    fcb $0E
    fcb $0F
    fcb $10
    fcb $09
    fcb $CC
    fcb $0F
    fcb $0E
    fcb $FF
    fcb $0E
    fcb $0F
    fcb $CC
LFB26
    fcb $09
    fcb $11
    fcb $FF
    fcb $0E
    fcb $0F
    fcb $CC
    fcb $FF
    fcb $0E
    fcb $0F
    fcb $C6
    fcb $F2
    fcb $F4
    fcb $11
    fcb $FF
    fcb $2F,$2E
    fcb $F1
    fcb $67,$A8
    fcb $C6
    fcb $6C
    fcb $A4,$46,$3E
    fcb $01
    fcb $95,$67
    fcb $AE,$46,$8B
    fcb $71
    fcb $35,$A5
    fcb $53,$02
    fcb $A9
    fcb $FF
    fcb $CC
    fcb $13,$FF
    fcb $13,$41
    fcb $0A
    fcb $94,$FF
    fcb $41
    fcb $8A,$13,$FF
    fcb $13,$42
    fcb $2A,$94
    fcb $13,$FF
    fcb $2E,$FF
    fcb $FF
    fcb $13,$42
    fcb $A9
    fcb $FF
    fcb $42,$AA,$13
    fcb $FF
    fcb $13,$47
    fcb $07
    fcb $AD,$EF,$23
    fcb $09
    fcb $6D
    fcb $B5
    fcb $2C,$FF
    fcb $FF
    fcb $65
    fcb $21
    fcb $A4,$6D,$13
    fcb $01
    fcb $95,$61
    fcb $35,$27
    fcb $A8,$6D,$1A,$A8
    fcb $FF
    fcb $ED
    fcb $E2,$D0
    fcb $13,$FF
    fcb $13,$47
    fcb $07
    fcb $90
    fcb $6F
    fcb $B6
    fcb $50
    fcb $1F,$A3
    fcb $FF
    fcb $2D,$FF
    fcb $FF
    fcb $D0,$13
    fcb $FF
    fcb $13,$47
    fcb $07
    fcb $8B,$25,$E7
    fcb $0F
    fcb $4B,$01,$8A
    fcb $E7,$32
    fcb $6C
    fcb $01
    fcb $A4,$E7,$09
    fcb $6C
    fcb $01
    fcb $A4,$67,$AC
    fcb $0B
    fcb $6C
    fcb $01
    fcb $24,$BE
    fcb $E7,$EC
    fcb $EB
    fcb $13,$6C
    fcb $02
    fcb $A9
    fcb $EB
    fcb $33,$6C
    fcb $02
    fcb $A9
    fcb $EB
    fcb $6C
    fcb $02
    fcb $AA,$13,$6F
    fcb $B7
    fcb $13,$4B
    fcb $A0
    fcb $2E,$FF
    fcb $FF
    fcb $09
    fcb $10
    fcb $F5
    fcb $4B,$31,$96
    fcb $FF
    fcb $12,$F8
    fcb $47,$B4,$4B,$02
    fcb $8D,$FF,$CB
    fcb $13,$FF
    fcb $13,$F5
    fcb $E6,$13
    fcb $4F,$A3,$09
    fcb $10
    fcb $F2
    fcb $13,$F3
    fcb $09
    fcb $10
    fcb $72,$B3,$13
    fcb $73,$AB,$F2
    fcb $FA
    fcb $13,$6E
    fcb $A5,$6E,$0F
    fcb $8B,$0F,$6E
    fcb $A5,$6E,$0F
    fcb $8B,$11,$F7
    fcb $2E,$E0
    fcb $F7
    fcb $F5
    fcb $13,$76
    fcb $25,$B2
    fcb $75,$23,$B3
    fcb $F6
    fcb $4F,$A3,$11
    fcb $ED
    fcb $4F,$90,$FF
    fcb $0E
    fcb $0B
    fcb $F5
    fcb $FF
    fcb $0E
    fcb $25,$2E
    fcb $F5
    fcb $FF
    fcb $0E
    fcb $29,$09
    fcb $F5
    fcb $EC
    fcb $24,$75
    fcb $AD,$75,$27
    fcb $28,$B4
    fcb $57,$17,$02
    fcb $8D,$75,$27
    fcb $A8,$4F,$0F,$A8
    fcb $0C
    fcb $75,$B2,$FA
    fcb $10
    fcb $75,$B2,$60
    fcb $B3
    fcb $13,$F5
    fcb $E0,$10
    fcb $F5
    fcb $33,$09
    fcb $E0,$75
    fcb $B3
    fcb $4F,$B7,$75
    fcb $2C,$A4
    fcb $5E,$01
    fcb $95,$0E
    fcb $F5
LFC4F
    fcb $0E
    fcb $2C,$CF
    fcb $75,$B0,$5F
    fcb $02
    fcb $89
    fcb $F5
    fcb $E0,$F9
    fcb $C5
    fcb $69
    fcb $2E,$A7
    fcb $75,$0C,$AC
    fcb $E9
    fcb $C5
    fcb $6F
    fcb $B2
    fcb $FA
    fcb $6F
    fcb $AB,$13,$5E
    fcb $02
    fcb $A9
    fcb $6F
    fcb $AB,$33,$5E
    fcb $02
    fcb $A9
    fcb $6F
    fcb $AB,$5E,$02
    fcb $AA,$6F,$A4
    fcb $13,$5E
    fcb $01
    fcb $95,$EF
    fcb $24,$DE
    fcb $EF
    fcb $DF,$FF
    fcb $25,$F5
    fcb $FF
    fcb $13,$F5
    fcb $61
    fcb $B2
    fcb $FA
    fcb $E1,$F5
    fcb $13,$6E
    fcb $2C,$B9
    fcb $75,$0B,$2D
    fcb $93,$13
    fcb $6E
    fcb $23,$A5
    fcb $6E
    fcb $0F
    fcb $0B
    fcb $B7
    fcb $6E
    fcb $07
    fcb $B4
    fcb $75,$0B,$02
    fcb $8D,$23,$FF
    fcb $2E,$CF
    fcb $FF
    fcb $2E,$27
    fcb $D7,$13
    fcb $11
    fcb $FF
    fcb $2E,$CF
    fcb $29,$EE
    fcb $4F,$8B,$09
    fcb $10
    fcb $EE
    fcb $13,$4F
    fcb $8B,$FF,$33
    fcb $34,$13
    fcb $F5
    fcb $E6,$0E
    fcb $57,$A8,$74
    fcb $28,$25
    fcb $B2
    fcb $4F,$36,$B3
    fcb $73,$B3,$13
    fcb $57,$17,$37
    fcb $B7
    fcb $FF
    fcb $D8,$73
    fcb $39,$23
    fcb $A8,$77,$37,$06
    fcb $01
    fcb $AA,$13,$FF
    fcb $13,$42
    fcb $09
LFCE2
    fcb $93,$E8
    fcb $E8
    fcb $65
    fcb $2F,$B0
    fcb $42,$09,$13
    fcb $02
    fcb $89
    fcb $6F
    fcb $B7
    fcb $42,$09,$A0
    fcb $75,$B4,$13
LFCF5
    fcb $42,$09,$1E
    fcb $02
    fcb $8D,$FF,$30
    fcb $FF
    fcb $FF
    fcb $42,$89,$13
    fcb $FF
    fcb $13,$42
    fcb $2A,$31
    fcb $9F
    fcb $75,$21,$B2
    fcb $42,$08,$30
    fcb $BA,$75
    fcb $A5,$13,$42
    fcb $2A,$31
    fcb $9F
    fcb $F5
    fcb $42,$08,$B0
    fcb $13,$FF
    fcb $13,$FB
    fcb $13,$E5
    fcb $0E
    fcb $09
    fcb $4E,$93,$32
    fcb $FF
    fcb $FF
    fcb $11
    fcb $FF
    fcb $E7,$FF
    fcb $CE
    fcb $13,$FF
    fcb $13,$47
    fcb $07
    fcb $37,$B7,$E8
    fcb $E5,$09
    fcb $69
    fcb $2F,$AE
    fcb $66,$0F
    fcb $8B,$09,$75
    fcb $B2
LFD3E
    fcb $09
    fcb $66,$B3
    fcb $75,$B2,$09
    fcb $65
    fcb $B3
    fcb $09
    fcb $F5
    fcb $09
    fcb $66,$96
    fcb $09
    fcb $73,$B5,$09
    fcb $65
    fcb $96,$09,$65
    fcb $A4,$13,$6B
    fcb $01
    fcb $95,$61
    fcb $29,$A4
    fcb $77,$37,$07,$07
    fcb $01
    fcb $95,$0E
    fcb $69
    fcb $2F,$AE
    fcb $65
LFD66
    fcb $0F
    fcb $8B,$0A,$FF
    fcb $13,$EB
    fcb $09
    fcb $10
    fcb $0A
    fcb $25,$FF
    fcb $13,$EB
    fcb $09
    fcb $11
    fcb $14,$FF
    fcb $13,$EB
    fcb $35,$FF
    fcb $13,$F7
    fcb $13,$10
    fcb $09
    fcb $FF
    fcb $13,$EB
    fcb $13,$63
    fcb $A8,$77,$37,$02
    fcb $A9
    fcb $FF
    fcb $23,$0F
    fcb $FF
    fcb $09
    fcb $ED
    fcb $6B
    fcb $90
    fcb $09
    fcb $FF
    fcb $2E,$07
    fcb $EB
    fcb $FF
    fcb $F7
    fcb $FF
    fcb $07
    fcb $33,$42
    fcb $91,$63
    fcb $A8,$42,$B2,$13
    fcb $FF
    fcb $13,$42
    fcb $0D
    fcb $93,$13
    fcb $68
    fcb $25,$80
    fcb $09
    fcb $52,$93
    fcb $13,$68
    fcb $A5,$13,$52
    fcb $8F
    fcb $EF
    fcb $13,$42
    fcb $0D
    fcb $9F
    fcb $6F
    fcb $24,$21
    fcb $B9
    fcb $42,$0D,$1F
    fcb $21
    fcb $94,$68
    fcb $A1
    fcb $0E
    fcb $13,$52
    fcb $9A,$13
    fcb $68
    fcb $29,$B3
    fcb $13,$52
    fcb $0C
    fcb $37,$B7,$13
    fcb $68
    fcb $25,$B9
    fcb $52,$94
    fcb $13,$68
    fcb $25,$32
    fcb $A5,$52,$AF
    fcb $68
    fcb $25,$B2
    fcb $76,$B3,$68,$25
    fcb $29,$B2
    fcb $52,$AF
    fcb $13,$68
    fcb $25,$AD
    fcb $10
    fcb $52,$07
    fcb $90
    fcb $68
    fcb $25,$33
    fcb $A5,$13,$52
    fcb $13,$AB
    fcb $13,$68
    fcb $25,$AE
    fcb $52,$07
    fcb $8B,$68,$32
    fcb $2F,$35
    fcb $27,$A8,$13
    fcb $5D,$27
    fcb $9F
LFE0B
    fcb $68
    fcb $2F,$33
    fcb $A5,$52,$35
    fcb $B7
    fcb $68
    fcb $2F,$35
    fcb $27,$A8,$13
    fcb $52,$B5
    fcb $13,$68
    fcb $35,$B3
    fcb $52,$0F
    fcb $37,$B7,$68
    fcb $A5,$13,$D2
    fcb $E8
    fcb $DD,$09
    fcb $10
    fcb $65
LFE2B
    fcb $A4,$13,$42
    fcb $0D
    fcb $0C
    fcb $01
    fcb $95,$33
    fcb $E9
    fcb $09
    fcb $2E,$42
    fcb $B2
    fcb $E9
    fcb $09
    fcb $E5,$75
    fcb $B2
    fcb $09
    fcb $42,$32,$B3
    fcb $F5
    fcb $21
    fcb $42,$32,$96
    fcb $13,$77
    fcb $AF
    fcb $42,$0D,$9F
    fcb $FF
    fcb $34,$FF
    fcb $FF
    fcb $33,$42
    fcb $91,$FF
    fcb $42,$8D,$13
    fcb $FF
    fcb $13,$71
    fcb $9F
    fcb $EE
    fcb $29,$59
    fcb $16,$8B,$13
    fcb $EE
    fcb $4F,$8B,$13
    fcb $70
    fcb $AF
    fcb $4F,$02,$09
    fcb $98,$0D,$F2
    fcb $09
    fcb $56,$B3,$F2
    fcb $09
    fcb $71
    fcb $16,$B3,$F2
    fcb $11
    fcb $F3
    fcb $FF
    fcb $0E
    fcb $13,$CF
    fcb $FF
    fcb $0E
    fcb $0E
    fcb $CF
    fcb $F9
    fcb $C6
    fcb $13,$27
    fcb $FF
    fcb $09
    fcb $FF
    fcb $27,$FF,$0B
    fcb $FF
    fcb $27,$FF,$09
    fcb $EE
    fcb $0D
    fcb $FF
    fcb $DF,$FF
    fcb $71
    fcb $96,$13,$FF
LFE97
    fcb $13,$63
    fcb $93,$69
    fcb $25,$B7
    fcb $63
    fcb $31
    fcb $9F
    fcb $FF
    fcb $E3,$13
    fcb $FF
    fcb $13,$41
    fcb $21
    fcb $0F
    fcb $01
    fcb $3F,$3E
    fcb $31
    fcb $96,$13,$65
    fcb $32,$A5
LFEB1
    fcb $6E
    fcb $B4
    fcb $13,$61
    fcb $B3
LFEB6
    fcb $13,$6E
    fcb $0F
    fcb $AB,$E1,$33
    fcb $6E
    fcb $98,$E1,$34
    fcb $6E
    fcb $97,$61,$AE
    fcb $6E
    fcb $18,$8B
    fcb $68
    fcb $21
    fcb $B4
    fcb $70
    fcb $18,$02
    fcb $8D,$E8,$2F
    fcb $11
    fcb $F9
    fcb $68
    fcb $AF
    fcb $79,$9F,$EF
    fcb $2D,$6E
    fcb $8F
    fcb $E8
    fcb $F0
    fcb $61
    fcb $B2
    fcb $6E
    fcb $BA,$6F
    fcb $B2
    fcb $0E
    fcb $6E
    fcb $B3
    fcb $F2
    fcb $CE
    fcb $FF
    fcb $EE
    fcb $13,$FF
    fcb $13,$47
    fcb $02
    fcb $29,$B7
    fcb $13,$FF
    fcb $EB
    fcb $FF
    fcb $42,$29,$B7
    fcb $6F
    fcb $35,$B2
    fcb $59,$BA
    fcb $13,$FF
    fcb $13,$6E
    fcb $86,$6F,$35
    fcb $2E,$A7
    fcb $59,$0F
    fcb $AC,$13,$6F
    fcb $B5
    fcb $59,$9F
    fcb $65
    fcb $21
    fcb $B2
    fcb $10
    fcb $59,$BC
    fcb $13,$65
    fcb $B3
    fcb $59,$07
    fcb $37,$B7,$13
    fcb $FF
    fcb $D9,$09
    fcb $11
    fcb $FF
    fcb $13,$D3
    fcb $09
    fcb $11
    fcb $FF
    fcb $29,$D3
    fcb $13,$10
    fcb $FF
    fcb $13,$C6
    fcb $13,$10
    fcb $FF
    fcb $09
    fcb $C6
    fcb $13,$10
    fcb $FF
LFF34
    fcb $0E
    fcb $0F
    fcb $10
    fcb $09
    fcb $CC
    fcb $13,$10
    fcb $FF
    fcb $0E
    fcb $09
    fcb $C6
    fcb $FF
    fcb $CC
    fcb $13,$FF
    fcb $13,$6B
    fcb $93,$FF
    fcb $3A,$FF
    fcb $FF
    fcb $EB
    fcb $D0,$6B
    fcb $3C,$B5
    fcb $D1,$6E
    fcb $0F
    fcb $8B,$D2,$42
    fcb $0D
    fcb $9F
    fcb $D3,$5D
    fcb $0E
    fcb $93,$D4
    fcb $68
    fcb $BA,$D5
    fcb $68
    fcb $06
    fcb $A3,$D6,$77
    fcb $37,$0C,$02
    fcb $29,$B7
    fcb $D7,$77
    fcb $37,$07,$23
    fcb $0C
    fcb $8B,$D8,$54
    fcb $02
    fcb $8D,$D9,$4B
    fcb $06
    fcb $8B,$FE
LFF79
    fcb $F8
LFF7A
    fcb $11
    fcb $F8
    fcb $3E,$F8
    fcb $CE
    fcb $F9
    fcb $18,$F9
    fcb $51
    fcb $F9
    fcb $A5,$FA,$5B
    fcb $FA
    fcb $6D
LFF89
    fcb $FA
    fcb $B1
    fcb $FA
    fcb $E0,$FB
    fcb $4C,$FB,$55
    fcb $FB
    fcb $66,$FB
    fcb $84,$FB
    fcb $96,$FB,$DC
    fcb $FC
    fcb $D4,$FD
    fcb $00
    fcb $FD
    fcb $19,$FD
    fcb $2B,$FD
    fcb $99
    fcb $FE
    fcb $56,$FE,$95
    fcb $FE
    fcb $A2,$FE,$E9
    fcb $FE
    fcb $F7
    fcb $FF
    fcb $41
    fcb $FF
    fcb $4B,$00,$00
    fcb $00
    fcb $00
    fcb $00
    fcb $00
    fcb $00
    fcb $00
    fcb $00
    fcb $00
    fcb $00
    fcb $00
    fcb $00
    fcb $00
    fcb $00
    fcb $00
    fcb $00
    fcb $00
    fcb $00
    fcb $00
    fcb $00
    fcb $00
    fcb $00
    fcb $00
    fcb $00
    fcb $00
    fcb $00
    fcb $00
    fcb $00
    fcb $00
    fcb $00
    zmb 32
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
    FDB RESET1 6309 Trap Vector
    FDB RESET1 SWI3
    FDB RESET1 SWI2
    FDB TIMER1 FIRQ - TIMER
    FDB INT1 IRQ - Allophone Load Request
    FDB RESET1 SWI
    FDB INT3 - NMI New Byte from Host
    FDB RESET1 - RESET

