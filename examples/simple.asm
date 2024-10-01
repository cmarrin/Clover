* 6809 assembly generated from Clover source

    include BOSS9.inc
    org $200

    TFR S,Y
    LEAS -2,S
    JSR Simple_main
    LEAS 2,S
    JMP exit

Simple_main
    PSHS U
    TFR S,U
    LEAS -3,S
    ; //
    ; //  simple.Clover
    ; //  Clover
    ; //
    ; //  Created by Chris Marrin on 5/25/24.
    ; //
    ; 
    ; struct Simple
    ; {
    ; 
    ; function int16_t main()
    ; {
    ;     int8_t a = 5;
    ;     uint8_t b = 6;
    LDA #5
    STA -1,U
    ;     int8_t c = a * 10;
    LDA #6
    STA -2,U
    ;     
    CLR ,-S
    LDA -1,U
    BPL L1
    NEG 0,S
    NEG A
L1
    TFR A,B
    LDA #10
    BPL L2
    NEG 0,S
    NEG A
L2
    MUL
    TST 0,S
    BPL L3
    NEGB
L3
    LEAS 3,S
    TFR B,A
    STA -3,U
    ;     if (a == b)
    ;         core.printf("Passed\n");
    LDA -2,U
    PSHS A
    LDA -1,U
    CMPA 0,S
    LEAS 1,S
    BNE L4
    ;     else
    LDD #String+$0
    PSHS D
    JSR printf
    BRA L5
L4
    ;         core.printf("Failed\n");
    ;     
    LDD #String+$8
    PSHS D
    JSR printf
L5
    ;     return 0;
    ; }
    LDD #0
    TFR U,S
    PULS U
    RTS

Constants

String
    FCC "Passed"
    FCB $0a
    FCB $00
    FCC "Failed"
    FCB $0a
    FCB $00

    end $200
