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
    LEAS -1,S
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
    ;     a *= 1;
    LDA #5
    STA -1,U
    ;     core.printf("a=%d\n", a);
    CLR ,-S
    LDA -1,U
    BPL L1
    NEG 0,S
    NEGA
L1
    TFR A,B
    LDA #1
    BPL L2
    NEG 0,S
    NEGA
L2
    MUL
    TST 0,S
    BPL L3
    NEGB
L3
    LEAS 1,S
    TFR B,A
    STA -1,U
    ;     return 0;
    LDA -1,U
    TFR A,B
    CLRA
    ADDD #0
    PSHS D
    LDD #String+$0
    PSHS D
    JSR printf
    ; }
    LDD #0
    TFR U,S
    PULS U
    RTS

Constants

String
    FCC "a=%d"
    FCB $0a
    FCB $00

    end $200
