* 6809 assembly generated from Clover source

    include BOSS9.inc
    org $200

    LEAS -0,S
    TFR S,Y
    LEAS -2,S
    JSR Simple_main
    LEAS 2,S
    JMP exit

Simple_main
    PSHS U
    TFR S,U
    LEAS -2,S
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
    ; function int16 main()
    ; {
    ;     int8 a = 5;
    ;     int8 b = a + 1;
    LDA #$05
    STA -1,U
    ;     
    LDA -1,U
    PSHS A
    LDA #$01
    ADDA 0,S
    LEAS 1,S
    STA -2,U
    ;     if (a < b) {
    LDA -1,U
    PSHS A
    LDA -2,U
    CMPA 0,S
    BLT L3
    CLRA
    BRA L4
L3
    LDA #1
L4
    LEAS 1,S
    BEQ L5
    ;         core.printf("Hello\n");
    ;     }
    LDD #String+$0
    PSHS D
    JSR printf
L5
    ;     
    ;     return 0;
    ; }
    LDD #$0000
    TFR U,S
    PULS U
    RTS

Constants

String
    FCC "Hello"
    FCB $0a
    FCB $00

    end $200
