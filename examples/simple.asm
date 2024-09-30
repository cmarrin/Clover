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
    ; function int16 main()
    ; {
    ;     int8 a = 5;
    ;     int8 b = 6;
    LDA #$05
    STA -1,U
    ;     int8 c = ~a;
    LDA #$06
    STA -2,U
    ;     
    LDA -1,U
    COMA
    STA -3,U
    ;     if (a + 2 == b + 3)
    ;         core.printf("Passed\n");
    LDA #$03
    PSHS A
    LDA -2,U
    ADDA 0,S
    LEAS 1,S
    PSHS A
    LDA #$02
    PSHS A
    LDA -1,U
    ADDA 0,S
    LEAS 1,S
    CMPA 0,S
    LEAS 1,S
    BNE L1
    ;     else
    LDD #String+$0
    PSHS D
    JSR printf
    BRA L2
L1
    ;         core.printf("Failed\n");
    ;     
    LDD #String+$8
    PSHS D
    JSR printf
L2
    ;     return 0;
    ; }
    LDD #$0000
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
