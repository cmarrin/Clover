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
    ;     uint8 a = 5;
    ;     uint8 b = 6;
    LDA #5
    STA -1,U
    ;     uint8 c = a * 10;
    LDA #6
    STA -2,U
    ;     
    LDA #10
    PSHS A
    LDA -1,U
    PSHS A
    CLR ,-S
    LDB 1,S
    LDA 2,S
    MUL
    LEAS 3,S
    TFR B, A
    STA -3,U
    ;     if (a == b)
    ;         core.printf("Passed\n");
    LDA -1,U
    CMPA -2,U
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
