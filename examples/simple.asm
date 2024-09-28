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
    LEAS -4,S
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
    ;     int16 b = a + 1;
    LDA #$05
    STA -1,U
    ;     int8 c = b - 1;
    LDA -1,U
    PSHS A
    LDA #$01
    ADDA 0,S
    LEAS 1,S
    TFR A,B
    SEX
    STD -3,U
    ;     
    LDD -3,U
    PSHS D
    LDD #$0001
    PSHS D
    LDD 2,S
    SUBD 0,S
    LEAS 4,S
    TFR B,A
    STA -4,U
    ;     return 0;
    ; }
    LDD #$0000
    TFR U,S
    PULS U
    RTS

Constants

String

    end $200
