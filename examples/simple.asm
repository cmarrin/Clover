* 6809 assembly generated from Clover source

    include BOSS9.inc
    org $200

    LEAS -0,S
    TFR S,Y
    LEAS -2,S
    JSR Simple_main
    LEAS 2,S
    JMP exit

Simple_f
    PSHS U
    TFR S,U
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
    ; function int8 f(int8 x, int8 y)
    ; {
    ;     return x + y;
    ; }
    LDA 6,U
    PSHS A
    LDA 7,U
    ADDA 0,S
    LEAS 1,S
    TFR U,S
    PULS U
    RTS

Simple_main
    PSHS U
    TFR S,U
    LEAS -3,S
    ; 
    ; function int16 main()
    ; {
    ;     int8 a = 5;
    ;     int8 b = 6;
    LDA #$05
    STA -1,U
    ;     int8 c = f(a, b + 1);
    LDA #$06
    STA -2,U
    ;     
    LDA -2,U
    PSHS A
    LDA #$01
    ADDA 0,S
    LEAS 1,S
    PSHS A
    LDA -1,U
    PSHS A
    LEAS -2,S
    JSR f
    LEAS 2,S
    LEAS 2,S
    STA -3,U
    ;     return 0;
    ; }
    LDD #$0000
    TFR U,S
    PULS U
    RTS

Constants

String

    end $200
