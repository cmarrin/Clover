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
    LEAS -12,S
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
    ; struct A
    ; {
    ;     int16 b;
    ;     int16 c;
    ; };
    ; 
    ; function int16 main()
    ; {
    ;     A a[3];
    ;     a[1].b = a[2].c;
    ;     return 0;
    LEAX -12,U
    LDA #$02
    LDB #4
    MUL
    LEAX D,X
    LEAX 2,X
    LDD 0,X
    LEAX -12,U
    PSHS D
    LDA #$01
    LDB #4
    MUL
    LEAX D,X
    PULS D
    STD 0,X
    ; }
    LDD #$0000
    TFR U,S
    PULS U
    RTS

Constants

String

    end $200
