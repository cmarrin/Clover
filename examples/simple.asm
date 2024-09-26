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
    ; struct A
    ; {
    ;     int16 b;
    ;     int16 c;
    ; };
    ; 
    ; function int16 main()
    ; {
    ;     A a;
    ;     a.b = a.c;
    ;     return 0;
    LDD -2,U
    LEAX -4,U
    STD 0,X
    ; }
    LDD #$0000
    TFR U,S
    PULS U
    RTS

Constants

String

    end $200
