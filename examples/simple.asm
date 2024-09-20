* 6809 assembly generated from Clover source

    include BOSS9.inc
    org $200
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
    ; //struct A
    ; //{
    ; //    int8 b = 12;
    ; //    int16 c = 24;
    ; //};
    ; //
    ; //const A arr[ ] = { 8, 9, 10, 11, 12, 13 };
    ; //
    ; //int8 xxx = 9;
    ; //
    ; 
    ; const uint16 array[4] = { 2, 4, 6, 8 };
    ; 
    ; function int16 main()
    ; {
    ;     uint16* p = &array[0];
    ;     for (uint8 i = 0; i < 4; i++) {
    LEAX Constants+0
    PSHS X
    PULS D
    STD 1,U
    LDA #$00
    PSHS A
    PULS A
    STA 2,U
L1
    LDA 2,U
    PSHS A
    LDA #$04
    PSHS A
    LDA 1,S
    CMPA 0,S
    LEAS 2,S
    BLT L2
    CLRA
    BRA L2
L2
    LDA #1
L3
    PSHS A
    PULS A
    BEQ L4
    ;         core.printf("array[%d] = %d\n", i, *p);
    ;         p++;
    LDD 1,U
    PSHS D
    LDA 2,U
    PSHS A
    PULS B
    CLRA
    PSHS D
    LEAX String+$0
    PSHS X
    JSR printf
    ;     }
    LEAX 1,U
    PSHS X
    PULS X
    LDD 0,X
    PSHS D
    ADDD #2
    STD 0,X
L5
    LEAX 2,U
    PSHS X
    PULS X
    LDA 0,X
    PSHS A
    ADDA #1
    STA 0,X
    BRA L1
L4
    ;     
    ; //    int16 a = 5;
    ; //    uint16 b = 6;
    ; //    int8 c = 8;
    ; //    int8 d = 7;
    ; // 
    ; //    bool x = a < b;
    ; //    
    ; //    if (a < b && c > d) {
    ; //        core.printf("Hello\n");
    ; //    }
    ;     
    ; //    A a;
    ; //    
    ; //    core.printf(" v1=%d, v2=%d\n", arr[1].b, arr[2].c);
    ; //    core.printf(" a.b=%d, a.c=%d\n", a.b, a.c);
    ; //    core.printf(" xxx=%d\n", xxx);
    ;     return 0;
    ; }
    LDD #$0000
    PSHS D
6