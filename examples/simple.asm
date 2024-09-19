* 6809 assembly generated from Clover source

    include BOSS9.inc
    org $200
ó    ; //
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
    LEAX 0,?
    PSHS X
    PULS D
    STD 1,U
    LDA #$00
    PSHS A
    PULS A
    STA 2,U
    LDA 2,U
    PSHS A
    LDA #$04
    PSHS A
    LDA 1,S
    CMPA 0,S
    LEAS 2,S
    BLT L0
    CLRA
    BRA L2
L0
    LDA #1
L1
    PSHS A
QM    ;         core.printf("array[%d] = %d\n", i, *p);
    ;         p++;
    LDD 1,U
    PSHS D
    LDA 2,U
    PSHS A
0    LEAX String+$0
    PSHS X
Xå    ;     }
    LEAX 1,U
    PSHS X
    PULS X
    LDD 0,X
    PSHS D
    ADDD #2
    STD 0,X
    LEAX 2,U
    PSHS X
    PULS X
    LDA 0,X
    PSHS A
    ADDA #1
    STA 0,X
Wß    ;     
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