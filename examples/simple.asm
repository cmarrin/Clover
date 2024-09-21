* 6809 assembly generated from Clover source

    include BOSS9.inc
    org $200
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
    ;     int16 a = 3;
    ;     int16 b = array[a];
    LDD #$0003
    PSHS D
    PULS D
    STD 1,U
    ; //    if (a == 1 && b == 3) {
    LEAX Constants+0
    PSHS X
    LDD 1,U
    PSHS D
    PULS D
    PSHS D
    PSHS D
    LDA #2
    MUL
    STD 0,S
    LDA 2,S
    LDB #2
    MUL
    ADDB 0,S
    PULS D
    LEAS 2,S
    ADDD 0,S
    STD 0,S
    PULS X
    LDD 0,X
    PSHS D
    PULS D
    STD 3,U
    ; //        b = 10;
    ; //    }
    ; //    uint16* p = &array[0];
    ; //    for (uint8 i = 0; i < 4; i++) {
    ; //        core.printf("array[%d] = %d\n", i, *p);
    ; //        p++;
    ; //    }
    ; //
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
    RTS
