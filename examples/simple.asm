* 6809 assembly generated from Clover source

    include BOSS9.inc
    org $200
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
    ;     int8 a = 2;
    ;     int8 b;
    LDA #$02
    PSHS A
    PULS A
    STA 0,U
    ;     switch (a) {
    ;         case 1: b = 1;
    LDA 0,U
    PSHS A
    LEAX L1
    PSHS X
    LDD #3
    PSHS D
    JSR switch1
    PULS X
    JMP 0,X
L1
    FCB 1
    LBRA L2
    FCB 2
    LBRA L3
    FCB 3
    LBRA L4
    ;         case 2: b = 2;
    ;         case 3: b = 3;
    ;         default: b = 4;
    ;     }
    LDA #$04
    PSHS A
    PULS A
    STA 1,U
    LBRA L5
L2
    LDA #$01
    PSHS A
    PULS A
    STA 1,U
    LBRA L5
L3
    LDA #$02
    PSHS A
    PULS A
    STA 1,U
    LBRA L5
L4
    LDA #$03
    PSHS A
    PULS A
    STA 1,U
L5
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
6