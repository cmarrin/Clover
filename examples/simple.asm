* 6809 a   �  vgenerated from Clover source

include BOSS9.inc
org $200
�LDA #$0c
PSHS A
PULS A
STA 0,Y
LDD #$0018
PSHS D
PULS D
STD 1,Y
4�LEAX 2,U
PSHS X
 ILEAX 0,?
PSHS X
LDA #$02
PSHS A
8-LEAX 0,?
PSHS X
LDA #$01
PSHS A
2 v1=%d, v2=%d
 X�LDD 1,U
PSHS D
-LDA 2,U
PSHS A
2 a.b=%d, a.c=%d
 X�LDA 0,Y
PSHS A
2	 xxx=%d
 X�LDD #$0000
PSHS D
LDD #$0000
PSHS D
7�LDA #$09
PSHS A
PULS A
STA 0,Y
4