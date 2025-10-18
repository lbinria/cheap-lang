; Set delay timer to 120
LD V0, 120  ; Load 120 into V0
LD dt, V0   ; Set delay timer to 120

; Set sound timer to 60
LD V1, 60   ; Load 60 into V1
LD st, V1   ; Set sound timer to 60

; Check delay timer in a loop
Loop:
    LD V2, dt  ; Load delay timer into V2
    SE V2, V0   ; Skip next instruction if delay timer is 0
    JP Loop     ; Jump back to the Loop if not zero