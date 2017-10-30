sub reset = $e000
    +for jmp

sub nmi_routine = $e003
    rti

sub irq_routine = $e004
    rti

sub interrupts = $fffa
    +raw16 #nmi_routine
    +raw16 #reset
    +raw16 #irq_routine
