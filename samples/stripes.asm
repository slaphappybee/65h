extern w8 ppu_status = $2002

sub reset = $e000
    cld
    sei

    +for bpl

sub nmi_routine = $e006
    rti

sub irq_routine = $e007
    rti

sub interrupts = $fffa
    +raw16 #nmi_routine
    +raw16 #reset
    +raw16 #irq_routine
