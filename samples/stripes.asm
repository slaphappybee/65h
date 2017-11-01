extern w8 ppu_status = $2002

sub reset = $e000
    cld
    sei

    +for bpl
        lda ppu_status

sub nmi_routine = $e007
    rti

sub irq_routine = $e008
    rti

sub interrupts = $fffa
    +raw16 #nmi_routine
    +raw16 #reset
    +raw16 #irq_routine
