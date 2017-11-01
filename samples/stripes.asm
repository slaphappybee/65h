extern w8 ppu_control_0 = $2000
extern w8 ppu_control_1 = $2001
extern w8 ppu_status = $2002
extern w8 vram_addr_0 = $2005
extern w8 vram_addr_1 = $2006
extern w8 vram_io = $2007

const w16 vram_palette = $3f00
const w16 vram_attr_0 = $23c0

sub reset = $c000
    cld
    sei

    +for bpl
        lda ppu_status

    ldx #$0
    stx ppu_control_0
    stx ppu_control_1

    dex
    txs

    +tux_io vram_addr_1 #vram_palette

    ldx #$0
    +for ldy #$10 bne dey
        +tua vram_io palette,X
        inx

    +tux_io vram_addr_1 #vram_attr_0

    ldx #$0
    +for ldy #$40 bne dey
        +tua vram_io stripes,X
        inx

    +tua ppu_control_1 #$a

    +for jmp

sub nmi_routine = $f000
    rti

sub irq_routine = $f001
    rti

sub palette = $f010
    +raw8 #$30 #$30 #$30 #$30
    +raw8 #$16 #$16 #$16 #$16
    +raw8 #$1a #$1a #$1a #$1a
    +raw8 #$11 #$11 #$11 #$11

sub stripes = $f020
    +raw8 #$00 #$00 #$00 #$00 #$00 #$00 #$00 #$00
    +raw8 #$55 #$55 #$55 #$55 #$55 #$55 #$55 #$55
    +raw8 #$aa #$aa #$aa #$aa #$aa #$aa #$aa #$aa
    +raw8 #$ff #$ff #$ff #$ff #$ff #$ff #$ff #$ff
    +raw8 #$00 #$00 #$00 #$00 #$00 #$00 #$00 #$00
    +raw8 #$55 #$55 #$55 #$55 #$55 #$55 #$55 #$55
    +raw8 #$aa #$aa #$aa #$aa #$aa #$aa #$aa #$aa
    +raw8 #$ff #$ff #$ff #$ff #$ff #$ff #$ff #$ff

sub interrupts = $fffa
    +raw16 #nmi_routine
    +raw16 #reset
    +raw16 #irq_routine
