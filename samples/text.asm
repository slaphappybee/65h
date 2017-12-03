extern w8 ppu_control_0 = $2000
extern w8 ppu_control_1 = $2001
extern w8 ppu_status = $2002
extern w8 vram_addr_0 = $2005
extern w8 vram_addr_1 = $2006
extern w8 vram_io = $2007

const w16 vram_palette = $3f00
const w16 vram_name_0 = $2000
const w16 vram_name_0_s = $2080
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
        +tua vram_io attr_table,X
        inx

    +tux_io vram_addr_1 #vram_name_0_s

    sec
    ldx #$0
    +for ldy #$20 bne dey
        lda text,X
        sbc #$20
        sta vram_io
        inx

    lda #$0
    sta vram_addr_0
    sta vram_addr_0
    
    +tua ppu_control_1 #$a

    +for jmp

sub nmi_routine = $f000
    rti

sub irq_routine = $f001
    rti

sub palette = $f010
    +raw8 #$0f #$30 #$30 #$30
    +raw8 #$0f #$30 #$30 #$30
    +raw8 #$0f #$30 #$30 #$30
    +raw8 #$0f #$30 #$30 #$30

sub text = $f020
    +raw8 #$44 #$4f #$4e #$27 #$54 #$20 #$50 #$55
    +raw8 #$54 #$20 #$4c #$41 #$42 #$45 #$4c #$53
    +raw8 #$20 #$4f #$4e #$20 #$55 #$53 #$20 #$20
    +raw8 #$20 #$20 #$20 #$20 #$20 #$20 #$20 #$20

sub attr_table = $f040
    +raw8 #$00 #$00 #$00 #$00 #$00 #$00 #$00 #$00
    +raw8 #$00 #$00 #$00 #$00 #$00 #$00 #$00 #$00
    +raw8 #$00 #$00 #$00 #$00 #$00 #$00 #$00 #$00
    +raw8 #$00 #$00 #$00 #$00 #$00 #$00 #$00 #$00
    +raw8 #$00 #$00 #$00 #$00 #$00 #$00 #$00 #$00
    +raw8 #$00 #$00 #$00 #$00 #$00 #$00 #$00 #$00
    +raw8 #$00 #$00 #$00 #$00 #$00 #$00 #$00 #$00
    +raw8 #$00 #$00 #$00 #$00 #$00 #$00 #$00 #$00

sub interrupts = $fffa
    +raw16 #nmi_routine
    +raw16 #reset
    +raw16 #irq_routine
