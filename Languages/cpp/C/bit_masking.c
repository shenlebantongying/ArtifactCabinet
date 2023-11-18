#include <stdint.h>
#include <stdio.h>

// review endianness & bit

/*

1 hex -> 4 bits
2 hex -> 8 bits -> 1 byte
32bit uint -> 4 bytes -> 4 * 2 hex

0x ff ff ff ff
can be used to store 4 x 8bit uint (2^8 = 256)

---------

endianness -> how value stored in memory!

                  v most significant bit
number 4 -> 0 0 1 0
            ^ least significant bit

In little endian machine:

    4 will be stored as

    the most significant bit it on lower address<- 0 1 0 0 -> the least significant bit on higher address

    which can be described as "order of bits are reversed"

However, endianness only describes order in memory, not in cpu.

<< shifting toward the most significant bit

>> shifting toward the least significant bit

in little endian

x = 4 -> in memory MSb 0 1 0 0 LSb

4 << 1 => 1 0 0 0 -> 8
4 >> 1 => 0 0 1 0 -> 2

*/

int main()
{

    // Save 4 uint8 then recover them

    uint32_t nn = 0x00'00'00'00;

    uint8_t n1 = 10; // 0a
    uint8_t n2 = 50; // 32
    uint8_t n3 = 60; // 3c
    uint8_t n4 = 200; // c8


    // note this assumes original value are all 0
    nn |= n1 << 0 * 8;
    nn |= n2 << 1 * 8;
    nn |= n3 << 2 * 8;
    nn |= n4 << 3 * 8;

    // nn -> 0a 32 3c c8

    // recover those values

    // two methods
    // -> uses mask then shift
    // -> uses shift then & 0xff


    // Note that in memory, those masks are reversed, LSb mask manipulate the bits of highest address
    uint32_t LSb = 0b1111'0000'0000'0000;
    uint32_t MSb = 0x00'00'00'FF;

    // Note the & 0xff -> after shifting, the result would have extra bits

    uint32_t r1 = (nn & MSb);
    uint32_t r2 = (nn >> 1 * 8) & 0xFF;
    uint8_t r3 = (nn >> 2 * 8) & 0xFF;
    uint8_t r4 = (nn & LSb) >> 3 * 8;

    printf("%d,%d,%d,%d", r1, r2, r3, r4);

    // Other things

    // Clear certain bits

    uint32_t clearOneByte =0xff'ff'00'ff;
    uint32_t dirty = 0x00'00'ff'00;

    dirty = dirty & clearOneByte;

    // [ One reason for little endian ]

    // for uint32_t 4 -> 0x00'00'00'04
    // for little endian machine -> 04'00'00'00 <---
    // for big endian machine -> -00'00'00'04      |
    //                                             | The first byte's address are the same for little endian machine !
    // but for uint16_t 4 -> 0x00'04               |
    // for little endian machine -> 04'00 <--------
    // for big endian machine -> - 00'04

    uint32_t a =4;
    uint8_t b = 4;

}