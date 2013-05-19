prs
===

This library implements PRS compression and decompression.

Compression
-----------

Use the `prs:compress/1` function. It expects a binary, and
returns that same binary compressed using PRS.

``` erlang
Compressed = prs:compress(Bin).
```

Decompression
-------------

Use the `prs:decompress/1` function. It expects a binary that
was compressed using PRS, and returns it decompressed.

``` erlang
Bin = prs:decompress(Compressed).
```

Format
------

PRS compression is a variant of LZ77 compression.

There are three important terms to understand.

 *  control byte: 8 bits telling the decompressor how the next chunk
    of data has been compressed. There are many control bytes in the
    compressed file.

 *  run-length encoding: A method of compression that points to an
    offset and a size in a dictionary which contains the data to be
    copied when decompressing.

 *  sliding window: Dictionary used when performing run-length encoding.
    The dictionary is actually the last N bytes of data that were
    compressed earlier in the file.

The sliding window can go back up to 0x1fff bytes at most.

PRS compression has four different ways of storing compressed data.
Every time data is stored, some bits are added to the next control
byte. If this fills up the control byte, then it is written before
the data that is in the process of being stored.

The rest of this document covers the different storage methods,
and the final sequence indicating the end of the compressed data.

Raw byte
--------

Control bits: 1
Data bytes: The raw byte itself

Raw bytes are stored as themselves plus a control bit set to 1.
All other storage methods set their first control bit to 0.

Short search
------------

Control bits: 0, 0, A, B
Data byte: Offset

 *  The offset is obtained directly from the data byte.

 *  The size is calculated using the formula `B + A * 2 + 2`.

The short search can only be used for offsets between 0 and 255,
and sizes between 2 and 5.

Long search, small size
-----------------------

Control bits: 0, 1
Data bytes: OffsetR:5, Size:3, OffsetL

OffsetR is stored over 5 bits, while Size is stored over 3 bits.
The value of Size must not be 0.

 *  The offset must be reconstituted by shifting OffsetL 5 bits to the
    left and adding OffsetR.

 *  The size is calculated using the formula `Size + 2`.

This form of long search can only be used for offsets between 0
and 0x1fff, and sizes between 3 and 9.

Long search, big size
---------------------

Control bits: 0, 1
Data bytes: OffsetR:5, 0:3, OffsetL, Size

OffsetR is stored over 5 bits, with the 3 other bits set to 0.

 *  The offset must be reconstituted by shifting OffsetL 5 bits to the
    left and adding OffsetR.

 *  The size is calculated using the formula `Size + 1`.

This form of long search can only be used for offsets between 0
and 0x1fff, and sizes between 1 and 255.

Final sequence
--------------

Control bits: 0, 1
Data bytes: 0, 0
