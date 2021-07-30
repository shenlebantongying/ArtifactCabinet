> TODO: using same chunk of memory between two programs

To precisely control memory map, one need

+ madvise: Tell the OS how you expect to read certain pages.
+ mlock: Tell the OS that memory ranges cannot be paged out.
+ msync: Tell the OS to flush memory ranges out to disk.
