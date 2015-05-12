# erlang-sigsegv
A test application which reliably produces segmentation faults in the Erlang VM, specifically when garbage collection and old-code purging happen in parallel.

To run this test, copy and compile the code, then start the application from the Erlang shell:

```erlang
Erlang/OTP 17 [erts-6.3] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V6.3  (abort with ^G)
1> application:start(sigsegv). 
ok
2> Starting child... (63 remain)
Starting child... (62 remain)
Starting child... (61 remain)
Starting child... (60 remain)
Starting child... (59 remain)
Starting child... (58 remain)
Starting child... (57 remain)
Starting child... (56 remain)
Starting child... (55 remain)
Starting child... (54 remain)
Starting child... (53 remain)
Starting child... (52 remain)
Starting child... (51 remain)
Starting child... (50 remain)
Starting child... (49 remain)
Starting child... (48 remain)
Starting child... (47 remain)
Starting child... (46 remain)
Starting child... (45 remain)
Starting child... (44 remain)
Starting child... (43 remain)
Starting child... (42 remain)
Starting child... (41 remain)
Starting child... (40 remain)
Starting child... (39 remain)
Starting child... (38 remain)
Starting child... (37 remain)
Starting child... (36 remain)
Starting child... (35 remain)
Starting child... (34 remain)
Starting child... (33 remain)
Starting child... (32 remain)
Starting child... (31 remain)
Starting child... (30 remain)
Starting child... (29 remain)
Starting child... (28 remain)
Starting child... (27 remain)
Starting child... (26 remain)
Starting child... (25 remain)
Starting child... (24 remain)
Starting child... (23 remain)
Starting child... (22 remain)
Starting child... (21 remain)
Starting child... (20 remain)
Starting child... (19 remain)
Starting child... (18 remain)
Starting child... (17 remain)
Starting child... (16 remain)
Starting child... (15 remain)
Starting child... (14 remain)
Starting child... (13 remain)
Starting child... (12 remain)
Starting child... (11 remain)
Starting child... (10 remain)
Starting child... (9 remain)
Starting child... (8 remain)
Starting child... (7 remain)
Starting child... (6 remain)
Starting child... (5 remain)
Starting child... (4 remain)
Starting child... (3 remain)
Starting child... (2 remain)
Starting child... (1 remain)
Starting child... (0 remain)

2> Segmentation fault (core dumped)
```

This may take anywhere from a few seconds to several minutes to produce the conditions which cause the segmentation fault.

After running in GDB, one of the stacktraces produced is as follows:

```
Program received signal SIGSEGV, Segmentation fault.
                                                    [Switching to Thread 0x7ffff3b3e700 (LWP 26743)]
sweep_one_area (n_hp=0x7fffe8862028, n_htop=0x7fffe8862c48, src=src@entry=0x7fffe9ec2028 "", src_size=src_size@entry=600224) at beam/erl_gc.c:1816
1816				mb->base = binary_bytes(*origptr);
(gdb) bt
#0  sweep_one_area (n_hp=0x7fffe8862028, n_htop=0x7fffe8862c48, src=src@entry=0x7fffe9ec2028 "", src_size=src_size@entry=600224) at beam/erl_gc.c:1816
#1  0x0000000000527ea0 in do_minor (nobj=1, objv=0x7ffff3b3dd50, new_sz=121536, p=0x7ffff5c80800) at beam/erl_gc.c:1160
#2  minor_collection (recl=<synthetic pointer>, nobj=1, objv=0x7ffff3b3dd50, need=0, p=0x7ffff5c80800) at beam/erl_gc.c:876
#3  erts_garbage_collect (p=0x7ffff5c80800, need=need@entry=0, objv=objv@entry=0x7ffff3b3dd50, nobj=nobj@entry=1) at beam/erl_gc.c:450
#4  0x000000000052877b in erts_gc_after_bif_call (p=0x7ffff5c80800, result=140736302308346, regs=<optimized out>, arity=<optimized out>) at beam/erl_gc.c:370
#5  0x0000000000571951 in process_main () at beam/beam_emu.c:2787
#6  0x00000000004a9a70 in sched_thread_func (vesdp=0x7ffff51cc8c0) at beam/erl_process.c:7743
#7  0x00000000006056fb in thr_wrapper (vtwd=0x7fffffffd9a0) at pthread/ethread.c:106
#8  0x00007ffff704d374 in start_thread () from /usr/lib/libpthread.so.0
#9  0x00007ffff6b8327d in clone () from /usr/lib/libc.so.6
```

Please note that there *is* another location from which a segmentation fault can occur, and that is in the function `check_process_code` defined in `erts/emulator/beam/beam_bif_load.c`.

Unfortunately, thus far I have not been able to produce this second form of segmentation fault with this test application, and have only observed it on a production machine (e.g., no debug symbols, full optimization) running code that this test case is *based* on.
