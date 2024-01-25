day04::main:
 push    rbp
 push    r15
 push    r14
 push    r13
 push    r12
 push    rbx
 sub     rsp, 600
 mov     qword, ptr, [rsp, +, 64], 0
 mov     qword, ptr, [rsp, +, 72], 23285
 lea     rax, [rip, +, .L__unnamed_5]
 mov     qword, ptr, [rsp, +, 80], rax
 mov     qword, ptr, [rsp, +, 88], 23285
 mov     qword, ptr, [rsp, +, 96], 0
 mov     qword, ptr, [rsp, +, 104], 23285
 mov     qword, ptr, [rsp, +, 112], 1
 movabs  rax, 42949672970
 mov     qword, ptr, [rsp, +, 120], rax
 mov     word, ptr, [rsp, +, 128], 0
 lea     rdi, [rsp, +, 64]
 call    <core::str::iter::Lines as core::iter::traits::iterator::Iterator>::next
 test    rax, rax
 je      .LBB19_54
 mov     rbp, rax
 mov     r14, rdx
 xor     r15d, r15d
 movabs  rbx, 4294981120
 xor     eax, eax
 mov     qword, ptr, [rsp, +, 144], rax
 xor     eax, eax
 mov     qword, ptr, [rsp, +, 32], rax
 jmp     .LBB19_4
.LBB19_2:
 mov     r15, qword, ptr, [rsp, +, 16]
.LBB19_3:
 inc     r15d
 lea     rdi, [rsp, +, 64]
 call    <core::str::iter::Lines as core::iter::traits::iterator::Iterator>::next
 mov     rbp, rax
 mov     r14, rdx
 test    rax, rax
 je      .LBB19_55
.LBB19_4:
 mov     qword, ptr, [rsp, +, 16], r15
 xor     r15d, r15d
 xor     eax, eax
 jmp     .LBB19_7
.LBB19_5:
 mov     r15b, 1
 xor     r14d, r14d
 mov     rbp, r12
 xor     r13d, r13d
.LBB19_6:
 test    r13, r13
 jne     .LBB19_15
.LBB19_7:
 test    al, al
 jne     .LBB19_2
 mov     r12, rbp
 mov     al, 1
 test    r14, r14
 je      .LBB19_5
 xor     r13d, r13d
 jmp     .LBB19_11
.LBB19_10:
 inc     r13
 cmp     r14, r13
 je      .LBB19_14
.LBB19_11:
 movzx   ecx, byte, ptr, [r12, +, r13]
 cmp     rcx, 32
 ja      .LBB19_10
 bt      rbx, rcx
 jae     .LBB19_10
 mov     rax, r13
 not     rax
 add     r14, rax
 lea     rbp, [r12, +, r13]
 inc     rbp
 xor     eax, eax
 jmp     .LBB19_6
.LBB19_14:
 mov     r15b, 1
 mov     rbp, r12
 mov     r13, r14
 jmp     .LBB19_6
.LBB19_15:
 test    r12, r12
 je      .LBB19_2
 mov     rax, qword, ptr, [rip, +, __rust_no_alloc_shim_is_unstable@GOTPCREL]
 movzx   eax, byte, ptr, [rax]
 mov     edi, 64
 mov     esi, 8
 call    qword, ptr, [rip, +, __rust_alloc@GOTPCREL]
 test    rax, rax
 je      .LBB19_341
 mov     qword, ptr, [rax], r12
 mov     qword, ptr, [rax, +, 8], r13
 mov     qword, ptr, [rsp, +, 176], 4
 mov     qword, ptr, [rsp, +, 184], rax
 mov     qword, ptr, [rsp, +, 192], 1
 mov     r10d, 1
 xor     ecx, ecx
 mov     qword, ptr, [rsp, +, 240], rcx
 xor     ecx, ecx
 mov     qword, ptr, [rsp, +, 56], rcx
.LBB19_18:
 mov     rcx, r14
 mov     rsi, rbp
 mov     edx, r15d
 jmp     .LBB19_21
.LBB19_19:
 mov     r15b, 1
 xor     ecx, ecx
 mov     rsi, r12
 xor     r13d, r13d
.LBB19_20:
 test    r13, r13
 jne     .LBB19_29
.LBB19_21:
 test    dl, dl
 jne     .LBB19_34
 mov     r12, rsi
 mov     dl, 1
 test    rcx, rcx
 je      .LBB19_19
 xor     r13d, r13d
 jmp     .LBB19_25
.LBB19_24:
 inc     r13
 cmp     rcx, r13
 je      .LBB19_28
.LBB19_25:
 movzx   esi, byte, ptr, [r12, +, r13]
 cmp     rsi, 32
 ja      .LBB19_24
 bt      rbx, rsi
 jae     .LBB19_24
 mov     r14, r13
 not     r14
 add     r14, rcx
 lea     rbp, [r12, +, r13]
 inc     rbp
 xor     edx, edx
 mov     rcx, r14
 mov     rsi, rbp
 jmp     .LBB19_20
.LBB19_28:
 mov     r15b, 1
 mov     rsi, r12
 mov     r13, rcx
 jmp     .LBB19_20
.LBB19_29:
 test    r12, r12
 je      .LBB19_34
 cmp     r10, qword, ptr, [rsp, +, 176]
 jne     .LBB19_33
 mov     edx, 1
 lea     rdi, [rsp, +, 176]
 mov     qword, ptr, [rsp, +, 336], r10
 mov     rsi, r10
 call    alloc::raw_vec::RawVec<T,A>::reserve::do_reserve_and_handle
 mov     rax, qword, ptr, [rsp, +, 184]
 mov     r10, qword, ptr, [rsp, +, 336]
.LBB19_33:
 mov     rcx, r10
 shl     rcx, 4
 mov     qword, ptr, [rax, +, rcx], r12
 mov     qword, ptr, [rax, +, rcx, +, 8], r13
 inc     r10
 mov     qword, ptr, [rsp, +, 192], r10
 inc     qword, ptr, [rsp, +, 56]
 mov     rcx, qword, ptr, [rsp, +, 240]
 inc     cl
 mov     qword, ptr, [rsp, +, 240], rcx
 jmp     .LBB19_18
.LBB19_34:
 mov     rdi, qword, ptr, [rsp, +, 184]
 test    r10, r10
 je      .LBB19_41
 mov     rax, qword, ptr, [rdi, +, 8]
 cmp     r10, 1
 mov     r15, qword, ptr, [rsp, +, 16]
 movabs  r9, 1152921504606846975
 je      .LBB19_42
 lea     rdx, [r10, +, r9]
 mov     rsi, rdx
 and     rsi, r9
 dec     rsi
 movzx   ecx, byte, ptr, [rsp, +, 240]
 cmp     rsi, 3
 jae     .LBB19_44
 xor     esi, esi
.LBB19_38:
 test    dl, 3
 je      .LBB19_42
 shl     rsi, 4
 lea     rdx, [rdi, +, rsi]
 add     rdx, 24
 and     ecx, 3
 shl     rcx, 4
 xor     esi, esi
.LBB19_40:
 mov     r8, qword, ptr, [rdx, +, rsi]
 cmp     rax, r8
 cmovbe  rax, r8
 add     rsi, 16
 cmp     rcx, rsi
 jne     .LBB19_40
 jmp     .LBB19_42
.LBB19_41:
 xor     eax, eax
 mov     r15, qword, ptr, [rsp, +, 16]
.LBB19_42:
 mov     rsi, qword, ptr, [rsp, +, 176]
 mov     rcx, qword, ptr, [rsp, +, 32]
 cmp     rax, rcx
 cmova   rcx, rax
 mov     qword, ptr, [rsp, +, 32], rcx
 mov     rax, qword, ptr, [rsp, +, 144]
 cmp     r10, rax
 cmova   rax, r10
 mov     qword, ptr, [rsp, +, 144], rax
 test    rsi, rsi
 je      .LBB19_3
 shl     rsi, 4
 mov     edx, 8
 call    qword, ptr, [rip, +, __rust_dealloc@GOTPCREL]
 jmp     .LBB19_3
.LBB19_44:
 lea     r8, [rdi, +, 72]
 mov     r11, qword, ptr, [rsp, +, 56]
 and     r11, r9
 mov     esi, ecx
 and     esi, 3
 sub     r11, rsi
 xor     esi, esi
 jmp     .LBB19_46
.LBB19_45:
 add     rsi, 4
 add     r8, 64
 cmp     r11, rsi
 je      .LBB19_38
.LBB19_46:
 mov     r9, qword, ptr, [r8, -, 48]
 cmp     rax, r9
 ja      .LBB19_47
 mov     rax, r9
 mov     r9, qword, ptr, [r8, -, 32]
 cmp     rax, r9
 jbe     .LBB19_51
.LBB19_48:
 mov     r9, qword, ptr, [r8, -, 16]
 cmp     rax, r9
 ja      .LBB19_49
.LBB19_52:
 mov     rax, r9
 mov     r9, qword, ptr, [r8]
 cmp     rax, r9
 ja      .LBB19_45
 jmp     .LBB19_53
.LBB19_47:
 mov     r9, qword, ptr, [r8, -, 32]
 cmp     rax, r9
 ja      .LBB19_48
.LBB19_51:
 mov     rax, r9
 mov     r9, qword, ptr, [r8, -, 16]
 cmp     rax, r9
 jbe     .LBB19_52
.LBB19_49:
 mov     r9, qword, ptr, [r8]
 cmp     rax, r9
 ja      .LBB19_45
.LBB19_53:
 mov     rax, r9
 jmp     .LBB19_45
.LBB19_54:
 xor     eax, eax
 mov     qword, ptr, [rsp, +, 32], rax
 xor     eax, eax
 mov     qword, ptr, [rsp, +, 144], rax
 xor     r15d, r15d
.LBB19_55:
 mov     rax, qword, ptr, [rsp, +, 32]
 mov     qword, ptr, [rsp, +, 528], rax
 mov     rax, qword, ptr, [rsp, +, 144]
 mov     qword, ptr, [rsp, +, 536], rax
 mov     dword, ptr, [rsp, +, 460], r15d
 lea     rax, [rsp, +, 528]
 mov     qword, ptr, [rsp, +, 64], rax
 mov     rcx, qword, ptr, [rip, +, _ZN4core3fmt3num3imp54_$LT$impl$u20$core..fmt..Display$u20$for$u20$usize$GT$3fmt17hd879ea1c6b6d5e73E@GOTPCREL]
 mov     qword, ptr, [rsp, +, 72], rcx
 lea     rax, [rsp, +, 536]
 mov     qword, ptr, [rsp, +, 80], rax
 mov     qword, ptr, [rsp, +, 88], rcx
 lea     rax, [rsp, +, 460]
 mov     qword, ptr, [rsp, +, 96], rax
 mov     rax, qword, ptr, [rip, +, _ZN4core3fmt3num3imp52_$LT$impl$u20$core..fmt..Display$u20$for$u20$i32$GT$3fmt17he70a8c8bc62f228bE@GOTPCREL]
 mov     qword, ptr, [rsp, +, 104], rax
 lea     rax, [rip, +, .L__unnamed_6]
 mov     qword, ptr, [rsp, +, 176], rax
 mov     qword, ptr, [rsp, +, 184], 4
 mov     qword, ptr, [rsp, +, 208], 0
 lea     rax, [rsp, +, 64]
 mov     qword, ptr, [rsp, +, 192], rax
 mov     qword, ptr, [rsp, +, 200], 3
 lea     rdi, [rsp, +, 176]
 call    qword, ptr, [rip, +, _ZN3std2io5stdio6_print17hfb5fdd73d3d2794dE@GOTPCREL]
 call    qword, ptr, [rip, +, _ZN3std4time7Instant3now17h4229229b6d335a9fE@GOTPCREL]
 mov     qword, ptr, [rsp, +, 544], rax
 mov     dword, ptr, [rsp, +, 552], edx
 mov     qword, ptr, [rsp, +, 64], 0
 mov     qword, ptr, [rsp, +, 72], 23285
 lea     rax, [rip, +, .L__unnamed_5]
 mov     qword, ptr, [rsp, +, 80], rax
 mov     qword, ptr, [rsp, +, 88], 23285
 mov     qword, ptr, [rsp, +, 96], 0
 mov     qword, ptr, [rsp, +, 104], 23285
 mov     qword, ptr, [rsp, +, 112], 1
 movabs  rax, 42949672970
 mov     qword, ptr, [rsp, +, 120], rax
 mov     word, ptr, [rsp, +, 128], 0
 lea     rdi, [rsp, +, 64]
 call    <core::str::iter::Lines as core::iter::traits::iterator::Iterator>::next
 test    rax, rax
 je      .LBB19_83
 mov     rbx, rax
 mov     r12, rdx
 xor     r14d, r14d
 mov     r15, qword, ptr, [rip, +, _ZN3std4hash6random11RandomState3new4KEYS7__getit5__KEY17h50d7eafb4bd391a9E@GOTTPOFF]
 jmp     .LBB19_59
.LBB19_57:
 mov     bl, 1
.LBB19_58:
 movzx   eax, bl
 add     r14, rax
 lea     rdi, [rsp, +, 64]
 call    <core::str::iter::Lines as core::iter::traits::iterator::Iterator>::next
 mov     rbx, rax
 mov     r12, rdx
 test    rax, rax
 je      .LBB19_84
.LBB19_59:
 cmp     qword, ptr, fs:[r15], 0
 jne     .LBB19_61
 xor     edi, edi
 call    std::sys::common::thread_local::fast_local::Key<T>::try_initialize
.LBB19_61:
 movups  xmm0, xmmword, ptr, [rip, +, .L__unnamed_7+16]
 movaps  xmmword, ptr, [rsp, +, 288], xmm0
 movdqu  xmm0, xmmword, ptr, fs:[r15, +, 8]
 inc     qword, ptr, fs:[r15, +, 8]
 movdqu  xmm1, xmmword, ptr, [rip, +, .L__unnamed_7]
 movdqa  xmmword, ptr, [rsp, +, 272], xmm1
 movdqa  xmmword, ptr, [rsp, +, 304], xmm0
 mov     rax, r12
 add     rax, rbx
 mov     qword, ptr, [rsp, +, 176], 0
 mov     qword, ptr, [rsp, +, 184], r12
 mov     qword, ptr, [rsp, +, 192], rbx
 mov     qword, ptr, [rsp, +, 200], r12
 mov     qword, ptr, [rsp, +, 208], rbx
 mov     qword, ptr, [rsp, +, 216], rax
 mov     qword, ptr, [rsp, +, 224], 0
 mov     word, ptr, [rsp, +, 232], 1
 lea     rdi, [rsp, +, 176]
 call    <core::str::iter::SplitWhitespace as core::iter::traits::iterator::Iterator>::next
 test    rax, rax
 je      .LBB19_57
 mov     rbp, rax
 mov     rbx, rdx
 mov     qword, ptr, [rsp, +, 56], r14
 lea     r15, [rip, +, .L__unnamed_8]
 xor     r14d, r14d
.LBB19_63:
 mov     qword, ptr, [rsp, +, 384], rbp
 mov     qword, ptr, [rsp, +, 392], rbx
 mov     rdi, qword, ptr, [rsp, +, 304]
 mov     rsi, qword, ptr, [rsp, +, 312]
 lea     rdx, [rsp, +, 384]
 call    core::hash::BuildHasher::hash_one
 mov     r12, rax
 test    r14, r14
 jne     .LBB19_66
 lea     rdi, [rsp, +, 272]
 lea     rsi, [rsp, +, 304]
 call    hashbrown::raw::RawTable<T,A>::reserve_rehash
 mov     r15, qword, ptr, [rsp, +, 272]
.LBB19_66:
 mov     r13, qword, ptr, [rsp, +, 280]
 mov     rax, r12
 shr     rax, 57
 mov     qword, ptr, [rsp, +, 328], rax
 movd    xmm0, eax
 punpcklbw xmm0, xmm0
 pshuflw xmm0, xmm0, 0
 pshufd  xmm0, xmm0, 0
 movdqa  xmmword, ptr, [rsp, +, 240], xmm0
 xor     eax, eax
 mov     qword, ptr, [rsp, +, 144], rax
 xor     eax, eax
 mov     qword, ptr, [rsp, +, 16], rax
.LBB19_67:
 and     r12, r13
 movdqu  xmm0, xmmword, ptr, [r15, +, r12]
 movdqa  xmmword, ptr, [rsp, +, 32], xmm0
 pcmpeqb xmm0, xmmword, ptr, [rsp, +, 240]
 pmovmskb r14d, xmm0
 test    r14d, r14d
 jne     .LBB19_73
.LBB19_68:
 cmp     qword, ptr, [rsp, +, 16], 1
 mov     ecx, 1
 movdqa  xmm0, xmmword, ptr, [rsp, +, 32]
 je      .LBB19_70
 pmovmskb eax, xmm0
 xor     ecx, ecx
 test    eax, eax
 setne   cl
 rep     bsf, eax, eax
 add     rax, r12
 and     rax, r13
 mov     qword, ptr, [rsp, +, 336], rax
.LBB19_70:
 pcmpeqb xmm0, xmmword, ptr, [rip, +, .LCPI19_0]
 pmovmskb eax, xmm0
 test    eax, eax
 jne     .LBB19_75
 mov     qword, ptr, [rsp, +, 16], rcx
 mov     rax, qword, ptr, [rsp, +, 144]
 add     r12, rax
 add     r12, 16
 add     rax, 16
 mov     qword, ptr, [rsp, +, 144], rax
 jmp     .LBB19_67
.LBB19_72:
 lea     eax, [r14, -, 1]
 and     eax, r14d
 mov     r14d, eax
 test    ax, ax
 je      .LBB19_68
.LBB19_73:
 rep     bsf, ecx, r14d
 add     rcx, r12
 and     rcx, r13
 shl     rcx, 4
 mov     rax, r15
 sub     rax, rcx
 cmp     qword, ptr, [rax, -, 8], rbx
 jne     .LBB19_72
 mov     rsi, qword, ptr, [rax, -, 16]
 mov     rdi, rbp
 mov     rdx, rbx
 call    qword, ptr, [rip, +, bcmp@GOTPCREL]
 test    eax, eax
 jne     .LBB19_72
 jmp     .LBB19_79
.LBB19_75:
 mov     rdx, qword, ptr, [rsp, +, 336]
 movzx   eax, byte, ptr, [r15, +, rdx]
 test    al, al
 js      .LBB19_77
 movdqa  xmm0, xmmword, ptr, [r15]
 pmovmskb eax, xmm0
 rep     bsf, edx, eax
 movzx   eax, byte, ptr, [r15, +, rdx]
.LBB19_77:
 and     al, 1
 movzx   eax, al
 mov     r14, qword, ptr, [rsp, +, 288]
 sub     r14, rax
 mov     qword, ptr, [rsp, +, 288], r14
 lea     rax, [rdx, -, 16]
 and     rax, r13
 mov     rcx, qword, ptr, [rsp, +, 328]
 mov     byte, ptr, [r15, +, rdx], cl
 mov     byte, ptr, [rax, +, r15, +, 16], cl
 inc     qword, ptr, [rsp, +, 296]
 shl     rdx, 4
 mov     rax, r15
 sub     rax, rdx
 neg     rdx
 mov     qword, ptr, [r15, +, rdx, -, 16], rbp
 mov     qword, ptr, [rax, -, 8], rbx
 lea     rdi, [rsp, +, 176]
 call    <core::str::iter::SplitWhitespace as core::iter::traits::iterator::Iterator>::next
 mov     rbp, rax
 mov     rbx, rdx
 test    rax, rax
 jne     .LBB19_63
 mov     bl, 1
 mov     r13, qword, ptr, [rsp, +, 280]
 jmp     .LBB19_80
.LBB19_79:
 xor     ebx, ebx
.LBB19_80:
 test    r13, r13
 mov     r14, qword, ptr, [rsp, +, 56]
 mov     r15, qword, ptr, [rip, +, _ZN3std4hash6random11RandomState3new4KEYS7__getit5__KEY17h50d7eafb4bd391a9E@GOTTPOFF]
 je      .LBB19_58
 mov     rax, r13
 shl     rax, 4
 add     r13, rax
 add     r13, 33
 je      .LBB19_58
 mov     rdi, qword, ptr, [rsp, +, 272]
 sub     rdi, rax
 add     rdi, -16
 mov     edx, 16
 mov     rsi, r13
 call    qword, ptr, [rip, +, __rust_dealloc@GOTPCREL]
 jmp     .LBB19_58
.LBB19_83:
 xor     r14d, r14d
.LBB19_84:
 mov     qword, ptr, [rsp, +, 384], r14
 lea     rdi, [rsp, +, 544]
 call    qword, ptr, [rip, +, _ZN3std4time7Instant7elapsed17h4d8d8261e1074b2cE@GOTPCREL]
 mov     qword, ptr, [rsp, +, 272], rax
 mov     dword, ptr, [rsp, +, 280], edx
 lea     rax, [rsp, +, 384]
 mov     qword, ptr, [rsp, +, 176], rax
 mov     rax, qword, ptr, [rip, +, _ZN4core3fmt3num3imp54_$LT$impl$u20$core..fmt..Display$u20$for$u20$usize$GT$3fmt17hd879ea1c6b6d5e73E@GOTPCREL]
 mov     qword, ptr, [rsp, +, 184], rax
 lea     rax, [rsp, +, 272]
 mov     qword, ptr, [rsp, +, 192], rax
 mov     rax, qword, ptr, [rip, +, _ZN57_$LT$core..time..Duration$u20$as$u20$core..fmt..Debug$GT$3fmt17h4747c8e613e16eb4E@GOTPCREL]
 mov     qword, ptr, [rsp, +, 200], rax
 lea     rax, [rip, +, .L__unnamed_9]
 mov     qword, ptr, [rsp, +, 64], rax
 mov     qword, ptr, [rsp, +, 72], 3
 mov     qword, ptr, [rsp, +, 96], 0
 lea     rax, [rsp, +, 176]
 mov     qword, ptr, [rsp, +, 80], rax
 mov     qword, ptr, [rsp, +, 88], 2
 lea     rdi, [rsp, +, 64]
 call    qword, ptr, [rip, +, _ZN3std2io5stdio6_print17hfb5fdd73d3d2794dE@GOTPCREL]
 call    qword, ptr, [rip, +, _ZN3std4time7Instant3now17h4229229b6d335a9fE@GOTPCREL]
 mov     qword, ptr, [rsp, +, 560], rax
 mov     dword, ptr, [rsp, +, 568], edx
 mov     qword, ptr, [rsp, +, 64], 0
 mov     qword, ptr, [rsp, +, 72], 23285
 lea     rax, [rip, +, .L__unnamed_5]
 mov     qword, ptr, [rsp, +, 80], rax
 mov     qword, ptr, [rsp, +, 88], 23285
 mov     qword, ptr, [rsp, +, 96], 0
 mov     qword, ptr, [rsp, +, 104], 23285
 mov     qword, ptr, [rsp, +, 112], 1
 movabs  rax, 42949672970
 mov     qword, ptr, [rsp, +, 120], rax
 mov     word, ptr, [rsp, +, 128], 0
 lea     rdi, [rsp, +, 64]
 call    <core::str::iter::Lines as core::iter::traits::iterator::Iterator>::next
 test    rax, rax
 je      .LBB19_242
 mov     rbx, rax
 mov     r14, rdx
 xor     eax, eax
 mov     qword, ptr, [rsp, +, 440], rax
 mov     rcx, qword, ptr, [rip, +, _ZN3std4hash6random11RandomState3new4KEYS7__getit5__KEY17h50d7eafb4bd391a9E@GOTTPOFF]
 lea     rax, [rsp, +, 272]
 mov     qword, ptr, [rsp, +, 496], rax
 lea     rax, [rip, +, .L__unnamed_10]
 mov     qword, ptr, [rsp, +, 464], rax
 jmp     .LBB19_87
.LBB19_86:
 add     qword, ptr, [rsp, +, 440], r15
 lea     rdi, [rsp, +, 64]
 call    <core::str::iter::Lines as core::iter::traits::iterator::Iterator>::next
 mov     rbx, rax
 mov     r14, rdx
 test    rax, rax
 mov     rcx, qword, ptr, [rip, +, _ZN3std4hash6random11RandomState3new4KEYS7__getit5__KEY17h50d7eafb4bd391a9E@GOTTPOFF]
 je      .LBB19_243
.LBB19_87:
 cmp     qword, ptr, fs:[rcx], 0
 jne     .LBB19_89
 xor     edi, edi
 call    std::sys::common::thread_local::fast_local::Key<T>::try_initialize
 mov     rcx, qword, ptr, [rip, +, _ZN3std4hash6random11RandomState3new4KEYS7__getit5__KEY17h50d7eafb4bd391a9E@GOTTPOFF]
.LBB19_89:
 movups  xmm0, xmmword, ptr, [rip, +, .L__unnamed_7+16]
 movaps  xmmword, ptr, [rsp, +, 400], xmm0
 movdqu  xmm0, xmmword, ptr, fs:[rcx, +, 8]
 inc     qword, ptr, fs:[rcx, +, 8]
 movdqu  xmm1, xmmword, ptr, [rip, +, .L__unnamed_7]
 movdqa  xmmword, ptr, [rsp, +, 384], xmm1
 movdqa  xmmword, ptr, [rsp, +, 416], xmm0
 mov     rax, r14
 add     rax, rbx
 mov     qword, ptr, [rsp, +, 176], 0
 mov     qword, ptr, [rsp, +, 184], r14
 mov     qword, ptr, [rsp, +, 192], rbx
 mov     qword, ptr, [rsp, +, 200], r14
 mov     qword, ptr, [rsp, +, 208], rbx
 mov     qword, ptr, [rsp, +, 216], rax
 mov     qword, ptr, [rsp, +, 224], 0
 mov     word, ptr, [rsp, +, 232], 1
 lea     rdi, [rsp, +, 176]
 call    <core::str::iter::SplitWhitespace as core::iter::traits::iterator::Iterator>::next
 test    rax, rax
 je      .LBB19_231
 mov     r14, rax
 xor     eax, eax
 mov     qword, ptr, [rsp, +, 448], rax
 lea     rax, [rip, +, .L__unnamed_8]
 mov     qword, ptr, [rsp, +, 480], rax
 xor     ebx, ebx
.LBB19_91:
 mov     r13d, 4
 test    rdx, rdx
 mov     qword, ptr, [rsp, +, 520], rbx
 je      .LBB19_97
 movzx   ebp, byte, ptr, [r14]
 test    bpl, bpl
 js      .LBB19_94
 lea     r12, [r14, +, 1]
 jmp     .LBB19_101
.LBB19_94:
 mov     eax, ebp
 and     eax, 31
 movzx   esi, byte, ptr, [r14, +, 1]
 and     esi, 63
 cmp     bpl, -33
 jbe     .LBB19_98
 movzx   ecx, byte, ptr, [r14, +, 2]
 shl     esi, 6
 and     ecx, 63
 or      ecx, esi
 cmp     bpl, -16
 jb      .LBB19_99
 movzx   ebp, byte, ptr, [r14, +, 3]
 and     eax, 7
 shl     eax, 18
 shl     ecx, 6
 and     ebp, 63
 or      ebp, ecx
 or      ebp, eax
 cmp     ebp, 1114112
 jne     .LBB19_100
.LBB19_97:
 xor     ebx, ebx
 xor     r8d, r8d
 jmp     .LBB19_212
.LBB19_98:
 lea     r12, [r14, +, 2]
 shl     eax, 6
 or      eax, esi
 mov     ebp, eax
 jmp     .LBB19_101
.LBB19_99:
 lea     r12, [r14, +, 3]
 shl     eax, 12
 or      ecx, eax
 mov     ebp, ecx
 jmp     .LBB19_101
.LBB19_100:
 lea     r12, [r14, +, 4]
.LBB19_101:
 add     r14, rdx
 lea     rcx, [r14, +, 3]
 mov     qword, ptr, [rsp, +, 16], rcx
 sub     rcx, r12
 shr     rcx, 2
 cmp     rcx, 4
 mov     eax, 3
 cmovb   rcx, rax
 movabs  rax, 2305843009213693950
 cmp     rcx, rax
 ja      .LBB19_339
 inc     rcx
 mov     rbx, rcx
 lea     r15, [4*rcx]
 mov     rax, qword, ptr, [rip, +, __rust_no_alloc_shim_is_unstable@GOTPCREL]
 movzx   eax, byte, ptr, [rax]
 mov     esi, 4
 mov     rdi, r15
 call    qword, ptr, [rip, +, __rust_alloc@GOTPCREL]
 test    rax, rax
 je      .LBB19_340
 mov     dword, ptr, [rax], ebp
 mov     r8d, 1
 cmp     r12, r14
 je      .LBB19_125
 mov     qword, ptr, [rsp], rax
 mov     rdi, rax
 mov     r13, rbx
 jmp     .LBB19_107
.LBB19_105:
 mov     rbx, r13
.LBB19_106:
 mov     dword, ptr, [rdi, +, 4*r8], ebp
 inc     r8
 mov     r13, rbx
 cmp     r12, r14
 mov     rbp, rbx
 je      .LBB19_127
.LBB19_107:
 movzx   ebp, byte, ptr, [r12]
 test    bpl, bpl
 js      .LBB19_109
 inc     r12
 cmp     r8, r13
 jne     .LBB19_105
 jmp     .LBB19_115
.LBB19_109:
 mov     eax, ebp
 and     eax, 31
 movzx   edx, byte, ptr, [r12, +, 1]
 and     edx, 63
 cmp     bpl, -33
 jbe     .LBB19_113
 movzx   ecx, byte, ptr, [r12, +, 2]
 shl     edx, 6
 and     ecx, 63
 or      ecx, edx
 cmp     bpl, -16
 jb      .LBB19_114
 movzx   ebp, byte, ptr, [r12, +, 3]
 and     eax, 7
 shl     eax, 18
 shl     ecx, 6
 and     ebp, 63
 or      ebp, ecx
 or      ebp, eax
 cmp     ebp, 1114112
 je      .LBB19_126
 add     r12, 4
 cmp     r8, r13
 jne     .LBB19_105
 jmp     .LBB19_115
.LBB19_113:
 add     r12, 2
 shl     eax, 6
 or      eax, edx
 mov     ebp, eax
 cmp     r8, r13
 je      .LBB19_115
 jmp     .LBB19_105
.LBB19_114:
 add     r12, 3
 shl     eax, 12
 or      ecx, eax
 mov     ebp, ecx
 cmp     r8, r13
 jne     .LBB19_105
.LBB19_115:
 mov     rcx, qword, ptr, [rsp, +, 16]
 sub     rcx, r12
 shr     rcx, 2
 inc     rcx
 add     rcx, r13
 jb      .LBB19_330
 lea     rax, [2*r13]
 cmp     rax, rcx
 cmova   rcx, rax
 cmp     rcx, 5
 jae     .LBB19_118
 mov     ecx, 4
.LBB19_118:
 movabs  rax, 2305843009213693950
 inc     rax
 xor     esi, esi
 mov     rbx, rcx
 cmp     rcx, rax
 setbe   al
 test    r13, r13
 mov     rcx, r13
 je      .LBB19_120
 mov     r15, r8
 mov     r13, rcx
 lea     rcx, [4*rcx]
 mov     rdx, qword, ptr, [rsp]
 mov     qword, ptr, [rsp, +, 272], rdx
 mov     qword, ptr, [rsp, +, 288], rcx
 mov     ecx, 4
 jmp     .LBB19_121
.LBB19_120:
 mov     r13, rcx
 mov     r15, r8
 xor     ecx, ecx
.LBB19_121:
 lea     rdx, [4*rbx]
 mov     sil, al
 shl     rsi, 2
 mov     qword, ptr, [rsp, +, 280], rcx
 lea     rdi, [rsp, +, 576]
 lea     rcx, [rsp, +, 272]
 call    alloc::raw_vec::finish_grow
 cmp     qword, ptr, [rsp, +, 576], 0
 mov     rdi, qword, ptr, [rsp, +, 584]
 je      .LBB19_124
 movabs  rax, -9223372036854775807
 cmp     rdi, rax
 jne     .LBB19_329
 mov     r8, r15
 mov     rbx, r13
 mov     rdi, qword, ptr, [rsp]
 jmp     .LBB19_106
.LBB19_124:
 mov     qword, ptr, [rsp], rdi
 mov     r8, r15
 jmp     .LBB19_106
.LBB19_125:
 mov     r13, rax
 jmp     .LBB19_212
.LBB19_126:
 mov     rbp, r13
.LBB19_127:
 cmp     r8, 21
 mov     qword, ptr, [rsp, +, 264], r8
 jae     .LBB19_131
 cmp     r8, 1
 mov     r13, qword, ptr, [rsp]
 jbe     .LBB19_211
 mov     edx, 1
 mov     rdi, r13
 mov     rsi, r8
 call    core::slice::sort::insertion_sort_shift_left
 mov     rbx, rbp
 mov     r8, qword, ptr, [rsp, +, 264]
 jmp     .LBB19_212
.LBB19_131:
 lea     rdi, [r8, +, r8]
 movabs  rax, 9223372036854775793
 add     rax, 11
 and     rdi, rax
 mov     rbx, qword, ptr, [rip, +, __rust_no_alloc_shim_is_unstable@GOTPCREL]
 movzx   eax, byte, ptr, [rbx]
 mov     esi, 4
 mov     qword, ptr, [rsp, +, 472], rdi
 mov     r14, qword, ptr, [rip, +, __rust_alloc@GOTPCREL]
 call    r14
 test    rax, rax
 mov     r13, qword, ptr, [rsp]
 mov     qword, ptr, [rsp, +, 368], rbp
 je      .LBB19_343
 mov     r12, rax
 movzx   eax, byte, ptr, [rbx]
 mov     edi, 256
 mov     esi, 8
 call    r14
 test    rax, rax
 mov     qword, ptr, [rsp, +, 8], r12
 je      .LBB19_342
 mov     rbx, rax
 lea     rax, [r13, +, 16]
 mov     qword, ptr, [rsp, +, 512], rax
 lea     rax, [r13, -, 16]
 mov     qword, ptr, [rsp, +, 504], rax
 lea     rax, [r13, -, 4]
 mov     qword, ptr, [rsp, +, 488], rax
 mov     eax, 16
 mov     qword, ptr, [rsp, +, 360], rax
 xor     eax, eax
 xor     r15d, r15d
 mov     rsi, qword, ptr, [rsp, +, 264]
 mov     qword, ptr, [rsp], r13
 jmp     .LBB19_136
.LBB19_134:
 mov     r15d, 1
.LBB19_135:
 mov     rax, qword, ptr, [rsp, +, 48]
 cmp     rax, rsi
 jae     .LBB19_210
.LBB19_136:
 mov     r11, rax
 mov     rdx, rsi
 sub     rdx, rax
 lea     rdi, [4*rax]
 add     rdi, r13
 cmp     rdx, 2
 mov     qword, ptr, [rsp, +, 168], rbx
 jae     .LBB19_147
 mov     r8, rsi
.LBB19_138:
 mov     rbx, r8
 sub     rbx, r11
 jb      .LBB19_331
 cmp     r8, rsi
 ja      .LBB19_331
 jae     .LBB19_150
 cmp     rdx, 10
 jae     .LBB19_150
 lea     rbx, [r11, +, 10]
 cmp     rbx, rsi
 cmovae  rbx, rsi
 mov     qword, ptr, [rsp, +, 48], rbx
 mov     r14, r11
 sub     rbx, r11
 jb      .LBB19_333
 cmp     rdx, 1
 adc     rdx, 0
 mov     rsi, rbx
 call    core::slice::sort::insertion_sort_shift_left
 mov     rsi, qword, ptr, [rsp, +, 264]
 mov     r11, r14
 cmp     r15, qword, ptr, [rsp, +, 360]
 jne     .LBB19_151
.LBB19_145:
 mov     qword, ptr, [rsp, +, 16], r11
 lea     rax, [r15, +, r15]
 mov     qword, ptr, [rsp, +, 360], rax
 mov     rdi, r15
 shl     rdi, 5
 mov     rax, qword, ptr, [rip, +, __rust_no_alloc_shim_is_unstable@GOTPCREL]
 movzx   eax, byte, ptr, [rax]
 mov     esi, 8
 call    qword, ptr, [rip, +, __rust_alloc@GOTPCREL]
 test    rax, rax
 je      .LBB19_334
 mov     r13, r15
 mov     r15, rax
 mov     r12, r13
 shl     r12, 4
 mov     rdi, rax
 mov     r14, qword, ptr, [rsp, +, 168]
 mov     rsi, r14
 mov     rdx, r12
 call    qword, ptr, [rip, +, memcpy@GOTPCREL]
 mov     edx, 8
 mov     rdi, r14
 mov     rsi, r12
 call    qword, ptr, [rip, +, __rust_dealloc@GOTPCREL]
 mov     r14, r15
 mov     rsi, qword, ptr, [rsp, +, 264]
 mov     r12, qword, ptr, [rsp, +, 8]
 mov     r15, r13
 mov     r13, qword, ptr, [rsp]
 mov     r11, qword, ptr, [rsp, +, 16]
 jmp     .LBB19_152
.LBB19_147:
 mov     eax, dword, ptr, [rdi]
 mov     ecx, dword, ptr, [rdi, +, 4]
 cmp     ecx, eax
 jae     .LBB19_186
 cmp     rdx, 2
 jne     .LBB19_188
 lea     r8, [r11, +, 2]
 mov     edx, 2
 jmp     .LBB19_196
.LBB19_150:
 mov     qword, ptr, [rsp, +, 48], r8
 cmp     r15, qword, ptr, [rsp, +, 360]
 je      .LBB19_145
.LBB19_151:
 mov     r14, qword, ptr, [rsp, +, 168]
.LBB19_152:
 mov     rax, r15
 shl     rax, 4
 mov     qword, ptr, [r14, +, rax], rbx
 mov     qword, ptr, [r14, +, rax, +, 8], r11
 inc     r15
 cmp     r15, 2
 mov     rbx, r14
 jb      .LBB19_135
 mov     qword, ptr, [rsp, +, 168], rbx
 jmp     .LBB19_157
.LBB19_154:
 mov     r12, qword, ptr, [rsp, +, 8]
 mov     rsi, r12
.LBB19_155:
 mov     r15, qword, ptr, [rsp, +, 32]
.LBB19_156:
 sub     rdx, rsi
 mov     rdi, rbx
 call    qword, ptr, [rip, +, memcpy@GOTPCREL]
 mov     rax, qword, ptr, [rsp, +, 56]
 add     rax, qword, ptr, [rsp, +, 16]
 mov     rcx, qword, ptr, [rsp, +, 328]
 mov     qword, ptr, [rcx], rax
 mov     rax, qword, ptr, [rsp, +, 240]
 mov     rcx, qword, ptr, [rsp, +, 376]
 mov     qword, ptr, [rcx], rax
 mov     rdi, qword, ptr, [rsp, +, 336]
 lea     rsi, [rdi, +, 16]
 mov     rax, qword, ptr, [rsp, +, 144]
 not     rax
 add     r14, rax
 shl     r14, 4
 mov     rdx, r14
 call    qword, ptr, [rip, +, memmove@GOTPCREL]
 cmp     r15, 1
 mov     rsi, qword, ptr, [rsp, +, 264]
 mov     rbp, qword, ptr, [rsp, +, 368]
 mov     rbx, qword, ptr, [rsp, +, 168]
 jbe     .LBB19_134
.LBB19_157:
 mov     r14, r15
 lea     r9, [r15, -, 1]
 mov     rcx, r9
 shl     rcx, 4
 mov     rax, qword, ptr, [rbx, +, rcx]
 mov     rcx, qword, ptr, [rbx, +, rcx, +, 8]
 add     rcx, rax
 cmp     rcx, rsi
 je      .LBB19_163
 mov     rdx, r14
 shl     rdx, 4
 mov     r8, qword, ptr, [rdx, +, rbx, -, 32]
 cmp     r8, rax
 jbe     .LBB19_163
 cmp     r14, 2
 jbe     .LBB19_201
 lea     r10, [r14, -, 3]
 mov     rcx, r10
 shl     rcx, 4
 mov     rcx, qword, ptr, [rbx, +, rcx]
 lea     rdi, [r8, +, rax]
 cmp     rcx, rdi
 jbe     .LBB19_165
 cmp     r14, 3
 jbe     .LBB19_208
 add     r8, rcx
 cmp     qword, ptr, [rdx, +, rbx, -, 64], r8
 jbe     .LBB19_165
 jmp     .LBB19_209
.LBB19_163:
 cmp     r14, 3
 jb      .LBB19_166
 lea     r10, [r14, -, 3]
 mov     rcx, r10
 shl     rcx, 4
 mov     rcx, qword, ptr, [rbx, +, rcx]
.LBB19_165:
 cmp     rcx, rax
 jb      .LBB19_167
.LBB19_166:
 lea     r10, [r14, -, 2]
.LBB19_167:
 cmp     r14, r10
 jbe     .LBB19_326
 lea     rax, [r10, +, 1]
 cmp     r14, rax
 jbe     .LBB19_325
 mov     qword, ptr, [rsp, +, 32], r9
 mov     rcx, r10
 shl     rcx, 4
 mov     rdi, qword, ptr, [rbx, +, rcx, +, 8]
 shl     rax, 4
 mov     rdx, qword, ptr, [rbx, +, rax]
 mov     r15, qword, ptr, [rbx, +, rax, +, 8]
 add     r15, rdx
 mov     r12, r15
 sub     r12, rdi
 jb      .LBB19_328
 cmp     r15, rsi
 ja      .LBB19_324
 mov     qword, ptr, [rsp, +, 56], rdx
 mov     qword, ptr, [rsp, +, 144], r10
 lea     rdx, [rbx, +, rcx]
 mov     qword, ptr, [rsp, +, 336], rdx
 mov     rcx, qword, ptr, [rbx, +, rcx]
 lea     rdx, [rbx, +, rax]
 mov     qword, ptr, [rsp, +, 328], rdx
 lea     rax, [rbx, +, rax, +, 8]
 mov     qword, ptr, [rsp, +, 376], rax
 mov     qword, ptr, [rsp, +, 240], rdi
 lea     rbx, [4*rdi]
 add     rbx, r13
 lea     rbp, [rbx, +, 4*rcx]
 mov     r13, r12
 sub     r13, rcx
 cmp     r13, rcx
 mov     qword, ptr, [rsp, +, 16], rcx
 jae     .LBB19_178
 lea     rdx, [4*r13]
 mov     r12, qword, ptr, [rsp, +, 8]
 mov     rdi, r12
 mov     rsi, rbp
 call    qword, ptr, [rip, +, memcpy@GOTPCREL]
 lea     rdx, [r12, +, 4*r13]
 cmp     qword, ptr, [rsp, +, 16], 0
 jle     .LBB19_183
 test    r13, r13
 jle     .LBB19_183
 mov     rax, qword, ptr, [rsp, +, 488]
 lea     rax, [rax, +, 4*r15]
 mov     r13, qword, ptr, [rsp]
 mov     r15, qword, ptr, [rsp, +, 32]
.LBB19_175:
 mov     ecx, dword, ptr, [rdx, -, 4]
 mov     esi, dword, ptr, [rbp, -, 4]
 cmp     ecx, esi
 mov     edi, 0
 sbb     rdi, rdi
 cmp     ecx, esi
 mov     r8d, 0
 adc     r8, -1
 cmp     ecx, esi
 lea     rbp, [rbp, +, 4*rdi]
 lea     rdx, [rdx, +, 4*r8]
 mov     rcx, rdx
 cmovb   rcx, rbp
 mov     ecx, dword, ptr, [rcx]
 mov     dword, ptr, [rax], ecx
 cmp     rbp, rbx
 jbe     .LBB19_177
 add     rax, -4
 cmp     rdx, r12
 ja      .LBB19_175
.LBB19_177:
 mov     rbx, rbp
 mov     rsi, r12
 jmp     .LBB19_156
.LBB19_178:
 lea     rdx, [4*rcx]
 mov     r13, qword, ptr, [rsp, +, 8]
 mov     rdi, r13
 mov     rsi, rbx
 call    qword, ptr, [rip, +, memcpy@GOTPCREL]
 mov     rax, qword, ptr, [rsp, +, 16]
 lea     rdx, [4*rax]
 add     rdx, r13
 test    rax, rax
 jle     .LBB19_184
 cmp     r12, rax
 mov     r13, qword, ptr, [rsp]
 jle     .LBB19_154
 lea     rax, [4*r15]
 add     rax, r13
 mov     r12, qword, ptr, [rsp, +, 8]
 mov     rsi, r12
 mov     r15, qword, ptr, [rsp, +, 32]
.LBB19_181:
 mov     r8d, dword, ptr, [rbp]
 mov     r9d, dword, ptr, [rsi]
 xor     r10d, r10d
 xor     ecx, ecx
 cmp     r8d, r9d
 setae   r10b
 setb    dil
 cmovb   r9d, r8d
 mov     dword, ptr, [rbx], r9d
 add     rbx, 4
 lea     rsi, [rsi, +, 4*r10]
 cmp     rsi, rdx
 jae     .LBB19_156
 mov     cl, dil
 lea     rbp, [rbp, +, 4*rcx]
 cmp     rbp, rax
 jb      .LBB19_181
 jmp     .LBB19_156
.LBB19_183:
 mov     rbx, rbp
 jmp     .LBB19_185
.LBB19_184:
 mov     r12, qword, ptr, [rsp, +, 8]
.LBB19_185:
 mov     rsi, r12
 mov     r13, qword, ptr, [rsp]
 jmp     .LBB19_155
.LBB19_186:
 cmp     rdx, 2
 jne     .LBB19_191
 lea     r8, [r11, +, 2]
 mov     edx, 2
 jmp     .LBB19_138
.LBB19_188:
 mov     r10d, 2
 mov     r8d, ecx
.LBB19_189:
 mov     r9d, r8d
 mov     r8d, dword, ptr, [rdi, +, 4*r10]
 cmp     r8d, r9d
 jae     .LBB19_195
 inc     r10
 cmp     rdx, r10
 jne     .LBB19_189
 jmp     .LBB19_194
.LBB19_191:
 mov     r10d, 2
 mov     r8d, ecx
.LBB19_192:
 mov     r9d, r8d
 mov     r8d, dword, ptr, [rdi, +, 4*r10]
 cmp     r8d, r9d
 jb      .LBB19_195
 inc     r10
 cmp     rdx, r10
 jne     .LBB19_192
.LBB19_194:
 mov     r10, rdx
.LBB19_195:
 lea     r8, [r10, +, r11]
 mov     rdx, r10
 cmp     ecx, eax
 jae     .LBB19_138
.LBB19_196:
 cmp     r11, r8
 ja      .LBB19_336
 cmp     r8, rsi
 ja      .LBB19_335
 cmp     rdx, 2
 jb      .LBB19_138
 mov     qword, ptr, [rsp, +, 48], r8
 mov     rax, rdx
 shr     rax
 cmp     rdx, 16
 jae     .LBB19_202
 mov     r14, r11
 xor     ecx, ecx
 jmp     .LBB19_205
.LBB19_201:
 mov     r15d, 2
 jmp     .LBB19_135
.LBB19_202:
 movabs  rcx, 9223372036854775793
 add     rcx, 7
 and     rcx, rax
 mov     r9, r11
 mov     r11, rax
 and     r11, -8
 neg     r11
 mov     r8, qword, ptr, [rsp, +, 512]
 lea     r8, [r8, +, 4*r9]
 mov     r14, r9
 add     r9, rdx
 mov     r10, qword, ptr, [rsp, +, 504]
 lea     r9, [r10, +, 4*r9]
 xor     r10d, r10d
.LBB19_203:
 movdqu  xmm0, xmmword, ptr, [r8, -, 16]
 movdqu  xmm1, xmmword, ptr, [r8]
 movdqu  xmm2, xmmword, ptr, [r9, +, 4*r10, -, 16]
 movdqu  xmm3, xmmword, ptr, [r9, +, 4*r10]
 pshufd  xmm3, xmm3, 27
 pshufd  xmm2, xmm2, 27
 movdqu  xmmword, ptr, [r8, -, 16], xmm3
 movdqu  xmmword, ptr, [r8], xmm2
 pshufd  xmm0, xmm0, 27
 movdqu  xmmword, ptr, [r9, +, 4*r10], xmm0
 pshufd  xmm0, xmm1, 27
 movdqu  xmmword, ptr, [r9, +, 4*r10, -, 16], xmm0
 add     r10, -8
 add     r8, 32
 cmp     r11, r10
 jne     .LBB19_203
 cmp     rax, rcx
 je      .LBB19_207
.LBB19_205:
 neg     rax
 mov     r11, rcx
 neg     r11
 shl     rcx, 2
 mov     r8, r14
 lea     rcx, [rcx, +, 4*r14]
 add     rcx, r13
 lea     r8, [r14, +, rdx]
 mov     r9, qword, ptr, [rsp, +, 488]
 lea     r8, [r9, +, 4*r8]
.LBB19_206:
 mov     r9d, dword, ptr, [rcx]
 mov     r10d, dword, ptr, [r8, +, 4*r11]
 mov     dword, ptr, [rcx], r10d
 mov     dword, ptr, [r8, +, 4*r11], r9d
 dec     r11
 add     rcx, 4
 cmp     rax, r11
 jne     .LBB19_206
.LBB19_207:
 mov     r8, qword, ptr, [rsp, +, 48]
 mov     r11, r14
 jmp     .LBB19_138
.LBB19_208:
 mov     r15d, 3
 jmp     .LBB19_135
.LBB19_209:
 mov     r15, r14
 jmp     .LBB19_135
.LBB19_210:
 mov     rsi, qword, ptr, [rsp, +, 360]
 shl     rsi, 4
 mov     edx, 8
 mov     rdi, rbx
 mov     rbx, qword, ptr, [rip, +, __rust_dealloc@GOTPCREL]
 call    rbx
 mov     edx, 4
 mov     rdi, r12
 mov     rsi, qword, ptr, [rsp, +, 472]
 call    rbx
 mov     r8, qword, ptr, [rsp, +, 264]
.LBB19_211:
 mov     rbx, rbp
.LBB19_212:
 mov     qword, ptr, [rsp, +, 272], rbx
 mov     qword, ptr, [rsp, +, 280], r13
 mov     qword, ptr, [rsp, +, 288], r8
 mov     rdi, qword, ptr, [rsp, +, 416]
 mov     rsi, qword, ptr, [rsp, +, 424]
 lea     rdx, [rsp, +, 272]
 mov     rbp, r8
 call    core::hash::BuildHasher::hash_one
 mov     r14, rax
 cmp     qword, ptr, [rsp, +, 520], 0
 mov     rcx, qword, ptr, [rsp, +, 448]
 mov     rdx, qword, ptr, [rsp, +, 480]
 jne     .LBB19_215
 lea     rdi, [rsp, +, 384]
 lea     rsi, [rsp, +, 416]
 call    hashbrown::raw::RawTable<T,A>::reserve_rehash
 mov     rdx, qword, ptr, [rsp, +, 384]
 mov     rcx, qword, ptr, [rsp, +, 392]
.LBB19_215:
 mov     qword, ptr, [rsp, +, 240], rbx
 mov     qword, ptr, [rsp], r13
 mov     rax, r14
 shr     rax, 57
 mov     qword, ptr, [rsp, +, 336], rax
 movd    xmm0, eax
 punpcklbw xmm0, xmm0
 pshuflw xmm0, xmm0, 0
 pshufd  xmm0, xmm0, 0
 movdqa  xmmword, ptr, [rsp, +, 144], xmm0
 lea     r15, [4*rbp]
 xor     eax, eax
 mov     qword, ptr, [rsp, +, 32], rax
 xor     ebx, ebx
 mov     rax, rcx
 mov     qword, ptr, [rsp, +, 448], rcx
.LBB19_216:
 and     r14, rcx
 movdqu  xmm0, xmmword, ptr, [rdx, +, r14]
 movdqa  xmmword, ptr, [rsp, +, 16], xmm0
 pcmpeqb xmm0, xmmword, ptr, [rsp, +, 144]
 pmovmskb r13d, xmm0
 test    r13d, r13d
 jne     .LBB19_222
.LBB19_217:
 cmp     rbx, 1
 mov     ebx, 1
 movdqa  xmm0, xmmword, ptr, [rsp, +, 16]
 je      .LBB19_219
 pmovmskb eax, xmm0
 xor     ebx, ebx
 test    eax, eax
 setne   bl
 rep     bsf, eax, eax
 add     rax, r14
 and     rax, rcx
 mov     qword, ptr, [rsp, +, 56], rax
.LBB19_219:
 pcmpeqb xmm0, xmmword, ptr, [rip, +, .LCPI19_0]
 pmovmskb eax, xmm0
 test    eax, eax
 jne     .LBB19_224
 mov     rax, qword, ptr, [rsp, +, 32]
 add     r14, rax
 add     r14, 16
 add     rax, 16
 mov     qword, ptr, [rsp, +, 32], rax
 jmp     .LBB19_216
.LBB19_221:
 lea     eax, [r13, -, 1]
 and     eax, r13d
 mov     r13d, eax
 test    ax, ax
 je      .LBB19_217
.LBB19_222:
 rep     bsf, eax, r13d
 add     rax, r14
 and     rax, rcx
 neg     rax
 lea     rax, [rax, +, 2*rax]
 cmp     rbp, qword, ptr, [rdx, +, 8*rax, -, 8]
 jne     .LBB19_221
 lea     rax, [rdx, +, 8*rax]
 mov     rsi, qword, ptr, [rax, -, 16]
 mov     rdi, qword, ptr, [rsp]
 mov     r12, rdx
 mov     rdx, r15
 call    qword, ptr, [rip, +, bcmp@GOTPCREL]
 mov     rdx, r12
 mov     rcx, qword, ptr, [rsp, +, 448]
 test    eax, eax
 jne     .LBB19_221
 jmp     .LBB19_227
.LBB19_224:
 mov     rsi, qword, ptr, [rsp, +, 56]
 movzx   eax, byte, ptr, [rdx, +, rsi]
 test    al, al
 js      .LBB19_226
 movdqa  xmm0, xmmword, ptr, [rdx]
 pmovmskb eax, xmm0
 rep     bsf, esi, eax
 movzx   eax, byte, ptr, [rdx, +, rsi]
.LBB19_226:
 and     al, 1
 movzx   eax, al
 mov     rbx, qword, ptr, [rsp, +, 400]
 sub     rbx, rax
 mov     qword, ptr, [rsp, +, 400], rbx
 lea     rax, [rsi, -, 16]
 and     rax, rcx
 mov     rcx, qword, ptr, [rsp, +, 336]
 mov     byte, ptr, [rdx, +, rsi], cl
 mov     byte, ptr, [rax, +, rdx, +, 16], cl
 inc     qword, ptr, [rsp, +, 408]
 neg     rsi
 lea     rax, [rsi, +, 2*rsi]
 mov     rcx, qword, ptr, [rsp, +, 288]
 mov     qword, ptr, [rdx, +, 8*rax, -, 8], rcx
 movdqu  xmm0, xmmword, ptr, [rsp, +, 272]
 mov     qword, ptr, [rsp, +, 480], rdx
 movdqu  xmmword, ptr, [rdx, +, 8*rax, -, 24], xmm0
 lea     rdi, [rsp, +, 176]
 call    <core::str::iter::SplitWhitespace as core::iter::traits::iterator::Iterator>::next
 mov     r14, rax
 mov     r15d, 1
 test    rax, rax
 jne     .LBB19_91
 jmp     .LBB19_230
.LBB19_227:
 mov     rsi, qword, ptr, [rsp, +, 240]
 test    rsi, rsi
 je      .LBB19_229
 shl     rsi, 2
 mov     edx, 4
 mov     rdi, qword, ptr, [rsp]
 call    qword, ptr, [rip, +, __rust_dealloc@GOTPCREL]
.LBB19_229:
 xor     r15d, r15d
.LBB19_230:
 mov     rbx, qword, ptr, [rsp, +, 392]
 test    rbx, rbx
 jne     .LBB19_232
 jmp     .LBB19_86
.LBB19_231:
 mov     r15d, 1
 mov     rbx, qword, ptr, [rsp, +, 392]
 test    rbx, rbx
 je      .LBB19_86
.LBB19_232:
 mov     qword, ptr, [rsp, +, 16], r15
 mov     r14, qword, ptr, [rsp, +, 384]
 mov     r12, qword, ptr, [rsp, +, 408]
 test    r12, r12
 je      .LBB19_240
 movdqa  xmm0, xmmword, ptr, [r14]
 pmovmskb r15d, xmm0
 not     r15d
 lea     r13, [r14, +, 16]
 mov     rbp, r14
 jmp     .LBB19_235
.LBB19_234:
 lea     eax, [r15, -, 1]
 and     eax, r15d
 mov     r15d, eax
 dec     r12
 je      .LBB19_240
.LBB19_235:
 test    r15w, r15w
 jne     .LBB19_238
.LBB19_236:
 movdqa  xmm0, xmmword, ptr, [r13]
 pmovmskb r15d, xmm0
 add     rbp, -384
 add     r13, 16
 cmp     r15d, 65535
 je      .LBB19_236
 not     r15d
.LBB19_238:
 rep     bsf, eax, r15d
 neg     rax
 lea     rax, [rax, +, 2*rax]
 mov     rsi, qword, ptr, [rbp, +, 8*rax, -, 24]
 test    rsi, rsi
 je      .LBB19_234
 lea     rax, [8*rax]
 add     rax, rbp
 mov     rdi, qword, ptr, [rax, -, 16]
 shl     rsi, 2
 mov     edx, 4
 call    qword, ptr, [rip, +, __rust_dealloc@GOTPCREL]
 jmp     .LBB19_234
.LBB19_240:
 lea     rax, [rbx, +, 1]
 mov     ecx, 24
 mul     rcx
 add     rax, 15
 and     rax, -16
 add     rbx, rax
 add     rbx, 17
 mov     r15, qword, ptr, [rsp, +, 16]
 je      .LBB19_86
 sub     r14, rax
 mov     edx, 16
 mov     rdi, r14
 mov     rsi, rbx
 call    qword, ptr, [rip, +, __rust_dealloc@GOTPCREL]
 jmp     .LBB19_86
.LBB19_242:
 xor     eax, eax
 mov     qword, ptr, [rsp, +, 440], rax
.LBB19_243:
 mov     rax, qword, ptr, [rsp, +, 440]
 mov     qword, ptr, [rsp, +, 384], rax
 lea     rdi, [rsp, +, 560]
 call    qword, ptr, [rip, +, _ZN3std4time7Instant7elapsed17h4d8d8261e1074b2cE@GOTPCREL]
 mov     qword, ptr, [rsp, +, 272], rax
 mov     dword, ptr, [rsp, +, 280], edx
 lea     rax, [rsp, +, 384]
 mov     qword, ptr, [rsp, +, 176], rax
 mov     rax, qword, ptr, [rip, +, _ZN4core3fmt3num3imp54_$LT$impl$u20$core..fmt..Display$u20$for$u20$usize$GT$3fmt17hd879ea1c6b6d5e73E@GOTPCREL]
 mov     qword, ptr, [rsp, +, 184], rax
 lea     rax, [rsp, +, 272]
 mov     qword, ptr, [rsp, +, 192], rax
 mov     rax, qword, ptr, [rip, +, _ZN57_$LT$core..time..Duration$u20$as$u20$core..fmt..Debug$GT$3fmt17h4747c8e613e16eb4E@GOTPCREL]
 mov     qword, ptr, [rsp, +, 200], rax
 lea     rax, [rip, +, .L__unnamed_11]
 mov     qword, ptr, [rsp, +, 64], rax
 mov     qword, ptr, [rsp, +, 72], 3
 mov     qword, ptr, [rsp, +, 96], 0
 lea     rax, [rsp, +, 176]
 mov     qword, ptr, [rsp, +, 80], rax
 mov     qword, ptr, [rsp, +, 88], 2
 lea     rdi, [rsp, +, 64]
 call    qword, ptr, [rip, +, _ZN3std2io5stdio6_print17hfb5fdd73d3d2794dE@GOTPCREL]
 mov     rbx, qword, ptr, [rip, +, _ZN3std4hash6random11RandomState3new4KEYS7__getit5__KEY17h50d7eafb4bd391a9E@GOTTPOFF]
 cmp     qword, ptr, fs:[rbx], 0
 jne     .LBB19_245
 xor     edi, edi
 call    std::sys::common::thread_local::fast_local::Key<T>::try_initialize
.LBB19_245:
 movups  xmm0, xmmword, ptr, [rip, +, .L__unnamed_7+16]
 movaps  xmmword, ptr, [rsp, +, 80], xmm0
 movups  xmm0, xmmword, ptr, [rip, +, .L__unnamed_7]
 movaps  xmmword, ptr, [rsp, +, 64], xmm0
 movdqu  xmm0, xmmword, ptr, fs:[rbx, +, 8]
 inc     qword, ptr, fs:[rbx, +, 8]
 movdqa  xmmword, ptr, [rsp, +, 96], xmm0
 lea     rsi, [rip, +, .L__unnamed_8]
 xor     r12d, r12d
 xor     eax, eax
 mov     qword, ptr, [rsp, +, 8], rax
 xor     ecx, ecx
 lea     rbx, [rip, +, .L__unnamed_5]
 jmp     .LBB19_248
.LBB19_246:
 mov     qword, ptr, [rsp, +, 88], 0
 lea     rax, [r12, +, 1]
 mov     rcx, rax
 shr     rcx, 3
 and     rax, -8
 sub     rax, rcx
 cmp     r12, 8
 cmovb   rax, r12
 mov     qword, ptr, [rsp, +, 80], rax
.LBB19_247:
 lea     rcx, [rbp, +, 1]
 cmp     rbp, 23284
 jae     .LBB19_280
.LBB19_248:
 xor     r14d, r14d
.LBB19_249:
 lea     rax, [rcx, +, r14]
 movzx   r15d, byte, ptr, [rbx, +, rax]
 cmp     r15d, 10
 je      .LBB19_252
 cmp     r15d, 32
 je      .LBB19_252
 lea     rax, [rcx, +, r14]
 inc     rax
 inc     r14
 dec     rax
 cmp     rax, 23284
 jb      .LBB19_249
 jmp     .LBB19_319
.LBB19_252:
 mov     rbp, rcx
 add     rbp, r14
 jb      .LBB19_338
 mov     r13, rsi
 mov     qword, ptr, [rsp, +, 376], rcx
 lea     rax, [rbx, +, rcx]
 mov     qword, ptr, [rsp, +, 32], rax
 mov     qword, ptr, [rsp, +, 176], rax
 mov     qword, ptr, [rsp, +, 184], r14
 mov     rdi, qword, ptr, [rsp, +, 96]
 mov     rsi, qword, ptr, [rsp, +, 104]
 lea     rdx, [rsp, +, 176]
 call    core::hash::BuildHasher::hash_one
 mov     rbx, rax
 cmp     qword, ptr, [rsp, +, 80], 0
 mov     rsi, r13
 jne     .LBB19_256
 lea     rdi, [rsp, +, 64]
 lea     rsi, [rsp, +, 96]
 call    hashbrown::raw::RawTable<T,A>::reserve_rehash
 mov     rsi, qword, ptr, [rsp, +, 64]
 mov     r12, qword, ptr, [rsp, +, 72]
.LBB19_256:
 mov     rax, rbx
 shr     rax, 57
 mov     qword, ptr, [rsp], rax
 movd    xmm0, eax
 punpcklbw xmm0, xmm0
 pshuflw xmm0, xmm0, 0
 pshufd  xmm0, xmm0, 0
 movdqa  xmmword, ptr, [rsp, +, 336], xmm0
 xor     eax, eax
 mov     qword, ptr, [rsp, +, 56], rax
 xor     eax, eax
 mov     qword, ptr, [rsp, +, 144], rax
 mov     qword, ptr, [rsp, +, 16], rsi
.LBB19_257:
 and     rbx, r12
 movdqu  xmm0, xmmword, ptr, [rsi, +, rbx]
 movdqa  xmmword, ptr, [rsp, +, 240], xmm0
 pcmpeqb xmm0, xmmword, ptr, [rsp, +, 336]
 pmovmskb r13d, xmm0
 test    r13d, r13d
 jne     .LBB19_263
.LBB19_258:
 cmp     qword, ptr, [rsp, +, 144], 1
 mov     ecx, 1
 movdqa  xmm0, xmmword, ptr, [rsp, +, 240]
 je      .LBB19_260
 pmovmskb eax, xmm0
 xor     ecx, ecx
 test    eax, eax
 setne   cl
 rep     bsf, eax, eax
 add     rax, rbx
 and     rax, r12
 mov     qword, ptr, [rsp, +, 328], rax
.LBB19_260:
 pcmpeqb xmm0, xmmword, ptr, [rip, +, .LCPI19_0]
 pmovmskb eax, xmm0
 test    eax, eax
 jne     .LBB19_272
 mov     qword, ptr, [rsp, +, 144], rcx
 mov     rax, qword, ptr, [rsp, +, 56]
 add     rbx, rax
 add     rbx, 16
 add     rax, 16
 mov     qword, ptr, [rsp, +, 56], rax
 jmp     .LBB19_257
.LBB19_262:
 lea     eax, [r13, -, 1]
 and     eax, r13d
 mov     r13d, eax
 test    ax, ax
 je      .LBB19_258
.LBB19_263:
 rep     bsf, ecx, r13d
 add     rcx, rbx
 and     rcx, r12
 shl     rcx, 4
 mov     rax, rsi
 sub     rax, rcx
 cmp     qword, ptr, [rax, -, 8], r14
 jne     .LBB19_262
 mov     rsi, qword, ptr, [rax, -, 16]
 mov     rdi, qword, ptr, [rsp, +, 32]
 mov     rdx, r14
 call    qword, ptr, [rip, +, bcmp@GOTPCREL]
 mov     rsi, qword, ptr, [rsp, +, 16]
 test    eax, eax
 jne     .LBB19_262
 mov     edi, 23285
 cmp     rbp, 23284
 ja      .LBB19_323
 lea     rbx, [rip, +, .L__unnamed_5]
.LBB19_267:
 cmp     byte, ptr, [rbp, +, rbx], 10
 je      .LBB19_269
 inc     rbp
 lea     rax, [rbp, -, 1]
 cmp     rax, 23284
 jb      .LBB19_267
 jmp     .LBB19_323
.LBB19_269:
 cmp     qword, ptr, [rsp, +, 88], 0
 je      .LBB19_247
 mov     r12, qword, ptr, [rsp, +, 72]
 test    r12, r12
 je      .LBB19_246
 lea     rdx, [r12, +, 17]
 mov     rdi, rsi
 mov     esi, 255
 call    qword, ptr, [rip, +, memset@GOTPCREL]
 mov     rsi, qword, ptr, [rsp, +, 16]
 jmp     .LBB19_246
.LBB19_272:
 mov     rdx, qword, ptr, [rsp, +, 328]
 movzx   eax, byte, ptr, [rsi, +, rdx]
 test    al, al
 js      .LBB19_274
 movdqa  xmm0, xmmword, ptr, [rsi]
 pmovmskb eax, xmm0
 rep     bsf, edx, eax
 movzx   eax, byte, ptr, [rsi, +, rdx]
.LBB19_274:
 and     al, 1
 movzx   eax, al
 sub     qword, ptr, [rsp, +, 80], rax
 lea     rax, [rdx, -, 16]
 and     rax, r12
 mov     rcx, qword, ptr, [rsp]
 mov     byte, ptr, [rsi, +, rdx], cl
 mov     byte, ptr, [rax, +, rsi, +, 16], cl
 inc     qword, ptr, [rsp, +, 88]
 shl     rdx, 4
 mov     rax, rsi
 sub     rax, rdx
 neg     rdx
 mov     rcx, qword, ptr, [rsp, +, 32]
 mov     qword, ptr, [rsi, +, rdx, -, 16], rcx
 mov     qword, ptr, [rax, -, 8], r14
 cmp     r15b, 10
 lea     rbx, [rip, +, .L__unnamed_5]
 mov     rcx, qword, ptr, [rsp, +, 376]
 jne     .LBB19_279
 inc     qword, ptr, [rsp, +, 8]
 cmp     qword, ptr, [rsp, +, 88], 0
 je      .LBB19_279
 mov     r12, qword, ptr, [rsp, +, 72]
 test    r12, r12
 je      .LBB19_278
 lea     rdx, [r12, +, 17]
 mov     rdi, rsi
 mov     esi, 255
 call    qword, ptr, [rip, +, memset@GOTPCREL]
 mov     rsi, qword, ptr, [rsp, +, 16]
.LBB19_278:
 mov     qword, ptr, [rsp, +, 88], 0
 lea     rax, [r12, +, 1]
 mov     rcx, rax
 shr     rcx, 3
 and     rax, -8
 sub     rax, rcx
 cmp     r12, 8
 cmovb   rax, r12
 mov     qword, ptr, [rsp, +, 80], rax
 mov     rcx, qword, ptr, [rsp, +, 376]
.LBB19_279:
 lea     rax, [rcx, +, r14]
 add     rcx, r14
 inc     rcx
 cmp     rax, 23284
 jb      .LBB19_248
.LBB19_280:
 mov     rsi, qword, ptr, [rsp, +, 72]
 test    rsi, rsi
 je      .LBB19_283
 mov     rax, rsi
 shl     rax, 4
 add     rsi, rax
 add     rsi, 33
 je      .LBB19_283
 mov     rdi, qword, ptr, [rsp, +, 64]
 sub     rdi, rax
 add     rdi, -16
 mov     edx, 16
 call    qword, ptr, [rip, +, __rust_dealloc@GOTPCREL]
.LBB19_283:
 mov     rax, qword, ptr, [rsp, +, 8]
 mov     qword, ptr, [rsp, +, 272], rax
 lea     rax, [rsp, +, 272]
 mov     qword, ptr, [rsp, +, 176], rax
 mov     rax, qword, ptr, [rip, +, _ZN4core3fmt3num3imp54_$LT$impl$u20$core..fmt..Display$u20$for$u20$usize$GT$3fmt17hd879ea1c6b6d5e73E@GOTPCREL]
 mov     qword, ptr, [rsp, +, 184], rax
 lea     rax, [rip, +, .L__unnamed_12]
 mov     qword, ptr, [rsp, +, 64], rax
 mov     qword, ptr, [rsp, +, 72], 2
 mov     qword, ptr, [rsp, +, 96], 0
 lea     rax, [rsp, +, 176]
 mov     qword, ptr, [rsp, +, 80], rax
 mov     qword, ptr, [rsp, +, 88], 1
 lea     rdi, [rsp, +, 64]
 call    qword, ptr, [rip, +, _ZN3std2io5stdio6_print17hfb5fdd73d3d2794dE@GOTPCREL]
 mov     rax, qword, ptr, [rip, +, __rust_no_alloc_shim_is_unstable@GOTPCREL]
 movzx   eax, byte, ptr, [rax]
 mov     edi, 256
 mov     esi, 8
 call    qword, ptr, [rip, +, __rust_alloc@GOTPCREL]
 test    rax, rax
 je      .LBB19_345
 mov     r14, rax
 mov     qword, ptr, [rsp, +, 176], 16
 mov     qword, ptr, [rsp, +, 184], rax
 mov     qword, ptr, [rsp, +, 192], 0
 mov     qword, ptr, [rsp, +, 64], 0
 mov     qword, ptr, [rsp, +, 72], 23285
 mov     qword, ptr, [rsp, +, 80], rbx
 mov     qword, ptr, [rsp, +, 88], 23285
 mov     qword, ptr, [rsp, +, 96], 0
 mov     qword, ptr, [rsp, +, 104], 23285
 mov     qword, ptr, [rsp, +, 112], 1
 movabs  rax, 42949672970
 mov     qword, ptr, [rsp, +, 120], rax
 mov     word, ptr, [rsp, +, 128], 0
 lea     rdi, [rsp, +, 64]
 call    <core::str::iter::Lines as core::iter::traits::iterator::Iterator>::next
 mov     rcx, rax
 test    rax, rax
 je      .LBB19_317
 mov     r12, rdx
 xor     eax, eax
 mov     qword, ptr, [rsp, +, 144], rax
 movabs  rbp, 4294981120
 xor     r15d, r15d
.LBB19_287:
 xor     esi, esi
.LBB19_288:
 mov     rax, r12
 mov     qword, ptr, [rsp, +, 16], rcx
 mov     rdx, rcx
 mov     dword, ptr, [rsp, +, 32], esi
 mov     ecx, esi
 jmp     .LBB19_291
.LBB19_289:
 mov     al, 1
 mov     dword, ptr, [rsp, +, 32], eax
 xor     eax, eax
 mov     rdx, rbx
 xor     r13d, r13d
.LBB19_290:
 test    r13, r13
 jne     .LBB19_299
.LBB19_291:
 test    cl, cl
 jne     .LBB19_310
 mov     rbx, rdx
 mov     cl, 1
 test    rax, rax
 je      .LBB19_289
 xor     r13d, r13d
 jmp     .LBB19_295
.LBB19_294:
 inc     r13
 cmp     rax, r13
 je      .LBB19_298
.LBB19_295:
 movzx   edx, byte, ptr, [rbx, +, r13]
 cmp     rdx, 32
 ja      .LBB19_294
 bt      rbp, rdx
 jae     .LBB19_294
 mov     r12, r13
 not     r12
 add     r12, rax
 lea     rdx, [rbx, +, r13]
 inc     rdx
 xor     ecx, ecx
 mov     rax, r12
 mov     qword, ptr, [rsp, +, 16], rdx
 jmp     .LBB19_290
.LBB19_298:
 mov     dl, 1
 mov     dword, ptr, [rsp, +, 32], edx
 mov     rdx, rbx
 mov     r13, rax
 jmp     .LBB19_290
.LBB19_299:
 test    rbx, rbx
 je      .LBB19_310
 mov     qword, ptr, [rsp, +, 240], r15
 cmp     qword, ptr, [rsp, +, 144], 0
 je      .LBB19_305
 mov     r15, qword, ptr, [rsp, +, 144]
 shl     r15, 4
 xor     ebp, ebp
 jmp     .LBB19_303
.LBB19_302:
 add     rbp, 16
 cmp     r15, rbp
 je      .LBB19_305
.LBB19_303:
 cmp     qword, ptr, [r14, +, rbp, +, 8], r13
 jne     .LBB19_302
 mov     rdi, qword, ptr, [r14, +, rbp]
 mov     rsi, rbx
 mov     rdx, r13
 call    qword, ptr, [rip, +, bcmp@GOTPCREL]
 test    eax, eax
 jne     .LBB19_302
 jmp     .LBB19_313
.LBB19_305:
 mov     rsi, qword, ptr, [rsp, +, 144]
 cmp     rsi, qword, ptr, [rsp, +, 176]
 movabs  rbp, 4294981120
 jne     .LBB19_308
 lea     rdi, [rsp, +, 176]
 call    alloc::raw_vec::RawVec<T,A>::reserve_for_push
 mov     r14, qword, ptr, [rsp, +, 184]
 mov     rcx, qword, ptr, [rsp, +, 192]
 jmp     .LBB19_309
.LBB19_308:
 mov     rcx, rsi
.LBB19_309:
 mov     r15, qword, ptr, [rsp, +, 240]
 mov     rax, rcx
 shl     rax, 4
 mov     qword, ptr, [r14, +, rax], rbx
 mov     qword, ptr, [r14, +, rax, +, 8], r13
 inc     rcx
 mov     rax, rcx
 mov     qword, ptr, [rsp, +, 144], rcx
 mov     qword, ptr, [rsp, +, 192], rcx
 mov     rcx, qword, ptr, [rsp, +, 16]
 mov     esi, dword, ptr, [rsp, +, 32]
 jmp     .LBB19_288
.LBB19_310:
 mov     qword, ptr, [rsp, +, 192], 0
 inc     r15
 xor     eax, eax
 mov     qword, ptr, [rsp, +, 144], rax
.LBB19_311:
 lea     rdi, [rsp, +, 64]
 call    <core::str::iter::Lines as core::iter::traits::iterator::Iterator>::next
 mov     rcx, rax
 mov     r12, rdx
 test    rax, rax
 jne     .LBB19_287
 jmp     .LBB19_314
.LBB19_313:
 mov     r15, qword, ptr, [rsp, +, 240]
 movabs  rbp, 4294981120
 jmp     .LBB19_311
.LBB19_314:
 mov     rsi, qword, ptr, [rsp, +, 176]
 test    rsi, rsi
 je      .LBB19_321
 mov     r14, qword, ptr, [rsp, +, 184]
 shl     rsi, 4
 jmp     .LBB19_318
.LBB19_323:
 lea     rdx, [rip, +, .L__unnamed_13]
 jmp     .LBB19_320
.LBB19_317:
 mov     esi, 256
 xor     r15d, r15d
.LBB19_318:
 lea     rbx, [rip, +, .L__unnamed_12]
 mov     edx, 8
 mov     rdi, r14
 call    qword, ptr, [rip, +, __rust_dealloc@GOTPCREL]
 jmp     .LBB19_322
.LBB19_319:
 cmp     rcx, 23285
 mov     edi, 23284
 cmovae  rdi, rcx
 inc     rdi
 lea     rdx, [rip, +, .L__unnamed_14]
.LBB19_320:
 mov     esi, 23285
 call    qword, ptr, [rip, +, _ZN4core9panicking18panic_bounds_check17h8012f4ce5cad0c78E@GOTPCREL]
 jmp     .LBB19_344
.LBB19_321:
 lea     rbx, [rip, +, .L__unnamed_12]
.LBB19_322:
 mov     qword, ptr, [rsp, +, 272], r15
 lea     rax, [rsp, +, 272]
 mov     qword, ptr, [rsp, +, 176], rax
 mov     rax, qword, ptr, [rip, +, _ZN4core3fmt3num3imp54_$LT$impl$u20$core..fmt..Display$u20$for$u20$usize$GT$3fmt17hd879ea1c6b6d5e73E@GOTPCREL]
 mov     qword, ptr, [rsp, +, 184], rax
 mov     qword, ptr, [rsp, +, 64], rbx
 mov     qword, ptr, [rsp, +, 72], 2
 mov     qword, ptr, [rsp, +, 96], 0
 lea     rax, [rsp, +, 176]
 mov     qword, ptr, [rsp, +, 80], rax
 mov     qword, ptr, [rsp, +, 88], 1
 lea     rdi, [rsp, +, 64]
 call    qword, ptr, [rip, +, _ZN3std2io5stdio6_print17hfb5fdd73d3d2794dE@GOTPCREL]
 add     rsp, 600
 pop     rbx
 pop     r12
 pop     r13
 pop     r14
 pop     r15
 pop     rbp
 ret
.LBB19_324:
 lea     rdx, [rip, +, .L__unnamed_15]
 mov     rdi, r15
 mov     r12, qword, ptr, [rsp, +, 8]
 call    qword, ptr, [rip, +, _ZN4core5slice5index24slice_end_index_len_fail17h837701fcdf788087E@GOTPCREL]
 jmp     .LBB19_344
.LBB19_325:
 lea     rax, [rip, +, .L__unnamed_16]
 mov     qword, ptr, [rsp, +, 272], rax
 lea     rsi, [rip, +, .L__unnamed_17]
 jmp     .LBB19_327
.LBB19_326:
 lea     rax, [rip, +, .L__unnamed_16]
 mov     qword, ptr, [rsp, +, 272], rax
 lea     rsi, [rip, +, .L__unnamed_18]
.LBB19_327:
 mov     rdi, qword, ptr, [rsp, +, 496]
 mov     qword, ptr, [rdi, +, 8], 1
 lea     rax, [rip, +, .L__unnamed_19]
 mov     qword, ptr, [rdi, +, 16], rax
 pxor    xmm0, xmm0
 movdqu  xmmword, ptr, [rdi, +, 24], xmm0
 call    qword, ptr, [rip, +, _ZN4core9panicking9panic_fmt17h3f2350d70561f7cbE@GOTPCREL]
 jmp     .LBB19_344
.LBB19_328:
 lea     rdx, [rip, +, .L__unnamed_15]
 mov     rsi, r15
 mov     r12, qword, ptr, [rsp, +, 8]
 call    qword, ptr, [rip, +, _ZN4core5slice5index22slice_index_order_fail17h0f489964b63bc802E@GOTPCREL]
 jmp     .LBB19_344
.LBB19_329:
 test    rdi, rdi
 jne     .LBB19_332
.LBB19_330:
 call    qword, ptr, [rip, +, _ZN5alloc7raw_vec17capacity_overflow17h97ac14b5580c5b5dE@GOTPCREL]
 jmp     .LBB19_344
.LBB19_331:
 lea     rdi, [rip, +, .L__unnamed_20]
 lea     rdx, [rip, +, .L__unnamed_21]
 mov     esi, 44
 mov     rbx, qword, ptr, [rsp, +, 168]
 call    qword, ptr, [rip, +, _ZN4core9panicking5panic17h87fd92496103e3b8E@GOTPCREL]
 jmp     .LBB19_344
.LBB19_332:
 mov     rsi, qword, ptr, [rsp, +, 592]
 call    qword, ptr, [rip, +, _ZN5alloc5alloc18handle_alloc_error17h14ce362a8d45e3a5E@GOTPCREL]
 jmp     .LBB19_344
.LBB19_333:
 lea     rax, [rip, +, .L__unnamed_22]
 mov     qword, ptr, [rsp, +, 464], rax
 jmp     .LBB19_337
.LBB19_334:
 lea     rdi, [rip, +, .L__unnamed_23]
 lea     rdx, [rip, +, .L__unnamed_24]
 mov     esi, 43
 mov     r12, qword, ptr, [rsp, +, 8]
 mov     rbx, qword, ptr, [rsp, +, 168]
 mov     r13, qword, ptr, [rsp]
 call    qword, ptr, [rip, +, _ZN4core9panicking5panic17h87fd92496103e3b8E@GOTPCREL]
 jmp     .LBB19_344
.LBB19_335:
 lea     rdx, [rip, +, .L__unnamed_10]
 mov     rdi, r8
 call    qword, ptr, [rip, +, _ZN4core5slice5index24slice_end_index_len_fail17h837701fcdf788087E@GOTPCREL]
 jmp     .LBB19_344
.LBB19_336:
 mov     r14, r11
 mov     qword, ptr, [rsp, +, 48], r8
.LBB19_337:
 mov     rdi, r14
 mov     rsi, qword, ptr, [rsp, +, 48]
 mov     rdx, qword, ptr, [rsp, +, 464]
 mov     r13, qword, ptr, [rsp]
 mov     rbp, qword, ptr, [rsp, +, 368]
 mov     r12, qword, ptr, [rsp, +, 8]
 mov     rbx, qword, ptr, [rsp, +, 168]
 call    qword, ptr, [rip, +, _ZN4core5slice5index22slice_index_order_fail17h0f489964b63bc802E@GOTPCREL]
 jmp     .LBB19_344
.LBB19_338:
 add     r14, rcx
 lea     rdx, [rip, +, .L__unnamed_25]
 mov     rdi, rcx
 mov     rsi, r14
 call    qword, ptr, [rip, +, _ZN4core5slice5index22slice_index_order_fail17h0f489964b63bc802E@GOTPCREL]
 jmp     .LBB19_344
.LBB19_339:
 call    qword, ptr, [rip, +, _ZN5alloc7raw_vec17capacity_overflow17h97ac14b5580c5b5dE@GOTPCREL]
 jmp     .LBB19_344
.LBB19_340:
 mov     edi, 4
 mov     rsi, r15
 call    qword, ptr, [rip, +, _ZN5alloc5alloc18handle_alloc_error17h14ce362a8d45e3a5E@GOTPCREL]
 jmp     .LBB19_344
.LBB19_341:
 mov     edi, 8
 mov     esi, 64
 call    qword, ptr, [rip, +, _ZN5alloc5alloc18handle_alloc_error17h14ce362a8d45e3a5E@GOTPCREL]
.LBB19_342:
 lea     rdi, [rip, +, .L__unnamed_23]
 lea     rdx, [rip, +, .L__unnamed_26]
 mov     esi, 43
 call    qword, ptr, [rip, +, _ZN4core9panicking5panic17h87fd92496103e3b8E@GOTPCREL]
 jmp     .LBB19_344
.LBB19_343:
 lea     rdi, [rip, +, .L__unnamed_23]
 lea     rdx, [rip, +, .L__unnamed_27]
 mov     esi, 43
 call    qword, ptr, [rip, +, _ZN4core9panicking5panic17h87fd92496103e3b8E@GOTPCREL]
.LBB19_344:
 ud2
.LBB19_345:
 mov     edi, 8
 mov     esi, 256
 call    qword, ptr, [rip, +, _ZN5alloc5alloc18handle_alloc_error17h14ce362a8d45e3a5E@GOTPCREL]
.LBB19_346:
 jmp     .LBB19_358
.LBB19_347:
 jmp     .LBB19_358
.LBB19_348:
 mov     r14, rax
 jmp     .LBB19_366
.LBB19_349:
 jmp     .LBB19_358
.LBB19_350:
 mov     r14, rax
 mov     r13, qword, ptr, [rsp]
 mov     rbp, qword, ptr, [rsp, +, 368]
 jmp     .LBB19_366
.LBB19_351:
 mov     r14, rax
 mov     r13, qword, ptr, [rsp]
 mov     rbp, qword, ptr, [rsp, +, 368]
 mov     r12, qword, ptr, [rsp, +, 8]
 jmp     .LBB19_365
.LBB19_352:
 mov     r14, rax
 mov     rbp, rbx
 test    rbx, rbx
 jne     .LBB19_367
 jmp     .LBB19_368
.LBB19_353:
 mov     r14, rax
 mov     rsi, qword, ptr, [rsp, +, 280]
 test    rsi, rsi
 je      .LBB19_385
 mov     rax, rsi
 shl     rax, 4
 add     rsi, rax
 add     rsi, 33
 je      .LBB19_385
 mov     rdi, qword, ptr, [rsp, +, 272]
 jmp     .LBB19_383
.LBB19_356:
 jmp     .LBB19_380
.LBB19_357:
.LBB19_358:
 mov     r14, rax
 mov     rsi, qword, ptr, [rsp, +, 176]
 test    rsi, rsi
 je      .LBB19_385
 mov     rdi, qword, ptr, [rsp, +, 184]
 shl     rsi, 4
 mov     edx, 8
 jmp     .LBB19_384
.LBB19_360:
 mov     r14, rax
 jmp     .LBB19_368
.LBB19_361:
 mov     r14, rax
 mov     rbx, qword, ptr, [rsp, +, 168]
 jmp     .LBB19_364
.LBB19_362:
 mov     r14, rax
 mov     rbp, r13
 test    r13, r13
 mov     r13, qword, ptr, [rsp]
 jne     .LBB19_367
 jmp     .LBB19_368
.LBB19_363:
 mov     r14, rax
.LBB19_364:
 mov     rsi, qword, ptr, [rsp, +, 360]
 shl     rsi, 4
 mov     edx, 8
 mov     rdi, rbx
 call    qword, ptr, [rip, +, __rust_dealloc@GOTPCREL]
.LBB19_365:
 mov     edx, 4
 mov     rdi, r12
 mov     rsi, qword, ptr, [rsp, +, 472]
 call    qword, ptr, [rip, +, __rust_dealloc@GOTPCREL]
.LBB19_366:
 test    rbp, rbp
 je      .LBB19_368
.LBB19_367:
 shl     rbp, 2
 mov     edx, 4
 mov     rdi, r13
 mov     rsi, rbp
 call    qword, ptr, [rip, +, __rust_dealloc@GOTPCREL]
.LBB19_368:
 mov     rbx, qword, ptr, [rsp, +, 392]
 test    rbx, rbx
 je      .LBB19_385
 mov     rax, qword, ptr, [rsp, +, 384]
 mov     qword, ptr, [rsp, +, 16], rax
 mov     r12, qword, ptr, [rsp, +, 408]
 test    r12, r12
 jne     .LBB19_372
.LBB19_370:
 lea     rax, [rbx, +, 1]
 mov     ecx, 24
 mul     rcx
 add     rax, 15
 and     rax, -16
 add     rbx, rax
 add     rbx, 17
 je      .LBB19_385
 mov     rdi, qword, ptr, [rsp, +, 16]
 sub     rdi, rax
 mov     edx, 16
 mov     rsi, rbx
 jmp     .LBB19_384
.LBB19_372:
 mov     rbp, qword, ptr, [rsp, +, 16]
 movdqa  xmm0, xmmword, ptr, [rbp]
 pmovmskb r15d, xmm0
 not     r15d
 lea     r13, [rbp, +, 16]
 jmp     .LBB19_374
.LBB19_373:
 lea     eax, [r15, -, 1]
 and     eax, r15d
 mov     r15d, eax
 dec     r12
 je      .LBB19_370
.LBB19_374:
 test    r15w, r15w
 jne     .LBB19_377
.LBB19_375:
 movdqa  xmm0, xmmword, ptr, [r13]
 pmovmskb r15d, xmm0
 add     rbp, -384
 add     r13, 16
 cmp     r15d, 65535
 je      .LBB19_375
 not     r15d
.LBB19_377:
 rep     bsf, eax, r15d
 neg     rax
 lea     rax, [rax, +, 2*rax]
 mov     rsi, qword, ptr, [rbp, +, 8*rax, -, 24]
 test    rsi, rsi
 je      .LBB19_373
 lea     rax, [8*rax]
 add     rax, rbp
 mov     rdi, qword, ptr, [rax, -, 16]
 shl     rsi, 2
 mov     edx, 4
 call    qword, ptr, [rip, +, __rust_dealloc@GOTPCREL]
 jmp     .LBB19_373
.LBB19_379:
.LBB19_380:
 mov     r14, rax
 mov     rsi, qword, ptr, [rsp, +, 72]
 test    rsi, rsi
 je      .LBB19_385
 mov     rax, rsi
 shl     rax, 4
 add     rsi, rax
 add     rsi, 33
 je      .LBB19_385
 mov     rdi, qword, ptr, [rsp, +, 64]
.LBB19_383:
 sub     rdi, rax
 add     rdi, -16
 mov     edx, 16
.LBB19_384:
 call    qword, ptr, [rip, +, __rust_dealloc@GOTPCREL]
.LBB19_385:
 mov     rdi, r14
 call    _Unwind_Resume
