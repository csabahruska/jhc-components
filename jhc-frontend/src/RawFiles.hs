module RawFiles where

import Data.ByteString.Unsafe
import Data.ByteString
import System.IO.Unsafe

-- | Generated from src\/data\/ViaGhc.hs
{-# NOINLINE viaghc_hs #-}
viaghc_hs :: ByteString
viaghc_hs = unsafePerformIO $ unsafePackAddress "\
 \{-# OPTIONS_GHC -fglasgow-exts -fno-implicit-prelude #-}\n\
 \module Main(main) where\n\
 \\n\
 \import GHC.Int\n\
 \import GHC.Word\n\
 \import GHC.IOBase\n\
 \import GHC.Prim\n\
 \import GHC.Base\n\
 \import GHC.Ptr\n\
 \import GHC.Err\n\
 \\n\
 \type World__ = State# RealWorld\n\
 \type Array__ a = Array# a\n\
 \type MutArray__ a = MutableArray# RealWorld a\n\
 \type Ref__ a = MutVar# RealWorld a\n\
 \\n\
 \type Nothing = ()\n\
 \\n\
 \theNothing :: Nothing\n\
 \theNothing = ()\n\
 \\n\
 \type JIO a = World__ -> (# World__, a #)\n\
 \\n\
 \main :: IO ()\n\
 \main = IO $ \\rw -> case theRealMain rw of rw' -> (# rw', () #)\n\
 \\n\
 \unPtr :: Ptr a -> Addr#\n\
 \unPtr ptr = case ptr of\n\
 \    Ptr addr -> addr\n\
 \\n\
 \unFunPtr :: FunPtr a -> Addr#\n\
 \unFunPtr ptr = case ptr of\n\
 \    FunPtr addr -> addr\n\
 \\n\
 \fromBool :: Bool -> Int#\n\
 \fromBool b = case b of\n\
 \    False -> 0#\n\
 \    True -> 1#\n\
 \\n\
 \gteChar# a b = gtChar# a b || eqChar# a b\n\
 \lteChar# a b = ltChar# a b || eqChar# a b\n\
 \\n\
 \plusAddr__ :: Addr# -> Addr# -> Addr#\n\
 \plusAddr__ a1 a2 = plusAddr# a1 (addr2Int# a2)\n\
 \\n\
 \alloca__ :: Int# -> (Addr# -> JIO a) -> JIO a\n\
 \alloca__ size action s =\n\
 \     case newPinnedByteArray# size s      of { (# s, mbarr# #) ->\n\
 \     case unsafeFreezeByteArray# mbarr# s of { (# s, barr#  #) ->\n\
 \     case action (byteArrayContents# barr#) s of { (# s, r #) ->\n\
 \     case touch# barr# s of { s -> (# s, r #) }\n\
 \     }}}\n\
 \\n\
 \word2Char__ x = chr# (word2Int# x)\n\
 \char2Word__ x = int2Word# (ord# x)\n\
 \addr2Word__ x = int2Word# (addr2Int# x)\n\
 \word2Addr__ x = int2Addr# (word2Int# x)\n\
 \\n\
 \convertString :: [Char] -> ListTCon Char\n\
 \convertString [] = jhc_EmptyList\n\
 \convertString (x:xs) = jhc_Cons x (convertString xs)\n\
 \\n\
 \{-\n\
 \error__ :: Addr# -> a\n\
 \error__ s = unsafePerformIO $ do\n\
 \    error_show s\n\
 \    error_exit (I# 255#)\n\
 \\n\
 \errorInt__ :: Addr# -> Int#\n\
 \errorInt__ s = seq (unsafePerformIO $ do\n\
 \    error_show s\n\
 \    error_exit (I# 255#)) 0#\n\
 \\n\
 \errorWord__ :: Addr# -> Word#\n\
 \errorWord__ s = seq (unsafePerformIO $ do\n\
 \    error_show s\n\
 \    error_exit (I# 255#)) (int2Word# 0#)\n\
 \\n\
 \errorAddr__ :: Addr# -> Addr#\n\
 \errorAddr__ s = seq (unsafePerformIO $ do\n\
 \    error_show s\n\
 \    error_exit (I# 255#)) (int2Addr# 0#)\n\
 \foreign import ccall unsafe \"puts\" error_show :: Ptr a -> IO ()\n\
 \foreign import ccall unsafe \"exit\" error_exit :: Int -> IO a\n\
 \ -}\n\
 \\n\
 \{-# NOINLINE newWorld__ #-}\n\
 \newWorld__ :: a -> World__\n\
 \newWorld__ a = case lazy a of\n\
 \    _ -> realWorld#\n\
 \\n\
 \theRealMain :: World__ -> World__\n\
 \\n\
 \"#

-- | Generated from src\/data\/prelude.m4
{-# NOINLINE prelude_m4 #-}
prelude_m4 :: ByteString
prelude_m4 = unsafePerformIO $ unsafePackAddress "\
 \m4_divert(`-1')\n\
 \m4_define(LBRACE,{)\n\
 \m4_define(RBRACE,})\n\
 \m4_define(LPAREN,`(')\n\
 \m4_define(RPAREN,`)')\n\
 \m4_define(COMMA,`,')\n\
 \m4_changequote({{,}})\n\
 \\n\
 \m4_changecom({-,-})\n\
 \\n\
 \m4_define(ONCE,{{m4_ifdef(done-$1,{{m4_dnl}},{{m4_define(done-$1,1)$1}})}})\n\
 \\n\
 \m4_define({{m4_for}},{{m4_ifelse($#,0,{{{{$0}}}},{{m4_ifelse(m4_eval($2<=$3),1,\n\
 \{{m4_pushdef({{$1}},$2)$4{{}}m4_popdef({{$1}})$0({{$1}},m4_incr($2),$3,{{$4}})}})}})}})\n\
 \\n\
 \m4_define({{m4_foreach}},{{m4_ifelse(m4_eval($#>2),1,\n\
 \{{m4_pushdef({{$1}},{{$3}})$2{{}}m4_popdef({{$1}})m4_dnl\n\
 \{{}}m4_ifelse(m4_eval($#>3),1,{{$0({{$1}},{{$2}},m4_shift(m4_shift(m4_shift($@))))}})}})}})\n\
 \m4_divert{{}}m4_dnl\n\
 \"#

-- | Generated from src\/data\/targets.ini
{-# NOINLINE targets_ini #-}
targets_ini :: ByteString
targets_ini = unsafePerformIO $ unsafePackAddress "\
 \;\n\
 \; configuration file for architectures and compiler options.\n\
 \;\n\
 \; the final value set is the one used.\n\
 \;\n\
 \; all '-m' parameters on the command line are parsed and processed in order.\n\
 \;\n\
 \; there is an implicit -mdefault processed first\n\
 \; entries in the user config file are appended to this one.\n\
 \;\n\
 \; the cross compilation entries in this file should be treated as examples.\n\
 \; although they work out of the box for many systems, cross compilation\n\
 \; environments differ, so you may need to override them for your\n\
 \; specific setup.\n\
 \\n\
 \[default]\n\
 \cc=gcc\n\
 \gc=jgc\n\
 \cflags=-std=gnu99 -D_GNU_SOURCE -falign-functions=4 -ffast-math -Wextra -Wall -Wno-unused-parameter -fno-strict-aliasing\n\
 \cflags_debug=-g\n\
 \cflags_nodebug=-DNDEBUG -O3\n\
 \profile=false\n\
 \autoload=haskell2010,haskell-extras,haskell98\n\
 \\n\
 \\n\
 \; cross compilation entries\n\
 \\n\
 \[win32]\n\
 \cc=i386-mingw32-gcc\n\
 \executable_extension=.exe\n\
 \merge=i686\n\
 \\n\
 \[wii]\n\
 \cc=powerpc-eabi-gcc\n\
 \byteorder=be\n\
 \cflags+=-g -DGEKKO -D__WORDSIZE=32 -mrvl -mcpu=750 -meabi -mhard-float\n\
 \executable_extension=.elf\n\
 \bits=32\n\
 \bits_max=64\n\
 \merge=be32\n\
 \\n\
 \; macintosh, this is for cross compiling, not for native compilation on osx\n\
 \[osx]\n\
 \\n\
 \[osx-intel]\n\
 \cc=i686-apple-darwin9-gcc\n\
 \merge=i686\n\
 \merge=osx\n\
 \\n\
 \[osx-powerpc]\n\
 \cc=powerpc-apple-darwin9-gcc\n\
 \merge=be32\n\
 \merge=osx\n\
 \\n\
 \; a couple specific cpus\n\
 \[i686]\n\
 \merge=le32\n\
 \arch=i686\n\
 \bits_max=64\n\
 \cflags_nodebug+=-fomit-frame-pointer\n\
 \\n\
 \[x86_64]\n\
 \bits_max=64\n\
 \merge=le64\n\
 \\n\
 \[le32]\n\
 \\n\
 \byteorder=le\n\
 \merge=32\n\
 \\n\
 \[be32]\n\
 \byteorder=be\n\
 \merge=32\n\
 \\n\
 \[le64]\n\
 \byteorder=le\n\
 \merge=64\n\
 \\n\
 \[be64]\n\
 \byteorder=be\n\
 \merge=64\n\
 \\n\
 \[32]\n\
 \cflags+=-m32\n\
 \bits=32\n\
 \\n\
 \[64]\n\
 \cflags+=-m64\n\
 \bits=64\n\
 \\n\
 \"#

-- | Generated from rts\/rts\/constants.h
{-# NOINLINE constants_h #-}
constants_h :: ByteString
constants_h = unsafePerformIO $ unsafePackAddress "\
 \#ifndef RTS_CONSTANTS_H\n\
 \#define RTS_CONSTANTS_H\n\
 \/* these constants are shared between jhc-prim and the rts. */\n\
 \\n\
 \// Normal memory block.\n\
 \#define SLAB_FLAG_NONE         0\n\
 \\n\
 \// Each element has a finalizer-list as its second word.\n\
 \#define SLAB_FLAG_FINALIZER    1\n\
 \\n\
 \// In addition to whatever other finalization is done, 'free' should be called\n\
 \// on the first word of each entry.\n\
 \#define SLAB_FLAG_FREE         2\n\
 \\n\
 \// Finalizers should be delayed until entire slab is freed up and individually\n\
 \// freed members need not be kept track of.\n\
 \#define SLAB_FLAG_DELAY        4\n\
 \\n\
 \// A global finalizer exists for this slab\n\
 \#define SLAB_GLOBAL_FINALIZER  8\n\
 \\n\
 \// slab is a monolith, should be 'free'd when done with and not returned to\n\
 \// cache.\n\
 \#define SLAB_MONOLITH          16\n\
 \\n\
 \// virtual flags are never set in a cache but are used internally to keep track\n\
 \// of things.\n\
 \\n\
 \// virtual flag to indicate location is a value\n\
 \#define SLAB_VIRTUAL_VALUE     256\n\
 \\n\
 \// virtual flag to indicate location has a special intererpretation.\n\
 \#define SLAB_VIRTUAL_SPECIAL   512\n\
 \\n\
 \// virtual flag to indication location is a constant.\n\
 \#define SLAB_VIRTUAL_CONSTANT  1024\n\
 \\n\
 \// virtual flag to indication location has been freed. (for debugging)\n\
 \#define SLAB_VIRTUAL_FREED     2048\n\
 \\n\
 \// virtual flag to indication location is lazy.\n\
 \#define SLAB_VIRTUAL_LAZY      4096\n\
 \\n\
 \// virtual flag to indication location is func.\n\
 \#define SLAB_VIRTUAL_FUNC      8192\n\
 \\n\
 \#endif\n\
 \"#

-- | Generated from rts\/rts\/stableptr.c
{-# NOINLINE stableptr_c #-}
stableptr_c :: ByteString
stableptr_c = unsafePerformIO $ unsafePackAddress "\
 \#include \"jhc_rts_header.h\"\n\
 \#include \"sys/queue.h\"\n\
 \\n\
 \struct StablePtr_list root_StablePtrs = LIST_HEAD_INITIALIZER();\n\
 \\n\
 \wptr_t c_newStablePtr(sptr_t c)\n\
 \{\n\
 \        struct StablePtr *sp = malloc(sizeof(struct StablePtr));\n\
 \        sp->contents = c;\n\
 \        LIST_INSERT_HEAD(&root_StablePtrs, sp, link);\n\
 \        assert(GET_PTYPE(sp) == 0);\n\
 \        return (wptr_t)TO_SPTR(P_VALUE, (wptr_t)sp);\n\
 \}\n\
 \\n\
 \void c_freeStablePtr(wptr_t wp)\n\
 \{\n\
 \        struct StablePtr *sp = FROM_SPTR((HsPtr)wp);\n\
 \        LIST_REMOVE(sp, link);\n\
 \        free(sp);\n\
 \}\n\
 \\n\
 \sptr_t c_derefStablePtr(wptr_t wp)\n\
 \{\n\
 \        struct StablePtr *sp = FROM_SPTR((HsPtr)wp);\n\
 \        return sp->contents;\n\
 \}\n\
 \\n\
 \void hs_free_stable_ptr(HsStablePtr sp)\n\
 \{\n\
 \        c_freeStablePtr((HsStablePtr)sp);\n\
 \}\n\
 \void hs_free_fun_ptr(HsFunPtr fp) {}\n\
 \\n\
 \/*\n\
 \wptr_t c_castPtrToStablePtr(void *)\n\
 \void * c_castStablePtrToPtr(wptr_t)\n\
 \*/\n\
 \"#

-- | Generated from rts\/sys\/queue.h
{-# NOINLINE queue_h #-}
queue_h :: ByteString
queue_h = unsafePerformIO $ unsafePackAddress "\
 \/*\n\
 \ * Copyright (c) 1991, 1993\n\
 \ *\x0009\&The Regents of the University of California.  All rights reserved.\n\
 \ *\n\
 \ * Redistribution and use in source and binary forms, with or without\n\
 \ * modification, are permitted provided that the following conditions\n\
 \ * are met:\n\
 \ * 1. Redistributions of source code must retain the above copyright\n\
 \ *    notice, this list of conditions and the following disclaimer.\n\
 \ * 2. Redistributions in binary form must reproduce the above copyright\n\
 \ *    notice, this list of conditions and the following disclaimer in the\n\
 \ *    documentation and/or other materials provided with the distribution.\n\
 \ * 3. Neither the name of the University nor the names of its contributors\n\
 \ *    may be used to endorse or promote products derived from this software\n\
 \ *    without specific prior written permission.\n\
 \ *\n\
 \ * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND\n\
 \ * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE\n\
 \ * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE\n\
 \ * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE\n\
 \ * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL\n\
 \ * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS\n\
 \ * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)\n\
 \ * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT\n\
 \ * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY\n\
 \ * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF\n\
 \ * SUCH DAMAGE.\n\
 \ *\n\
 \ *\x0009\&@(#)queue.h\x0009\&8.5 (Berkeley) 8/20/94\n\
 \ */\n\
 \\n\
 \#ifndef\x0009\&_SYS_QUEUE_H_\n\
 \#define\x0009\&_SYS_QUEUE_H_\n\
 \\n\
 \/*\n\
 \ * This file defines five types of data structures: singly-linked lists,\n\
 \ * lists, simple queues, tail queues, and circular queues.\n\
 \ *\n\
 \ * A singly-linked list is headed by a single forward pointer. The\n\
 \ * elements are singly linked for minimum space and pointer manipulation\n\
 \ * overhead at the expense of O(n) removal for arbitrary elements. New\n\
 \ * elements can be added to the list after an existing element or at the\n\
 \ * head of the list.  Elements being removed from the head of the list\n\
 \ * should use the explicit macro for this purpose for optimum\n\
 \ * efficiency. A singly-linked list may only be traversed in the forward\n\
 \ * direction.  Singly-linked lists are ideal for applications with large\n\
 \ * datasets and few or no removals or for implementing a LIFO queue.\n\
 \ *\n\
 \ * A list is headed by a single forward pointer (or an array of forward\n\
 \ * pointers for a hash table header). The elements are doubly linked\n\
 \ * so that an arbitrary element can be removed without a need to\n\
 \ * traverse the list. New elements can be added to the list before\n\
 \ * or after an existing element or at the head of the list. A list\n\
 \ * may only be traversed in the forward direction.\n\
 \ *\n\
 \ * A simple queue is headed by a pair of pointers, one the head of the\n\
 \ * list and the other to the tail of the list. The elements are singly\n\
 \ * linked to save space, so elements can only be removed from the\n\
 \ * head of the list. New elements can be added to the list after\n\
 \ * an existing element, at the head of the list, or at the end of the\n\
 \ * list. A simple queue may only be traversed in the forward direction.\n\
 \ *\n\
 \ * A tail queue is headed by a pair of pointers, one to the head of the\n\
 \ * list and the other to the tail of the list. The elements are doubly\n\
 \ * linked so that an arbitrary element can be removed without a need to\n\
 \ * traverse the list. New elements can be added to the list before or\n\
 \ * after an existing element, at the head of the list, or at the end of\n\
 \ * the list. A tail queue may be traversed in either direction.\n\
 \ *\n\
 \ * A circle queue is headed by a pair of pointers, one to the head of the\n\
 \ * list and the other to the tail of the list. The elements are doubly\n\
 \ * linked so that an arbitrary element can be removed without a need to\n\
 \ * traverse the list. New elements can be added to the list before or after\n\
 \ * an existing element, at the head of the list, or at the end of the list.\n\
 \ * A circle queue may be traversed in either direction, but has a more\n\
 \ * complex end of list detection.\n\
 \ *\n\
 \ * For details on the use of these macros, see the queue(3) manual page.\n\
 \ */\n\
 \\n\
 \/*\n\
 \ * List definitions.\n\
 \ */\n\
 \#define\x0009\&LIST_HEAD(name, type)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \struct name {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&struct type *lh_first;\x0009\&/* first element */\x0009\&\x0009\&\x0009\&\\\n\
 \}\n\
 \\n\
 \#define\x0009\&LIST_HEAD_INITIALIZER(head)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&{ NULL }\n\
 \\n\
 \#define\x0009\&LIST_ENTRY(type)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \struct {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&struct type *le_next;\x0009\&/* next element */\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&struct type **le_prev;\x0009\&/* address of previous next element */\x0009\&\\\n\
 \}\n\
 \\n\
 \/*\n\
 \ * List functions.\n\
 \ */\n\
 \#define\x0009\&LIST_INIT(head) do {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(head)->lh_first = NULL;\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&LIST_INSERT_AFTER(listelm, elm, field) do {\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&if (((elm)->field.le_next = (listelm)->field.le_next) != NULL)\x0009\&\\\n\
 \\x0009\&\x0009\&(listelm)->field.le_next->field.le_prev =\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&    &(elm)->field.le_next;\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(listelm)->field.le_next = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.le_prev = &(listelm)->field.le_next;\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&LIST_INSERT_BEFORE(listelm, elm, field) do {\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.le_prev = (listelm)->field.le_prev;\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.le_next = (listelm);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&*(listelm)->field.le_prev = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(listelm)->field.le_prev = &(elm)->field.le_next;\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&LIST_INSERT_HEAD(head, elm, field) do {\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&if (((elm)->field.le_next = (head)->lh_first) != NULL)\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(head)->lh_first->field.le_prev = &(elm)->field.le_next;\\\n\
 \\x0009\&(head)->lh_first = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.le_prev = &(head)->lh_first;\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&LIST_REMOVE(elm, field) do {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&if ((elm)->field.le_next != NULL)\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(elm)->field.le_next->field.le_prev = \x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&    (elm)->field.le_prev;\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&*(elm)->field.le_prev = (elm)->field.le_next;\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&LIST_FOREACH(var, head, field)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&for ((var) = ((head)->lh_first);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(var);\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(var) = ((var)->field.le_next))\n\
 \\n\
 \/*\n\
 \ * List access methods.\n\
 \ */\n\
 \#define\x0009\&LIST_EMPTY(head)\x0009\&\x0009\&((head)->lh_first == NULL)\n\
 \#define\x0009\&LIST_FIRST(head)\x0009\&\x0009\&((head)->lh_first)\n\
 \#define\x0009\&LIST_NEXT(elm, field)\x0009\&\x0009\&((elm)->field.le_next)\n\
 \\n\
 \/*\n\
 \ * Singly-linked List definitions.\n\
 \ */\n\
 \#define\x0009\&SLIST_HEAD(name, type)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \struct name {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&struct type *slh_first;\x0009\&/* first element */\x0009\&\x0009\&\x0009\&\\\n\
 \}\n\
 \\n\
 \#define\x0009\&SLIST_HEAD_INITIALIZER(head)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&{ NULL }\n\
 \\n\
 \#define\x0009\&SLIST_ENTRY(type)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \struct {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&struct type *sle_next;\x0009\&/* next element */\x0009\&\x0009\&\x0009\&\\\n\
 \}\n\
 \\n\
 \/*\n\
 \ * Singly-linked List functions.\n\
 \ */\n\
 \#define\x0009\&SLIST_INIT(head) do {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(head)->slh_first = NULL;\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&SLIST_INSERT_AFTER(slistelm, elm, field) do {\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.sle_next = (slistelm)->field.sle_next;\x0009\&\x0009\&\\\n\
 \\x0009\&(slistelm)->field.sle_next = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&SLIST_INSERT_HEAD(head, elm, field) do {\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.sle_next = (head)->slh_first;\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(head)->slh_first = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&SLIST_REMOVE_HEAD(head, field) do {\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(head)->slh_first = (head)->slh_first->field.sle_next;\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&SLIST_REMOVE(head, elm, type, field) do {\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&if ((head)->slh_first == (elm)) {\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&SLIST_REMOVE_HEAD((head), field);\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&}\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&else {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&struct type *curelm = (head)->slh_first;\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&while(curelm->field.sle_next != (elm))\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&\x0009\&curelm = curelm->field.sle_next;\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&curelm->field.sle_next =\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&    curelm->field.sle_next->field.sle_next;\x0009\&\x0009\&\\\n\
 \\x0009\&}\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&SLIST_FOREACH(var, head, field)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&for((var) = (head)->slh_first; (var); (var) = (var)->field.sle_next)\n\
 \\n\
 \/*\n\
 \ * Singly-linked List access methods.\n\
 \ */\n\
 \#define\x0009\&SLIST_EMPTY(head)\x0009\&((head)->slh_first == NULL)\n\
 \#define\x0009\&SLIST_FIRST(head)\x0009\&((head)->slh_first)\n\
 \#define\x0009\&SLIST_NEXT(elm, field)\x0009\&((elm)->field.sle_next)\n\
 \\n\
 \/*\n\
 \ * Singly-linked Tail queue declarations.\n\
 \ */\n\
 \#define\x0009\&STAILQ_HEAD(name, type)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \struct name {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&struct type *stqh_first;\x0009\&/* first element */\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&struct type **stqh_last;\x0009\&/* addr of last next element */\x0009\&\x0009\&\\\n\
 \}\n\
 \\n\
 \#define\x0009\&STAILQ_HEAD_INITIALIZER(head)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&{ NULL, &(head).stqh_first }\n\
 \\n\
 \#define\x0009\&STAILQ_ENTRY(type)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \struct {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&struct type *stqe_next;\x0009\&/* next element */\x0009\&\x0009\&\x0009\&\\\n\
 \}\n\
 \\n\
 \/*\n\
 \ * Singly-linked Tail queue functions.\n\
 \ */\n\
 \#define\x0009\&STAILQ_INIT(head) do {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(head)->stqh_first = NULL;\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(head)->stqh_last = &(head)->stqh_first;\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&STAILQ_INSERT_HEAD(head, elm, field) do {\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&if (((elm)->field.stqe_next = (head)->stqh_first) == NULL)\x0009\&\\\n\
 \\x0009\&\x0009\&(head)->stqh_last = &(elm)->field.stqe_next;\x0009\&\x0009\&\\\n\
 \\x0009\&(head)->stqh_first = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&STAILQ_INSERT_TAIL(head, elm, field) do {\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.stqe_next = NULL;\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&*(head)->stqh_last = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(head)->stqh_last = &(elm)->field.stqe_next;\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&STAILQ_INSERT_AFTER(head, listelm, elm, field) do {\x0009\&\x0009\&\\\n\
 \\x0009\&if (((elm)->field.stqe_next = (listelm)->field.stqe_next) == NULL)\\\n\
 \\x0009\&\x0009\&(head)->stqh_last = &(elm)->field.stqe_next;\x0009\&\x0009\&\\\n\
 \\x0009\&(listelm)->field.stqe_next = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&STAILQ_REMOVE_HEAD(head, field) do {\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&if (((head)->stqh_first = (head)->stqh_first->field.stqe_next) == NULL) \\\n\
 \\x0009\&\x0009\&(head)->stqh_last = &(head)->stqh_first;\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&STAILQ_REMOVE(head, elm, type, field) do {\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&if ((head)->stqh_first == (elm)) {\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&STAILQ_REMOVE_HEAD((head), field);\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&} else {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&struct type *curelm = (head)->stqh_first;\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&while (curelm->field.stqe_next != (elm))\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&\x0009\&curelm = curelm->field.stqe_next;\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&if ((curelm->field.stqe_next =\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&\x0009\&curelm->field.stqe_next->field.stqe_next) == NULL) \\\n\
 \\x0009\&\x0009\&\x0009\&    (head)->stqh_last = &(curelm)->field.stqe_next; \\\n\
 \\x0009\&}\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&STAILQ_FOREACH(var, head, field)\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&for ((var) = ((head)->stqh_first);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(var);\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(var) = ((var)->field.stqe_next))\n\
 \\n\
 \#define\x0009\&STAILQ_CONCAT(head1, head2) do {\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&if (!STAILQ_EMPTY((head2))) {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&*(head1)->stqh_last = (head2)->stqh_first;\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(head1)->stqh_last = (head2)->stqh_last;\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&STAILQ_INIT((head2));\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&}\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \/*\n\
 \ * Singly-linked Tail queue access methods.\n\
 \ */\n\
 \#define\x0009\&STAILQ_EMPTY(head)\x0009\&((head)->stqh_first == NULL)\n\
 \#define\x0009\&STAILQ_FIRST(head)\x0009\&((head)->stqh_first)\n\
 \#define\x0009\&STAILQ_NEXT(elm, field)\x0009\&((elm)->field.stqe_next)\n\
 \\n\
 \/*\n\
 \ * Simple queue definitions.\n\
 \ */\n\
 \#define\x0009\&SIMPLEQ_HEAD(name, type)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \struct name {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&struct type *sqh_first;\x0009\&/* first element */\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&struct type **sqh_last;\x0009\&/* addr of last next element */\x0009\&\x0009\&\\\n\
 \}\n\
 \\n\
 \#define\x0009\&SIMPLEQ_HEAD_INITIALIZER(head)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&{ NULL, &(head).sqh_first }\n\
 \\n\
 \#define\x0009\&SIMPLEQ_ENTRY(type)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \struct {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&struct type *sqe_next;\x0009\&/* next element */\x0009\&\x0009\&\x0009\&\\\n\
 \}\n\
 \\n\
 \/*\n\
 \ * Simple queue functions.\n\
 \ */\n\
 \#define\x0009\&SIMPLEQ_INIT(head) do {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(head)->sqh_first = NULL;\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(head)->sqh_last = &(head)->sqh_first;\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&SIMPLEQ_INSERT_HEAD(head, elm, field) do {\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&if (((elm)->field.sqe_next = (head)->sqh_first) == NULL)\x0009\&\\\n\
 \\x0009\&\x0009\&(head)->sqh_last = &(elm)->field.sqe_next;\x0009\&\x0009\&\\\n\
 \\x0009\&(head)->sqh_first = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&SIMPLEQ_INSERT_TAIL(head, elm, field) do {\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.sqe_next = NULL;\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&*(head)->sqh_last = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(head)->sqh_last = &(elm)->field.sqe_next;\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&SIMPLEQ_INSERT_AFTER(head, listelm, elm, field) do {\x0009\&\x0009\&\\\n\
 \\x0009\&if (((elm)->field.sqe_next = (listelm)->field.sqe_next) == NULL)\\\n\
 \\x0009\&\x0009\&(head)->sqh_last = &(elm)->field.sqe_next;\x0009\&\x0009\&\\\n\
 \\x0009\&(listelm)->field.sqe_next = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&SIMPLEQ_REMOVE_HEAD(head, field) do {\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&if (((head)->sqh_first = (head)->sqh_first->field.sqe_next) == NULL) \\\n\
 \\x0009\&\x0009\&(head)->sqh_last = &(head)->sqh_first;\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&SIMPLEQ_REMOVE(head, elm, type, field) do {\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&if ((head)->sqh_first == (elm)) {\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&SIMPLEQ_REMOVE_HEAD((head), field);\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&} else {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&struct type *curelm = (head)->sqh_first;\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&while (curelm->field.sqe_next != (elm))\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&\x0009\&curelm = curelm->field.sqe_next;\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&if ((curelm->field.sqe_next =\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&\x0009\&curelm->field.sqe_next->field.sqe_next) == NULL) \\\n\
 \\x0009\&\x0009\&\x0009\&    (head)->sqh_last = &(curelm)->field.sqe_next; \\\n\
 \\x0009\&}\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&SIMPLEQ_FOREACH(var, head, field)\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&for ((var) = ((head)->sqh_first);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(var);\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(var) = ((var)->field.sqe_next))\n\
 \\n\
 \/*\n\
 \ * Simple queue access methods.\n\
 \ */\n\
 \#define\x0009\&SIMPLEQ_EMPTY(head)\x0009\&\x0009\&((head)->sqh_first == NULL)\n\
 \#define\x0009\&SIMPLEQ_FIRST(head)\x0009\&\x0009\&((head)->sqh_first)\n\
 \#define\x0009\&SIMPLEQ_NEXT(elm, field)\x0009\&((elm)->field.sqe_next)\n\
 \\n\
 \/*\n\
 \ * Tail queue definitions.\n\
 \ */\n\
 \#define\x0009\&_TAILQ_HEAD(name, type, qual)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \struct name {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&qual type *tqh_first;\x0009\&\x0009\&/* first element */\x0009\&\x0009\&\\\n\
 \\x0009\&qual type *qual *tqh_last;\x0009\&/* addr of last next element */\x0009\&\\\n\
 \}\n\
 \#define TAILQ_HEAD(name, type)\x0009\&_TAILQ_HEAD(name, struct type,)\n\
 \\n\
 \#define\x0009\&TAILQ_HEAD_INITIALIZER(head)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&{ NULL, &(head).tqh_first }\n\
 \\n\
 \#define\x0009\&_TAILQ_ENTRY(type, qual)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \struct {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&qual type *tqe_next;\x0009\&\x0009\&/* next element */\x0009\&\x0009\&\\\n\
 \\x0009\&qual type *qual *tqe_prev;\x0009\&/* address of previous next element */\\\n\
 \}\n\
 \#define TAILQ_ENTRY(type)\x0009\&_TAILQ_ENTRY(struct type,)\n\
 \\n\
 \/*\n\
 \ * Tail queue functions.\n\
 \ */\n\
 \#define\x0009\&TAILQ_INIT(head) do {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(head)->tqh_first = NULL;\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(head)->tqh_last = &(head)->tqh_first;\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&TAILQ_INSERT_HEAD(head, elm, field) do {\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&if (((elm)->field.tqe_next = (head)->tqh_first) != NULL)\x0009\&\\\n\
 \\x0009\&\x0009\&(head)->tqh_first->field.tqe_prev =\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&    &(elm)->field.tqe_next;\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&else\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(head)->tqh_last = &(elm)->field.tqe_next;\x0009\&\x0009\&\\\n\
 \\x0009\&(head)->tqh_first = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.tqe_prev = &(head)->tqh_first;\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&TAILQ_INSERT_TAIL(head, elm, field) do {\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.tqe_next = NULL;\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.tqe_prev = (head)->tqh_last;\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&*(head)->tqh_last = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(head)->tqh_last = &(elm)->field.tqe_next;\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&TAILQ_INSERT_AFTER(head, listelm, elm, field) do {\x0009\&\x0009\&\\\n\
 \\x0009\&if (((elm)->field.tqe_next = (listelm)->field.tqe_next) != NULL)\\\n\
 \\x0009\&\x0009\&(elm)->field.tqe_next->field.tqe_prev = \x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&    &(elm)->field.tqe_next;\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&else\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(head)->tqh_last = &(elm)->field.tqe_next;\x0009\&\x0009\&\\\n\
 \\x0009\&(listelm)->field.tqe_next = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.tqe_prev = &(listelm)->field.tqe_next;\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&TAILQ_INSERT_BEFORE(listelm, elm, field) do {\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.tqe_prev = (listelm)->field.tqe_prev;\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.tqe_next = (listelm);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&*(listelm)->field.tqe_prev = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(listelm)->field.tqe_prev = &(elm)->field.tqe_next;\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&TAILQ_REMOVE(head, elm, field) do {\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&if (((elm)->field.tqe_next) != NULL)\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(elm)->field.tqe_next->field.tqe_prev = \x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&    (elm)->field.tqe_prev;\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&else\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(head)->tqh_last = (elm)->field.tqe_prev;\x0009\&\x0009\&\\\n\
 \\x0009\&*(elm)->field.tqe_prev = (elm)->field.tqe_next;\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&TAILQ_FOREACH(var, head, field)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&for ((var) = ((head)->tqh_first);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(var);\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(var) = ((var)->field.tqe_next))\n\
 \\n\
 \#define\x0009\&TAILQ_FOREACH_REVERSE(var, head, headname, field)\x0009\&\x0009\&\\\n\
 \\x0009\&for ((var) = (*(((struct headname *)((head)->tqh_last))->tqh_last));\x0009\&\\\n\
 \\x0009\&\x0009\&(var);\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(var) = (*(((struct headname *)((var)->field.tqe_prev))->tqh_last)))\n\
 \\n\
 \#define\x0009\&TAILQ_CONCAT(head1, head2, field) do {\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&if (!TAILQ_EMPTY(head2)) {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&*(head1)->tqh_last = (head2)->tqh_first;\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(head2)->tqh_first->field.tqe_prev = (head1)->tqh_last;\x0009\&\\\n\
 \\x0009\&\x0009\&(head1)->tqh_last = (head2)->tqh_last;\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&TAILQ_INIT((head2));\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&}\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \/*\n\
 \ * Tail queue access methods.\n\
 \ */\n\
 \#define\x0009\&TAILQ_EMPTY(head)\x0009\&\x0009\&((head)->tqh_first == NULL)\n\
 \#define\x0009\&TAILQ_FIRST(head)\x0009\&\x0009\&((head)->tqh_first)\n\
 \#define\x0009\&TAILQ_NEXT(elm, field)\x0009\&\x0009\&((elm)->field.tqe_next)\n\
 \\n\
 \#define\x0009\&TAILQ_LAST(head, headname) \\\n\
 \\x0009\&(*(((struct headname *)((head)->tqh_last))->tqh_last))\n\
 \#define\x0009\&TAILQ_PREV(elm, headname, field) \\\n\
 \\x0009\&(*(((struct headname *)((elm)->field.tqe_prev))->tqh_last))\n\
 \\n\
 \/*\n\
 \ * Circular queue definitions.\n\
 \ */\n\
 \#define\x0009\&CIRCLEQ_HEAD(name, type)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \struct name {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&struct type *cqh_first;\x0009\&\x0009\&/* first element */\x0009\&\x0009\&\\\n\
 \\x0009\&struct type *cqh_last;\x0009\&\x0009\&/* last element */\x0009\&\x0009\&\\\n\
 \}\n\
 \\n\
 \#define\x0009\&CIRCLEQ_HEAD_INITIALIZER(head)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&{ (void *)&head, (void *)&head }\n\
 \\n\
 \#define\x0009\&CIRCLEQ_ENTRY(type)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \struct {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&struct type *cqe_next;\x0009\&\x0009\&/* next element */\x0009\&\x0009\&\\\n\
 \\x0009\&struct type *cqe_prev;\x0009\&\x0009\&/* previous element */\x0009\&\x0009\&\\\n\
 \}\n\
 \\n\
 \/*\n\
 \ * Circular queue functions.\n\
 \ */\n\
 \#define\x0009\&CIRCLEQ_INIT(head) do {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(head)->cqh_first = (void *)(head);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(head)->cqh_last = (void *)(head);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&CIRCLEQ_INSERT_AFTER(head, listelm, elm, field) do {\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.cqe_next = (listelm)->field.cqe_next;\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.cqe_prev = (listelm);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&if ((listelm)->field.cqe_next == (void *)(head))\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(head)->cqh_last = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&else\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(listelm)->field.cqe_next->field.cqe_prev = (elm);\x0009\&\\\n\
 \\x0009\&(listelm)->field.cqe_next = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&CIRCLEQ_INSERT_BEFORE(head, listelm, elm, field) do {\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.cqe_next = (listelm);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.cqe_prev = (listelm)->field.cqe_prev;\x0009\&\x0009\&\\\n\
 \\x0009\&if ((listelm)->field.cqe_prev == (void *)(head))\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(head)->cqh_first = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&else\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(listelm)->field.cqe_prev->field.cqe_next = (elm);\x0009\&\\\n\
 \\x0009\&(listelm)->field.cqe_prev = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&CIRCLEQ_INSERT_HEAD(head, elm, field) do {\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.cqe_next = (head)->cqh_first;\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.cqe_prev = (void *)(head);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&if ((head)->cqh_last == (void *)(head))\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(head)->cqh_last = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&else\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(head)->cqh_first->field.cqe_prev = (elm);\x0009\&\x0009\&\\\n\
 \\x0009\&(head)->cqh_first = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&CIRCLEQ_INSERT_TAIL(head, elm, field) do {\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.cqe_next = (void *)(head);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.cqe_prev = (head)->cqh_last;\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&if ((head)->cqh_first == (void *)(head))\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(head)->cqh_first = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&else\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(head)->cqh_last->field.cqe_next = (elm);\x0009\&\x0009\&\\\n\
 \\x0009\&(head)->cqh_last = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&CIRCLEQ_REMOVE(head, elm, field) do {\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&if ((elm)->field.cqe_next == (void *)(head))\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(head)->cqh_last = (elm)->field.cqe_prev;\x0009\&\x0009\&\\\n\
 \\x0009\&else\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(elm)->field.cqe_next->field.cqe_prev =\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&    (elm)->field.cqe_prev;\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&if ((elm)->field.cqe_prev == (void *)(head))\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(head)->cqh_first = (elm)->field.cqe_next;\x0009\&\x0009\&\\\n\
 \\x0009\&else\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(elm)->field.cqe_prev->field.cqe_next =\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&    (elm)->field.cqe_next;\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&CIRCLEQ_FOREACH(var, head, field)\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&for ((var) = ((head)->cqh_first);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(var) != (const void *)(head);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(var) = ((var)->field.cqe_next))\n\
 \\n\
 \#define\x0009\&CIRCLEQ_FOREACH_REVERSE(var, head, field)\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&for ((var) = ((head)->cqh_last);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(var) != (const void *)(head);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(var) = ((var)->field.cqe_prev))\n\
 \\n\
 \/*\n\
 \ * Circular queue access methods.\n\
 \ */\n\
 \#define\x0009\&CIRCLEQ_EMPTY(head)\x0009\&\x0009\&((head)->cqh_first == (void *)(head))\n\
 \#define\x0009\&CIRCLEQ_FIRST(head)\x0009\&\x0009\&((head)->cqh_first)\n\
 \#define\x0009\&CIRCLEQ_LAST(head)\x0009\&\x0009\&((head)->cqh_last)\n\
 \#define\x0009\&CIRCLEQ_NEXT(elm, field)\x0009\&((elm)->field.cqe_next)\n\
 \#define\x0009\&CIRCLEQ_PREV(elm, field)\x0009\&((elm)->field.cqe_prev)\n\
 \\n\
 \#define CIRCLEQ_LOOP_NEXT(head, elm, field)\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(((elm)->field.cqe_next == (void *)(head))\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&    ? ((head)->cqh_first)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&    : (elm->field.cqe_next))\n\
 \#define CIRCLEQ_LOOP_PREV(head, elm, field)\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(((elm)->field.cqe_prev == (void *)(head))\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&    ? ((head)->cqh_last)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&    : (elm->field.cqe_prev))\n\
 \\n\
 \#endif\x0009\&/* sys/queue.h */\n\
 \"#

-- | Generated from rts\/HsFFI.h
{-# NOINLINE hsffi_h #-}
hsffi_h :: ByteString
hsffi_h = unsafePerformIO $ unsafePackAddress "\
 \/* HsFFI.h for jhc */\n\
 \\n\
 \#ifndef _JHC_HSFFI_H\n\
 \#define _JHC_HSFFI_H\n\
 \\n\
 \#include <stdint.h>\n\
 \#include <stdbool.h>\n\
 \#include <stddef.h>\n\
 \\n\
 \typedef int32_t HsInt;\n\
 \typedef int8_t  HsInt8;\n\
 \typedef int16_t HsInt16;\n\
 \typedef int32_t HsInt32;\n\
 \typedef int64_t HsInt64;\n\
 \\n\
 \typedef uint32_t HsWord;\n\
 \typedef uint8_t  HsWord8;\n\
 \typedef uint16_t HsWord16;\n\
 \typedef uint32_t HsWord32;\n\
 \typedef uint64_t HsWord64;\n\
 \\n\
 \typedef wchar_t HsChar;\n\
 \typedef bool HsBool;\n\
 \\n\
 \typedef double HsDouble;\n\
 \typedef float HsFloat;\n\
 \\n\
 \typedef void *HsPtr;\n\
 \typedef void (*HsFunPtr)(void);\n\
 \typedef void *HsStablePtr;\n\
 \\n\
 \#define HS_BOOL_FALSE 0\n\
 \#define HS_BOOL_TRUE 1\n\
 \\n\
 \void hs_init (int *argc, char **argv[]);\n\
 \void hs_exit (void);\n\
 \void hs_set_argv(int argc, char *argv[]);\n\
 \void hs_perform_gc(void);\n\
 \void hs_free_stable_ptr(HsStablePtr sp);\n\
 \void hs_free_fun_ptr(HsFunPtr fp);\n\
 \\n\
 \#endif\n\
 \"#

-- | Generated from rts\/sys\/wsize.h
{-# NOINLINE wsize_h #-}
wsize_h :: ByteString
wsize_h = unsafePerformIO $ unsafePackAddress "\
 \#ifndef WSIZE_H\n\
 \#define WSIZE_H\n\
 \\n\
 \/*\n\
 \ * wsize.h\n\
 \ * define appropriate __WORDSIZE and __BYTE_ORDER macros\n\
 \ *\n\
 \ * always use operating systems headers rather than checking for architectures\n\
 \ * when possible. if adding new cases. Checking the CPU type should be a last\n\
 \ * resort.\n\
 \ *\n\
 \ */\n\
 \\n\
 \#include <limits.h>\n\
 \\n\
 \#ifdef __linux__\n\
 \#include<endian.h>\n\
 \#endif\n\
 \\n\
 \#ifndef __LITTLE_ENDIAN\n\
 \#define\x0009\&__LITTLE_ENDIAN\x0009\&1234\n\
 \#endif\n\
 \#ifndef __BIG_ENDIAN\n\
 \#define\x0009\&__BIG_ENDIAN\x0009\&4321\n\
 \#endif\n\
 \#ifndef __PDP_ENDIAN\n\
 \#define\x0009\&__PDP_ENDIAN\x0009\&3412\n\
 \#endif\n\
 \\n\
 \#ifndef __BYTE_ORDER\n\
 \#ifdef _BIG_ENDIAN\n\
 \#define __BYTE_ORDER __BIG_ENDIAN\n\
 \#elif defined(__BIG_ENDIAN__)\n\
 \#define __BYTE_ORDER __BIG_ENDIAN\n\
 \#elif defined(_LITTLE_ENDIAN)\n\
 \#define __BYTE_ORDER __LITTLE_ENDIAN\n\
 \#elif defined(__LITTLE_ENDIAN__)\n\
 \#define __BYTE_ORDER __LITTLE_ENDIAN\n\
 \#elif defined(__i386__)\n\
 \#define __BYTE_ORDER __LITTLE_ENDIAN\n\
 \#else\n\
 \#error Could not determine Byte Order\n\
 \#endif\n\
 \#endif\n\
 \\n\
 \#ifndef __WORDSIZE\n\
 \#ifdef __SIZEOF_POINTER__\n\
 \#define __WORDSIZE (CHAR_BIT*__SIZEOF_POINTER__)\n\
 \#elif defined(__i386__)\n\
 \#define __WORDSIZE 32\n\
 \#elif defined(__x86_64__)\n\
 \#define __WORDSIZE 64\n\
 \#else\n\
 \#error Could not determine bitsize\n\
 \#endif\n\
 \#endif\n\
 \\n\
 \#ifdef TEST_WSIZE\n\
 \#include <stdio.h>\n\
 \int\n\
 \main(int argc, char *argv[])\n\
 \{\n\
 \    printf(\"__WORDSIZE:   %i\\n\", __WORDSIZE);\n\
 \    printf(\"__BYTE_ORDER: %i\\n\", __BYTE_ORDER);\n\
 \    return 0;\n\
 \}\n\
 \#endif\n\
 \\n\
 \#endif\n\
 \"#

-- | Generated from rts\/sys\/bitarray.h
{-# NOINLINE bitarray_h #-}
bitarray_h :: ByteString
bitarray_h = unsafePerformIO $ unsafePackAddress "\
 \#ifndef BITARRAY_H\n\
 \#define BITARRAY_H\n\
 \\n\
 \#include <limits.h>\n\
 \#include <stdbool.h>\n\
 \\n\
 \typedef unsigned long bitarray_t;\n\
 \\n\
 \#define BITS_PER_UNIT (bitarray_t)(CHAR_BIT*sizeof(bitarray_t))\n\
 \#define BITARRAY_SIZE(bits) (((bits) + (BITS_PER_UNIT - 1)) / BITS_PER_UNIT)\n\
 \#define BITARRAY_SIZE_IN_BYTES(bits) (sizeof(bitarray_t)*BITARRAY_SIZE(bits))\n\
 \\n\
 \#define WHICH_BIT(bit)  \\\n\
 \    (1UL << ((((bitarray_t)(bit)) % BITS_PER_UNIT)))\n\
 \\n\
 \#define OFFSET_IN_ARRAY(array,bit) \\\n\
 \    (((bitarray_t *)(array))[((bitarray_t)(bit)) / BITS_PER_UNIT])\n\
 \\n\
 \#define BIT_IS_SET(array,bit)  \\\n\
 \    (OFFSET_IN_ARRAY(array,bit) & WHICH_BIT(bit))\n\
 \\n\
 \#define BIT_IS_UNSET(array,bit) \\\n\
 \    (!(BIT_IS_SET(array,bit)))\n\
 \\n\
 \#define BIT_SET(array,bit) \\\n\
 \    (OFFSET_IN_ARRAY(array,bit) |= WHICH_BIT(bit))\n\
 \\n\
 \#define BIT_UNSET(array,bit) \\\n\
 \    (OFFSET_IN_ARRAY(array,bit) &= ~WHICH_BIT(bit))\n\
 \\n\
 \#define BIT_TOGGLE(array,bit) \\\n\
 \    (OFFSET_IN_ARRAY(array,bit) ^= WHICH_BIT(bit))\n\
 \\n\
 \#define BIT_COPY(dest,src,bit)  \\\n\
 \    do { BIT_IS_SET((src),(bit)) ? BIT_SET((dest),(bit)) : BIT_UNSET((dest),(bit)) } while(0)\n\
 \\n\
 \#define BIT_VALUE(array,bit) \\\n\
 \    (BIT_IS_SET((array),(bit)) ? true : false)\n\
 \\n\
 \#define BIT_SET_VALUE(array,bit,value) \\\n\
 \    do { (value) ? BIT_SET((array),(bit)) : BIT_UNSET((array),(bit)) } while(0)\n\
 \\n\
 \#endif\n\
 \"#

-- | Generated from ChangeLog
{-# NOINLINE changelog #-}
changelog :: ByteString
changelog = unsafePerformIO $ unsafePackAddress "\
 \Tue Feb  7 03:53:23 PST 2012  John Meacham <john@repetae.net>\n\
 \  * clean up handling of raw files, keep source file separate when compiling. include useful header files when compiling.\n\
 \\n\
 \Tue Feb  7 02:55:10 PST 2012  John Meacham <john@repetae.net>\n\
 \  * make foreign imports work with unboxed IO types, improve primitive operators.\n\
 \\n\
 \Mon Feb  6 22:32:02 PST 2012  John Meacham <john@repetae.net>\n\
 \  * add support for 'Coerce' grin primitive, add atomic allocs in grin.\n\
 \\n\
 \Mon Feb  6 17:57:19 PST 2012  John Meacham <john@repetae.net>\n\
 \  * add Jhc.Prim.Rts to access properties of the RTS\n\
 \\n\
 \Mon Feb  6 10:11:22 PST 2012  John Meacham <john@repetae.net>\n\
 \  * clean up foreignptr implementation some.\n\
 \\n\
 \Mon Feb  6 07:30:22 PST 2012  John Meacham <john@repetae.net>\n\
 \  * allow extra C code to be bundled within .hl files, include bytestring in the standard external libraries.\n\
 \\n\
 \Mon Feb  6 06:03:18 PST 2012  John Meacham <john@repetae.net>\n\
 \  * pass -D flags to hsc2hs, add CLong and CULong instances, fill out Foreign.ForeignPtr\n\
 \\n\
 \Mon Feb  6 04:24:44 PST 2012  John Meacham <john@repetae.net>\n\
 \  * remove old references to the haskell object directory, made everything cache based. added --purge-cache\n\
 \\n\
 \Mon Feb  6 03:22:32 PST 2012  John Meacham <john@repetae.net>\n\
 \  * update documentation, move Data.Char to base\n\
 \\n\
 \Mon Feb  6 01:52:09 PST 2012  John Meacham <john@repetae.net>\n\
 \  * remove redundant dependencies from 'base'\n\
 \\n\
 \Mon Feb  6 01:27:30 PST 2012  John Meacham <john@repetae.net>\n\
 \  * include location of 'import' declaration for unknown imports, move Data.Int and Data.Word out of jhc\n\
 \\n\
 \Mon Feb  6 00:13:57 PST 2012  John Meacham <john@repetae.net>\n\
 \  * move arrays to jhc-prim, remove special IORef specific primitives in favor of length 1 arrays\n\
 \\n\
 \Sun Feb  5 22:56:36 PST 2012  John Meacham <john@repetae.net>\n\
 \  * remove a lot of unneeded dependencies.\n\
 \\n\
 \Sun Feb  5 22:13:14 PST 2012  John Meacham <john@repetae.net>\n\
 \  * create Jhc.Class.Real to hold more advance numeric classes.\n\
 \\n\
 \Sun Feb  5 22:13:04 PST 2012  John Meacham <john@repetae.net>\n\
 \  * add script to check for and remove unneeded imports from libraries, remove a bunch from lib/jhc/\n\
 \\n\
 \Sun Feb  5 22:11:51 PST 2012  John Meacham <john@repetae.net>\n\
 \  * don't require 'error' to be in scope for where pattern desugaring, process renaming errors before kind inference\n\
 \\n\
 \Sun Feb  5 18:28:10 PST 2012  John Meacham <john@repetae.net>\n\
 \  * seperate --stop parameter from compilation mode\n\
 \\n\
 \Sun Feb  5 16:57:46 PST 2012  John Meacham <john@repetae.net>\n\
 \  * use quoteName mechanism to pre-rename certain names, fix class instance namespace bug\n\
 \\n\
 \Sun Feb  5 03:08:57 PST 2012  John Meacham <john@repetae.net>\n\
 \  * generate reverse name map during renaming\n\
 \\n\
 \Sun Feb  5 00:21:56 PST 2012  John Meacham <john@repetae.net>\n\
 \  * fix up library to pass the more strigent typechecking\n\
 \\n\
 \Sat Feb  4 18:58:15 PST 2012  John Meacham <john@repetae.net>\n\
 \  * check superclass constraints on instance declarations\n\
 \\n\
 \Sat Feb  4 17:56:51 PST 2012  John Meacham <john@repetae.net>\n\
 \  * properly check for duplicate instances\n\
 \\n\
 \Sat Feb  4 16:30:34 PST 2012  John Meacham <john@repetae.net>\n\
 \  * add ghc typecheck failure tests\n\
 \\n\
 \Sat Feb  4 16:02:03 PST 2012  John Meacham <john@repetae.net>\n\
 \  * improve kind checking, kind check instance heads before typechecking pass, switch HsInstDecl to use the HsClassHead\n\
 \\n\
 \Sat Feb  4 14:07:32 PST 2012  John Meacham <john@repetae.net>\n\
 \  * switch kind inference to use the unified warnings mechanism, switch class declarations to use a HsClassHead \n\
 \\n\
 \Thu Feb  2 20:17:57 PST 2012  John Meacham <john@repetae.net>\n\
 \  * turn WarnType into a real algebraic data type.\n\
 \\n\
 \Wed Feb  1 05:20:53 PST 2012  John Meacham <john@repetae.net>\n\
 \  * code cleanups, used located in hstype, use packedstring for filename.\n\
 \\n\
 \Wed Feb  1 03:02:17 PST 2012  John Meacham <john@repetae.net>\n\
 \  * add bange-patterns extension, add individual flags for various extensions\n\
 \\n\
 \Tue Jan 31 23:34:00 PST 2012  John Meacham <john@repetae.net>\n\
 \  * Rework the internal class representation to separate instances from class declarations\n\
 \\n\
 \Tue Jan 31 17:59:18 PST 2012  John Meacham <john@repetae.net>\n\
 \  * add DeNameable class to un-rename source before printing error messages.\n\
 \\n\
 \Tue Jan 31 13:49:51 PST 2012  John Meacham <john@repetae.net>\n\
 \  * make IO a newtype of ST\n\
 \\n\
 \Mon Jan 30 03:22:30 PST 2012  John Meacham <john@repetae.net>\n\
 \  * speed up parsing/lexing by using Name's rather than Strings\n\
 \\n\
 \Mon Jan 30 01:56:03 PST 2012  John Meacham <john@repetae.net>\n\
 \  tagged mydniquipepo\n\
 \"#

-- | Generated from src\/data\/shortchange.txt
{-# NOINLINE shortchange_txt #-}
shortchange_txt :: ByteString
shortchange_txt = unsafePerformIO $ unsafePackAddress "\
 \mydniquipepo-32\
 \"#

-- | Generated from rts\/rts\/gc_jgc.c
{-# NOINLINE gc_jgc_c #-}
gc_jgc_c :: ByteString
gc_jgc_c = unsafePerformIO $ unsafePackAddress "\
 \#include \"jhc_rts_header.h\"\n\
 \#include \"rts/cdefs.h\"\n\
 \#include \"rts/constants.h\"\n\
 \#include \"rts/gc_jgc_internal.h\"\n\
 \#include \"sys/bitarray.h\"\n\
 \#include \"sys/queue.h\"\n\
 \\n\
 \#if _JHC_GC == _JHC_GC_JGC\n\
 \\n\
 \#ifdef _JHC_JGC_FIXED_MEGABLOCK\n\
 \static char aligned_megablock_1[MEGABLOCK_SIZE] __attribute__((aligned(BLOCK_SIZE)));\n\
 \static char gc_stack_base_area[(1UL << 8)*sizeof(gc_t)];\n\
 \#endif\n\
 \gc_t saved_gc;\n\
 \struct s_arena *arena;\n\
 \static gc_t gc_stack_base;\n\
 \\n\
 \#define TO_GCPTR(x) (entry_t *)(FROM_SPTR(x))\n\
 \\n\
 \void gc_perform_gc(gc_t gc) A_STD;\n\
 \static bool s_set_used_bit(void *val) A_UNUSED;\n\
 \static void clear_used_bits(struct s_arena *arena) A_UNUSED;\n\
 \static void s_cleanup_blocks(struct s_arena *arena);\n\
 \static struct s_block *get_free_block(gc_t gc, struct s_arena *arena, bool retry);\n\
 \static void *jhc_aligned_alloc(unsigned size);\n\
 \\n\
 \typedef struct {\n\
 \        sptr_t ptrs[0];\n\
 \} entry_t;\n\
 \\n\
 \static const void *nh_start, *nh_end;\n\
 \\n\
 \static bool\n\
 \gc_check_heap(entry_t *s)\n\
 \{\n\
 \        return (s < (entry_t *)nh_start || s > (entry_t *)nh_end);\n\
 \}\n\
 \\n\
 \struct stack {\n\
 \        unsigned size;\n\
 \        unsigned ptr;\n\
 \        entry_t **stack;\n\
 \};\n\
 \\n\
 \#define EMPTY_STACK { 0, 0, NULL }\n\
 \\n\
 \static void\n\
 \stack_grow(struct stack *s, unsigned grow)\n\
 \{\n\
 \        s->size += grow;\n\
 \        s->stack = realloc(s->stack, sizeof(s->stack[0]) * s->size);\n\
 \        assert(s->stack);\n\
 \        debugf(\"stack:\");\n\
 \        for (unsigned i = 0; i < s->ptr; i++) {\n\
 \                debugf(\" %p\", (void *)s->stack[i]);\n\
 \        }\n\
 \        debugf(\"\\n\");\n\
 \}\n\
 \\n\
 \inline static void\n\
 \stack_check(struct stack *s, unsigned n)\n\
 \{\n\
 \        if (__predict_false(s->size - s->ptr < n)) {\n\
 \#ifndef _JHC_JGC_STACKGROW\n\
 \#define _JHC_JGC_STACKGROW (1024)\n\
 \#endif\n\
 \                stack_grow(s, n + (_JHC_JGC_STACKGROW));\n\
 \        }\n\
 \}\n\
 \\n\
 \static struct stack root_stack = EMPTY_STACK;\n\
 \\n\
 \void gc_add_root(gc_t gc, void *root)\n\
 \{\n\
 \        if (IS_PTR(root)) {\n\
 \                entry_t *nroot = TO_GCPTR(root);\n\
 \                if (gc_check_heap(nroot)) {\n\
 \                        stack_check(&root_stack, 1);\n\
 \                        root_stack.stack[root_stack.ptr++] = nroot;\n\
 \                }\n\
 \        }\n\
 \}\n\
 \\n\
 \static void\n\
 \gc_add_grey(struct stack *stack, entry_t *s)\n\
 \{\n\
 \        VALGRIND_MAKE_MEM_DEFINED(s, (S_BLOCK(s))->u.pi.size * sizeof(uintptr_t));\n\
 \        if (gc_check_heap(s) && s_set_used_bit(s))\n\
 \                stack->stack[stack->ptr++] = s;\n\
 \}\n\
 \\n\
 \static void\n\
 \gc_mark_deeper(struct stack *stack, unsigned *number_redirects)\n\
 \{\n\
 \        while (stack->ptr) {\n\
 \                entry_t *e = stack->stack[--stack->ptr];\n\
 \                struct s_block *pg = S_BLOCK(e);\n\
 \                if (!(pg->flags & SLAB_MONOLITH))\n\
 \                        VALGRIND_MAKE_MEM_DEFINED(e, pg->u.pi.size * sizeof(uintptr_t));\n\
 \                debugf(\"Processing Grey: %p\\n\", e);\n\
 \                unsigned num_ptrs = pg->flags & SLAB_MONOLITH ? pg->u.m.num_ptrs : pg->u.pi.num_ptrs;\n\
 \                stack_check(stack, num_ptrs);\n\
 \                for (unsigned i = 0; i < num_ptrs; i++) {\n\
 \                        if (1 && (P_LAZY == GET_PTYPE(e->ptrs[i]))) {\n\
 \                                VALGRIND_MAKE_MEM_DEFINED(FROM_SPTR(e->ptrs[i]), sizeof(uintptr_t));\n\
 \                                if (!IS_LAZY(GETHEAD(FROM_SPTR(e->ptrs[i])))) {\n\
 \                                        number_redirects[0]++;\n\
 \                                        debugf(\" *\");\n\
 \                                        e->ptrs[i] = (sptr_t)GETHEAD(FROM_SPTR(e->ptrs[i]));\n\
 \                                }\n\
 \                        }\n\
 \                        if (IS_PTR(e->ptrs[i])) {\n\
 \                                entry_t *ptr = TO_GCPTR(e->ptrs[i]);\n\
 \                                debugf(\"Following: %p %p\\n\", e->ptrs[i], ptr);\n\
 \                                gc_add_grey(stack, ptr);\n\
 \                        }\n\
 \                }\n\
 \        }\n\
 \}\n\
 \\n\
 \#if defined(_JHC_JGC_SAVING_MALLOC_HEAP)\n\
 \#define DO_GC_MARK_DEEPER(S,N)  gc_mark_deeper((S),(N))\n\
 \#else\n\
 \#define DO_GC_MARK_DEEPER(S,N)  do { } while (/* CONSTCOND */ 0)\n\
 \#endif\n\
 \\n\
 \void A_STD\n\
 \gc_perform_gc(gc_t gc)\n\
 \{\n\
 \        profile_push(&gc_gc_time);\n\
 \        arena->number_gcs++;\n\
 \        unsigned number_redirects = 0;\n\
 \        unsigned number_stack = 0;\n\
 \        unsigned number_ptr = 0;\n\
 \        struct stack stack = EMPTY_STACK;\n\
 \        clear_used_bits(arena);\n\
 \        debugf(\"Setting Roots:\");\n\
 \        stack_check(&stack, root_stack.ptr);\n\
 \        for (unsigned i = 0; i < root_stack.ptr; i++) {\n\
 \                gc_add_grey(&stack, root_stack.stack[i]);\n\
 \                debugf(\" %p\", root_stack.stack[i]);\n\
 \                DO_GC_MARK_DEEPER(&stack, &number_redirects);\n\
 \        }\n\
 \        debugf(\" # \");\n\
 \        struct StablePtr *sp;\n\
 \        LIST_FOREACH(sp, &root_StablePtrs, link) {\n\
 \                gc_add_grey(&stack, (entry_t *)sp);\n\
 \                debugf(\" %p\", sp);\n\
 \                DO_GC_MARK_DEEPER(&stack, &number_redirects);\n\
 \        }\n\
 \        debugf(\"\\n\");\n\
 \        debugf(\"Trace:\");\n\
 \#if defined(_JHC_JGC_SAVING_MALLOC_HEAP)\n\
 \        stack_check(&stack, 1); // Just alloc\n\
 \#else\n\
 \        stack_check(&stack, gc - gc_stack_base);\n\
 \#endif\n\
 \        number_stack = gc - gc_stack_base;\n\
 \        for (unsigned i = 0; i < number_stack; i++) {\n\
 \                debugf(\" |\");\n\
 \                // TODO - short circuit redirects on stack\n\
 \                sptr_t ptr = gc_stack_base[i];\n\
 \                if (1 && (IS_LAZY(ptr))) {\n\
 \                        assert(GET_PTYPE(ptr) == P_LAZY);\n\
 \                        VALGRIND_MAKE_MEM_DEFINED(FROM_SPTR(ptr), sizeof(uintptr_t));\n\
 \                        if (!IS_LAZY(GETHEAD(FROM_SPTR(ptr)))) {\n\
 \                                void *gptr = TO_GCPTR(ptr);\n\
 \                                if (gc_check_heap(gptr))\n\
 \                                        s_set_used_bit(gptr);\n\
 \                                number_redirects++;\n\
 \                                debugf(\" *\");\n\
 \                                ptr = (sptr_t)GETHEAD(FROM_SPTR(ptr));\n\
 \                        }\n\
 \                }\n\
 \                if (__predict_false(!IS_PTR(ptr))) {\n\
 \                        debugf(\" -\");\n\
 \                        continue;\n\
 \                }\n\
 \                number_ptr++;\n\
 \                entry_t *e = TO_GCPTR(ptr);\n\
 \                debugf(\" %p\", (void *)e);\n\
 \                gc_add_grey(&stack, e);\n\
 \                DO_GC_MARK_DEEPER(&stack, &number_redirects);\n\
 \        }\n\
 \        debugf(\"\\n\");\n\
 \        gc_mark_deeper(&stack, &number_redirects); // Final marking\n\
 \        free(stack.stack);\n\
 \        s_cleanup_blocks(arena);\n\
 \        if (JHC_STATUS) {\n\
 \                fprintf(stderr, \"%3u - %6u Used: %4u Thresh: %4u Ss: %5u Ps: %5u Rs: %5u Root: %3u\\n\",\n\
 \                        arena->number_gcs,\n\
 \                        arena->number_allocs,\n\
 \                        (unsigned)arena->block_used,\n\
 \                        (unsigned)arena->block_threshold,\n\
 \                        number_stack,\n\
 \                        number_ptr,\n\
 \                        number_redirects,\n\
 \                        (unsigned)root_stack.ptr\n\
 \                       );\n\
 \                arena->number_allocs = 0;\n\
 \        }\n\
 \        profile_pop(&gc_gc_time);\n\
 \}\n\
 \\n\
 \// 7 to share caches with the first 7 tuples\n\
 \#define GC_STATIC_ARRAY_NUM 7\n\
 \#define GC_MAX_BLOCK_ENTRIES 150\n\
 \\n\
 \static struct s_cache *array_caches[GC_STATIC_ARRAY_NUM];\n\
 \static struct s_cache *array_caches_atomic[GC_STATIC_ARRAY_NUM];\n\
 \\n\
 \void\n\
 \jhc_alloc_init(void)\n\
 \{\n\
 \        VALGRIND_PRINTF(\"Jhc-Valgrind mode active.\\n\");\n\
 \#ifdef _JHC_JGC_FIXED_MEGABLOCK\n\
 \        saved_gc = gc_stack_base = (void *) gc_stack_base_area;\n\
 \#else\n\
 \        saved_gc = gc_stack_base = malloc((1UL << 18) * sizeof(gc_stack_base[0]));\n\
 \#endif\n\
 \        arena = new_arena();\n\
 \        if (nh_stuff[0]) {\n\
 \                nh_end = nh_start = nh_stuff[0];\n\
 \                for (int i = 1; nh_stuff[i]; i++) {\n\
 \                        if (nh_stuff[i] < nh_start)\n\
 \                                nh_start = nh_stuff[i];\n\
 \                        if (nh_stuff[i] > nh_end)\n\
 \                                nh_end = nh_stuff[i];\n\
 \                }\n\
 \        }\n\
 \        for (int i = 0; i < GC_STATIC_ARRAY_NUM; i++) {\n\
 \                find_cache(&array_caches[i], arena, i + 1, i + 1);\n\
 \                find_cache(&array_caches_atomic[i], arena, i + 1, 0);\n\
 \        }\n\
 \}\n\
 \\n\
 \void\n\
 \jhc_alloc_fini(void)\n\
 \{\n\
 \        if (_JHC_PROFILE || JHC_STATUS) {\n\
 \                fprintf(stderr, \"arena: %p\\n\", arena);\n\
 \                fprintf(stderr, \"  block_used: %i\\n\", arena->block_used);\n\
 \                fprintf(stderr, \"  block_threshold: %i\\n\", arena->block_threshold);\n\
 \                struct s_cache *sc;\n\
 \                SLIST_FOREACH(sc, &arena->caches, next)\n\
 \                print_cache(sc);\n\
 \        }\n\
 \}\n\
 \\n\
 \heap_t A_STD\n\
 \(gc_alloc)(gc_t gc, struct s_cache **sc, unsigned count, unsigned nptrs)\n\
 \{\n\
 \        assert(nptrs <= count);\n\
 \        entry_t *e = s_alloc(gc, find_cache(sc, arena, count, nptrs));\n\
 \        VALGRIND_MAKE_MEM_UNDEFINED(e, sizeof(uintptr_t)*count);\n\
 \        debugf(\"gc_alloc: %p %i %i\\n\", (void *)e, count, nptrs);\n\
 \        return (void *)e;\n\
 \}\n\
 \\n\
 \static heap_t A_STD\n\
 \s_monoblock(struct s_arena *arena, unsigned size, unsigned nptrs, unsigned flags)\n\
 \{\n\
 \        struct s_block *b = jhc_aligned_alloc(size * sizeof(uintptr_t));\n\
 \        b->flags = flags | SLAB_MONOLITH;\n\
 \        b->color = (sizeof(struct s_block) + BITARRAY_SIZE_IN_BYTES(1) +\n\
 \                    sizeof(uintptr_t) - 1) / sizeof(uintptr_t);\n\
 \        b->u.m.num_ptrs = nptrs;\n\
 \        SLIST_INSERT_HEAD(&arena->monolithic_blocks, b, link);\n\
 \        b->used[0] = 1;\n\
 \        return (void *)b + b->color * sizeof(uintptr_t);\n\
 \}\n\
 \\n\
 \// Allocate an array of count garbage collectable locations in the garbage\n\
 \// collected heap.\n\
 \heap_t A_STD\n\
 \gc_array_alloc(gc_t gc, unsigned count)\n\
 \{\n\
 \        if (!count)\n\
 \                return NULL;\n\
 \        if (count <= GC_STATIC_ARRAY_NUM)\n\
 \                return (wptr_t)s_alloc(gc, array_caches[count - 1]);\n\
 \        if (count < GC_MAX_BLOCK_ENTRIES)\n\
 \                return s_alloc(gc, find_cache(NULL, arena, count, count));\n\
 \        return s_monoblock(arena, count, count, 0);\n\
 \        abort();\n\
 \}\n\
 \\n\
 \// Allocate an array of count non-garbage collectable locations in the garbage\n\
 \// collected heap.\n\
 \heap_t A_STD\n\
 \gc_array_alloc_atomic(gc_t gc, unsigned count, unsigned flags)\n\
 \{\n\
 \        if (!count)\n\
 \                return NULL;\n\
 \        if (count <= GC_STATIC_ARRAY_NUM && !flags)\n\
 \                return (wptr_t)s_alloc(gc, array_caches_atomic[count - 1]);\n\
 \        if (count < GC_MAX_BLOCK_ENTRIES && !flags)\n\
 \                return s_alloc(gc, find_cache(NULL, arena, count, 0));\n\
 \        return s_monoblock(arena, count, count, flags);\n\
 \        abort();\n\
 \}\n\
 \\n\
 \/* This finds a bit that isn't set, sets it, then returns its index.  It\n\
 \ * assumes that a bit is available to be found, otherwise it goes into an\n\
 \ * infinite loop. */\n\
 \\n\
 \static unsigned\n\
 \bitset_find_free(unsigned *next_free, int n, bitarray_t ba[static n])\n\
 \{\n\
 \        assert(*next_free < (unsigned)n);\n\
 \        unsigned i = *next_free;\n\
 \        do {\n\
 \                int o = __builtin_ffsl(~ba[i]);\n\
 \                if (__predict_true(o)) {\n\
 \                        ba[i] |= (1UL << (o - 1));\n\
 \                        *next_free = i;\n\
 \                        return (i * BITS_PER_UNIT + (o - 1));\n\
 \                }\n\
 \                i = (i + 1) % n;\n\
 \                assert(i != *next_free);\n\
 \        } while (1);\n\
 \}\n\
 \\n\
 \static void *\n\
 \jhc_aligned_alloc(unsigned size)\n\
 \{\n\
 \        void *base;\n\
 \#if defined(__WIN32__)\n\
 \        base = _aligned_malloc(MEGABLOCK_SIZE, BLOCK_SIZE);\n\
 \        int ret = !base;\n\
 \#elif defined(__ARM_EABI__)\n\
 \        base = memalign(BLOCK_SIZE, MEGABLOCK_SIZE);\n\
 \        int ret = !base;\n\
 \#elif (defined(__ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__) && __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ <  1060)\n\
 \        assert(sysconf(_SC_PAGESIZE) == BLOCK_SIZE);\n\
 \        base = valloc(MEGABLOCK_SIZE);\n\
 \        int ret = !base;\n\
 \#else\n\
 \        int ret = posix_memalign(&base, BLOCK_SIZE, MEGABLOCK_SIZE);\n\
 \#endif\n\
 \        if (ret != 0) {\n\
 \                fprintf(stderr, \"Unable to allocate memory for aligned alloc: %u\\n\", size);\n\
 \                abort();\n\
 \        }\n\
 \        return base;\n\
 \}\n\
 \\n\
 \struct s_megablock *\n\
 \s_new_megablock(struct s_arena *arena)\n\
 \{\n\
 \        struct s_megablock *mb = malloc(sizeof(*mb));\n\
 \#ifdef _JHC_JGC_FIXED_MEGABLOCK\n\
 \        static int count = 0;\n\
 \        if (count != 0) {\n\
 \                abort();\n\
 \        }\n\
 \        count++;\n\
 \        mb->base = aligned_megablock_1;\n\
 \#else\n\
 \        mb->base = jhc_aligned_alloc(MEGABLOCK_SIZE);\n\
 \#endif\n\
 \        VALGRIND_MAKE_MEM_NOACCESS(mb->base, MEGABLOCK_SIZE);\n\
 \        mb->next_free = 0;\n\
 \        return mb;\n\
 \}\n\
 \\n\
 \/* block allocator */\n\
 \\n\
 \static struct s_block *\n\
 \get_free_block(gc_t gc, struct s_arena *arena, bool retry)\n\
 \{\n\
 \        arena->block_used++;\n\
 \        if (__predict_true(SLIST_FIRST(&arena->free_blocks))) {\n\
 \                struct s_block *pg = SLIST_FIRST(&arena->free_blocks);\n\
 \                SLIST_REMOVE_HEAD(&arena->free_blocks, link);\n\
 \                return pg;\n\
 \        } else {\n\
 \#ifdef _JHC_JGC_NAIVEGC\n\
 \                if (retry == false) {\n\
 \                        gc_perform_gc(gc);\n\
 \                        return NULL;\n\
 \                }\n\
 \#else\n\
 \                if ((arena->block_used >= arena->block_threshold)) {\n\
 \                        gc_perform_gc(gc);\n\
 \                        // if we are still using 80% of the heap after a gc, raise the threshold.\n\
 \                        if (__predict_false((unsigned)arena->block_used * 10 >= arena->block_threshold * 9)) {\n\
 \                                arena->block_threshold *= 2;\n\
 \                        }\n\
 \                }\n\
 \#endif\n\
 \                if (__predict_false(!arena->current_megablock))\n\
 \                        arena->current_megablock = s_new_megablock(arena);\n\
 \                struct s_megablock *mb = arena->current_megablock;\n\
 \                struct s_block *pg = mb->base + BLOCK_SIZE * mb->next_free;\n\
 \                mb->next_free++;\n\
 \                if (mb->next_free == MEGABLOCK_SIZE / BLOCK_SIZE) {\n\
 \                        SLIST_INSERT_HEAD(&arena->megablocks, mb, next);\n\
 \                        arena->current_megablock = NULL;\n\
 \                }\n\
 \                VALGRIND_MAKE_MEM_UNDEFINED(pg, sizeof(struct s_block));\n\
 \                pg->u.pi.num_free = 0;\n\
 \                return pg;\n\
 \        }\n\
 \}\n\
 \\n\
 \typedef void (*finalizer_ptr)(HsPtr arg);\n\
 \typedef void (*finalizer_env_ptr)(HsPtr env, HsPtr arg);\n\
 \\n\
 \void hs_foreignptr_env_helper(HsPtr env, HsPtr arg)\n\
 \{\n\
 \        ((finalizer_ptr)env)(arg);\n\
 \}\n\
 \\n\
 \static void\n\
 \s_cleanup_blocks(struct s_arena *arena)\n\
 \{\n\
 \        struct s_block *pg = SLIST_FIRST(&arena->monolithic_blocks);\n\
 \        SLIST_INIT(&arena->monolithic_blocks);\n\
 \        while (pg) {\n\
 \                if (pg->used[0]) {\n\
 \                        SLIST_INSERT_HEAD(&arena->monolithic_blocks, pg, link);\n\
 \                        pg = SLIST_NEXT(pg, link);\n\
 \                } else {\n\
 \                        if (pg->flags & SLAB_FLAG_FINALIZER) {\n\
 \                                HsPtr *ptr = (HsPtr *)pg;\n\
 \                                if (ptr[pg->color + 1]) {\n\
 \                                        finalizer_ptr *fp = ptr[pg->color + 1];\n\
 \                                        do {\n\
 \                                                fp[0](ptr[pg->color]);\n\
 \                                        } while (*++fp);\n\
 \                                }\n\
 \                        }\n\
 \                        void *ptr = pg;\n\
 \                        pg = SLIST_NEXT(pg, link);\n\
 \                        free(ptr);\n\
 \                }\n\
 \        }\n\
 \        struct s_cache *sc = SLIST_FIRST(&arena->caches);\n\
 \        for (; sc; sc = SLIST_NEXT(sc, next)) {\n\
 \                // 'best' keeps track of the block with the fewest free spots\n\
 \                // and percolates it to the front, effectively a single pass\n\
 \                // of a bubblesort to help combat fragmentation. It does\n\
 \                // not increase the complexity of the cleanup algorithm as\n\
 \                // we had to scan every block anyway, but over many passes\n\
 \                // of the GC it will eventually result in a more sorted list\n\
 \                // than would occur by chance.\n\
 \                struct s_block *best = NULL;\n\
 \                int free_best = 4096;\n\
 \                pg = SLIST_FIRST(&sc->blocks);\n\
 \                struct s_block *fpg = SLIST_FIRST(&sc->full_blocks);\n\
 \                SLIST_INIT(&sc->blocks);\n\
 \                SLIST_INIT(&sc->full_blocks);\n\
 \                if (!pg) {\n\
 \                        pg = fpg;\n\
 \                        fpg = NULL;\n\
 \                }\n\
 \                while (pg) {\n\
 \                        struct s_block *npg = SLIST_NEXT(pg, link);\n\
 \                        if (__predict_false(pg->u.pi.num_free == 0)) {\n\
 \                                // Add full blockes to the cache's full block list.\n\
 \                                SLIST_INSERT_HEAD(&sc->full_blocks, pg, link);\n\
 \                        } else if (__predict_true(pg->u.pi.num_free == sc->num_entries)) {\n\
 \                                // Return completely free block to arena free block list.\n\
 \                                arena->block_used--;\n\
 \                                VALGRIND_MAKE_MEM_NOACCESS((char *)pg + sizeof(struct s_block),\n\
 \                                                           BLOCK_SIZE - sizeof(struct s_block));\n\
 \                                SLIST_INSERT_HEAD(&arena->free_blocks, pg, link);\n\
 \                        } else {\n\
 \                                if (!best) {\n\
 \                                        free_best = pg->u.pi.num_free;\n\
 \                                        best = pg;\n\
 \                                } else {\n\
 \                                        if (pg->u.pi.num_free < free_best) {\n\
 \                                                struct s_block *tmp = best;\n\
 \                                                best = pg; pg = tmp;\n\
 \                                                free_best = pg->u.pi.num_free;\n\
 \                                        }\n\
 \                                        SLIST_INSERT_HEAD(&sc->blocks, pg, link);\n\
 \                                }\n\
 \                        }\n\
 \                        if (!npg && fpg) {\n\
 \                                pg = fpg;\n\
 \                                fpg = NULL;\n\
 \                        } else\n\
 \                                pg = npg;\n\
 \                }\n\
 \                if (best)\n\
 \                        SLIST_INSERT_HEAD(&sc->blocks, best, link);\n\
 \        }\n\
 \}\n\
 \\n\
 \inline static void\n\
 \clear_block_used_bits(unsigned num_entries, struct s_block *pg)\n\
 \{\n\
 \        pg->u.pi.num_free = num_entries;\n\
 \        memset(pg->used, 0, BITARRAY_SIZE_IN_BYTES(num_entries) - sizeof(pg->used[0]));\n\
 \        int excess = num_entries % BITS_PER_UNIT;\n\
 \        pg->used[BITARRAY_SIZE(num_entries) - 1] = ~((1UL << excess) - 1);\n\
 \#if JHC_VALGRIND\n\
 \        unsigned header =  sizeof(struct s_block) + BITARRAY_SIZE_IN_BYTES(num_entries);\n\
 \        VALGRIND_MAKE_MEM_NOACCESS((char *)pg + header, BLOCK_SIZE - header);\n\
 \#endif\n\
 \}\n\
 \\n\
 \/*\n\
 \ * allocators\n\
 \ */\n\
 \\n\
 \heap_t A_STD\n\
 \s_alloc(gc_t gc, struct s_cache *sc)\n\
 \{\n\
 \#if _JHC_PROFILE\n\
 \        sc->allocations++;\n\
 \        sc->arena->number_allocs++;\n\
 \#endif\n\
 \        bool retry = false;\n\
 \        struct s_block *pg;\n\
 \retry_s_alloc:\n\
 \        pg = SLIST_FIRST(&sc->blocks);\n\
 \        if (__predict_false(!pg)) {\n\
 \                pg = get_free_block(gc, sc->arena, retry);\n\
 \                if (__predict_false(!pg)) {\n\
 \                        retry = true;\n\
 \                        goto retry_s_alloc;\n\
 \                }\n\
 \                VALGRIND_MAKE_MEM_NOACCESS(pg, BLOCK_SIZE);\n\
 \                VALGRIND_MAKE_MEM_DEFINED(pg, sizeof(struct s_block));\n\
 \                if (sc->num_entries != pg->u.pi.num_free)\n\
 \                        VALGRIND_MAKE_MEM_UNDEFINED((char *)pg->used,\n\
 \                                                    BITARRAY_SIZE_IN_BYTES(sc->num_entries));\n\
 \                else\n\
 \                        VALGRIND_MAKE_MEM_DEFINED((char *)pg->used,\n\
 \                                                  BITARRAY_SIZE_IN_BYTES(sc->num_entries));\n\
 \                assert(pg);\n\
 \                pg->flags = sc->flags;\n\
 \                pg->color = sc->color;\n\
 \                pg->u.pi.num_ptrs = sc->num_ptrs;\n\
 \                pg->u.pi.size = sc->size;\n\
 \                pg->u.pi.next_free = 0;\n\
 \                SLIST_INSERT_HEAD(&sc->blocks, pg, link);\n\
 \                if (sc->num_entries != pg->u.pi.num_free)\n\
 \                        clear_block_used_bits(sc->num_entries, pg);\n\
 \                pg->used[0] = 1; //set the first bit\n\
 \                pg->u.pi.num_free = sc->num_entries - 1;\n\
 \                return (uintptr_t *)pg + pg->color;\n\
 \        } else {\n\
 \                __builtin_prefetch(pg->used, 1);\n\
 \                pg->u.pi.num_free--;\n\
 \                unsigned next_free = pg->u.pi.next_free;\n\
 \                unsigned found = bitset_find_free(&next_free, BITARRAY_SIZE(sc->num_entries), pg->used);\n\
 \                pg->u.pi.next_free = next_free;\n\
 \                void *val = (uintptr_t *)pg + pg->color + found * pg->u.pi.size;\n\
 \                if (__predict_false(0 == pg->u.pi.num_free)) {\n\
 \                        assert(pg == SLIST_FIRST(&sc->blocks));\n\
 \                        SLIST_REMOVE_HEAD(&sc->blocks, link);\n\
 \                        SLIST_INSERT_HEAD(&sc->full_blocks, pg, link);\n\
 \                }\n\
 \                assert(S_BLOCK(val) == pg);\n\
 \                //printf(\"s_alloc: val: %p s_block: %p size: %i color: %i found: %i num_free: %i\\n\", val, pg, pg->pi.size, pg->pi.color, found, pg->num_free);\n\
 \                return val;\n\
 \        }\n\
 \}\n\
 \\n\
 \struct s_cache *\n\
 \new_cache(struct s_arena *arena, unsigned short size, unsigned short num_ptrs)\n\
 \{\n\
 \        struct s_cache *sc = malloc(sizeof(*sc));\n\
 \        memset(sc, 0, sizeof(*sc));\n\
 \        sc->arena = arena;\n\
 \        sc->size = size;\n\
 \        sc->num_ptrs = num_ptrs;\n\
 \        sc->flags = 0;\n\
 \        size_t excess = BLOCK_SIZE - sizeof(struct s_block);\n\
 \        sc->num_entries = (8 * excess) / (8 * sizeof(uintptr_t) * size + 1) - 1;\n\
 \        sc->color = (sizeof(struct s_block) + BITARRAY_SIZE_IN_BYTES(sc->num_entries) +\n\
 \                     sizeof(uintptr_t) - 1) / sizeof(uintptr_t);\n\
 \        SLIST_INIT(&sc->blocks);\n\
 \        SLIST_INIT(&sc->full_blocks);\n\
 \        SLIST_INSERT_HEAD(&arena->caches, sc, next);\n\
 \        return sc;\n\
 \}\n\
 \\n\
 \// clear all used bits, must be followed by a marking phase.\n\
 \static void\n\
 \clear_used_bits(struct s_arena *arena)\n\
 \{\n\
 \        struct s_block *pg;\n\
 \        SLIST_FOREACH(pg, &arena->monolithic_blocks, link)\n\
 \        pg->used[0] = 0;\n\
 \        struct s_cache *sc = SLIST_FIRST(&arena->caches);\n\
 \        for (; sc; sc = SLIST_NEXT(sc, next)) {\n\
 \                SLIST_FOREACH(pg, &sc->blocks, link)\n\
 \                clear_block_used_bits(sc->num_entries, pg);\n\
 \                SLIST_FOREACH(pg, &sc->full_blocks, link)\n\
 \                clear_block_used_bits(sc->num_entries, pg);\n\
 \        }\n\
 \}\n\
 \\n\
 \// Set a used bit. returns true if the tagged node should be scanned by the GC.\n\
 \// this happens when the used bit was not previously set and the node contains\n\
 \// internal pointers.\n\
 \\n\
 \static bool\n\
 \s_set_used_bit(void *val)\n\
 \{\n\
 \        assert(val);\n\
 \        struct s_block *pg = S_BLOCK(val);\n\
 \        unsigned int offset = ((uintptr_t *)val - (uintptr_t *)pg) - pg->color;\n\
 \        if (__predict_true(BIT_IS_UNSET(pg->used, offset / pg->u.pi.size))) {\n\
 \                if (pg->flags & SLAB_MONOLITH) {\n\
 \                        pg->used[0] = 1;\n\
 \                        return (bool)pg->u.m.num_ptrs;\n\
 \                } else {\n\
 \                        BIT_SET(pg->used, offset / pg->u.pi.size);\n\
 \                        pg->u.pi.num_free--;\n\
 \                        return (bool)pg->u.pi.num_ptrs;\n\
 \                }\n\
 \        }\n\
 \        return false;\n\
 \}\n\
 \\n\
 \struct s_cache *\n\
 \find_cache(struct s_cache **rsc, struct s_arena *arena,\n\
 \           unsigned short size, unsigned short num_ptrs)\n\
 \{\n\
 \        if (__predict_true(rsc && *rsc))\n\
 \                return *rsc;\n\
 \        struct s_cache *sc = SLIST_FIRST(&arena->caches);\n\
 \        for (; sc; sc = SLIST_NEXT(sc, next)) {\n\
 \                if (sc->size == size && sc->num_ptrs == num_ptrs)\n\
 \                        goto found;\n\
 \        }\n\
 \        sc = new_cache(arena, size, num_ptrs);\n\
 \found:\n\
 \        if (rsc)\n\
 \                *rsc = sc;\n\
 \        return sc;\n\
 \}\n\
 \\n\
 \struct s_arena *\n\
 \new_arena(void)\n\
 \{\n\
 \        struct s_arena *arena = malloc(sizeof(struct s_arena));\n\
 \        SLIST_INIT(&arena->caches);\n\
 \        SLIST_INIT(&arena->free_blocks);\n\
 \        SLIST_INIT(&arena->megablocks);\n\
 \        SLIST_INIT(&arena->monolithic_blocks);\n\
 \        arena->block_used = 0;\n\
 \        arena->block_threshold = 8;\n\
 \        arena->current_megablock = NULL;\n\
 \        return arena;\n\
 \}\n\
 \\n\
 \uint32_t\n\
 \get_heap_flags(void *sp)\n\
 \{\n\
 \        uint32_t ret = 0;\n\
 \        switch (GET_PTYPE(sp)) {\n\
 \        case P_VALUE: return SLAB_VIRTUAL_VALUE;\n\
 \        case P_FUNC: return SLAB_VIRTUAL_FUNC;\n\
 \        case P_LAZY:\n\
 \                ret |= SLAB_VIRTUAL_LAZY;\n\
 \        case P_WHNF:\n\
 \                if (S_BLOCK(sp) == NULL)\n\
 \                        return (ret | SLAB_VIRTUAL_SPECIAL);\n\
 \                if ((void *)sp >= nh_start && (void *)sp <= nh_end)\n\
 \                        return (ret | SLAB_VIRTUAL_CONSTANT);\n\
 \                return ret |= S_BLOCK(sp)->flags;\n\
 \        }\n\
 \        return ret;\n\
 \}\n\
 \\n\
 \heap_t A_STD\n\
 \gc_malloc_foreignptr(unsigned alignment, unsigned size, bool finalizer)\n\
 \{\n\
 \        // we don't allow higher alignments yet.\n\
 \        assert(alignment <= sizeof(uintptr_t));\n\
 \        // no finalizers yet\n\
 \        assert(!finalizer);\n\
 \        unsigned spacing = 1 + finalizer;\n\
 \        wptr_t *res = gc_array_alloc_atomic(saved_gc, spacing + TO_BLOCKS(size),\n\
 \                                            finalizer ? SLAB_FLAG_FINALIZER : SLAB_FLAG_NONE);\n\
 \        res[0] = (wptr_t)(res + spacing);\n\
 \        if (finalizer)\n\
 \                res[1] = NULL;\n\
 \        return TO_SPTR(P_WHNF, res);\n\
 \}\n\
 \\n\
 \heap_t A_STD\n\
 \gc_new_foreignptr(HsPtr ptr)\n\
 \{\n\
 \        HsPtr *res = gc_array_alloc_atomic(saved_gc, 2, SLAB_FLAG_FINALIZER);\n\
 \        res[0] = ptr;\n\
 \        res[1] = NULL;\n\
 \        return TO_SPTR(P_WHNF, res);\n\
 \}\n\
 \\n\
 \bool A_STD\n\
 \gc_add_foreignptr_finalizer(wptr_t fp, HsFunPtr finalizer)\n\
 \{\n\
 \        if (!(SLAB_FLAG_FINALIZER & get_heap_flags(fp)))\n\
 \                return false;\n\
 \        HsFunPtr **res = (HsFunPtr **)FROM_SPTR(fp);\n\
 \        unsigned len = 0;\n\
 \        if (res[1])\n\
 \                while (res[1][len++]);\n\
 \        else\n\
 \                len = 1;\n\
 \        res[1] = realloc(res[1], (len + 1) * sizeof(HsFunPtr));\n\
 \        HsFunPtr *ptrs = res[1];\n\
 \        ptrs[len - 1] = finalizer;\n\
 \        ptrs[len] = NULL;\n\
 \        return true;\n\
 \}\n\
 \\n\
 \void\n\
 \print_cache(struct s_cache *sc)\n\
 \{\n\
 \        fprintf(stderr, \"num_entries: %i with %lu bytes of header\\n\",\n\
 \                (int)sc->num_entries, sizeof(struct s_block) +\n\
 \                BITARRAY_SIZE_IN_BYTES(sc->num_entries));\n\
 \        fprintf(stderr, \"  size: %i words %i ptrs\\n\",\n\
 \                (int)sc->size, (int)sc->num_ptrs);\n\
 \#if _JHC_PROFILE\n\
 \        fprintf(stderr, \"  allocations: %lu\\n\", (unsigned long)sc->allocations);\n\
 \#endif\n\
 \        if (SLIST_EMPTY(&sc->blocks) && SLIST_EMPTY(&sc->full_blocks))\n\
 \                return;\n\
 \        fprintf(stderr, \"  blocks:\\n\");\n\
 \        fprintf(stderr, \"%20s %9s %9s %s\\n\", \"block\", \"num_free\", \"next_free\", \"status\");\n\
 \        struct s_block *pg;\n\
 \        SLIST_FOREACH(pg, &sc->blocks, link)\n\
 \        fprintf(stderr, \"%20p %9i %9i %c\\n\", pg, pg->u.pi.num_free, pg->u.pi.next_free, 'P');\n\
 \        SLIST_FOREACH(pg, &sc->full_blocks, link)\n\
 \        fprintf(stderr, \"%20p %9i %9i %c\\n\", pg, pg->u.pi.num_free, pg->u.pi.next_free, 'F');\n\
 \}\n\
 \\n\
 \void hs_perform_gc(void)\n\
 \{\n\
 \        gc_perform_gc(saved_gc);\n\
 \}\n\
 \\n\
 \#endif\n\
 \"#

-- | Generated from rts\/rts\/gc_jgc.h
{-# NOINLINE gc_jgc_h #-}
gc_jgc_h :: ByteString
gc_jgc_h = unsafePerformIO $ unsafePackAddress "\
 \#ifndef JHC_GC_JGC_H\n\
 \#define JHC_GC_JGC_H\n\
 \\n\
 \#include <stdbool.h>\n\
 \#include <stdint.h>\n\
 \#include \"sys/queue.h\"\n\
 \#include \"HsFFI.h\"\n\
 \\n\
 \struct sptr;\n\
 \struct s_arena;\n\
 \struct s_cache;\n\
 \typedef void **gc_t;\n\
 \typedef void *heap_t;  // a pointer into the GCed heap.\n\
 \\n\
 \#if defined(_JHC_JGC_BLOCK_SHIFT) && defined(_JHC_JGC_MEGABLOCK_SHIFT)\n\
 \#if (_JHC_JGC_BLOCK_SHIFT) >= (_JHC_JGC_MEGABLOCK_SHIFT)\n\
 \#error \"_JHC_JGC_MEGABLOCK_SHIFT should be larger than _JHC_JGC_BLOCK_SHIFT.\"\n\
 \#endif\n\
 \#elif defined(_JHC_JGC_BLOCK_SHIFT) || defined(_JHC_JGC_MEGABLOCK_SHIFT)\n\
 \#error \"Should define both _JHC_JGC_BLOCK_SHIFT and _JHC_JGC_MEGABLOCK_SHIFT.\"\n\
 \#else\n\
 \#define _JHC_JGC_BLOCK_SHIFT     12\n\
 \#define _JHC_JGC_MEGABLOCK_SHIFT 20\n\
 \#endif /* defined(_JHC_JGC_BLOCK_SHIFT) && defined(_JHC_JGC_MEGABLOCK_SHIFT) */\n\
 \\n\
 \#define BLOCK_SIZE     (1UL << (_JHC_JGC_BLOCK_SHIFT))\n\
 \#define MEGABLOCK_SIZE (1UL << (_JHC_JGC_MEGABLOCK_SHIFT))\n\
 \#define S_BLOCK(val) ((struct s_block *)((uintptr_t)(val) & ~(BLOCK_SIZE - 1)))\n\
 \#define TO_BLOCKS(x) (((x) + sizeof(uintptr_t) - 1)/sizeof(uintptr_t))\n\
 \\n\
 \extern struct s_arena *arena;\n\
 \extern gc_t saved_gc;\n\
 \\n\
 \void print_cache(struct s_cache *sc);\n\
 \struct s_cache *new_cache(struct s_arena *arena, unsigned short size,\n\
 \                          unsigned short num_ptrs);\n\
 \struct s_arena *new_arena(void);\n\
 \struct s_cache *find_cache(struct s_cache **rsc, struct s_arena *arena,\n\
 \                           unsigned short size, unsigned short num_ptrs);\n\
 \void gc_add_root(gc_t gc, void *root);\n\
 \void A_STD gc_perform_gc(gc_t gc);\n\
 \uint32_t get_heap_flags(void *sp);\n\
 \\n\
 \heap_t s_alloc(gc_t gc, struct s_cache *sc) A_STD;\n\
 \heap_t (gc_alloc)(gc_t gc, struct s_cache **sc, unsigned count, unsigned nptrs) A_STD;\n\
 \heap_t gc_array_alloc(gc_t gc, unsigned count) A_STD;\n\
 \heap_t gc_array_alloc_atomic(gc_t gc, unsigned count, unsigned slab_flags) A_STD;\n\
 \/* foreignptr, saved_gc must be set properly. */\n\
 \heap_t gc_malloc_foreignptr(unsigned alignment, unsigned size, bool finalizer) A_STD;\n\
 \heap_t gc_new_foreignptr(HsPtr ptr) A_STD;\n\
 \bool gc_add_foreignptr_finalizer(struct sptr *fp, HsFunPtr finalizer) A_STD;\n\
 \\n\
 \#define gc_frame0(gc,n,...) void *ptrs[n] = { __VA_ARGS__ }; \\\n\
 \        for(int i = 0; i < n; i++) gc[i] = (sptr_t)ptrs[i]; \\\n\
 \        gc_t sgc = gc;  gc_t gc = sgc + n;\n\
 \#define gc_frame1(gc,p1) gc[0] = (sptr_t)p1; gc_t sgc = gc;  gc_t gc = sgc + 1;\n\
 \#define gc_frame2(gc,p1,p2) gc[0] = (sptr_t)p1; gc[1] = (sptr_t)p2; \\\n\
 \                                    gc_t sgc = gc;  gc_t gc = sgc + 2;\n\
 \\n\
 \struct StablePtr {\n\
 \        LIST_ENTRY(StablePtr) link;\n\
 \        struct sptr *contents;\n\
 \};\n\
 \\n\
 \extern LIST_HEAD(StablePtr_list, StablePtr) root_StablePtrs;\n\
 \\n\
 \#endif\n\
 \"#

-- | Generated from rts\/rts\/profile.c
{-# NOINLINE profile_c #-}
profile_c :: ByteString
profile_c = unsafePerformIO $ unsafePackAddress "\
 \// profiling and debugging code.\n\
 \\n\
 \#if defined(__WIN32__)\n\
 \#define HAVE_TIMES 0\n\
 \#else\n\
 \#define HAVE_TIMES 1\n\
 \#endif\n\
 \\n\
 \#include <stdio.h>\n\
 \#include <stdlib.h>\n\
 \#if HAVE_TIMES\n\
 \#include <sys/times.h>\n\
 \#include <time.h>\n\
 \#endif\n\
 \#include <unistd.h>\n\
 \\n\
 \#include \"jhc_rts_header.h\"\n\
 \\n\
 \void A_UNUSED\n\
 \profile_print_header(FILE *file, char *value_unit)\n\
 \{\n\
 \        fprintf(file, \"JOB \\\"%s\", jhc_progname);\n\
 \        for (int i = 0; i < jhc_argc; i++)\n\
 \                fprintf(file, \" %s\", jhc_argv[i]);\n\
 \        fprintf(file, \"\\\"\\n\");\n\
 \        fprintf(file, \"DATE \\\"%s\\\"\\n\", ctime(NULL));\n\
 \        fprintf(file, \"SAMPLE_UNIT \\\"seconds\\\"\\n\");\n\
 \        fprintf(file, \"VALUE_UNIT \\\"%s\\\"\\n\", value_unit ? value_unit : \"bytes\");\n\
 \}\n\
 \\n\
 \#if HAVE_TIMES\n\
 \struct profile_stack {\n\
 \        struct tms tm_total;\n\
 \        struct tms tm_pushed;\n\
 \};\n\
 \\n\
 \struct profile_stack gc_alloc_time;\n\
 \struct profile_stack gc_gc_time;\n\
 \\n\
 \void\n\
 \jhc_profile_push(struct profile_stack *ps)\n\
 \{\n\
 \        times(&ps->tm_pushed);\n\
 \}\n\
 \\n\
 \void\n\
 \jhc_profile_pop(struct profile_stack *ps)\n\
 \{\n\
 \        struct tms tm;\n\
 \        times(&tm);\n\
 \        ps->tm_total.tms_utime += tm.tms_utime - ps->tm_pushed.tms_utime;\n\
 \        ps->tm_total.tms_stime += tm.tms_stime - ps->tm_pushed.tms_stime;\n\
 \}\n\
 \\n\
 \void print_times(struct tms *tm)\n\
 \{\n\
 \#if  !defined(__WIN32__) && !defined(__ARM_EABI__)\n\
 \        float cpt = (float)sysconf(_SC_CLK_TCK);\n\
 \        fprintf(stderr, \"User Time:   %.2fs\\n\", (float)tm->tms_utime / cpt);\n\
 \        fprintf(stderr, \"System Time: %.2fs\\n\", (float)tm->tms_stime / cpt);\n\
 \        fprintf(stderr, \"Total Time:  %.2fs\\n\", (float)(tm->tms_stime + tm->tms_utime) / cpt);\n\
 \#endif\n\
 \        return;\n\
 \}\n\
 \#else\n\
 \\n\
 \struct profile_stack;\n\
 \void jhc_profile_push(struct profile_stack *ps) {}\n\
 \void jhc_profile_pop(struct profile_stack *ps) {}\n\
 \\n\
 \#endif\n\
 \\n\
 \void A_COLD\n\
 \jhc_print_profile(void)\n\
 \{\n\
 \        if (!(_JHC_PROFILE || getenv(\"JHC_RTS_PROFILE\"))) return;\n\
 \        fprintf(stderr, \"\\n-----------------\\n\");\n\
 \        fprintf(stderr, \"Profiling: %s\\n\", jhc_progname);\n\
 \        fprintf(stderr, \"Command: %s\\n\", jhc_command);\n\
 \        fprintf(stderr, \"Complie: %s\\n\", jhc_c_compile);\n\
 \        fprintf(stderr, \"Version: %s\\n\\n\", jhc_version);\n\
 \#if HAVE_TIMES\n\
 \        struct tms tm;\n\
 \        times(&tm);\n\
 \        print_times(&tm);\n\
 \#endif\n\
 \#if _JHC_PROFILE\n\
 \        print_times(&gc_gc_time.tm_total);\n\
 \        print_times(&gc_alloc_time.tm_total);\n\
 \#endif\n\
 \        fprintf(stderr, \"-----------------\\n\");\n\
 \}\n\
 \\n\
 \#if _JHC_PROFILE && _JHC_GC != _JHC_GC_JGC\n\
 \\n\
 \#define BUCKETS 7\n\
 \static unsigned alloced[BUCKETS];\n\
 \static unsigned alloced_atomic[BUCKETS];\n\
 \\n\
 \static void\n\
 \alloc_count(int n, int atomic)\n\
 \{\n\
 \        n = n ? ((n - 1) / sizeof(void *)) + 1 : 0;\n\
 \        n = n > BUCKETS - 1 ? BUCKETS - 1 : n;\n\
 \        (atomic ? alloced_atomic : alloced)[n]++;\n\
 \}\n\
 \\n\
 \static void\n\
 \print_alloc_size_stats(void)\n\
 \{\n\
 \        char fmt[] = \"%10s %10s %10s %10s %10s\\n\";\n\
 \        char fmt2[] = \"%10u %10u %10u %10u %10u\\n\";\n\
 \        fprintf(stderr, fmt, \"Size\", \"Normal\", \"Atomic\", \"Total\", \"Accum\");\n\
 \        fprintf(stderr, fmt, \"----\", \"------\", \"------\", \"-----\", \"-----\");\n\
 \        unsigned accum = 0;\n\
 \        for (int i = 0; i < BUCKETS; i++) {\n\
 \                accum += alloced[i] + alloced_atomic[i];\n\
 \                fprintf(stderr, fmt2, i, alloced[i], alloced_atomic[i], alloced_atomic[i] + alloced[i], accum);\n\
 \        }\n\
 \}\n\
 \#endif\n\
 \\n\
 \#if JHC_MEM_ANNOTATE && _JHC_GC == _JHC_GC_JGC\n\
 \#include <Judy.h>\n\
 \\n\
 \static Pvoid_t mem_annotate = NULL;\n\
 \\n\
 \#define XSTR(x) #x\n\
 \#define STR(x) XSTR(x)\n\
 \#define gc_alloc(gc,sc,c,nptrs) \\\n\
 \    gc_alloc_annot(gc,sc,c,nptrs,(__FILE__ \":\" STR(__LINE__)))\n\
 \\n\
 \A_UNUSED static void *\n\
 \gc_alloc_annot(gc_t gc, struct s_cache **sc, unsigned count, unsigned nptrs, char *str)\n\
 \{\n\
 \        void *ret = (gc_alloc)(gc, sc, count, nptrs);\n\
 \        PWord_t pval;\n\
 \        JLI(pval, mem_annotate, (Word_t)ret);\n\
 \        *pval = (Word_t)str;\n\
 \        return ret;\n\
 \}\n\
 \\n\
 \char *\n\
 \gc_lookup(void *ptr)\n\
 \{\n\
 \        PWord_t pval;\n\
 \        JLG(pval, mem_annotate, (Word_t)ptr & ~(Word_t)3);\n\
 \        return pval ? (char *)*pval : \"(none)\";\n\
 \}\n\
 \\n\
 \#endif\n\
 \\n\
 \#if _JHC_DEBUG  && _JHC_GC == _JHC_GC_JGC\n\
 \\n\
 \// these ensure the type synonyms are available to the debugger\n\
 \uintptr_t _dummy1;\n\
 \node_t *_dummy2;\n\
 \dnode_t *_dummy3;\n\
 \sptr_t *_dummy4;\n\
 \fptr_t *_dummy5;\n\
 \wptr_t *_dummy6;\n\
 \\n\
 \bool A_UNUSED\n\
 \jhc_valid_whnf(wptr_t s)\n\
 \{\n\
 \        return ((GET_PTYPE(s) == P_VALUE) || ((GET_PTYPE(s) == P_WHNF) && jhc_malloc_sanity(s, P_WHNF)));\n\
 \}\n\
 \\n\
 \bool A_UNUSED\n\
 \jhc_valid_lazy(sptr_t s)\n\
 \{\n\
 \        if (jhc_valid_whnf((wptr_t)s))\n\
 \                return true;\n\
 \        assert(GET_PTYPE(s) == P_LAZY);\n\
 \        node_t *ds = (node_t *)FROM_SPTR(s);\n\
 \        assert(jhc_malloc_sanity(ds, P_LAZY));\n\
 \        if (IS_LAZY(ds->head)) {\n\
 \                if (ds->head == BLACK_HOLE) return true;\n\
 \                assert(GET_PTYPE(ds->head) == P_FUNC);\n\
 \                return true;\n\
 \        } else\n\
 \                return jhc_valid_whnf((wptr_t)ds->head);\n\
 \}\n\
 \\n\
 \#endif\n\
 \\n\
 \#if _JHC_DEBUG\n\
 \wptr_t A_STD\n\
 \promote(sptr_t s)\n\
 \{\n\
 \        assert(!IS_LAZY(s));\n\
 \        assert(jhc_valid_whnf((wptr_t)s));\n\
 \        return (wptr_t)s;\n\
 \}\n\
 \\n\
 \sptr_t A_STD\n\
 \demote(wptr_t s)\n\
 \{\n\
 \        assert(!IS_LAZY(s));\n\
 \        assert(jhc_valid_whnf(s));\n\
 \        return (sptr_t)s;\n\
 \}\n\
 \\n\
 \void A_STD\n\
 \update(void *thunk, wptr_t new)\n\
 \{\n\
 \        assert(GETHEAD(thunk) == BLACK_HOLE);\n\
 \        assert(!IS_LAZY(new));\n\
 \        GETHEAD(thunk) = (fptr_t)new;\n\
 \}\n\
 \\n\
 \#endif\n\
 \"#

-- | Generated from rts\/rts\/profile.h
{-# NOINLINE profile_h #-}
profile_h :: ByteString
profile_h = unsafePerformIO $ unsafePackAddress "\
 \#ifndef RTS_PROFILE_H\n\
 \#define RTS_PROFILE_H\n\
 \\n\
 \#include <stdio.h>\n\
 \#include \"rts/cdefs.h\"\n\
 \\n\
 \#ifndef JHC_VALGRIND\n\
 \#define JHC_VALGRIND 0\n\
 \#endif\n\
 \\n\
 \#ifndef JHC_MEM_ANNOTATE\n\
 \#define JHC_MEM_ANNOTATE 0\n\
 \#endif\n\
 \\n\
 \#ifndef _JHC_PROFILE\n\
 \#define _JHC_PROFILE 0\n\
 \#endif\n\
 \\n\
 \#if JHC_VALGRIND\n\
 \#include <valgrind/valgrind.h>\n\
 \#include <valgrind/memcheck.h>\n\
 \#else\n\
 \#define VALGRIND_MAKE_MEM_UNDEFINED(x,y) \\\n\
 \    do { } while (0)\n\
 \#define VALGRIND_MAKE_MEM_DEFINED(x,y) \\\n\
 \    do { } while (0)\n\
 \#define VALGRIND_MAKE_MEM_NOACCESS(x,y) \\\n\
 \    do { } while (0)\n\
 \#define VALGRIND_PRINTF(...) \\\n\
 \    do { } while (0)\n\
 \#endif\n\
 \\n\
 \void A_UNUSED profile_print_header(FILE *file, char *value_unit);\n\
 \void A_COLD jhc_print_profile(void);\n\
 \\n\
 \#if _JHC_PROFILE\n\
 \struct profile_stack;\n\
 \extern struct profile_stack gc_alloc_time;\n\
 \extern struct profile_stack gc_gc_time;\n\
 \void jhc_profile_push(struct profile_stack *ps);\n\
 \void jhc_profile_pop(struct profile_stack *ps);\n\
 \#define profile_push(x) jhc_profile_push(x)\n\
 \#define profile_pop(x)  jhc_profile_pop(x)\n\
 \#else\n\
 \#define profile_push(x)          do { } while(0)\n\
 \#define profile_pop(x)           do { } while(0)\n\
 \#define alloc_count(x,y)         do { } while(0)\n\
 \#define print_alloc_size_stats() do { } while(0)\n\
 \#endif\n\
 \\n\
 \#if JHC_STATUS > 1\n\
 \#define debugf(...) fprintf(stderr,__VA_ARGS__)\n\
 \#else\n\
 \#define debugf(...) do { } while (0)\n\
 \#endif\n\
 \\n\
 \#endif\n\
 \"#

-- | Generated from rts\/rts\/cdefs.h
{-# NOINLINE cdefs_h #-}
cdefs_h :: ByteString
cdefs_h = unsafePerformIO $ unsafePackAddress "\
 \#ifndef RTS_CDEFS_H\n\
 \#define RTS_CDEFS_H\n\
 \\n\
 \// GNU attributes\n\
 \#if !defined(__predict_true)\n\
 \#ifdef __GNUC__\n\
 \#  define __predict_true(exp)     __builtin_expect(!!(exp), 1)\n\
 \#  define __predict_false(exp)    __builtin_expect(!!(exp), 0)\n\
 \#else\n\
 \#  define __predict_true(exp)     (exp)\n\
 \#  define __predict_false(exp)    (exp)\n\
 \#endif\n\
 \#endif\n\
 \\n\
 \#ifdef __GNUC__\n\
 \#define A_ALIGNED  __attribute__ ((aligned))\n\
 \#define A_CONST    __attribute__ ((const))\n\
 \#define A_MALLOC   __attribute__ ((malloc))\n\
 \#define A_MAYALIAS __attribute__ ((__may_alias__))\n\
 \#define A_NORETURN __attribute__ ((noreturn))\n\
 \#define A_PURE     __attribute__ ((pure))\n\
 \#define A_UNUSED   __attribute__ ((unused))\n\
 \#ifdef __i386__\n\
 \#define A_REGPARM __attribute__ ((fastcall))\n\
 \#else\n\
 \#define A_REGPARM\n\
 \#endif\n\
 \#define A_STD    A_REGPARM\n\
 \\n\
 \#else\n\
 \#define A_ALIGNED\n\
 \#define A_CONST\n\
 \#define A_MALLOC\n\
 \#define A_MAYALIAS\n\
 \#define A_NORETURN\n\
 \#define A_PURE\n\
 \#define A_UNUSED\n\
 \#define A_STD\n\
 \#endif\n\
 \\n\
 \// these should be enabled with newer versions of gcc\n\
 \#define A_HOT\n\
 \#define A_COLD\n\
 \#define A_FALIGNED\n\
 \\n\
 \#endif\n\
 \"#

-- | Generated from rts\/rts\/rts_support.c
{-# NOINLINE rts_support_c #-}
rts_support_c :: ByteString
rts_support_c = unsafePerformIO $ unsafePackAddress "\
 \#include <locale.h>\n\
 \#include <stdio.h>\n\
 \#include <stdlib.h>\n\
 \\n\
 \#include \"HsFFI.h\"\n\
 \#include \"rts/gc.h\"\n\
 \#include \"rts/profile.h\"\n\
 \#include \"rts/rts_support.h\"\n\
 \\n\
 \jmp_buf jhc_uncaught;\n\
 \int jhc_argc;\n\
 \char **jhc_argv;\n\
 \char *jhc_progname;\n\
 \\n\
 \#ifdef __WIN32__\n\
 \A_UNUSED char *jhc_options_os =  \"mingw32\";\n\
 \A_UNUSED char *jhc_options_arch = \"i386\";\n\
 \#elif defined(__ARM_EABI__)\n\
 \A_UNUSED char *jhc_options_os =  \"nds\";\n\
 \A_UNUSED char *jhc_options_arch = \"ARM\";\n\
 \#else\n\
 \A_UNUSED char *jhc_options_os = \"(unknown os)\";\n\
 \A_UNUSED char *jhc_options_arch = \"(unknown arch)\";\n\
 \#endif\n\
 \\n\
 \void\n\
 \hs_set_argv(int argc, char *argv[])\n\
 \{\n\
 \        jhc_argc = argc - 1;\n\
 \        jhc_argv = argv + 1;\n\
 \        jhc_progname = argv[0];\n\
 \}\n\
 \\n\
 \void A_NORETURN A_UNUSED A_COLD\n\
 \jhc_exit(int n)\n\
 \{\n\
 \        fflush(stdout);\n\
 \        jhc_print_profile();\n\
 \        exit(n);\n\
 \}\n\
 \\n\
 \void  A_NORETURN A_UNUSED  A_COLD\n\
 \jhc_error(char *s)\n\
 \{\n\
 \        fflush(stdout);\n\
 \        fputs(s, stderr);\n\
 \        fputs(\"\\n\", stderr);\n\
 \        jhc_exit(1);\n\
 \}\n\
 \\n\
 \void  A_NORETURN A_UNUSED  A_COLD\n\
 \jhc_case_fell_off(int n)\n\
 \{\n\
 \        fflush(stdout);\n\
 \        fprintf(stderr, \"\\n%s:%i: case fell off\\n\", __FILE__, n);\n\
 \        abort();\n\
 \}\n\
 \\n\
 \void jhc_hs_init(void);\n\
 \\n\
 \static int hs_init_count;\n\
 \void\n\
 \hs_init(int *argc, char **argv[])\n\
 \{\n\
 \        if (!hs_init_count++) {\n\
 \                jhc_alloc_init();\n\
 \                jhc_hs_init();\n\
 \                hs_set_argv(*argc, *argv);\n\
 \#if JHC_isPosix\n\
 \                struct utsname jhc_utsname;\n\
 \                if (!uname(&jhc_utsname)) {\n\
 \                        jhc_options_arch = jhc_utsname.machine;\n\
 \                        jhc_options_os   = jhc_utsname.sysname;\n\
 \                }\n\
 \#endif\n\
 \                setlocale(LC_ALL, \"\");\n\
 \        }\n\
 \}\n\
 \\n\
 \void\n\
 \hs_exit(void)\n\
 \{\n\
 \        if (!hs_init_count) {\n\
 \                fprintf(stderr, \"hs_exit() called before hs_init()\\n\");\n\
 \                abort();\n\
 \        }\n\
 \        if (!--hs_init_count) {\n\
 \                jhc_alloc_fini();\n\
 \                jhc_exit(0);\n\
 \        }\n\
 \}\n\
 \"#

-- | Generated from rts\/rts\/rts_support.h
{-# NOINLINE rts_support_h #-}
rts_support_h :: ByteString
rts_support_h = unsafePerformIO $ unsafePackAddress "\
 \#ifndef RTS_SUPPORT_H\n\
 \#define RTS_SUPPORT_H\n\
 \\n\
 \#include <setjmp.h>\n\
 \#include \"rts/cdefs.h\"\n\
 \\n\
 \extern jmp_buf jhc_uncaught;\n\
 \A_UNUSED extern char *jhc_options_os;\n\
 \A_UNUSED extern char *jhc_options_arch;\n\
 \extern int jhc_argc;\n\
 \extern char **jhc_argv;\n\
 \extern char *jhc_progname;\n\
 \\n\
 \extern char jhc_c_compile[];\n\
 \extern char jhc_command[];\n\
 \extern char jhc_version[];\n\
 \\n\
 \void A_NORETURN A_UNUSED A_COLD jhc_exit(int n);\n\
 \void A_NORETURN A_UNUSED A_COLD jhc_error(char *s);\n\
 \void A_NORETURN A_UNUSED A_COLD jhc_case_fell_off(int n);\n\
 \\n\
 \#define jhc_setjmp(jb) setjmp(*(jb))\n\
 \#define jhc_longjmp(jb) longjmp(*(jb),1)\n\
 \\n\
 \#define prim_umaxbound(t) ((t)~((t)0))\n\
 \#define prim_maxbound(t) ((t)(~((t)1 << (sizeof(t)*CHAR_BIT - 1))))\n\
 \#define prim_minbound(t) ((t)(((t)1 << (sizeof(t)*CHAR_BIT - 1))))\n\
 \\n\
 \#endif\n\
 \"#

-- | Generated from rts\/rts\/gc.h
{-# NOINLINE gc_h #-}
gc_h :: ByteString
gc_h = unsafePerformIO $ unsafePackAddress "\
 \#ifndef JHC_GC_H\n\
 \#define JHC_GC_H\n\
 \\n\
 \#define _JHC_GC_NONE   0\n\
 \#define _JHC_GC_JGC    1\n\
 \#define _JHC_GC_BOEHM  2\n\
 \#define _JHC_GC_REGION 3\n\
 \\n\
 \#ifndef _JHC_GC\n\
 \#define _JHC_GC _JHC_GC_NONE\n\
 \#endif\n\
 \\n\
 \void jhc_alloc_init(void);\n\
 \void jhc_alloc_fini(void);\n\
 \\n\
 \#include \"rts/gc_none.h\"\n\
 \#include \"rts/gc_jgc.h\"\n\
 \\n\
 \#endif\n\
 \"#

-- | Generated from rts\/rts\/gc_none.c
{-# NOINLINE gc_none_c #-}
gc_none_c :: ByteString
gc_none_c = unsafePerformIO $ unsafePackAddress "\
 \#include <stdio.h>\n\
 \#include <stdlib.h>\n\
 \\n\
 \#include \"rts/gc.h\"\n\
 \#include \"rts/profile.h\"\n\
 \\n\
 \#if _JHC_GC == _JHC_GC_BOEHM\n\
 \\n\
 \void hs_perform_gc(void)\n\
 \{\n\
 \        GC_gcollect();\n\
 \}\n\
 \\n\
 \void jhc_alloc_init(void)\n\
 \{\n\
 \        GC_INIT();\n\
 \}\n\
 \void jhc_alloc_fini(void) { }\n\
 \\n\
 \#elif _JHC_GC == _JHC_GC_NONE\n\
 \\n\
 \// memory allocated in 1MB chunks.\n\
 \#define JHC_MEM_CHUNK_SIZE (1 << 20)\n\
 \\n\
 \static char initial_chunk[JHC_MEM_CHUNK_SIZE];\n\
 \\n\
 \static void *jhc_current_chunk = initial_chunk;\n\
 \static unsigned mem_chunks, mem_offset;\n\
 \\n\
 \void jhc_alloc_init(void) {}\n\
 \\n\
 \void\n\
 \jhc_alloc_fini(void)\n\
 \{\n\
 \        if (_JHC_PROFILE) {\n\
 \                fprintf(stderr, \"Memory Allocated: %u bytes\\n\", (JHC_MEM_CHUNK_SIZE * (mem_chunks)) + mem_offset);\n\
 \                print_alloc_size_stats();\n\
 \        }\n\
 \}\n\
 \\n\
 \static void\n\
 \jhc_malloc_grow(void)\n\
 \{\n\
 \        void *c = malloc(JHC_MEM_CHUNK_SIZE);\n\
 \        if (!c) {\n\
 \                fputs(\"Out of memory!\\n\", stderr);\n\
 \                abort();\n\
 \        }\n\
 \        mem_chunks++;\n\
 \        jhc_current_chunk = c;\n\
 \        mem_offset = 0;\n\
 \}\n\
 \\n\
 \#define M_ALIGN(a,n) ((n) - 1 + ((a) - ((n) - 1) % (a)))\n\
 \\n\
 \static inline void *A_MALLOC\n\
 \jhc_malloc_basic(size_t n)\n\
 \{\n\
 \        n = M_ALIGN(sizeof(void *), n);\n\
 \        if (n > (JHC_MEM_CHUNK_SIZE - mem_offset))\n\
 \                jhc_malloc_grow();\n\
 \        void *ret = jhc_current_chunk + mem_offset;\n\
 \        mem_offset += n;\n\
 \        return ret;\n\
 \}\n\
 \\n\
 \#if _JHC_DEBUG\n\
 \\n\
 \void *A_MALLOC\n\
 \jhc_malloc_debug(size_t n, int line, int atomic)\n\
 \{\n\
 \        alloc_count(n, atomic);\n\
 \        void *ret = jhc_malloc_basic(n + sizeof(uintptr_t));\n\
 \        *((uintptr_t *)ret) = line;\n\
 \        return ret + sizeof(uintptr_t);\n\
 \}\n\
 \\n\
 \#else\n\
 \\n\
 \void *A_MALLOC\n\
 \jhc_malloc(size_t n)\n\
 \{\n\
 \        alloc_count(n, 0);\n\
 \        return jhc_malloc_basic(n);\n\
 \}\n\
 \\n\
 \#undef jhc_malloc_atomic\n\
 \void *A_MALLOC\n\
 \jhc_malloc_atomic(size_t n)\n\
 \{\n\
 \        alloc_count(n, 1);\n\
 \        return jhc_malloc_basic(n);\n\
 \}\n\
 \\n\
 \#endif\n\
 \\n\
 \#endif\n\
 \"#

-- | Generated from rts\/rts\/gc_none.h
{-# NOINLINE gc_none_h #-}
gc_none_h :: ByteString
gc_none_h = unsafePerformIO $ unsafePackAddress "\
 \#ifndef GC_NONE_H\n\
 \#define GC_NONE_H\n\
 \\n\
 \#include <stddef.h>\n\
 \#include \"rts/cdefs.h\"\n\
 \\n\
 \#define jhc_malloc_sanity(p,t) (1)\n\
 \\n\
 \#if _JHC_GC == _JHC_GC_BOEHM\n\
 \\n\
 \#include \"rts/profile.h\"\n\
 \\n\
 \#include <gc/gc.h>\n\
 \\n\
 \#define jhc_malloc GC_malloc\n\
 \#define jhc_malloc_atomic GC_malloc_atomic\n\
 \\n\
 \#elif _JHC_GC == _JHC_GC_NONE\n\
 \\n\
 \#if _JHC_DEBUG\n\
 \void *A_MALLOC jhc_malloc_debug(size_t n, int line, int atomic);\n\
 \#define jhc_malloc(n) jhc_malloc_debug(n,__LINE__,0)\n\
 \#define jhc_malloc_atomic(n) jhc_malloc_debug(n,__LINE__,1)\n\
 \#else\n\
 \void *A_MALLOC jhc_malloc(size_t n);\n\
 \void *A_MALLOC jhc_malloc_atomic(size_t n);\n\
 \#endif\n\
 \\n\
 \#endif\n\
 \\n\
 \#endif\n\
 \"#

-- | Generated from rts\/rts\/jhc_rts.c
{-# NOINLINE jhc_rts_c #-}
jhc_rts_c :: ByteString
jhc_rts_c = unsafePerformIO $ unsafePackAddress "\
 \/*@Internals\n\
 \\n\
 \# The Run Time System\n\
 \\n\
 \Jhc is very minimalist in that it does not have a precompiled run time system,\n\
 \but rather generates what is needed as part of the compilation process.\n\
 \However, back ends do have specific run-time representations of data, which can\n\
 \be affected by things like the choice of garbage collector. The following\n\
 \describes the general layout for the C based back-ends, but compiler options\n\
 \such as garbage collection method or whether we do full program analysis, will\n\
 \affect which features are used and whether certain optimized layouts are\n\
 \possible.\n\
 \\n\
 \Unboxed values directly translate to values in the target language, an unboxed\n\
 \Int will translate directly into an 'int' as an argument and an unboxed pointer\n\
 \will be a raw pointer. Unboxed values have no special interpretation and are\n\
 \_not_ followed by the garbage collector. If the target language does not\n\
 \support a feature such as multiple return values, it will have to be simulated.\n\
 \It would not be wrong to think of Grin code that only deals with unboxed values\n\
 \to be isomorphic to C-- or C augmented with multiple return values.\n\
 \\n\
 \Boxed values have a standard representation and can be followed. Unlike some\n\
 \other implementation, being boxed does not imply the object is located on the\n\
 \heap. It may be on the stack, heap, or even embedded within the smart pointer\n\
 \itself. Being boxed only means that the object may be represented by a smart\n\
 \pointer, which may or may not actually be a pointer in the traditional sense.\n\
 \\n\
 \A boxed value in jhc is represented by a 'smart pointer' of c type sptr_t. a\n\
 \smart pointer is the size of a native pointer, but can take on different roles\n\
 \depending on a pair of tag bits, called the ptype.\n\
 \\n\
 \smart pointers take on a general form as follows:\n\
 \\n\
 \    -------------------------\n\
 \    |    payload        | GL|\n\
 \    -------------------------\n\
 \\n\
 \      G - if set, then the garbage collector should not treat value as a pointer to be followed\n\
 \      L - lazy, this bit being set means the value is potentially not in WHNF\n\
 \\n\
 \A sptr_t on its own in the wild can only take on one of the following forms:\n\
 \\n\
 \    -------------------------\n\
 \    |    whnf raw value | 10|\n\
 \    -------------------------\n\
 \\n\
 \    -------------------------\n\
 \    |    whnf location  | 00|\n\
 \    -------------------------\n\
 \\n\
 \WHNF stands for 'Weak Head Normal Form' and means that the value is not a\n\
 \suspended function and hence not a pointer to a thunk. It may be directly\n\
 \examined and need not be evaluated. wptr_t is an alias for sptr_t that is\n\
 \guarenteed to be of one of the above forms. It is used to improve safety for\n\
 \when we can statically know that a value is WHNF and hence we can skip the\n\
 \expensive 'eval'.\n\
 \\n\
 \The difference between the raw value and the whnf location is that the first\n\
 \contains uninterpreted bits, while the second is a pointer to a location on the\n\
 \heap or stack and hence the garbage collector should follow it. The format of\n\
 \the memory pointed to by the whnf location is unspecified and dependent on the\n\
 \actual type being represented.\n\
 \\n\
 \Partial (unsaturated) applications are normal WHNF values. Saturated\n\
 \applications which may be 'eval'ed and updated are called thunks and must not\n\
 \be pointed to by WHNF pointers. Their representation follows.\n\
 \\n\
 \    -------------------------\n\
 \    |   lazy location   | 01|\n\
 \    -------------------------\n\
 \\n\
 \A lazy location points to either a thunk, or a redirection to a WHNF value. A\n\
 \lazy location is always a pointer to an allocated block of memory which always\n\
 \begins with a restricted smart pointer. This restricted smart pointer is represented by\n\
 \the C type alias 'fptr_t'. fptr_t's only occur as the first entry in a lazy\n\
 \location, they never are passed around as objects in their own right.\n\
 \\n\
 \A fptr_t may be a whnf value or a code pointer. If a fptr_t is a whnf value (of one of\n\
 \the two forms given above) then it is called a redirection, the lazy location should be\n\
 \treated exactly as if it were the whnf given. This is used to redirect an evaluated\n\
 \thunk to its computed value.\n\
 \\n\
 \A fptr_t may also be a 'code pointer' in which case the lazy location is called\n\
 \a thunk. A code pointer is a pointer to executable machine code that evaluates\n\
 \a closure and returns a wptr_t, the returned wptr_t is then generally written\n\
 \over the code pointer, turning the thunk into a redirection. It is the\n\
 \responsibility of the code pointed to to perform this redirection.\n\
 \\n\
 \    -------------------------\n\
 \    |    code pointer   | 11|\n\
 \    -------------------------\n\
 \    |     data ...          |\n\
 \\n\
 \When debugging, the special code pointer BLACK_HOLE is also sometimes stored in\n\
 \a fptr_t to detect certain run-time errors.\n\
 \\n\
 \Note that unlike other implementations, a fptr_t may _not_ be another lazy\n\
 \location. you can not have chained redirections, a redirection is always a\n\
 \redirection to a whnf value.\n\
 \\n\
 \    sptr_t - a tagged smart pointer, may contain a whnf value or a lazy location.\n\
 \    wptr_t - a tagged smart pointer that contains a whnf value (either raw or a location)\n\
 \    fptr_t - a tagged smart pointer, may contain a whnf value indicating a redirection, or a code pointer indicating a thunk.\n\
 \\n\
 \*/\n\
 \\n\
 \#include \"jhc_rts_header.h\"\n\
 \\n\
 \#if _JHC_GC == _JHC_GC_JGC\n\
 \typedef wptr_t (*eval_fn)(gc_t gc, node_t *node) A_STD;\n\
 \#else\n\
 \typedef wptr_t (*eval_fn)(node_t *node) A_STD;\n\
 \#endif\n\
 \\n\
 \// like eval but you know the target is in WHNF or is a already evaluated indirection\n\
 \static inline wptr_t A_STD A_UNUSED  A_HOT\n\
 \follow(sptr_t s)\n\
 \{\n\
 \        assert(jhc_valid_lazy(s));\n\
 \        if (IS_LAZY(s)) {\n\
 \                sptr_t h = (sptr_t)(GETHEAD(FROM_SPTR(s)));\n\
 \                assert(!IS_LAZY(h));\n\
 \                return (wptr_t)h;\n\
 \        }\n\
 \        return (wptr_t)s;\n\
 \}\n\
 \\n\
 \wptr_t A_STD A_UNUSED  A_HOT\n\
 \#if _JHC_GC == _JHC_GC_JGC\n\
 \eval(gc_t gc, sptr_t s)\n\
 \#else\n\
 \eval(sptr_t s)\n\
 \#endif\n\
 \{\n\
 \        assert(jhc_valid_lazy(s));\n\
 \        if (IS_LAZY(s)) {\n\
 \                assert(GET_PTYPE(s) == P_LAZY);\n\
 \                void *ds = FROM_SPTR(s);\n\
 \                sptr_t h = (sptr_t)(GETHEAD(ds));\n\
 \                assert((fptr_t)h != BLACK_HOLE);\n\
 \                if (IS_LAZY(h)) {\n\
 \                        eval_fn fn = (eval_fn)FROM_SPTR(h);\n\
 \                        assert(GET_PTYPE(h) == P_FUNC);\n\
 \#if _JHC_DEBUG\n\
 \                        GETHEAD(ds) = BLACK_HOLE;\n\
 \#endif\n\
 \                        fn = (eval_fn)SET_THUMB_BIT(fn);\n\
 \#if _JHC_GC == _JHC_GC_JGC\n\
 \                        wptr_t r = (*fn)(gc, NODEP(ds));\n\
 \#else\n\
 \                        wptr_t r = (*fn)(NODEP(ds));\n\
 \#endif\n\
 \#if _JHC_DEBUG\n\
 \                        assert(GETHEAD(ds) != BLACK_HOLE);\n\
 \#endif\n\
 \                        return r;\n\
 \                }\n\
 \                return (wptr_t)h;\n\
 \        }\n\
 \        assert(jhc_valid_whnf((wptr_t)s));\n\
 \        return (wptr_t)s;\n\
 \}\n\
 \\n\
 \#if _JHC_STANDALONE\n\
 \int\n\
 \main(int argc, char *argv[])\n\
 \{\n\
 \        hs_init(&argc, &argv);\n\
 \        if (jhc_setjmp(&jhc_uncaught))\n\
 \                jhc_error(\"Uncaught Exception\");\n\
 \        else\n\
 \                _amain();\n\
 \        hs_exit();\n\
 \        return 0;\n\
 \}\n\
 \#endif\n\
 \"#

-- | Generated from rts\/rts\/jhc_rts.h
{-# NOINLINE jhc_rts_h #-}
jhc_rts_h :: ByteString
jhc_rts_h = unsafePerformIO $ unsafePackAddress "\
 \#ifndef JHC_RTS_H\n\
 \#define JHC_RTS_H\n\
 \\n\
 \#include \"rts/profile.h\"\n\
 \#include \"rts/gc.h\"\n\
 \\n\
 \struct sptr;\n\
 \struct wptr;\n\
 \struct fptr;\n\
 \\n\
 \// we use dummy structs here so the compiler will catch any attempt\n\
 \// to use one type in anothers place\n\
 \typedef struct sptr *sptr_t;\n\
 \typedef struct sptr *wptr_t;\n\
 \typedef struct fptr *fptr_t;\n\
 \typedef uintptr_t     what_t;\n\
 \\n\
 \typedef struct node {\n\
 \        fptr_t head;\n\
 \        sptr_t rest[];\n\
 \} A_MAYALIAS node_t;\n\
 \\n\
 \typedef struct dnode {\n\
 \        what_t what;\n\
 \        sptr_t rest[];\n\
 \} A_MAYALIAS dnode_t;\n\
 \\n\
 \#define P_WHNF  0x0\n\
 \#define P_LAZY  0x1\n\
 \#define P_VALUE 0x2\n\
 \#define P_FUNC  0x3\n\
 \\n\
 \#define IS_LAZY(x)     (bool)(((uintptr_t)(x)) & 0x1)\n\
 \#define IS_PTR(x)      (bool)(!(((uintptr_t)(x)) & 0x2))\n\
 \\n\
 \#define FROM_SPTR(x)   (typeof (x))((uintptr_t)(x) & ~0x3)  // remove a ptype from a smart pointer\n\
 \#define GET_PTYPE(x)   ((uintptr_t)(x) & 0x3)               // return the ptype associated with a smart pointer\n\
 \#define TO_SPTR(t,x)   (typeof (x))((uintptr_t)(x) | (t))   // attach a ptype to a smart pointer\n\
 \#define TO_SPTR_C(t,x) (typeof (x))((uintptr_t)(x) + (t))   // attach a ptype to a smart pointer, suitable for use by constant initialializers\n\
 \\n\
 \#define GETHEAD(x)   (NODEP(x)->head)\n\
 \#define NODEP(x)     ((node_t *)(x))\n\
 \#define DNODEP(x)    ((dnode_t *)(x))\n\
 \\n\
 \#define MKLAZY(fn)    TO_SPTR(P_LAZY,(sptr_t)fn)\n\
 \#define MKLAZY_C(fn)  TO_SPTR_C(P_LAZY,(sptr_t)fn)\n\
 \#define TO_FPTR(fn)   TO_SPTR_C(P_FUNC,(fptr_t)fn)\n\
 \\n\
 \#define RAW_SET_F(n)   ((wptr_t)(((intptr_t)(n) << 2) | P_VALUE))\n\
 \#define RAW_SET_UF(n)  ((wptr_t)(((uintptr_t)(n) << 2) | P_VALUE))\n\
 \#define RAW_GET_F(n)   ((intptr_t)(n) >> 2)\n\
 \#define RAW_GET_UF(n)  ((uintptr_t)(n) >> 2)\n\
 \\n\
 \#define RAW_SET_16(w)  (wptr_t)(((uintptr_t)(w) << 16) | P_VALUE)\n\
 \#define RAW_GET_16(n)  ((intptr_t)(n) >> 16)\n\
 \#define RAW_GET_U16(n) ((uintptr_t)(n) >> 16)\n\
 \\n\
 \// demote is always safe, we must only promote when we know the argument is a WHNF\n\
 \#define PROMOTE(n)   ((wptr_t)(n))\n\
 \#define DEMOTE(n)    ((sptr_t)(n))\n\
 \\n\
 \#define FETCH_TAG(x)      RAW_GET_U16(IS_PTR(x) ? FETCH_MEM_TAG(x) : (what_t)(x))\n\
 \#define FETCH_RAW_TAG(x)  RAW_GET_U16(x)\n\
 \#define SET_RAW_TAG(x)    RAW_SET_16(x)\n\
 \#define FETCH_MEM_TAG(x)  (DNODEP(x)->what)\n\
 \#define SET_MEM_TAG(x,v)  (DNODEP(x)->what = (what_t)RAW_SET_16(v))\n\
 \\n\
 \#define BLACK_HOLE TO_FPTR(0xDEADBEE0)\n\
 \\n\
 \wptr_t A_STD\n\
 \#if _JHC_GC == _JHC_GC_JGC\n\
 \eval(gc_t gc, sptr_t s);\n\
 \#else\n\
 \eval(sptr_t s);\n\
 \#endif\n\
 \\n\
 \// both promote and demote evaluate to nothing when debugging is not enabled\n\
 \// otherwise, they check that their arguments are in the correct form.\n\
 \#if _JHC_DEBUG\n\
 \wptr_t A_STD promote(sptr_t s);\n\
 \sptr_t A_STD demote(wptr_t s);\n\
 \void   A_STD update(void *, wptr_t);\n\
 \#else\n\
 \#define promote(x) PROMOTE(x)\n\
 \#define demote(x) DEMOTE(x)\n\
 \inline static void update(void *t, wptr_t n)\n\
 \{\n\
 \        GETHEAD(t) = (fptr_t)n;\n\
 \}\n\
 \#endif\n\
 \\n\
 \#if _JHC_DEBUG && _JHC_GC == _JHC_GC_JGC\n\
 \bool jhc_valid_whnf(wptr_t s);\n\
 \bool jhc_valid_lazy(sptr_t s);\n\
 \#else\n\
 \#define jhc_valid_whnf(x) true\n\
 \#define jhc_valid_lazy(x) true\n\
 \#endif\n\
 \\n\
 \#endif\n\
 \\n\
 \/*\n\
 \ * Detail:\n\
 \ * http://communities.mentor.com/community/cs/archives/arm-gnu/msg01904.html\n\
 \ */\n\
 \#ifdef _JHC_ARM_STAY_IN_THUMB_MODE\n\
 \#define SET_THUMB_BIT(fn)    TO_SPTR(0x1,(sptr_t)fn)\n\
 \#else\n\
 \#define SET_THUMB_BIT(fn)    (fn)\n\
 \#endif\n\
 \"#

-- | Generated from rts\/lib\/lib_cbits.c
{-# NOINLINE lib_cbits_c #-}
lib_cbits_c :: ByteString
lib_cbits_c = unsafePerformIO $ unsafePackAddress "\
 \/*\n\
 \ * this file contains C only needed to help support the standard libraries\n\
 \ */\n\
 \\n\
 \#include <stdio.h>\n\
 \\n\
 \#include \"HsFFI.h\"\n\
 \#include \"rts/cdefs.h\"\n\
 \\n\
 \HsInt jhc_stdrnd[2] A_UNUSED = { 1, 1 };\n\
 \HsInt jhc_data_unique A_UNUSED;\n\
 \\n\
 \HsBool A_UNUSED\n\
 \jhc_wait_for_input(FILE *f, HsInt timeout)\n\
 \{\n\
 \#if JHC_isPosix\n\
 \        fd_set fds;\n\
 \        FD_ZERO(&fds);\n\
 \        FD_SET(fileno(f), &fds);\n\
 \        struct timeval to = { 0, timeout * 1000 };\n\
 \        int retval = select(1, &fds, NULL, &fds, &to);\n\
 \        if (retval)\n\
 \                return HS_BOOL_TRUE;\n\
 \        else\n\
 \                return HS_BOOL_FALSE;\n\
 \#else\n\
 \        return HS_BOOL_FALSE;\n\
 \#endif\n\
 \}\n\
 \\n\
 \uint32_t\n\
 \jhc_hash32(uint32_t key)\n\
 \{\n\
 \        int c2 = 0x27d4eb2d;    // a prime or an odd constant\n\
 \        key = (key ^ 61) ^ (key >> 16);\n\
 \        key = key + (key << 3);\n\
 \        key = key ^ (key >> 4);\n\
 \        key = key * c2;\n\
 \        key = key ^ (key >> 15);\n\
 \        return key;\n\
 \}\n\
 \\n\
 \uint64_t\n\
 \jhc_hash64(uint64_t key)\n\
 \{\n\
 \        key = (~key) + (key << 21);     // key = (key << 21) - key - 1;\n\
 \        key = key ^ (key >> 24);\n\
 \        key = (key + (key << 3)) + (key << 8);  // key * 265\n\
 \        key = key ^ (key >> 14);\n\
 \        key = (key + (key << 2)) + (key << 4);  // key * 21\n\
 \        key = key ^ (key >> 28);\n\
 \        key = key + (key << 31);\n\
 \        return key;\n\
 \}\n\
 \\n\
 \uintptr_t\n\
 \jhc_hashptr(uintptr_t key)\n\
 \{\n\
 \        if (sizeof(uintptr_t) == sizeof(uint32_t)) {\n\
 \                return (uintptr_t) jhc_hash32((uint32_t) key);\n\
 \        } else {\n\
 \                return (uintptr_t) jhc_hash64((uint64_t) key);\n\
 \        }\n\
 \}\n\
 \"#

-- | Generated from rts\/jhc_rts_header.h
{-# NOINLINE jhc_rts_header_h #-}
jhc_rts_header_h :: ByteString
jhc_rts_header_h = unsafePerformIO $ unsafePackAddress "\
 \#include <assert.h>\n\
 \#include <errno.h>\n\
 \#include <float.h>\n\
 \#include <limits.h>\n\
 \#include <locale.h>\n\
 \#include <math.h>\n\
 \#include <stdio.h>\n\
 \#include <stdlib.h>\n\
 \#include <string.h>\n\
 \#include <time.h>\n\
 \#include <unistd.h>\n\
 \#include <wchar.h>\n\
 \#include <setjmp.h>\n\
 \#ifndef __WIN32__\n\
 \#ifdef __ARM_EABI__\n\
 \#include <malloc.h>\n\
 \#else\n\
 \#include <sys/select.h>\n\
 \#include <sys/utsname.h>\n\
 \#endif\n\
 \#include <sys/times.h>\n\
 \#include <sys/types.h>\n\
 \#include <sys/param.h>\n\
 \#else\n\
 \#include <malloc.h>\n\
 \#endif\n\
 \\n\
 \#include \"HsFFI.h\"\n\
 \#include \"sys/wsize.h\"\n\
 \#include \"rts/cdefs.h\"\n\
 \\n\
 \#ifndef _JHC_DEBUG\n\
 \#ifdef NDEBUG\n\
 \#define _JHC_DEBUG 0\n\
 \#else\n\
 \#define _JHC_DEBUG 1\n\
 \#endif\n\
 \#endif\n\
 \\n\
 \#ifndef _JHC_STANDALONE\n\
 \#define _JHC_STANDALONE 1\n\
 \#endif\n\
 \\n\
 \#ifndef JHC_STATUS\n\
 \#define JHC_STATUS 0\n\
 \#endif\n\
 \\n\
 \#ifdef __WIN32__\n\
 \#define JHC_isWindows   1\n\
 \#define JHC_isBigEndian 0\n\
 \#else\n\
 \#define JHC_isWindows 0\n\
 \#define JHC_isBigEndian (__BYTE_ORDER == __BIG_ENDIAN)\n\
 \#endif\n\
 \\n\
 \#ifdef __ARM_EABI__\n\
 \#define JHC_isRawHardware 1\n\
 \#else\n\
 \#define JHC_isRawHardware 0\n\
 \#endif\n\
 \\n\
 \#define JHC_isPosix (!JHC_isWindows && !JHC_isRawHardware)\n\
 \\n\
 \// the program will provide the following\n\
 \void _amain(void);\n\
 \void jhc_hs_init(void);\n\
 \extern const void *const nh_stuff[];\n\
 \\n\
 \#include \"rts/profile.h\"\n\
 \#include \"rts/rts_support.h\"\n\
 \#include \"rts/gc.h\"\n\
 \#include \"rts/jhc_rts.h\"\n\
 \#include \"lib/lib_cbits.h\"\n\
 \"#

-- | Generated from rts\/lib\/lib_cbits.h
{-# NOINLINE lib_cbits_h #-}
lib_cbits_h :: ByteString
lib_cbits_h = unsafePerformIO $ unsafePackAddress "\
 \#ifndef LIB_CBITS_H\n\
 \#define LIB_CBITS_H\n\
 \\n\
 \#include \"HsFFI.h\"\n\
 \struct FILE;\n\
 \\n\
 \extern HsInt jhc_stdrnd[2];\n\
 \extern HsInt jhc_data_unique;\n\
 \HsBool jhc_wait_for_input(FILE *f, HsInt timeout);\n\
 \\n\
 \#ifdef __WIN32__\n\
 \#define getchar_unlocked() getchar()\n\
 \#define putchar_unlocked(x) putchar(x)\n\
 \#define getc_unlocked(x) getc(x)\n\
 \#define putc_unlocked(x,y) putc(x,y)\n\
 \#endif\n\
 \\n\
 \inline static int A_UNUSED\n\
 \jhc_utf8_getchar(void)\n\
 \{\n\
 \        return getchar_unlocked();\n\
 \}\n\
 \\n\
 \inline static int A_UNUSED\n\
 \jhc_utf8_getc(FILE *f)\n\
 \{\n\
 \        return getc_unlocked(f);\n\
 \}\n\
 \\n\
 \inline static int A_UNUSED\n\
 \jhc_utf8_putchar(int ch)\n\
 \{\n\
 \        return putchar_unlocked(ch);\n\
 \}\n\
 \\n\
 \inline static int A_UNUSED\n\
 \jhc_utf8_putc(int ch, FILE *f)\n\
 \{\n\
 \        return putc_unlocked(ch, f);\n\
 \}\n\
 \\n\
 \#endif\n\
 \"#

-- | Generated from rts\/rts\/gc_jgc_internal.h
{-# NOINLINE gc_jgc_internal_h #-}
gc_jgc_internal_h :: ByteString
gc_jgc_internal_h = unsafePerformIO $ unsafePackAddress "\
 \#ifndef GC_JGC_INTERNAL_H\n\
 \#define GC_JGC_INTERNAL_H\n\
 \\n\
 \#include \"rts/gc_jgc.h\"\n\
 \#include \"sys/bitarray.h\"\n\
 \#include \"sys/queue.h\"\n\
 \\n\
 \#if _JHC_GC == _JHC_GC_JGC\n\
 \\n\
 \struct s_arena {\n\
 \        struct s_megablock *current_megablock;\n\
 \        SLIST_HEAD(, s_block) free_blocks;\n\
 \        unsigned block_used;\n\
 \        unsigned block_threshold;\n\
 \        SLIST_HEAD(, s_cache) caches;\n\
 \        SLIST_HEAD(, s_block) monolithic_blocks;\n\
 \        SLIST_HEAD(, s_megablock) megablocks;\n\
 \        unsigned number_gcs;    // number of garbage collections\n\
 \        unsigned number_allocs; // number of allocations since last garbage collection\n\
 \};\n\
 \\n\
 \struct s_megablock {\n\
 \        void *base;\n\
 \        unsigned next_free;\n\
 \        SLIST_ENTRY(s_megablock) next;\n\
 \};\n\
 \\n\
 \struct s_block {\n\
 \        SLIST_ENTRY(s_block) link;\n\
 \        unsigned char flags;  // defined in rts/constants.h\n\
 \        unsigned char color;  // offset in words to first entry.\n\
 \        union {\n\
 \                // A normal block.\n\
 \                struct {\n\
 \                        unsigned char num_ptrs;\n\
 \                        unsigned char size;\n\
 \                        unsigned short num_free;\n\
 \                        unsigned short next_free;\n\
 \                } pi;\n\
 \                // A monolithic block.\n\
 \                struct {\n\
 \                        unsigned num_ptrs;\n\
 \                } m;\n\
 \        } u;\n\
 \        bitarray_t used[];\n\
 \};\n\
 \\n\
 \struct s_cache {\n\
 \        SLIST_ENTRY(s_cache) next;\n\
 \        SLIST_HEAD(, s_block) blocks;\n\
 \        SLIST_HEAD(, s_block) full_blocks;\n\
 \        unsigned char color;\n\
 \        unsigned char size;\n\
 \        unsigned char num_ptrs;\n\
 \        unsigned char flags;\n\
 \        unsigned short num_entries;\n\
 \        struct s_arena *arena;\n\
 \#if _JHC_PROFILE\n\
 \        unsigned allocations;\n\
 \#endif\n\
 \};\n\
 \#endif\n\
 \#endif\n\
 \"#



