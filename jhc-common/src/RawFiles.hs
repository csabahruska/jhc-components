{-# LANGUAGE TemplateHaskell #-}
module RawFiles where

import Data.ByteString
import Data.FileEmbed

bitarray_h        = $(embedFile "embed/sys/bitarray.h") :: ByteString
queue_h           = $(embedFile "embed/sys/queue.h") :: ByteString
wsize_h           = $(embedFile "embed/sys/wsize.h") :: ByteString

lib_cbits_c       = $(embedFile "embed/lib/lib_cbits.c") :: ByteString
lib_cbits_h       = $(embedFile "embed/lib/lib_cbits.h") :: ByteString

cdefs_h           = $(embedFile "embed/rts/cdefs.h") :: ByteString
constants_h       = $(embedFile "embed/rts/constants.h") :: ByteString
gc_h              = $(embedFile "embed/rts/gc.h") :: ByteString
gc_jgc_c          = $(embedFile "embed/rts/gc_jgc.c") :: ByteString
gc_jgc_h          = $(embedFile "embed/rts/gc_jgc.h") :: ByteString
gc_jgc_internal_h = $(embedFile "embed/rts/gc_jgc_internal.h") :: ByteString
gc_none_c         = $(embedFile "embed/rts/gc_none.c") :: ByteString
gc_none_h         = $(embedFile "embed/rts/gc_none.h") :: ByteString
jhc_rts_c         = $(embedFile "embed/rts/jhc_rts.c") :: ByteString
jhc_rts_h         = $(embedFile "embed/rts/jhc_rts.h") :: ByteString
profile_c         = $(embedFile "embed/rts/profile.c") :: ByteString
profile_h         = $(embedFile "embed/rts/profile.h") :: ByteString
rts_support_c     = $(embedFile "embed/rts/rts_support.c") :: ByteString
rts_support_h     = $(embedFile "embed/rts/rts_support.h") :: ByteString
stableptr_c       = $(embedFile "embed/rts/stableptr.c") :: ByteString

hsffi_h           = $(embedFile "embed/HsFFI.h") :: ByteString
jhc_rts_header_h  = $(embedFile "embed/jhc_rts_header.h") :: ByteString

changelog         = $(embedFile "embed/ChangeLog") :: ByteString
prelude_m4        = $(embedFile "embed/prelude.m4") :: ByteString
shortchange_txt   = $(embedFile "embed/shortchange.txt") :: ByteString
targets_ini       = $(embedFile "embed/targets.ini") :: ByteString
viaghc_hs         = $(embedFile "embed/ViaGhc.hs") :: ByteString
