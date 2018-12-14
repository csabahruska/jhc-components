{-# LANGUAGE M4 #-}
module Foreign.C.Error (
    Errno(Errno),  eOK,  e2BIG,  eACCES,  eADDRINUSE,  eADDRNOTAVAIL,  eADV,
    eAFNOSUPPORT,  eAGAIN,  eALREADY,  eBADF,  eBADMSG,  eBADRPC,  eBUSY,
    eCHILD,  eCOMM,  eCONNABORTED,  eCONNREFUSED,  eCONNRESET,  eDEADLK,
    eDESTADDRREQ,  eDIRTY,  eDOM,  eDQUOT,  eEXIST,  eFAULT,  eFBIG,  eFTYPE,
    eHOSTDOWN,  eHOSTUNREACH,  eIDRM,  eILSEQ,  eINPROGRESS,  eINTR,  eINVAL,
    eIO,  eISCONN,  eISDIR,  eLOOP,  eMFILE,  eMLINK,  eMSGSIZE,  eMULTIHOP,
    eNAMETOOLONG,  eNETDOWN,  eNETRESET,  eNETUNREACH,  eNFILE,  eNOBUFS,
    eNODATA,  eNODEV,  eNOENT,  eNOEXEC,  eNOLCK,  eNOLINK,  eNOMEM,  eNOMSG,
    eNONET,  eNOPROTOOPT,  eNOSPC,  eNOSR,  eNOSTR,  eNOSYS,  eNOTBLK,
    eNOTCONN,  eNOTDIR,  eNOTEMPTY,  eNOTSOCK,  eNOTTY,  eNXIO,  eOPNOTSUPP,
    ePERM,  ePFNOSUPPORT,  ePIPE,  ePROCLIM,  ePROCUNAVAIL,  ePROGMISMATCH,
    ePROGUNAVAIL,  ePROTO,  ePROTONOSUPPORT,  ePROTOTYPE,  eRANGE,  eREMCHG,
    eREMOTE,  eROFS,  eRPCMISMATCH,  eRREMOTE,  eSHUTDOWN,  eSOCKTNOSUPPORT,
    eSPIPE,  eSRCH,  eSRMNT,  eSTALE,  eTIME,  eTIMEDOUT,  eTOOMANYREFS,
    eTXTBSY,  eUSERS,  eWOULDBLOCK,  eXDEV,  isValidErrno,  getErrno,
    resetErrno,  errnoToIOError,  throwErrno,  throwErrnoIf,  throwErrnoIf_,
    throwErrnoIfRetry,  throwErrnoIfRetry_,  throwErrnoIfMinus1,
    throwErrnoIfMinus1_,  throwErrnoIfMinus1Retry,  throwErrnoIfMinus1Retry_,
    throwErrnoIfNull,  throwErrnoIfNullRetry,  throwErrnoIfRetryMayBlock,
    throwErrnoIfRetryMayBlock_,  throwErrnoIfMinus1RetryMayBlock,
    throwErrnoIfMinus1RetryMayBlock_,  throwErrnoIfNullRetryMayBlock,
    throwErrnoPath,  throwErrnoPathIf,  throwErrnoPathIf_,
    throwErrnoPathIfNull,  throwErrnoPathIfMinus1,  throwErrnoPathIfMinus1_
  ) where

import Jhc.Prim.C

-- move to jhc.type.c
newtype Errno = Errno CInt

-- | Yield 'True' if the given 'Errno' value is valid on the system.
-- This implies that the 'Eq' instance of 'Errno' is also system dependent
-- as it is only defined for valid values of 'Errno'.

isValidErrno               :: Errno -> Bool
isValidErrno (Errno errno)  = errno `ciNeq` -1
foreign import primitive "NEq" ciNeq :: CInt -> CInt -> Bool


-- | Get the current value of @errno@ in the current thread.
getErrno :: IO Errno
getErrno = do e <- peek _errno; return (Errno e)
foreign import ccall "errno.h &errno" _errno :: Ptr CInt


m4_define(ERR,{{foreign import primitive "const.E$1" e$1 :: Errno}})

ERR(2BIG)
ERR(ACCES)
ERR(ADDRINUSE)
ERR(ADDRNOTAVAIL)
ERR(ADV)
ERR(AFNOSUPPORT)
ERR(AGAIN)
ERR(ALREADY)
ERR(BADF)
ERR(BADMSG)
ERR(BADRPC)
ERR(BUSY)
ERR(CHILD)
ERR(COMM)
ERR(CONNABORTED)
ERR(CONNREFUSED)
ERR(CONNRESET)
ERR(DEADLK)
ERR(DESTADDRREQ)
ERR(DIRTY)
ERR(DOM)
ERR(DQUOT)
ERR(EXIST)
ERR(FAULT)
ERR(FBIG)
ERR(FTYPE)
ERR(HOSTDOWN)
ERR(HOSTUNREACH)
ERR(IDRM)
ERR(ILSEQ)
ERR(INPROGRESS)
ERR(INTR)
ERR(INVAL)
ERR(IO)
ERR(ISCONN)
ERR(ISDIR)
ERR(LOOP)
ERR(MFILE)
ERR(MLINK)
ERR(MSGSIZE)
ERR(MULTIHOP)
ERR(NAMETOOLONG)
ERR(NETDOWN)
ERR(NETRESET)
ERR(NETUNREACH)
ERR(NFILE)
ERR(NOBUFS)
ERR(NODATA)
ERR(NODEV)
ERR(NOENT)
ERR(NOEXEC)
ERR(NOLCK)
ERR(NOLINK)
ERR(NOMEM)
ERR(NOMSG)
ERR(NONET)
ERR(NOPROTOOPT)
ERR(NOSPC)
ERR(NOSR)
ERR(NOSTR)
ERR(NOSYS)
ERR(NOTBLK)
ERR(NOTCONN)
ERR(NOTDIR)
ERR(NOTEMPTY)
ERR(NOTSOCK)
ERR(NOTTY)
ERR(NXIO)
ERR(OK)
ERR(OPNOTSUPP)
ERR(PERM)
ERR(PFNOSUPPORT)
ERR(PIPE)
ERR(PROCLIM)
ERR(PROCUNAVAIL)
ERR(PROGMISMATCH)
ERR(PROGUNAVAIL)
ERR(PROTO)
ERR(PROTONOSUPPORT)
ERR(PROTOTYPE)
ERR(RANGE)
ERR(REMCHG)
ERR(REMOTE)
ERR(ROFS)
ERR(RPCMISMATCH)
ERR(RREMOTE)
ERR(SHUTDOWN)
ERR(SOCKTNOSUPPORT)
ERR(SPIPE)
ERR(SRCH)
ERR(SRMNT)
ERR(STALE)
ERR(TIME)
ERR(TIMEDOUT)
ERR(TOOMANYREFS)
ERR(TXTBSY)
ERR(USERS)
ERR(WOULDBLOCK)
ERR(XDEV)
