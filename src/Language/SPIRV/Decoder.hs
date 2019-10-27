module Language.SPIRV.Decoder(
    decodeBinary
  ) where

import Language.SPIRV.GenericDecoder
import Language.SPIRV.Types

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Bits
import Data.Int
import Data.List
import Data.Word
import Text.Printf
import qualified Data.ByteString as S


type DS a = D DecSpvSt a

data DecSpvSt =
  DecSpvSt {
    dssFlipByteOrder :: !Bool -- based on
  , dssIdBound :: !Int
  }

decodeBinary :: S.ByteString -> Either String Binary
decodeBinary = runD dBinary (DecSpvSt False maxBound)

dBinary :: DS Binary
dBinary = start
  where start = do
          magic <- dX32_LE "SPIR-V magic" id
          if magic == spvMAGIC then dBin
            else if magic == spvMAGIC_rev
              then dModifyUserState (\dss -> dss{dssFlipByteOrder = True}) >> dBin
              else dErrorAt 0 "invalid SPIR-V magic"

        dBin = do
          version <- dX32 "version" id
          generator <- dX32 "generator" id
          bound <- dX32 "bound" id
          dModifyUserState (\dss -> dss{dssIdBound = fromIntegral bound})
          schema <- dX32 "schema" id

          is <- dInsts
          let vmaj = fromIntegral ((version`shiftR`16).&.0xFF) :: Int
              vmin = fromIntegral ((version`shiftR` 8).&.0xFF) :: Int
          return $ Binary (vmaj,vmin) generator schema is

spvMAGIC :: Word32
spvMAGIC = 0x07230203
spvMAGIC_rev :: Word32
spvMAGIC_rev = 0x03022307

dX32 :: String -> (Word32 -> a) -> DS a
dX32 what f = do
  dss <- dGetUserState
  if dssFlipByteOrder dss then dX32_BE what f else dX32_LE what f

dX64 :: String -> (Word64 -> a) -> DS a
dX64 what f = do
  lo32 <- dX32 (what++"[31:0]")  (fromIntegral :: Word32 -> Word64)
  hi32 <- dX32 (what++"[63:32]") (fromIntegral :: Word32 -> Word64)
  return $ f ((hi32`shiftL`32) .|. lo32)

dString :: DS String
dString = concat . reverse <$> loop []
  where loop :: [String] -> DS [String]
        loop rstrs = do
          bs <- dBytes
          case S.splitAt 4 bs of
            (bs_pfx,bs_sfx)
              | S.length bs_pfx < 4 -> dError "underflow reading Literal String word"
              | otherwise -> do
                dSkip 4
                let w8s = takeWhile (/=0) (S.unpack bs_pfx)
                    wcs = map (chr . fromIntegral) w8s :: String
                if length w8s == 4 then loop (wcs:rstrs)
                  else return (wcs:rstrs)

-- decodeEnum :: Word32 -> (Word32 -> a) -> [(Word32, a)] -> [a]
-- decodeEnum val cons_unknown = enumCase val
--   where enumCase 0 _ = []
--         enumCase v ((av,a):eas)
--           | (av .&. v) /= 0 = a:enumCase (v .&. complement av) eas
--           | otherwise = enumCase v eas
--         enumCase v [] = [cons_unknown v]

dEnum :: Word32 -> (Word32 -> [Word32] -> a) -> [(Word32,DS a)] -> DS [a]
dEnum val cons_unknown = enumCase [] val
  where enumCase ras 0 _ = return $ reverse ras
        enumCase ras v ((av,da):eas)
          | (av .&. v) /= 0 = do {a<-da; enumCase (a:ras) (v .&. complement av) eas}
          | otherwise = enumCase ras v eas
        enumCase ras v [] = enumCase (cons_unknown v []:ras) 0 []

dXID :: String -> DS ID
dXID what = dX32 what fromIntegral

dInsts :: DS [Op]
dInsts = do
  let decLoop rops = do
        nleft <- S.length <$> dBytes
        if nleft == 0 then return (reverse rops)
          else do
            off <- dOffset
            inst_hdr <- dX32 "instruction header" id
            let nwrds = fromIntegral ((inst_hdr .&. 0xFFFF0000)`shiftR`16) :: Int
                opcode = fromIntegral (inst_hdr .&. 0xFFFF) :: Word16
            op <- dInst opcode nwrds
            end_off <- dOffset
            when (end_off - off /= 4*nwrds) $ do
              dErrorAt off $
                "INTERNAL ERROR: decoding op #" ++ show opcode ++
                " instruction consumed (" ++ show ((end_off - off)`div`4) ++ " words, " ++
                "but header reports " ++ show nwrds ++")"
            decLoop (op:rops)
  decLoop []

-- Generated from tools/Generate.hs
dInst :: Word16 -> Int -> DS Op
dInst opcode nwords = do
  inst_start_off <- dOffset
  --
  let dWordsLeft :: DS Int
      dWordsLeft = do
        off <- dOffset
        return (nwords - 1 - ((off - inst_start_off)`div`4))
  let dOpt :: DS a -> DS (Maybe a) -- decodes something if there's bits left for this inst
      dOpt da = do
        n <- dWordsLeft
        if n <= 0 then return Nothing else (Just <$> da)
  let dMany :: DS a -> DS [a] -- decodes something if there's bits left for this inst
      dMany da = do
        n <- dWordsLeft
        if n <= 0 then return [] else do {a<-da; (a:) <$> dMany da}
  let dLiteralContextDependentNumber :: DS LiteralContextDependentNumber
      dLiteralContextDependentNumber = do -- must always be a suffix
        n <- dWordsLeft
        case n of
          1 -> fromIntegral <$> dX32 "LiteralContextDependentNumber" id
          2 -> dX64 "LiteralContextDependentNumber" fromIntegral
          _ -> dErrorAt inst_start_off "LiteralContextDependentNumber with unexpected number of words left"
  --
  case opcode of
    0 -> return OpNop
    1 -> OpUndef <$> dIdResultType <*> dIdResult
    2 -> OpSourceContinued <$> dLiteralString
    3 -> OpSource <$> dSourceLanguage <*> dLiteralInteger32 <*> dOpt dIdRef <*> dOpt dLiteralString
    4 -> OpSourceExtension <$> dLiteralString
    5 -> OpName <$> dIdRef <*> dLiteralString
    6 -> OpMemberName <$> dIdRef <*> dLiteralInteger32 <*> dLiteralString
    7 -> OpString <$> dIdResult <*> dLiteralString
    8 -> OpLine <$> dIdRef <*> dLiteralInteger32 <*> dLiteralInteger32
    10 -> OpExtension <$> dLiteralString
    11 -> OpExtInstImport <$> dIdResult <*> dLiteralString
    12 -> OpExtInst <$> dIdResultType <*> dIdResult <*> dIdRef <*> dLiteralExtInstInteger <*> dMany dIdRef
    14 -> OpMemoryModel <$> dAddressingModel <*> dMemoryModel
    15 -> OpEntryPoint <$> dExecutionModel <*> dIdRef <*> dLiteralString <*> dMany dIdRef
    16 -> OpExecutionMode <$> dIdRef <*> dExecutionMode
    17 -> OpCapability <$> dCapability
    19 -> OpTypeVoid <$> dIdResult
    20 -> OpTypeBool <$> dIdResult
    21 -> OpTypeInt <$> dIdResult <*> dLiteralInteger32 <*> dLiteralInteger32
    22 -> OpTypeFloat <$> dIdResult <*> dLiteralInteger32
    23 -> OpTypeVector <$> dIdResult <*> dIdRef <*> dLiteralInteger32
    24 -> OpTypeMatrix <$> dIdResult <*> dIdRef <*> dLiteralInteger32
    25 -> OpTypeImage <$> dIdResult <*> dIdRef <*> dDim <*> dLiteralInteger32 <*> dLiteralInteger32 <*> dLiteralInteger32 <*> dLiteralInteger32 <*> dImageFormat <*> dOpt dAccessQualifier
    26 -> OpTypeSampler <$> dIdResult
    27 -> OpTypeSampledImage <$> dIdResult <*> dIdRef
    28 -> OpTypeArray <$> dIdResult <*> dIdRef <*> dIdRef
    29 -> OpTypeRuntimeArray <$> dIdResult <*> dIdRef
    30 -> OpTypeStruct <$> dIdResult <*> dMany dIdRef
    31 -> OpTypeOpaque <$> dIdResult <*> dLiteralString
    32 -> OpTypePointer <$> dIdResult <*> dStorageClass <*> dIdRef
    33 -> OpTypeFunction <$> dIdResult <*> dIdRef <*> dMany dIdRef
    34 -> OpTypeEvent <$> dIdResult
    35 -> OpTypeDeviceEvent <$> dIdResult
    36 -> OpTypeReserveId <$> dIdResult
    37 -> OpTypeQueue <$> dIdResult
    38 -> OpTypePipe <$> dIdResult <*> dAccessQualifier
    39 -> OpTypeForwardPointer <$> dIdRef <*> dStorageClass
    41 -> OpConstantTrue <$> dIdResultType <*> dIdResult
    42 -> OpConstantFalse <$> dIdResultType <*> dIdResult
    43 -> OpConstant <$> dIdResultType <*> dIdResult <*> dLiteralContextDependentNumber
    44 -> OpConstantComposite <$> dIdResultType <*> dIdResult <*> dMany dIdRef
    45 -> OpConstantSampler <$> dIdResultType <*> dIdResult <*> dSamplerAddressingMode <*> dLiteralInteger32 <*> dSamplerFilterMode
    46 -> OpConstantNull <$> dIdResultType <*> dIdResult
    48 -> OpSpecConstantTrue <$> dIdResultType <*> dIdResult
    49 -> OpSpecConstantFalse <$> dIdResultType <*> dIdResult
    50 -> OpSpecConstant <$> dIdResultType <*> dIdResult <*> dLiteralContextDependentNumber
    51 -> OpSpecConstantComposite <$> dIdResultType <*> dIdResult <*> dMany dIdRef
    52 -> OpSpecConstantOp <$> dIdResultType <*> dIdResult <*> dLiteralSpecConstantOpInteger <*> dMany dIdRef
    54 -> OpFunction <$> dIdResultType <*> dIdResult <*> dFunctionControl <*> dIdRef
    55 -> OpFunctionParameter <$> dIdResultType <*> dIdResult
    56 -> return OpFunctionEnd
    57 -> OpFunctionCall <$> dIdResultType <*> dIdResult <*> dIdRef <*> dMany dIdRef
    59 -> OpVariable <$> dIdResultType <*> dIdResult <*> dStorageClass <*> dOpt dIdRef
    60 -> OpImageTexelPointer <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dIdRef
    61 -> OpLoad <$> dIdResultType <*> dIdResult <*> dIdRef <*> dOpt dMemoryAccess
    62 -> OpStore <$> dIdRef <*> dIdRef <*> dOpt dMemoryAccess
    63 -> OpCopyMemory <$> dIdRef <*> dIdRef <*> dOpt dMemoryAccess
    64 -> OpCopyMemorySized <$> dIdRef <*> dIdRef <*> dIdRef <*> dOpt dMemoryAccess
    65 -> OpAccessChain <$> dIdResultType <*> dIdResult <*> dIdRef <*> dMany dIdRef
    66 -> OpInBoundsAccessChain <$> dIdResultType <*> dIdResult <*> dIdRef <*> dMany dIdRef
    67 -> OpPtrAccessChain <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dMany dIdRef
    68 -> OpArrayLength <$> dIdResultType <*> dIdResult <*> dIdRef <*> dLiteralInteger32
    69 -> OpGenericPtrMemSemantics <$> dIdResultType <*> dIdResult <*> dIdRef
    70 -> OpInBoundsPtrAccessChain <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dMany dIdRef
    71 -> OpDecorate <$> dIdRef <*> dDecoration
    72 -> OpMemberDecorate <$> dIdRef <*> dLiteralInteger32 <*> dDecoration
    73 -> OpDecorationGroup <$> dIdResult
    74 -> OpGroupDecorate <$> dIdRef <*> dMany dIdRef
    75 -> OpGroupMemberDecorate <$> dIdRef <*> dMany dPairIdRefLiteralInteger
    77 -> OpVectorExtractDynamic <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    78 -> OpVectorInsertDynamic <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dIdRef
    79 -> OpVectorShuffle <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dMany dLiteralInteger32
    80 -> OpCompositeConstruct <$> dIdResultType <*> dIdResult <*> dMany dIdRef
    81 -> OpCompositeExtract <$> dIdResultType <*> dIdResult <*> dIdRef <*> dMany dLiteralInteger32
    82 -> OpCompositeInsert <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dMany dLiteralInteger32
    83 -> OpCopyObject <$> dIdResultType <*> dIdResult <*> dIdRef
    84 -> OpTranspose <$> dIdResultType <*> dIdResult <*> dIdRef
    86 -> OpSampledImage <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    87 -> OpImageSampleImplicitLod <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dOpt dImageOperands
    88 -> OpImageSampleExplicitLod <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dImageOperands
    89 -> OpImageSampleDrefImplicitLod <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dIdRef <*> dOpt dImageOperands
    90 -> OpImageSampleDrefExplicitLod <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dIdRef <*> dImageOperands
    91 -> OpImageSampleProjImplicitLod <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dOpt dImageOperands
    92 -> OpImageSampleProjExplicitLod <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dImageOperands
    93 -> OpImageSampleProjDrefImplicitLod <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dIdRef <*> dOpt dImageOperands
    94 -> OpImageSampleProjDrefExplicitLod <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dIdRef <*> dImageOperands
    95 -> OpImageFetch <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dOpt dImageOperands
    96 -> OpImageGather <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dIdRef <*> dOpt dImageOperands
    97 -> OpImageDrefGather <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dIdRef <*> dOpt dImageOperands
    98 -> OpImageRead <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dOpt dImageOperands
    99 -> OpImageWrite <$> dIdRef <*> dIdRef <*> dIdRef <*> dOpt dImageOperands
    100 -> OpImage <$> dIdResultType <*> dIdResult <*> dIdRef
    101 -> OpImageQueryFormat <$> dIdResultType <*> dIdResult <*> dIdRef
    102 -> OpImageQueryOrder <$> dIdResultType <*> dIdResult <*> dIdRef
    103 -> OpImageQuerySizeLod <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    104 -> OpImageQuerySize <$> dIdResultType <*> dIdResult <*> dIdRef
    105 -> OpImageQueryLod <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    106 -> OpImageQueryLevels <$> dIdResultType <*> dIdResult <*> dIdRef
    107 -> OpImageQuerySamples <$> dIdResultType <*> dIdResult <*> dIdRef
    109 -> OpConvertFToU <$> dIdResultType <*> dIdResult <*> dIdRef
    110 -> OpConvertFToS <$> dIdResultType <*> dIdResult <*> dIdRef
    111 -> OpConvertSToF <$> dIdResultType <*> dIdResult <*> dIdRef
    112 -> OpConvertUToF <$> dIdResultType <*> dIdResult <*> dIdRef
    113 -> OpUConvert <$> dIdResultType <*> dIdResult <*> dIdRef
    114 -> OpSConvert <$> dIdResultType <*> dIdResult <*> dIdRef
    115 -> OpFConvert <$> dIdResultType <*> dIdResult <*> dIdRef
    116 -> OpQuantizeToF16 <$> dIdResultType <*> dIdResult <*> dIdRef
    117 -> OpConvertPtrToU <$> dIdResultType <*> dIdResult <*> dIdRef
    118 -> OpSatConvertSToU <$> dIdResultType <*> dIdResult <*> dIdRef
    119 -> OpSatConvertUToS <$> dIdResultType <*> dIdResult <*> dIdRef
    120 -> OpConvertUToPtr <$> dIdResultType <*> dIdResult <*> dIdRef
    121 -> OpPtrCastToGeneric <$> dIdResultType <*> dIdResult <*> dIdRef
    122 -> OpGenericCastToPtr <$> dIdResultType <*> dIdResult <*> dIdRef
    123 -> OpGenericCastToPtrExplicit <$> dIdResultType <*> dIdResult <*> dIdRef <*> dStorageClass
    124 -> OpBitcast <$> dIdResultType <*> dIdResult <*> dIdRef
    126 -> OpSNegate <$> dIdResultType <*> dIdResult <*> dIdRef
    127 -> OpFNegate <$> dIdResultType <*> dIdResult <*> dIdRef
    128 -> OpIAdd <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    129 -> OpFAdd <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    130 -> OpISub <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    131 -> OpFSub <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    132 -> OpIMul <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    133 -> OpFMul <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    134 -> OpUDiv <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    135 -> OpSDiv <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    136 -> OpFDiv <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    137 -> OpUMod <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    138 -> OpSRem <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    139 -> OpSMod <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    140 -> OpFRem <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    141 -> OpFMod <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    142 -> OpVectorTimesScalar <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    143 -> OpMatrixTimesScalar <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    144 -> OpVectorTimesMatrix <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    145 -> OpMatrixTimesVector <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    146 -> OpMatrixTimesMatrix <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    147 -> OpOuterProduct <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    148 -> OpDot <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    149 -> OpIAddCarry <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    150 -> OpISubBorrow <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    151 -> OpUMulExtended <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    152 -> OpSMulExtended <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    154 -> OpAny <$> dIdResultType <*> dIdResult <*> dIdRef
    155 -> OpAll <$> dIdResultType <*> dIdResult <*> dIdRef
    156 -> OpIsNan <$> dIdResultType <*> dIdResult <*> dIdRef
    157 -> OpIsInf <$> dIdResultType <*> dIdResult <*> dIdRef
    158 -> OpIsFinite <$> dIdResultType <*> dIdResult <*> dIdRef
    159 -> OpIsNormal <$> dIdResultType <*> dIdResult <*> dIdRef
    160 -> OpSignBitSet <$> dIdResultType <*> dIdResult <*> dIdRef
    161 -> OpLessOrGreater <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    162 -> OpOrdered <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    163 -> OpUnordered <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    164 -> OpLogicalEqual <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    165 -> OpLogicalNotEqual <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    166 -> OpLogicalOr <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    167 -> OpLogicalAnd <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    168 -> OpLogicalNot <$> dIdResultType <*> dIdResult <*> dIdRef
    169 -> OpSelect <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dIdRef
    170 -> OpIEqual <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    171 -> OpINotEqual <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    172 -> OpUGreaterThan <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    173 -> OpSGreaterThan <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    174 -> OpUGreaterThanEqual <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    175 -> OpSGreaterThanEqual <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    176 -> OpULessThan <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    177 -> OpSLessThan <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    178 -> OpULessThanEqual <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    179 -> OpSLessThanEqual <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    180 -> OpFOrdEqual <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    181 -> OpFUnordEqual <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    182 -> OpFOrdNotEqual <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    183 -> OpFUnordNotEqual <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    184 -> OpFOrdLessThan <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    185 -> OpFUnordLessThan <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    186 -> OpFOrdGreaterThan <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    187 -> OpFUnordGreaterThan <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    188 -> OpFOrdLessThanEqual <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    189 -> OpFUnordLessThanEqual <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    190 -> OpFOrdGreaterThanEqual <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    191 -> OpFUnordGreaterThanEqual <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    194 -> OpShiftRightLogical <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    195 -> OpShiftRightArithmetic <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    196 -> OpShiftLeftLogical <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    197 -> OpBitwiseOr <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    198 -> OpBitwiseXor <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    199 -> OpBitwiseAnd <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    200 -> OpNot <$> dIdResultType <*> dIdResult <*> dIdRef
    201 -> OpBitFieldInsert <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dIdRef <*> dIdRef
    202 -> OpBitFieldSExtract <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dIdRef
    203 -> OpBitFieldUExtract <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dIdRef
    204 -> OpBitReverse <$> dIdResultType <*> dIdResult <*> dIdRef
    205 -> OpBitCount <$> dIdResultType <*> dIdResult <*> dIdRef
    207 -> OpDPdx <$> dIdResultType <*> dIdResult <*> dIdRef
    208 -> OpDPdy <$> dIdResultType <*> dIdResult <*> dIdRef
    209 -> OpFwidth <$> dIdResultType <*> dIdResult <*> dIdRef
    210 -> OpDPdxFine <$> dIdResultType <*> dIdResult <*> dIdRef
    211 -> OpDPdyFine <$> dIdResultType <*> dIdResult <*> dIdRef
    212 -> OpFwidthFine <$> dIdResultType <*> dIdResult <*> dIdRef
    213 -> OpDPdxCoarse <$> dIdResultType <*> dIdResult <*> dIdRef
    214 -> OpDPdyCoarse <$> dIdResultType <*> dIdResult <*> dIdRef
    215 -> OpFwidthCoarse <$> dIdResultType <*> dIdResult <*> dIdRef
    218 -> return OpEmitVertex
    219 -> return OpEndPrimitive
    220 -> OpEmitStreamVertex <$> dIdRef
    221 -> OpEndStreamPrimitive <$> dIdRef
    224 -> OpControlBarrier <$> dIdScope <*> dIdScope <*> dIdMemorySemantics
    225 -> OpMemoryBarrier <$> dIdScope <*> dIdMemorySemantics
    227 -> OpAtomicLoad <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdScope <*> dIdMemorySemantics
    228 -> OpAtomicStore <$> dIdRef <*> dIdScope <*> dIdMemorySemantics <*> dIdRef
    229 -> OpAtomicExchange <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdScope <*> dIdMemorySemantics <*> dIdRef
    230 -> OpAtomicCompareExchange <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdScope <*> dIdMemorySemantics <*> dIdMemorySemantics <*> dIdRef <*> dIdRef
    231 -> OpAtomicCompareExchangeWeak <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdScope <*> dIdMemorySemantics <*> dIdMemorySemantics <*> dIdRef <*> dIdRef
    232 -> OpAtomicIIncrement <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdScope <*> dIdMemorySemantics
    233 -> OpAtomicIDecrement <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdScope <*> dIdMemorySemantics
    234 -> OpAtomicIAdd <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdScope <*> dIdMemorySemantics <*> dIdRef
    235 -> OpAtomicISub <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdScope <*> dIdMemorySemantics <*> dIdRef
    236 -> OpAtomicSMin <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdScope <*> dIdMemorySemantics <*> dIdRef
    237 -> OpAtomicUMin <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdScope <*> dIdMemorySemantics <*> dIdRef
    238 -> OpAtomicSMax <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdScope <*> dIdMemorySemantics <*> dIdRef
    239 -> OpAtomicUMax <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdScope <*> dIdMemorySemantics <*> dIdRef
    240 -> OpAtomicAnd <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdScope <*> dIdMemorySemantics <*> dIdRef
    241 -> OpAtomicOr <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdScope <*> dIdMemorySemantics <*> dIdRef
    242 -> OpAtomicXor <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdScope <*> dIdMemorySemantics <*> dIdRef
    245 -> OpPhi <$> dIdResultType <*> dIdResult <*> dMany dPairIdRefIdRef
    246 -> OpLoopMerge <$> dIdRef <*> dIdRef <*> dLoopControl
    247 -> OpSelectionMerge <$> dIdRef <*> dSelectionControl
    248 -> OpLabel <$> dIdResult
    249 -> OpBranch <$> dIdRef
    250 -> OpBranchConditional <$> dIdRef <*> dIdRef <*> dIdRef <*> dMany dLiteralInteger32
    251 -> OpSwitch <$> dIdRef <*> dIdRef <*> dMany dPairLiteralIntegerIdRef
    252 -> return OpKill
    253 -> return OpReturn
    254 -> OpReturnValue <$> dIdRef
    255 -> return OpUnreachable
    256 -> OpLifetimeStart <$> dIdRef <*> dLiteralInteger32
    257 -> OpLifetimeStop <$> dIdRef <*> dLiteralInteger32
    259 -> OpGroupAsyncCopy <$> dIdResultType <*> dIdResult <*> dIdScope <*> dIdRef <*> dIdRef <*> dIdRef <*> dIdRef <*> dIdRef
    260 -> OpGroupWaitEvents <$> dIdScope <*> dIdRef <*> dIdRef
    261 -> OpGroupAll <$> dIdResultType <*> dIdResult <*> dIdScope <*> dIdRef
    262 -> OpGroupAny <$> dIdResultType <*> dIdResult <*> dIdScope <*> dIdRef
    263 -> OpGroupBroadcast <$> dIdResultType <*> dIdResult <*> dIdScope <*> dIdRef <*> dIdRef
    264 -> OpGroupIAdd <$> dIdResultType <*> dIdResult <*> dIdScope <*> dGroupOperation <*> dIdRef
    265 -> OpGroupFAdd <$> dIdResultType <*> dIdResult <*> dIdScope <*> dGroupOperation <*> dIdRef
    266 -> OpGroupFMin <$> dIdResultType <*> dIdResult <*> dIdScope <*> dGroupOperation <*> dIdRef
    267 -> OpGroupUMin <$> dIdResultType <*> dIdResult <*> dIdScope <*> dGroupOperation <*> dIdRef
    268 -> OpGroupSMin <$> dIdResultType <*> dIdResult <*> dIdScope <*> dGroupOperation <*> dIdRef
    269 -> OpGroupFMax <$> dIdResultType <*> dIdResult <*> dIdScope <*> dGroupOperation <*> dIdRef
    270 -> OpGroupUMax <$> dIdResultType <*> dIdResult <*> dIdScope <*> dGroupOperation <*> dIdRef
    271 -> OpGroupSMax <$> dIdResultType <*> dIdResult <*> dIdScope <*> dGroupOperation <*> dIdRef
    274 -> OpReadPipe <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dIdRef <*> dIdRef
    275 -> OpWritePipe <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dIdRef <*> dIdRef
    276 -> OpReservedReadPipe <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dIdRef <*> dIdRef <*> dIdRef <*> dIdRef
    277 -> OpReservedWritePipe <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dIdRef <*> dIdRef <*> dIdRef <*> dIdRef
    278 -> OpReserveReadPipePackets <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dIdRef <*> dIdRef
    279 -> OpReserveWritePipePackets <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dIdRef <*> dIdRef
    280 -> OpCommitReadPipe <$> dIdRef <*> dIdRef <*> dIdRef <*> dIdRef
    281 -> OpCommitWritePipe <$> dIdRef <*> dIdRef <*> dIdRef <*> dIdRef
    282 -> OpIsValidReserveId <$> dIdResultType <*> dIdResult <*> dIdRef
    283 -> OpGetNumPipePackets <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dIdRef
    284 -> OpGetMaxPipePackets <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dIdRef
    285 -> OpGroupReserveReadPipePackets <$> dIdResultType <*> dIdResult <*> dIdScope <*> dIdRef <*> dIdRef <*> dIdRef <*> dIdRef
    286 -> OpGroupReserveWritePipePackets <$> dIdResultType <*> dIdResult <*> dIdScope <*> dIdRef <*> dIdRef <*> dIdRef <*> dIdRef
    287 -> OpGroupCommitReadPipe <$> dIdScope <*> dIdRef <*> dIdRef <*> dIdRef <*> dIdRef
    288 -> OpGroupCommitWritePipe <$> dIdScope <*> dIdRef <*> dIdRef <*> dIdRef <*> dIdRef
    291 -> OpEnqueueMarker <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dIdRef <*> dIdRef
    292 -> OpEnqueueKernel <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dIdRef <*> dIdRef <*> dIdRef <*> dIdRef <*> dIdRef <*> dIdRef <*> dIdRef <*> dIdRef <*> dMany dIdRef
    293 -> OpGetKernelNDrangeSubGroupCount <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dIdRef <*> dIdRef <*> dIdRef
    294 -> OpGetKernelNDrangeMaxSubGroupSize <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dIdRef <*> dIdRef <*> dIdRef
    295 -> OpGetKernelWorkGroupSize <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dIdRef <*> dIdRef
    296 -> OpGetKernelPreferredWorkGroupSizeMultiple <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dIdRef <*> dIdRef
    297 -> OpRetainEvent <$> dIdRef
    298 -> OpReleaseEvent <$> dIdRef
    299 -> OpCreateUserEvent <$> dIdResultType <*> dIdResult
    300 -> OpIsValidEvent <$> dIdResultType <*> dIdResult <*> dIdRef
    301 -> OpSetUserEventStatus <$> dIdRef <*> dIdRef
    302 -> OpCaptureEventProfilingInfo <$> dIdRef <*> dIdRef <*> dIdRef
    303 -> OpGetDefaultQueue <$> dIdResultType <*> dIdResult
    304 -> OpBuildNDRange <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dIdRef
    305 -> OpImageSparseSampleImplicitLod <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dOpt dImageOperands
    306 -> OpImageSparseSampleExplicitLod <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dImageOperands
    307 -> OpImageSparseSampleDrefImplicitLod <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dIdRef <*> dOpt dImageOperands
    308 -> OpImageSparseSampleDrefExplicitLod <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dIdRef <*> dImageOperands
    309 -> OpImageSparseSampleProjImplicitLod <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dOpt dImageOperands
    310 -> OpImageSparseSampleProjExplicitLod <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dImageOperands
    311 -> OpImageSparseSampleProjDrefImplicitLod <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dIdRef <*> dOpt dImageOperands
    312 -> OpImageSparseSampleProjDrefExplicitLod <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dIdRef <*> dImageOperands
    313 -> OpImageSparseFetch <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dOpt dImageOperands
    314 -> OpImageSparseGather <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dIdRef <*> dOpt dImageOperands
    315 -> OpImageSparseDrefGather <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dIdRef <*> dOpt dImageOperands
    316 -> OpImageSparseTexelsResident <$> dIdResultType <*> dIdResult <*> dIdRef
    317 -> return OpNoLine
    318 -> OpAtomicFlagTestAndSet <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdScope <*> dIdMemorySemantics
    319 -> OpAtomicFlagClear <$> dIdRef <*> dIdScope <*> dIdMemorySemantics
    320 -> OpImageSparseRead <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dOpt dImageOperands
    321 -> OpSizeOf <$> dIdResultType <*> dIdResult <*> dIdRef
    322 -> OpTypePipeStorage <$> dIdResult
    323 -> OpConstantPipeStorage <$> dIdResultType <*> dIdResult <*> dLiteralInteger32 <*> dLiteralInteger32 <*> dLiteralInteger32
    324 -> OpCreatePipeFromPipeStorage <$> dIdResultType <*> dIdResult <*> dIdRef
    325 -> OpGetKernelLocalSizeForSubgroupCount <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dIdRef <*> dIdRef <*> dIdRef
    326 -> OpGetKernelMaxNumSubgroups <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dIdRef <*> dIdRef
    327 -> OpTypeNamedBarrier <$> dIdResult
    328 -> OpNamedBarrierInitialize <$> dIdResultType <*> dIdResult <*> dIdRef
    329 -> OpMemoryNamedBarrier <$> dIdRef <*> dIdScope <*> dIdMemorySemantics
    330 -> OpModuleProcessed <$> dLiteralString
    331 -> OpExecutionModeId <$> dIdRef <*> dExecutionMode
    332 -> OpDecorateId <$> dIdRef <*> dDecoration
    4421 -> OpSubgroupBallotKHR <$> dIdResultType <*> dIdResult <*> dIdRef
    4422 -> OpSubgroupFirstInvocationKHR <$> dIdResultType <*> dIdResult <*> dIdRef
    4428 -> OpSubgroupAllKHR <$> dIdResultType <*> dIdResult <*> dIdRef
    4429 -> OpSubgroupAnyKHR <$> dIdResultType <*> dIdResult <*> dIdRef
    4430 -> OpSubgroupAllEqualKHR <$> dIdResultType <*> dIdResult <*> dIdRef
    4432 -> OpSubgroupReadInvocationKHR <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    5000 -> OpGroupIAddNonUniformAMD <$> dIdResultType <*> dIdResult <*> dIdScope <*> dGroupOperation <*> dIdRef
    5001 -> OpGroupFAddNonUniformAMD <$> dIdResultType <*> dIdResult <*> dIdScope <*> dGroupOperation <*> dIdRef
    5002 -> OpGroupFMinNonUniformAMD <$> dIdResultType <*> dIdResult <*> dIdScope <*> dGroupOperation <*> dIdRef
    5003 -> OpGroupUMinNonUniformAMD <$> dIdResultType <*> dIdResult <*> dIdScope <*> dGroupOperation <*> dIdRef
    5004 -> OpGroupSMinNonUniformAMD <$> dIdResultType <*> dIdResult <*> dIdScope <*> dGroupOperation <*> dIdRef
    5005 -> OpGroupFMaxNonUniformAMD <$> dIdResultType <*> dIdResult <*> dIdScope <*> dGroupOperation <*> dIdRef
    5006 -> OpGroupUMaxNonUniformAMD <$> dIdResultType <*> dIdResult <*> dIdScope <*> dGroupOperation <*> dIdRef
    5007 -> OpGroupSMaxNonUniformAMD <$> dIdResultType <*> dIdResult <*> dIdScope <*> dGroupOperation <*> dIdRef
    5011 -> OpFragmentMaskFetchAMD <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    5012 -> OpFragmentFetchAMD <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dIdRef
    5571 -> OpSubgroupShuffleINTEL <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    5572 -> OpSubgroupShuffleDownINTEL <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dIdRef
    5573 -> OpSubgroupShuffleUpINTEL <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef <*> dIdRef
    5574 -> OpSubgroupShuffleXorINTEL <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    5575 -> OpSubgroupBlockReadINTEL <$> dIdResultType <*> dIdResult <*> dIdRef
    5576 -> OpSubgroupBlockWriteINTEL <$> dIdRef <*> dIdRef
    5577 -> OpSubgroupImageBlockReadINTEL <$> dIdResultType <*> dIdResult <*> dIdRef <*> dIdRef
    5578 -> OpSubgroupImageBlockWriteINTEL <$> dIdRef <*> dIdRef <*> dIdRef
    5632 -> OpDecorateStringGOOGLE <$> dIdRef <*> dDecoration
    5633 -> OpMemberDecorateStringGOOGLE <$> dIdRef <*> dLiteralInteger32 <*> dDecoration
    _ -> OpRAW opcode <$> sequence (replicate (nwords-1) (dX32 "raw instruction word" id))

dImageOperands :: DS [ImageOperands]
dImageOperands = do
  val <- dX32 "ImageOperands" id
  dEnum val IO_UNKNOWN
    [
      (0x01,IO_Bias <$> dIdRef)
    , (0x02,IO_Lod <$> dIdRef)
    , (0x04,IO_Grad <$> dIdRef <*> dIdRef)
    , (0x08,IO_ConstOffset <$> dIdRef)
    , (0x10,IO_Offset <$> dIdRef)
    , (0x20,IO_ConstOffsets <$> dIdRef)
    , (0x40,IO_Sample <$> dIdRef)
    , (0x80,IO_MinLod <$> dIdRef)
    ]

dFPFastMathMode :: DS [FPFastMathMode]
dFPFastMathMode = do
  val <- dX32 "FPFastMathMode" id
  dEnum val FPFMM_UNKNOWN
    [
      (0x01,return FPFMM_NotNaN)
    , (0x02,return FPFMM_NotInf)
    , (0x04,return FPFMM_NSZ)
    , (0x08,return FPFMM_AllowRecip)
    , (0x10,return FPFMM_Fast)
    ]

dSelectionControl :: DS [SelectionControl]
dSelectionControl = do
  val <- dX32 "SelectionControl" id
  dEnum val SelectionControl_UNKNOWN [(0x1,return SC_Flatten),(0x2,return SC_DontFlatten)]
dLoopControl :: DS [LoopControl]
dLoopControl = do
  val <- dX32 "LoopControl" id
  dEnum val LC_UNKNOWN
    [
      (0x1,return LC_Unroll)
    , (0x2,return LC_DontUnroll)
    , (0x4,return LC_DependencyInfinite)
    , (0x8,LC_DependencyLength <$> dLiteralInteger32)
    ]

dFunctionControl :: DS [FunctionControl]
dFunctionControl = do
  val <- dX32 "FunctionControl" id
  dEnum val FC_UNKNOWN
    [
      (0x1,return FC_Inline)
    , (0x2,return FC_DontInline)
    , (0x4,return FC_Pure)
    , (0x8,return FC_Const)
    ]

dMemorySemantics :: DS [MemorySemantics]
dMemorySemantics = do
  val <- dX32 "MemorySemantics" id
  dEnum val MS_UNKNOWN
    [
      (0x002,return MS_Acquire)
    , (0x004,return MS_Release)
    , (0x008,return MS_AcquireRelease)
    , (0x010,return MS_SequentiallyConsistent)
    , (0x040,return MS_UniformMemory)
    , (0x080,return MS_SubgroupMemory)
    , (0x100,return MS_WorkgroupMemory)
    , (0x200,return MS_CrossWorkgroupMemory)
    , (0x400,return MS_AtomicCounterMemory)
    , (0x800,return MS_ImageMemory)
    ]

dMemoryAccess :: DS [MemoryAccess]
dMemoryAccess = do
  val <- dX32 "MemoryAccess" id
  dEnum val MA_UNKNOWN [(0x1,return MA_Volatile),(0x2,MA_Aligned <$> dLiteralInteger32),(0x4,return MA_Nontemporal)]
dKernelProfilingInfo :: DS [KernelProfilingInfo]
dKernelProfilingInfo = do
  val <- dX32 "KernelProfilingInfo" id
  dEnum val KPI_UNKNOWN [(0x1,return KPI_CmdExecTime)]
dSourceLanguage :: DS SourceLanguage
dSourceLanguage = do
  val <- dX32 "SourceLanguage" id
  case val of
    0 -> return SL_Unknown
    1 -> return SL_ESSL
    2 -> return SL_GLSL
    3 -> return SL_OpenCL_C
    4 -> return SL_OpenCL_CPP
    5 -> return SL_HLSL
    _ -> return $ SL_UNKNOWN val [] -- FIXME: pass hint to tell how long the instruction is

dExecutionModel :: DS ExecutionModel
dExecutionModel = do
  val <- dX32 "ExecutionModel" id
  case val of
    0 -> return EM_Vertex
    1 -> return EM_TessellationControl
    2 -> return EM_TessellationEvaluation
    3 -> return EM_Geometry
    4 -> return EM_Fragment
    5 -> return EM_GLCompute
    6 -> return EM_Kernel
    _ -> return $ ExecutionModel_UNKNOWN val [] -- FIXME: pass hint to tell how long the instruction is

dAddressingModel :: DS AddressingModel
dAddressingModel = do
  val <- dX32 "AddressingModel" id
  case val of
    0 -> return AM_Logical
    1 -> return AM_Physical32
    2 -> return AM_Physical64
    _ -> return $ AM_UNKNOWN val [] -- FIXME: pass hint to tell how long the instruction is

dMemoryModel :: DS MemoryModel
dMemoryModel = do
  val <- dX32 "MemoryModel" id
  case val of
    0 -> return MM_Simple
    1 -> return MM_GLSL450
    2 -> return MM_OpenCL
    _ -> return $ MM_UNKNOWN val [] -- FIXME: pass hint to tell how long the instruction is

dExecutionMode :: DS ExecutionMode
dExecutionMode = do
  val <- dX32 "ExecutionMode" id
  case val of
    0 -> EM_Invocations <$> dLiteralInteger32
    1 -> return EM_SpacingEqual
    2 -> return EM_SpacingFractionalEven
    3 -> return EM_SpacingFractionalOdd
    4 -> return EM_VertexOrderCw
    5 -> return EM_VertexOrderCcw
    6 -> return EM_PixelCenterInteger
    7 -> return EM_OriginUpperLeft
    8 -> return EM_OriginLowerLeft
    9 -> return EM_EarlyFragmentTests
    10 -> return EM_PointMode
    11 -> return EM_Xfb
    12 -> return EM_DepthReplacing
    14 -> return EM_DepthGreater
    15 -> return EM_DepthLess
    16 -> return EM_DepthUnchanged
    17 -> EM_LocalSize <$> dLiteralInteger32 <*> dLiteralInteger32 <*> dLiteralInteger32
    18 -> EM_LocalSizeHint <$> dLiteralInteger32 <*> dLiteralInteger32 <*> dLiteralInteger32
    19 -> return EM_InputPoints
    20 -> return EM_InputLines
    21 -> return EM_InputLinesAdjacency
    22 -> return EM_Triangles
    23 -> return EM_InputTrianglesAdjacency
    24 -> return EM_Quads
    25 -> return EM_Isolines
    26 -> EM_OutputVertices <$> dLiteralInteger32
    27 -> return EM_OutputPoints
    28 -> return EM_OutputLineStrip
    29 -> return EM_OutputTriangleStrip
    30 -> EM_VecTypeHint <$> dLiteralInteger32
    31 -> return EM_ContractionOff
    33 -> return EM_Initializer
    34 -> return EM_Finalizer
    35 -> EM_SubgroupSize <$> dLiteralInteger32
    36 -> EM_SubgroupsPerWorkgroup <$> dLiteralInteger32
    37 -> EM_SubgroupsPerWorkgroupId <$> dIdRef
    38 -> EM_LocalSizeId <$> dIdRef <*> dIdRef <*> dIdRef
    39 -> EM_LocalSizeHintId <$> dIdRef
    4446 -> return EM_PostDepthCoverage
    5027 -> return EM_StencilRefReplacingEXT
    _ -> return $ EM_UNKNOWN val [] -- FIXME: pass hint to tell how long the instruction is

dStorageClass :: DS StorageClass
dStorageClass = do
  val <- dX32 "StorageClass" id
  case val of
    0 -> return SC_UniformConstant
    1 -> return SC_Input
    2 -> return SC_Uniform
    3 -> return SC_Output
    4 -> return SC_Workgroup
    5 -> return SC_CrossWorkgroup
    6 -> return SC_Private
    7 -> return SC_Function
    8 -> return SC_Generic
    9 -> return SC_PushConstant
    10 -> return SC_AtomicCounter
    11 -> return SC_Image
    12 -> return SC_StorageBuffer
    _ -> return $ SC_UNKNOWN val [] -- FIXME: pass hint to tell how long the instruction is

dDim :: DS Dim
dDim = do
  val <- dX32 "Dim" id
  case val of
    0 -> return D_1D
    1 -> return D_2D
    2 -> return D_3D
    3 -> return D_Cube
    4 -> return D_Rect
    5 -> return D_Buffer
    6 -> return D_SubpassData
    _ -> return $ D_UNKNOWN val [] -- FIXME: pass hint to tell how long the instruction is

dSamplerAddressingMode :: DS SamplerAddressingMode
dSamplerAddressingMode = do
  val <- dX32 "SamplerAddressingMode" id
  case val of
    0 -> return SAM_None
    1 -> return SAM_ClampToEdge
    2 -> return SAM_Clamp
    3 -> return SAM_Repeat
    4 -> return SAM_RepeatMirrored
    _ -> return $ SAM_UNKNOWN val [] -- FIXME: pass hint to tell how long the instruction is

dSamplerFilterMode :: DS SamplerFilterMode
dSamplerFilterMode = do
  val <- dX32 "SamplerFilterMode" id
  case val of
    0 -> return SFM_Nearest
    1 -> return SFM_Linear
    _ -> return $ SFM_UNKNOWN val [] -- FIXME: pass hint to tell how long the instruction is

dImageFormat :: DS ImageFormat
dImageFormat = do
  val <- dX32 "ImageFormat" id
  case val of
    0 -> return IF_Unknown
    1 -> return IF_Rgba32f
    2 -> return IF_Rgba16f
    3 -> return IF_R32f
    4 -> return IF_Rgba8
    5 -> return IF_Rgba8Snorm
    6 -> return IF_Rg32f
    7 -> return IF_Rg16f
    8 -> return IF_R11fG11fB10f
    9 -> return IF_R16f
    10 -> return IF_Rgba16
    11 -> return IF_Rgb10A2
    12 -> return IF_Rg16
    13 -> return IF_Rg8
    14 -> return IF_R16
    15 -> return IF_R8
    16 -> return IF_Rgba16Snorm
    17 -> return IF_Rg16Snorm
    18 -> return IF_Rg8Snorm
    19 -> return IF_R16Snorm
    20 -> return IF_R8Snorm
    21 -> return IF_Rgba32i
    22 -> return IF_Rgba16i
    23 -> return IF_Rgba8i
    24 -> return IF_R32i
    25 -> return IF_Rg32i
    26 -> return IF_Rg16i
    27 -> return IF_Rg8i
    28 -> return IF_R16i
    29 -> return IF_R8i
    30 -> return IF_Rgba32ui
    31 -> return IF_Rgba16ui
    32 -> return IF_Rgba8ui
    33 -> return IF_R32ui
    34 -> return IF_Rgb10a2ui
    35 -> return IF_Rg32ui
    36 -> return IF_Rg16ui
    37 -> return IF_Rg8ui
    38 -> return IF_R16ui
    39 -> return IF_R8ui
    _ -> return $ IF_UNKNOWN val [] -- FIXME: pass hint to tell how long the instruction is

dImageChannelOrder :: DS ImageChannelOrder
dImageChannelOrder = do
  val <- dX32 "ImageChannelOrder" id
  case val of
    0 -> return ICO_R
    1 -> return ICO_A
    2 -> return ICO_RG
    3 -> return ICO_RA
    4 -> return ICO_RGB
    5 -> return ICO_RGBA
    6 -> return ICO_BGRA
    7 -> return ICO_ARGB
    8 -> return ICO_Intensity
    9 -> return ICO_Luminance
    10 -> return ICO_Rx
    11 -> return ICO_RGx
    12 -> return ICO_RGBx
    13 -> return ICO_Depth
    14 -> return ICO_DepthStencil
    15 -> return ICO_sRGB
    16 -> return ICO_sRGBx
    17 -> return ICO_sRGBA
    18 -> return ICO_sBGRA
    19 -> return ICO_ABGR
    _ -> return $ ICO_UNKNOWN val [] -- FIXME: pass hint to tell how long the instruction is

dImageChannelDataType :: DS ImageChannelDataType
dImageChannelDataType = do
  val <- dX32 "ImageChannelDataType" id
  case val of
    0 -> return ICDT_SnormInt8
    1 -> return ICDT_SnormInt16
    2 -> return ICDT_UnormInt8
    3 -> return ICDT_UnormInt16
    4 -> return ICDT_UnormShort565
    5 -> return ICDT_UnormShort555
    6 -> return ICDT_UnormInt101010
    7 -> return ICDT_SignedInt8
    8 -> return ICDT_SignedInt16
    9 -> return ICDT_SignedInt32
    10 -> return ICDT_UnsignedInt8
    11 -> return ICDT_UnsignedInt16
    12 -> return ICDT_UnsignedInt32
    13 -> return ICDT_HalfFloat
    14 -> return ICDT_Float
    15 -> return ICDT_UnormInt24
    16 -> return ICDT_UnormInt101010_2
    _ -> return $ ICDT_UNKNOWN val [] -- FIXME: pass hint to tell how long the instruction is

dFPRoundingMode :: DS FPRoundingMode
dFPRoundingMode = do
  val <- dX32 "FPRoundingMode" id
  case val of
    0 -> return FPRM_RTE
    1 -> return FPRM_RTZ
    2 -> return FPRM_RTP
    3 -> return FPRM_RTN
    _ -> return $ FPRM_UNKNOWN val [] -- FIXME: pass hint to tell how long the instruction is

dLinkageType :: DS LinkageType
dLinkageType = do
  val <- dX32 "LinkageType" id
  case val of
    0 -> return LT_Export
    1 -> return LT_Import
    _ -> return $ LT_UNKNOWN val [] -- FIXME: pass hint to tell how long the instruction is

dAccessQualifier :: DS AccessQualifier
dAccessQualifier = do
  val <- dX32 "AccessQualifier" id
  case val of
    0 -> return AQ_ReadOnly
    1 -> return AQ_WriteOnly
    2 -> return AQ_ReadWrite
    _ -> return $ AQ_UNKNOWN val [] -- FIXME: pass hint to tell how long the instruction is

dFunctionParameterAttribute :: DS FunctionParameterAttribute
dFunctionParameterAttribute = do
  val <- dX32 "FunctionParameterAttribute" id
  case val of
    0 -> return FPA_Zext
    1 -> return FPA_Sext
    2 -> return FPA_ByVal
    3 -> return FPA_Sret
    4 -> return FPA_NoAlias
    5 -> return FPA_NoCapture
    6 -> return FPA_NoWrite
    7 -> return FPA_NoReadWrite
    _ -> return $ FPA_UNKNOWN val [] -- FIXME: pass hint to tell how long the instruction is

dDecoration :: DS Decoration
dDecoration = do
  val <- dX32 "Decoration" id
  case val of
    0 -> return D_RelaxedPrecision
    1 -> D_SpecId <$> dLiteralInteger32
    2 -> return D_Block
    3 -> return D_BufferBlock
    4 -> return D_RowMajor
    5 -> return D_ColMajor
    6 -> D_ArrayStride <$> dLiteralInteger32
    7 -> D_MatrixStride <$> dLiteralInteger32
    8 -> return D_GLSLShared
    9 -> return D_GLSLPacked
    10 -> return D_CPacked
    11 -> D_BuiltIn <$> dBuiltIn
    13 -> return D_NoPerspective
    14 -> return D_Flat
    15 -> return D_Patch
    16 -> return D_Centroid
    17 -> return D_Sample
    18 -> return D_Invariant
    19 -> return D_Restrict
    20 -> return D_Aliased
    21 -> return D_Volatile
    22 -> return D_Constant
    23 -> return D_Coherent
    24 -> return D_NonWritable
    25 -> return D_NonReadable
    26 -> return D_Uniform
    28 -> return D_SaturatedConversion
    29 -> D_Stream <$> dLiteralInteger32
    30 -> D_Location <$> dLiteralInteger32
    31 -> D_Component <$> dLiteralInteger32
    32 -> D_Index <$> dLiteralInteger32
    33 -> D_Binding <$> dLiteralInteger32
    34 -> D_DescriptorSet <$> dLiteralInteger32
    35 -> D_Offset <$> dLiteralInteger32
    36 -> D_XfbBuffer <$> dLiteralInteger32
    37 -> D_XfbStride <$> dLiteralInteger32
    38 -> D_FuncParamAttr <$> dFunctionParameterAttribute
    39 -> D_FPRoundingMode <$> dFPRoundingMode
    40 -> D_FPFastMathMode <$> dFPFastMathMode
    41 -> D_LinkageAttributes <$> dLiteralString <*> dLinkageType
    42 -> return D_NoContraction
    43 -> D_InputAttachmentIndex <$> dLiteralInteger32
    44 -> D_Alignment <$> dLiteralInteger32
    45 -> D_MaxByteOffset <$> dLiteralInteger64
    46 -> D_AlignmentId <$> dIdRef
    47 -> D_MaxByteOffsetId <$> dIdRef
    4999 -> return D_ExplicitInterpAMD
    5248 -> return D_OverrideCoverageNV
    5250 -> return D_PassthroughNV
    5252 -> return D_ViewportRelativeNV
    5256 -> D_SecondaryViewportRelativeNV <$> dLiteralInteger32
    5634 -> D_HlslCounterBufferGOOGLE <$> dIdRef
    5635 -> D_HlslSemanticGOOGLE <$> dLiteralString
    _ -> return $ Decoration_UNKNOWN val [] -- FIXME: pass hint to tell how long the instruction is

dBuiltIn :: DS BuiltIn
dBuiltIn = do
  val <- dX32 "BuiltIn" id
  case val of
    0 -> return BI_Position
    1 -> return BI_PointSize
    3 -> return BI_ClipDistance
    4 -> return BI_CullDistance
    5 -> return BI_VertexId
    6 -> return BI_InstanceId
    7 -> return BI_PrimitiveId
    8 -> return BI_InvocationId
    9 -> return BI_Layer
    10 -> return BI_ViewportIndex
    11 -> return BI_TessLevelOuter
    12 -> return BI_TessLevelInner
    13 -> return BI_TessCoord
    14 -> return BI_PatchVertices
    15 -> return BI_FragCoord
    16 -> return BI_PointCoord
    17 -> return BI_FrontFacing
    18 -> return BI_SampleId
    19 -> return BI_SamplePosition
    20 -> return BI_SampleMask
    22 -> return BI_FragDepth
    23 -> return BI_HelperInvocation
    24 -> return BI_NumWorkgroups
    25 -> return BI_WorkgroupSize
    26 -> return BI_WorkgroupId
    27 -> return BI_LocalInvocationId
    28 -> return BI_GlobalInvocationId
    29 -> return BI_LocalInvocationIndex
    30 -> return BI_WorkDim
    31 -> return BI_GlobalSize
    32 -> return BI_EnqueuedWorkgroupSize
    33 -> return BI_GlobalOffset
    34 -> return BI_GlobalLinearId
    36 -> return BI_SubgroupSize
    37 -> return BI_SubgroupMaxSize
    38 -> return BI_NumSubgroups
    39 -> return BI_NumEnqueuedSubgroups
    40 -> return BI_SubgroupId
    41 -> return BI_SubgroupLocalInvocationId
    42 -> return BI_VertexIndex
    43 -> return BI_InstanceIndex
    4416 -> return BI_SubgroupEqMaskKHR
    4417 -> return BI_SubgroupGeMaskKHR
    4418 -> return BI_SubgroupGtMaskKHR
    4419 -> return BI_SubgroupLeMaskKHR
    4420 -> return BI_SubgroupLtMaskKHR
    4424 -> return BI_BaseVertex
    4425 -> return BI_BaseInstance
    4426 -> return BI_DrawIndex
    4438 -> return BI_DeviceIndex
    4440 -> return BI_ViewIndex
    4992 -> return BI_BaryCoordNoPerspAMD
    4993 -> return BI_BaryCoordNoPerspCentroidAMD
    4994 -> return BI_BaryCoordNoPerspSampleAMD
    4995 -> return BI_BaryCoordSmoothAMD
    4996 -> return BI_BaryCoordSmoothCentroidAMD
    4997 -> return BI_BaryCoordSmoothSampleAMD
    4998 -> return BI_BaryCoordPullModelAMD
    5014 -> return BI_FragStencilRefEXT
    5253 -> return BI_ViewportMaskNV
    5257 -> return BI_SecondaryPositionNV
    5258 -> return BI_SecondaryViewportMaskNV
    5261 -> return BI_PositionPerViewNV
    5262 -> return BI_ViewportMaskPerViewNV
    _ -> return $ BI_UNKNOWN val [] -- FIXME: pass hint to tell how long the instruction is

dScope :: DS Scope
dScope = do
  val <- dX32 "Scope" id
  case val of
    0 -> return S_CrossDevice
    1 -> return S_Device
    2 -> return S_Workgroup
    3 -> return S_Subgroup
    4 -> return S_Invocation
    _ -> return $ S_UNKNOWN val [] -- FIXME: pass hint to tell how long the instruction is

dGroupOperation :: DS GroupOperation
dGroupOperation = do
  val <- dX32 "GroupOperation" id
  case val of
    0 -> return GO_Reduce
    1 -> return GO_InclusiveScan
    2 -> return GO_ExclusiveScan
    _ -> return $ GO_UNKNOWN val [] -- FIXME: pass hint to tell how long the instruction is

dKernelEnqueueFlags :: DS KernelEnqueueFlags
dKernelEnqueueFlags = do
  val <- dX32 "KernelEnqueueFlags" id
  case val of
    0 -> return KEF_NoWait
    1 -> return KEF_WaitKernel
    2 -> return KEF_WaitWorkGroup
    _ -> return $ KEF_UNKNOWN val [] -- FIXME: pass hint to tell how long the instruction is

dCapability :: DS Capability
dCapability = do
  val <- dX32 "Capability" id
  case val of
    0 -> return C_Matrix
    1 -> return C_Shader
    2 -> return C_Geometry
    3 -> return C_Tessellation
    4 -> return C_Addresses
    5 -> return C_Linkage
    6 -> return C_Kernel
    7 -> return C_Vector16
    8 -> return C_Float16Buffer
    9 -> return C_Float16
    10 -> return C_Float64
    11 -> return C_Int64
    12 -> return C_Int64Atomics
    13 -> return C_ImageBasic
    14 -> return C_ImageReadWrite
    15 -> return C_ImageMipmap
    17 -> return C_Pipes
    18 -> return C_Groups
    19 -> return C_DeviceEnqueue
    20 -> return C_LiteralSampler
    21 -> return C_AtomicStorage
    22 -> return C_Int16
    23 -> return C_TessellationPointSize
    24 -> return C_GeometryPointSize
    25 -> return C_ImageGatherExtended
    27 -> return C_StorageImageMultisample
    28 -> return C_UniformBufferArrayDynamicIndexing
    29 -> return C_SampledImageArrayDynamicIndexing
    30 -> return C_StorageBufferArrayDynamicIndexing
    31 -> return C_StorageImageArrayDynamicIndexing
    32 -> return C_ClipDistance
    33 -> return C_CullDistance
    34 -> return C_ImageCubeArray
    35 -> return C_SampleRateShading
    36 -> return C_ImageRect
    37 -> return C_SampledRect
    38 -> return C_GenericPointer
    39 -> return C_Int8
    40 -> return C_InputAttachment
    41 -> return C_SparseResidency
    42 -> return C_MinLod
    43 -> return C_Sampled1D
    44 -> return C_Image1D
    45 -> return C_SampledCubeArray
    46 -> return C_SampledBuffer
    47 -> return C_ImageBuffer
    48 -> return C_ImageMSArray
    49 -> return C_StorageImageExtendedFormats
    50 -> return C_ImageQuery
    51 -> return C_DerivativeControl
    52 -> return C_InterpolationFunction
    53 -> return C_TransformFeedback
    54 -> return C_GeometryStreams
    55 -> return C_StorageImageReadWithoutFormat
    56 -> return C_StorageImageWriteWithoutFormat
    57 -> return C_MultiViewport
    58 -> return C_SubgroupDispatch
    59 -> return C_NamedBarrier
    60 -> return C_PipeStorage
    4423 -> return C_SubgroupBallotKHR
    4427 -> return C_DrawParameters
    4431 -> return C_SubgroupVoteKHR
    4433 -> return C_StorageBuffer16BitAccess
--  4433 -> return C_StorageUniformBufferBlock16
    4434 -> return C_UniformAndStorageBuffer16BitAccess
--  4434 -> return C_StorageUniform16
    4435 -> return C_StoragePushConstant16
    4436 -> return C_StorageInputOutput16
    4437 -> return C_DeviceGroup
    4439 -> return C_MultiView
    4441 -> return C_VariablePointersStorageBuffer
    4442 -> return C_VariablePointers
    4445 -> return C_AtomicStorageOps
    4447 -> return C_SampleMaskPostDepthCoverage
    5009 -> return C_ImageGatherBiasLodAMD
    5010 -> return C_FragmentMaskAMD
    5013 -> return C_StencilExportEXT
    5015 -> return C_ImageReadWriteLodAMD
    5249 -> return C_SampleMaskOverrideCoverageNV
    5251 -> return C_GeometryShaderPassthroughNV
    5254 -> return C_ShaderViewportIndexLayerEXT
--  5254 -> return C_ShaderViewportIndexLayerNV
    5255 -> return C_ShaderViewportMaskNV
    5259 -> return C_ShaderStereoViewNV
    5260 -> return C_PerViewAttributesNV
    5568 -> return C_SubgroupShuffleINTEL
    5569 -> return C_SubgroupBufferBlockIOINTEL
    5570 -> return C_SubgroupImageBlockIOINTEL
    _ -> return $ C_UNKNOWN val [] -- FIXME: pass hint to tell how long the instruction is

dIdResultType :: DS IdResultType
dIdResultType = dXID "IdResultType"

dIdResult :: DS IdResult
dIdResult = dXID "IdResult"

dIdMemorySemantics :: DS IdMemorySemantics
dIdMemorySemantics = dXID "IdMemorySemantics"

dIdScope :: DS IdScope
dIdScope = dXID "IdScope"

dIdRef :: DS IdRef
dIdRef = dXID "IdRef"

dLiteralInteger32 :: DS LiteralInteger32
dLiteralInteger32 = dX32 "LiteralInteger32" id

dLiteralInteger64 :: DS LiteralInteger64
dLiteralInteger64 = dX64 "LiteralInteger64" id

dLiteralString :: DS LiteralString
dLiteralString = dString

dLiteralExtInstInteger :: DS LiteralExtInstInteger
dLiteralExtInstInteger = dX32 "LiteralExtInstInteger" id

dLiteralSpecConstantOpInteger :: DS LiteralSpecConstantOpInteger
dLiteralSpecConstantOpInteger = dX32 "LiteralSpecConstantOpInteger" id

dPairLiteralIntegerIdRef :: DS PairLiteralIntegerIdRef
dPairLiteralIntegerIdRef = PairLiteralIntegerIdRef <$>  dLiteralInteger32 <*>  dIdRef

dPairIdRefLiteralInteger :: DS PairIdRefLiteralInteger
dPairIdRefLiteralInteger = PairIdRefLiteralInteger <$>  dIdRef <*>  dLiteralInteger32

dPairIdRefIdRef :: DS PairIdRefIdRef
dPairIdRefIdRef = PairIdRefIdRef <$>  dIdRef <*>  dIdRef

