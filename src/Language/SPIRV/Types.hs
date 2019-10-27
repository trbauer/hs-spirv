module Language.SPIRV.Types where

import Data.Int
import Data.Word


data Binary =
  Binary {
    bVersion :: !(Int,Int)
  , bGenerator :: !Word32
  , bSchema :: !Word32
  , bOps :: ![Op]
  } deriving (Show,Eq)

type ID = Int

data Op =
    OpNop
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpNop
  | OpUndef !IdResultType !IdResult
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpUndef
  | OpSourceContinued !LiteralString
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpSourceContinued
  | OpSource !SourceLanguage !LiteralInteger32 !(Maybe IdRef) !(Maybe LiteralString)
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpSource
  | OpSourceExtension !LiteralString
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpSourceExtension
  | OpName !IdRef !LiteralString
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpName
  | OpMemberName !IdRef !LiteralInteger32 !LiteralString
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpMemberName
  | OpString !IdResult !LiteralString
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpString
  | OpLine !IdRef !LiteralInteger32 !LiteralInteger32
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpLine
  | OpExtension !LiteralString
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpExtension
  | OpExtInstImport !IdResult !LiteralString
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpExtInstImport
  | OpExtInst !IdResultType !IdResult !IdRef !LiteralExtInstInteger ![IdRef]
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpExtInst
  | OpMemoryModel !AddressingModel !MemoryModel
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpMemoryModel
  | OpEntryPoint !ExecutionModel !IdRef !LiteralString ![IdRef]
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpEntryPoint
  | OpExecutionMode !IdRef !ExecutionMode
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpExecutionMode
  | OpCapability !Capability
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpCapability
  | OpTypeVoid !IdResult
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpTypeVoid
  | OpTypeBool !IdResult
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpTypeBool
  | OpTypeInt !IdResult !LiteralInteger32 !LiteralInteger32
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpTypeInt
  | OpTypeFloat !IdResult !LiteralInteger32
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpTypeFloat
  | OpTypeVector !IdResult !IdRef !LiteralInteger32
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpTypeVector
  | OpTypeMatrix !IdResult !IdRef !LiteralInteger32
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpTypeMatrix
  | OpTypeImage !IdResult !IdRef !Dim !LiteralInteger32 !LiteralInteger32 !LiteralInteger32 !LiteralInteger32 !ImageFormat !(Maybe AccessQualifier)
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpTypeImage
  | OpTypeSampler !IdResult
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpTypeSampler
  | OpTypeSampledImage !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpTypeSampledImage
  | OpTypeArray !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpTypeArray
  | OpTypeRuntimeArray !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpTypeRuntimeArray
  | OpTypeStruct !IdResult ![IdRef]
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpTypeStruct
  | OpTypeOpaque !IdResult !LiteralString
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpTypeOpaque
  | OpTypePointer !IdResult !StorageClass !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpTypePointer
  | OpTypeFunction !IdResult !IdRef ![IdRef]
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpTypeFunction
  | OpTypeEvent !IdResult
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpTypeEvent
  | OpTypeDeviceEvent !IdResult
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpTypeDeviceEvent
  | OpTypeReserveId !IdResult
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpTypeReserveId
  | OpTypeQueue !IdResult
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpTypeQueue
  | OpTypePipe !IdResult !AccessQualifier
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpTypePipe
  | OpTypeForwardPointer !IdRef !StorageClass
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpTypeForwardPointer
  | OpConstantTrue !IdResultType !IdResult
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpConstantTrue
  | OpConstantFalse !IdResultType !IdResult
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpConstantFalse
  | OpConstant !IdResultType !IdResult !LiteralContextDependentNumber
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpConstant
  | OpConstantComposite !IdResultType !IdResult ![IdRef]
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpConstantComposite
  | OpConstantSampler !IdResultType !IdResult !SamplerAddressingMode !LiteralInteger32 !SamplerFilterMode
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpConstantSampler
  | OpConstantNull !IdResultType !IdResult
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpConstantNull
  | OpSpecConstantTrue !IdResultType !IdResult
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpSpecConstantTrue
  | OpSpecConstantFalse !IdResultType !IdResult
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpSpecConstantFalse
  | OpSpecConstant !IdResultType !IdResult !LiteralContextDependentNumber
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpSpecConstant
  | OpSpecConstantComposite !IdResultType !IdResult ![IdRef]
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpSpecConstantComposite
  | OpSpecConstantOp !IdResultType !IdResult !LiteralSpecConstantOpInteger ![IdRef]
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpSpecConstantOp
  | OpFunction !IdResultType !IdResult ![FunctionControl] !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpFunction
  | OpFunctionParameter !IdResultType !IdResult
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpFunctionParameter
  | OpFunctionEnd
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpFunctionEnd
  | OpFunctionCall !IdResultType !IdResult !IdRef ![IdRef]
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpFunctionCall
  | OpVariable !IdResultType !IdResult !StorageClass !(Maybe IdRef)
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpVariable
  | OpImageTexelPointer !IdResultType !IdResult !IdRef !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpImageTexelPointer
  | OpLoad !IdResultType !IdResult !IdRef !(Maybe [MemoryAccess])
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpLoad
  | OpStore !IdRef !IdRef !(Maybe [MemoryAccess])
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpStore
  | OpCopyMemory !IdRef !IdRef !(Maybe [MemoryAccess])
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpCopyMemory
  | OpCopyMemorySized !IdRef !IdRef !IdRef !(Maybe [MemoryAccess])
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpCopyMemorySized
  | OpAccessChain !IdResultType !IdResult !IdRef ![IdRef]
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpAccessChain
  | OpInBoundsAccessChain !IdResultType !IdResult !IdRef ![IdRef]
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpInBoundsAccessChain
  | OpPtrAccessChain !IdResultType !IdResult !IdRef !IdRef ![IdRef]
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpPtrAccessChain
  | OpArrayLength !IdResultType !IdResult !IdRef !LiteralInteger32
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpArrayLength
  | OpGenericPtrMemSemantics !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpGenericPtrMemSemantics
  | OpInBoundsPtrAccessChain !IdResultType !IdResult !IdRef !IdRef ![IdRef]
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpInBoundsPtrAccessChain
  | OpDecorate !IdRef !Decoration
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpDecorate
  | OpMemberDecorate !IdRef !LiteralInteger32 !Decoration
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpMemberDecorate
  | OpDecorationGroup !IdResult
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpDecorationGroup
  | OpGroupDecorate !IdRef ![IdRef]
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpGroupDecorate
  | OpGroupMemberDecorate !IdRef ![PairIdRefLiteralInteger]
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpGroupMemberDecorate
  | OpVectorExtractDynamic !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpVectorExtractDynamic
  | OpVectorInsertDynamic !IdResultType !IdResult !IdRef !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpVectorInsertDynamic
  | OpVectorShuffle !IdResultType !IdResult !IdRef !IdRef ![LiteralInteger32]
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpVectorShuffle
  | OpCompositeConstruct !IdResultType !IdResult ![IdRef]
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpCompositeConstruct
  | OpCompositeExtract !IdResultType !IdResult !IdRef ![LiteralInteger32]
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpCompositeExtract
  | OpCompositeInsert !IdResultType !IdResult !IdRef !IdRef ![LiteralInteger32]
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpCompositeInsert
  | OpCopyObject !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpCopyObject
  | OpTranspose !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpTranspose
  | OpSampledImage !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpSampledImage
  | OpImageSampleImplicitLod !IdResultType !IdResult !IdRef !IdRef !(Maybe [ImageOperands])
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpImageSampleImplicitLod
  | OpImageSampleExplicitLod !IdResultType !IdResult !IdRef !IdRef ![ImageOperands]
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpImageSampleExplicitLod
  | OpImageSampleDrefImplicitLod !IdResultType !IdResult !IdRef !IdRef !IdRef !(Maybe [ImageOperands])
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpImageSampleDrefImplicitLod
  | OpImageSampleDrefExplicitLod !IdResultType !IdResult !IdRef !IdRef !IdRef ![ImageOperands]
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpImageSampleDrefExplicitLod
  | OpImageSampleProjImplicitLod !IdResultType !IdResult !IdRef !IdRef !(Maybe [ImageOperands])
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpImageSampleProjImplicitLod
  | OpImageSampleProjExplicitLod !IdResultType !IdResult !IdRef !IdRef ![ImageOperands]
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpImageSampleProjExplicitLod
  | OpImageSampleProjDrefImplicitLod !IdResultType !IdResult !IdRef !IdRef !IdRef !(Maybe [ImageOperands])
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpImageSampleProjDrefImplicitLod
  | OpImageSampleProjDrefExplicitLod !IdResultType !IdResult !IdRef !IdRef !IdRef ![ImageOperands]
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpImageSampleProjDrefExplicitLod
  | OpImageFetch !IdResultType !IdResult !IdRef !IdRef !(Maybe [ImageOperands])
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpImageFetch
  | OpImageGather !IdResultType !IdResult !IdRef !IdRef !IdRef !(Maybe [ImageOperands])
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpImageGather
  | OpImageDrefGather !IdResultType !IdResult !IdRef !IdRef !IdRef !(Maybe [ImageOperands])
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpImageDrefGather
  | OpImageRead !IdResultType !IdResult !IdRef !IdRef !(Maybe [ImageOperands])
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpImageRead
  | OpImageWrite !IdRef !IdRef !IdRef !(Maybe [ImageOperands])
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpImageWrite
  | OpImage !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpImage
  | OpImageQueryFormat !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpImageQueryFormat
  | OpImageQueryOrder !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpImageQueryOrder
  | OpImageQuerySizeLod !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpImageQuerySizeLod
  | OpImageQuerySize !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpImageQuerySize
  | OpImageQueryLod !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpImageQueryLod
  | OpImageQueryLevels !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpImageQueryLevels
  | OpImageQuerySamples !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpImageQuerySamples
  | OpConvertFToU !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpConvertFToU
  | OpConvertFToS !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpConvertFToS
  | OpConvertSToF !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpConvertSToF
  | OpConvertUToF !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpConvertUToF
  | OpUConvert !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpUConvert
  | OpSConvert !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpSConvert
  | OpFConvert !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpFConvert
  | OpQuantizeToF16 !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpQuantizeToF16
  | OpConvertPtrToU !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpConvertPtrToU
  | OpSatConvertSToU !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpSatConvertSToU
  | OpSatConvertUToS !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpSatConvertUToS
  | OpConvertUToPtr !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpConvertUToPtr
  | OpPtrCastToGeneric !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpPtrCastToGeneric
  | OpGenericCastToPtr !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpGenericCastToPtr
  | OpGenericCastToPtrExplicit !IdResultType !IdResult !IdRef !StorageClass
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpGenericCastToPtrExplicit
  | OpBitcast !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpBitcast
  | OpSNegate !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpSNegate
  | OpFNegate !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpFNegate
  | OpIAdd !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpIAdd
  | OpFAdd !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpFAdd
  | OpISub !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpISub
  | OpFSub !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpFSub
  | OpIMul !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpIMul
  | OpFMul !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpFMul
  | OpUDiv !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpUDiv
  | OpSDiv !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpSDiv
  | OpFDiv !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpFDiv
  | OpUMod !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpUMod
  | OpSRem !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpSRem
  | OpSMod !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpSMod
  | OpFRem !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpFRem
  | OpFMod !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpFMod
  | OpVectorTimesScalar !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpVectorTimesScalar
  | OpMatrixTimesScalar !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpMatrixTimesScalar
  | OpVectorTimesMatrix !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpVectorTimesMatrix
  | OpMatrixTimesVector !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpMatrixTimesVector
  | OpMatrixTimesMatrix !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpMatrixTimesMatrix
  | OpOuterProduct !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpOuterProduct
  | OpDot !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpDot
  | OpIAddCarry !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpIAddCarry
  | OpISubBorrow !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpISubBorrow
  | OpUMulExtended !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpUMulExtended
  | OpSMulExtended !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpSMulExtended
  | OpAny !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpAny
  | OpAll !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpAll
  | OpIsNan !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpIsNan
  | OpIsInf !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpIsInf
  | OpIsFinite !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpIsFinite
  | OpIsNormal !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpIsNormal
  | OpSignBitSet !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpSignBitSet
  | OpLessOrGreater !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpLessOrGreater
  | OpOrdered !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpOrdered
  | OpUnordered !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpUnordered
  | OpLogicalEqual !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpLogicalEqual
  | OpLogicalNotEqual !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpLogicalNotEqual
  | OpLogicalOr !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpLogicalOr
  | OpLogicalAnd !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpLogicalAnd
  | OpLogicalNot !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpLogicalNot
  | OpSelect !IdResultType !IdResult !IdRef !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpSelect
  | OpIEqual !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpIEqual
  | OpINotEqual !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpINotEqual
  | OpUGreaterThan !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpUGreaterThan
  | OpSGreaterThan !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpSGreaterThan
  | OpUGreaterThanEqual !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpUGreaterThanEqual
  | OpSGreaterThanEqual !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpSGreaterThanEqual
  | OpULessThan !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpULessThan
  | OpSLessThan !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpSLessThan
  | OpULessThanEqual !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpULessThanEqual
  | OpSLessThanEqual !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpSLessThanEqual
  | OpFOrdEqual !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpFOrdEqual
  | OpFUnordEqual !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpFUnordEqual
  | OpFOrdNotEqual !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpFOrdNotEqual
  | OpFUnordNotEqual !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpFUnordNotEqual
  | OpFOrdLessThan !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpFOrdLessThan
  | OpFUnordLessThan !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpFUnordLessThan
  | OpFOrdGreaterThan !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpFOrdGreaterThan
  | OpFUnordGreaterThan !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpFUnordGreaterThan
  | OpFOrdLessThanEqual !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpFOrdLessThanEqual
  | OpFUnordLessThanEqual !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpFUnordLessThanEqual
  | OpFOrdGreaterThanEqual !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpFOrdGreaterThanEqual
  | OpFUnordGreaterThanEqual !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpFUnordGreaterThanEqual
  | OpShiftRightLogical !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpShiftRightLogical
  | OpShiftRightArithmetic !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpShiftRightArithmetic
  | OpShiftLeftLogical !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpShiftLeftLogical
  | OpBitwiseOr !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpBitwiseOr
  | OpBitwiseXor !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpBitwiseXor
  | OpBitwiseAnd !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpBitwiseAnd
  | OpNot !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpNot
  | OpBitFieldInsert !IdResultType !IdResult !IdRef !IdRef !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpBitFieldInsert
  | OpBitFieldSExtract !IdResultType !IdResult !IdRef !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpBitFieldSExtract
  | OpBitFieldUExtract !IdResultType !IdResult !IdRef !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpBitFieldUExtract
  | OpBitReverse !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpBitReverse
  | OpBitCount !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpBitCount
  | OpDPdx !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpDPdx
  | OpDPdy !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpDPdy
  | OpFwidth !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpFwidth
  | OpDPdxFine !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpDPdxFine
  | OpDPdyFine !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpDPdyFine
  | OpFwidthFine !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpFwidthFine
  | OpDPdxCoarse !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpDPdxCoarse
  | OpDPdyCoarse !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpDPdyCoarse
  | OpFwidthCoarse !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpFwidthCoarse
  | OpEmitVertex
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpEmitVertex
  | OpEndPrimitive
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpEndPrimitive
  | OpEmitStreamVertex !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpEmitStreamVertex
  | OpEndStreamPrimitive !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpEndStreamPrimitive
  | OpControlBarrier !IdScope !IdScope !IdMemorySemantics
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpControlBarrier
  | OpMemoryBarrier !IdScope !IdMemorySemantics
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpMemoryBarrier
  | OpAtomicLoad !IdResultType !IdResult !IdRef !IdScope !IdMemorySemantics
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpAtomicLoad
  | OpAtomicStore !IdRef !IdScope !IdMemorySemantics !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpAtomicStore
  | OpAtomicExchange !IdResultType !IdResult !IdRef !IdScope !IdMemorySemantics !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpAtomicExchange
  | OpAtomicCompareExchange !IdResultType !IdResult !IdRef !IdScope !IdMemorySemantics !IdMemorySemantics !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpAtomicCompareExchange
  | OpAtomicCompareExchangeWeak !IdResultType !IdResult !IdRef !IdScope !IdMemorySemantics !IdMemorySemantics !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpAtomicCompareExchangeWeak
  | OpAtomicIIncrement !IdResultType !IdResult !IdRef !IdScope !IdMemorySemantics
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpAtomicIIncrement
  | OpAtomicIDecrement !IdResultType !IdResult !IdRef !IdScope !IdMemorySemantics
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpAtomicIDecrement
  | OpAtomicIAdd !IdResultType !IdResult !IdRef !IdScope !IdMemorySemantics !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpAtomicIAdd
  | OpAtomicISub !IdResultType !IdResult !IdRef !IdScope !IdMemorySemantics !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpAtomicISub
  | OpAtomicSMin !IdResultType !IdResult !IdRef !IdScope !IdMemorySemantics !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpAtomicSMin
  | OpAtomicUMin !IdResultType !IdResult !IdRef !IdScope !IdMemorySemantics !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpAtomicUMin
  | OpAtomicSMax !IdResultType !IdResult !IdRef !IdScope !IdMemorySemantics !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpAtomicSMax
  | OpAtomicUMax !IdResultType !IdResult !IdRef !IdScope !IdMemorySemantics !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpAtomicUMax
  | OpAtomicAnd !IdResultType !IdResult !IdRef !IdScope !IdMemorySemantics !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpAtomicAnd
  | OpAtomicOr !IdResultType !IdResult !IdRef !IdScope !IdMemorySemantics !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpAtomicOr
  | OpAtomicXor !IdResultType !IdResult !IdRef !IdScope !IdMemorySemantics !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpAtomicXor
  | OpPhi !IdResultType !IdResult ![PairIdRefIdRef]
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpPhi
  | OpLoopMerge !IdRef !IdRef ![LoopControl]
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpLoopMerge
  | OpSelectionMerge !IdRef ![SelectionControl]
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpSelectionMerge
  | OpLabel !IdResult
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpLabel
  | OpBranch !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpBranch
  | OpBranchConditional !IdRef !IdRef !IdRef ![LiteralInteger32]
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpBranchConditional
  | OpSwitch !IdRef !IdRef ![PairLiteralIntegerIdRef]
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpSwitch
  | OpKill
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpKill
  | OpReturn
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpReturn
  | OpReturnValue !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpReturnValue
  | OpUnreachable
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpUnreachable
  | OpLifetimeStart !IdRef !LiteralInteger32
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpLifetimeStart
  | OpLifetimeStop !IdRef !LiteralInteger32
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpLifetimeStop
  | OpGroupAsyncCopy !IdResultType !IdResult !IdScope !IdRef !IdRef !IdRef !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpGroupAsyncCopy
  | OpGroupWaitEvents !IdScope !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpGroupWaitEvents
  | OpGroupAll !IdResultType !IdResult !IdScope !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpGroupAll
  | OpGroupAny !IdResultType !IdResult !IdScope !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpGroupAny
  | OpGroupBroadcast !IdResultType !IdResult !IdScope !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpGroupBroadcast
  | OpGroupIAdd !IdResultType !IdResult !IdScope !GroupOperation !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpGroupIAdd
  | OpGroupFAdd !IdResultType !IdResult !IdScope !GroupOperation !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpGroupFAdd
  | OpGroupFMin !IdResultType !IdResult !IdScope !GroupOperation !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpGroupFMin
  | OpGroupUMin !IdResultType !IdResult !IdScope !GroupOperation !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpGroupUMin
  | OpGroupSMin !IdResultType !IdResult !IdScope !GroupOperation !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpGroupSMin
  | OpGroupFMax !IdResultType !IdResult !IdScope !GroupOperation !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpGroupFMax
  | OpGroupUMax !IdResultType !IdResult !IdScope !GroupOperation !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpGroupUMax
  | OpGroupSMax !IdResultType !IdResult !IdScope !GroupOperation !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpGroupSMax
  | OpReadPipe !IdResultType !IdResult !IdRef !IdRef !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpReadPipe
  | OpWritePipe !IdResultType !IdResult !IdRef !IdRef !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpWritePipe
  | OpReservedReadPipe !IdResultType !IdResult !IdRef !IdRef !IdRef !IdRef !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpReservedReadPipe
  | OpReservedWritePipe !IdResultType !IdResult !IdRef !IdRef !IdRef !IdRef !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpReservedWritePipe
  | OpReserveReadPipePackets !IdResultType !IdResult !IdRef !IdRef !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpReserveReadPipePackets
  | OpReserveWritePipePackets !IdResultType !IdResult !IdRef !IdRef !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpReserveWritePipePackets
  | OpCommitReadPipe !IdRef !IdRef !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpCommitReadPipe
  | OpCommitWritePipe !IdRef !IdRef !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpCommitWritePipe
  | OpIsValidReserveId !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpIsValidReserveId
  | OpGetNumPipePackets !IdResultType !IdResult !IdRef !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpGetNumPipePackets
  | OpGetMaxPipePackets !IdResultType !IdResult !IdRef !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpGetMaxPipePackets
  | OpGroupReserveReadPipePackets !IdResultType !IdResult !IdScope !IdRef !IdRef !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpGroupReserveReadPipePackets
  | OpGroupReserveWritePipePackets !IdResultType !IdResult !IdScope !IdRef !IdRef !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpGroupReserveWritePipePackets
  | OpGroupCommitReadPipe !IdScope !IdRef !IdRef !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpGroupCommitReadPipe
  | OpGroupCommitWritePipe !IdScope !IdRef !IdRef !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpGroupCommitWritePipe
  | OpEnqueueMarker !IdResultType !IdResult !IdRef !IdRef !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpEnqueueMarker
  | OpEnqueueKernel !IdResultType !IdResult !IdRef !IdRef !IdRef !IdRef !IdRef !IdRef !IdRef !IdRef !IdRef !IdRef ![IdRef]
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpEnqueueKernel
  | OpGetKernelNDrangeSubGroupCount !IdResultType !IdResult !IdRef !IdRef !IdRef !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpGetKernelNDrangeSubGroupCount
  | OpGetKernelNDrangeMaxSubGroupSize !IdResultType !IdResult !IdRef !IdRef !IdRef !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpGetKernelNDrangeMaxSubGroupSize
  | OpGetKernelWorkGroupSize !IdResultType !IdResult !IdRef !IdRef !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpGetKernelWorkGroupSize
  | OpGetKernelPreferredWorkGroupSizeMultiple !IdResultType !IdResult !IdRef !IdRef !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpGetKernelPreferredWorkGroupSizeMultiple
  | OpRetainEvent !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpRetainEvent
  | OpReleaseEvent !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpReleaseEvent
  | OpCreateUserEvent !IdResultType !IdResult
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpCreateUserEvent
  | OpIsValidEvent !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpIsValidEvent
  | OpSetUserEventStatus !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpSetUserEventStatus
  | OpCaptureEventProfilingInfo !IdRef !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpCaptureEventProfilingInfo
  | OpGetDefaultQueue !IdResultType !IdResult
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpGetDefaultQueue
  | OpBuildNDRange !IdResultType !IdResult !IdRef !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpBuildNDRange
  | OpImageSparseSampleImplicitLod !IdResultType !IdResult !IdRef !IdRef !(Maybe [ImageOperands])
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpImageSparseSampleImplicitLod
  | OpImageSparseSampleExplicitLod !IdResultType !IdResult !IdRef !IdRef ![ImageOperands]
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpImageSparseSampleExplicitLod
  | OpImageSparseSampleDrefImplicitLod !IdResultType !IdResult !IdRef !IdRef !IdRef !(Maybe [ImageOperands])
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpImageSparseSampleDrefImplicitLod
  | OpImageSparseSampleDrefExplicitLod !IdResultType !IdResult !IdRef !IdRef !IdRef ![ImageOperands]
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpImageSparseSampleDrefExplicitLod
  | OpImageSparseSampleProjImplicitLod !IdResultType !IdResult !IdRef !IdRef !(Maybe [ImageOperands])
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpImageSparseSampleProjImplicitLod
  | OpImageSparseSampleProjExplicitLod !IdResultType !IdResult !IdRef !IdRef ![ImageOperands]
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpImageSparseSampleProjExplicitLod
  | OpImageSparseSampleProjDrefImplicitLod !IdResultType !IdResult !IdRef !IdRef !IdRef !(Maybe [ImageOperands])
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpImageSparseSampleProjDrefImplicitLod
  | OpImageSparseSampleProjDrefExplicitLod !IdResultType !IdResult !IdRef !IdRef !IdRef ![ImageOperands]
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpImageSparseSampleProjDrefExplicitLod
  | OpImageSparseFetch !IdResultType !IdResult !IdRef !IdRef !(Maybe [ImageOperands])
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpImageSparseFetch
  | OpImageSparseGather !IdResultType !IdResult !IdRef !IdRef !IdRef !(Maybe [ImageOperands])
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpImageSparseGather
  | OpImageSparseDrefGather !IdResultType !IdResult !IdRef !IdRef !IdRef !(Maybe [ImageOperands])
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpImageSparseDrefGather
  | OpImageSparseTexelsResident !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpImageSparseTexelsResident
  | OpNoLine
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpNoLine
  | OpAtomicFlagTestAndSet !IdResultType !IdResult !IdRef !IdScope !IdMemorySemantics
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpAtomicFlagTestAndSet
  | OpAtomicFlagClear !IdRef !IdScope !IdMemorySemantics
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpAtomicFlagClear
  | OpImageSparseRead !IdResultType !IdResult !IdRef !IdRef !(Maybe [ImageOperands])
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpImageSparseRead
  | OpSizeOf !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpSizeOf
  | OpTypePipeStorage !IdResult
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpTypePipeStorage
  | OpConstantPipeStorage !IdResultType !IdResult !LiteralInteger32 !LiteralInteger32 !LiteralInteger32
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpConstantPipeStorage
  | OpCreatePipeFromPipeStorage !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpCreatePipeFromPipeStorage
  | OpGetKernelLocalSizeForSubgroupCount !IdResultType !IdResult !IdRef !IdRef !IdRef !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpGetKernelLocalSizeForSubgroupCount
  | OpGetKernelMaxNumSubgroups !IdResultType !IdResult !IdRef !IdRef !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpGetKernelMaxNumSubgroups
  | OpTypeNamedBarrier !IdResult
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpTypeNamedBarrier
  | OpNamedBarrierInitialize !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpNamedBarrierInitialize
  | OpMemoryNamedBarrier !IdRef !IdScope !IdMemorySemantics
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpMemoryNamedBarrier
  | OpModuleProcessed !LiteralString
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpModuleProcessed
  | OpExecutionModeId !IdRef !ExecutionMode
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpExecutionModeId
  | OpDecorateId !IdRef !Decoration
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpDecorateId
  | OpSubgroupBallotKHR !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpSubgroupBallotKHR
  | OpSubgroupFirstInvocationKHR !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpSubgroupFirstInvocationKHR
  | OpSubgroupAllKHR !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpSubgroupAllKHR
  | OpSubgroupAnyKHR !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpSubgroupAnyKHR
  | OpSubgroupAllEqualKHR !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpSubgroupAllEqualKHR
  | OpSubgroupReadInvocationKHR !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpSubgroupReadInvocationKHR
  | OpGroupIAddNonUniformAMD !IdResultType !IdResult !IdScope !GroupOperation !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpGroupIAddNonUniformAMD
  | OpGroupFAddNonUniformAMD !IdResultType !IdResult !IdScope !GroupOperation !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpGroupFAddNonUniformAMD
  | OpGroupFMinNonUniformAMD !IdResultType !IdResult !IdScope !GroupOperation !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpGroupFMinNonUniformAMD
  | OpGroupUMinNonUniformAMD !IdResultType !IdResult !IdScope !GroupOperation !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpGroupUMinNonUniformAMD
  | OpGroupSMinNonUniformAMD !IdResultType !IdResult !IdScope !GroupOperation !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpGroupSMinNonUniformAMD
  | OpGroupFMaxNonUniformAMD !IdResultType !IdResult !IdScope !GroupOperation !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpGroupFMaxNonUniformAMD
  | OpGroupUMaxNonUniformAMD !IdResultType !IdResult !IdScope !GroupOperation !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpGroupUMaxNonUniformAMD
  | OpGroupSMaxNonUniformAMD !IdResultType !IdResult !IdScope !GroupOperation !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpGroupSMaxNonUniformAMD
  | OpFragmentMaskFetchAMD !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpFragmentMaskFetchAMD
  | OpFragmentFetchAMD !IdResultType !IdResult !IdRef !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpFragmentFetchAMD
  | OpSubgroupShuffleINTEL !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpSubgroupShuffleINTEL
  | OpSubgroupShuffleDownINTEL !IdResultType !IdResult !IdRef !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpSubgroupShuffleDownINTEL
  | OpSubgroupShuffleUpINTEL !IdResultType !IdResult !IdRef !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpSubgroupShuffleUpINTEL
  | OpSubgroupShuffleXorINTEL !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpSubgroupShuffleXorINTEL
  | OpSubgroupBlockReadINTEL !IdResultType !IdResult !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpSubgroupBlockReadINTEL
  | OpSubgroupBlockWriteINTEL !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpSubgroupBlockWriteINTEL
  | OpSubgroupImageBlockReadINTEL !IdResultType !IdResult !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpSubgroupImageBlockReadINTEL
  | OpSubgroupImageBlockWriteINTEL !IdRef !IdRef !IdRef
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpSubgroupImageBlockWriteINTEL
  | OpDecorateStringGOOGLE !IdRef !Decoration
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpDecorateStringGOOGLE
  | OpMemberDecorateStringGOOGLE !IdRef !LiteralInteger32 !Decoration
        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#OpMemberDecorateStringGOOGLE
  | OpRAW !Word16 ![Word32]
    -- ^ This represents an unrecognized SPIR-V operation (e.g. from a newer version of SPIR-V?)
  deriving (Show,Eq)

-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#ImageOperands
data ImageOperands =
    IO_None
      -- ^ 0x0
  | IO_Bias                   !IdRef
      -- ^ 0x1  ()
  | IO_Lod                    !IdRef
      -- ^ 0x2  ()
  | IO_Grad                   !IdRef !IdRef
      -- ^ 0x4  (, )
  | IO_ConstOffset            !IdRef
      -- ^ 0x8  ()
  | IO_Offset                 !IdRef
      -- ^ 0x10  ()
  | IO_ConstOffsets           !IdRef
      -- ^ 0x20  ()
  | IO_Sample                 !IdRef
      -- ^ 0x40  ()
  | IO_MinLod                 !IdRef
      -- ^ 0x80  ()
  | IO_UNKNOWN !Word32 ![Word32]
  deriving (Show,Eq,Ord)

-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#FPFastMathMode
data FPFastMathMode =
    FPFMM_None
      -- ^ 0x0
  | FPFMM_NotNaN
      -- ^ 0x1
  | FPFMM_NotInf
      -- ^ 0x2
  | FPFMM_NSZ
      -- ^ 0x4
  | FPFMM_AllowRecip
      -- ^ 0x8
  | FPFMM_Fast
      -- ^ 0x10
  | FPFMM_UNKNOWN !Word32 ![Word32]
  deriving (Show,Eq,Ord)

-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#SelectionControl
data SelectionControl =
    SC_None
      -- ^ 0x0
  | SC_Flatten
      -- ^ 0x1
  | SC_DontFlatten
      -- ^ 0x2
  | SelectionControl_UNKNOWN !Word32 ![Word32]
  deriving (Show,Eq,Ord)

-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#LoopControl
data LoopControl =
    LC_None
      -- ^ 0x0
  | LC_Unroll
      -- ^ 0x1
  | LC_DontUnroll
      -- ^ 0x2
  | LC_DependencyInfinite
      -- ^ 0x4
  | LC_DependencyLength       !LiteralInteger32
      -- ^ 0x8  ()
  | LC_UNKNOWN !Word32 ![Word32]
  deriving (Show,Eq,Ord)

-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#FunctionControl
data FunctionControl =
    FC_None
      -- ^ 0x0
  | FC_Inline
      -- ^ 0x1
  | FC_DontInline
      -- ^ 0x2
  | FC_Pure
      -- ^ 0x4
  | FC_Const
      -- ^ 0x8
  | FC_UNKNOWN !Word32 ![Word32]
  deriving (Show,Eq,Ord)

-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#MemorySemantics
data MemorySemantics =
    MS_Relaxed
      -- ^ 0x0
  | MS_None
      -- ^ 0x0
  | MS_Acquire
      -- ^ 0x2
  | MS_Release
      -- ^ 0x4
  | MS_AcquireRelease
      -- ^ 0x8
  | MS_SequentiallyConsistent
      -- ^ 0x10
  | MS_UniformMemory
      -- ^ 0x40
  | MS_SubgroupMemory
      -- ^ 0x80
  | MS_WorkgroupMemory
      -- ^ 0x100
  | MS_CrossWorkgroupMemory
      -- ^ 0x200
  | MS_AtomicCounterMemory
      -- ^ 0x400
  | MS_ImageMemory
      -- ^ 0x800
  | MS_UNKNOWN !Word32 ![Word32]
  deriving (Show,Eq,Ord)

-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#MemoryAccess
data MemoryAccess =
    MA_None
      -- ^ 0x0
  | MA_Volatile
      -- ^ 0x1
  | MA_Aligned                !LiteralInteger32
      -- ^ 0x2  ()
  | MA_Nontemporal
      -- ^ 0x4
  | MA_UNKNOWN !Word32 ![Word32]
  deriving (Show,Eq,Ord)

-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#KernelProfilingInfo
data KernelProfilingInfo =
    KPI_None
      -- ^ 0x0
  | KPI_CmdExecTime
      -- ^ 0x1
  | KPI_UNKNOWN !Word32 ![Word32]
  deriving (Show,Eq,Ord)

-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#SourceLanguage
data SourceLanguage =
    SL_Unknown
      -- ^ 0x0
  | SL_ESSL
      -- ^ 0x1
  | SL_GLSL
      -- ^ 0x2
  | SL_OpenCL_C
      -- ^ 0x3
  | SL_OpenCL_CPP
      -- ^ 0x4
  | SL_HLSL
      -- ^ 0x5
  | SL_UNKNOWN !Word32 ![Word32]
  deriving (Show,Eq,Ord)

-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#ExecutionModel
data ExecutionModel =
    EM_Vertex
      -- ^ 0x0
  | EM_TessellationControl
      -- ^ 0x1
  | EM_TessellationEvaluation
      -- ^ 0x2
  | EM_Geometry
      -- ^ 0x3
  | EM_Fragment
      -- ^ 0x4
  | EM_GLCompute
      -- ^ 0x5
  | EM_Kernel
      -- ^ 0x6
  | ExecutionModel_UNKNOWN !Word32 ![Word32]
  deriving (Show,Eq,Ord)

-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#AddressingModel
data AddressingModel =
    AM_Logical
      -- ^ 0x0
  | AM_Physical32
      -- ^ 0x1
  | AM_Physical64
      -- ^ 0x2
  | AM_UNKNOWN !Word32 ![Word32]
  deriving (Show,Eq,Ord)

-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#MemoryModel
data MemoryModel =
    MM_Simple
      -- ^ 0x0
  | MM_GLSL450
      -- ^ 0x1
  | MM_OpenCL
      -- ^ 0x2
  | MM_UNKNOWN !Word32 ![Word32]
  deriving (Show,Eq,Ord)

-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#ExecutionMode
data ExecutionMode =
    EM_Invocations            !LiteralInteger32
      -- ^ 0x0  ('Number of <<Invocation,invocations>>')
  | EM_SpacingEqual
      -- ^ 0x1
  | EM_SpacingFractionalEven
      -- ^ 0x2
  | EM_SpacingFractionalOdd
      -- ^ 0x3
  | EM_VertexOrderCw
      -- ^ 0x4
  | EM_VertexOrderCcw
      -- ^ 0x5
  | EM_PixelCenterInteger
      -- ^ 0x6
  | EM_OriginUpperLeft
      -- ^ 0x7
  | EM_OriginLowerLeft
      -- ^ 0x8
  | EM_EarlyFragmentTests
      -- ^ 0x9
  | EM_PointMode
      -- ^ 0xA
  | EM_Xfb
      -- ^ 0xB
  | EM_DepthReplacing
      -- ^ 0xC
  | EM_DepthGreater
      -- ^ 0xE
  | EM_DepthLess
      -- ^ 0xF
  | EM_DepthUnchanged
      -- ^ 0x10
  | EM_LocalSize              !LiteralInteger32 !LiteralInteger32 !LiteralInteger32
      -- ^ 0x11  ('x size', 'y size', 'z size')
  | EM_LocalSizeHint          !LiteralInteger32 !LiteralInteger32 !LiteralInteger32
      -- ^ 0x12  ('x size', 'y size', 'z size')
  | EM_InputPoints
      -- ^ 0x13
  | EM_InputLines
      -- ^ 0x14
  | EM_InputLinesAdjacency
      -- ^ 0x15
  | EM_Triangles
      -- ^ 0x16
  | EM_InputTrianglesAdjacency
      -- ^ 0x17
  | EM_Quads
      -- ^ 0x18
  | EM_Isolines
      -- ^ 0x19
  | EM_OutputVertices         !LiteralInteger32
      -- ^ 0x1A  ('Vertex count')
  | EM_OutputPoints
      -- ^ 0x1B
  | EM_OutputLineStrip
      -- ^ 0x1C
  | EM_OutputTriangleStrip
      -- ^ 0x1D
  | EM_VecTypeHint            !LiteralInteger32
      -- ^ 0x1E  ('Vector type')
  | EM_ContractionOff
      -- ^ 0x1F
  | EM_Initializer
      -- ^ 0x21
  | EM_Finalizer
      -- ^ 0x22
  | EM_SubgroupSize           !LiteralInteger32
      -- ^ 0x23  ('Subgroup Size')
  | EM_SubgroupsPerWorkgroup  !LiteralInteger32
      -- ^ 0x24  ('Subgroups Per Workgroup')
  | EM_SubgroupsPerWorkgroupId  !IdRef
      -- ^ 0x25  ('Subgroups Per Workgroup')
  | EM_LocalSizeId            !IdRef !IdRef !IdRef
      -- ^ 0x26  ('x size', 'y size', 'z size')
  | EM_LocalSizeHintId        !IdRef
      -- ^ 0x27  ('Local Size Hint')
  | EM_PostDepthCoverage
      -- ^ 0x115E
  | EM_StencilRefReplacingEXT
      -- ^ 0x13A3
  | EM_UNKNOWN !Word32 ![Word32]
  deriving (Show,Eq,Ord)

-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#StorageClass
data StorageClass =
    SC_UniformConstant
      -- ^ 0x0
  | SC_Input
      -- ^ 0x1
  | SC_Uniform
      -- ^ 0x2
  | SC_Output
      -- ^ 0x3
  | SC_Workgroup
      -- ^ 0x4
  | SC_CrossWorkgroup
      -- ^ 0x5
  | SC_Private
      -- ^ 0x6
  | SC_Function
      -- ^ 0x7
  | SC_Generic
      -- ^ 0x8
  | SC_PushConstant
      -- ^ 0x9
  | SC_AtomicCounter
      -- ^ 0xA
  | SC_Image
      -- ^ 0xB
  | SC_StorageBuffer
      -- ^ 0xC
  | SC_UNKNOWN !Word32 ![Word32]
  deriving (Show,Eq,Ord)

-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#Dim
data Dim =
    D_1D
      -- ^ 0x0
  | D_2D
      -- ^ 0x1
  | D_3D
      -- ^ 0x2
  | D_Cube
      -- ^ 0x3
  | D_Rect
      -- ^ 0x4
  | D_Buffer
      -- ^ 0x5
  | D_SubpassData
      -- ^ 0x6
  | D_UNKNOWN !Word32 ![Word32]
  deriving (Show,Eq,Ord)

-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#SamplerAddressingMode
data SamplerAddressingMode =
    SAM_None
      -- ^ 0x0
  | SAM_ClampToEdge
      -- ^ 0x1
  | SAM_Clamp
      -- ^ 0x2
  | SAM_Repeat
      -- ^ 0x3
  | SAM_RepeatMirrored
      -- ^ 0x4
  | SAM_UNKNOWN !Word32 ![Word32]
  deriving (Show,Eq,Ord)

-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#SamplerFilterMode
data SamplerFilterMode =
    SFM_Nearest
      -- ^ 0x0
  | SFM_Linear
      -- ^ 0x1
  | SFM_UNKNOWN !Word32 ![Word32]
  deriving (Show,Eq,Ord)

-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#ImageFormat
data ImageFormat =
    IF_Unknown
      -- ^ 0x0
  | IF_Rgba32f
      -- ^ 0x1
  | IF_Rgba16f
      -- ^ 0x2
  | IF_R32f
      -- ^ 0x3
  | IF_Rgba8
      -- ^ 0x4
  | IF_Rgba8Snorm
      -- ^ 0x5
  | IF_Rg32f
      -- ^ 0x6
  | IF_Rg16f
      -- ^ 0x7
  | IF_R11fG11fB10f
      -- ^ 0x8
  | IF_R16f
      -- ^ 0x9
  | IF_Rgba16
      -- ^ 0xA
  | IF_Rgb10A2
      -- ^ 0xB
  | IF_Rg16
      -- ^ 0xC
  | IF_Rg8
      -- ^ 0xD
  | IF_R16
      -- ^ 0xE
  | IF_R8
      -- ^ 0xF
  | IF_Rgba16Snorm
      -- ^ 0x10
  | IF_Rg16Snorm
      -- ^ 0x11
  | IF_Rg8Snorm
      -- ^ 0x12
  | IF_R16Snorm
      -- ^ 0x13
  | IF_R8Snorm
      -- ^ 0x14
  | IF_Rgba32i
      -- ^ 0x15
  | IF_Rgba16i
      -- ^ 0x16
  | IF_Rgba8i
      -- ^ 0x17
  | IF_R32i
      -- ^ 0x18
  | IF_Rg32i
      -- ^ 0x19
  | IF_Rg16i
      -- ^ 0x1A
  | IF_Rg8i
      -- ^ 0x1B
  | IF_R16i
      -- ^ 0x1C
  | IF_R8i
      -- ^ 0x1D
  | IF_Rgba32ui
      -- ^ 0x1E
  | IF_Rgba16ui
      -- ^ 0x1F
  | IF_Rgba8ui
      -- ^ 0x20
  | IF_R32ui
      -- ^ 0x21
  | IF_Rgb10a2ui
      -- ^ 0x22
  | IF_Rg32ui
      -- ^ 0x23
  | IF_Rg16ui
      -- ^ 0x24
  | IF_Rg8ui
      -- ^ 0x25
  | IF_R16ui
      -- ^ 0x26
  | IF_R8ui
      -- ^ 0x27
  | IF_UNKNOWN !Word32 ![Word32]
  deriving (Show,Eq,Ord)

-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#ImageChannelOrder
data ImageChannelOrder =
    ICO_R
      -- ^ 0x0
  | ICO_A
      -- ^ 0x1
  | ICO_RG
      -- ^ 0x2
  | ICO_RA
      -- ^ 0x3
  | ICO_RGB
      -- ^ 0x4
  | ICO_RGBA
      -- ^ 0x5
  | ICO_BGRA
      -- ^ 0x6
  | ICO_ARGB
      -- ^ 0x7
  | ICO_Intensity
      -- ^ 0x8
  | ICO_Luminance
      -- ^ 0x9
  | ICO_Rx
      -- ^ 0xA
  | ICO_RGx
      -- ^ 0xB
  | ICO_RGBx
      -- ^ 0xC
  | ICO_Depth
      -- ^ 0xD
  | ICO_DepthStencil
      -- ^ 0xE
  | ICO_sRGB
      -- ^ 0xF
  | ICO_sRGBx
      -- ^ 0x10
  | ICO_sRGBA
      -- ^ 0x11
  | ICO_sBGRA
      -- ^ 0x12
  | ICO_ABGR
      -- ^ 0x13
  | ICO_UNKNOWN !Word32 ![Word32]
  deriving (Show,Eq,Ord)

-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#ImageChannelDataType
data ImageChannelDataType =
    ICDT_SnormInt8
      -- ^ 0x0
  | ICDT_SnormInt16
      -- ^ 0x1
  | ICDT_UnormInt8
      -- ^ 0x2
  | ICDT_UnormInt16
      -- ^ 0x3
  | ICDT_UnormShort565
      -- ^ 0x4
  | ICDT_UnormShort555
      -- ^ 0x5
  | ICDT_UnormInt101010
      -- ^ 0x6
  | ICDT_SignedInt8
      -- ^ 0x7
  | ICDT_SignedInt16
      -- ^ 0x8
  | ICDT_SignedInt32
      -- ^ 0x9
  | ICDT_UnsignedInt8
      -- ^ 0xA
  | ICDT_UnsignedInt16
      -- ^ 0xB
  | ICDT_UnsignedInt32
      -- ^ 0xC
  | ICDT_HalfFloat
      -- ^ 0xD
  | ICDT_Float
      -- ^ 0xE
  | ICDT_UnormInt24
      -- ^ 0xF
  | ICDT_UnormInt101010_2
      -- ^ 0x10
  | ICDT_UNKNOWN !Word32 ![Word32]
  deriving (Show,Eq,Ord)

-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#FPRoundingMode
data FPRoundingMode =
    FPRM_RTE
      -- ^ 0x0
  | FPRM_RTZ
      -- ^ 0x1
  | FPRM_RTP
      -- ^ 0x2
  | FPRM_RTN
      -- ^ 0x3
  | FPRM_UNKNOWN !Word32 ![Word32]
  deriving (Show,Eq,Ord)

-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#LinkageType
data LinkageType =
    LT_Export
      -- ^ 0x0
  | LT_Import
      -- ^ 0x1
  | LT_UNKNOWN !Word32 ![Word32]
  deriving (Show,Eq,Ord)

-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#AccessQualifier
data AccessQualifier =
    AQ_ReadOnly
      -- ^ 0x0
  | AQ_WriteOnly
      -- ^ 0x1
  | AQ_ReadWrite
      -- ^ 0x2
  | AQ_UNKNOWN !Word32 ![Word32]
  deriving (Show,Eq,Ord)

-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#FunctionParameterAttribute
data FunctionParameterAttribute =
    FPA_Zext
      -- ^ 0x0
  | FPA_Sext
      -- ^ 0x1
  | FPA_ByVal
      -- ^ 0x2
  | FPA_Sret
      -- ^ 0x3
  | FPA_NoAlias
      -- ^ 0x4
  | FPA_NoCapture
      -- ^ 0x5
  | FPA_NoWrite
      -- ^ 0x6
  | FPA_NoReadWrite
      -- ^ 0x7
  | FPA_UNKNOWN !Word32 ![Word32]
  deriving (Show,Eq,Ord)

-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#Decoration
data Decoration =
    D_RelaxedPrecision
      -- ^ 0x0
  | D_SpecId                  !LiteralInteger32
      -- ^ 0x1  ('Specialization Constant ID')
  | D_Block
      -- ^ 0x2
  | D_BufferBlock
      -- ^ 0x3
  | D_RowMajor
      -- ^ 0x4
  | D_ColMajor
      -- ^ 0x5
  | D_ArrayStride             !LiteralInteger32
      -- ^ 0x6  ('Array Stride')
  | D_MatrixStride            !LiteralInteger32
      -- ^ 0x7  ('Matrix Stride')
  | D_GLSLShared
      -- ^ 0x8
  | D_GLSLPacked
      -- ^ 0x9
  | D_CPacked
      -- ^ 0xA
  | D_BuiltIn                 !BuiltIn
      -- ^ 0xB  ()
  | D_NoPerspective
      -- ^ 0xD
  | D_Flat
      -- ^ 0xE
  | D_Patch
      -- ^ 0xF
  | D_Centroid
      -- ^ 0x10
  | D_Sample
      -- ^ 0x11
  | D_Invariant
      -- ^ 0x12
  | D_Restrict
      -- ^ 0x13
  | D_Aliased
      -- ^ 0x14
  | D_Volatile
      -- ^ 0x15
  | D_Constant
      -- ^ 0x16
  | D_Coherent
      -- ^ 0x17
  | D_NonWritable
      -- ^ 0x18
  | D_NonReadable
      -- ^ 0x19
  | D_Uniform
      -- ^ 0x1A
  | D_SaturatedConversion
      -- ^ 0x1C
  | D_Stream                  !LiteralInteger32
      -- ^ 0x1D  ('Stream Number')
  | D_Location                !LiteralInteger32
      -- ^ 0x1E  ('Location')
  | D_Component               !LiteralInteger32
      -- ^ 0x1F  ('Component')
  | D_Index                   !LiteralInteger32
      -- ^ 0x20  ('Index')
  | D_Binding                 !LiteralInteger32
      -- ^ 0x21  ('Binding Point')
  | D_DescriptorSet           !LiteralInteger32
      -- ^ 0x22  ('Descriptor Set')
  | D_Offset                  !LiteralInteger32
      -- ^ 0x23  ('Byte Offset')
  | D_XfbBuffer               !LiteralInteger32
      -- ^ 0x24  ('XFB Buffer Number')
  | D_XfbStride               !LiteralInteger32
      -- ^ 0x25  ('XFB Stride')
  | D_FuncParamAttr           !FunctionParameterAttribute
      -- ^ 0x26  ('Function Parameter Attribute')
  | D_FPRoundingMode          !FPRoundingMode
      -- ^ 0x27  ('Floating-Point Rounding Mode')
  | D_FPFastMathMode          ![FPFastMathMode]
      -- ^ 0x28  ('Fast-Math Mode')
  | D_LinkageAttributes       !LiteralString !LinkageType
      -- ^ 0x29  ('Name', 'Linkage Type')
  | D_NoContraction
      -- ^ 0x2A
  | D_InputAttachmentIndex    !LiteralInteger32
      -- ^ 0x2B  ('Attachment Index')
  | D_Alignment               !LiteralInteger32
      -- ^ 0x2C  ('Alignment')
  | D_MaxByteOffset           !LiteralInteger64
      -- ^ 0x2D  ('Max Byte Offset')
  | D_AlignmentId             !IdRef
      -- ^ 0x2E  ('Alignment')
  | D_MaxByteOffsetId         !IdRef
      -- ^ 0x2F  ('Max Byte Offset')
  | D_ExplicitInterpAMD
      -- ^ 0x1387
  | D_OverrideCoverageNV
      -- ^ 0x1480
  | D_PassthroughNV
      -- ^ 0x1482
  | D_ViewportRelativeNV
      -- ^ 0x1484
  | D_SecondaryViewportRelativeNV  !LiteralInteger32
      -- ^ 0x1488  ('Offset')
  | D_HlslCounterBufferGOOGLE  !IdRef
      -- ^ 0x1602  ('Counter Buffer')
  | D_HlslSemanticGOOGLE      !LiteralString
      -- ^ 0x1603  ('Semantic')
  | Decoration_UNKNOWN !Word32 ![Word32]
  deriving (Show,Eq,Ord)

-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#BuiltIn
data BuiltIn =
    BI_Position
      -- ^ 0x0
  | BI_PointSize
      -- ^ 0x1
  | BI_ClipDistance
      -- ^ 0x3
  | BI_CullDistance
      -- ^ 0x4
  | BI_VertexId
      -- ^ 0x5
  | BI_InstanceId
      -- ^ 0x6
  | BI_PrimitiveId
      -- ^ 0x7
  | BI_InvocationId
      -- ^ 0x8
  | BI_Layer
      -- ^ 0x9
  | BI_ViewportIndex
      -- ^ 0xA
  | BI_TessLevelOuter
      -- ^ 0xB
  | BI_TessLevelInner
      -- ^ 0xC
  | BI_TessCoord
      -- ^ 0xD
  | BI_PatchVertices
      -- ^ 0xE
  | BI_FragCoord
      -- ^ 0xF
  | BI_PointCoord
      -- ^ 0x10
  | BI_FrontFacing
      -- ^ 0x11
  | BI_SampleId
      -- ^ 0x12
  | BI_SamplePosition
      -- ^ 0x13
  | BI_SampleMask
      -- ^ 0x14
  | BI_FragDepth
      -- ^ 0x16
  | BI_HelperInvocation
      -- ^ 0x17
  | BI_NumWorkgroups
      -- ^ 0x18
  | BI_WorkgroupSize
      -- ^ 0x19
  | BI_WorkgroupId
      -- ^ 0x1A
  | BI_LocalInvocationId
      -- ^ 0x1B
  | BI_GlobalInvocationId
      -- ^ 0x1C
  | BI_LocalInvocationIndex
      -- ^ 0x1D
  | BI_WorkDim
      -- ^ 0x1E
  | BI_GlobalSize
      -- ^ 0x1F
  | BI_EnqueuedWorkgroupSize
      -- ^ 0x20
  | BI_GlobalOffset
      -- ^ 0x21
  | BI_GlobalLinearId
      -- ^ 0x22
  | BI_SubgroupSize
      -- ^ 0x24
  | BI_SubgroupMaxSize
      -- ^ 0x25
  | BI_NumSubgroups
      -- ^ 0x26
  | BI_NumEnqueuedSubgroups
      -- ^ 0x27
  | BI_SubgroupId
      -- ^ 0x28
  | BI_SubgroupLocalInvocationId
      -- ^ 0x29
  | BI_VertexIndex
      -- ^ 0x2A
  | BI_InstanceIndex
      -- ^ 0x2B
  | BI_SubgroupEqMaskKHR
      -- ^ 0x1140
  | BI_SubgroupGeMaskKHR
      -- ^ 0x1141
  | BI_SubgroupGtMaskKHR
      -- ^ 0x1142
  | BI_SubgroupLeMaskKHR
      -- ^ 0x1143
  | BI_SubgroupLtMaskKHR
      -- ^ 0x1144
  | BI_BaseVertex
      -- ^ 0x1148
  | BI_BaseInstance
      -- ^ 0x1149
  | BI_DrawIndex
      -- ^ 0x114A
  | BI_DeviceIndex
      -- ^ 0x1156
  | BI_ViewIndex
      -- ^ 0x1158
  | BI_BaryCoordNoPerspAMD
      -- ^ 0x1380
  | BI_BaryCoordNoPerspCentroidAMD
      -- ^ 0x1381
  | BI_BaryCoordNoPerspSampleAMD
      -- ^ 0x1382
  | BI_BaryCoordSmoothAMD
      -- ^ 0x1383
  | BI_BaryCoordSmoothCentroidAMD
      -- ^ 0x1384
  | BI_BaryCoordSmoothSampleAMD
      -- ^ 0x1385
  | BI_BaryCoordPullModelAMD
      -- ^ 0x1386
  | BI_FragStencilRefEXT
      -- ^ 0x1396
  | BI_ViewportMaskNV
      -- ^ 0x1485
  | BI_SecondaryPositionNV
      -- ^ 0x1489
  | BI_SecondaryViewportMaskNV
      -- ^ 0x148A
  | BI_PositionPerViewNV
      -- ^ 0x148D
  | BI_ViewportMaskPerViewNV
      -- ^ 0x148E
  | BI_UNKNOWN !Word32 ![Word32]
  deriving (Show,Eq,Ord)

-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#Scope
data Scope =
    S_CrossDevice
      -- ^ 0x0
  | S_Device
      -- ^ 0x1
  | S_Workgroup
      -- ^ 0x2
  | S_Subgroup
      -- ^ 0x3
  | S_Invocation
      -- ^ 0x4
  | S_UNKNOWN !Word32 ![Word32]
  deriving (Show,Eq,Ord)

-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#GroupOperation
data GroupOperation =
    GO_Reduce
      -- ^ 0x0
  | GO_InclusiveScan
      -- ^ 0x1
  | GO_ExclusiveScan
      -- ^ 0x2
  | GO_UNKNOWN !Word32 ![Word32]
  deriving (Show,Eq,Ord)

-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#KernelEnqueueFlags
data KernelEnqueueFlags =
    KEF_NoWait
      -- ^ 0x0
  | KEF_WaitKernel
      -- ^ 0x1
  | KEF_WaitWorkGroup
      -- ^ 0x2
  | KEF_UNKNOWN !Word32 ![Word32]
  deriving (Show,Eq,Ord)

-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#Capability
data Capability =
    C_Matrix
      -- ^ 0x0
  | C_Shader
      -- ^ 0x1
  | C_Geometry
      -- ^ 0x2
  | C_Tessellation
      -- ^ 0x3
  | C_Addresses
      -- ^ 0x4
  | C_Linkage
      -- ^ 0x5
  | C_Kernel
      -- ^ 0x6
  | C_Vector16
      -- ^ 0x7
  | C_Float16Buffer
      -- ^ 0x8
  | C_Float16
      -- ^ 0x9
  | C_Float64
      -- ^ 0xA
  | C_Int64
      -- ^ 0xB
  | C_Int64Atomics
      -- ^ 0xC
  | C_ImageBasic
      -- ^ 0xD
  | C_ImageReadWrite
      -- ^ 0xE
  | C_ImageMipmap
      -- ^ 0xF
  | C_Pipes
      -- ^ 0x11
  | C_Groups
      -- ^ 0x12
  | C_DeviceEnqueue
      -- ^ 0x13
  | C_LiteralSampler
      -- ^ 0x14
  | C_AtomicStorage
      -- ^ 0x15
  | C_Int16
      -- ^ 0x16
  | C_TessellationPointSize
      -- ^ 0x17
  | C_GeometryPointSize
      -- ^ 0x18
  | C_ImageGatherExtended
      -- ^ 0x19
  | C_StorageImageMultisample
      -- ^ 0x1B
  | C_UniformBufferArrayDynamicIndexing
      -- ^ 0x1C
  | C_SampledImageArrayDynamicIndexing
      -- ^ 0x1D
  | C_StorageBufferArrayDynamicIndexing
      -- ^ 0x1E
  | C_StorageImageArrayDynamicIndexing
      -- ^ 0x1F
  | C_ClipDistance
      -- ^ 0x20
  | C_CullDistance
      -- ^ 0x21
  | C_ImageCubeArray
      -- ^ 0x22
  | C_SampleRateShading
      -- ^ 0x23
  | C_ImageRect
      -- ^ 0x24
  | C_SampledRect
      -- ^ 0x25
  | C_GenericPointer
      -- ^ 0x26
  | C_Int8
      -- ^ 0x27
  | C_InputAttachment
      -- ^ 0x28
  | C_SparseResidency
      -- ^ 0x29
  | C_MinLod
      -- ^ 0x2A
  | C_Sampled1D
      -- ^ 0x2B
  | C_Image1D
      -- ^ 0x2C
  | C_SampledCubeArray
      -- ^ 0x2D
  | C_SampledBuffer
      -- ^ 0x2E
  | C_ImageBuffer
      -- ^ 0x2F
  | C_ImageMSArray
      -- ^ 0x30
  | C_StorageImageExtendedFormats
      -- ^ 0x31
  | C_ImageQuery
      -- ^ 0x32
  | C_DerivativeControl
      -- ^ 0x33
  | C_InterpolationFunction
      -- ^ 0x34
  | C_TransformFeedback
      -- ^ 0x35
  | C_GeometryStreams
      -- ^ 0x36
  | C_StorageImageReadWithoutFormat
      -- ^ 0x37
  | C_StorageImageWriteWithoutFormat
      -- ^ 0x38
  | C_MultiViewport
      -- ^ 0x39
  | C_SubgroupDispatch
      -- ^ 0x3A
  | C_NamedBarrier
      -- ^ 0x3B
  | C_PipeStorage
      -- ^ 0x3C
  | C_SubgroupBallotKHR
      -- ^ 0x1147
  | C_DrawParameters
      -- ^ 0x114B
  | C_SubgroupVoteKHR
      -- ^ 0x114F
  | C_StorageBuffer16BitAccess
      -- ^ 0x1151
--  C_StorageUniformBufferBlock16
      -- ^ 0x1151
  | C_UniformAndStorageBuffer16BitAccess
      -- ^ 0x1152
--  C_StorageUniform16
      -- ^ 0x1152
  | C_StoragePushConstant16
      -- ^ 0x1153
  | C_StorageInputOutput16
      -- ^ 0x1154
  | C_DeviceGroup
      -- ^ 0x1155
  | C_MultiView
      -- ^ 0x1157
  | C_VariablePointersStorageBuffer
      -- ^ 0x1159
  | C_VariablePointers
      -- ^ 0x115A
  | C_AtomicStorageOps
      -- ^ 0x115D
  | C_SampleMaskPostDepthCoverage
      -- ^ 0x115F
  | C_ImageGatherBiasLodAMD
      -- ^ 0x1391
  | C_FragmentMaskAMD
      -- ^ 0x1392
  | C_StencilExportEXT
      -- ^ 0x1395
  | C_ImageReadWriteLodAMD
      -- ^ 0x1397
  | C_SampleMaskOverrideCoverageNV
      -- ^ 0x1481
  | C_GeometryShaderPassthroughNV
      -- ^ 0x1483
  | C_ShaderViewportIndexLayerEXT
      -- ^ 0x1486
--  C_ShaderViewportIndexLayerNV
      -- ^ 0x1486
  | C_ShaderViewportMaskNV
      -- ^ 0x1487
  | C_ShaderStereoViewNV
      -- ^ 0x148B
  | C_PerViewAttributesNV
      -- ^ 0x148C
  | C_SubgroupShuffleINTEL
      -- ^ 0x15C0
  | C_SubgroupBufferBlockIOINTEL
      -- ^ 0x15C1
  | C_SubgroupImageBlockIOINTEL
      -- ^ 0x15C2
  | C_UNKNOWN !Word32 ![Word32]
  deriving (Show,Eq,Ord)

-- Reference to an <id> representing the result's type of the enclosing instruction
-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#IdResultType
type IdResultType = ID

-- Definition of an <id> representing the result of the enclosing instruction
-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#IdResult
type IdResult = ID

-- Reference to an <id> representing a 32-bit integer that is a mask from the MemorySemantics operand kind
-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#IdMemorySemantics
type IdMemorySemantics = ID

-- Reference to an <id> representing a 32-bit integer that is a mask from the Scope operand kind
-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#IdScope
type IdScope = ID

-- Reference to an <id>
-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#IdRef
type IdRef = ID

-- An integer consuming one or more words
-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#LiteralInteger
type LiteralInteger = Int64

-- A 32-bit unsigned integer
-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#LiteralInteger32
type LiteralInteger32 = Word32

-- A 64-bit unsigned integer
-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#LiteralInteger64
type LiteralInteger64 = Word64

-- A null-terminated stream of characters consuming an integral number of words
-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#LiteralString
type LiteralString = String

-- A literal number whose size and format are determined by a previous operand in the enclosing instruction
-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#LiteralContextDependentNumber
type LiteralContextDependentNumber = Int64

-- A 32-bit unsigned integer indicating which instruction to use and determining the layout of following operands (for OpExtInst)
-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#LiteralExtInstInteger
type LiteralExtInstInteger = Word32

-- An opcode indicating the operation to be performed and determining the layout of following operands (for OpSpecConstantOp)
-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#LiteralSpecConstantOpInteger
type LiteralSpecConstantOpInteger = Word32

-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#PairLiteralIntegerIdRef
data PairLiteralIntegerIdRef =
  PairLiteralIntegerIdRef !LiteralInteger32 !IdRef
  deriving (Show,Eq,Ord)

-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#PairIdRefLiteralInteger
data PairIdRefLiteralInteger =
  PairIdRefLiteralInteger !IdRef !LiteralInteger32
  deriving (Show,Eq,Ord)

-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#PairIdRefIdRef
data PairIdRefIdRef =
  PairIdRefIdRef !IdRef !IdRef
  deriving (Show,Eq,Ord)
