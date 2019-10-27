-- extracts IR from the SPIV JSON Grammar
-- generates decoders etc...

import Control.Monad
import Data.Char
import Data.List
import Debug.Trace
import Text.Printf

import GHC.Real

import Text.JSON

-- from SPIRV-Heeaders https://github.com/KhronosGroup/SPIRV-Headers/blob/master/include/spirv/1.2/spirv.core.grammar.json
-- TODO: https://raw.githubusercontent.com/KhronosGroup/SPIRV-Headers/master/include/spirv/1.2/extinst.glsl.std.450.grammar.json
-- TODO: https://raw.githubusercontent.com/KhronosGroup/SPIRV-Headers/master/include/spirv/1.2/extinst.opencl.std.100.grammar.json
main :: IO ()
main = do
  runCoreSpirv "tools/spirv.core.grammar.json"
  -- runExtSpirv "tools/extinst.glsl.std.450.grammar.json" "tools/extinst.opencl.std.100.grammar.json"
  putStrLn "Copy stubs into target files"

runExtSpirv :: FilePath -> FilePath -> IO ()
runExtSpirv fp_glsl fp_ocl = do
  let process fp = do
        jso <- decodeObj <$> readFile fp
        case getField jso "instructions" of
          JSArray ops -> do
            -- return ops
            return ()
            -- parseOp :: JSValue -> IO SPVOp

  process fp_glsl
  process fp_ocl

  return ()


runCoreSpirv :: FilePath -> IO ()
runCoreSpirv fp = do
  fstr <- readFile fp
  let jso = decodeObj fstr
  case (getField jso "instructions",getField jso "operand_kinds") of
    (JSArray ops,JSArray opdts) -> do
--  case "instructions" `lookup` fromJSObject (decodeObj fstr) of
--    Nothing -> error "no 'instructions' field found"
--    Just (JSArray ops) -> do

      spv_ops <- mapM parseOp ops

      -- mapM_ emitDataType opdts
      dts <- mapM parseDataType opdts
      -- mapM_ print dts
      writeFile "Types.stub.hs" $
--        "module Language.SPIRV.Types where\n" ++
        "-- SPLICE IN BELOW type ID = ... line" ++
        "-- Generated from tools/Generate.hs\n" ++
--        "\n" ++
--        "import Data.Int\n" ++
--        "import Data.Word\n" ++
--        "\n" ++
--        "\n" ++
--        "type ID = Int\n" ++
--        "\n" ++
        fmtOpDefinition (map snd dts) spv_ops ++
        "\n" ++
        intercalate "\n" (map (fmtDataTypeDefinition (map snd dts)) dts)
      let decs = filter (\st -> not (spvtName st `elem` skip)) (map snd dts)
            where skip = ["LiteralContextDependentNumber","LiteralInteger"]
      writeFile "SPIRVDecoder.stub.hs" $
        "-- Generated from tools/Generate.hs\n" ++
        fmtOpDecoder (map snd dts) spv_ops ++
        "\n" ++
        intercalate "\n" (map fmtDataTypeDecoder decs) ++ "\n"


fmtOpDefinition :: [SPVType] -> [SPVOp] -> String
fmtOpDefinition sts (spv_op0:spv_ops) =
    "data Op =\n" ++
    fmtOpConstructor "    " spv_op0 ++
    concatMap (fmtOpConstructor "  | ") spv_ops ++
    "  | OpRAW !Word16 ![Word32]\n" ++
    "    -- ^ This represents an unrecognized SPIR-V operation (e.g. from a newer version of SPIR-V?)\n" ++
    "  deriving (Show,Eq)\n" ++
    ""
  where fmtOpConstructor :: String -> SPVOp -> String
        fmtOpConstructor indent spv =
            indent ++ spvName spv ++ " " ++ intercalate " " (map fmtOpnd (spvOperands spv)) ++ "\n" ++
            "        -- ^ https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#" ++ spvName spv ++ "\n"  ++
        --    "        -- " ++ intercalate ", " (map spvoName) ++ "\n"
            ""
          where fmtOpnd spvo = "!" ++ wrap body
                  where wrap =
                          case spvoQuantifier spvo of
                            "?" -> \x -> "(Maybe " ++ x ++  ")"
                            "*" -> \x -> "[" ++ x ++  "]"
                            "" -> id
                        body =
                          case find (\st -> spvtName st == spvoKind spvo) sts of
                            Just (SPVTypeBitset _ _) -> "[" ++ spvoKind spvo ++ "]"
                            _ -> spvoKind spvo

fmtOpDecoder :: [SPVType] -> [SPVOp] -> String
fmtOpDecoder sts sos =
    "dInst :: Word16 -> Int -> DS Op\n" ++
    "dInst opcode nwords = do\n" ++
    "  inst_start_off <- dOffset\n" ++
    "  --\n"++
    "  let dWordsLeft :: DS Int\n" ++
    "      dWordsLeft = do\n" ++
    "        off <- dOffset\n" ++
    "        return (nwords - 1 - ((off - inst_start_off)`div`4))\n" ++
    "  let dOpt :: DS a -> DS (Maybe a) -- decodes something if there's bits left for this inst \n" ++
    "      dOpt da = do\n" ++
    "        n <- dWordsLeft\n" ++
    "        if n <= 0 then return Nothing else (Just <$> da)\n" ++
    "  let dMany :: DS a -> DS [a] -- decodes something if there's bits left for this inst \n" ++
    "      dMany da = do\n" ++
    "        n <- dWordsLeft\n" ++
    "        if n <= 0 then return [] else do {a<-da; (a:) <$> dMany da}\n" ++
    "  let dLiteralContextDependentNumber :: DS LiteralContextDependentNumber\n" ++
    "      dLiteralContextDependentNumber = do -- must always be a suffix\n" ++
    "        n <- dWordsLeft\n" ++
    "        case n of\n" ++
    "          1 -> fromIntegral <$> dX32 \"LiteralContextDependentNumber\" id\n" ++
    "          2 -> dX64 \"LiteralContextDependentNumber\" fromIntegral\n" ++
    "          _ -> dErrorAt inst_start_off \"LiteralContextDependentNumber with unexpected number of words left\"\n" ++
    "  --\n"++
    "  case opcode of\n" ++
    concatMap fmtOp sos ++
    "    _ -> OpRAW opcode <$> sequence (replicate (nwords-1) (dX32 \"raw instruction word\" id))\n" ++
    ""
  where fmtOp so
          | all (/=Nothing) operand_decoders = "    " ++ show (spvCode so) ++ " -> " ++ applicative_decode ++ "\n"
          | otherwise = "    " ++ show (spvCode so) ++ " -> do -- " ++ spvName so ++ "\n" ++ decoder_fixup_needed
          where applicative_decode
                  | null (spvOperands so) = "return " ++ spvName so
                  | otherwise =
                    spvName so ++ " <$> " ++ intercalate " <*> " (map fmtOpndDec operand_decoders)
                  where fmtOpndDec Nothing = error "cannot find decoder"
                        fmtOpndDec (Just d) = d

                operand_decoders :: [Maybe String]
                operand_decoders = findOperandDecoders 0 (spvOperands so)
                  where findOperandDecoders :: Int -> [SPVOpnd] -> [Maybe String]
                        findOperandDecoders _     [] = []
                        findOperandDecoders op_ix (soo:soos) = maybe_decoder:findOperandDecoders (op_ix+1) soos
                          where maybe_decoder
                                  | is_fixed_size =
                                    case spvoQuantifier soo of
                                      "" -> Just ("d" ++spvoKind soo)
                                      "?" -> Just ("dOpt d" ++spvoKind soo)
                                      "*"
                                        | null soos -> Just ("dMany d" ++spvoKind soo)
                                      _ -> Nothing
                                  -- | spvoKind soo ++ spvoQuantifier soo == "IdRef*" = Just "dIdsSuffix"
                                  -- | spvoKind soo ++ spvoQuantifier soo == "IdRef?" = Just "dIdOptSuffix"
                                  -- TODO: other quantifiers
                                  | otherwise =
                                    case (spvName so,op_ix) `lookup` special_fixed_cases of
                                      Just d -> Just d
                                      _ -> Nothing -- let it fallback to manual case
                                  where is_fixed_size = isFixedWidthTypeName sts (spvoKind soo)

                                isOperandFixedWidth :: SPVOpnd -> Bool
                                isOperandFixedWidth soo =
                                    null (spvoQuantifier soo) && isFixedWidthTypeName sts (spvoKind soo)

                -- special cases for decode
                special_fixed_cases :: [((String,Int),String)]
                special_fixed_cases =
                    [
                    -- OpSource
                    --   mkCase "OpSource" 2 "dIdOptSuffix"
                    -- , mkCase "OpSource" 3 "dStringOptSuffix"
                    --
                    --  mkCase "OpTypeImage" 8 "dOpt dAccessQualifier"
                    --
                         mkCase "OpConstant" 2 "dLiteralContextDependentNumber"
                    --
                       , mkCase "OpSpecConstant" 2 "dLiteralContextDependentNumber" -- the spec has LiteralSpecConstantOpInteger
                    --
                       , mkCase "OpSpecConstantOp" 2 "dLiteralInteger"
                    -- , mkCase "OpGroupMemberDecorate" 2 "dLiteralInteger"
                    ]
                  where mkCase t i dec = ((t,i),dec)

                -- manually decode needed
                decoder_fixup_needed :: String
                decoder_fixup_needed = decode_operands ++ return_cons
                  where decode_operands = concatMap decOperand $ zip3 [0..] operand_decoders (spvOperands so)
                        return_cons = do_indent ++ "return $ " ++ spvName so ++ concatMap (\ix -> " opnd"++show ix) [0..(length (spvOperands so) - 1)] ++ "\n"
                        do_indent = "      "
                        --
                        decOperand :: (Int, Maybe String, SPVOpnd) -> String
                        decOperand (op_ix,md,soo) =
                          case md of
                            Just d ->
                              do_indent ++ "opnd" ++ show op_ix ++ " <- " ++ d ++ "\n"
                            Nothing ->
                              do_indent ++ "opnd" ++ show op_ix ++ " <- error FIXME: " ++ spvoKind soo ++ spvoQuantifier soo ++ "\n"


isFixedWidthTypeName :: [SPVType] -> String -> Bool
isFixedWidthTypeName sts name =
  case find ((==name) . spvtName) sts of
    Just st -> isFixedWidthType sts st

isFixedWidthType :: [SPVType] -> SPVType -> Bool
isFixedWidthType sts st =
  case st of
    SPVTypeEnum _ _ -> True
    SPVTypeBitset _ _ -> True
    SPVTypeID _ -> True
    SPVTypeLiteralString -> True
    SPVTypeLiteralIntegral nm -> primitiveType st /= Nothing
--      nm `elem` ["LiteralInteger","LiteralInteger32","LiteralExtInstInteger","LiteralSpecConstantOpInteger"]
    SPVTypeComposite _ es -> all (isFixedWidthTypeName sts) es
--    _ -> False


-- prefixes symbol to make it unique
fmtSpvEnumSymbol :: SPVType -> String -> String
fmtSpvEnumSymbol st sym = pfx ++ "_" ++ sym
  where -- pfx = spvtName st -- would use the full name
        pfx -- more compact def. uses caps chars "FooBarBaz" -> "FBB"
          | sym /= "UNKNOWN" = filter isUpper (spvtName st)
          | otherwise =
            case spvtName st of
              "ExecutionModel" -> spvtName st -- conflicts with ExecutionMode
              "SelectionControl" -> spvtName st -- conflicts with StorageClass
              "Decoration" -> spvtName st -- Dim and Decoration conflict
              name -> filter isUpper name


-- some enums are replicated (mapped by same ordinal)
-- we manually omit them
--   http://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_16bit_storage.html
isDuplicateEnum :: String -> Bool
isDuplicateEnum = (`elem`duplicates)
  where duplicates =
          [
            "StorageUniformBufferBlock16"
          , "StorageUniform16"
          , "ShaderViewportIndexLayerNV"
          ]

fmtDataTypeDefinition :: [SPVType] -> (String,SPVType) -> String
fmtDataTypeDefinition sts (doc,st) =
    doc_text ++
    doc_url_text ++
    data_type_text
  where doc_text = if null doc then "" else ("-- " ++ doc ++ "\n")
        doc_url_text = "-- https://www.khronos.org/registry/spir-v/specs/1.2/SPIRV.html#" ++ spvtName st ++ "\n"

        fmtEnumOrBitSet :: String -> [(String,Int,[(String,String)])] -> String
        fmtEnumOrBitSet name (e0:es) =
            "data " ++ name ++ " =\n" ++
            fmtElem "    " e0 ++
            concatMap (fmtElem "  | ") es ++
            "  | " ++ fmtSpvEnumSymbol st "UNKNOWN" ++ " !Word32 ![Word32]\n" ++
            "  deriving (Show,Eq,Ord)\n"
          where fmtElem indent (e,val,ps) =
                    this_indent ++ printf "%-24s " (fmtSpvEnumSymbol st e) ++ param_args ++ "\n" ++
                    map (const ' ') indent ++ printf "  -- ^ 0x%X " val ++ param_docs ++ "\n"
                  where this_indent
                          | isDuplicateEnum e = "-- " ++ drop 3 indent
                          | otherwise = indent
                        param_args = concatMap fmtParamType ps
                          where fmtParamType (k,_) = " !" ++ t
                                  where t = case find (\st -> spvtName st == k) sts of
                                              Just (SPVTypeBitset _ _) -> "[" ++ k ++ "]"
                                              _ -> k
                        param_docs = if null ps then "" else (" (" ++ intercalate ", " (map snd ps) ++ ")")

        data_type_text =
          case st of
            SPVTypeEnum name es -> fmtEnumOrBitSet name es
            SPVTypeBitset name es -> fmtEnumOrBitSet name es
            SPVTypeID nm -> "type " ++ nm ++ " = ID\n"
            SPVTypeLiteralIntegral "LiteralInteger" ->
              "type LiteralInteger = Int64\n"
            SPVTypeLiteralIntegral "LiteralContextDependentNumber" ->
              "type LiteralContextDependentNumber = Int64\n"
            SPVTypeLiteralIntegral _ ->
              case primitiveType st of
                Just t -> "type " ++ spvtName st ++ " = " ++ t ++ "\n"
                Nothing -> "type " ++ spvtName st ++ " = ???\n"
              where integral_type = "Word32" -- "Integer" (could be other stuff too)
            SPVTypeLiteralString -> "type " ++ spvtName st ++ " = String\n"
            SPVTypeComposite _ es ->
              "data " ++ spvtName st ++ " =\n" ++
              "  " ++ spvtName st ++ concatMap (\e -> " !" ++ e) es ++ "\n" ++
              "  deriving (Show,Eq,Ord)\n" ++
              ""

fmtDataTypeDecoder :: SPVType -> String
fmtDataTypeDecoder st =
    "d" ++ spvtName st ++ " :: DS " ++ data_type ++ "\n" ++
    decoder_body ++
    ""
  where data_type =
          case st of
            SPVTypeBitset _ _ -> "[" ++ spvtName st ++ "]"
            _ -> spvtName st

        decoder_body :: String
        decoder_body =
          case st of
            SPVTypeEnum nm es ->
              "d" ++ spvtName st ++ " = do\n" ++
              "  val <- dX32 \"" ++ spvtName st ++ "\" id\n" ++
              "  case val of\n" ++
              concatMap fmtEnumCase es ++
              "    _ -> return $ " ++ fmtSpvEnumSymbol st "UNKNOWN" ++ " val [] -- FIXME: pass hint to tell how long the instruction is\n"
            SPVTypeBitset nm es ->
                "d" ++ spvtName st ++ " = do\n" ++
                "  val <- dX32 \"" ++ spvtName st ++ "\" id\n" ++
                "  dEnum val " ++ fmtSpvEnumSymbol st "UNKNOWN" ++ enum_list

              where enum_list
                      | length non_zero_enums <= 3 =
                        " [" ++ intercalate "," (map fmtEnum non_zero_enums) ++ "]"
                      | otherwise =
                        "\n" ++
                        "    [\n" ++
                        fmtEnumLine "      " (head non_zero_enums) ++
                        concatMap (fmtEnumLine "    , ") (tail non_zero_enums) ++
                        "    ]\n"
                    non_zero_enums = filter ((/=0) . snd3) es
                    fmtEnumLine indent e = indent ++ fmtEnum e ++ "\n"

                    fmtEnum (nm,cd,ps) = "(" ++ fmtCode cd ++ "," ++ enum_case ++")"
                      where enum_case
                              | null ps = "return " ++ fmtSpvEnumSymbol st nm
                              | otherwise = fmtSpvEnumSymbol st nm ++ " <$> " ++ intercalate " <*> " (map (\(k,_) -> "d" ++ k) ps)

                    fmtCode :: Int -> String
                    fmtCode = printf ("0x%0" ++ show max_ds ++ "X")
                     where max_ds :: Int
                           max_ds = maximum (1:map (hexDigits . snd3) es)

                           hexDigits :: Int -> Int
                           hexDigits = length . (printf "%X" :: Int -> String)

            SPVTypeID _ ->
              "d" ++ spvtName st ++ " = dXID " ++ show (spvtName st) ++ "\n"
            SPVTypeLiteralString ->
              "d" ++ spvtName st ++ " = dString\n"
            SPVTypeLiteralIntegral "LiteralInteger" ->
              "d" ++ spvtName st ++ " = ???\n"
            SPVTypeLiteralIntegral _ ->
              case primitiveType st of
                Just "Word32" ->
                  "d" ++ spvtName st ++ " = dX32 \""++spvtName st++"\" id\n"
                Just "Word64" ->
                  "d" ++ spvtName st ++ " = dX64 \""++spvtName st++"\" id\n"
                Nothing ->
                  "d" ++ spvtName st ++ " = ???\n"
            SPVTypeComposite nm ps ->
              "d" ++ spvtName st ++ " = " ++ spvtName st ++ " <$> " ++
                intercalate " <*> " (map (\p -> " d" ++ p) ps) ++ "\n"
            -- _ -> "  error \"TODO: decode " ++ spvtName st ++ "\"\n"

        fmtEnumCase :: (String,Int,[(String,String)]) -> String
        fmtEnumCase (nm,cd,ps) = this_indent ++ show cd ++ " -> " ++ cons ++ "\n"
          where this_indent
                  | isDuplicateEnum nm = "--  "
                  | otherwise = "    "
                cons
                  | null ps = "return " ++ fmtSpvEnumSymbol st nm
                  | otherwise = fmtSpvEnumSymbol st nm ++ " <$> " ++ intercalate " <*> " (map (\(k,_) -> "d" ++ k) ps)


snd3 (_,y,_) = y

primitiveType :: SPVType -> Maybe String
primitiveType st =
  case st of
    SPVTypeLiteralIntegral "LiteralInteger32" -> Just "Word32"
    SPVTypeLiteralIntegral "LiteralInteger64" -> Just "Word64"
--    SPVTypeLiteralIntegral "LiteralInteger"   -> Just "Int64" -- context sensitive
    SPVTypeLiteralIntegral "LiteralExtInstInteger" -> Just "Word32" -- "A 32-bit unsigned ..."
    SPVTypeLiteralIntegral "LiteralSpecConstantOpInteger" -> Just "Word32"
    _ -> Nothing



-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------

data SPVOp =
  SPVOp {
    spvName :: !String -- e.g. OpFAdd
  , spvCode :: !Int -- e.g. 129
  , spvOperands :: ![SPVOpnd]
  } deriving Show

data SPVOpnd =
  SPVOpnd {
    spvoKind :: !String -- "ID" or
  , spvoQuantifier :: !String -- e.g. "?"
  , spvoName :: !String -- optional (can be "")
  } deriving Show

data SPVType =
    -- list of (enumerant, value, params); params are (kind,name) pairs
    SPVTypeEnum   !String [(String,Int,[(String,String)])] -- e.g. Decoration
  | SPVTypeBitset !String [(String,Int,[(String,String)])] -- some take an argument
  | SPVTypeID     !String -- e.g. IdResultType, IdRef, ...
  | SPVTypeLiteralIntegral !String  -- LiteralInteger, LiteralExtInstInteger, ...
  | SPVTypeLiteralString            -- LiteralString
  | SPVTypeComposite !String [String] -- e.g. PairLiteralIntegerIdRef
  deriving Show
spvtName :: SPVType -> String
spvtName st =
  case st of
    SPVTypeEnum nm _ -> nm
    SPVTypeBitset nm _ -> nm
    SPVTypeID nm -> nm
    SPVTypeLiteralIntegral nm -> nm
    SPVTypeLiteralString -> "LiteralString"
    SPVTypeComposite nm _ -> nm

-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------


decodeObj :: String -> JSObject JSValue
decodeObj inp =
  case decode inp of
    Ok a -> a
    Error e -> error e


parseDataType :: JSValue -> IO (String,SPVType)
parseDataType (JSObject jso) = do
    t <- parseBody
    return (doc,t)
  where category = getStringField jso "category"
        kind = getStringField jso "kind"
        doc = maybe "" id (findStringField jso "doc")

        parseBody :: IO SPVType
        parseBody
          | category == "BitEnum"   = parseBitset
          | category == "ValueEnum" = parseEnum
          | category == "Id" = parseId
          | category == "Literal" = return $
            case kind of
              "LiteralInteger" -> SPVTypeLiteralIntegral kind
              "LiteralString" -> SPVTypeLiteralString
              -- TODO: something better here
              _ -> SPVTypeLiteralIntegral kind
          | category == "Composite" = parseComposite
          | otherwise = error $ category ++ ": unhandled data category"

        parseEnum :: IO SPVType
        parseEnum =
          case getField jso "enumerants" of
            JSArray jses -> return $ SPVTypeEnum kind (map parseEnum jses)
              where parseEnum (JSObject eo) = (enum_name,enum_value,params)
                      where enum_name = getStringField eo "enumerant"
                            enum_value = getIntField eo "value"
                            params = parseParams eo

        parseParams :: JSObject JSValue -> [(String,String)]
        parseParams eo =
          case findField eo "parameters" of
            Nothing -> []
            Just (JSArray jsps) -> map parseParam jsps
              where parseParam (JSObject po) = (kind,name)
                      where kind = getStringField po "kind"
                            name =
                              case findStringField po "name" of
                                Nothing -> ""
                                Just nm -> nm

        parseBitset :: IO SPVType
        parseBitset =
          case getField jso "enumerants" of
            JSArray jses -> return $ SPVTypeBitset kind (map parseEnum jses)
              where parseEnum (JSObject eo) = (enum_name,enum_value,params)
                      where enum_name = getStringField eo "enumerant"
                            enum_value = getIntField eo "value"
                            params = parseParams eo

        -- putStrLn "TODO: emitDataType.handleId"
        parseId = return (SPVTypeID kind)

        parseComposite = return $
          case getField jso "bases" of
            JSArray es -> SPVTypeComposite kind (map parseCompositeElems es)
              where parseCompositeElems (JSString str) = fromJSString str


getField :: JSObject JSValue -> String -> JSValue
getField jso field =
  case jso `findField` field of
    Nothing -> error $ "cannot find field " ++ show field ++ " in " ++ show jso
    Just jsv -> jsv
findField :: JSObject JSValue -> String -> Maybe JSValue
findField jso field = field `lookup` fromJSObject jso

getStringField :: JSObject JSValue -> String -> String
getStringField jso field =
  case field `lookup` fromJSObject jso of
    Nothing -> error $ "cannot find field " ++ show field ++ " in " ++ show jso
    Just (JSString str) -> fromJSString str
getIntField :: JSObject JSValue -> String -> Int
getIntField jso field =
  case field `lookup` fromJSObject jso of
    Nothing -> error $ "cannot find field " ++ show field ++ " in " ++ show jso
    Just (JSRational False (jsopcd :% 1)) -> fromIntegral jsopcd :: Int
    Just (JSString str) ->
      -- some fields are wrapped in strings....
      case reads (fromJSString str) of
        [(x,"")] -> x
        _ -> error $ "malformed int field " ++ show field ++ " in " ++ show jso
findStringField :: JSObject JSValue -> String -> Maybe String
findStringField jso field =
  case findField jso field of
    Nothing -> Nothing
    Just (JSString str) -> Just $ fromJSString str






parseOp :: JSValue -> IO SPVOp
parseOp (JSObject val) = do
    case ("opname" `lookup` fromJSObject val,"opcode" `lookup` fromJSObject val) of
      (Nothing,_) -> error "cannot find 'opname'"
      (_,Nothing) -> error "cannot find 'opcode'"
      (Just (JSString jsopnm),Just (JSRational False (jsopcd :% 1))) ->
        handleOperands (fromJSString jsopnm) (fromIntegral jsopcd) $
          case "operands" `lookup` fromJSObject val of
            Nothing -> []
            Just (JSArray objs) -> objs
  where handleOperands :: String -> Int -> [JSValue] -> IO SPVOp
        handleOperands opnm opcd jsopnds = do
            opnds <- mapM handleOpnd jsopnds
            return (SPVOp opnm opcd opnds)
          where handleOpnd :: JSValue -> IO SPVOpnd
                handleOpnd jsv =
                  case findStringField "kind" jsv of
                    Just kind -> do
                      return $
                        SPVOpnd
                          kind
                          (findStringFieldEmpty "quantifier" jsv)
                          (findStringFieldEmpty "name" jsv)

                findStringFieldEmpty :: String -> JSValue -> String
                findStringFieldEmpty f jsv =
                  maybe "" id (findStringField f jsv)

                findStringField :: String -> JSValue -> Maybe String
                findStringField f (JSObject jso) =
                  case f `lookup` fromJSObject jso of
                    Nothing -> Nothing
                    Just (JSString jsv) -> Just $ fromJSString jsv

