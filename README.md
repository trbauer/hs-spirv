A Simple SPIR-V Library

The tools directory contains a script used to generate parts of the IR.

# Use

import Language.SPIRV.SPRIV

    example = do
      b <- decodeBinary "file.spv"   
      process (bOps b)
