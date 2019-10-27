@set OCLOC_EXE=E:\dev\clasm-local\clasm\drivers\gfx-driver-ci-master-3162\Standalones\ocloc.exe
@"%OCLOC_EXE%" -file %1.cl -device skl
@mv %1_Gen9core.spv %1.spv
@rm *_Gen9core*
@spirv-dis --raw-id --offsets %1.spv > %1.spvtxt
