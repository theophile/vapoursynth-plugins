include ../../config.mak

ifeq ($(HAVE_NASM),yes)
ifeq ($(HAVE_FFTW3F),yes)

LIBNAME = mvtools
LIBADD = -lfftw3f
local_CPPFLAGS = -DMVTOOLS_X86 -DPACKAGE_VERSION='"$(shell cat VERSION)"'

VSASFLAGS = -I./src/asm/include/ -w -Worphan-labels -Wunrecognized-char -Dprivate_prefix=mvtools -DHIGH_BIT_DEPTH=0 -DBIT_DEPTH=8
ifeq ($(HAVE_X86_64),yes)
VSASFLAGS += -DARCH_X86_64=1 -DPIC -f elf64
else
VSASFLAGS += -DARCH_X86_64=0 -f elf32
endif

%.o: %.asm
	$(nasmcompile)

%_AVX2.o: %_AVX2.cpp
	$(CXX_silent)$(CXX) $(VSCXXFLAGS) -mavx2 -o $@ $^

endif #HAVE_FFTW3F
endif #HAVE_NASM

include ../../cxx_cc.inc

