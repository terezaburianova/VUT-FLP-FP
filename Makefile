HC_OPTS = -Wall
OUT = ./out
SRCS = src/Main.hs src/Structure.hs src/Classification.hs
OBJS = out/Main.o out/Structure.o out/Classification.o
# .PHONY : all clean

all: build

library: ${SRCS}
	@mkdir -p ${OUT}
	@ghc ${SRCS} -o ${OUT}/Main ${HC_OPTS} -hidir ${OUT} -odir ${OUT}

${OBJS}: lib

build: src/Main.hs lib
	@mkdir -p ${OUT}
	@ghc -i${OUT} -c src/Main.hs -hidir ${OUT} -odir ${OUT} ${HC_OPTS}
	@ghc ${OBJS} -o flp-fun ${HC_OPTS}

clean:
	rm -rf ${OUT} flp-fun
