CC = gcc
CFLAGS = `idris --include`

foreign.o: foreign.c foreign.h

clean: .PHONY 
	rm foreign.o

.PHONY: 
