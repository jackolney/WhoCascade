CXX = g++
CXXFLAGS = "-mtune=native -03 -Wall -pedantic -Wconversion"

ODIR = ./

OBJ = main.cpp euler.cpp initial.cpp parameters.cpp output.cpp

OBJECTS = $(patsubst %,$(ODIR)/%,$(OBJ))

OUT = cascade.so

cascade: $(OBJECTS)
	@echo 'Compiling...'
	R CMD SHLIB -o $(OUT) $(OBJECTS)
	@echo 'Done.'

.PHONY: clean

clean:
	@echo 'Cleaning...'
	rm -f $(ODIR)/*.o
	@echo 'Done.'

cleanall:
	@echo 'Cleaning all...'
	rm -f $(ODIR)/*.o
	rm -f *.so
	@echo 'Done.'