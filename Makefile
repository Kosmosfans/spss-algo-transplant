# compiler and flags
FC = gfortran
FFLAGS = -O2 -Wall

# directory structure
SRC_DIR = src
BUILD_DIR = build
BIN_DIR = bin

# default program name
PROGRAM_NAME = spss_program

# collect .f90 from src (recurses if needed with find)
LIB_SRCS := $(shell find $(SRC_DIR) -name '*.f90' | tr '\n' ' ')

# change if you want to specify a different main program
SRCS := $(LIB_SRCS) examples/test_kmeans.f90

# create build and bin directories if they don't exist
$(shell mkdir -p $(BUILD_DIR) $(BIN_DIR))

all: $(BIN_DIR)/$(PROGRAM_NAME)

# compile all Fortran files at once to avoid manual module ordering
$(BIN_DIR)/$(PROGRAM_NAME):
	$(FC) $(FFLAGS) -J$(BUILD_DIR) $(SRCS) -o $@

run: all
	./$(BIN_DIR)/$(PROGRAM_NAME)

clean:
	rm -rf $(BUILD_DIR)/* $(BIN_DIR)/*

.PHONY: all clean