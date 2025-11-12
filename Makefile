# Makefile for compiling COBOL programs

# Compiler
COB_COMPILER := cobc

# Compiler flags
COB_FLAGS := -x -fixed -Isrc/copybook

# Source directory
SRC_DIR := src

# Binary directory
BIN_DIR := bin

# Add all subdirectories of src to VPATH so make can find the source files
VPATH = $(shell find $(SRC_DIR) -type d)

# Target executables in the bin directory
TARGETS := \
	$(BIN_DIR)/SORT01 \
	$(BIN_DIR)/SUMM01 \
	$(BIN_DIR)/MATCH01 \
	$(BIN_DIR)/SELECT01 \
	$(BIN_DIR)/REFORMAT01 \
	$(BIN_DIR)/SALES_PROC \
	$(BIN_DIR)/CREATE_MASTER \
	$(BIN_DIR)/INV_UPDATE

.PHONY: all clean

all: $(TARGETS)

$(TARGETS): | $(BIN_DIR)

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

# Pattern rule to compile any .cbl file into an executable in the bin directory
$(BIN_DIR)/%: %.cbl
	$(COB_COMPILER) $(COB_FLAGS) -o $@ $<

clean:
	rm -f $(BIN_DIR)/*
