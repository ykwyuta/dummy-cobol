# Makefile for compiling COBOL programs

# Compiler
COB_COMPILER := cobc

# Compiler flags
COB_FLAGS := -fixed -Isrc/copybook

# Source directory
SRC_DIR := src

# Binary directory
BIN_DIR := bin

# Add all subdirectories of src to VPATH so make can find the source files
VPATH = $(shell find $(SRC_DIR) -type d)

# Target executables in the bin directory
EXEC_TARGETS := \
	$(BIN_DIR)/SORT01 \
	$(BIN_DIR)/SUMM01 \
	$(BIN_DIR)/MATCH01 \
	$(BIN_DIR)/SELECT01 \
	$(BIN_DIR)/REFORMAT01 \
	$(BIN_DIR)/SALES_PROC \
	$(BIN_DIR)/CREATE_MASTER

# Target modules in the bin directory
MODULE_TARGETS := \
	$(BIN_DIR)/INV_UPDATE.so


.PHONY: all clean

all: $(EXEC_TARGETS) $(MODULE_TARGETS)

$(EXEC_TARGETS) $(MODULE_TARGETS): | $(BIN_DIR)

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

# Pattern rule to compile any .cbl file into an executable in the bin directory
$(BIN_DIR)/%: %.cbl
	$(COB_COMPILER) -x $(COB_FLAGS) -o $@ $<

# Pattern rule to compile any .cbl file into a module in the bin directory
$(BIN_DIR)/%.so: %.cbl
	$(COB_COMPILER) -m $(COB_FLAGS) -o $@ $<

clean:
	rm -f $(BIN_DIR)/* $(BIN_DIR)/*.so
