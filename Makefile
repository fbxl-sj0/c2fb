# SQLite FreeBASIC port tools
# File: Makefile
# Build C2FB and the standalone frontend for its API-table adapter.
# This file does not build the SQLite engine.

FBC ?= fbc
CC ?= cc
PYTHON ?= python3
TARGET ?= build/c2fb
API_ROUTINES_TARGET ?= build/api_routines
CANDIDATE_DIR ?= build/candidates
THREADSAFE ?= 1
SQLITE_SOURCE_DIR ?= ../freebasic-sqlite
FB_SOURCE_DIR ?= $(SQLITE_SOURCE_DIR)/fb_src

# Any C2FB include module change must rebuild the composed converter.
C2FB_MODULES := $(wildcard c2fb_*.bi)

.PHONY: all converter api-routines test check-candidates check-procedures clean

all: converter api-routines

converter: $(TARGET)

$(TARGET): c2fb.bas $(C2FB_MODULES)
	mkdir -p $(dir $@)
	$(FBC) -w all -x $@ $<

$(API_ROUTINES_TARGET): api_routines.bas c2fb_api_routines.bi c2fb_file_input.bi c2fb_output.bi c2fb_generated_file_output.bi c2fb_csv_output.bi c2fb_model.bi c2fb_limits.bi
	mkdir -p $(dir $@)
	$(FBC) -w all -x $@ $<

api-routines: $(API_ROUTINES_TARGET)

test: $(TARGET)
	$(PYTHON) tests/test_c2fb.py --converter '$(abspath $(TARGET))' --fbc '$(FBC)' --cc '$(CC)' \
		--sqlite-source-directory '$(abspath $(SQLITE_SOURCE_DIR))'

check-candidates:
	$(PYTHON) check_candidates.py '$(CANDIDATE_DIR)' --fb-source '$(FB_SOURCE_DIR)' --fbc '$(FBC)' --threadsafe '$(THREADSAFE)'

check-procedures:
	$(PYTHON) check_candidates.py '$(CANDIDATE_DIR)' --fb-source '$(FB_SOURCE_DIR)' --fbc '$(FBC)' --threadsafe '$(THREADSAFE)' --per-procedure

clean:
	rm -rf build

# end of fb_tools/c2fb/Makefile
