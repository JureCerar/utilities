VERSION := $(shell cat .VERSION)
DATE    := $(shell date +"%d %b %Y %H:%M:%S")

APP     := rungmx
SRC     := src/main.sh
DIR 		:= /usr/local/bin/
INSTALL := install

# Allow shell commands
.ONESHELL:

default: $(SRC)
	@cp $(SRC) $(APP)
	@sed 's/_PROGRAM/$(APP)/g' -i $(APP)
	@sed 's/_VERSION/$(VERSION)/g' -i $(APP)
	@sed 's/_DATE/$(DATE)/g' -i $(APP)
	@echo "-- Build done."
	@echo "-- Program will be installed to: '$(DIR)'"

.PHONY: clean test install

test: default
	@echo "Test 1 ..."
	@./$(APP) -h
	@echo "Test 2 ..."
	@cd example
	./../$(APP) "-c" "conf.gro" "min" "eq"
	# Remove all except topology and conf
	@ls -p | grep -v / | egrep -v "conf.gro|topol.top|rungmx.log" | xargs rm

install: $(APP)
	@$(INSTALL) $(APP) $(DIR)

clean:
	@rm -f $(APP)
