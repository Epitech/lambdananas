#
#

NAME=lambdananas
CP=cp

BIN_PATH=`stack path --local-install-root`/bin/lambdananas-exe
INSTALL_PATH=$(HOME)/bin/

SRC = app/Main.hs $(wildcard src/*.hs)

all: $(NAME)

run:
	stack build
	$(CP) $(BIN_PATH) $(NAME)
	rm -f ./stack.yaml ./package.yaml ./lambdananas.cabal ./stack.yaml.lock

$(NAME): $(SRC)
	./change_version.sh linux
	@$(MAKE) run

mac:	$(SRC)
	./change_version.sh mac
	@$(MAKE) run


install: $(NAME)
	upx --best $(NAME) || true
	$(CP) $(NAME) $(INSTALL_PATH)

tests_run:
	stack test

tests_run_coverage:
	stack test --coverage

clean:
	stack clean

fclean: clean
	$(RM) $(NAME)

re: fclean all

.PHONY: all mac tests_run clean fclean re
