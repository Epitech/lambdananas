#
#

NAME=hsc
CP=cp

BIN_PATH=`stack path --local-install-root`/bin/haskell-style-checker-exe
INSTALL_PATH=$(HOME)/bin/

$(NAME):
	stack build
	cp $(BIN_PATH) $(NAME)

all: $(NAME)

install: $(NAME)
	upx --best $(NAME) || true
	cp $(NAME) $(INSTALL_PATH)

tests_run:
	stack test

clean:
	stack clean

fclean: clean
	$(RM) $(NAME)

re: fclean all

.PHONY: all run_tests clean fclean re
