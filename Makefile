#
#

NAME=hsc
CP=cp

all:
	stack build
	cp `stack path --local-install-root`/bin/haskell-style-checker-exe $(NAME)

clean:
	stack clean

fclean: clean
	$(RM) $(NAME)

re: fclean all

.PHONY: all clean fclean re
